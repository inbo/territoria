#' Import the observations
#'
#' The function overwrites any existing table with observations.
#' @param observations a data.frame with the observations.
#' @param max_dist maximum clustering distance in m.
#' @param max_edge The maximum edge length in m.
#' We apply a Delaunay triangulation to the observations per survey.
#' After removing the edges larger than `max_edge`, we see which observations
#' result in a connected graph.
#' Every connected graph is assigned a new survey id.
#' @param conn a DBI connection to an SQLite database.
#' @export
#' @importFrom assertthat assert_that has_name is.number is.string noNA
#' @importFrom deldir deldir
#' @importFrom igraph decompose graph_from_data_frame V
#' @importFrom RSQLite dbClearResult dbSendQuery dbWriteTable
#' @importFrom stats aggregate rnorm
import_observations <- function(
  observations, conn, max_dist = 336, max_edge = 1500
) {
  assert_that(
    inherits(observations, "data.frame"),
    is.number(max_dist), noNA(max_dist), max_dist > 0,
    is.number(max_edge), noNA(max_edge), max_edge > 0, max_dist < max_edge
  )
  assert_that(
    has_name(observations, "id"), has_name(observations, "user"),
    has_name(observations, "x"), has_name(observations, "y"),
    has_name(observations, "survey"), has_name(observations, "status"),
    has_name(observations, "region")
  )
  assert_that(is.numeric(observations$x), is.numeric(observations$y))
  assert_that(
    noNA(observations$id), noNA(observations$x), noNA(observations$y),
    noNA(observations$survey), noNA(observations$status),
    noNA(observations$user)
  )
  observations$id <- make_integer(observations$id)
  observations$survey <- make_integer(observations$survey)
  observations$status <- make_integer(observations$status)
  observations$user <- make_integer(observations$user)
  observations$region <- make_integer(observations$region)
  assert_that(
    anyDuplicated(observations$id) == 0, msg = "duplicate values in id"
  )
  unique_combo <- unique(observations[, c("survey", "user")])
  assert_that(
    anyDuplicated(unique_combo) == 0, msg = "`survey` with multiple `user`"
  )

  diagonal <- diff(range(observations$x)) ^ 2 + diff(range(observations$y)) ^ 2
  assert_that(
    max_dist < sqrt(diagonal),
    msg = "`max_dist` is larger that diagonal of the bounding box"
  )

  assert_that(inherits(conn, "SQLiteConnection"))

  sql <- "DROP TABLE IF EXISTS distance"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "DROP TABLE IF EXISTS survey"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "DROP TABLE IF EXISTS observation"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "DROP TABLE IF EXISTS unlikely"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "CREATE TABLE survey (
  id INTEGER PRIMARY KEY, original INTEGER NOT NULL, user INTEGER NOT NULL
)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "CREATE TABLE unlikely (
  survey INTEGER NOT NULL, reason character NOT NULL, value NUMERIC NOT NULL
)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "CREATE TABLE observation (
  id INTEGER PRIMARY KEY, x REAL NOT NULL, y REAL NOT NULL,
  group_x INTEGER NOT NULL, group_y INTEGER NOT NULL, survey INTEGER NOT NULL,
  status INTEGER NOT NULL, cluster INTEGER NOT NULL, region INTEGER,
  user INTEGER NOT NULL
)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  # force observations into a data.frame to avoid problems with sf objects
  observations <- as.data.frame(observations)

  # splits surveys into clearly distinct groups
  observations$original <- observations$survey
  # surveys with a bounding box diagonal smaller that the max_edge are OK
  bb_min <- aggregate(cbind(x, y) ~ survey, data = observations, FUN = min)
  bb_max <- aggregate(cbind(x, y) ~ survey, data = observations, FUN = max)
  diagonal <- sqrt((bb_max$x - bb_min$x) ^ 2 + (bb_max$y - bb_min$y) ^ 2)
  to_do <- bb_min$survey[diagonal >= max_edge]
  done <- observations[!observations$survey %in% to_do, ]
  # handle surveys with a bounding box diagonal larger than the max_edge
  for (i in to_do) {
    candidate <- which(observations$survey == i)
    # make Delaunay triangulation
    dd <- deldir(
      x = rnorm(length(candidate), mean = observations$x[candidate], sd = 0.01),
      y = rnorm(length(candidate), mean = observations$y[candidate], sd = 0.01),
      id = observations$id[candidate]
    )
    # calculate length of edges
    edges <- data.frame(
      id1 = dd$delsgs$ind1, id2 = dd$delsgs$ind2,
      length = sqrt(
        (dd$delsgs$x2 - dd$delsgs$x1) ^ 2 + (dd$delsgs$y2 - dd$delsgs$y1) ^ 2
      )
    )
    # do nothing when every edge is smaller than the max_edge
    if (max(edges$length) < max_edge) {
      done <- rbind(done, observations[candidate, ])
      next
    }
    # remove edges larger than the max_edge and decompose the graph
    edges[edges$length < max_edge, ] |>
      rbind(
        data.frame(
          id1 = observations$id[candidate], id2 = observations$id[candidate],
          length = 0
        )
      ) |>
      graph_from_data_frame(directed = FALSE) |>
      decompose() -> sg
    # do nothing when there is this one graph
    if (length(sg) == 1) {
      done <- rbind(done, observations[candidate, ])
      next
    }
    # split the survey into groups when there are multiple graphs
    for (j in seq_along(sg)) {
      V(sg[[j]]) |>
        names() |>
        as.integer() -> relevant
      extra <- observations[observations$id %in% relevant, ]
      extra$survey <- j
      done <- rbind(done, extra)
    }
  }
  done$survey <- interaction(done$original, done$survey, drop = TRUE) |>
    as.integer()

  # store the original survey id and user id
  surveys <- unique(done[, c("survey", "original", "user")])
  colnames(surveys) <- c("id", "original", "user")
  dbWriteTable(conn, name = "survey", append = TRUE, value = surveys)

  # store the observations with new survey id
  done$cluster <- done$id
  done$group_x <- floor(done$x / max_dist / 2)
  done$group_y <- floor(done$y / max_dist / 2)
  cols <- c(
    "id", "x", "y", "survey", "status", "cluster", "group_x", "group_y",
    "region", "user"
  )
  dbWriteTable(
    conn, name = "observation", append = TRUE, value = done[, cols]
  )

  # create an index on the observation table
  sql <- "CREATE INDEX IF NOT EXISTS observation_idx ON
  observation (group_x, group_y)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
  return(invisible(NULL))
}

#' Make a vector integer
#' @noRd
#' @importFrom assertthat assert_that
make_integer <- function(x) {
  if (is.integer(x)) {
    return(x)
  }
  assert_that(is.numeric(x))
  assert_that(
    max(abs(x - round(x))) < 1e-6, msg = "Large difference to nearest integer"
  )
  return(as.integer(round(x)))
}
