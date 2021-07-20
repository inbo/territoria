#' Import the observations
#'
#' The function overwrites any existing table with observations.
#' @param observations a data.frame with the observations.
#' @param max_dist maximum clustering distance in m.
#' @param conn a DBI connection to an SQLite database.
#' @export
#' @importFrom assertthat assert_that has_name is.number is.string noNA
#' @importFrom RSQLite dbClearResult dbSendQuery dbWriteTable
import_observations <- function(observations, conn, max_dist = 336) {
  assert_that(inherits(observations, "data.frame"))
  assert_that(
    has_name(observations, "id"),
    has_name(observations, "x"), has_name(observations, "y"),
    has_name(observations, "survey"), has_name(observations, "status")
  )
  assert_that(is.numeric(observations$x), is.numeric(observations$y))
  assert_that(
    noNA(observations$id), noNA(observations$x), noNA(observations$y),
    noNA(observations$survey), noNA(observations$status)
  )
  observations$id <- make_integer(observations$id)
  observations$survey <- make_integer(observations$survey)
  observations$status <- make_integer(observations$status)
  assert_that(
    anyDuplicated(observations$id) == 0, msg = "duplicate values in id"
  )
  diagonal <- diff(range(observations$x)) ^ 2 + diff(range(observations$y)) ^ 2
  assert_that(
    max_dist < sqrt(diagonal),
    msg = "`max_dist` is larger that diagonal of the bounding box"
  )

  assert_that(inherits(conn, "SQLiteConnection"))

  assert_that(is.number(max_dist), max_dist > 0)

  sql <- "DROP TABLE IF EXISTS distance"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "DROP TABLE IF EXISTS observation"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  sql <- "CREATE TABLE observation (
  id INTEGER PRIMARY KEY, x REAL NOT NULL, y REAL NOT NULL,
  group_x INTEGER NOT NULL, group_y INTEGER NOT NULL, survey INTEGER NOT NULL,
  status INTEGER NOT NULL, cluster INTEGER NOT NULL)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)

  # force observations into a data.frame to avoid problems with sf objects
  observations <- as.data.frame(observations)

  observations$cluster <- observations$id
  observations$group_x <- floor(observations$x / max_dist / 2)
  observations$group_y <- floor(observations$y / max_dist / 2)
  cols <- c("id", "x", "y", "survey", "status", "cluster", "group_x", "group_y")
  dbWriteTable(
    conn, name = "observation", append = TRUE, value = observations[, cols]
  )

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
