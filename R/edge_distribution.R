#' @title Edge distribution
#' @description
#' This function returns the length of the edges of the Delaunay triangulation
#' per survey.
#' The function returns only edges with length smaller than twice `max_dist`.
#' @inheritParams import_observations
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom deldir deldir
#' @importFrom RSQLite dbGetQuery
#' @export
edge_distribution <- function(conn, max_dist = 336) {
  assert_that(
    inherits(conn, "SQLiteConnection"),
    is.number(max_dist), noNA(max_dist), max_dist > 0
  )
  "SELECT survey, user, x, y FROM observation" |>
    dbGetQuery(conn = conn) -> observations
  table(observations$survey) |>
    as.data.frame() -> n_survey
  n_survey[n_survey$Freq > 1, ] |>
    merge(x = observations, by.y = "Var1", by.x = "survey") -> observations
  edges <- data.frame(survey = integer(0), user = integer(0),
                      length = numeric(0))
  for (i in unique(observations$survey)) {
    candidate <- which(observations$survey == i)
    user <- observations[candidate[1], "user"]
    dd <- deldir(
      x = rnorm(length(candidate), mean = observations$x[candidate], sd = 0.01),
      y = rnorm(length(candidate), mean = observations$y[candidate], sd = 0.01),
      id = observations$id[candidate]
    )
    extra <- data.frame(
      survey = i,
      user = user,
      length = sqrt(
        (dd$delsgs$x2 - dd$delsgs$x1) ^ 2 + (dd$delsgs$y2 - dd$delsgs$y1) ^ 2
      )
    )
    extra[extra$length <= 2 * max_dist, ] |>
      rbind(edges) -> edges
  }
  return(edges)
}
