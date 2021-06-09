#' Import the observations
#'
#' The function overwrites any existing table with observations.
#' @param observations a data.frame with the observations.
#' @param max_dist maximum clustering distance in m.
#' @param conn a DBI connection to an SQLite database.
#' @export
#' @importFrom assertthat assert_that has_name is.string
#' @importFrom RSQLite dbClearResult dbSendQuery dbWriteTable
import_observations <- function(observations, conn, max_dist = 336) {
  assert_that(inherits(observations, "data.frame"))
  assert_that(
    has_name(observations, "x"), has_name(observations, "y"),
    has_name(observations, "survey"), has_name(observations, "status")
  )
  assert_that(
    is.numeric(observations$x), is.numeric(observations$y),
    is.integer(observations$survey), is.integer(observations$status)
  )
  assert_that(inherits(conn, "SQLiteConnection"))

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

  observations$id <- observations$cluster <- seq_along(observations$x)
  observations$group_x <- floor(observations$x / max_dist / 2)
  observations$group_y <- floor(observations$y / max_dist / 2)
  cols <- c("id", "x", "y", "survey", "status", "cluster", "group_x", "group_y")
  dbWriteTable(
    conn, name = "observation", append = TRUE, value = observations[, cols]
  )
}
