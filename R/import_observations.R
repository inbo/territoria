#' Import the observations
#'
#' The function overwrites any existing table with observations.
#' @param observations a data.frame with the observations.
#' @param conn a DBI connection to an SQLite database.
#' @export
#' @importFrom assertthat assert_that has_name is.string
#' @importFrom RSQLite dbWriteTable
import_observations <- function(observations, conn = connect_db()) {
  assert_that(inherits(observations, "data.frame"))
  assert_that(
    has_name(observations, "x"), has_name(observations, "y"),
    has_name(observations, "survey"), has_name(observations, "status")
  )
  assert_that(
    is.numeric(observations$x), is.numeric(observations$y),
    is.integer(observations$survey), is.integer(observations$status)
  )
  dbWriteTable(
    conn, name = "observation", overwrite = TRUE,
    value = observations[, c("x", "y", "survey", "status")]
  )
}
