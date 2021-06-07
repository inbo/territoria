#' Connect to a database
#' @param db The file name of the database
#' @export
#' @importFrom RSQLite dbConnect SQLite
connect_db <- function(db = tempfile("territoria", fileext = ".sqlite")) {
  assert_that(is.string(db))
  dbConnect(SQLite(), dbname = db)
}
