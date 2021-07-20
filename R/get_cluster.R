#' Get the information from the clusters
#' @inheritParams import_observations
#' @export
#' @importFrom assertthat assert_that
#' @importFrom RSQLite dbGetQuery
get_cluster <- function(conn) {
  assert_that(inherits(conn, "SQLiteConnection"))
  assert_that(
    "observation" %in% dbListTables(conn, "observation"),
    msg = "No observations found. Did you run `import_observations()`?"
  )
  obs <- dbGetQuery(
    conn, "SELECT id, x, y, survey, status, cluster FROM observation"
  )
  cluster <- dbGetQuery(conn, "
SELECT
  cluster, COUNT(cluster) AS n_obs, MAX(status) AS max_status,
  AVG(x) AS centroid_x, AVG(y) AS centroid_y
FROM observation
GROUP BY cluster")
  return(list(cluster = cluster, observations = obs))
}
