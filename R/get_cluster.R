#' Get the information from the clusters
#' @inheritParams import_observations
#' @export
#' @importFrom RSQLite dbGetQuery
get_cluster <- function(conn) {
  obs <- dbGetQuery(
    conn, "SELECT x, y, survey, status, cluster FROM observation"
  )
  cluster <- dbGetQuery(conn, "
SELECT
  cluster, COUNT(cluster) AS n_obs, MAX(status) AS max_status,
  AVG(x) AS centroid_x, AVG(y) AS centroid_y
FROM observation
GROUP BY cluster")
  return(list(cluster = cluster, observations = obs))
}
