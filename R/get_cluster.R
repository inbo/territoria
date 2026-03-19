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
  "SELECT
o.id, o.x, o.y, o.survey, o.status, o.cluster
FROM observation AS o
LEFT JOIN unlikely AS u ON o.survey = u.survey
WHERE u.value IS NULL" |>
    dbGetQuery(conn = conn) -> obs
  cluster <- dbGetQuery(conn, "
SELECT
  o.cluster, COUNT(o.cluster) AS n_obs, MAX(status) AS max_status,
  AVG(o.x) AS centroid_x, AVG(o.y) AS centroid_y
FROM observation AS o
LEFT JOIN unlikely AS u ON o.survey = u.survey
WHERE u.value IS NULL
GROUP BY o.cluster")
  return(list(cluster = cluster, observations = obs))
}
