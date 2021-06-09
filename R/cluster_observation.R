#' Cluster observations
#' @inheritParams import_observations
#' @param status highest status to include while clustering the observations.
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom RSQLite dbClearResult dbGetQuery dbListTables dbSendQuery
cluster_observation <- function(conn, status, max_dist = 336) {
  assert_that(inherits(conn, "SQLiteConnection"))
  assert_that(
    "observation" %in% dbListTables(conn),
    msg = "No observations found. Did you run `import_observations()`?"
  )
  assert_that(
    "distance" %in% dbListTables(conn),
    msg = "No distance matrix found. Did you run `distance_matrix()`?"
  )
  assert_that(is.count(status), noNA(status))
  assert_that(is.number(max_dist), noNA(max_dist), max_dist > 0)
  candidate_sql <- sprintf("WITH cte_dist AS (
  SELECT
    MIN(o1.cluster, o2.cluster) AS cl1, MAX(o1.cluster, o2.cluster) AS cl2,
    iif(
      o1.survey == o2.survey OR o1.status < %1$f OR o2.status < %1$f,
      %2$f, distance
    ) AS distance
  FROM distance AS d
  INNER JOIN observation AS o1 ON d.id_1 = o1.id
  INNER JOIN observation AS o2 ON d.id_2 = o2.id
  WHERE o1.cluster != o2.cluster
)

SELECT cl1, cl2, MAX(distance) AS distance
FROM cte_dist
GROUP BY cl1, cl2
ORDER BY MAX(distance)
LIMIT 1",
    status, 3 * max_dist
  )
  while (TRUE) {
    candidate <- dbGetQuery(conn, candidate_sql)
    if (nrow(candidate) == 0 || candidate$distance > max_dist) {
      break
    }
    sql <- sprintf(
      "UPDATE observation SET cluster = %i WHERE cluster == %i",
      candidate$cl1, candidate$cl2
    )
    res <- dbSendQuery(conn, sql)
    dbClearResult(res)
  }
}
