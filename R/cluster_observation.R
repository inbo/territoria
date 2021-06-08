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
  candidate_sql <- sprintf(
    "WITH cte_relevant AS (
      SELECT
        min(ob1.cluster, ob2.cluster) AS cl_1,
        max(ob1.cluster, ob2.cluster) AS cl_2, d.distance
      FROM distance AS d
      INNER JOIN observation AS ob1 ON d.id_1 = ob1.id
      INNER JOIN observation AS ob2 ON d.id_2 = ob2.id
      WHERE ob1.status >= %1$i AND ob2.status >= %1$i AND
        ob1.cluster != ob2.cluster
    )

    SELECT cl_1, cl_2, max(distance) AS max_dist
    FROM cte_relevant
    GROUP BY cl_1, cl_2
    ORDER BY max(distance)
    LIMIT 1",
    status
  )
  while (TRUE) {
    candidate <- dbGetQuery(conn, candidate_sql)
    if (nrow(candidate) == 0 || candidate$max_dist > max_dist) {
      break
    }
    sql <- sprintf(
      "UPDATE observation SET cluster = %i WHERE cluster == %i",
      candidate$cl_1, candidate$cl_2
    )
    res <- dbSendQuery(conn, sql)
    dbClearResult(res)
  }
}
