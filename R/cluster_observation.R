#' Cluster observations
#' @inheritParams import_observations
#' @param status highest status to include while clustering the observations.
#' @param reset reset the current clustering.
#'   Defaults to `FALSE`
#' @export
#' @importFrom assertthat assert_that is.flag is.number noNA
#' @importFrom RSQLite dbClearResult dbGetQuery dbListTables dbSendQuery
cluster_observation <- function(conn, status, max_dist = 336, reset = FALSE) {
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
  assert_that(is.flag(reset), noNA(reset))

  if (reset) {
    res <- dbSendQuery(conn, "UPDATE observation SET cluster = id")
    dbClearResult(res)
  }

  candidate_sql <- sprintf("WITH cte_base AS (
  SELECT
    MIN(o1.cluster, o2.cluster) AS cl1, MAX(o1.cluster, o2.cluster) AS cl2,
    d.distance, o1.survey = o2.survey AS same_survey
  FROM distance AS d
  INNER JOIN observation AS o1 ON d.id_1 = o1.id
  INNER JOIN observation AS o2 ON d.id_2 = o2.id
  WHERE o1.status >= %1$i AND o2.status >= %1$i AND o1.cluster != o2.cluster
),
cte_different_survey AS (
  SELECT cl1, cl2 FROM cte_base GROUP BY cl1, cl2 HAVING MAX(same_survey) = 0
),
cte_long_list AS (
  SELECT b.cl1, b.cl2, MAX(b.distance) AS distance
  FROM cte_base AS b
  INNER JOIN cte_different_survey AS d ON b.cl1 = d.cl1 AND b.cl2 = d.cl2
  GROUP BY b.cl1, b.cl2
  HAVING MAX(b.distance) < %2$f
),
cte_group AS (
  SELECT
    c.cl1, c.cl2, c.distance,
    MIN(o1.group_x, o2.group_x) AS gx1, MAX(o1.group_x, o2.group_x) AS gx2,
    MIN(o1.group_y, o2.group_y) AS gy1, MAX(o1.group_y, o2.group_y) AS gy2
  FROM cte_long_list AS c
  INNER JOIN observation AS o1 ON c.cl1 = o1.id
  INNER JOIN observation AS o2 ON c.cl2 = o2.id
)

SELECT
  cl1, cl2, MAX(distance) AS distance, MIN(gx1) AS gx1, MAX(gx2) AS gx2,
  MIN(gy1) AS gy1, MAX(gy2) AS gy2
FROM cte_group
GROUP BY cl1, cl2
ORDER BY MAX(distance)", status, max_dist)
  while (TRUE) {
    message(".", appendLF = FALSE)
    candidate <- dbGetQuery(conn, candidate_sql)
    if (nrow(candidate) == 0 || candidate$distance > max_dist) {
      break
    }
    i <- 1
    while (i < nrow(candidate)) {
      overlapping <- which(
        (
          abs(candidate$gx1 - candidate$gx1[i]) <= 1 &
            abs(candidate$gy1 - candidate$gy1[i]) <= 1
        ) |
          (
            abs(candidate$gx2 - candidate$gx1[i]) <= 1 &
              abs(candidate$gy2 - candidate$gy1[i]) <= 1
          ) |
          (
            abs(candidate$gx1 - candidate$gx2[i]) <= 1 &
              abs(candidate$gy1 - candidate$gy2[i]) <= 1
          ) |
          (
            abs(candidate$gx2 - candidate$gx2[i]) <= 1 &
              abs(candidate$gy2 - candidate$gy2[i]) <= 1
          ) |
          (candidate$cl1 == candidate$cl1[i]) |
          (candidate$cl1 == candidate$cl2[i]) |
          (candidate$cl2 == candidate$cl1[i]) |
          (candidate$cl2 == candidate$cl2[i])
      )
      overlapping <- overlapping[overlapping > i]
      if (length(overlapping)) {
        candidate <- candidate[-overlapping, ]
      }
      i <- i + 1
    }
    dbWriteTable(
      conn = conn, name = "new_cluster", value = candidate, temporary = TRUE,
      overwrite = TRUE
    )
    sql <- "UPDATE observation
    SET cluster = (SELECT cl1 FROM new_cluster WHERE cl2 = cluster)
    WHERE cluster IN (SELECT cl2 FROM new_cluster WHERE cl2 = cluster)"
    res <- dbSendQuery(conn, sql)
    dbClearResult(res)
  }
  return(invisible(NULL))
}
