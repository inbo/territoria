#' Calculate the distance matrix
#' @inheritParams import_observations
#' @export
#' @importFrom assertthat assert_that is.number
#' @importFrom RSQLite dbClearResult dbListTables dbSendQuery
distance_matrix <- function(conn, max_dist = 336) {
  assert_that(inherits(conn, "SQLiteConnection"))
  assert_that(
    "observation" %in% dbListTables(conn),
    msg = "No observations found. Did you run `import_observations()`?"
  )
  assert_that(is.number(max_dist), max_dist > 0)
  sql <- "DROP TABLE IF EXISTS distance"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
  sql <- "CREATE TABLE distance (id_1 INTEGER, id_2 INTEGER, distance REAL)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
  sql <- "CREATE UNIQUE INDEX IF NOT EXISTS idx_distance_id_1_id_2
ON distance (id_1, id_2)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
  sql <- "CREATE INDEX IF NOT EXISTS idx_distance_distance
ON distance (distance)"
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
  sql <- sprintf(
    "WITH cte_obs AS (
  SELECT
    id, x, y, group_x, group_y, group_x + 1 AS group_x1, group_y + 1 AS group_y1
  FROM observation
),
cte_distance AS (
  SELECT
    c1.id AS id_1, c2.id AS id_2,
    sqrt((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y)) AS
      distance
  FROM cte_obs AS c1
  INNER JOIN cte_obs AS c2
    ON c1.group_x = c2.group_x AND c1.group_y = c2.group_y
  WHERE c1.id < c2.id
UNION ALL
  SELECT
    c1.id AS id_1, c2.id AS id_2,
    sqrt((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y)) AS
      distance
  FROM cte_obs AS c1
  INNER JOIN cte_obs AS c2
    ON c1.group_x = c2.group_x1 AND c1.group_y = c2.group_y
UNION ALL
  SELECT
    c1.id AS id_1, c2.id AS id_2,
    sqrt((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y)) AS
      distance
  FROM cte_obs AS c1
  INNER JOIN cte_obs AS c2
    ON c1.group_x = c2.group_x1 AND c1.group_y = c2.group_y1
UNION ALL
  SELECT
    c1.id AS id_1, c2.id AS id_2,
    sqrt((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y)) AS
      distance
  FROM cte_obs AS c1
  INNER JOIN cte_obs AS c2
    ON c1.group_x = c2.group_x AND c1.group_y = c2.group_y1
)

INSERT INTO distance
SELECT min(id_1, id_2) AS id_1, max(id_1, id_2) AS id_2, distance
FROM cte_distance
WHERE distance <= %1$f
ORDER BY distance
",
    max_dist * 2
  )
  res <- dbSendQuery(conn, sql)
  dbClearResult(res)
}
