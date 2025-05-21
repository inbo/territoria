#' Get an overview of the unlikely reasons for each survey and region
#' @inheritParams import_observations
#' @export
#' @importFrom assertthat assert_that
#' @importFrom RSQLite dbGetQuery
get_unlikely_summary <- function(conn = conn) {
  assert_that(inherits(conn, "SQLiteConnection"))
  "WITH cte AS (
  SELECT survey, region, COUNT(id) n
  FROM observation
  WHERE region IS NOT NULL
  GROUP BY survey, region
),
cte_survey AS (
  SELECT survey, region FROM cte GROUP BY survey HAVING n = MAX(n)
)
SELECT c.region, c.survey, u.reason
FROM cte_survey AS c
INNER JOIN unlikely AS u ON c.survey = u.survey
ORDER BY c.region, c.survey" |>
    dbGetQuery(conn = conn)
}
