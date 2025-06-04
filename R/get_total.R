#' Get the total number of individuals per region
#' Returns the sum of `value` per region.
#' Clusters spanning more than one region get each a fraction of `value` based
#' on the number of observations of the cluster in a region divided by the total
#' number of observations in the cluster.
#' @inheritParams import_observations
#' @param value A string representing the value per cluster to be calculated.
#' Default is `"IIF(max_status >= 2, 1, iif(n_total > 1, 0.5, 0))"`.
#' Available variables are
#'  - `max_status`: the highest status of a cluster.
#'  - `n_total`: the total number of observations per cluster
#'  - `n_region`: the number of observations in a cluster from the same region.
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom RSQLite dbGetQuery
get_total <- function(
  conn, value = "IIF(max_status >= 2, 1, iif(n_total > 1, 0.5, 0))"
) {
  assert_that(
    inherits(conn, "SQLiteConnection"), is.string(value), noNA(value)
  )
  sprintf(
    "WITH cte_obs AS (
  SELECT o.region, o.cluster, o.status
  FROM observation AS o
  LEFT JOIN unlikely AS u ON o.survey = u.survey
  WHERE u.value IS NULL
),
cte_region AS (
  SELECT cluster, region, COUNT(cluster) AS n_region
  FROM cte_obs
  WHERE region IS NOT NULL
  GROUP BY cluster, region
),
cte_cluster AS (
  SELECT cluster, MAX(status) AS max_status, COUNT(status) AS n_total
  FROM cte_obs
  GROUP BY cluster
),
cte AS (
  SELECT
    r.region, c.cluster, 1.0 * r.n_region / c.n_total AS fraction,
    %s AS value
  FROM cte_region AS r
  INNER JOIN cte_cluster AS c ON r.cluster = c.cluster
)

SELECT region, SUM(value * fraction) AS individuals FROM cte GROUP BY region",
    value
  ) |>
    dbGetQuery(conn = conn)
}
