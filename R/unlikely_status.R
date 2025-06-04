#' Detect surveys with an unlikely status distribution
#' @inheritParams import_observations
#' @param threshold A numeric value between 0 and 1 indicating the average
#' proportion of the observations with status above or equal to the
#' `status_split `.
#' Default of 0.5.
#' @param status_split An integer value indicating which status level splits
#' the status into two groups.
#' The default is 2.
#' @param alpha A numeric value between 0 and 1 indicating the family-wise Type
#' I error.
#' Default of 0.01.
#' @return A numeric value between 0 and 1 indicating the proportion of
#' surveys with an unlikely status distribution.
#' The list of unlikely surveys is written to the database.
#' @export
#' @importFrom assertthat assert_that is.count is.number noNA
#' @importFrom RSQLite dbGetQuery dbWriteTable
#' @importFrom stats aggregate binom.test
unlikely_status <- function(
  conn, threshold = 0.5, status_split = 2, alpha = 0.01
) {
  assert_that(
    is.number(threshold), noNA(threshold), threshold > 0,
    is.count(status_split), noNA(status_split),
    is.number(alpha), noNA(alpha), alpha > 0, alpha < 1,
    inherits(conn, "SQLiteConnection")
  )
  sprintf(
    "SELECT survey, status >= %1$i AS above, COUNT(id) AS n
FROM observation
GROUP BY survey, status >= %1$i",
    status_split
  ) |>
    dbGetQuery(conn = conn) -> status_obs
  merge(
    status_obs[status_obs$above == 1, c("survey", "n")],
    status_obs[status_obs$above == 0, c("survey", "n")],
    by = "survey", all = TRUE
  ) -> status_obs
  status_obs$n.x[is.na(status_obs$n.x)] <- 0
  status_obs$n.y[is.na(status_obs$n.y)] <- 0
  status_obs[, c("n.x", "n.y")] |>
    apply(
      1, threshold = threshold,
      FUN = function(x, threshold) {
        binom.test(x = x, alternative = "greater", p = threshold)$p.value
      }
    ) -> status_obs$p_value
  status_obs <- status_obs[order(status_obs$p_value, -status_obs$n.y), ]
  status_obs$log_p <- log(1 - status_obs$p_value)
  c(0, diff(status_obs$n.x) != 0 | diff(status_obs$n.y) != 0) |>
    cumsum() -> status_obs$group
  status_group <- aggregate(log_p ~ group, data = status_obs, FUN = sum)
  status_group$value <- 1 - exp(cumsum(status_group$log_p))
  status_group[status_group$value < alpha, c("group", "value")] |>
    merge(status_obs[, c("survey", "group")], by = "group") -> unlikely
  unlikely$reason <- sprintf(
    "fraction status above or equal to %i greather than %.0f%%", status_split,
    100 * threshold
  )
  unlikely[, c("survey", "reason", "value")] |>
    dbWriteTable(conn = conn, name = "unlikely", append = TRUE)
  return(nrow(unlikely) / nrow(status_obs))
}
