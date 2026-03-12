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
    "SELECT user, survey, status >= %1$i AS above, COUNT(id) AS n
FROM observation
GROUP BY survey, status >= %1$i",
    status_split
  ) |>
    dbGetQuery(conn = conn) -> status_obs

  # test for each survey
  merge(
    status_obs[status_obs$above == 1, c("survey", "n")],
    status_obs[status_obs$above == 0, c("survey", "n")],
    by = "survey", all = TRUE
  ) -> status_obs_s
  status_obs_s$n.x[is.na(status_obs_s$n.x)] <- 0
  status_obs_s$n.y[is.na(status_obs_s$n.y)] <- 0
  status_obs_s[, c("n.x", "n.y")] |>
    apply(
      1, threshold = threshold,
      FUN = function(x, threshold) {
        binom.test(x = x, alternative = "greater", p = threshold)$p.value
      }
    ) -> status_obs_s$p_value
  status_obs_s <- status_obs_s[order(status_obs_s$p_value, -status_obs_s$n.y), ]
  status_obs_s$log_p <- log(1 - status_obs_s$p_value)
  c(0, diff(status_obs_s$n.x) != 0 | diff(status_obs_s$n.y) != 0) |>
    cumsum() -> status_obs_s$group
  status_group <- aggregate(log_p ~ group, data = status_obs_s, FUN = sum)
  status_group$value <- 1 - exp(cumsum(status_group$log_p))
  status_group[status_group$value < alpha, c("group", "value")] |>
    merge(status_obs_s[, c("survey", "group")], by = "group") -> unlikely_s
  unlikely_s$reason <- sprintf(
    "fraction status above or equal to %i greater than %.0f%% in the survey",
    status_split, 100 * threshold
  )
  # test for each user
  sprintf(
    "SELECT user, status >= %1$i AS above, COUNT(id) AS n
FROM observation
GROUP BY user, status >= %1$i",
    status_split
  ) |>
    dbGetQuery(conn = conn) -> status_obs_u
  merge(
    status_obs_u[status_obs_u$above == 1, c("user", "n")],
    status_obs_u[status_obs_u$above == 0, c("user", "n")],
    by = "user", all = TRUE
  ) -> status_obs_u
  status_obs_u$n.x[is.na(status_obs_u$n.x)] <- 0
  status_obs_u$n.y[is.na(status_obs_u$n.y)] <- 0
  status_obs_u[, c("n.x", "n.y")] |>
    apply(
      1, threshold = threshold,
      FUN = function(x, threshold) {
        binom.test(x = x, alternative = "greater", p = threshold)$p.value
      }
    ) -> status_obs_u$p_value
  status_obs_u <- status_obs_u[order(status_obs_u$p_value, -status_obs_u$n.y), ]
  status_obs_u$log_p <- log(1 - status_obs_u$p_value)
  c(0, diff(status_obs_u$n.x) != 0 | diff(status_obs_u$n.y) != 0) |>
    cumsum() -> status_obs_u$group
  status_group <- aggregate(log_p ~ group, data = status_obs_u, FUN = sum)
  status_group$value <- 1 - exp(cumsum(status_group$log_p))
  status_group[status_group$value < alpha, c("group", "value")] |>
    merge(status_obs_u[, c("user", "group")] |>
            merge(status_obs[, c("user", "survey")], by = "user") |>
            unique(),
          by = "group") -> unlikely_u
  unlikely_u$reason <- sprintf(
    "fraction status above or equal to %i greater than %.0f%% for the user",
    status_split, 100 * threshold
  )

  unlikely <- rbind(unlikely_s, unlikely_u[, -3])
  unlikely[, c("survey", "reason", "value")] |>
    dbWriteTable(conn = conn, name = "unlikely", append = TRUE)
  return(nrow(unlikely_s) / nrow(status_obs))
}
