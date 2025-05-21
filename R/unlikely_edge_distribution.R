#' Check for unlikely distribution of Delaunay edges
#'
#' This function compares the distribution of Delaunay edges per survey in the
#' `conn` database with the distribution of Delaunay edges of all surveys in the
#' `conn_reference` database.
#' It uses a Wilcoxon test to check if the distribution of edges is
#' significantly different.
#' The function returns the proportion of surveys with an unlikely distribution
#' and writes the list of unlikely surveys to the database.
#' @inheritParams import_observations
#' @inheritParams unlikely_status
#' @param conn_reference a connection to the reference database.
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom RSQLite dbWriteTable
#' @importFrom stats wilcox.test
unlikely_edge_distribution <- function(
  conn, conn_reference, max_dist = 336, alpha = 0.01
) {
  assert_that(is.number(alpha), noNA(alpha), alpha > 0, alpha < 1)
  edges <- edge_distribution(conn = conn, max_dist = max_dist)
  reference <- edge_distribution(conn = conn_reference, max_dist = max_dist)
  survey <- unique(edges$survey)
  p <- vapply(
    survey, FUN.VALUE = numeric(1), edges = edges, reference = reference,
    FUN = function(i, edges, reference) {
      wilcox.test(
        x = edges$length[edges$survey == i], y = reference$length
      )$p.value
    }
  )
  dist_test <- data.frame(survey = survey, p_value = p)
  dist_test <- dist_test[order(dist_test$p_value), ]
  dist_test$log_p <- log(1 - dist_test$p_value)
  c(0, diff(dist_test$p_value) > 0) |>
    cumsum() -> dist_test$group
  dist_group <- aggregate(log_p ~ group, data = dist_test, FUN = sum)
  dist_group$value <- 1 - exp(cumsum(dist_group$log_p))
  dist_group[dist_group$value < alpha, c("group", "value")] |>
    merge(dist_test[, c("survey", "group")], by = "group") -> unlikely
  unlikely$reason <- "Difference in distribution of Delaunay edges"
  unlikely[, c("survey", "reason", "value")] |>
    dbWriteTable(conn = conn, name = "unlikely", append = TRUE)
  return(nrow(unlikely) / nrow(dist_test))
}
