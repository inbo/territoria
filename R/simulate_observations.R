#' Simulate a dataset in a square area
#' @param density Density as the number of territoria per m².
#' @param area Area in which to simulate territoria in m².
#' @param gamma interaction parameter of the Strauss process.
#' See `gamma` in `spatstat.core::rStrauss()`.
#' @param n_survey Number of surveys.
#' @param p_detection Probability of detection within a survey.
#' @param status_distribution a weighting vector for statuses.
#' The order of the vector is the number of the status.
#' @inheritParams import_observations
#' @export
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom mvtnorm rmvnorm
#' @importFrom spatstat.core rStrauss
#' @importFrom spatstat.geom owin
#' @importFrom stats rbinom
simulate_observations <- function(
  density = 10e-6, area = 4e6, gamma = 0.5, max_dist = 336, n_survey = 4,
  p_detection = 0.6, status_distribution = c(0.2, 0.5, 0.3)
) {
  assert_that(
    is.number(density), is.number(area), is.count(n_survey),
    is.number(p_detection), is.numeric(status_distribution)
  )
  assert_that(
    density > 0, area > 0, p_detection > 0, p_detection <= 1,
    length(status_distribution) >= 1, all(status_distribution > 0)
  )
  xrange <- c(0, sqrt(area))
  window <- owin(xrange = xrange, yrange = xrange)
  territoria <- rStrauss(
    beta = density, gamma = gamma, R = max_dist / 2, W = window
  )
  centroids <- data.frame(x = territoria$x, y = territoria$y)
  observations <- do.call("rbind", rep(list(centroids), n_survey)) +
    rmvnorm(territoria$n * n_survey, sigma = diag(2) * max_dist ^ 2 * 0.0425)
  observations$survey <- rep(seq_len(n_survey), each = territoria$n)
  observations$status <- sample(
    seq_along(status_distribution), size = nrow(observations), replace = TRUE,
    prob = status_distribution
  )
  observations$observed <- rbinom(
    nrow(observations), size = 1, prob = p_detection
  ) == 1
  observations$id <- seq_along(observations$x)
  return(list(observations = observations, centroids = centroids))
}
