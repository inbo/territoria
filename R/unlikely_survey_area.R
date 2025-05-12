#' Detect surveys with an unlikely surveyed area
#'
#' First we create a Delaunay triangulation of the survey area.
#' Then we ignore all triangles with an edge larger than `max_edge`.
#' Finally we calculate the area of the remaining triangles.
#' When the sum of the area of the remaining triangles is larger than
#' `max_area`, we consider the survey area unlikely.
#' @inheritParams import_observations
#' @inheritParams sf::st_as_sf
#' @inheritParams sf::st_buffer
#' @inheritParams sf::st_concave_hull
#' @param max_area the maximum area of the concave hull.
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom dplyr group_by mutate summarise
#' @importFrom RSQLite dbGetQuery dbWriteTable
#' @importFrom rlang .data
#' @importFrom sf st_area st_as_sf st_buffer st_concave_hull st_intersection
#' st_union
#' @export
unlikely_survey_area <- function(
  conn, relevant_area, max_area = 3e6, dist = 50, crs = 31370, ratio = 0.95
) {
  assert_that(
    inherits(conn, "SQLiteConnection"), inherits(relevant_area, "sf"),
    is.number(ratio), noNA(ratio), 0 <= ratio, ratio <= 1,
    is.number(dist), noNA(dist), dist > 0,
    is.number(max_area), noNA(max_area), max_area > 0
  )
  "SELECT survey, x, y FROM observation" |>
    dbGetQuery(conn = conn) |>
    st_as_sf(coords = c("x", "y"), crs = crs) |>
    group_by(.data$survey) |>
    summarise(
      geometry = st_union(.data$geometry) |>
        st_buffer(dist = dist) |>
        st_concave_hull(ratio = ratio),
      .groups = "drop"
    ) |>
    st_intersection(relevant_area) |>
    group_by(.data$survey) |>
    summarise(geometry = st_union(.data$geometry), .groups = "drop") |>
    mutate(
      area = st_area(.data$geometry) |>
        as.numeric()
    ) -> survey
  survey |>
    st_drop_geometry() |>
    filter(.data$area >= max_area) |>
    transmute(
      .data$survey, value = .data$area,
      reason = sprintf(
        "Area of relevant concave hull larger than %.0f ha", max_area / 1e4
      )
    ) -> unlikely
  dbWriteTable(conn = conn, name = "unlikely", value = unlikely, append = TRUE)
  return(nrow(unlikely) / nrow(survey))
}
