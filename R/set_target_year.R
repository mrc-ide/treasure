#' Set the global target year for inflation adjustments
#'
#' @param year Target year to use when adjusting for inflation
set_target_year <- function(year) {
  options(treasure.target_year = year)
}
