#' Set the global region for inflation adjustments
#'
#' @param region Target region to use when adjusting for inflation
#' @export
set_region <- function(region) {
  options(treasure.region = region)
}

#' Set the global target year for inflation adjustments
#'
#' @param year Target year to use when adjusting for inflation
#' @export
set_target_year <- function(year) {
  options(treasure.target_year = year)
}

