#' Set the global region for inflation adjustments
#'
#' @param region Target region to use when adjusting for inflation
set_region <- function(region) {
  options(treasure.region = region)
}
