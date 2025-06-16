#' Inflate cost to a target year using SSA CPI
#'
#' Adjust a given cost from its original year to a specified target year using
#' the Consumer Price Index (CPI) values from the `cpi` dataset. Note this appoach
#' uses region median CPIs estimates. For a more correct approach at the country
#' level see \url{https://linkinghub.elsevier.com/retrieve/pii/S1098-3015(19)32149-7}
#'
#' @param cost Numeric value of the original cost.
#' @param cost_year Integer indicating the year corresponding to `cost`.
#' @param target_year Integer indicating the year to adjust the cost to. If not
#'   supplied, the value of `getOption("treasure.target_year")` is used.
#' @param region World region. If not supplied, the value of
#'   `getOption("treasure.region")` is used.
#'
#' @return Numeric value of the inflation-adjusted cost in `target_year` dollars.
#'
#' @details
#' The `ssa_cpi` dataset must be included in the package and contain columns:
#' - `year`: Calendar year.
#' - `cpi`: CPI index value for that year.
#'
#' This function is vectorised over all arguments.
#'
#' @examples
#' # Adjust $0.26 from 2007 to 2024
#' inflation_adjust(0.26, 2007, 2024)
#'
#' @export
inflation_adjust <- function(cost, cost_year, target_year = NULL, region = NULL) {
  if (is.null(target_year)) {
    target_year <- getOption("treasure.target_year")
  }
  if (is.null(region)) {
    region <- getOption("treasure.region")
  }

  n <- max(length(cost), length(cost_year), length(target_year), length(region))
  cost <- rep(cost, length.out = n)
  cost_year <- rep(cost_year, length.out = n)
  target_year <- rep(target_year, length.out = n)
  region <- rep(region, length.out = n)

  mapply(function(cst, cy, ty, reg) {
    cpi_region <- cpi[cpi$region == reg, ]
    if (!(cy %in% cpi_region$year)) {
      stop(paste("cost_year not found for region:", cy))
    }
    if (!(ty %in% cpi_region$year)) {
      stop(paste("target_year not found for region:", ty))
    }
    cpi_base   <- cpi_region$cpi[cpi_region$year == cy]
    cpi_target <- cpi_region$cpi[cpi_region$year == ty]
    cst * (cpi_target / cpi_base)
  }, cost, cost_year, target_year, region, SIMPLIFY = TRUE)
}
