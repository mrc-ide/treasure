#' Inflate cost to a target year using SSA CPI
#'
#' Adjust a given cost from its original year to a specified target year using
#' the Consumer Price Index (CPI) values from the `cpi` dataset. Note this appoach
#' uses region median CPIs estimates. For a more correct approach at the country
#' level see \url{https://linkinghub.elsevier.com/retrieve/pii/S1098-3015(19)32149-7}
#'
#' @param cost Numeric value of the original cost.
#' @param cost_year Integer indicating the year corresponding to `cost`.
#' @param target_year Integer indicating the year to adjust the cost to.
#' @param region World region, defaults to SSA
#'
#' @return Numeric value of the inflation-adjusted cost in `target_year` dollars.
#'
#' @details
#' The `ssa_cpi` dataset must be included in the package and contain columns:
#' - `year`: Calendar year.
#' - `cpi`: CPI index value for that year.
#'
#' @examples
#' # Adjust $0.26 from 2007 to 2024
#' inflation_adjust(0.26, 2007, 2024)
#'
#' @export
inflation_adjust <- function(cost, cost_year, target_year, region = "Sub-Saharan Africa") {
  # Ensure single values
  if (length(cost) != 1 || length(cost_year) != 1 || length(target_year) != 1) {
    stop("cost, cost_year, and target_year must be single values")
  }

  # Filter CPI data for region and check years exist
  cpi_region <- cpi[cpi$region == region, ]
  if (!(cost_year %in% cpi_region$year)) {
    stop(paste("cost_year not found for region:", cost_year))
  }
  if (!(target_year %in% cpi_region$year)) {
    stop(paste("target_year not found for region:", target_year))
  }

  # Retrieve CPI values
  cpi_base   <- cpi_region$cpi[cpi_region$year == cost_year]
  cpi_target <- cpi_region$cpi[cpi_region$year == target_year]

  # Compute and return adjusted cost
  cost * (cpi_target / cpi_base)
}
