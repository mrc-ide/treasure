utils::globalVariables("cpi")

#' Inflate cost to a target year using SSA CPI
#'
#' Adjust a given cost from its original year to a specified target year using
#' the Consumer Price Index (CPI) values from the `cpi` dataset. Note this approach
#' uses region median CPI estimates. For a more correct approach at the country
#' level see \url{https://linkinghub.elsevier.com/retrieve/pii/S1098-3015(19)32149-7}
#'
#' @param cost Numeric value of the original cost.
#' @param cost_year Integer indicating the year corresponding to `cost`.
#' @param target_year Integer indicating the year to adjust the cost to. If not
#'   supplied, the value of `getOption("treasure.target_year")` is used.
#' @param region World region. If not supplied, the value of
#'   `getOption("treasure.region")` is used.
#' @param adjust Logical indicator if inflation adjustment should be made. Default is true.
#'
#' @return Numeric value of the inflation-adjusted cost in `target_year` dollars.
#'
#' @details
#' The `cpi` dataset must be included in the package and contain columns:
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
inflation_adjust <- function(cost, cost_year, target_year = NULL, region = "Sub-Saharan Africa", adjust = TRUE) {

  # In the unlikely event no inflation adjustment needed
  if(!adjust){
    return(cost)
  }
  # If the user does not override arguments we take package default parameters for adjustment
  if (is.null(target_year)) {
    target_year <- getOption("treasure.target_year", default = 2024)
  }
  cpi_region <- cpi[cpi$region == region, ]
  cpi_base   <- cpi_region$cpi[cpi_region$year == cost_year]
  cpi_target <- cpi_region$cpi[cpi_region$year == target_year]

  cost * (cpi_target / cpi_base)
}


#' Set the global target year for inflation adjustments
#'
#' Note, price year adjustment refers to the process of bringing all costs to a
#' common baseline year to allow for valid comparisons. This involves adjusting
#' for past inflation using historical consumer price indices or deflators.
#' For example, a cost reported in 2017 USD might be inflated to 2024 USD using
#' appropriate indices. This is supported by the package and ensures consistency
#' across input data sources. Future inflation assumptions, by contrast,
#' involve projecting current prices into future years based on assumed rates of
#' inflation (e.g. 2% annually). This is typically used when estimating nominal
#' future costs or budgets over time. This package does not support forward-looking
#' inflation projections; users wishing to apply such assumptions should do so
#' outside the package workflow.
#'
#' @param year Target year to use when adjusting for inflation
#' @export
set_target_year <- function(year) {
  options(treasure.target_year = year)
}
