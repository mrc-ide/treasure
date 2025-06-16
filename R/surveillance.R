#' Cost surveillance
#'
#' Cost of epidemiological and entomological surveillance.
#'
#' @param pop_at_risk Population at risk
#' @param cost_per_pop_at_risk Cost per population at risk
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return Surveillance costs
#' @export
#'
#' @references
#' \strong{cost_per_pop_at_risk}
#'
#' Estimate from Patouillard et al (2017), consisting of a summary from the strategic
#' plans from: Botswana, Nigeria, India, Eritrea, Swaziland, Namibia.
#'
#' \url{https://gh.bmj.com/content/2/2/e000176}.
cost_surveillance <- function(pop_at_risk, cost_per_pop_at_risk = 0.05,
                              input_year = 2017, ..){
  if(any(pop_at_risk < 0)){
    stop("All pop_at_risk estimates must be >= 0")
  }
  if(any(cost_per_pop_at_risk < 0)){
    stop("Surveillance cost inputs must be >= 0")
  }

  unit_cost <- inflation_adjust(cost_per_pop_at_risk, input_year, ...)
  cost <- pop_at_risk * unit_cost
  return(cost)
}
