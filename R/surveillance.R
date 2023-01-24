#' Cost surveillance
#'
#' Cost of epidemiological and entomological surveillance.
#'
#' @param pop_at_risk Population at risk
#' @param cost_per_pop_at_risk Cost per population at risk
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
cost_surveillance <- function(pop_at_risk, cost_per_pop_at_risk = 0.05){
  if(any(pop_at_risk < 0)){
    stop("All pop_at_risk estimates must be >= 0")
  }
  if(any(cost_per_pop_at_risk < 0)){
    stop("Surveillance cost inputs must be >= 0")
  }

  cost <- pop_at_risk * cost_per_pop_at_risk
  return(cost)
}
