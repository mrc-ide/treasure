#' Cost PMC
#'
#' @param n_doses Number of PMC doses
#' @param pmc_cost_per_dose_delivered Cost per dose delivered
#'
#' @return PMC costs
#' @export
#'
#' @references
#' \strong{PMC_cost_per_dose_delivered}
#'
#' Current default is the average economic cost of administering 3 rounds of PMC
#'   (annually), of sulfadoxine–pyrimethamine in Tanzania, Ghana, Mozambique and Gabon.
#'    The cost per dose delivered is the average cost of trial results for
#'    three PMC cycles divided by the number of cycles (0.39 / 3). Costs
#'    are in 2007 USD$. Cost have been inclfated to adjust for a roughly 3 fold
#'    increase in SP costs (GF price reference data)
#'
#' Conteh et al (2010) table S4
#'
#' \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010313}.
cost_pmc <- function(n_doses, pmc_cost_per_dose_delivered = 0.3894){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(pmc_cost_per_dose_delivered < 0)){
    stop("PMC cost inputs must be >= 0")
  }

  cost <- n_doses * pmc_cost_per_dose_delivered
  return(cost)
}
