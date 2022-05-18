#' Cost IPTi
#'
#' @param n_doses Number of IPTi doses
#' @param ipti_cost_per_dose_delivered Cost per dose delivered
#'
#' @return IPTi costs
#' @export
#'
#' @references
#' \strong{IPTi_cost_per_dose_delivered}
#'
#' Current default is the average economic cost of administering 3 rounds of IPTi
#'   (annually), of sulfadoxine–pyrimethamine in Tanzania, Ghana, Mozambique and Gabon.
#'    The cost per dose delivered is the average cost of trial results for
#'    three IPTi cycles divided by the number of cycles (0.39 / 3). Costs
#'    are in 2007 USD$.
#'
#' Conteh et al (2010) table S4
#'
#' \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010313}.
cost_ipti <- function(n_doses, ipti_cost_per_dose_delivered = 0.1298){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(ipti_cost_per_dose_delivered < 0)){
    stop("IPTi cost inputs must be >= 0")
  }

  cost <- n_doses * ipti_cost_per_dose_delivered
  return(cost)
}
