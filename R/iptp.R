#' Cost IPTp
#'
#' @param n_administrations Number of IPTp doses
#' @param iptp_cost_per_administration Cost per dose delivered
#'
#' @return IPTp costs
#' @export
#'
#' @references
#' \strong{Dosing information}
#'
#' Starting as early as possible in the second trimester, IPTp-SP is
#' recommended for all pregnant women at each scheduled antenatal care (ANC)
#' visit until the time of delivery, provided that the doses are given at least
#' one month apart.
#'
#' IPTp-SP should ideally be administered as directly observed therapy (DOT)
#' of three tablets sulfadoxine/pyrimethamine
#' (each tablet containing500 mg/25 mg SP) giving the total required dosage
#' of 1500 mg/75 mg SP.
#'
#' \url{https://www.who.int/publications-detail-redirect/WHO-HTM-GMP-2014.4}
#'
#' \strong{IPTp_cost_per_dose_delivered}
#'
#' Current default is the total cost per administration of IPTp-SP
#' (health care worker time and commodity cost) with sulfadoxine–pyrimethamine.
#' Costs are in 2012 USD$.
#'
#' Fernandes et al (2016). Table 2.
#'
#' \url{https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1539-4}
cost_iptp <- function(n_administrations, iptp_cost_per_administration = 0.79){
  if(any(n_administrations < 0)){
    stop("All n_administrations estimates must be >= 0")
  }
  if(any(iptp_cost_per_administration < 0)){
    stop("IPTp cost inputs must be >= 0")
  }

  cost <- n_administrations * iptp_cost_per_administration
  return(cost)
}
