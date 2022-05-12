#' Cost RTS,S
#'
#' @param n_doses Number of RTS,S doses
#' @param rtss_cost_per_dose Cost per RTS,S dose
#' @param rtss_cosumables_cost Cost for consumables for one dose (e.g injection and reconstitution syringes, safety box etc.)
#' @param rtss_delivery_cost Cost for delivery of one dose
#'
#' @return RTS,S costs
#' @export
#'
#' @references
#' \strong{rtss_cost_per_dose}
#'
#' Current default is the middle value of a commonly used assumed range for dose
#' costs ($2, $5, $10). The true cost per does is currently unknown.
#'
#' Penny et al (2016)
#' \url{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext}.
#'
#' \strong{rtss_cosumables_cost}
#'
#' Penny et al (2016)
#' \url{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext}.
#'
#' \strong{rtss_delivery_cost}
#'
#' Current default is the recurring economic cost of delivery per dose from the MVIP.
#'
#' WHO Full Evidence Report on the RTS,S/AS01 Malaria Vaccine (2021)
#' \url{https://cdn.who.int/media/docs/default-source/immunization/mvip/full-evidence-report-on-the-rtss-as01-malaria-vaccine-for-sage-mpag-(sept2021).pdf?sfvrsn=c9737be_5}.
cost_rtss <- function(n_doses, rtss_cost_per_dose = 5, rtss_cosumables_cost = 1.52, rtss_delivery_cost = 1.62){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(rtss_cost_per_dose < 0) | any(rtss_cosumables_cost < 0) | any(rtss_delivery_cost < 0)){
    stop("RTSS cost inputs must be >= 0")
  }
  rtss_cost_per_dose_delivered <- rtss_cost_per_dose + rtss_cosumables_cost + rtss_delivery_cost
  cost <- n_doses * rtss_cost_per_dose_delivered
  return(cost)
}
