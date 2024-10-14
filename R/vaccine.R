#' Cost RTS,S
#'
#' @param n_doses Number of RTS,S doses
#' @param rtss_cost_per_dose Cost per RTS,S dose
#' @param rtss_consumables_cost Cost for consumables for one dose (e.g injection and reconstitution syringes, safety box etc.)
#' @param rtss_delivery_cost Cost for delivery of one dose
#'
#' @return RTS,S costs
#' @export
#'
#' @references
#' \strong{rtss_cost_per_dose}
#'
#' Current default is based on the EUR9.30 per dose quoted in
#' \url{https://www.unicef.org/supply/media/19456/file/Malaria\%20-\%20Vaccine\%20-\%20QA\%20-\%20October\%202023\%20-\%20English\%20.pdf}
#'
cost_rtss <- function(n_doses, rtss_cost_per_dose = 10.02, rtss_consumables_cost = 1.52, rtss_delivery_cost = 1.48){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(rtss_cost_per_dose < 0) | any(rtss_consumables_cost < 0) | any(rtss_delivery_cost < 0)){
    stop("RTSS cost inputs must be >= 0")
  }
  rtss_cost_per_dose_delivered <- rtss_cost_per_dose + rtss_consumables_cost + rtss_delivery_cost
  cost <- n_doses * rtss_cost_per_dose_delivered
  return(cost)
}

#' Cost R21
#'
#' @param n_doses Number of R21 doses
#' @param r21_cost_per_dose Cost per R21 dose
#' @param r21_consumables_cost Cost for consumables for one dose (e.g injection and reconstitution syringes, safety box etc.)
#' @param r21_delivery_cost Cost for delivery of one dose
#'
#' @return R21 costs
#' @export
#'
#' @references
#' \strong{r21_cost_per_dose}
#'
#' Current default is based on the $4 per dose as used in
#' \url{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(23)00816-2/fulltext}
#'
#' Penny et al (2016)
#' \url{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext}.
#'
#' \strong{r21_consumables_cost}
#'
#' Penny et al (2016)
#' \url{https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(15)00725-4/fulltext}.
#'
#' \strong{r21_delivery_cost}
#'
#' Following methodology summarising available information from trials and MVIP in SI of
#' \url{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(23)00816-2/fulltext}
#'
#' Delivery cost per dose:
#' Age-based: $1.48 (default)
#' Seasonal: $3.75
#' Hybrid: $2.36
cost_r21 <- function(n_doses, r21_cost_per_dose = 4, r21_consumables_cost = 1.52, r21_delivery_cost = 1.48){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(r21_cost_per_dose < 0) | any(r21_consumables_cost < 0) | any(r21_delivery_cost < 0)){
    stop("R21 cost inputs must be >= 0")
  }
  r21_cost_per_dose_delivered <- r21_cost_per_dose + r21_consumables_cost + r21_delivery_cost
  cost <- n_doses * r21_cost_per_dose_delivered
  return(cost)
}
