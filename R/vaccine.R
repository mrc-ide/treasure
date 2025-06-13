#' Number of vaccine doses delivered
#'
#' Note, if duration for full course (primary + boosters) is >1 year, costs for
#' the full course will be all assigned to the year of the primary series.
#'
#' @param vaccine_cov A single value or vector of vaccine coverage.
#' @param par_vaccine Population at risk within vaccine-eligible age range estimates.
#' @param n_dose_primary_series Number of doses in the primary series
#' @param booster_coverage_downscale Drop off in coverage between primary series and
#' boosters such that `booster coverage = vaccine_cov * booster_coverage_downscale`
#' @param n_boosters Number of booster doses
#'
#' @return The total number of vaccine doses delivered.
#' @export
commodity_doses_vaccine <- function(vaccine_cov, par_vaccine, n_dose_primary_series = 3, booster_coverage_downscale = 0.8, n_boosters = 1){
  stopifnot(
    is.numeric(vaccine_cov),
    is.numeric(par_vaccine),
    is.numeric(n_dose_primary_series),
    is.numeric(booster_coverage_downscale),
    is.numeric(n_boosters)
  )
  stopifnot(
    all(vaccine_cov >= 0 & vaccine_cov <= 1),
    all(par_vaccine >= 0),
    n_dose_primary_series >= 0,
    all(booster_coverage_downscale >= 0 & booster_coverage_downscale <= 1),
    n_boosters >= 0
  )
  stopifnot(
    length(n_dose_primary_series) == 1,
    length(booster_coverage_downscale) == 1,
    length(n_boosters) == 1,
    length(vaccine_cov) == length(par_vaccine)
  )

  n_vaccine <- vaccine_cov * par_vaccine
  n_doses_rtss <- round((n_vaccine * n_dose_primary_series) + (n_vaccine * booster_coverage_downscale * n_boosters))
  return(n_doses_rtss)
}

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
