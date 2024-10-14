#' Cost RDTs
#'
#' RDTs are used for diagnosis of malaria. When costing it is also common to add
#' additional costs for RDTs used to diagnose non-malaria fevers.
#'
#' @param n_tests Number of tests
#' @param rdt_unit_cost Unit cost for rapid diagnostic test
#' @param delivery_mark_up A mark up for in-country delivery to a public health facility.
#' Expressed as a proportion of the test unit cost.
#'
#' @return RDT costs
#' @export
#'
#' @references
#' \strong{rdt_unit_cost}
#'
#' Current default is the average cost for an RDT from Normal orders, Reference price per test EXW.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: RDTs, version: quarter 1, 2022
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/hiv-malaria-rapid-diagnostic-tests/}.
#'
#' \strong{delivery_mark_up}
#'
#' Assummed 15%
#'
#' Patouillard et al (2017)
#'
#' \url{https://gh.bmj.com/content/2/2/e000176}
cost_rdt <- function(n_tests, rdt_unit_cost = 0.46, delivery_mark_up = 0.15){
  if(any(n_tests < 0)){
    stop("All n_tests estimates must be >= 0")
  }
  if(any(rdt_unit_cost < 0) | any(delivery_mark_up < 0)){
    stop("RDT cost inputs must be >= 0")
  }

  cost_per_test_delivered <- rdt_unit_cost + (rdt_unit_cost * delivery_mark_up)
  cost <- n_tests * cost_per_test_delivered
  return(cost)
}

#' Cost Artemether/Lumefantrine treatment
#'
#' Note the cost per dose is for a single dose (20/120 mg). A treatment course typically
#' constitutes Artemether + lumefantrine given twice a day for 3 days following
#' weight-based guidelines:
#' \itemize{
#'  \item{5 to <15 kg:}{ 20/120 mg}
#'  \item{15 to <25 kg:}{ 40/240 mg}
#'  \item{35 to <35 kg:}{ 60/360 mg}
#'  \item{>= 35 kg:}{ 80/480 mg}
#' }
#' So course for a single adult (weighing >=35kg) may constitute
#' 3 days x 2 times daily x 4 doses (4 x 20/120mg = 80/480mg) = 24 doses.
#'
#' @param n_doses Number of tests
#' @param cost_per_dose Cost per dose is for a single dose (20/120 mg)
#'
#' @return AL costs
#' @export
#'
#' @references
#' \strong{dosing information}
#'
#' \url{https://www.who.int/publications/i/item/guidelines-for-malaria}
#'
#' \strong{cost_per_dose}
#'
#' Current default is the average cost for 20/120mg.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Antimalarial medicines, version: quarter 1, 2022
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/antimalarial-medicines/}.
cost_al <- function(n_doses, cost_per_dose = 0.30){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(cost_per_dose < 0)){
    stop("AL cost inputs must be >= 0")
  }

  cost <- n_doses * cost_per_dose
  return(cost)
}

#' Cost primaquine treatment
#'
#' Note the cost per dose is for a single dose (7.5 mg). A treatment course typically
#' constitutes primaquine 0.25 mg base/kg bw per day for 14 days#'
#' So course for a single adult (weighing 50kg) may constitute
#' 14 days x 0.25mg x 50kg (14 x 0.25 x 50 / 7.5) = 25 doses.
#'
#' @param n_doses Number of tests
#' @param cost_per_dose Cost per dose is for a single dose (7.5 mg)
#'
#' @return primaquine costs
#' @export
#'
#' @references
#' \strong{dosing information}
#'
#' \url{https://www.who.int/publications/i/item/guidelines-for-malaria}
#'
#' \strong{cost_per_dose}
#'
#' Current default is the average cost for 7.5mg.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Antimalarial medicines, version: quarter 1, 2022
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/antimalarial-medicines/}.
cost_primaquine <- function(n_doses, cost_per_dose = 0.40){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(cost_per_dose < 0)){
    stop("Primaquine cost inputs must be >= 0")
  }

  cost <- n_doses * cost_per_dose
  return(cost)
}

#' Cost per outpatient visit
#'
#' For information on country specific outpatient costs from WHO CHOICE see \code{?who_coice}
#'
#' @param n_visits Number of visits
#' @param cost_per_visit Cost per visit
#'
#' @return Outpatient costs
#' @export
cost_outpatient <- function(n_visits, cost_per_visit){
  if(any(n_visits < 0)){
    stop("All n_visits estimates must be >= 0")
  }
  if(any(cost_per_visit < 0)){
    stop("Outpatient cost inputs must be >= 0")
  }

  cost <- n_visits * cost_per_visit
  return(cost)
}

#' Cost per inpatient visit
#'
#' For information on country specific inpatient costs from WHO CHOICE see \code{?who_coice}
#'
#' @param n_visits Number of visits
#' @param cost_per_day Cost per day
#' @param average_stay_duration Average duration of stay, defaults to 3 days following Patouillard et al 2017.
#'
#' @return Inpatient costs
#' @export
cost_inpatient <- function(n_visits, cost_per_day, average_stay_duration = 3){
  if(any(n_visits < 0)){
    stop("All n_visits estimates must be >= 0")
  }
  if(any(cost_per_day < 0)){
    stop("Inpatient cost inputs must be >= 0")
  }

  cost <- n_visits * cost_per_day * average_stay_duration
  return(cost)
}

