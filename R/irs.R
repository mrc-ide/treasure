#' Number of people-rounds of IRS
#'
#' @param irs_cov A single value or vector of IRS coverage.
#' @param n_rounds The number of spray rounds per year
#' @param par Population at risk estimates.
#'
#' @return The total number of person-rounds of IRS protection.
#' @export
commodity_person_rounds_irs <- function(irs_cov, n_rounds, par){
  stopifnot(
    is.numeric(irs_cov),
    is.numeric(n_rounds),
    is.numeric(par)
  )
  stopifnot(
    all(irs_cov >= 0 & irs_cov <= 1),
    n_rounds >= 0,
    all(par >= 0)
  )
  stopifnot(
    length(n_rounds) == 1,
    length(irs_cov) == length(par)
  )

  round(irs_cov * n_rounds * par)
}

#' Number of structure-rounds of IRS
#'
#' Assumes 1 structure per household
#'
#' @inherit commodity_person_rounds_irs
#' @param hh_size The average number of occupants per household
#'
#' @return The total number of structure-rounds of IRS protection.
#' @export
commodity_structure_rounds_irs <- function(irs_cov, n_rounds, par, hh_size){
  stopifnot(
    is.numeric(irs_cov),
    is.numeric(n_rounds),
    is.numeric(par),
    is.numeric(hh_size)
  )
  stopifnot(
    all(irs_cov >= 0 & irs_cov <= 1),
    n_rounds >= 0,
    all(par >= 0),
    hh_size >= 0
  )
  stopifnot(
    length(n_rounds) == 1,
    length(hh_size) == 1,
    length(irs_cov) == length(par)
  )

  round((irs_cov * n_rounds * par) / hh_size)
}

#' Cost long lasting IRS
#'
#' @param n_protected Number of people protected
#' @param cost_per_person_protected Cost per person protected
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return Long lasting IRS costs
#' @export
#'
#' @references
#' \strong{cost_per_person_protected}
#'
#' Current default is the (unweighted) average economic cost for long lasting
#' (Oganophosphate/Neonicotinoid/Neonicotinoid/pyrethroid mix) IRS
#' from PMI reports for: Uganda, Zambia, Ethiopia, Tanzania, Kenya, Mozambique,
#' Benin, Ghana, Rwanda, Madagascar, Burkina Faso, Senegal, Zimbabwe, Mali
#' Malawi and Cote d'Ivoire
#'
#' PMI IRS Country Programs: 2020, Comparative Cost Analysis, table CC2
#'
#' \url{https://www.pmi.gov/pmi-vectorlink-cost-study-report_2020_approved-june-14-2021-sxf-508/}.
cost_ll_irs_person <- function(n_protected, cost_per_person_protected = 7.44,
                               input_year = 2020,
                               ...){
  if(any(n_protected < 0)){
    stop("All n_protected estimates must be >= 0")
  }
  if(any(cost_per_person_protected < 0)){
    stop("Long lasting IRS cost inputs must be >= 0")
  }
  unit_cost <- inflation_adjust(cost_per_person_protected, input_year, ...)
  cost <- n_protected * unit_cost
  return(cost)
}

#' Cost actellic IRS
#'
#' @param n_sprayed Number of structures sprayed
#' @param cost_per_structure_sprayed Cost per structure sprayed
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return Long lasting IRS costs
#' @export
#'
#' @references
#' \strong{cost_per_structure_sprayed}
#'
#' Current default is the (unweighted) average economic cost for long lasting
#' (Oganophosphate/Neonicotinoid/Neonicotinoid/pyrethroid mix) IRS
#' from PMI reports for: Uganda, Zambia, Ethiopia, Tanzania, Kenya, Mozambique,
#' Benin, Ghana, Rwanda, Madagascar, Burkina Faso, Senegal, Zimbabwe, Mali
#' Malawi and Cote d'Ivoire
#'
#' PMI IRS Country Programs: 2020, Comparative Cost Analysis, table CC2
#'
#' \url{https://www.pmi.gov/pmi-vectorlink-cost-study-report_2020_approved-june-14-2021-sxf-508/}.
cost_ll_irs_structure <- function(n_sprayed, cost_per_structure_sprayed = 26.36,
                                  input_year = 2020,
                                  ...){
  if(any(n_sprayed < 0)){
    stop("All n_sprayed estimates must be >= 0")
  }
  if(any(cost_per_structure_sprayed < 0)){
    stop("Long lasting IRS cost inputs must be >= 0")
  }
  unit_cost <- inflation_adjust(cost_per_structure_sprayed, input_year, ...)
  cost <- n_sprayed * unit_cost
  return(cost)
}

#' Cost DDT IRS
#'
#' @param n_sprayed Number of structures sprayed
#' @param cost_per_structure_sprayed Cost per structure sprayed
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return DDT IRS costs
#' @export
#'
#' @references
#' \strong{cost_per_structure_sprayed}
#'
#' Current default is the (mid point) average cost for DDT IRS
#' from Table 3 of Walker (2008)
#'
#' \url{https://resjournals.onlinelibrary.wiley.com/doi/10.1046/j.1365-2915.2000.00262.x}.
cost_ddt_irs_structure <- function(n_sprayed, cost_per_structure_sprayed = 2.25,
                                   input_year = 1999,
                                   ...){
  if(any(n_sprayed < 0)){
    stop("All n_sprayed estimates must be >= 0")
  }
  if(any(cost_per_structure_sprayed < 0)){
    stop("DDT IRS cost inputs must be >= 0")
  }
  unit_cost <- inflation_adjust(cost_per_structure_sprayed, input_year, ...)
  cost <- n_sprayed * unit_cost
  return(cost)
}
