#' Cost long lasting IRS
#'
#' @param n_protected Number of people protected
#' @param cost_per_person_protected Cost per person protected
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
cost_ll_irs_person <- function(n_protected, cost_per_person_protected = 7.44){
  if(any(n_protected < 0)){
    stop("All n_protected estimates must be >= 0")
  }
  if(any(cost_per_person_protected < 0)){
    stop("Long lasting IRS cost inputs must be >= 0")
  }
  cost <- n_protected * cost_per_person_protected
  return(cost)
}

#' Cost actellic IRS
#'
#' @param n_sprayed Number of structures sprayed
#' @param cost_per_structure_sprayed Cost per structure sprayed
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
cost_ll_irs_structure <- function(n_sprayed, cost_per_structure_sprayed = 26.36){
  if(any(n_sprayed < 0)){
    stop("All n_sprayed estimates must be >= 0")
  }
  if(any(cost_per_structure_sprayed < 0)){
    stop("Long lasting IRS cost inputs must be >= 0")
  }
  cost <- n_sprayed * cost_per_structure_sprayed
  return(cost)
}
