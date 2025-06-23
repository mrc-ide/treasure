#' Number of doses of PMC
#'
#' @param pmc_cov A single value or vector of pmc coverage.
#' @param n_rounds The number of pmc rounds per year
#' @param par_pmc Population at risk within pmc-eligible age range estimates.
#'
#' @return The total number of pmc doses delivered.
#' @export
commodity_doses_pmc <- function(pmc_cov, par_pmc, n_rounds = 3){
  stopifnot(
    is.numeric(pmc_cov),
    is.numeric(par_pmc),
    is.numeric(n_rounds)
  )
  stopifnot(
    all(pmc_cov >= 0 & pmc_cov <= 1),
    n_rounds >= 0,
    all(par_pmc >= 0)
  )
  stopifnot(
    length(n_rounds) == 1,
    length(pmc_cov) == length(par_pmc)
  )

  round(pmc_cov * n_rounds * par_pmc)
}

#' Cost PMC
#'
#' @param n_doses Number of PMC doses. Numeric scalar or vector.
#' @param pmc_cost_per_dose_delivered Cost per dose delivered. Numeric scalar or vector.
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
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
#'    are in 2007 USD$. Cost have been inflated to adjust for a roughly 3 fold
#'    increase in SP costs (GF price reference data)
#'
#' Conteh et al (2010) table S4
#'
#' \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0010313}.
cost_pmc <- function(n_doses, pmc_cost_per_dose_delivered = 0.3894,
                     input_year = 2007,
                     ...){
  check_lengths(n_doses, pmc_cost_per_dose_delivered)
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(pmc_cost_per_dose_delivered < 0)){
    stop("PMC cost inputs must be >= 0")
  }

  unit_cost <- inflation_adjust(pmc_cost_per_dose_delivered, input_year, ...)
  cost <- n_doses * unit_cost
  return(cost)
}
