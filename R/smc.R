#' Number of doses of SMC
#'
#' @param smc_cov A single value or vector of SMC coverage.
#' @param n_rounds A vector of the number of SMC rounds per year
#' @param par_smc Population at risk within SMC-eligible age range estimates.
#'
#' @return The total number of SMC doses delivered.
#' @export
commodity_doses_smc <- function(smc_cov, n_rounds, par_smc){
  stopifnot(
    is.numeric(smc_cov),
    is.numeric(n_rounds),
    is.numeric(par_smc)
  )
  stopifnot(
    all(smc_cov >= 0 & smc_cov <= 1),
    n_rounds >= 0,
    all(par_smc >= 0)
  )
  stopifnot(
    length(smc_cov) == length(n_rounds),
    length(smc_cov) == length(par_smc)
  )

  round(smc_cov * n_rounds * par_smc)
}

#' Cost SMC
#'
#' @param n_doses Number of SMC doses. Numeric scalar or vector.
#' @param smc_cost_per_dose_delivered Cost per dose delivered. Numeric scalar or vector.
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return SMC costs
#' @export
#'
#' @references
#' \strong{smc_cost_per_dose_delivered}
#'
#' Current default is the average economic cost of administering four monthly
#'   SMC cycles of sulfadoxine–pyrimethamine plus amodiaquine for children
#'   younger than 5 years across seven countries in the Sahel subregion
#'   (Burkina Faso, Chad, Guinea, Mali, Niger, Nigeria, and The Gambia)
#'   in 2016. These include multiple delivery methods (door to door, fixed point,
#'    mobile point, mixed). The cost per dose delivered is the average cost of
#'    four monthly SMC cycles divided by the number of cycles (3.63 / 4). Costs
#'    are in 2016 USD$.
#'
#' Gilmartin et al (2021)
#'
#' \url{https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30475-7/fulltext}.
cost_smc <- function(n_doses, smc_cost_per_dose_delivered = 0.9075,
                     input_year = 2016.,
                     ...){
  check_lengths(n_doses, smc_cost_per_dose_delivered)
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(smc_cost_per_dose_delivered < 0)){
    stop("SMC cost inputs must be >= 0")
  }

  unit_cost <- inflation_adjust(smc_cost_per_dose_delivered, input_year, ...)
  cost <- n_doses * unit_cost
  return(cost)
}
