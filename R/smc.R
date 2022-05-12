#' Cost SMC
#'
#' @param n_doses Number of SMC doses
#' @param smc_cost_per_dose_delivered Cost per dose delivered
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
cost_smc <- function(n_doses, smc_cost_per_dose_delivered = 0.9075){
  if(any(n_doses < 0)){
    stop("All n_doses estimates must be >= 0")
  }
  if(any(smc_cost_per_dose_delivered < 0)){
    stop("SMC cost inputs must be >= 0")
  }

  cost <- n_doses * smc_cost_per_dose_delivered
  return(cost)
}
