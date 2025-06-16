#' Cost pACD
#'
#' Cost for proactive case detection.
#'
#' @param n_tested Number of people tested
#' @param cost_per_person_tested Cost per person tested
#' @param input_year Year the unit costs are reported in
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return pACD costs
#' @export
#'
#' @references
#' \strong{cost_per_person_tested}
#'
#' Silumbe et al (2015)
#'
#' \url{https://malariajournal.biomedcentral.com/articles/10.1186/s12936-015-0722-3}.
cost_pacd <- function(n_tested, cost_per_person_tested = 4.79, input_year = 2015,...){
  if(any(n_tested < 0)){
    stop("All n_tested estimates must be >= 0")
  }
  if(any(cost_per_person_tested < 0)){
    stop("pACD cost inputs must be >= 0")
  }

  unit_cost <- inflation_adjust(cost_per_person_tested, input_year, ...)
  cost <- n_tested * unit_cost
  return(cost)
}

#' Cost rACD
#'
#' Cost for reactive case detection.
#'
#' @param n_tested Number of people tested
#' @param cost_per_person_tested Cost per person tested
#' @param ... Additional arguments passed to `inflation_adjust()`
#'
#' @return pACD costs
#' @export
#'
#' @references
#' \strong{cost_per_person_tested}
#'
#' Larson et al (2016)
#'
#' \url{https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1457-5}.
cost_racd <- function(n_tested, cost_per_person_tested = 38.63, input_year = 2016,...){
  if(any(n_tested < 0)){
    stop("All n_tested estimates must be >= 0")
  }
  if(any(cost_per_person_tested < 0)){
    stop("rACD cost inputs must be >= 0")
  }

  unit_cost <- inflation_adjust(cost_per_person_tested, input_year, ...)
  cost <- n_tested * unit_cost
  return(cost)
}
