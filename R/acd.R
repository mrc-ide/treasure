#' Cost pACD
#'
#' Cost for proactive case detection.
#'
#' @param n_tested Number of people tested
#' @param cost_per_person_tested Cost per person tested
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
cost_pacd <- function(n_tested, cost_per_person_tested = 4.79){
  if(any(n_tested < 0)){
    stop("All n_tested estimates must be >= 0")
  }
  if(any(cost_per_person_tested < 0)){
    stop("pACD cost inputs must be >= 0")
  }

  cost <- n_tested * cost_per_person_tested
  return(cost)
}

#' Cost rACD
#'
#' Cost for reactive case detection.
#'
#' @param n_tested Number of people tested
#' @param cost_per_person_tested Cost per person tested
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
cost_racd <- function(n_tested, cost_per_person_tested = 38.63){
  if(any(n_tested < 0)){
    stop("All n_tested estimates must be >= 0")
  }
  if(any(cost_per_person_tested < 0)){
    stop("rACD cost inputs must be >= 0")
  }

  cost <- n_tested * cost_per_person_tested
  return(cost)
}
