#' Estimate the number of bed nets required to match usage target
#'
#' @param usage A single value or vector of desired target usages to model.
#' @param use_rate A single value or vector of usage rates.
#' @param distribution_timesteps Timesteps of distributions (days). By default,
#' we can assume that net distributions happen on the first day of each year.
#' For example c(1, 366)
#' @param crop_timesteps Timesteps of crop estimates (days). If assuming distribtions
#' occur on the first day of each year, a reasonable assumption would be that the
#' crop (and therefore corresponding usage) estimates were taken at the mid-point of each year.
#' For example c(1, 366) + 183.
#' @param half_life Country-specific half-life of nets in days.
#' @param par Population at risk estimates.
#' @param ... additional arguments for the crop_to_distribution function in netz
#'
#' @return Number of nets
#'
#' @references
#' Uses a version of the net stock and flow model as described by:
#' Bertozzi-Villa, Amelia, et al. Nature communications 12.1 (2021): 1-12.
#' @export
commodity_nets <- function(usage, use_rate, distribution_timesteps, crop_timesteps, half_life, par, ...){
  stopifnot(
    is.numeric(usage),
    is.numeric(use_rate),
    is.numeric(distribution_timesteps),
    is.numeric(crop_timesteps),
    is.numeric(half_life),
    is.numeric(par)
  )
  stopifnot(
    all(usage >= 0 & usage <= 1),
    all(use_rate >= 0 & use_rate <= 1),
    all(distribution_timesteps >= 0),
    all(crop_timesteps >= 0),
    half_life >= 0,
    all(par >= 0)
  )
  stopifnot(
    length(half_life) == 1,
    length(usage) == length(par)
  )

  access <- netz::usage_to_access(usage = usage, use_rate = use_rate)
  crop <- netz::access_to_crop(access = access)
  dist <- netz::crop_to_distribution(
    crop = crop,
    crop_timesteps = crop_timesteps,
    distribution_timesteps = distribution_timesteps,
    half_life = half_life,
    ...
  )
  n_nets <- round(dist * par)
  return(n_nets)
}

#' Cost standard LLINS
#'
#' @param n_llin Number of standard LLIN bed nets
#' @param llin_unit_cost Commodity unit cost per standard LLIN bed net.
#' @param llin_delivery_cost Cost to deliver one standard LLIN bet net.
#'
#' @return LLIN costs
#' @export
#'
#' @references
#' \strong{llin_unit_cost}
#'
#' Current default is the average cost for a pyrethroid-only ITN, including,
#'   hooks, strings, bag and customisation.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Insecticide-Treated Nets, accessed 16-12-2024
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/}.
#'
#' \strong{llin_delivery_cost}
#'
#' Sherrard-Smith et al (2022)
#'
#' \url{https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext}.
cost_llin <- function(n_llin, llin_unit_cost = 2.02, llin_delivery_cost = 1.50) {
  if(any(n_llin < 0)){
    stop("All llin_n estimates must be >= 0")
  }
  if(any(llin_unit_cost < 0) | any(llin_delivery_cost < 0)){
    stop("LLIN cost inputs must be >= 0")
  }

  cost_per_net_delivered <- llin_unit_cost + llin_delivery_cost
  cost <- n_llin * cost_per_net_delivered
  return(cost)
}

#' Cost pyrethroid-PBO ITN
#'
#' @param n_pbo_itn Number of pyrethroid-PBO bed nets
#' @param pbo_itn_unit_cost Commodity unit cost per pyrethroid-PBO ITN bed net.
#' @param pbo_itn_delivery_cost Cost to deliver one pyrethroid-PBO ITN bet net.
#'
#' @return LLIN costs
#' @export
#' @references
#' \strong{pbo_itn_unit_cost}
#'
#' Current default is the average cost for a pyrethroid-PBO ITN, including,
#'   hooks, strings, bag and customisation.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Insecticide-Treated Nets, accessed 16-12-2024
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/}.
#'
#' \strong{pbo_itn_delivery_cost}
#'
#' Sherrard-Smith et al (2022)
#'
#' \url{https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext}.
cost_pbo_itn <- function(n_pbo_itn, pbo_itn_unit_cost = 2.63, pbo_itn_delivery_cost = 1.50) {
  if(any(n_pbo_itn < 0)){
    stop("All llin_n estimates must be >= 0")
  }
  if(any(pbo_itn_unit_cost < 0) | any(pbo_itn_delivery_cost < 0)){
    stop("PBO cost inputs must be >= 0")
  }

  cost_per_net_delivered <- pbo_itn_unit_cost + pbo_itn_delivery_cost
  cost <- n_pbo_itn * cost_per_net_delivered
  return(cost)
}

#' Cost pyrethroid-chlorfenapyr (dual ai) ITN
#'
#' @param n_dualai_itn Number of pyrethroid-chlorfenapyr bed nets
#' @param dualai_itn_unit_cost Commodity unit cost per pyrethroid-chlorfenapyr ITN bed net.
#' @param dualai_itn_delivery_cost Cost to deliver one pyrethroid-chlorfenapyr ITN bet net.
#'
#' @return LLIN costs
#' @export
#' @references
#' \strong{dualai_itn_unit_cost}
#'
#' Current default is the average cost for a pyrethroid-chlorfenapyr ITN, including,
#'   hooks, strings, bag and customisation.
#'
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Insecticide-Treated Nets, accessed 16-12-2024
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/}.
#'
#' \strong{dualai_itn_delivery_cost}
#'
#' Sherrard-Smith et al (2022)
#'
#' \url{https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext}.
cost_dualai_itn <- function(n_dualai_itn, dualai_itn_unit_cost = 2.70, dualai_itn_delivery_cost = 1.50) {
  if(any(n_dualai_itn < 0)){
    stop("All llin_n estimates must be >= 0")
  }
  if(any(dualai_itn_unit_cost < 0) | any(dualai_itn_delivery_cost < 0)){
    stop("Dual ai cost inputs must be >= 0")
  }

  cost_per_net_delivered <- dualai_itn_unit_cost + dualai_itn_delivery_cost
  cost <- n_dualai_itn * cost_per_net_delivered
  return(cost)
}

