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
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Insecticide-Treated Nets, version: quarter 1, 2022
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/}.
#'
#' \strong{llin_delivery_cost}
#'
#' Sherrard-Smith et al (2022)
#'
#' \url{https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext}.
cost_llin <- function(n_llin, llin_unit_cost = 2.52, llin_delivery_cost = 1.50) {
  if(any(n_llin < 0)){
    stop("All llin_n estimates must be >= 0")
  }
  if(any(llin_unit_cost < 0) | any(llin_delivery_cost < 0)){
    stop("Cost inputs must be >= 0")
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
#' The Global Fund Pooled Procurement Mechanism Reference Pricing: Insecticide-Treated Nets, version: quarter 1, 2022
#'
#' \url{https://www.theglobalfund.org/en/sourcing-management/health-products/long-lasting-insecticidal-nets/}.
#'
#' \strong{pbo_itn_delivery_cost}
#'
#' Sherrard-Smith et al (2022)
#'
#' \url{https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00296-5/fulltext}.
cost_pbo_itn <- function(n_pbo_itn, pbo_itn_unit_cost = 3.51, pbo_itn_delivery_cost = 1.50) {
  if(any(n_pbo_itn < 0)){
    stop("All llin_n estimates must be >= 0")
  }
  if(any(pbo_itn_unit_cost < 0) | any(pbo_itn_delivery_cost < 0)){
    stop("Cost inputs must be >= 0")
  }

  cost_per_net_delivered <- pbo_itn_unit_cost + pbo_itn_delivery_cost
  cost <- n_pbo_itn * cost_per_net_delivered
  return(cost)
}
