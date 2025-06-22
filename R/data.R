#' WHO CHOICE inpatient/outpatient costs.
#'
#' Data from: World Health Organization.
#'   Cost effectiveness and strategic planning (WHO-CHOICE). 2021-02-12
#'
#' @format A data frame with 1816 rows and 10 variables:
#' \describe{
#'   \item{region_country}{Region or country}
#'   \item{iso3c}{Country iso3c code}
#'   \item{type}{Inpatient or outpatient}
#'   \item{level}{Health facility level}
#'   \item{model}{Model prediction}
#'   \item{sample_mean}{Mean value from sample}
#'   \item{upper_95_ui}{High estimate (95 percent uncertainty interval)}
#'   \item{lower_95_ui}{Low estimate (95 percent uncertainty interval)}
#'   \item{sd}{Standard deviation}
#'   \item{definition}{Definition}
#' }
#' @source \url{https://www.who.int/publications/m/item/who-choice-estimates-of-cost-for-inpatient-and-outpatient-health-service-delivery}
"who_choice"

#' World Bank Consumer Price Index (CPI)
#'
#' Regional CPI by year, taken as median of country-level CPI
#'
#' Data from: World Bank.
#'
#' @format A data frame with 1816 rows and 10 variables:
#' \describe{
#'   \item{year}{Year}
#'   \item{region}{Region}
#'   \item{cpi}{Consumer Price Index}
#' }
#' @source \url{https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=ZG}
"cpi"
