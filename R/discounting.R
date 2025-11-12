#' Discount Future Values with Flexible Start Year
#'
#' Applies standard discounting to future values starting from a specified year.
#' Values before the start year remain unchanged.
#'
#' @param years Numeric vector of years/time periods
#' @param values Numeric vector of values to discount (same length as years)
#' @param discount_rate Numeric discount rate as decimal (e.g., 0.03 for 3 percent)
#' @param discount_start_year Numeric year when discounting should begin
#' @return Numeric vector of discounted values (same length as input)
#' @examples
#' years <- 2020:2025
#' costs <- c(1000, 1200, 1500, 1800, 2000, 2200)
#' discounted <- discount_future_values(years, costs, 0.03, 2023)
#' @export
discount_future_values <- function(
    years,
    values,
    discount_rate,
    discount_start_year
) {

  # Input validation
  if (length(years) != length(values)) {
    stop("'years' and 'values' must have the same length")
  }

  if (discount_rate < 0 || discount_rate > 1) {
    stop("'discount_rate' should be between 0 and 1 (e.g., 0.03 for 3%)")
  }

  if (!is.numeric(years) || !is.numeric(values) || !is.numeric(discount_rate) || !is.numeric(discount_start_year)) {
    stop("All inputs must be numeric")
  }

  # Initialize discounted values as copy of original values
  discounted_values <- values

  # Apply discounting only to years >= discount_start_year
  years_to_discount <- years >= discount_start_year

  if (any(years_to_discount)) {
    # Calculate years since discounting started
    years_since_start <- years[years_to_discount] - discount_start_year

    # Apply standard discounting formula: PV = FV / (1 + r)^t
    discount_factors <- 1 / (1 + discount_rate)^years_since_start

    # Apply discounting
    discounted_values[years_to_discount] <- values[years_to_discount] * discount_factors
  }

  return(discounted_values)
}
