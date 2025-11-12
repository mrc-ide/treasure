# Discount Future Values with Flexible Start Year

Applies standard discounting to future values starting from a specified
year. Values before the start year remain unchanged.

## Usage

``` r
discount_future_values(years, values, discount_rate, discount_start_year)
```

## Arguments

- years:

  Numeric vector of years/time periods

- values:

  Numeric vector of values to discount (same length as years)

- discount_rate:

  Numeric discount rate as decimal (e.g., 0.03 for 3 percent)

- discount_start_year:

  Numeric year when discounting should begin

## Value

Numeric vector of discounted values (same length as input)

## Examples

``` r
years <- 2020:2025
costs <- c(1000, 1200, 1500, 1800, 2000, 2200)
discounted <- discount_future_values(years, costs, 0.03, 2023)
```
