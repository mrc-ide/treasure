# Test suite for discount_future_values function
# Basic functionality tests
test_that("basic discounting works correctly", {
  years <- c(2020, 2021, 2022, 2023)
  values <- c(100, 100, 100, 100)
  discount_rate <- 0.03
  start_year <- 2021

  result <- discount_future_values(years, values, discount_rate, start_year)

  # First year should be unchanged
  expect_equal(result[1], 100)

  # Second year should be unchanged (start year)
  expect_equal(result[2], 100)

  # Third year should be discounted by 1 year
  expect_equal(result[3], 100 / (1.03^1), tolerance = 1e-6)

  # Fourth year should be discounted by 2 years
  expect_equal(result[4], 100 / (1.03^2), tolerance = 1e-6)
})

test_that("no discounting before start year", {
  years <- c(2020, 2021, 2022, 2023, 2024)
  values <- c(100, 200, 300, 400, 500)
  discount_rate <- 0.05
  start_year <- 2023

  result <- discount_future_values(years, values, discount_rate, start_year)

  # Values before start year should be unchanged
  expect_equal(result[1:3], values[1:3])

  # Values from start year onwards should be discounted
  expect_equal(result[4], 400)  # Start year, no discounting
  expect_equal(result[5], 500 / 1.05, tolerance = 1e-6)
})

test_that("all years before start year remain unchanged", {
  years <- 2015:2020
  values <- c(50, 75, 100, 125, 150, 175)
  discount_rate <- 0.03
  start_year <- 2025  # All years before start

  result <- discount_future_values(years, values, discount_rate, start_year)

  expect_equal(result, values)
})

test_that("all years from start year get discounted", {
  years <- 2020:2023
  values <- c(100, 100, 100, 100)
  discount_rate <- 0.04
  start_year <- 2020  # All years from start

  result <- discount_future_values(years, values, discount_rate, start_year)

  expected <- c(
    100,                    # Year 0
    100 / 1.04,            # Year 1
    100 / (1.04^2),        # Year 2
    100 / (1.04^3)         # Year 3
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

# Edge cases
test_that("single value works", {
  result <- discount_future_values(2023, 100, 0.03, 2023)
  expect_equal(result, 100)

  result2 <- discount_future_values(2024, 100, 0.03, 2023)
  expect_equal(result2, 100 / 1.03, tolerance = 1e-6)
})

test_that("zero discount rate works", {
  years <- 2020:2023
  values <- c(100, 200, 300, 400)

  result <- discount_future_values(years, values, 0, 2021)

  expect_equal(result, values)
})

test_that("non-integer years work", {
  years <- c(2020.5, 2021.5, 2022.5)
  values <- c(100, 100, 100)
  discount_rate <- 0.02
  start_year <- 2021

  result <- discount_future_values(years, values, discount_rate, start_year)

  expect_equal(result[1], 100)  # Before start
  expect_equal(result[2], 100 / (1.02^0.5), tolerance = 1e-6)
  expect_equal(result[3], 100 / (1.02^1.5), tolerance = 1e-6)
})

# Input validation tests
test_that("input validation works", {
  years <- c(2020, 2021, 2022)
  values <- c(100, 200, 300)

  # Mismatched lengths
  expect_error(
    discount_future_values(c(2020, 2021), values, 0.03, 2021),
    "'years' and 'values' must have the same length"
  )

  # Invalid discount rate - negative
  expect_error(
    discount_future_values(years, values, -0.01, 2021),
    "'discount_rate' should be between 0 and 1"
  )

  # Invalid discount rate - too high
  expect_error(
    discount_future_values(years, values, 1.5, 2021),
    "'discount_rate' should be between 0 and 1"
  )

  # Non-numeric inputs
  expect_error(
    discount_future_values(c("2020", "2021"), values[1:2], 0.03, 2021),
    "All inputs must be numeric"
  )

  expect_error(
    discount_future_values(years, c("100", "200", "300"), 0.03, 2021),
    "All inputs must be numeric"
  )

  expect_error(
    discount_future_values(years, values, "0.03", 2021),
    "All inputs must be numeric"
  )

  expect_error(
    discount_future_values(years, values, 0.03, "2021"),
    "All inputs must be numeric"
  )
})

# Mathematical accuracy tests
test_that("mathematical accuracy", {
  # Test with known values
  years <- c(2020, 2021, 2022)
  values <- c(1000, 1000, 1000)
  discount_rate <- 0.05
  start_year <- 2020

  result <- discount_future_values(years, values, discount_rate, start_year)

  expect_equal(result[1], 1000, tolerance = 1e-10)
  expect_equal(result[2], 1000/1.05, tolerance = 1e-10)
  expect_equal(result[3], 1000/(1.05^2), tolerance = 1e-10)
})

test_that("return type and length", {
  years <- 2020:2025
  values <- rep(100, 6)

  result <- discount_future_values(years, values, 0.03, 2022)

  expect_type(result, "double")
  expect_length(result, length(values))
  expect_equal(length(result), length(years))
})

# Realistic health economics scenario
test_that("realistic health economics example", {
  # 10-year cost projection starting 2024, discount from 2025 at 3%
  years <- 2024:2033
  annual_costs <- rep(50000, 10)  # £50k per year

  result <- discount_future_values(years, annual_costs, 0.03, 2025)

  # 2024 should be unchanged
  expect_equal(result[1], 50000)

  # 2025 should be unchanged (start year)
  expect_equal(result[2], 50000)

  # 2026 should be discounted by 1 year
  expect_equal(result[3], 50000/1.03, tolerance = 1)

  # All values should be <= original (discounting reduces future values)
  expect_true(all(result <= annual_costs))

  # Values should decrease over time (after start year)
  expect_true(all(diff(result[2:10]) <= 0))
})

