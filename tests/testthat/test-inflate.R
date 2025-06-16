test_that("inflation adjustment uses CPI ratios", {
  cpi_region <- cpi[cpi$region == "Sub-Saharan Africa", ]
  base <- cpi_region$cpi[cpi_region$year == 2007]
  target <- cpi_region$cpi[cpi_region$year == 2024]
  expect_equal(inflation_adjust(0.26, 2007, 2024), 0.26 * (target / base))
})

test_that("inflation adjustment input validation", {
  expect_error(inflation_adjust(1, 1900, 2024), "cost_year not found")
  expect_error(inflation_adjust(1, 2007, 3000), "target_year not found")
  expect_error(inflation_adjust(c(1,2), 2007, 2024), "single values")
})
