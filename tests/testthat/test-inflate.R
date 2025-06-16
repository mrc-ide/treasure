test_that("inflation adjustment uses CPI ratios", {
  cpi_region <- cpi[cpi$region == "Sub-Saharan Africa", ]
  base <- cpi_region$cpi[cpi_region$year == 2007]
  target <- cpi_region$cpi[cpi_region$year == 2024]
  expect_equal(inflation_adjust(0.26, 2007, 2024), 0.26 * (target / base))
})

test_that("inflation adjustment uses global option when target_year missing", {
  set_target_year(2024)
  cpi_region <- cpi[cpi$region == "Sub-Saharan Africa", ]
  base <- cpi_region$cpi[cpi_region$year == 2007]
  target <- cpi_region$cpi[cpi_region$year == 2024]
  expect_equal(inflation_adjust(0.26, 2007), 0.26 * (target / base))
})

test_that("inflation adjustment uses global option when region missing", {
  set_region("South Asia")
  cpi_region <- cpi[cpi$region == "South Asia", ]
  base <- cpi_region$cpi[cpi_region$year == 2007]
  target <- cpi_region$cpi[cpi_region$year == 2024]
  expect_equal(inflation_adjust(0.26, 2007, 2024),
               0.26 * (target / base))
})

test_that("inflation adjustment handles missing years", {
  expect_length(inflation_adjust(1, 1900, 2024), 0)
  expect_length(inflation_adjust(1, 2007, 3000), 0)
})

test_that("inflation adjustment vectorises", {
  cpi_region <- cpi[cpi$region == "Sub-Saharan Africa", ]
  base1 <- cpi_region$cpi[cpi_region$year == 2007]
  base2 <- cpi_region$cpi[cpi_region$year == 2008]
  target <- cpi_region$cpi[cpi_region$year == 2024]
  expect_equal(
    inflation_adjust(c(0.26, 0.52), c(2007, 2008), 2024),
    c(0.26 * (target / base1), 0.52 * (target / base2))
  )
})

test_that("inflation adjustment can be skipped", {
  expect_equal(inflation_adjust(0.5, 2007, 2024, adjust = FALSE), 0.5)
})
