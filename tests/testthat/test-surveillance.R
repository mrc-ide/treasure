test_that("multiplication works", {
  unit <- inflation_adjust(0.05, 1999, 2024)
  expect_equal(cost_surveillance(pop_at_risk = 1), 1 * unit)
  expect_equal(cost_surveillance(pop_at_risk = 2), 2 * unit)
  expect_equal(cost_surveillance(pop_at_risk = c(1, 2)), c(1, 2) * unit)

  expect_equal(
    cost_surveillance(pop_at_risk = 1, cost_per_pop_at_risk  = 2, target_year = 2024),
    1 * inflation_adjust(2, 1999, 2024)
  )

  expect_error(cost_surveillance(pop_at_risk = -1), "All pop_at_risk estimates must be >= 0")
  expect_error(cost_surveillance(pop_at_risk = 1, cost_per_pop_at_risk = -1), "Surveillance cost inputs must be >= 0")
})
