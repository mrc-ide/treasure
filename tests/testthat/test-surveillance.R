test_that("multiplication works", {
  expect_equal(cost_surveillance(pop_at_risk = 1), 1 * 0.05)
  expect_equal(cost_surveillance(pop_at_risk = 2), 2 * 0.05)
  expect_equal(cost_surveillance(pop_at_risk = c(1, 2)), c(1, 2) * 0.05)

  expect_equal(cost_surveillance(pop_at_risk = 1, cost_per_pop_at_risk  = 2), 1 * 2)

  expect_error(cost_surveillance(pop_at_risk = -1), "All pop_at_risk estimates must be >= 0")
  expect_error(cost_surveillance(pop_at_risk = 1, cost_per_pop_at_risk = -1), "Surveillance cost inputs must be >= 0")
})
