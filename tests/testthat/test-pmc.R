test_that("PMC costing", {
  expect_equal(cost_pmc(n_doses = 1), 1 * 0.3894)
  expect_equal(cost_pmc(n_doses = 2), 2 * 0.3894)
  expect_equal(cost_pmc(n_doses = c(1, 2)), c(1, 2) * 0.3894)

  expect_equal(cost_pmc(n_doses = 1, pmc_cost_per_dose_delivered  = 2), 1 * 2)

  expect_error(cost_pmc(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_pmc(n_doses = 1, pmc_cost_per_dose_delivered = -1), "PMC cost inputs must be >= 0")
})
