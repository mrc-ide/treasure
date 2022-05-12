test_that("SMC costing", {
  expect_equal(cost_smc(n_doses = 1), 1 * 0.9075)
  expect_equal(cost_smc(n_doses = 2), 2 * 0.9075)
  expect_equal(cost_smc(n_doses = c(1, 2)), c(1, 2) * 0.9075)

  expect_equal(cost_smc(n_doses = 1, smc_cost_per_dose_delivered  = 2), 1 * 2)

  expect_error(cost_smc(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_smc(n_doses = 1, smc_cost_per_dose_delivered = -1), "SMC cost inputs must be >= 0")
})
