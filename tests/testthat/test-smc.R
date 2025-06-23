test_that("SMC costing", {
  unit <- inflation_adjust(0.9075, 2016, 2024)
  expect_equal(cost_smc(n_doses = 1), 1 * unit)
  expect_equal(cost_smc(n_doses = 2), 2 * unit)
  expect_equal(cost_smc(n_doses = c(1, 2)), c(1, 2) * unit)

  expect_equal(
    cost_smc(n_doses = 1, smc_cost_per_dose_delivered  = 2, target_year = 2024),
    1 * inflation_adjust(2, 2016, 2024)
  )

  expect_error(cost_smc(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_smc(n_doses = 1, smc_cost_per_dose_delivered = -1), "SMC cost inputs must be >= 0")
})

test_that("SMC commodity doses", {
  expect_equal(
    commodity_doses_smc(smc_cov = 0.5, n_rounds = 4, par_smc = 100),
    round(0.5 * 4 * 100)
  )
  expect_equal(
    commodity_doses_smc(smc_cov = c(0.1, 0.2), n_rounds = c(3, 3), par_smc = c(100, 50)),
    round(c(0.1 * 3 * 100, 0.2 * 3 * 50))
  )
  expect_error(
    commodity_doses_smc(smc_cov = c(0.1, 0.2), n_rounds = 3, par_smc = 100),
    "is not TRUE"
  )
})
