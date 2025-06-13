test_that("PMC costing", {
  expect_equal(cost_pmc(n_doses = 1), 1 * 0.3894)
  expect_equal(cost_pmc(n_doses = 2), 2 * 0.3894)
  expect_equal(cost_pmc(n_doses = c(1, 2)), c(1, 2) * 0.3894)

  expect_equal(cost_pmc(n_doses = 1, pmc_cost_per_dose_delivered  = 2), 1 * 2)

  expect_error(cost_pmc(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_pmc(n_doses = 1, pmc_cost_per_dose_delivered = -1), "PMC cost inputs must be >= 0")
})

test_that("PMC commodity doses", {
  expect_equal(
    commodity_doses_pmc(pmc_cov = 0.5, par_pmc = 100, n_rounds = 3),
    round(0.5 * 3 * 100)
  )
  expect_equal(
    commodity_doses_pmc(pmc_cov = c(0.1, 0.2), par_pmc = c(100, 50), n_rounds = 3),
    round(c(0.1 * 3 * 100, 0.2 * 3 * 50))
  )
  expect_error(
    commodity_doses_pmc(pmc_cov = c(0.1, 0.2), par_pmc = 100, n_rounds = 3),
    "length(pmc_cov) == length(par_pmc)"
  )
})
