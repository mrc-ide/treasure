test_that("IPTi costing", {
  expect_equal(cost_ipti(n_doses = 1), 1 * 0.1298)
  expect_equal(cost_ipti(n_doses = 2), 2 * 0.1298)
  expect_equal(cost_ipti(n_doses = c(1, 2)), c(1, 2) * 0.1298)

  expect_equal(cost_ipti(n_doses = 1, ipti_cost_per_dose_delivered  = 2), 1 * 2)

  expect_error(cost_ipti(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_ipti(n_doses = 1, ipti_cost_per_dose_delivered = -1), "IPTi cost inputs must be >= 0")
})
