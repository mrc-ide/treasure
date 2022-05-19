test_that("IPTp costing", {
  expect_equal(cost_iptp(n_administrations = 1), 1 * 0.79)
  expect_equal(cost_iptp(n_administrations = 2), 2 * 0.79)
  expect_equal(cost_iptp(n_administrations = c(1, 2)), c(1, 2) * 0.79)

  expect_equal(cost_iptp(n_administrations = 1, iptp_cost_per_administration  = 2), 1 * 2)

  expect_error(cost_iptp(n_administrations = -1), "All n_administrations estimates must be >= 0")
  expect_error(cost_iptp(n_administrations = 1, iptp_cost_per_administration = -1), "IPTp cost inputs must be >= 0")
})
