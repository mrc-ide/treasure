test_that("IPTp costing", {
  unit <- inflation_adjust(0.79, 2012, 2024)
  expect_equal(cost_iptp(n_administrations = 1), 1 * unit)
  expect_equal(cost_iptp(n_administrations = 2), 2 * unit)
  expect_equal(cost_iptp(n_administrations = c(1, 2)), c(1, 2) * unit)

  expect_equal(
    cost_iptp(n_administrations = 1, iptp_cost_per_administration  = 2, target_year = 2024),
    1 * inflation_adjust(2, 2012, 2024)
  )

  expect_error(cost_iptp(n_administrations = -1), "All n_administrations estimates must be >= 0")
  expect_error(cost_iptp(n_administrations = 1, iptp_cost_per_administration = -1), "IPTp cost inputs must be >= 0")
})
