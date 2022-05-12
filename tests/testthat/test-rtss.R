test_that("RTSS costing", {
  expect_equal(cost_rtss(n_doses = 1), 1 * (5 + 1.52 + 1.62))
  expect_equal(cost_rtss(n_doses = 2), 2 * (5 + 1.52 + 1.62))
  expect_equal(cost_rtss(n_doses = c(1, 2)), c(1, 2) * (5 + 1.52 + 1.62))

  expect_equal(cost_rtss(n_doses = 1, rtss_cost_per_dose  = 2,
                         rtss_cosumables_cost  = 3, rtss_delivery_cost = 4),
               1 * (2 + 3 + 4))

  expect_error(cost_rtss(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_cost_per_dose = -1), "RTSS cost inputs must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_cosumables_cost = -1), "RTSS cost inputs must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_delivery_cost = -1), "RTSS cost inputs must be >= 0")
})
