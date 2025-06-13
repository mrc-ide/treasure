test_that("RTSS costing", {
  expect_equal(cost_rtss(n_doses = 1), 1 * (10.02 + 1.52 + 1.48))
  expect_equal(cost_rtss(n_doses = 2), 2 * (10.02 + 1.52 + 1.48))
  expect_equal(cost_rtss(n_doses = c(1, 2)), c(1, 2) * (10.02 + 1.52 + 1.48))

  expect_equal(cost_rtss(n_doses = 1, rtss_cost_per_dose  = 2,
                         rtss_consumables_cost  = 3, rtss_delivery_cost = 4),
               1 * (2 + 3 + 4))

  expect_error(cost_rtss(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_cost_per_dose = -1), "RTSS cost inputs must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_consumables_cost = -1), "RTSS cost inputs must be >= 0")
  expect_error(cost_rtss(n_doses = 1, rtss_delivery_cost = -1), "RTSS cost inputs must be >= 0")
})

test_that("R21 costing", {
  expect_equal(cost_r21(n_doses = 1), 1 * (4 + 1.52 + 1.48))
  expect_equal(cost_r21(n_doses = 2), 2 * (4 + 1.52 + 1.48))
  expect_equal(cost_r21(n_doses = c(1, 2)), c(1, 2) * (4 + 1.52 + 1.48))

  expect_equal(cost_r21(n_doses = 1, r21_cost_per_dose  = 2,
                        r21_consumables_cost  = 3, r21_delivery_cost = 4),
               1 * (2 + 3 + 4))

  expect_error(cost_r21(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_r21(n_doses = 1, r21_cost_per_dose = -1), "R21 cost inputs must be >= 0")
  expect_error(cost_r21(n_doses = 1, r21_consumables_cost = -1), "R21 cost inputs must be >= 0")
  expect_error(cost_r21(n_doses = 1, r21_delivery_cost = -1), "R21 cost inputs must be >= 0")
})

test_that("Vaccine commodity doses", {
  expect_equal(
    commodity_doses_vaccine(vaccine_cov = 0.5, par_vaccine = 100),
    round((0.5 * 100 * 3) + (0.5 * 100 * 0.8 * 1))
  )
  expect_equal(
    commodity_doses_vaccine(vaccine_cov = c(0.5, 0.6), par_vaccine = c(100, 50)),
    round(c(
      (0.5 * 100 * 3) + (0.5 * 100 * 0.8 * 1),
      (0.6 * 50 * 3) + (0.6 * 50 * 0.8 * 1)
    ))
  )
})
