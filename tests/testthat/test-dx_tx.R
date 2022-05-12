test_that("RDT costing", {
  expect_equal(cost_rdt(n_tests = 1), 1 * (0.46 * 1.15))
  expect_equal(cost_rdt(n_tests = 2), 2 * (0.46 * 1.15))
  expect_equal(cost_rdt(n_tests = c(1, 2)), c(1, 2) * (0.46 * 1.15))

  expect_equal(cost_rdt(n_tests = 1, rdt_unit_cost  = 2), 1 * (2 * 1.15))
  expect_equal(cost_rdt(n_tests = 1, delivery_mark_up  = 0.5), 1 * (0.46 * 1.5))

  expect_error(cost_rdt(n_tests = -1), "All n_tests estimates must be >= 0")
  expect_error(cost_rdt(n_tests = 1, rdt_unit_cost = -1), "RDT cost inputs must be >= 0")
  expect_error(cost_rdt(n_tests = 1, delivery_mark_up = -1), "RDT cost inputs must be >= 0")
})

test_that("AL costing", {
  expect_equal(cost_al(n_doses = 1), 1 * 0.3)
  expect_equal(cost_al(n_doses = 2), 2 * 0.3)
  expect_equal(cost_al(n_doses = c(1, 2)), c(1, 2) * 0.3)

  expect_equal(cost_al(n_doses = 1, cost_per_dose  = 2), 1 * 2)

  expect_error(cost_al(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_al(n_doses = 1, cost_per_dose = -1), "AL cost inputs must be >= 0")
})
