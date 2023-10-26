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

test_that("Primaquine costing", {
  expect_equal(cost_primaquine(n_doses = 1), 1 * 0.4)
  expect_equal(cost_primaquine(n_doses = 2), 2 * 0.4)
  expect_equal(cost_primaquine(n_doses = c(1, 2)), c(1, 2) * 0.4)

  expect_equal(cost_primaquine(n_doses = 1, cost_per_dose  = 2), 1 * 2)

  expect_error(cost_primaquine(n_doses = -1), "All n_doses estimates must be >= 0")
  expect_error(cost_primaquine(n_doses = 1, cost_per_dose = -1), "Primaquine cost inputs must be >= 0")
})

test_that("WHO CHOICE costing", {
  # Outpatient
  expect_equal(cost_outpatient(n_visits = 1, cost_per_visit = 1), 1 * 1)
  expect_equal(cost_outpatient(n_visits = 2, cost_per_visit = 1), 2 * 1)
  expect_equal(cost_outpatient(n_visits = c(1, 2), cost_per_visit = 1), c(1, 2) * 1)

  expect_equal(cost_outpatient(n_visits = 1, cost_per_visit = 2), 1 * 2)

  expect_error(cost_outpatient(n_visits = -1, cost_per_visit = 1), "All n_visits estimates must be >= 0")
  expect_error(cost_outpatient(n_visits = 1, cost_per_visit = -1), "Outpatient cost inputs must be >= 0")

  # Inpatient
  expect_equal(cost_inpatient(n_visits = 1, cost_per_day = 1), 1 * 1 * 3)
  expect_equal(cost_inpatient(n_visits = 1, cost_per_day = 1, average_stay_duration = 4), 1 * 1 * 4)
  expect_equal(cost_inpatient(n_visits = 2, cost_per_day = 1), 2 * 1 * 3)
  expect_equal(cost_inpatient(n_visits = c(1, 2), cost_per_day = 1), c(1, 2) * 1 * 3)

  expect_equal(cost_inpatient(n_visits = 1, cost_per_day = 2), 1 * 2 * 3)

  expect_error(cost_inpatient(n_visits = -1, cost_per_day = 1), "All n_visits estimates must be >= 0")
  expect_error(cost_inpatient(n_visits = 1, cost_per_day = -1), "Inpatient cost inputs must be >= 0")
})
