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

test_that("commodity diagnostic calculations", {
  expect_equal(
    commodity_rdt_tests(n_cases = 100, treatment_coverage = 0.5, proportion_rdt = 0.8, proportion_tested = 0.5),
    round(100 * 0.5 * 0.8 * 0.5)
  )
  expect_equal(
    commodity_microscopy_tests(n_cases = c(100, 50), treatment_coverage = c(0.5, 0.5), proportion_microscopy = c(0.6, 0.4), proportion_tested = 1),
    round(c(100 * 0.5 * 0.6 * 1, 50 * 0.5 * 0.4 * 1))
  )

  expect_equal(
    commodity_nmf_rdt_tests(n_nmf = 10, treatment_coverage = 0.5, proportion_rdt = 1, proportion_tested = 1, pfpr = 0.1, pfpr_threshold = 0.05),
    round(10 * 0.5 * 1 * 1)
  )
  expect_equal(
    commodity_nmf_rdt_tests(n_nmf = 10, treatment_coverage = 0.5, proportion_rdt = 1, proportion_tested = 1, pfpr = 0.02, pfpr_threshold = 0.05),
    0
  )

  expect_equal(
    commodity_nmf_microscopy_tests(n_nmf = 10, treatment_coverage = 0.5, proportion_microscopy = 1, proportion_tested = 1, pfpr = 0.1, pfpr_threshold = 0.05),
    round(10 * 0.5 * 1 * 1)
  )
  expect_equal(
    commodity_nmf_microscopy_tests(n_nmf = 10, treatment_coverage = 0.5, proportion_microscopy = 1, proportion_tested = 1, pfpr = 0.03, pfpr_threshold = 0.05),
    0
  )
})

test_that("commodity treatment doses", {
  expect_equal(
    commodity_al_doses(
      n_cases = c(10, 20, 30),
      treatment_coverage = c(1, 1, 1),
      proportion_act = c(1, 1, 1),
      age_upper = c(5, 15, 20)
    ),
    c(60, 300, 720)
  )

  expect_equal(
    commodity_nmf_al_doses(
      n_nmf = c(10, 20, 30),
      treatment_coverage = c(1, 1, 1),
      proportion_act = c(1, 1, 1),
      age_upper = c(5, 15, 20),
      pfpr = c(0.1, 0.06, 0.1),
      pfpr_threshold = 0.05
    ),
    round(c(10, 20, 30) * c(0.1, 0.06, 0.1) * c(1, 1, 1) * c(1, 1, 1) * c(6, 15, 24))
  )

  expect_equal(
    commodity_nmf_al_doses(
      n_nmf = 10,
      treatment_coverage = 1,
      proportion_act = 1,
      age_upper = 5,
      pfpr = 0.01,
      pfpr_threshold = 0.05
    ),
    0
  )
})
