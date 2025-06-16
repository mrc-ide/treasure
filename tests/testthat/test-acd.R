test_that("pACD works", {
  unit <- inflation_adjust(4.79, 1999, 2024)
  expect_equal(cost_pacd(n_tested = 1), 1 * unit)
  expect_equal(cost_pacd(n_tested = 2), 2 * unit)
  expect_equal(cost_pacd(n_tested = c(1, 2)), c(1, 2) * unit)

  expect_equal(
    cost_pacd(n_tested = 1, cost_per_person_tested  = 2, target_year = 2024),
    1 * inflation_adjust(2, 1999, 2024)
  )

  expect_error(cost_pacd(n_tested = -1), "All n_tested estimates must be >= 0")
  expect_error(cost_pacd(n_tested = 1, cost_per_person_tested = -1), "pACD cost inputs must be >= 0")
})

test_that("rACD works", {
  unit <- inflation_adjust(38.63, 1999, 2024)
  expect_equal(cost_racd(n_tested = 1), 1 * unit)
  expect_equal(cost_racd(n_tested = 2), 2 * unit)
  expect_equal(cost_racd(n_tested = c(1, 2)), c(1, 2) * unit)

  expect_equal(
    cost_racd(n_tested = 1, cost_per_person_tested  = 2, target_year = 2024),
    1 * inflation_adjust(2, 1999, 2024)
  )

  expect_error(cost_racd(n_tested = -1), "All n_tested estimates must be >= 0")
  expect_error(cost_racd(n_tested = 1, cost_per_person_tested = -1), "rACD cost inputs must be >= 0")
})
