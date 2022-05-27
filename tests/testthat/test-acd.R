test_that("pACD works", {
  expect_equal(cost_pacd(n_tested = 1), 1 * 4.79)
  expect_equal(cost_pacd(n_tested = 2), 2 * 4.79)
  expect_equal(cost_pacd(n_tested = c(1, 2)), c(1, 2) * 4.79)

  expect_equal(cost_pacd(n_tested = 1, cost_per_person_tested  = 2), 1 * 2)

  expect_error(cost_pacd(n_tested = -1), "All n_tested estimates must be >= 0")
  expect_error(cost_pacd(n_tested = 1, cost_per_person_tested = -1), "pACD cost inputs must be >= 0")
})

test_that("rACD works", {
  expect_equal(cost_racd(n_tested = 1), 1 * 38.63)
  expect_equal(cost_racd(n_tested = 2), 2 * 38.63)
  expect_equal(cost_racd(n_tested = c(1, 2)), c(1, 2) * 38.63)

  expect_equal(cost_racd(n_tested = 1, cost_per_person_tested  = 2), 1 * 2)

  expect_error(cost_racd(n_tested = -1), "All n_tested estimates must be >= 0")
  expect_error(cost_racd(n_tested = 1, cost_per_person_tested = -1), "rACD cost inputs must be >= 0")
})
