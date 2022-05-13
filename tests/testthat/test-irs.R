test_that("IRS costing", {
  # Long lasting per person
  expect_equal(cost_ll_irs_person(n_protected = 1), 1 * 7.44)
  expect_equal(cost_ll_irs_person(n_protected = 2), 2 * 7.44)
  expect_equal(cost_ll_irs_person(n_protected = c(1, 2)), c(1, 2) * 7.44)

  expect_equal(cost_ll_irs_person(n_protected = 1, cost_per_person_protected  = 2), 1 * 2)

  expect_error(cost_ll_irs_person(n_protected = -1), "All n_protected estimates must be >= 0")
  expect_error(cost_ll_irs_person(n_protected = 1, cost_per_person_protected = -1), "Long lasting IRS cost inputs must be >= 0")

  # Long lasting per structure
  expect_equal(cost_ll_irs_structure(n_sprayed = 1), 1 * 26.36)
  expect_equal(cost_ll_irs_structure(n_sprayed = 2), 2 * 26.36)
  expect_equal(cost_ll_irs_structure(n_sprayed = c(1, 2)), c(1, 2) * 26.36)

  expect_equal(cost_ll_irs_structure(n_sprayed = 1, cost_per_structure_sprayed  = 2), 1 * 2)

  expect_error(cost_ll_irs_structure(n_sprayed = -1), "All n_sprayed estimates must be >= 0")
  expect_error(cost_ll_irs_structure(n_sprayed = 1, cost_per_structure_sprayed = -1), "Long lasting IRS cost inputs must be >= 0")
})
