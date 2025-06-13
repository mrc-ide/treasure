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

test_that("IRS commodity people rounds", {
  expect_equal(
    commodity_person_rounds_irs(irs_cov = 0.5, n_rounds = 2, par = 100),
    round(0.5 * 2 * 100)
  )
  expect_equal(
    commodity_person_rounds_irs(irs_cov = c(0.1, 0.2), n_rounds = 3, par = c(100, 50)),
    round(c(0.1 * 3 * 100, 0.2 * 3 * 50))
  )
  expect_error(
    commodity_person_rounds_irs(irs_cov = c(0.1, 0.2), n_rounds = 3, par = 100),
    "length(irs_cov) == length(par)"
  )
})

test_that("IRS commodity structure rounds", {
  expect_equal(
    commodity_structure_rounds_irs(irs_cov = 0.5, n_rounds = 2, par = 100, hh_size = 5),
    round((0.5 * 2 * 100) / 5)
  )
  expect_equal(
    commodity_structure_rounds_irs(
      irs_cov = c(0.2, 0.4), n_rounds = 1, par = c(50, 50), hh_size = 5
    ),
    round(c(0.2 * 1 * 50, 0.4 * 1 * 50) / 5)
  )
  expect_error(
    commodity_structure_rounds_irs(irs_cov = c(0.2, 0.4), n_rounds = 1, par = 50, hh_size = 5),
    "length(irs_cov) == length(par)"
  )
  expect_error(
    commodity_structure_rounds_irs(irs_cov = 0.2, n_rounds = 1, par = 50, hh_size = c(5, 5)),
    "length(hh_size) == 1"
  )
})
