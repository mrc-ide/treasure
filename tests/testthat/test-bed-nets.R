test_that("LLIN costing", {
  expect_equal(cost_llin(n_llin = 1), 1 * (2.52 + 1.50))
  expect_equal(cost_llin(n_llin = 2), 2 * (2.52 + 1.50))
  expect_equal(cost_llin(n_llin = c(1, 2)), c(1, 2) * (2.52 + 1.50))

  expect_equal(cost_llin(n_llin = 1, llin_unit_cost  = 2, llin_delivery_cost  = 3), 1 * (2 + 3))

  expect_error(cost_llin(n_llin = -1), "All llin_n estimates must be >= 0")
  expect_error(cost_llin(n_llin = 1, llin_unit_cost = -1), "Cost inputs must be >= 0")
  expect_error(cost_llin(n_llin = 1, llin_delivery_cost = -1), "Cost inputs must be >= 0")
})

test_that("Pyrethroid-PBO costing", {
  expect_equal(cost_pbo_itn(n_pbo_itn = 1), 1 * (3.51 + 1.50))
  expect_equal(cost_pbo_itn(n_pbo_itn = 2), 2 * (3.51 + 1.50))
  expect_equal(cost_pbo_itn(n_pbo_itn = c(1, 2)), c(1, 2) * (3.51 + 1.50))

  expect_equal(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_unit_cost = 2, pbo_itn_delivery_cost  = 3), 1 * (2 + 3))

  expect_error(cost_pbo_itn(n_pbo_itn = -1), "All llin_n estimates must be >= 0")
  expect_error(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_unit_cost = -1), "Cost inputs must be >= 0")
  expect_error(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_delivery_cost = -1), "Cost inputs must be >= 0")
})
