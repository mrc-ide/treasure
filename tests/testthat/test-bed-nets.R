test_that("LLIN costing", {
  expect_equal(cost_llin(n_llin = 1), 1 * (2.02 + 1.50))
  expect_equal(cost_llin(n_llin = 2), 2 * (2.02 + 1.50))
  expect_equal(cost_llin(n_llin = c(1, 2)), c(1, 2) * (2.02 + 1.50))

  expect_equal(cost_llin(n_llin = 1, llin_unit_cost  = 2, llin_delivery_cost  = 3), 1 * (2 + 3))

  expect_error(cost_llin(n_llin = -1), "All llin_n estimates must be >= 0")
  expect_error(cost_llin(n_llin = 1, llin_unit_cost = -1), "LLIN cost inputs must be >= 0")
  expect_error(cost_llin(n_llin = 1, llin_delivery_cost = -1), "LLIN cost inputs must be >= 0")
})

test_that("Pyrethroid-PBO costing", {
  expect_equal(cost_pbo_itn(n_pbo_itn = 1), 1 * (2.63 + 1.50))
  expect_equal(cost_pbo_itn(n_pbo_itn = 2), 2 * (2.63 + 1.50))
  expect_equal(cost_pbo_itn(n_pbo_itn = c(1, 2)), c(1, 2) * (2.63 + 1.50))

  expect_equal(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_unit_cost = 2, pbo_itn_delivery_cost  = 3), 1 * (2 + 3))

  expect_error(cost_pbo_itn(n_pbo_itn = -1), "All llin_n estimates must be >= 0")
  expect_error(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_unit_cost = -1), "PBO cost inputs must be >= 0")
  expect_error(cost_pbo_itn(n_pbo_itn = 1, pbo_itn_delivery_cost = -1), "PBO cost inputs must be >= 0")
})

test_that("Pyrethroid-chlorfenapyr costing", {
  expect_equal(cost_dualai_itn(n_dualai_itn = 1), 1 * (2.70 + 1.50))
  expect_equal(cost_dualai_itn(n_dualai_itn = 2), 2 * (2.70 + 1.50))
  expect_equal(cost_dualai_itn(n_dualai_itn = c(1, 2)), c(1, 2) * (2.70 + 1.50))

  expect_equal(cost_dualai_itn(n_dualai_itn = 1, dualai_itn_unit_cost = 2, dualai_itn_delivery_cost  = 3), 1 * (2 + 3))

  expect_error(cost_dualai_itn(n_dualai_itn = -1), "All llin_n estimates must be >= 0")
  expect_error(cost_dualai_itn(n_dualai_itn = 1, dualai_itn_unit_cost = -1), "Dual ai cost inputs must be >= 0")
  expect_error(cost_dualai_itn(n_dualai_itn = 1, dualai_itn_delivery_cost = -1), "Dual ai cost inputs must be >= 0")
})

test_that("Commodity nets", {
  skip_if_not_installed("netz")
  usage <- c(0.5, 0.6)
  use_rate <- 0.5
  dist_steps <- c(1, 366)
  crop_steps <- dist_steps + 183
  half_life <- 730
  par <- c(100, 100)

  expected <- {
    access <- netz::usage_to_access(usage = usage, use_rate = use_rate)
    crop <- netz::access_to_crop(access = access)
    dist <- netz::crop_to_distribution(
      crop = crop,
      crop_timesteps = crop_steps,
      distribution_timesteps = dist_steps,
      half_life = half_life
    )
    round(dist * par)
  }

  expect_equal(
    commodity_nets(
      usage = usage,
      use_rate = use_rate,
      distribution_timesteps = dist_steps,
      crop_timesteps = crop_steps,
      half_life = half_life,
      par = par
    ),
    expected
  )
})
