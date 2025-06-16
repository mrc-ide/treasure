test_that("set_region sets global option", {
  old <- getOption("treasure.region")
  set_region("South Asia")
  expect_equal(getOption("treasure.region"), "South Asia")
  options(treasure.region = old)
})

test_that("set_target_year sets global option", {
  old <- getOption("treasure.target_year")
  set_target_year(2030)
  expect_equal(getOption("treasure.target_year"), 2030)
  options(treasure.target_year = old)
})
