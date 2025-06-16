test_that("set_target_year sets global option", {
  old <- getOption("treasure.target_year")
  set_target_year(2030)
  expect_equal(getOption("treasure.target_year"), 2030)
  options(treasure.target_year = old)
})
