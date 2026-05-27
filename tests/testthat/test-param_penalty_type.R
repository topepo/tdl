test_that("penalty_type() returns a qualitative param object", {
  p <- penalty_type()

  expect_s3_class(p, "qual_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "character")
})

test_that("penalty_type() has the correct default values and label", {
  p <- penalty_type()

  expect_equal(p$values, c("L1", "L2"))
  expect_equal(p$label, c(penalty_type = "Penalty Type"))
})

test_that("penalty_type() works with grid utilities", {
  p <- penalty_type()

  grid <- dials::grid_regular(p, levels = 2)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 2)
  expect_named(grid, "penalty_type")
  expect_equal(sort(grid$penalty_type), c("L1", "L2"))
})
