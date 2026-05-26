test_that("penalty_average() returns a quantitative param object", {
  p <- penalty_average()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "double")
})

test_that("penalty_average() has the correct default range and label", {
  p <- penalty_average()

  expect_equal(
    dials::range_get(p, original = FALSE),
    list(lower = -15, upper = -5)
  )
  expect_equal(p$label, c(penalty_average = "Penalty Average"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_false(is.null(p$trans))
})

test_that("penalty_average() accepts a custom range", {
  p <- penalty_average(range = c(-12, -4))

  expect_equal(
    dials::range_get(p, original = FALSE),
    list(lower = -12, upper = -4)
  )
})

test_that("penalty_average() works with grid and value utilities", {
  p <- penalty_average()

  set.seed(7291)
  samples <- dials::value_sample(p, 5)
  expect_length(samples, 5)
  expect_true(all(samples > 0))

  seq_vals <- dials::value_seq(p, 4)
  expect_length(seq_vals, 4)
  expect_true(all(seq_vals > 0))

  grid <- dials::grid_regular(p, levels = 3)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 3)
  expect_named(grid, "penalty_average")
})
