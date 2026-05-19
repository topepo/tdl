test_that("step_rate() returns a quantitative param object", {
  p <- step_rate()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "double")
})

test_that("step_rate() has the correct default range and label", {
  p <- step_rate()

  expect_equal(
    dials::range_get(p, original = FALSE),
    list(lower = 0, upper = 8)
  )
  expect_equal(p$label, c(step_rate = "Step Rate"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_false(is.null(p$trans))
})

test_that("step_rate() accepts a custom range", {
  p <- step_rate(range = c(2, 6))

  expect_equal(
    dials::range_get(p, original = FALSE),
    list(lower = 2, upper = 6)
  )
})

test_that("step_rate() works with grid and value utilities", {
  p <- step_rate()

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
  expect_named(grid, "step_rate")
})
