test_that("bottleneck_units() returns a quantitative param object", {
  p <- bottleneck_units()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "integer")
})

test_that("bottleneck_units() has the correct default range and label", {
  p <- bottleneck_units()

  expect_equal(
    dials::range_get(p, original = TRUE),
    list(lower = 2L, upper = 25L)
  )
  expect_equal(p$label, c(bottleneck_units = "# Bottleneck Units"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_null(p$trans)
})

test_that("bottleneck_units() accepts a custom range", {
  p <- bottleneck_units(range = c(4L, 8L))

  expect_equal(
    dials::range_get(p, original = TRUE),
    list(lower = 4L, upper = 8L)
  )
})

test_that("bottleneck_units() works with grid and value utilities", {
  p <- bottleneck_units()

  set.seed(7291)
  samples <- dials::value_sample(p, 5)
  expect_length(samples, 5)
  expect_true(all(samples >= 2L & samples <= 25L))
  expect_true(all(samples == as.integer(samples)))

  seq_vals <- dials::value_seq(p, 4)
  expect_length(seq_vals, 4)
  expect_equal(min(seq_vals), 2L)
  expect_equal(max(seq_vals), 25L)

  grid <- dials::grid_regular(p, levels = 3)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 3)
  expect_named(grid, "bottleneck_units")
})
