test_that("dropout_embedding() returns a quantitative param object", {
  p <- dropout_embedding()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "double")
})

test_that("dropout_embedding() has the correct default range and label", {
  p <- dropout_embedding()

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 0, upper = 0.5))
  expect_equal(p$label, c(dropout_embedding = "Embedding Dropout Rate"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_null(p$trans)
})

test_that("dropout_embedding() accepts a custom range", {
  p <- dropout_embedding(range = c(0, 0.3))

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 0, upper = 0.3))
})

test_that("dropout_embedding() works with grid and value utilities", {
  p <- dropout_embedding()

  set.seed(7291)
  samples <- dials::value_sample(p, 5)
  expect_length(samples, 5)
  expect_true(all(samples >= 0 & samples <= 0.5))

  seq_vals <- dials::value_seq(p, 4)
  expect_length(seq_vals, 4)
  expect_equal(min(seq_vals), 0)
  expect_equal(max(seq_vals), 0.5)

  grid <- dials::grid_regular(p, levels = 3)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 3)
  expect_named(grid, "dropout_embedding")
})
