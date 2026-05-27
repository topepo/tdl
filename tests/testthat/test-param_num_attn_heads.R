test_that("num_attn_heads() returns a quantitative param object", {
  p <- num_attn_heads()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "integer")
})

test_that("num_attn_heads() has the correct default range and label", {
  p <- num_attn_heads()

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 1L, upper = 8L))
  expect_equal(p$label, c(num_attn_heads = "# Attention Heads"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_null(p$trans)
})

test_that("num_attn_heads() accepts a custom range", {
  p <- num_attn_heads(range = c(2L, 4L))

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 2L, upper = 4L))
})

test_that("num_attn_heads() works with grid and value utilities", {
  p <- num_attn_heads()

  set.seed(7291)
  samples <- dials::value_sample(p, 5)
  expect_length(samples, 5)
  expect_true(all(samples >= 1L & samples <= 8L))
  expect_true(all(samples == as.integer(samples)))

  seq_vals <- dials::value_seq(p, 4)
  expect_length(seq_vals, 4)
  expect_equal(min(seq_vals), 1L)
  expect_equal(max(seq_vals), 8L)

  grid <- dials::grid_regular(p, levels = 3)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 3)
  expect_named(grid, "num_attn_heads")
})
