test_that("num_attn_feat() returns a quantitative param object", {
  p <- num_attn_feat()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "integer")
})

test_that("num_attn_feat() has the correct default range and label", {
  p <- num_attn_feat()

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 8L, upper = 64L))
  expect_equal(p$label, c(num_attn_feat = "# Attention Features"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_null(p$trans)
})

test_that("num_attn_feat() accepts a custom range", {
  p <- num_attn_feat(range = c(16L, 32L))

  expect_equal(dials::range_get(p, original = TRUE), list(lower = 16L, upper = 32L))
})

test_that("num_attn_feat() works with grid and value utilities", {
  p <- num_attn_feat()

  set.seed(7291)
  samples <- dials::value_sample(p, 5)
  expect_length(samples, 5)
  expect_true(all(samples >= 8L & samples <= 64L))
  expect_true(all(samples == as.integer(samples)))

  seq_vals <- dials::value_seq(p, 4)
  expect_length(seq_vals, 4)
  expect_equal(min(seq_vals), 8L)
  expect_equal(max(seq_vals), 64L)

  grid <- dials::grid_regular(p, levels = 3)
  expect_s3_class(grid, "tbl_df")
  expect_equal(nrow(grid), 3)
  expect_named(grid, "num_attn_feat")
})
