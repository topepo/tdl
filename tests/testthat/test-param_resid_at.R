test_that("resid_at() returns a quantitative param object", {
  p <- resid_at()

  expect_s3_class(p, "quant_param")
  expect_s3_class(p, "param")
  expect_equal(p$type, "integer")
})

test_that("resid_at() has the correct default range and label", {
  p <- resid_at()

  bounds <- dials::range_get(p, original = TRUE)
  expect_equal(bounds$lower, 2L)
  expect_true(dials::is_unknown(bounds$upper))

  expect_equal(p$label, c(resid_at = "Residual Locations"))
  expect_equal(p$inclusive, c(lower = TRUE, upper = TRUE))
  expect_null(p$trans)
})

test_that("resid_at() accepts a custom finite range", {
  p <- resid_at(range = c(2L, 6L))

  expect_equal(
    dials::range_get(p, original = TRUE),
    list(lower = 2L, upper = 6L)
  )
})

test_that("resid_at() can be finalized via update()", {
  p <- resid_at()
  p_finalized <- dials::range_set(p, c(2L, 10L))

  expect_equal(
    dials::range_get(p_finalized, original = TRUE),
    list(lower = 2L, upper = 10L)
  )

  set.seed(5628)
  samples <- dials::value_sample(p_finalized, 5)
  expect_length(samples, 5)
  expect_true(all(samples >= 2L & samples <= 10L))
})

test_that("resid_at() errors when sampling without a finalized upper bound", {
  p <- resid_at()

  expect_error(dials::value_sample(p, 3))
})
