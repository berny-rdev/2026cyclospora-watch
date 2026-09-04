test_that("wilson_ci returns NA for an empty denominator", {
  expect_true(all(is.na(wilson_ci(0, 0))))
})

test_that("wilson_ci brackets the point estimate", {
  ci <- wilson_ci(50, 100)
  expect_lt(ci[["lower"]], 0.5)
  expect_gt(ci[["upper"]], 0.5)
})

test_that("wilson_ci stays inside [0, 1] at the extremes", {
  # This is why Wilson rather than the normal approximation: at 0/n and n/n
  # the naive interval runs off the end of the scale.
  for (n in c(1, 5, 20, 164)) {
    lo <- wilson_ci(0, n); hi <- wilson_ci(n, n)
    expect_gte(lo[["lower"]], 0); expect_lte(lo[["upper"]], 1)
    expect_gte(hi[["lower"]], 0); expect_lte(hi[["upper"]], 1)
  }
})

test_that("wilson_ci narrows as n grows", {
  w <- function(n) diff(unname(wilson_ci(n / 2, n)))
  expect_gt(w(20), w(200))
})

test_that("add_wilson_ci adds percentage bounds to a frequency table", {
  freq <- tibble::tibble(category = c("cilantro", "romaine_head"), n_cases = c(47, 18))
  out  <- add_wilson_ci(freq, n_total = 164)
  expect_true(all(c("ci_low_pct", "ci_high_pct") %in% names(out)))
  expect_true(all(out$ci_low_pct  <= 100 * out$n_cases / 164))
  expect_true(all(out$ci_high_pct >= 100 * out$n_cases / 164))
})
