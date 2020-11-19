library(testthat)

test_that("stouffer_Z_scoreworks with all inputs",{
  pvals <- runif(100, 0, 1)
  weights <- runif(100, 0, 1)
  expect_is(stouffer_zscore(pvals, weights), "list")
  pval <- runif(1, 0, 1)
  combi <- stouffer_zscore(pval)
  expect_equal(pval, combi$p.value)
  pvals <- rep(pval, 100)
  for (i in 2:100) {
    combi <- stouffer_zscore(pvals[1:i], weights[1:i])
    expect_true(pval > combi$p.value)
  }
})
