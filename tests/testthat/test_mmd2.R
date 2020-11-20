library(testthat)
sklrn <- reticulate::import("sklearn.metrics")

test_that("the mmd_test works with all inputs",{
  set.seed(08)
  x <- matrix(c(runif(100, 0, 1),
                runif(100, -1, 1)),
              ncol = 2)
  y <- matrix(c(runif(100, 0, 1),
                runif(100, -1, 1)),
              ncol = 2)
  test <- mmd_test(x, y)
  expect_is(test, "list")
})
