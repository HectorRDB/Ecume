library(testthat)
skip_if_no_sklearn <- function() {
  have_sklearn <- reticulate::py_module_available("sklearn")
  if (!have_sklearn)
    skip("sklearn not available for testing")
}

test_that("the mmd_test works with all inputs",{
  set.seed(08)
  skip_if_no_sklearn()
  sklrn <- reticulate::import("sklearn.metrics")
  x <- matrix(c(runif(100, 0, 1),
                runif(100, -1, 1)),
              ncol = 2)
  y <- matrix(c(runif(100, 0, 1),
                runif(100, -1, 1)),
              ncol = 2)
  test <- mmd_test(x, y)
  expect_is(test, "list")
  test <- mmd_test(x, y, type = "linear")
  expect_is(test, "list")
  expect_error(mmd_test(x, y, type = "linear", null = "exact"))
})
