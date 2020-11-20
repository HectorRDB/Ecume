MMD2u <- function(K, m, n) {
  # The MMD^2_u unbiased statistic.
  Kx <- K[1:m, 1:m]
  diag(Kx) <- NA
  Ky <- K[(m + 1):n, (m + 1):n]
  Kxy <- K[1:m, (m + 1):n]
  term1 = 1.0 / (m * (m - 1.0)) * sum(Kx, na.rm = TRUE)
  term2 = 1.0 / (n * (n - 1.0)) * sum(Ky, na.rm = TRUE)
  term3 = 2.0 / (m * n) * sum(Kxy)
  return(term1 + term2 - term3)
}


compute_null_distribution <- function(K, m, n, iterations = 10000) {
  # Compute the bootstrap null-distribution of MMD2u.
  mmd2u_null = rep(0, iterations)
  for (i in seq_len(iterations)) {
    idx <- sample(m + n)
    K_i <- K[idx, idx]
    mmd2u_null[i] <- MMD2u(K_i, m, n)
  }
  return(mmd2u_null)

}

#' Perform the Maximum Mean Discrepancy unbiased boostrap test
#'
#' @description Maximum Mean Discrepancy Unbiased Test
#'
#' @param x d-dimenstional smaples from the first distribution
#' @param y d-dimenstional smaples from the first distribution
#' @param kernel_function A character that must match a known kernel. See details.
#' @param ... Further arguments passed to kernel functions
#' @examples
#'  if (reticulate::py_module_available("sklearn")) {
#'    x <- rnorm(1000, 0, 1)
#'    y <- rnorm(1000, 0, 2)
#'    mmd_test(x, y)
#'  }
#' \dontrun{
#'  sklrn <- reticulate::import("sklearn.metrics")
#'  print(sklrn$pairwise$PAIRWISE_KERNEL_FUNCTIONS)
#' }
#' @details
#' This computes the MMD^2 unbiased statistics from Gretton et al.
#' The code relies on the pairwise_kernel function from the python module
#' sklearn. To list the available kernels, see the examples.
#' @md
#' @references
#' Gretton, A., Borgwardt, K., Rasch, M. J., Schölkopf, B., & Smola, A. (2012).
#' *A Kernel Two-Sample Test* Journal of Machine Learning Research (2012)
#' @return
#' A list containing the following components:
#' \itemize{
#'   \item *statistic* the value of the test statistic.
#'   \item *p.value* the p-value of the test.
#' }
#' @importFrom reticulate import
#' @export
mmd_test <- function(x, y, kernel_function = 'rbf', iterations = 10^4,
                     ...) {
  proc <- basilisk::basiliskStart(my_env)
  on.exit(basilisk::basiliskStop(proc))
  some_useful_thing <- basilisk::basiliskRun(proc,
                                             function(X, Y, kernel_function) {
    # Compute MMD^2_u, its null distribution and the p-value of the
    # kernel two-sample test.
    sklrn <- reticulate::import("sklearn.metrics")
    m <- nrow(X)
    n <- nrow(Y)
    XY <- rbind(X, Y)
    args <- list(....)
    args$X <- XY
    args$metric <- kernel_function
    K <- do.call(what = sklrn$pairwise_kernels, args = args)
    mmd2u <- MMD2u(K, m, n)
    mmd2u_null <- compute_null_distribution(K, m, n, iterations = iterations)
    p_value <- max(1 / iterations, mean(mmd2u_null > mmd2u))
    return(mmd2u, mmd2u_null, p_value)
  }, arg1 = x, arg2 = y, kernel_function = kernel_function,
    iterations = iterations, ... = ...)
  return(list("statistic" = res$mmdu, "p.value" = res$p_value))
}
