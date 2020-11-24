.full_kernel <- function(X, Y, sklrn, kernel_function, ...) {
  XY <- rbind(X, Y)
  args <- list(...)
  args$X <- XY
  args$metric <- kernel_function
  K <- do.call(what = sklrn$pairwise_kernels, args = args)
  return(K)
}

MMD2u <- function(K, m, n) {
  # The MMD^2_u unbiased statistic.
  Kx2 <- K[1:m, 1:m]
  diag(Kx2) <- NA
  Ky2 <- K[(m + 1):(m + n), (m + 1):(m + n)]
  diag(Ky2) <- NA
  Kxy2 <- K[1:m, (m + 1):(m + n)]
  term1 <- (1.0 / (m * (m - 1.0))) * sum(Kx2, na.rm = TRUE)
  term2 <- (1.0 / (n * (n - 1.0))) * sum(Ky2, na.rm = TRUE)
  term3 <- (2.0 / (m * n)) * sum(Kxy2)
  return(term1 + term2 - term3)
}


compute_null_distribution_u <- function(K, m, n, iterations = 10000) {
  # Compute the bootstrap null-distribution of MMD2u.
  mmd2u_null <- rep(0, iterations)
  for (i in seq_len(iterations)) {
    idx <- sample(m + n)
    K_i <- K[idx, idx]
    mmd2u_null[i] <- MMD2u(K_i, m, n)
  }
  return(mmd2u_null)
}

.compress_kernel <- function(Kx, Ky, Kxy, Kyx, frac = .1) {
  m <- nrow(Kx)
  n <- nrow(Ky)
  l <- min((m ^ 2 - m) / 2, (n ^ 2 - n) / 2) * frac
  # Sample values from the pairwise kernel
  Kx_linear <- sample(Kx[upper.tri(Kx)], l)
  Ky_linear <- sample(Ky[upper.tri(Ky)], l)
  Kxy_linear <- sample(Kxy, l)
  Kyx_linear <- sample(Kyx, l)
  Kx_ <- c(Kx_linear, Kxy_linear)
  Ky_ <- c(Ky_linear, Kyx_linear)
  return(list("Kx_" = Kx_, "Ky_" = Ky_, "l" = l))
}

.ind_kernels <- function(X, Y, sklrn, kernel_function, frac = 0.1, ...){
  args <- list(...)
  args$metric <- kernel_function
  # Kernel for Y
  args$X <- Y
  Ky <- do.call(what = sklrn$pairwise_kernels, args = args)
  # diag(Ky) <- NA
  Ky[abs(Ky) < .Machine$double.eps] <- 0
  Ky <- Matrix::Matrix(data = Ky, sparse = TRUE)
  # Kernel for X
  args$X <- X
  Kx <- do.call(what = sklrn$pairwise_kernels, args = args)
  # diag(Kx) <- NA
  Kx[abs(Kx) < .Machine$double.eps] <- 0
  Kx <- Matrix::Matrix(data = Kx, sparse = TRUE)
  # Kernel for X/Y
  args$Y <- Y
  Kxy <- do.call(what = sklrn$pairwise_kernels, args = args)
  Kxy[abs(Kxy) < .Machine$double.eps] <- 0
  Kyx <- Matrix::Matrix(data = t(Kxy), sparse = TRUE)
  Kxy <- Matrix::Matrix(data = Kxy, sparse = TRUE)
  return(.compress_kernel(Kx, Ky, Kxy, Kyx, frac = frac))
}


MMDl <- function(Kx_, Ky_,  l) {
  # The MMD^2_u linear statistic.
  term1 <- (1 / l) * sum(Kx_[seq_len(l)])
  term2 <- (1 / l) * sum(Ky_[seq_len(l)])
  term3 <- (1 / l) * sum(Kx_[seq(l + 1, 2* l)])
  term4 <- (1 / l) * sum(Ky_[seq(l + 1, 2* l)])
  return(term1 + term2 - term3 - term4)
}

compute_null_distribution_l <- function(sample_Ks, iterations = 10000) {
  # Compute the bootstrap null-distribution of MMDl.
  l <- sample_Ks$l
  Kx_ <- sample_Ks$Kx_
  Ky_ <- sample_Ks$Ky_
  mmdl_null <- lapply(seq_len(iterations), function(i){
    ids <- sample(2 * l)
    Kx_i <- Kx_[ids]
    Ky_i <- Ky_[ids]
    return(MMDl(Kx_i, Ky_i, l))
  })
  return(unlist(mmdl_null))
}

#' Perform the Maximum Mean Discrepancy unbiased boostrap test
#'
#' @description Maximum Mean Discrepancy Unbiased Test
#'
#' @param x d-dimenstional smaples from the first distribution
#' @param y d-dimenstional smaples from the first distribution
#' @param kernel_function A character that must match a known kernel. See details.
#' @param type Which statistic to use. One of 'unbiased' or 'linear'. See
#' Gretton et al for details. Default to 'unbiased' if the two vectors are of
#' length less than \code{1000} and to 'linear' otherwise.
#' @param null How to asses the null distribution. This can only be set to exact
#' if the `type` is 'unbiased' and the `kernel_function` is 'rbf'.
#' @param iterations How many iterations to do to simulate the null distribution.
#' Default to 10^4. Only used if `null` is 'permutations'
#' @param ... Further arguments passed to kernel functions
#' @examples
#' if (reticulate::py_module_available("sklearn")) {
#'   x <- matrix(rnorm(1000, 0, 1), ncol = 10)
#'   y <- matrix(rnorm(1000, 0, 2), ncol = 10)
#'   mmd_test(x, y)
#'   x <- matrix(rnorm(1000, 0, 1), ncol = 10)
#'   y <- matrix(rnorm(1000, 0, 1), ncol = 10)
#'   mmd_test(x, y)
#' }
#' \dontrun{
#'  sklrn <- reticulate::import("sklearn.metrics")
#'  print(sklrn$pairwise$PAIRWISE_KERNEL_FUNCTIONS)
#' }
#' @details
#' This computes the MMD^2u unbiased statistic or the MMDl linear statistic
#' from Gretton et al. The code relies on the pairwise_kernel function from the
#' python module sklearn. To list the available kernels, see the examples.
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
mmd_test <- function(x, y, kernel_function = 'rbf',
                     type = ifelse(min(nrow(x), nrow(y)) < 1000,
                                   "unbiased", "linear"),
                     null = c("permutation", "exact"),
                     iterations = 10^4,
                     ...) {
  null <- match.arg(null)
  if (null == "exact" && (type == "linear" | kernel_function != 'rbf')) {
    stop("The exact mode only works with the unbiased statistic and the rbf kernel")
  }
  # Compute MMD^2_u or MMDl, its null distribution and the p-value of the
  # kernel two-sample test.
  X <- x
  Y <- y
  sklrn <- reticulate::import("sklearn.metrics")
  m <- nrow(X)
  n <- nrow(Y)
  if (type == "unbiased") {
    K <- .full_kernek(X, Y, sklrn, kernel_function, ...)
    statistic <- MMD2u(K, m, n)
    if (null == "permutations") {
      null <- compute_null_distribution_u(K, m, n ,iterations = iterations)
      p.value <- max(1 / iterations, mean(null > statistic))
    }
  }
  if (type == "linear") {
    sample_Ks <- .ind_kernels(X, Y, sklrn, kernel_function, frac = 0.1, ...)
    statistic <- MMDl(sample_Ks$Kx_, sample_Ks$Ky_, sample_Ks$l)
    null <- compute_null_distribution_l(sample_Ks, iterations = iterations)
    p.value <- max(1 / iterations, mean(null > statistic))
  }
  return(list("statistic" = statistic, "p.value" = p.value))
}
