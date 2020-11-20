.rbf <- function(x, y, g) {
  norm <- sum((x - y)^2)
  d <- -1.0 / (2 * (g^2))
  return(exp(d * norm))
}

h <- function(zs, d, k, ...) {
  x_1 <- zs[seq_len(d)]; x_2 <- zs[(d + 1):(2 * d)]
  y_1 <- zs[(2 * d + 1):(3 * d)]; y_2 <- zs[(3 * d + 1):(4 * d)]
  return(
    k(x_1, x_2, ...) + k(y_1, y_2, ...) - k(x_1, y_1, ...) - k(x_2, y_2, ...)
  )
}

.mmd_stat_linear <- function(x, y, k, ...) {
  n <- nrow(x)
  m <- ncol(y)
  d <- ncol(x)
  l <- min(m, n)
  x <- x[base::sample(seq_len(n), l), ]
  y <- y[base::sample(seq_len(m), l), ]
  l2 <- floor(l / 2)
  inds <- sample(seq_len(l), l2)
  zs <- cbind(x[inds,], x[-inds, ], y[inds,], y[-inds, ])
  res <- apply(zs, 1, h, d = d, k = k, ... = ...)
  return(res)
}

.mmd_stat_unbiased <- function(x, y, k, ...) {
  n <- nrow(x)
  m <- ncol(y)
  d <- ncol(x)
  term1 <- .vectorized_pdist(x, x)
  term1 <- apply(term1, 1, function(row) {
    apply(row, 1, k, ... = ...)
  })
  term1 <- sum(term1)
  term2 <- .vectorized_pdist(x, x)
  term1 <- apply(term1, 1, function(row) {
    apply(row, 1, k, ... = ...)
  })
  term1 <- sum(term1)
}

.mmd_stat <- function(x, y, type = c("Unbiased", "Biased", "Linear"), ...) {
  type <- match.arg(type)
  if (type == "Linear") {
    res <- .mmd_stat_linear(x, y, ...)
  } else if (type == "Biased") {
    res <- .mmd_stat_linear(x, y, ...)
  } else if (type == "Unbiased") {
    res <- .mmd_stat_linear(x, y, ...)
  }

  return(list("mean" = mean(res), "variance" = stats::var(res) / m2))
}
#
# .choose_g <- function(x, y) {
#   g <- 2^(-15:10)
#   l <- 10e-4
#   m2 <- ceiling(nrow(x)/2)
#   bg <- 1
#   br <- 0
#   for (i in seq_along(g)) {
#     stats <- .mmd_stat(x, y, g[i])
#     ratio = stats$mean / (sqrt(stats$variance * m2) + l)
#     if (ratio > br) {
#       br <- ratio
#       bg <- g[i]
#     }
#   }
#   return(bg)
# }
#
# mmd_test <- function(x, y, use_all) {
#   m <- nrow(x)
# }
#
#
# from __future__ import division
# import numpy as np
# from sys import stdout
# from sklearn.metrics import pairwise_kernels
#
#
# def MMD2u(K, m, n):
#   """The MMD^2_u unbiased statistic.
#     """
# Kx = K[:m, :m]
# Ky = K[m:, m:]
# Kxy = K[:m, m:]
# return 1.0 / (m * (m - 1.0)) * (Kx.sum() - Kx.diagonal().sum()) + \
# 1.0 / (n * (n - 1.0)) * (Ky.sum() - Ky.diagonal().sum()) - \
# 2.0 / (m * n) * Kxy.sum()
#
#
# def compute_null_distribution(K, m, n, iterations=10000, verbose=False,
#                               random_state=None, marker_interval=1000):
#   """Compute the bootstrap null-distribution of MMD2u.
#     """
# if type(random_state) == type(np.random.RandomState()):
#   rng = random_state
# else:
#   rng = np.random.RandomState(random_state)
#
# mmd2u_null = np.zeros(iterations)
# for i in range(iterations):
#   if verbose and (i % marker_interval) == 0:
#   print(i),
# stdout.flush()
# idx = rng.permutation(m+n)
# K_i = K[idx, idx[:, None]]
# mmd2u_null[i] = MMD2u(K_i, m, n)
#
# if verbose:
#   print("")
#
# return mmd2u_null
#
#
# def compute_null_distribution_given_permutations(K, m, n, permutation,
#                                                  iterations=None):
#   """Compute the bootstrap null-distribution of MMD2u given
#     predefined permutations.
#     Note:: verbosity is removed to improve speed.
#     """
# if iterations is None:
#   iterations = len(permutation)
#
# mmd2u_null = np.zeros(iterations)
# for i in range(iterations):
#   idx = permutation[i]
# K_i = K[idx, idx[:, None]]
# mmd2u_null[i] = MMD2u(K_i, m, n)
#
# return mmd2u_null
#
#
# def kernel_two_sample_test(X, Y, kernel_function='rbf', iterations=10000,
#                            verbose=False, random_state=None, **kwargs):
#   """Compute MMD^2_u, its null distribution and the p-value of the
#     kernel two-sample test.
#     Note that extra parameters captured by **kwargs will be passed to
#     pairwise_kernels() as kernel parameters. E.g. if
#     kernel_two_sample_test(..., kernel_function='rbf', gamma=0.1),
#     then this will result in getting the kernel through
#     kernel_function(metric='rbf', gamma=0.1).
#     """
# m = len(X)
# n = len(Y)
# XY = np.vstack([X, Y])
# K = pairwise_kernels(XY, metric=kernel_function, **kwargs)
# mmd2u = MMD2u(K, m, n)
# if verbose:
#   print("MMD^2_u = %s" % mmd2u)
# print("Computing the null distribution.")
#
# mmd2u_null = compute_null_distribution(K, m, n, iterations,
#                                        verbose=verbose,
#                                        random_state=random_state)
# p_value = max(1.0/iterations, (mmd2u_null > mmd2u).sum() /
#                 float(iterations))
# if verbose:
#   print("p-value ~= %s \t (resolution : %s)" % (p_value, 1.0/iterations))
#
# return mmd2u, mmd2u_null, p_value
