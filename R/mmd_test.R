.rbf <- function(x, y, g) {
  norm <- sum((x-y)^2)
  d <- -1.0 / (2 * (g^2))
  return(exp(d * norm))
}

.mmd_stat <- function(x,y,g) {
  m <- length(x)
  if ((m %% 2)==1) {
    m <- m - 1
  }
  m2 <- ceiling(m / 2)
  kxx <- rbf(x[1:m2,], x[(m2 + 1):m], g)
  kyy <- rbf(y[1:m2], y[(m2 + 1):m], g)
  kxy <- rbf(x[1:m2], y[(m2 + 1):m], g)
  kyx <- rbf(y[1:m2], x[(m2 + 1):m], g)
  res <- kxx + kyy - kxy - kyx
  return(list("mean" = mean(res), "variance" = var(res) / m2))
}

.choose_g <- function(x, y) {
  g <- 2^(-15:10)
  l <- 10e-4
  m2 <- ceiling(nrow(x)/2)
  bg <- 1
  br <- 0
  for (i in seq_along(g)) {
    stats <- .mmd_stat(x, y, g[i])
    ratio = stats$mean / (sqrt(stats$variance * m2) + l)
    if (ratio > br) {
      br <- ratio
      bg <- g[i]
    }
  }
  return(bg)
}

mmd_test <- function(x, y, use_all) {
  m <- nrow(x)
}
