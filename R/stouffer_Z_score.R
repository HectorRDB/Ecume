#' Stouffer
#'
#' @description Stouffer's Z-score method
#'
#' @param pvals A vector of p-values
#' @param weights A vector of weights
#' @examples
#'  pvals <- runif(100, 0, 1)
#'  weights <- runif(100, 0, 1)
#'  stouffer_zscore(pvals, weights)
#' @details
#' Given a set of i.i.d p-values and associated weights, it combines the
#' p-values \eqn{p_i}. Letting \eqn{\phi} be the standard normal cumulative distribution function
#' and \eqn{Z_i =\phi^{-1} (1-p_i)}, the meta-analysis Z-score is
#'
#' \deqn{Z = (\sum w_i Z_i) * (\sum (w_i)^2)^(-1/2)}
#' @md
#' @references
#' Samuel Andrew Stouffer. *Adjustment during army life*.  Princeton University Press, 1949.
#' @return
#' A list containing the following components:
#' \itemize{
#'   \item *statistic* the value of the test statistic.
#'   \item *p.value* the p-value of the test.
#' }
#' @export
#'
stouffer_zscore <- function(pvals, weights = rep(1, seq_along(pvals))) {
  if(length(pvals) != length(weights)) {
    stop("pvals and weights must have the same length")
  }
  Zs <- lapply(pvals, function(pval) {
    return(stats::qnorm(pval / 2, lower.tail = FALSE))
  }) %>%
    unlist()
  Z <- sum(Zs * weights) / sqrt(sum(weights^2))
  return(list("statistic" = Z, "p.value" = stats::pnorm(Z, lower.tail = FALSE)))
}
