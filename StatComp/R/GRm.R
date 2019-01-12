#' Monitor convergence of a M-H chain by Gelman-Rubin method.
#'
#' \code{GRm} use Gelman-Rubin method, which is based on comparing the behavior of several generated chains with respect to the variance of one or more scalar summary statistics. The
#' es- timates of the variance of the statistic are analogous to estimates based on between-sample and within-sample mean squared errors in a one-way analysis of variance(ANOVA).
#' @export
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics par
#' @importFrom graphics plot
#' @param M a matrix with k(nrow) chains and each chain have length n(ncol).
#' @param f the method to compute the scalar summary statistic that estimate some parameters of target distribution. The argument of f should be a numeric vector.
#' @param burn burn-in time of the chain.
#' @param graphic logical; FALSE is default. If TRUE, show the plot of Gelman-Rubin statistic sequence.
#' @return a numeric value of Gelman-Rubin statistic.
#' @examples
#'
#' \dontrun{
#' ## normal.chain is a funtion to generate M-H chain.
#' normal.chain = function(w, N, X1) {
#' x = rep(0, N)
#' x[1] = X1
#' prob <- function(y, a) {
#'   if (y < 0 || y >= w) return (0)
#'   return((0.5 + y/4)^a[1] * ((1-y)/4)^a[2] * ((1-y)/4)^a[3] * (y/4)^a[4])
#' }
#' for (i in 2:N) {
#'   u = runif(1)
#'   y = runif(1, 0, w)
#'   if(u < prob(y, type)/prob(x[i-1], type)) x[i] = y/w
#'   else x[i] = x[i-1]
#' }
#' return(x)
#' }
#'
#' X = matrix(0, nrow=k, ncol=n)
#'
#' ## x0 is the vector of initial values with length k.
#' for (i in 1:k) X[i, ] = normal.chain(w, n, x0[i])
#'
#' GRm(X, mean, burn) ## mean represents the \emph{mean} funciton.
#'
#' GRm(X, mean, burn, graphic = T) ## plot G-R statistic sequence at the same time.
#' }
#'

GRm = function(M, f, burn, graphic = FALSE) {
  n = ncol(M)
  k = nrow(M)

  psi = matrix(NA, nrow = k, ncol = n)
  psi[, 1] = M[, 1]
  for (i in 2:n) {
    psi[, i] = t(apply(M[, 1:i], 1, f))
  }

  psi.means = rowMeans(psi)
  B = n * var(psi.means)
  psi.w = apply(psi, 1, "var")
  W = mean(psi.w)
  v.hat = W*(n-1)/n + (B/n)
  r.hat = v.hat / W

  if (graphic == TRUE) {
    par(mfrow=c(1,1))
    rhat = rep(0, n)
    for (j in (burn+1):n) {
      psi1 = psi[, 1:j]
      n1 = ncol(psi1)
      psi.means = rowMeans(psi1)
      B = n1 * var(psi.means)
      psi.w = apply(psi1, 1, "var")
      psi.w = apply(psi1, 1, "var")
      W = mean(psi.w)
      v.hat = W*(n1-1)/n1 + (B/n1)
      rhat[j] = v.hat / W
    }
    plot((burn+1):n, rhat[(burn+1):n], type="l", xlab="", ylab="R", xaxt = "n")
    axis(1, at = seq(burn, n, length.out = 5))
    abline(h=1.2, lty=2)
  }
  return(r.hat)
}
