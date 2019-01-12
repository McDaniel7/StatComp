#' Generate a Metropolis-Hastings chain with student t(df, mu) distribution or Cauchy(l, s) distribution with proposal distribution Normal(xt, sigma).
#'
#' The Normal(xt, sigma) (random walk Metropolis sampler) is an example of a Metropolis sampler. \code{MHT} generates
#' a chain with asymptotic student t(df, mu) distribution while \code{MHC} generates Cauchy(l, s) distribution.
#'
#' @export
#' @importFrom stats rcauchy
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @importFrom stats dnorm
#' @importFrom stats dcauchy
#' @importFrom stats dt
#' @param n length of chain. (better if not so small).
#' @param df degrees of freedom (maybe non-integer) of t distribution.
#' @param ncp non-centrality parameter. If omitted, use the central t distribution.
#' @param a initial value of the chain. 1 is default.
#' @param sigma sd of proposal distribution. 1 is default.
#' @return return a numerical vector (chain) with length n.
#' @examples
#'
#' \dontrun{
#' ## a M-H chain with central t(3) distribution with length 1000
#' MHT(1000, 3, sigma = 2)
#'
#' ## a M-H chain with Cauchy(0, 1), stored in x.
#' n = 2000
#' x = MHC(n, 0, 1, a = -5)
#'
#' ## and you can see how the chain converge.
#' plot(x, type = "l", xlab = "x", xlim = 1:n, ylim = range(x))
#' }
#'

MHT = function(n, df, ncp = NULL, a = 1, sigma = 1) {
  x = numeric(n)
  x[1] = a
  u = runif(n)
  for (i in 2:n) {
    y = rnorm(1, x[i-1], sigma)
    if (is.null(ncp)) {
      if (u[i] <= (dt(y, df) / dt(x[i-1], df))) x[i] = y
      else x[i] = x[i-1]
    }
    else {
      if (u[i] <= (dt(y, df, ncp) / dt(x[i-1], df, ncp))) x[i] = y
      else x[i] = x[i-1]
    }
  }
  x
}

#' @describeIn MHT replace t distribution with Cauchy distribution
#' @export
#' @param l location of Cauchy distribution.
#' @param s scale of Cauchy distribution.
#' @return a vector (chain) of Cauchy distribution.
#'

MHC = function(n, l, s, a = 1, sigma = 1){
  x = numeric(n)
  x[1] = a
  u = runif(n)
  for (i in 2:n) {
    y = rnorm(1, x[i-1], sigma)
    if (u[i] <= (dcauchy(y, l, s) / dcauchy(x[i-1], l, s))) x[i] = y
    else x[i] = x[i-1]
  }
  x
}
