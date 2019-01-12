#' Two-sample Cramer-von Mises test.
#'
#' @description Test whether two samples have same distributuion. \code{CVM.test} will calculate the value of statistic W and return as a list (W0, p-value).
#' @export
#' @importFrom stats ecdf
#' @importFrom stats var
#' @param x the first sample.
#' @param y the second sample.
#' @return a named list with W0 and p-value of permutation test.
#'
#' @examples
#'
#' CVM.test(rnorm(10), rnorm(20))
#'
CVM.test = function(x, y){
  R = 499
  W = numeric(R)
  x = sort(x)
  y = sort(y)
  t = length(x) + length(y)
  K = 1:t
  z = c(x, y)
  n = length(x)
  m = t-n
  options(warn = -1)

  s = 0
  for (i in 1:n) s = s + (ecdf(x)(x[i])-ecdf(y)(x[i]))^2
  for (i in 1:m) s = s + (ecdf(x)(y[i])-ecdf(y)(y[i]))^2
  W0 = m*n*s/(m + n)^2

  for (j in 1:R) {
    k = sample(K, size = n, replace = F)
    x1 = z[k]
    y1 = z[-k]
    s1 = 0
    for (i in 1:n) s1 = s1 + (ecdf(x1)(x1[i])-ecdf(y1)(x1[i]))^2
    for (i in 1:m) s1 = s1 + (ecdf(x1)(y1[i])-ecdf(y1)(y1[i]))^2
    W[j] = m*n*s1/(m + n)^2
  }
  p = mean(c(W0, W) >= W0)
  options(warn = 0)
  list(W0 = W0, p.value = p)
}
