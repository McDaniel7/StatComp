## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----matrix--------------------------------------------------------------
a = matrix(1:12, nrow = 6, byrow = T)
b = rbind(c(1, 2, 3), c(4, 5, 6))
(c = a %*% b)
colnames(c) = c("col1", "col2", "col3")
c

## ----plot----------------------------------------------------------------
x = rnorm(10, sd = 0.7)
y = rnorm(10, sd = 0.7)
opar = par()
par(bg="wheat", mar=c(3, 2, 2.5, 0.25))
plot(x, y, type="n", xlab="", ylab="", xlim=c(-2, 2),
     ylim=c(-2, 2), xaxt="n", yaxt="n")
rect(-3, -3, 3 ,3, col = "snow")
points(x, y, pch = 23, col = "black", bg = "red")
axis(side=1, c(-2, 0, 2), labels=TRUE)
axis(side=2, c(-2, 0, 2), labels=TRUE)
title("How to customize a plot with R")
mtext("Ten random values", side=1,line = 1, at=1.5, cex=0.9, font=3)
mtext("Ten other values", line=0.5, at=-1.8, cex=0.9, font=3)

## ----analysis, eval= FALSE-----------------------------------------------
#  data = InsectSprays
#  aov.spray = aov(sqrt(count) ~ spray, data)
#  str(aov.spray)
#  layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
#  plot(aov.spray)

## ----inverse transform---------------------------------------------------
x = 0:4
p = c(0.1, 0.2, 0.2, 0.2, 0.3)
cp = cumsum(p) # sum by interval
m = 1000
r = numeric(m)

## ----inverse transform2--------------------------------------------------
(r = x[findInterval(runif(m), cp) + 1])
hist(r, breaks = seq(-0.5, 4.5, 1))

## ----sample function-----------------------------------------------------
r = sample(0:4, 1000, replace = T, prob = c(0.1, 0.2, 0.2, 0.2, 0.3))
hist(r, breaks = seq(-0.5, 4.5, 1))

## ----A-R method----------------------------------------------------------
G <- function(n, a, b){
  j = k = 0
  y = numeric(n)
  while(k < n){
    u = runif(1)
    j = j + 1 # record the total number of times to generate a sample of size n
    x = runif(1)
    if(x^(a - 1) * (1 - x)^(b - 1) > u) {
      # accept x
      k = k + 1
      y[k] = x
    }
  }
  list(j, y)
}

## ----A-R method 2--------------------------------------------------------
x = G(1000, 3, 2)
t = x[[1]]
(r = x[[2]])
hist(r, freq = F, ylim = c(0, 2))
curve(expr = 12 * x^2 *(1 - x), 0, 1, type = "l", add = TRUE) # add the curve of the probability density function.

## ----A-R method 3--------------------------------------------------------
hist(r, freq = F, ylim = c(0, 2), breaks = seq(0, 1, 0.05))
curve(expr = 12 * x^2 *(1 - x), 0, 1, type = "l", add = TRUE)

## ----A-R method 4--------------------------------------------------------
t
1000/t

## ----mixture-------------------------------------------------------------
n = 1000; r = 4; beta = 2
lambda = rgamma(n, r, beta) # a sample of lambda
(x = rexp(n, lambda))
hist(x, freq = F, breaks = seq(0, max(max(x)+1, 20), 1)) # max(max()) not only ensures breaks can cover the range of the x but also makes the xlim of x and y similar.

## ----mixture 2-----------------------------------------------------------
u = runif(n)
z = (1 - u)^(1/r)
y = beta / z - beta
hist(y, freq = F, breaks = seq(0, max(max(y)+1, 20), 1))

## ----5.4-----------------------------------------------------------------
Beta_es = function(x) {
  m = 10000
  t = runif(m, min = 0, max = x)
  g = function(y) x * y^2 * (1 - y)^2 / beta(3, 3)
  theta_hat = mean(g(t))
  theta_hat
} # Monte Carlo estimate

## ----5.4(2)--------------------------------------------------------------
x = 1:9
y1 = NULL
for (i in x) y1 = c(y1, Beta_es(i/10))
y2 = pbeta(x/10, 3, 3)
A = rbind(y1 = round(y1, 5), y2)
rownames(A) = c("Beta.es", "pbeta")
colnames(A) = c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")

## ----5.4(3)--------------------------------------------------------------
A

## ----5.9-----------------------------------------------------------------
G = function(m, sigma) {
  u = runif(m/2)
  x1 = sqrt(-2 * sigma^2 * log(1 - u))
  x_ = sqrt(-2 * sigma^2 * log(u))
  c(x1, x_)
} # generating function
m = 1e4
sigma = 3
result = G(m, sigma)
x1 = result[1:(m/2)]
x_ = result[(m/2 + 1):m]
var1 = (sd((x1 + x_)/2))^2
x2 = sqrt(-2 * sigma^2 * log(1 - runif(m/2)))
var2 = (sd((x1 + x2)/2))^2
c(var1, var2)

## ----5.9(2)--------------------------------------------------------------
100*(var2-var1)/var2

## ----5.13----------------------------------------------------------------
m = 1e4
u = runif(m)
theta_hat = se = numeric(2)

## ----5.13(2)-------------------------------------------------------------
x = 1/(1 - u) # using f1
fg1 = x^4 * exp(-x^2/2) / sqrt(2 * pi)
theta_hat[1] = mean(fg1)
se[1] = sd(fg1)

## ----5.13(3)-------------------------------------------------------------
x = sqrt(1 - 2 * log(1 - u)) # using f2
fg2 = x * exp(-1/2) / sqrt(2 * pi)
theta_hat[2] = mean(fg2)
se[2] = sd(fg2)

## ----5.13(4)-------------------------------------------------------------
B = rbind(theta_hat, se)
rownames(B) = c("theta", "se")
colnames(B) = c("f1", "f2")
B

## ----5.14----------------------------------------------------------------
m = 1e4
u = runif(m)
x = sqrt(1 - 2 * log(1 - u))
fg2 = x * exp(-1/2) / sqrt(2 * pi)
(theta_hat = mean(fg2))

## ----6.9-----------------------------------------------------------------
sym = function(x) {
  z = quantile(x, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  z = c(z, mean(x), median(x))
  names(z) = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "Mean", "Median")
  z
}

## ----6.9(2)--------------------------------------------------------------
n = 100
m = 100
G_r1 = G_r2 = G_r3 = numeric(m)
for (i in 1:m) {
  x = rlnorm(n)
  u = mean(x)
  x = sort(x)
  sum = 0
  for (j in 1:n) {sum = sum + (2 * j - n - 1) * x[j]}
  G_r1[i] = sum / 10000 / u
}
sym(G_r1)
hist(G_r1, freq = F, main = "Histogram of lognormal")

## ----6.9(3)--------------------------------------------------------------
for (i in 1:m) {
  x = runif(n)
  u = mean(x)
  x = sort(x)
  sum = 0
  for (j in 1:n) {sum = sum + (2 * j - n - 1) * x[j]}
  G_r2[i] = sum / 10000 / u
}
sym(G_r2)
hist(G_r2, freq = F, main = "Histogram of uniform")

## ----6.9(4)--------------------------------------------------------------
for (i in 1:m) {
  x = sample(0:1, n, rep = T, prob = c(0.9, 0.1))
  u = mean(x)
  x = sort(x)
  sum = 0
  for (j in 1:n) {sum = sum + (2 * j - n - 1) * x[j]}
  G_r3[i] = sum / 10000 / u
}
sym(G_r3)
hist(G_r3, freq = F, main = "Histogram of Bernoulli")

## ----6.9(5)--------------------------------------------------------------
G1 = mean(G_r1)
G2 = mean(G_r2)
G_l = (pnorm(1/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
G_u = 1/3
matrix(c(G1, G2, G_l, G_u), nrow = 2, byrow = T, dimnames = (list(c("theoretical", "empirical"), c("lognormal", "uniform"))))

## ----6.10----------------------------------------------------------------
mu = 0
sigma = 1
M = 1000
n = 50
G_hat = Sig1 = Sig2 = Mu_hat = Sigma_hat = result = G1 = G2 = numeric(M)
for (i in 1:M) {
  x = rlnorm(n, mu, sigma)
  y = log(x)
  S = 0
  y_ = mean(y)
  for (j in 1:length(y)) S = S + (y[j] - y_)^2
  S = sqrt(S / (n - 1))
  Mu_hat[i] = y_
  Sigma_hat[i] = S
  Sig1[i] = sqrt((n - 1) * S^2 / qchisq(0.025, n - 1, lower.tail = FALSE))
  Sig2[i] = sqrt((n - 1) * S^2 / qchisq(1 - 0.025, n - 1, lower.tail = FALSE))
  G_hat[i] = (pnorm(Sigma_hat[i]/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
}

## ----6.10(2)-------------------------------------------------------------
for (i in 1:M){
  G1[i] = (pnorm(Sig1[i]/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
  G2[i] = (pnorm(Sig2[i]/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
}

## ----6.10(3)-------------------------------------------------------------
EG = mean(G_hat)
G = (pnorm(1/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
c = c(EG, G)
names(c) = c("E[G]", "theoretical G")
c

## ----6.10(4)-------------------------------------------------------------
m = 1e3
mu_hat = mean(Mu_hat)
sigma_hat = mean(Sigma_hat)
c(mu_hat, sigma_hat)

## ----6.10(5)-------------------------------------------------------------
for (i in 1:M) {
  if ((G1[i] < G) && (G2[i] > G)) result[i] = 1
  else result[i] = 0
}
mean(result)

## ----6.10(6)-------------------------------------------------------------
library(boot)
library(MASS)
mu = 0
sigma = 1
n = 50
M = 200
Gini <- function(x) {
  x = sort(x)
  u = mean(x)
  sum = 0
  for (j in 1:length(x)) {sum = sum + (2 * j - length(x) - 1) * x[j]}
  G = sum / length(x)^2 / u
  G
}
boot.Gini = function(x, i) Gini(x[i])
ci.norm<-ci.basic<-ci.perc<-matrix(NA,M,2)
G = (pnorm(1/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
for (k in 1:M) {
  x = rlnorm(n)
  de <- boot(data=x,statistic=boot.Gini, R = 500)
  ci <- boot.ci(de,type=c("norm","basic","perc"))     
  ci.norm[k,]<-ci$norm[2:3]
  ci.basic[k,]<-ci$basic[4:5]
  ci.perc[k,]<-ci$percent[4:5]
}
  cat('norm =',mean(ci.norm[,1]<=G & ci.norm[,2]>=G),'basic =',mean(ci.basic[,1]<=G & ci.basic[,2]>=G), 'perc =',mean(ci.perc[,1]<=G & ci.perc[,2]>=G))

## ----6.10(7)-------------------------------------------------------------
library(boot)
library(MASS)
mu = 0
sigma = 1
n = 200
M = 200
Gini <- function(x) {
  x = sort(x)
  u = mean(x)
  sum = 0
  for (j in 1:length(x)) {sum = sum + (2 * j - length(x) - 1) * x[j]}
  G = sum / length(x)^2 / u
  G
}
boot.Gini = function(x, i) Gini(x[i])
ci.norm<-ci.basic<-ci.perc<-matrix(NA,M,2)
G = (pnorm(1/2, mean = 0, sd = sqrt(0.5)) - 0.5) * 2
for (k in 1:M) {
  x = rlnorm(n)
  de <- boot(data=x,statistic=boot.Gini, R = 500)
  ci <- boot.ci(de,type=c("norm","basic","perc"))     
  ci.norm[k,]<-ci$norm[2:3]
  ci.basic[k,]<-ci$basic[4:5]
  ci.perc[k,]<-ci$percent[4:5]
}
  cat('norm =',mean(ci.norm[,1]<=G & ci.norm[,2]>=G),'basic =',mean(ci.basic[,1]<=G & ci.basic[,2]>=G), 'perc =',mean(ci.perc[,1]<=G & ci.perc[,2]>=G))

## ----6.B-----------------------------------------------------------------
library(MASS)
m = 1000
result1 = result2 = result3 = numeric(m)
for (i in 1:m) {
  Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2, byrow = T)
  A = mvrnorm(n = 50, rep(0, 2), Sigma)
  x = A[, 1]
  y = A[, 2]
  c1 = cor.test(x, y, method = "pearson", alternative = "t")
  c2 = cor.test(x, y, method = "kendall", alternative = "t")
  c3 = cor.test(x, y, method = "spearm", alternative = "t")
  result1[i] = c1$p.value
  result2[i] = c2$p.value
  result3[i] = c3$p.value
}
mean(result1[] < 0.05)
mean(result2[] < 0.05)
mean(result3[] < 0.05)

## ----6.B(2)--------------------------------------------------------------
m = 1000
result1 = result2 = result3 = numeric(m)
for (i in 1:m) {
  x = runif(50)
  y = 1 / x
  c1 = cor.test(x, y, method = "pearson", alternative = "t")
  c2 = cor.test(x, y, method = "kendall", alternative = "t")
  c3 = cor.test(x, y, method = "spearm", alternative = "t")
  result1[i] = c1$p.value
  result2[i] = c2$p.value
  result3[i] = c3$p.value
}
mean(result1[] < 0.05)
mean(result2[] < 0.05)
mean(result3[] < 0.05)

## ----7.1(1)--------------------------------------------------------------
library(bootstrap) # for the law data
n = length(law$LSAT) # the times of jackknife of sample 'law'
m = length(law82$LSAT) # the times of jackknife of sample 'law82'

## ----7.1(2)--------------------------------------------------------------
cor1.hat = cor(law$LSAT, law$GPA)
cor1.jack = numeric(n)
for (i in 1:n) cor1.jack[i] = cor(law$LSAT[-i], law$GPA[-i])
bias1.jack = (n-1) * (mean(cor1.jack) - cor1.hat)
se1.jack = sqrt((n-1)*mean((cor1.jack-cor1.hat)^2))
x = c(cor1.hat, bias1.jack, se1.jack)
names(x) = c("cor", "bias", "se")
x # the law data

cor2.hat = cor(law82$LSAT, law82$GPA)
cor2.jack = numeric(m)
for (i in 1:m) cor2.jack[i] = cor(law82$LSAT[-i], law82$GPA[-i])
bias2.jack = (m-1) * (mean(cor2.jack) - cor2.hat)
se2.jack = sqrt((m-1)*mean((cor2.jack-cor2.hat)^2))
y = c(cor2.hat, bias2.jack, se2.jack)
names(y) = c("cor", "bias", "se")
y # the law82 data

## ----7.5(1)--------------------------------------------------------------
library(boot)
R = 1000 # times in one bootstrap
m = 1000 # total times of bootstrap

## ----7.5(2)--------------------------------------------------------------
boot.rate = function(x,i) mean(x[i])
ci.norm = ci.basic = ci.perc = ci.bca = matrix(NA,m,2)
for (i in 1:m) {
  de = boot(data=aircondit[,1], statistic=boot.rate, R)
  ci = boot.ci(de, type=c("norm","basic","perc","bca"))
  ci.norm[i,] = ci$norm[2:3]
  ci.basic[i,] = ci$basic[4:5]
  ci.perc[i,] = ci$percent[4:5]
  ci.bca[i,] = ci$bca[4:5]
}
  norm = c(mean(ci.norm[,1]), mean(ci.norm[,2]))
  basic = c(mean(ci.basic[,1]), mean(ci.basic[,2]))
  perc = c(mean(ci.perc[,1]), mean(ci.perc[,2]))
  bca = c(mean(ci.bca[,1]), mean(ci.bca[,2]))
  A = rbind(norm, basic, perc, bca)
  row.names(A) = c("norm", "basic", "perc", "bca")
  A

## ----7.8(1)--------------------------------------------------------------
n = 88 # the length of scor is the times of jackknife

## ----7.8(2)--------------------------------------------------------------
theta.hat = eigen(cov(scor))$value[1]/sum(eigen(cov(scor))$value)
theta.jack = numeric(n)
for (i in 1:n) theta.jack[i] = eigen(cov(scor[-i,]))$value[1]/sum(eigen(cov(scor[-i,]))$value)
bias.jack = (n-1) * (mean(theta.jack) - theta.hat)
se.jack = sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(theta=theta.hat,bias=bias.jack,se=se.jack),6)

## ----7.11(1)-------------------------------------------------------------
library(DAAG)
attach(ironslag)
n = length(magnetic) # length of sample

## ----7.11(2)-------------------------------------------------------------
e1 = e2 = e3 = e4 = numeric(n*(n-1)/2)

# fitting models on leave-two-out samples
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    y = magnetic[c(-i, -j)]
    x = chemical[c(-i, -j)]
    
    J1 = lm(y ~ x)
    yhat1.1 = J1$coef[1] + J1$coef[2] * chemical[i]
    yhat1.2 = J1$coef[1] + J1$coef[2] * chemical[j]
    e1[(2*n-i)*(i-1)/2 + j - i] = (magnetic[i] - yhat1.1 + magnetic[j] - yhat1.2) / 2
    
    J2 = lm(y ~ x + I(x^2))
    yhat2.1 = J2$coef[1] + J2$coef[2] * chemical[i] + J2$coef[3] * chemical[i] ^ 2
    yhat2.2 = J2$coef[1] + J2$coef[2] * chemical[j] + J2$coef[3] * chemical[j] ^ 2
    e2[(2*n-i)*(i-1)/2 + j - i] = (magnetic[i] - yhat2.1 + magnetic[j] - yhat2.2) / 2
    
    J3 = lm(log(y) ~ x) 
    logyhat3.1 = J3$coef[1] + + J3$coef[2] * chemical[i]
    logyhat3.2 = J3$coef[1] + + J3$coef[2] * chemical[j]
    yhat3.1 = exp(logyhat3.1)
    yhat3.2 = exp(logyhat3.2) 
    e3[(2*n-i)*(i-1)/2 + j - i] = (magnetic[i] - yhat3.1 + magnetic[j] - yhat3.2) / 2
    
    J4 = lm(log(y) ~ log(x))
    logyhat4.1 = J4$coef[1] + J4$coef[2] * log(chemical[i])
    logyhat4.2 = J4$coef[1] + J4$coef[2] * log(chemical[j])
    yhat4.1 = exp(logyhat4.1)
    yhat4.2 = exp(logyhat4.2)
    e4[(2*n-i)*(i-1)/2 + j - i] = (magnetic[i] - yhat4.1 + magnetic[j] - yhat4.2) / 2
  }
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ----16q1(1)-------------------------------------------------------------
library(nortest)
R = 999
W = numeric(R)
attach(chickwts)
x = sort(as.vector(weight[feed == "soybean"]))
y = sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
K = 1:26
z = c(x, y)
n = length(x)
m = 26-n
options(warn = -1)

## ----16q1(2)-------------------------------------------------------------
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
p

hist(W, main = "", freq = FALSE, xlab = "W (p = 0.42)", breaks = "scott")
points(W0, 0, cex = 1, pch = 16, col = "red")

## ----16q2(1)-------------------------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)
library(MASS)
set.seed(1)
prob = 1
m = 1e2
k = 3
p = 2
mu = 0.1
sd = 2
n1 = 50
n2 = 40
R = 99
n = n1+n2
N = c(n1,n2)

## ----16q2(2)-------------------------------------------------------------
Tn = function(z, ix, sizes, k){
  n1 = sizes[1]
  n2 = sizes[2]
  z = z[ix, ]
  NN = nn2(data = z, k = k+1)
  block1 = NN$nn.idx[1:n1, -1]
  block2 = NN$nn.idx[(n1+1):n, -1]
  i1 = sum(block1 < n1+.5)
  i2 = sum(block2 > n1+.5)
  (i1+i2)/(k*n)
}

eqdist.nn = function(z, sizes, k, R){
  boot.obj = boot(data = Z, statistic = Tn, R = R, sim = 'permutation', sizes = sizes, k = 3)
  ts = c(boot.obj$t0, boot.obj$t)
  p.value = mean(ts>=ts[1])
  list(statistic = ts[1], p.value = p.value)
}

p.values = matrix(NA, m ,3)
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2), rnorm(n2, sd = sd))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(3)-------------------------------------------------------------
mu = 0.1
sd = 1.5
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = mu, sd = sd), rnorm(n2, mean = mu, sd = sd))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(4.1)-----------------------------------------------------------
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = mu/2, sd = sd - 0.4), rnorm(n2, mean = mu/2, sd = sd - 0.4)) # mu/2 = 0.05   sd -0.4 = 1.1
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(4.2)-----------------------------------------------------------
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = mu/2, sd = sd - 0.2), rnorm(n2, mean = mu/2, sd = sd - 0.2)) # mu = 0.1   sd - 0.2 = 1.3
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(5)-------------------------------------------------------------
for(i in 1:m){
  X = matrix(rt(n1*p, df = 1), ncol = p)
  Y = cbind(rt(n2, df = 1.5), rt(n2, df = 1.5))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(6)-------------------------------------------------------------
for(i in 1:m){
  X = matrix(rt(n1*p, df = 1), ncol = p)
  Y = cbind(rt(n2, df = 3), rt(n2, df = 3))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(7)-------------------------------------------------------------
for(i in 1:m){
  p1 = 0.3
  p2 = 0.6
  U1 = runif(n1)
  U2 = runif(n2)
  X = ((U1<p1)*mvrnorm(n1, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U1<p1))*mvrnorm(n1, mu = c(1,1), Sigma = matrix(c(1.5,1.2,1.2,1), nrow = 2)))
  Y = ((U2<p2)*mvrnorm(n2, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U2<p2))*mvrnorm(n2, mu = c(1,1), Sigma = matrix(c(1.5,1.2,1.2,1), nrow = 2)))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(8)-------------------------------------------------------------
for(i in 1:m){
  p1 = 0.4
  p2 = 0.4
  U1 = runif(n1)
  U2 = runif(n2)
  X = ((U1<p1)*mvrnorm(n1, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U1<p1))*mvrnorm(n1, mu = c(1,1), Sigma = matrix(c(1,0,0,1), nrow = 2)))
  Y = ((U2<p2)*mvrnorm(n2, mu = c(1,0.5), Sigma = diag(c(2,1)))+(1-(U2<p2))*mvrnorm(n2, mu = c(1,1), Sigma = matrix(c(1.5,1.2,1.2,1), nrow = 2)))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----16q2(9)-------------------------------------------------------------
mu = 0.1
sd = 1.5
n1 = 100
n2 = 10
N = c(n1, n2)
n = n1+n2
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = mu, sd = sd), rnorm(n2, mean = mu, sd = sd))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ----16q2(10)------------------------------------------------------------
mu = 0.1
sd = 1.5
n1 = 100
n2 = 10
N = c(n1, n2)
n = n1+n2
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = mu, sd = sd-0.2), rnorm(n2, mean = mu, sd = sd-0.2))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ----16q3(1)-------------------------------------------------------------
sigma = 5
N = 100000 # size of MC sample.
k = 0 # accept rate

## ----16q3(2)-------------------------------------------------------------
x = numeric(N)
x[1] = rnorm(1, sd = sigma)
for (i in 2:N) {
  y = rnorm(1, mean = x[i-1], sd = sigma)
  u = runif(1)
  r = dcauchy(y) * dnorm(x[i-1], mean = y, sd = sigma) / (dcauchy(x[i-1]) * dnorm(y, mean = x[i-1], sd = sigma))
  if (u < r) {x[i] = y;k = k+1}
  else x[i] = x[i-1]
}
xp = x[10001:N]
plot(x, type = "l", xlab = "x", xlim = c(1, N), ylim = range(x))

k / (N-1)

c = (1:9) / 10
qc = qcauchy(c)
qx = quantile(xp, c)
qqplot(qc, qx, xlab = "Cauchy Quantiles", ylab = "Sample Quantiles")
g = function(x) {x}
curve(g, add = T, type = "l", col = "red")

c = (5:95) / 100
qc = qcauchy(c)
qx = quantile(x, c)
qqplot(qc, qx, xlab = "Cauchy Quantiles", ylab = "Sample Quantiles")
g = function(x) {x}
curve(g, add = T, type = "l", col = "red")

hist(x[(x>-20)&(x<20)], freq = F, breaks = seq(-20.5, 20.5, 0.5))
curve(dcauchy, add = T, col = "red")

## ----16q4(1)-------------------------------------------------------------
N = 5000 # length of MC.
burn = 1000 # burn-in time
w = 1 # width of the uniform support set
type = c(125, 18, 20, 34)

## ----16q4(2)-------------------------------------------------------------
x = numeric(N)
x[1] = 0.01
prob <- function(y, a) {
if (y < 0 || y >= 1) return (0) 
  return((0.5 + y/4)^a[1] * ((1-y)/4)^a[2] * ((1-y)/4)^a[3] * (y/4)^a[4])
}
for (i in 2:N) {
  u = runif(1)
  y = runif(1, 0, w)
  if(u < prob(y, type)/prob(x[i-1], type)) x[i] = y
  else x[i] = x[i-1]
}
print(round(type/sum(type), 3))
xp = x[(burn+1):N]
theta = mean(xp)
print(c("empirical theta: ", theta))
print(round(c(0.5 + theta/4, (1-theta)/4, (1-theta)/4, theta/4), 3))

plot(x, type = "l", xlab = "x", xlim = c(1, N), ylim = range(x))
hist(xp, freq = F)

## ----23q1(1)-------------------------------------------------------------
n = 2000 # length of each MC chain.
b = 100 # burn-in time
w = 1 # width of the uniform support set
type = c(125, 18, 20, 34)
k = 4 # number of chains to generate
x0 = c(-10, -5, 5, 10) #initial values for each chain

## ----23q1(2), eval=FALSE-------------------------------------------------
#  Gelman.Rubin = function(psi) {
#    psi = as.matrix(psi)
#    n = ncol(psi)
#    k = nrow(psi)
#    psi.means = rowMeans(psi)
#    B = n * var(psi.means)
#    psi.w = apply(psi, 1, "var")
#    W = mean(psi.w)
#    v.hat = W*(n-1)/n + (B/n)
#    r.hat = v.hat / W
#    return(r.hat)
#  }
#  
#  normal.chain = function(w, N, X1) {
#    x = rep(0, N)
#    x[1] = X1
#    prob <- function(y, a) {
#  if (y < 0 || y >= w) return (0)
#    return((0.5 + y/4)^a[1] * ((1-y)/4)^a[2] * ((1-y)/4)^a[3] * (y/4)^a[4])
#    }
#    for (i in 2:N) {
#    u = runif(1)
#    y = runif(1, 0, w)
#    if(u < prob(y, type)/prob(x[i-1], type)) x[i] = y/w
#    else x[i] = x[i-1]
#    }
#  
#    return(x)
#  }
#  
#  X = matrix(0, nrow=k, ncol=n)
#  for (i in 1:k) X[i, ] = normal.chain(w, n, x0[i])
#  psi = t(apply(X, 1, cumsum))
#  for (i in 1:nrow(psi)) psi[i,] = psi[i,] / (1:ncol(psi))
#  print(Gelman.Rubin(psi)) # R_hat
#  
#  par(mfrow=c(2,2)) # plot psi for the four chain
#  for (i in 1:k) plot(psi[i, (b+1):n], type="l", xlab=i, ylab=bquote(psi))
#  par(mfrow=c(1,1))
#  
#  rhat = rep(0, n) # plot the sequence of R_hat
#  for (j in (b+1):n) rhat[j] = Gelman.Rubin(psi[,1:j])
#  plot((b+1):n, rhat[(b+1):n], type="l", xlab="", ylab="R", xaxt = "n")
#  axis(1, at = c(100, 500, 1000, 1500, 2000))
#  abline(h=1.2, lty=2)

## ----23q2(1)-------------------------------------------------------------
Sk = function(k, a)  pt(sqrt(a^2*k/(k+1-a^2)), k, lower.tail = F, log.p = T)

A = matrix(NA, 25, 2)
k = c(4:25, 100, 500, 1000)
for (i in 1:25) {
  solution = uniroot(function(a)  {pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)), k[i]-1, lower.tail = F, log.p = T) - pt(sqrt(a^2*k[i]/(k[i]+1-a^2)), k[i], lower.tail = F, log.p = T)}, c(0.0001, sqrt(k[i])-0.0001))
  A[i,] = c(solution$root, Sk(k[i], solution$root))
}
A

plot(A[,1], A[,2], type = "p", col = "red")

## ----23q2(2), eval=FALSE-------------------------------------------------
#  N = 1e6
#  B = matrix(NA, 25, 2)
#  for (i in 1:25) {
#    g = function(a) {
#      x = rt(N, k[i]-1)
#      y = rt(N, k[i])
#      mean(log(x[x>0]) > 0.5*(2*log(a) + log(k[i]-1)-log(k[i]-a^2))) - mean(log(y[y>0]) > 0.5*(2*log(a) + log(k[i])-log(k[i]+1-a^2)))
#    }
#    solution2 = uniroot(g, c(0.1, sqrt(k[i])-0.1))
#    B[i,] = c(solution2$root, Sk(k[i], solution2$root))
#  }
#  B
#  
#  plot(B[,1], B[,2], type = "p", col = "red")

## ----30q1(1)-------------------------------------------------------------
eta = 0
theta = seq(1, 5, length.out = 12)
n = 50 # the number of quantiles

## ----30q1(2), eval=FALSE-------------------------------------------------
#  f = function(x, theta, eta) {1/(theta*pi*(1+((x-eta)/theta)^2))}
#  v = matrix(NA, length(theta), n)
#  q = seq(-10, 10, length.out = n)
#  for (i in 1:length(theta)) {
#    for (j in 1:n) {
#      v[i, j] = integrate(f, lower = -Inf, upper = q[j], rel.tol=.Machine$double.eps^0.25, theta = theta[i], eta)$value
#    }
#  }
#  par(mfrow = c(3, 4))
#  g = function(x) {x}
#  for (i in 1:12) {
#    qc = pcauchy(q, scale = theta[i])
#    qqplot(qc, v[i, ], xlab = "Cauchy Quantiles", ylab = "Integration Quantiles")
#    curve(g, add = T, type = "l", col = "red")
#  }

## ----echo=FALSE----------------------------------------------------------
dat <- rbind(Genotype=c('AA','BB','OO','AO','BO','AB','Sum'),
             Frequency=c('p^2','q^2','r^2','2pr','2qr','2pq',1),
             Count=c('nAA','nBB','nOO','nAO','nBO','nAB','n'))
knitr::kable(dat,format='latex')

## ----30q2(1)-------------------------------------------------------------
nA_ = 28
nB_ = 24
nOO = 41
nAB = 70
l = NULL
flag = T

## ----30q2(2)-------------------------------------------------------------
lv = function(p,q,r) nA_*log(p^2+2*p*r)+nB_*log(q^2+2*q*r)+2*nOO*log(r)+nAB*log(2*p*q) # log-maximum likelihood values

p1 = 0.4 # initial value
p2 = 0.4 # initial value
nAA = nA_*p1
nBB = nB_*p2
nAO = nA_-nAA
nBO = nB_-nBB
s = (nA_+nB_+nAB+nOO)*2
p = (2*nAA+nAO+nAB) / s
q = (2*nBB+nBO+nAB) / s
r = (2*nOO+nAO+nBO) / s
l = c(l, lv(p,q,r)) # first log-maximum likelihood values

EM = function(){
  time = 1
  while(flag == T){
    p1 = p/(p+2*r)
    p2 = q/(q+2*r)
    nAA = nA_*p1
    nBB = nB_*p2
    nAO = nA_-nAA
    nBO = nB_-nBB
    pnew = (2*nAA+nAO+nAB) / s
    qnew = (2*nBB+nBO+nAB) / s
    rnew = (2*nOO+nAO+nBO) / s
    flag = max(abs(pnew-p), abs(qnew-q), abs(rnew-r))>1e-12
    time = time + 1
    p = pnew
    q = qnew
    r = rnew
    l = c(l, lv(p,q,r))
  }
  list(p = p,q = q,r = r, time = time, lv = l)
}
result = EM()
result
plot(result$lv)

## ----7q1-----------------------------------------------------------------
attach(mtcars)

formulas = list(mpg~disp, mpg ~ I(1 / disp), mpg~disp+wt, mpg~I(1/disp)+wt)

# for loop
out1 = vector("list", length(formulas))
for (i in seq_along(formulas)) out1[[i]] = lm(formulas[[i]])
out1

# lapply()
out2 = vector("list", length(formulas))
out2 = lapply(seq_along(formulas), function(i){lm(formulas[[i]])})
out2

detach(mtcars)

## ----7q2-----------------------------------------------------------------
bootstraps = lapply(1:10, function(i) {
  rows = sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

# for loop
out3 = vector("list", length(bootstraps))
for (i in seq_along(bootstraps)) out3[[i]] = lm(mtcars$mpg~mtcars$disp, bootstraps[[i]])
out3

# lapply()
out4 = vector("list", length(formulas))
out4 = lapply(seq_along(bootstraps), function(i){lm(mtcars$mpg~mtcars$disp, bootstraps[[i]])})
out4

# without anonymous function
replicate(10, {
  rows = sample(1:nrow(mtcars), rep = TRUE)
  a = mtcars[rows, ]
  lm(a$mpg~a$disp)
}, simplify = F)

## ----7q3-----------------------------------------------------------------
rsq = function(mod) summary(mod)$r.squared

cat("out1",'\n', unlist(lapply(seq_along(out1), function(i) {rsq(out1[[i]])})), '\n')

cat("out2",'\n', unlist(lapply(seq_along(out2), function(i) {rsq(out2[[i]])})), '\n')

cat("out3",'\n', unlist(lapply(seq_along(out3), function(i) {rsq(out3[[i]])})), '\n')

cat("out4",'\n', unlist(lapply(seq_along(out4), function(i) {rsq(out4[[i]])})), '\n')

## ----7q4-----------------------------------------------------------------
trials = replicate(100,t.test(rpois(10, 10), rpois(7, 10)),simplify = FALSE)
sapply(seq_along(trials), function(i){trials[[i]]$p.value})

# without anonymous function
for(i in 1:length(trials)) print(trials[[i]]$p.value)

## ----7q5-----------------------------------------------------------------
library(parallel)

mcvMap = function(f, FUN.VALUE, ...) {
    out = mcMap(f, ...)
    vapply(out, identity, FUN.VALUE)
}

f = function(x) c(mean(x), sd(x))
x = list(a = rnorm(20, 3), b = rnorm(30, 5))
mcvMap(f = f, c(mean=0, sd=0), x)

## ----1214----------------------------------------------------------------
library(microbenchmark)

## ----14q1----------------------------------------------------------------
quickch = function(x, y) {
  A = table(x, y)
  sumxy = rowSums(A)
  sumin = colSums(A)
  fb = sumxy %*% t(sumin) / sum(A)
  chisq = sum((A - fb)^2 / fb)
  chisq
}

chisq.test(1:5, 6:10)
quickch(1:5, 6:10)

microbenchmark(chisq.test(1:5, 6:10),quickch(1:5, 6:10))

## ----14q2----------------------------------------------------------------
quickt = function(x, y) {
  a = sort(unique(x))
  b = sort(unique(y))
  A = matrix(0, length(a), length(b), dimnames = list(a, b))
  
  for (i in 1:length(x)) {
    m = which(a == x[i])
    n = which(b == y[i])
    A[m, n] = A[m, n] + 1
  }
  
  class(A) = "table"
  A
}
x = sample(1:5, replace = T)
y = sample(1:5, replace = T)
table(x, y)
quickt(x, y)

microbenchmark(table(x, y),quickt(x, y))


quickch1 = function(x, y) {
  A = quickt(x, y)
  sumxy = rowSums(A)
  sumin = colSums(A)
  fb = sumxy %*% t(sumin) / sum(A)
  chisq = sum((A - fb)^2 / fb)
  chisq
}

microbenchmark(chisq.test(1:5, 6:10),quickch1(1:5, 6:10))

## ----14q3----------------------------------------------------------------
library(Rcpp)
sourceCpp("/Users/Daniel/Desktop/MCMC.cpp")

# Rcpp
N = 5000 
burn = 1000
type = c(125, 18, 20, 34)
x = numeric(N)
MCMC(x, type, N)

xp = x[(burn+1):N]
theta = mean(xp)
print(c("empirical theta: ", theta))
print(round(c(0.5 + theta/4, (1-theta)/4, (1-theta)/4, theta/4), 3))

plot(x, type = "l", xlab = "x", xlim = c(1, N), ylim = range(x))
hist(xp, freq = F)

# R
RMCMC = function(o) {
  prob = function(y, a) {
  if (y < 0 || y >= 1) return (0) 
    return((0.5 + y/4)^a[1] * ((1-y)/4)^a[2] * ((1-y)/4)^a[3] * (y/4)^a[4])
  }
  x = numeric(N)
  x[1] = 0.01
  for (i in 2:N) {
    u = runif(1)
    y = runif(1)
    if(u < prob(y, type)/prob(x[i-1], type)) x[i] = y
    else x[i] = x[i-1]
  }
  x
}

x2 = numeric(N)
x2 = RMCMC()

xp2 = x2[(burn+1):N]
plot(x2, type = "l", xlab = "x2", xlim = c(1, N), ylim = range(x2))
hist(xp2, freq = F)

a = seq(0, 100, by = 2)/100
qqplot(quantile(xp, a), quantile(xp2, a))
g = function(x) x
curve(g, add = T, col = "red")

microbenchmark(MCMC(x, type, N), RMCMC())

