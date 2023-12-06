library(hypergeo)

f_pdf <- function(n, alpha, beta, k) {
  frac <- alpha/(alpha+beta)
  hg <- hypergeo(k-n+1, k+alpha, k+alpha+beta, frac)
  num <- gamma(alpha+beta) * choose(n-1, k) * (frac ** k) * gamma(k+alpha) * (1/gamma(k+alpha+beta)) * hg
  den <- gamma(alpha)
  return(num/den)
}

ks <- 0:100
ps <- f_pdf(100, 1, 5, ks)
plot(ks, ps, type="h", xlab = "Values", ylab = "Relative Probability Density", yaxt="n",
     main = "Discrete Probability Distribution")