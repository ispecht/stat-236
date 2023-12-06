library(hypergeo)

# this is really numerically instable
f_pdf <- function(n, alpha, beta, k) {
  frac <- alpha/(alpha+beta)
  hg <- hypergeo(k-n+1, k+alpha, k+alpha+beta, frac)
  # normalize by dividing by gamma(k+alpha+beta)
  num <- gamma(alpha+beta) * choose(n-1, k) * (frac ** k) * gamma(k+alpha) * (1/gamma(k+alpha+beta)) * hg
  den <- gamma(alpha)
  return(num/den)
}

n <- 100
alpha <- 1
beta <- 5

ks <- 0:n
ps <- f_pdf(n, alpha, beta, ks)
plot(ks, ps, type="h", xlab = "Degree", ylab = "Probability Density", yaxt="n", 
     main = "Probability Distribution, Beta(1, 5)", 
     cex.main = 1.6, cex.axis = 1.6, cex.lab = 1.6)
plot(log(ks), log(ps), type="p", xlab = "Log Degree", ylab = "Log Probability Density", yaxt="n", 
     main = "Log-Log Probability Distribution, Beta(1, 5)", 
     cex.main = 1.6, cex.axis = 1.6, cex.lab = 1.6)