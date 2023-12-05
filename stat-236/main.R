### Relevant libraries
library(igraph)
library(parallel)

### Function to simulate from our hierarchical model:
# theta_i ~ i.i.d. Beta(a,b)
# Y_ij ~ indep Bern(theta_i * theta_j), i < j
sim <- function(a, b, n){
  theta <- rbeta(n, a, b)
  out <- matrix(1, n, n)
  for (i in 1:(n-1)) {
    for(j in (i+1):n){
      draw <- runif(1) < theta[i] * theta[j]
      out[i,j] <- draw
      out[j,i] <- draw
    }
  }
  return(out)
}

# Beta parameters
a <- 2
b <- 5

# Size of matrix
n <- 100

# Draw an example n x n matrix
m <- sim(a,b,n)

# Compute the mean of y_ij, for i < j
get_mu <- function(m){
  mean(m[upper.tri(m)])
}

# Compute the mean of y_ij * y_ik, samples that have one overlapping entry.
# Call this tau
get_tau <- function(m){
  n <- ncol(m)
  tau <- 0
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for(k in (j+1):n){
        tau <- tau + m[i,j]*m[i,k] + m[i,j]*m[j,k] + m[i,k]*m[j,k]
      }
    }
  }
  tau <- tau / (3 * choose(n,3))
  tau
}

# What are the theoretical values of these quantities?
true_mu <- (a/(a+b))^2
true_tau <- true_mu*a*(1+a)/((a+b)*(1+a+b))

# MoM estimate
mom <- function(mu, tau){
  a1 <- (-tau*sqrt(mu) + mu^2) / (tau - mu^2)
  b1 <- (a - a * sqrt(mu))/sqrt(mu)
  a2 <- (tau*sqrt(mu) + mu^2) / (tau - mu^2)
  b2 <- -(a + a * sqrt(mu))/sqrt(mu)
  
  if(a1 > 0 & b1 > 0){
    return(c(a1,b1))
  }else if(a2 > 0 & b2 > 0){
    return(c(a2, b2))
  }else{
    return(c(NA,NA))
  }
  
}

# Monte Carlo
estim <- function(r, a, b, n){
  m <- sim(a,b,n)
  mu <- get_mu(m)
  tau <- get_tau(m)
  return(mom(mu,tau))
}

mc <- mclapply(1:1000, estim, a=2,b=5,n=100, mc.cores = 12)
as <- unlist(mc)[seq(1, 1999, 2)]
bs <- unlist(mc)[seq(2,2000,2)]
hist(as, main = "Histogram of alpha", xlab = "alpha")
mean(as)
hist(bs, main = "Histogram of beta", xlab = "beta")
mean(bs)

### Fitting to empirical data
load("ke_data.RData")
data <- list(CoAuthor, Dolphin, Fan, Football, Karate, Polbooks, UKFaculty)
fits <- list()
for (i in 1:length(data)) {
  fits[[i]] <- mom(get_mu(data[[i]]), get_tau(data[[i]]))
}
fits

