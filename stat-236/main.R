### Relevant libraries
library(igraph)

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
m <- sim(a,b,100)

# Compute the mean of y_ij, for i < j
mu <- mean(m[upper.tri(m)])

# Compute the mean of y_ij * y_ik, samples that have one overlapping entry.
# Call this tau
tau <- 0
for (i in 1:(n-2)) {
  for (j in (i+1):(n-1)) {
    for(k in (j+1):n){
      tau <- tau + m[i,j]*m[i,k] + m[i,j]*m[j,k] + m[i,k]*m[j,k]
    }
  }
}
tau <- tau / (3 * choose(n,3))

# What are the theoretical values of these quantities?
true_mu <- (a/(a+b))^2
true_tau <- true_mu + a*(1+a)/((a+b)*(1+a+b))


