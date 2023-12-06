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
    for (j in (i+1):n){
      draw <- runif(1) < theta[i] * theta[j]
      out[i,j] <- draw
      out[j,i] <- draw
    }
  }
  for (i in 1:n) {
    out[i,i] <- 0
  }
  return(out)
}

# Beta parameters
a <- 1
b <- 5

# Size of matrix
n <- 1000

# Draw an example n x n matrix
hist = rep(0, n)
for (i in 1:100) {
  if (i %% 10 == 0) {
    print(i)
  }
  m <- sim(a,b,n)
  g <- graph_from_adjacency_matrix(m, mode = "undirected")
  degrees <- degree_distribution(g)
  degrees <- c(degrees, rep(0, n-length(degrees)))
  hist <- hist + degrees
}

# m <- sim(a,b,n)
# g <- graph_from_adjacency_matrix(m, mode = "undirected")
# plot(g, vertex.color = "white", vertex.label = NA, vertex.label.cex = 0.8)

# Plot a histogram of degrees
# degrees <- degree_distribution(g)
# degrees <- c(degrees, rep(0, n-length(degrees)))
plot(1:n, degrees, type="h", xlab = "Degree", ylab = "Count", yaxt="n",
     main = "Histogram of Node Degrees, Beta(3, 3)",
     cex.main = 1, cex.axis = 1, cex.lab = 1)
