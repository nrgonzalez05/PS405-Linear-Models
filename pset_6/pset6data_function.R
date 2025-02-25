pset6data <- function(m, n){ 
  beta0 <- 0.3
  beta1 <- 0.8
  cluster.id <- sort(rep(1:m,n))
  d <- as.data.frame(cluster.id)
  d$X <- c()
  d$Y <- c()
  d$v <- c()
  d$epsilon <- c()
  for(i in 1:m){ # For each group
    v <- rnorm(1, 0, 0.5) # Group component in U
    mu <- rnorm(1, 0, 0.5) # Group component in X
    for(j in 1:n){ # For each observation in the group
      d$v[(i-1)*n+j] <- v
      d$X[(i-1)*n+j] <- rnorm(1, 0, 1) + mu
      d$epsilon[(i-1)*n+j] <- rnorm(1, 0, 0.5) # Individual component in U
      d$Y[(i-1)*n+j] <- beta0 + beta1*d$X[(i-1)*n+j] + d$v[(i-1)*n+j] + d$epsilon[(i-1)*n+j]
    }
  }
  return(d)
}


