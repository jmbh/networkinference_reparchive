# jonashaslbeck@gmail.com; March 10, 2022

# ------------------------------------------------------
# -------- Function to plot labels ---------------------
# ------------------------------------------------------

plotLab <- function(label, rot=0, cex=1) {
  par(mar=rep(0,4))
  plot.new()
  plot.window(xlim=c(-1, 1), ylim=c(-1, 1))
  text(0, 0, label, srt=rot, cex=cex)
}

# ------------------------------------------------------
# -------- Function for Data Generation ----------------
# ------------------------------------------------------

GenData2 <- function(p, pe, n) {

  # Below: Code by Lourens Waldorp
  p.star <- p*(p-1)/2  # number of parameters
  sd <- 1				       # standard deviation noise

  valid <- 0
  counter <- 0

  # In very rare cases, we sample non-positive definite thetas; we resample in these cases
  while(valid==0) {

    # obtain random graph
    graph <- erdos.renyi.game(p, p.or.m = pe, type = "gnp")
    adj <- as.matrix(get.adjacency(graph))

    # generate inverse covariance matrix
    pars <- runif(p.star, min = 0.1, max = 0.8)
    theta <- matrix(0,nrow=p,ncol=p)
    theta[upper.tri(theta)] <- pars
    theta <- theta * adj # create 0s in inv cov
    theta <- theta + t(theta)
    diag(theta) <- rowSums(theta)

    # If there are zeros in diagonal, replace with average of nonzero ones
    diag(theta)[diag(theta)==0] <- mean(diag(theta)[diag(theta)!=0])

    eig <- eigen(theta)
    if(all(eig$values > 0.001)) valid <- 1 else counter <- counter + 1

  } # end while

  sigma <- solve(theta) # covariance matrix
  pcors <- cor2pcor(sigma)

  # make square root of covariance matrix
  eigen.sigma <- eigen(sigma)
  sqrt.sigma <- eigen.sigma$vectors %*% diag(sqrt(eigen.sigma$val)) %*% t(eigen.sigma$vectors)

  # sample data
  Z <- matrix(rnorm(n*p,mean = 0, sd = sd),nrow=n,ncol=p)
  data <- Z %*% t(sqrt.sigma)

  outlist <- list("graph" = adj,
                  "pcors" = pcors,
                  "data" = data)

} # eoF




