mvn_generation <- function(times, dimension) {
  mu <- rep(0, dimension)
  Sigma <- crossprod(matrix(rnorm(dimension*10000), 10000, dimension))/10000
  mb <- microbenchmark::microbenchmark(MASS::mvrnorm(n = 1, mu = mu, Sigma = Sigma), times = times)
  return(mb)
}

lm_estimation <- function(times, n, p) {
  X <- matrix(rnorm(n*p), n, p)
  y <- matrix(rnorm(n), n, 1)
  mb <- microbenchmark::microbenchmark(lm(y~X), times = times)
  return(mb)
}

density_evaluation <- function(times, n) {
  x <- rnorm(n)
  mb <- microbenchmark::microbenchmark(dnorm(x), times = times)
  return(mb)
}

mcmc_sampling <- function(times, n) {
  mu <- c(1, 2)
  Sigma <- diag(c(2, 1))
  Sigma[1,2] <- 0.5
  Sigma[2,1] <- 0.5
  mcmc_temporary_fun <- function(n, mu, Sigma) {
    X <- matrix(NA, n, 2)
    X[1,] <- mu
    for (i in 2:n) {
      X[i,1] <- rnorm(1, mu[1]+Sigma[1,2]/Sigma[2,2]*(X[i-1,2]-mu[2]), sqrt((1-Sigma[1,2]^2/(Sigma[1,1]*Sigma[2,2]))*Sigma[1,1]))
      X[i,2] <- rnorm(1, mu[1]+Sigma[2,1]/Sigma[1,1]*(X[i,1]-mu[1]), sqrt((1-Sigma[2,1]^2/(Sigma[2,2]*Sigma[1,1]))*Sigma[2,2]))
    }
  }

  mb <- microbenchmark::microbenchmark(mcmc_temporary_fun(n, mu, Sigma), times = times)
  return(mb)
}
benchmark <- function(times = 100, mvn_dimension = 100, lm_n = 1000, lm_p = 50,
                      density_n = 100000, mcmc_n = 1000) {
  mb_mvn_generation <- mvn_generation(times, dimension = mvn_dimension)
  mb_lm_estimation <- lm_estimation(times, n = lm_n, p = lm_p)
  mb_density_evaluation <- density_evaluation(times, n = density_n)
  mb_mcmc_sampling <- mcmc_sampling(times, n = mcmc_n)
  return(list(
    mvn_generation = mb_mvn_generation,
    lm_estimation = mb_lm_estimation,
    density_evaluation = mb_density_evaluation,
    mcmc_sampling = mb_mcmc_sampling
    ))
}

# Example
bm <- benchmark()
milliseconds_medians <- sapply(bm, function(x) median(x$time)/1e6)
