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

benchmark <- function(times = 100, mvn_dimension = 100, lm_n = 1000, lm_p = 50) {
  mb_mvn_generation <- mvn_generation(times, dimension = mvn_dimension)
  mb_lm_estimation <- lm_estimation(times, n = lm_n, p = lm_p)
  return(list(
    mvn_generation = mb_mvn_generation,
    lm_estimation = mb_lm_estimation
    ))
}
