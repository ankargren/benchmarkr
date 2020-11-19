
mvn_generation <- function(times, dimension) {
  mu <- rep(0, dimension)
  Sigma <- crossprod(matrix(rnorm(dimension*10000), 10000, dimension))/10000
  mb <- microbenchmark::microbenchmark(MASS::mvrnorm(n = 1, mu = mu, Sigma = Sigma), times)
}

benchmark <- function(times) {

}
