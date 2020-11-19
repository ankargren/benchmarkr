mvn_generation <- function(times, n) {
  mu <- rep(0, n)
  Sigma <- crossprod(matrix(rnorm(n*10000), 10000, n))/10000
  mb <- microbenchmark::microbenchmark(MASS::mvrnorm(n = 1, mu = mu, Sigma = Sigma), times = times)
  mb$n <- n
  return(mb)
}

lm_estimation <- function(times, n, p) {
  X <- matrix(rnorm(n*p), n, p)
  y <- matrix(rnorm(n), n, 1)
  mb <- microbenchmark::microbenchmark(lm(y~X), times = times)
  mb$n <- n
  mb$p <- p
  return(mb)
}

density_evaluation <- function(times, n) {
  x <- rnorm(n)
  mb <- microbenchmark::microbenchmark(dnorm(x), times = times)
  mb$n <- n
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
  mb$n <- n
  return(mb)
}
benchmark <- function(times = 100, mvn_n = 100, lm_n = 1000, lm_p = 50,
                      density_n = 100000, mcmc_n = 1000) {
  defaults <- formals(benchmark)
  defaults_changed_vec <- c(
    mvn_generation = defaults$mvn_n != mvn_n,
    lm_estimation = (defaults$lm_n != lm_n) || (defaults$lm_p != lm_p),
    density_evaluation = defaults$density_n != density_n,
    mcmc_sampling = defaults$mcmc_n != mcmc_n
  )

  mb_mvn_generation <- mvn_generation(times, n = mvn_n)
  mb_lm_estimation <- lm_estimation(times, n = lm_n, p = lm_p)
  mb_density_evaluation <- density_evaluation(times, n = density_n)
  mb_mcmc_sampling <- mcmc_sampling(times, n = mcmc_n)

  mb_list <- list(
    `MVN Generation` = mb_mvn_generation,
    `LM Estimation` = mb_lm_estimation,
    `Density Evaluation` = mb_density_evaluation,
    `MCMC Sampling` = mb_mcmc_sampling
  )
  mb_vec <- sapply(mb_list, function(x) median(x$time))
  mb_df <- data.frame(
    step = names(mb_vec),
    time = mb_vec,
    defaults_changed = defaults_changed_vec
    )
  rownames(mb_df) <- NULL
  class(mb_df) <- c("benchmarkr", "data.frame")
  return(mb_df)
}



comparisons <- function() {
  comp1 <- data.frame(
    step = c("MVN Generation", "LM Estimation", "Density Evaluation", "MCMC Sampling"),
    time = c(2603802, 4084801, 2360808, 3851763),
    computer = rep("MBP 2017, 2.5 GHz i7, 8 Gb")
  )

  # Add more here

  comp <- rbind(comp1)
  return(comp)
}

print.benchmarkr <- function(x, ...) {
  print.data.frame(x)
}

plot.benchmarkr <- function(x, ...) {
  comp <- comparisons()
  current <- data.frame(step = x$step, time = x$time, computer = "Current")
  unchanged_steps <- x$step[!x$defaults_changed]
  comp <- comp[comp$step %in% unchanged_steps, ]
  plot_df <- rbind(comp, current)
  p <- ggplot(plot_df, aes(x = step, y = time/1e6, fill = computer)) +
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() +
    labs(y = "Milliseconds",
         x = "Benchmarking step",
         fill = "Computer") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
    scale_fill_brewer(palette="Dark2")
  return(p)
}

# Example with defaults
bm <- benchmark()
plot(bm)

# If we change a default, comparison is removed
bm <- benchmark(times = 1000)
plot(bm)
