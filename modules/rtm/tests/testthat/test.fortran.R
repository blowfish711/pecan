if (grepl("testthat", getwd())) setwd("../..")
install.packages(".", repos = NULL, type = "source")

library(PEcAnRTM)
library(testthat)
context("Test key Fortran functions")

# Multivariate normal draw
n <- as.integer(2)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2)
sample <- numeric(length(mu))
call_list <- list("test_mvrnorm", n = n, mu = mu, sigma = Sigma, sample = sample)

out <- do.call(.Fortran, call_list)
result <- out$sample
print(result)
