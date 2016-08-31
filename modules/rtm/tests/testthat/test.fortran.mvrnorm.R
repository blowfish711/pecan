#install.packages("~/Projects/pecan/pecan/modules/rtm", 
#                 repos = NULL, type = "source")

library(PEcAnRTM)
library(testthat)
context("Test Fortran multivariate normal distribution")

# Multivariate normal draw
nsamp <- as.integer(500000)
nvar <- as.integer(2)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2)
sample <- matrix(0, nsamp, nvar)
call_list <- list("test_mvrnorm", nsamp=nsamp, nvar=nvar, 
                  mu = mu, sigma = Sigma, sample = sample)

out <- do.call(.Fortran, call_list)
result <- out$sample
print(head(result))
print(colMeans(result))
print(cov(result))


# Block sampler
#ngibbs <- 500

#param <- c("N" = 1.5, "Cab" = 40, "Car" = 8, "Cw" = 0.01, "Cm" = 0.01)
#true_obs <- prospect(param, 5)[,1]
#noise <- do.call(cbind, lapply(1:5, function(x) generate.noise()))
#observed <- true_obs + noise
#colnames(observed) <- NULL

#inits <- default.settings.prospect$inits.function()
#pvals <- prior.defaultvals.prospect()
#pmu <- pvals$mu
#psd <- pvals$sigma
#plog <- rep(TRUE, length(pmu))
#minp <- default.settings.prospect$param.mins
#names(minp) <- names(pmu)

#outmat <- invert.fast(modname = "prospect_5", observed = observed,
#                      inits = inits, cons = NULL, pmu = pmu, psd = psd,
#                      plog = plog, minp = minp, ngibbs = ngibbs)

#pdf("fortran.pdf")
#tails <- floor(ngibbs*0.8) : ngibbs
#par(mfrow=c(1,2))
#for (i in 1:ncol(outmat)){
#    plot(outmat[,i], type='l', main = colnames(outmat)[i])
#    plot(outmat[tails,i], type='l', main = colnames(outmat)[i])
#}
#dev.off()
