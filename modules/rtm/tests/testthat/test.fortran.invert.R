#install.packages("~/Projects/pecan/pecan/modules/rtm", 
#                 repos = NULL, type = "source")

library(PEcAnRTM)
library(testthat)
context("Test Fortran block sampling inversion")

# Block sampler
ngibbs <- 10000

param <- c("N" = 1.5, "Cab" = 40, "Car" = 8, "Cw" = 0.01, "Cm" = 0.01)
true_obs <- prospect(param, 5)[,1]
noise <- do.call(cbind, lapply(1:5, function(x) generate.noise()))
observed <- true_obs + noise
colnames(observed) <- NULL

inits <- default.settings.prospect$inits.function()
pvals <- prior.defaultvals.prospect()
pmu <- pvals$mu
psd <- pvals$sigma
plog <- rep(TRUE, length(pmu))
minp <- default.settings.prospect$param.mins
names(minp) <- names(pmu)

outmat <- invert.fast(modname = "prospect_5", observed = observed,
                      inits = inits, cons = NULL, pmu = pmu, psd = psd,
                      plog = plog, minp = minp, ngibbs = ngibbs)

pdf("fortran.pdf")
tails <- floor(ngibbs*0.8) : ngibbs
par(mfrow=c(1,2))
for (i in 1:ncol(outmat)){
    plot(outmat[,i], type='l', main = colnames(outmat)[i])
    plot(outmat[tails,i], type='l', main = colnames(outmat)[i])
}
dev.off()
