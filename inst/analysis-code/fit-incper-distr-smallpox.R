## fit gamma incubation period distribution to smallpox data
## Nicholas Reich
## started: January 2015

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
library(foreach)
library(doParallel)

todays_date <- format(Sys.Date(), "%Y%m%d")

## read in data
d <- read.csv('inst/raw-data/smallpox-data.csv')

## inc per min and max calculated assuming that 6 means [6,7)
d$ip_min <- d$reported_incper
d$ip_max <- d$reported_incper + 1

## MCMC setup
nsamp <- 320000 ## 1320,000 samples
burnin <- 20000 ## remove 20,000 for burn-in
nthin <- 6      ## thin to leave 50,000 samples per chain
registerDoParallel(cores=20) ## 20 chains so 1,000,000 samples total

## run the MCMC fits
nfits <- 20 ## a reasonable number of parallel chains
pstr_gamma_params_smallpox <- foreach(i=1:nfits, .combine=rbind) %dopar% {
    inc_per_mcmc(dat=d, ip_cols = c("ip_min", "ip_max"),
                 dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                 theta0=log(c(runif(1, 35, 45), runif(1, .2, .5))), 
                 proposal_sd=.02, verbose=FALSE,
                 log_scale=TRUE)
}

colnames(pstr_gamma_params_smallpox) <- c('shape', 'scale', 'idx')
pstr_gamma_params_smallpox <- tbl_df(data.frame(pstr_gamma_params_smallpox)) %>% 
    mutate(shape = exp(shape),
           scale = exp(scale), 
           median = qgamma(.5, shape=shape, scale=scale),
           p95 = qgamma(.95, shape=shape, scale=scale),
           idx = 1:nrow(pstr_gamma_params_smallpox),
           chain = rep(1:nfits, each=(nsamp-burnin)/nthin))

## save posterior
save(pstr_gamma_params_smallpox, 
     file=paste0('inst/analysis-output/', todays_date, '-pstr-gamma-distr-params-smallpox.rda'))

## calculate bandwidths for plotting
sampled_params <- pstr_gamma_params_smallpox[sample(nrow(pstr_gamma_params_smallpox), size=2000),]
# hscv_smallpox <- get_robust_bandwidths(sampled_params, cols=c("shape", "scale"))
hscv_smallpox_p50 <- get_robust_bandwidths(sampled_params, cols=c("median", "p95"))

## calculate KDE for confidence region
kde_smallpox <- fit_kde(pstr_gamma_params_smallpox, H=hscv_smallpox_p50, max_size=10000)
save(kde_smallpox, file=paste0('inst/analysis-output/', todays_date, '-kde-smallpox.rda'))



