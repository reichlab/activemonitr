## fit gamma incubation period distribution to MERS data
## Nicholas Reich
## started: January 2015

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
library(foreach)
library(doParallel)

todays_date <- format(Sys.Date(), "%Y%m%d")

## read in data
d <- read.csv('inst/raw-data/data_mers.csv')

## IncP_max adjusted by adding 1 due to:
##     - min(IncP_min) = 0, indicating that the intervals start on the left.
##     - some records have IncP_min == IncP_max, indicating that to be SIC data, the IncP_max should have +1
##     - e.g. (9,9) should be (9, 10)
d$IncP_max <- d$IncP_max+1

## MCMC setup
nsamp <- 120000 ## 120,000 samples
burnin <- 20000 ## remove 20,000 for burn-in
nthin <- 2      ## thin to leave 50,000 samples per chain
registerDoParallel(cores=20) ## 20 chains so 1,000,000 samples total

## run the MCMC fits
nfits <- 20 ## a reasonable number of parallel chains
pstr_gamma_params_mers <- foreach(i=1:nfits, .combine='rbind') %dopar% {
    inc_per_mcmc(dat=d, ip_cols = c("IncP_min", "IncP_max"),
                 dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                 theta0=log(runif(2, 2, 4)), proposal_sd=.1, verbose=FALSE)
}

colnames(pstr_gamma_params_mers) <- c('shape', 'scale', 'idx')
pstr_gamma_params_mers <- tbl_df(data.frame(pstr_gamma_params_mers)) %>% 
    mutate(shape = exp(shape),
           scale = exp(scale), 
           median = qgamma(.5, shape=shape, scale=scale),
           p95 = qgamma(.95, shape=shape, scale=scale),
           idx = 1:nrow(pstr_gamma_params_mers),
           chain = rep(1:nfits, each=(nsamp-burnin)/nthin))

## save posterior
save(pstr_gamma_params_mers, 
     file=paste0('inst/analysis-output/', todays_date, '-pstr-gamma-distr-params-mers.rda'))

## calculate bandwidths for plotting
sampled_params <- pstr_gamma_params_mers[sample(nrow(pstr_gamma_params_mers), size=2000),]
#hscv_mers <- get_robust_bandwidths(sampled_params)
hscv_mers_p50 <- get_robust_bandwidths(sampled_params, cols=c("median", "p95"))

## calculate KDE for confidence region
kde_mers <- fit_kde(pstr_gamma_params_mers, H=hscv_mers_p50, max_size=10000)
save(kde_mers, file=paste0('inst/analysis-output/', todays_date, '-kde-mers.rda'))



