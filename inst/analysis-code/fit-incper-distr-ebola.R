## fit gamma incubation period distribution to Ebola data
## Nicholas Reich
## started: January 2015

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
library(foreach)
library(doParallel)

todays_date <- format(Sys.Date(), "%Y%m%d")

d <- read.table('inst/raw-data/DURATION.INCUB.txt')
dcc <- d[complete.cases(d),]

RUN_SENSITIVITY_ANALYSIS <- FALSE

if(RUN_SENSITIVITY_ANALYSIS) {
    sens_label <- "sens"
    dcc <- dcc[which(rowMeans(dcc)<25),]
} else {
    sens_label <- ""
}

nsamp <- 1100000
burnin <- 100000
nthin <- 100

nfits <- ncol(dcc)
registerDoParallel(cores=20)
## run the MCMC fits
pstr_gamma_params_ebola <- foreach(i=1:nfits, .combine='rbind') %dopar% {
    inc_per_mcmc_on_posterior(dat=dcc, dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                 theta0=log(runif(2, 2, 4)), proposal_sd=.1, verbose=FALSE,
                 which_sample=i)
}

colnames(pstr_gamma_params_ebola) <- c('shape', 'scale', 'idx')
pstr_gamma_params_ebola <- tbl_df(data.frame(pstr_gamma_params_ebola)) %>% 
    mutate(shape = exp(shape),
           scale = exp(scale), 
           median = qgamma(.5, shape=shape, scale=scale),
           p95 = qgamma(.95, shape=shape, scale=scale), 
           idx = 1:nrow(pstr_gamma_params_ebola))

## save posterior
save(pstr_gamma_params_ebola, 
     file=paste0('inst/analysis-output/', todays_date, '-pstr-gamma-distr-params-ebola', sens_label, '.rda'))

## calculate bandwidths for plotting
idx <- sample(nrow(pstr_gamma_params_ebola), size=2000, replace=FALSE)
sampled_params <- pstr_gamma_params_ebola[idx,]
#hscv_ebola <- get_robust_bandwidths(sampled_params)
hscv_ebola_p50 <- get_robust_bandwidths(sampled_params, cols=c("median", "p95"))

## calculate KDE for confidence region
kde_ebola <- fit_kde(pstr_gamma_params_ebola, H=hscv_ebola_p50, max_size=10000)
save(kde_ebola, file=paste0('inst/analysis-output/', todays_date, '-kde-ebola.rda'))


