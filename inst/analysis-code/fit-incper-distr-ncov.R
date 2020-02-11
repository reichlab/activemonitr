## fit gamma incubation period distribution to Ebola data
## Nicholas Reich and Stephen A Lauer
## started: February 2020

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
data(boot_lnorm_params_ncov)

## calculate bandwidths for plotting
hscv_ncov_p50 <- get_robust_bandwidths(boot_lnorm_params_ncov,
                                       cols=c("median", "p95"))

## calculate KDE for confidence region
kde_ncov <- fit_kde(boot_lnorm_params_ncov, H=hscv_ncov_p50, max_size=1000)
save(kde_ncov, file='data/kde_ncov.rda')
