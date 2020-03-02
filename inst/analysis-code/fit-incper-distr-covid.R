## fit gamma incubation period distribution to COVID-19 data
## Nicholas Reich and Stephen A Lauer
## started: February 2020

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
data(boot_lnorm_params_covid)

## calculate bandwidths for plotting
hscv_covid_p50 <- get_robust_bandwidths(boot_lnorm_params_covid,
                                        cols=c("median", "p95"))

## calculate KDE for confidence region
kde_covid <- fit_kde(boot_lnorm_params_covid, H=hscv_covid_p50, max_size=1000)
save(kde_covid, file='data/kde_covid.rda')
