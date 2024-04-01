## fit alternate incubation period distribution to COVID-19 data
## Nicholas Reich and Stephen A Lauer
## started: May 2020

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
bets_results_bootstrap <- read.csv("inst/raw-data/bets_results_bootstrap.csv") %>%
    transmute(shape=alpha, rate=beta, idx=1:n(), median=q50, p95=q95)
usethis::use_data(bets_results_bootstrap, overwrite=TRUE)

## calculate bandwidths for plotting
hscv_bets_p50 <- get_robust_bandwidths(bets_results_bootstrap,
                                       cols=c("median", "p95"))

## calculate KDE for confidence region
kde_covid_bets <- fit_kde(bets_results_bootstrap, H=hscv_bets_p50, max_size=1000)
save(kde_covid_bets, file='data/kde_covid_bets.rda')
