## fit gamma incubation period distribution to COVID-19 data
## Nicholas Reich and Stephen A Lauer
## started: February 2020

source('inst/analysis-code/inc-per-mcmc.R')
library(dplyr)
# data(boot_lnorm_params_covid)
set.seed(1)
boot_lnorm_params_covid_ucd <- data.frame(idx = 1:2000,
                                          meanlog = rnorm(2000, 1.63, 0.06122),
                                          sdlog = rnorm(2000, 0.50, 0.0255102)) %>%
    mutate(median=qlnorm(0.5, meanlog, sdlog),
           p95=qlnorm(0.95, meanlog, sdlog),
           p05=qlnorm(0.05, meanlog, sdlog)) %>%
    filter(median >= 4.53, median <= 5.75,
           p95 >= 9.49, p95 <= 14.20,
           p05 >= 1.83, p05 <= 2.75) %>%
    slice_sample(n=1000)

## calculate bandwidths for plotting
hscv_covid_p50 <- get_robust_bandwidths(boot_lnorm_params_covid_ucd,
                                        cols=c("median", "p95"))

## calculate KDE for confidence region
kde_covid_ucd <- fit_kde(boot_lnorm_params_covid_ucd,
                         H=hscv_covid_p50, max_size=1000)
save(kde_covid_ucd, file='data/kde_covid_ucd.rda')
save(boot_lnorm_params_covid_ucd, file='data/boot_lnorm_params_covid_ucd.rda')
