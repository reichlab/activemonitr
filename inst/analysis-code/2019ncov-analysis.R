### ncov incper analysis
### Jan 2020
### Nicholas Reich


library(tidyverse)
library(activemonitr) ## devtools::install_github('reichlab/activemonitr')

theme_set(theme_bw())

data("pstr_gamma_params_mers") ## loading posterior samples from MERS, as placeholder

## Figure 1
## plot with x-axis in days, y axis as cases missed per XX monitored 
## (similar to fig 3 of activemonitr manuscript)

phis <- c(1/100, 1/1000, 1/10000)
durs <- 1:25
yrange <- c(1e-18, 1e-2)

## determine max u for each disease such that max(u)/2 + m = 90th percentile of T
tmp <- mean(qgamma(.90, shape=pstr_gamma_params_mers$shape, scale=pstr_gamma_params_mers$scale))
maxu_mers <- 2*(tmp - mean(pstr_gamma_params_mers$median))

## make plots/data
ncov_monitor_probs <- plot_risk_uncertainty(pstr_gamma_params_mers, phi=phis, 
    max_u = maxu_mers,
    durations=durs, 
    return_plot=FALSE,
    return_data=TRUE,
    include_xlab = FALSE, yrange=yrange, 
    include_legend=FALSE)

ncov_monitor_cases <- ncov_monitor_probs$data %>% 
    mutate(
        escaped_cases_per_10k_p05 = p05 * 10000,
        escaped_cases_per_10k_p50 = p50 * 10000,
        escaped_cases_per_10k_p95 = p95 * 10000,
        risk_group = factor(phi, levels=phis, labels=c("high risk (1 in 100)", "some risk (1 in 1,000)", "low risk (1 in 10,000)"))
    )

ggplot(ncov_monitor_cases, aes(x=d, y=escaped_cases_per_10k_p50, color=risk_group, group=risk_group, fill=risk_group)) +
    geom_ribbon(aes(ymin=escaped_cases_per_10k_p05, ymax=escaped_cases_per_10k_p95), alpha=.5) +
    geom_line(size=2) +
    ylab("expected cases missed per 10,000 monitored") + xlab("duration of monitoring (days)") +
    scale_color_brewer(type="qual") + 
    scale_fill_brewer(type="qual") + 
    scale_y_log10()+
    theme(legend.position = c(.8,.8), legend.justification = c(1,1))




## Table 1
## table would show number of days you have to monitor to be XXX% confident 
## that you will miss less than YY cases out of 10,000 actually infected cases monitored


## Figure 2 ?
## plot showing posterior of nCoV incubation period distribution (median/95th percentile)
## and comparing to MERS and maybe SARS? (Similar to fig 2 in paper)

