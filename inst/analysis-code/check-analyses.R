## check/evaluate incubation period fits
## Nicholas Reich
## January 2015

source('inst/analysis-code/inc-per-mcmc.R')

########################################################
## load mers fits: object pstr_gamma_params_mers      ##
load("inst/analysis-output/20160503-pstr-gamma-distr-params-mers.rda")
load("inst/analysis-output/20160503-kde-mers.rda")

qplot(idx, shape, data=pstr_gamma_params_mers[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_mers[50000:100000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_mers[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_mers[50000:500000,], geom="line")


##########################################################
## load ebola fits: object pstr_gamma_params_ebola      ##
load("inst/analysis-output/20160502-pstr-gamma-distr-params-ebola.rda")
load("inst/analysis-output/20160502-kde-ebola.rda")

## these plots don't look stationary because each chain is not 
##  estimating exactly the same distribution
qplot(idx, shape, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_ebola[50000:500000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_ebola[50000:500000,], geom="line")

###########################################################
## load smallpox fits: object pstr_gamma_params_smallpox ##
load("inst/analysis-output/20160503-pstr-gamma-distr-params-smallpox.rda")
load("inst/analysis-output/20160503-kde-smallpox.rda")

qplot(idx, shape, data=pstr_gamma_params_smallpox[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_smallpox[50000:500000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_smallpox[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_smallpox[50000:500000,], geom="line")


#######################
## check convergence ##
#######################
acf(pstr_gamma_params_mers$shape)
acf(pstr_gamma_params_mers$scale)
get_Rhat(pstr_gamma_params_mers)

acf(pstr_gamma_params_ebola$shape)
acf(pstr_gamma_params_ebola$scale)

acf(pstr_gamma_params_smallpox$shape)
acf(pstr_gamma_params_smallpox$scale)
get_Rhat(pstr_gamma_params_smallpox)


#####################
## plot posteriors ##
#####################

library(gridExtra)
colors <- c("#1b9e77", "#d95f02", "#7570b3")
lighter_colors <- c("#8ecfbc", "#fdb174", "#b8b6d6")

plot_all_regions <- plot_modified_credible_regions(list(pstr_gamma_params_ebola,
                                                        pstr_gamma_params_mers,
                                                        pstr_gamma_params_smallpox), 
                                                   kdes=list(kde_ebola, 
                                                             kde_mers,
                                                             kde_smallpox),
                                                   label_txt=c("Ebola", "MERS-CoV", "Smallpox"),
                                                   colors=colors)

nplots <- 500
ebola_densities <- plot_gamma_densities(pstr_gamma_params_ebola, kde=kde_ebola,
                                        n_to_plot = nplots, 
                                        plotcolor=colors[1], 
                                        plotcolor_light = lighter_colors[1],
                                        label_txt="Ebola",
                                        xaxs_lab=FALSE)
mers_densities <- plot_gamma_densities(pstr_gamma_params_mers, kde=kde_mers,
                                       n_to_plot = nplots, 
                                       plotcolor=colors[2], 
                                       plotcolor_light = lighter_colors[2],
                                       label_txt="MERS-CoV",
                                       xaxs_lab=FALSE)
smallpox_densities <- plot_gamma_densities(pstr_gamma_params_smallpox, kde=kde_smallpox,
                                           n_to_plot = nplots, 
                                           plotcolor=colors[3], 
                                           plotcolor_light = lighter_colors[3],
                                           label_txt="Smallpox",
                                           xaxs_lab=TRUE)

grid.arrange(plot_all_regions,
             arrangeGrob(ebola_densities, mers_densities, smallpox_densities,
                         heights=c(.32, .32, .36)),
             ncol=2)

####################################
## plot posteriors w/ SENSITIVITY ##
####################################

pstr_gamma_params_ebola_orig <- pstr_gamma_params_ebola
kde_ebola_orig <- kde_ebola

load("inst/analysis-output/20160503-pstr-gamma-distr-params-ebolasens.rda")
load("inst/analysis-output/20160503-kde-ebolasens.rda")

plot_all_regions <- plot_modified_credible_regions(list(pstr_gamma_params_ebola_orig,
                                                        pstr_gamma_params_ebola,
                                                        pstr_gamma_params_mers,
                                                        pstr_gamma_params_smallpox), 
                                                   kdes=list(kde_ebola_orig,
                                                             kde_ebola,
                                                             kde_mers,
                                                             kde_smallpox),
                                                   label_txt=c("Ebola", "Ebola_sens", "MERS-CoV", "Smallpox"),
                                                   colors=c("#1b9e77", "#1b9e77", "#d95f02", "#7570b3"))

ebola_sens_densities <- plot_gamma_densities(pstr_gamma_params_ebola, kde=kde_ebola,
                                        n_to_plot = nplots, 
                                        plotcolor=colors[1], 
                                        plotcolor_light = lighter_colors[1],
                                        label_txt="Ebola (sens.)",
                                        xaxs_lab=FALSE)

grid.arrange(plot_all_regions,
             arrangeGrob(ebola_densities, ebola_sens_densities, mers_densities, smallpox_densities,
                         heights=c(.24, .24, .24, .28)),
             ncol=2)


## cost analysis with sensitivity
library(activeMonitr)
library(scales)

## $1.9m, 5321 monitored, 21 day duration
per_day_cost <- 1900000/(5321*21)

## calculate per-day hazard of symptoms with other pathogens
n_monitored <- 5321
n_symptomatic <- 103
s.21 <- 1-n_symptomatic*.3/n_monitored
per_day_hazard <- 1-s.21^(1/21)

cost_m <- c(10, 20)     ## per day cost of treatment
cost_trt <- c(3e6, 5e6) ## cost of response to single case
cost_exp <- c(0, 20e6)  ## cost of response to single case not captured by monitoring
cost_falsepos <- c(10000,30000) ## cost of false positive testing

cost_mat <- rbind(cost_m, cost_trt, cost_exp, cost_falsepos)

gamma_params <- c(median = mean(pstr_gamma_params_ebola$median), 
                  shape = mean(pstr_gamma_params_ebola$shape),
                  scale = mean(pstr_gamma_params_ebola$scale))
durs <- c(1.5, 2, 3, 5)
phis <- c(1/1000, 1/10000)
durs <- seq(.5, 5, by=.1)
phis <- c(1/1000, 1/10000)

costs <- calc_monitoring_costs(durs = durs,
                               probs_of_disease = phis,
                               per_day_hazard = per_day_hazard,
                               N = 100,
                               cost_mat = cost_mat,
                               gamma_params = gamma_params, 
                               return_scalar=FALSE)

costs$phi_lab <- factor(costs$phi, 
                        levels=c(1/1000, 1/10000),
                        labels=c("some or high risk", "low (but not zero) risk"))

## minimum costs 
min_costs <- costs %>%
    group_by(phi) %>%
    summarize(min_cost = min(maxcost),
              min_cost_dur = dur[which.min(maxcost)],
              min_cost_dur_days = min_cost_dur * gamma_params['median']) %>%
    as.matrix()

ggplot(costs, aes(x=dur*gamma_params['median'], 
                  color=phi_lab, fill=phi_lab)) + 
    geom_ribbon(aes(ymin=mincost, ymax=maxcost), alpha=.7) + 
    scale_y_continuous(breaks=c(1e5, 5e5, 10e5, 15e5, 20e5), labels=dollar,
                       name='Cost range of monitoring 100 individuals') +
    scale_x_continuous(name='Duration (in days)') +
    coord_cartesian(xlim=c(5, 43)) +
    scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
    scale_color_manual(values=c("#e41a1c", "#377eb8")) +
    geom_segment(aes(x=3, xend=min_costs[1, "min_cost_dur_days"], 
                     y=min_costs[1, "min_cost"], yend=min_costs[1, "min_cost"]), 
                 linetype=2, color="#377eb8") +
    geom_segment(aes(x=min_costs[1, "min_cost_dur_days"], xend=min_costs[1, "min_cost_dur_days"], 
                     y=0, yend=min_costs[1, "min_cost"]), 
                 linetype=2, color="#377eb8") +
    geom_segment(aes(x=3, xend=min_costs[2, "min_cost_dur_days"], 
                     y=min_costs[2, "min_cost"], yend=min_costs[2, "min_cost"]), 
                 linetype=2, color="#e41a1c") +
    geom_segment(aes(x=min_costs[2, "min_cost_dur_days"], xend=min_costs[2, "min_cost_dur_days"], 
                     y=0, yend=min_costs[2, "min_cost"]), 
                 linetype=2, color="#e41a1c") +
    theme(legend.title=element_blank(), legend.position=c(.8, .8))

min_costs[which(min_costs[,"phi"]==1e-4), "min_cost_dur_days"]
min_costs[which(min_costs[,"phi"]==1e-3), "min_cost_dur_days"]
