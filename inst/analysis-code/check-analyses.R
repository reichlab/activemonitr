## check/evaluate incubation period fits
## Nicholas Reich
## January 2015

source('inst/analysis-code/inc-per-mcmc.R')

########################################################
## load mers fits: object pstr_gamma_params_mers      ##
load("inst/analysis-output/20160429-pstr-gamma-distr-params-mers.rda")
load("inst/analysis-output/20160428-kde-mers.rda")


qplot(idx, shape, data=pstr_gamma_params_mers[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_mers[50000:100000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_mers[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_mers[50000:100000,], geom="line")

plot_gamma_posterior(pstr_gamma_params_mers, H=kde_mers)


##########################################################
## load ebola fits: object pstr_gamma_params_ebola      ##
load("inst/analysis-output/20160427-pstr-gamma-distr-params-ebola.rda")
load("inst/analysis-output/20160427-kde-ebola.rda")

qplot(idx, shape, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_ebola[50000:100000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_ebola[50000:100000,], geom="line")

plot_gamma_posterior(pstr_gamma_params_ebola, H=hscv_ebola)

##########################################################
## load ebola SENS fits: object pstr_gamma_params_ebola      ##
load("inst/analysis-output/20160427-pstr-gamma-distr-params-ebolasens.rda")
load("inst/analysis-output/20160427-kde-ebola.rda")

qplot(idx, shape, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_ebola[50000:100000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_ebola[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_ebola[50000:100000,], geom="line")

plot_gamma_posterior(pstr_gamma_params_ebola, H=hscv_ebola_p50)



###########################################################
## load smallpox fits: object pstr_gamma_params_smallpox ##
load("inst/analysis-output/20160429-pstr-gamma-distr-params-smallpox.rda")
load("inst/analysis-output/20160429-kde-smallpox.rda")

qplot(idx, shape, data=pstr_gamma_params_smallpox[1:50000,], geom="line")
qplot(idx, shape, data=pstr_gamma_params_smallpox[50000:100000,], geom="line")

qplot(idx, scale, data=pstr_gamma_params_smallpox[1:50000,], geom="line")
qplot(idx, scale, data=pstr_gamma_params_smallpox[50000:100000,], geom="line")


plot_gamma_posterior(pstr_gamma_params_smallpox, H=hscv_smallpox)

#######################
## check convergence ##
#######################
acf(pstr_gamma_params_mers$shape)
acf(pstr_gamma_params_mers$scale)
get_Rhat(pstr_gamma_params_mers)

acf(pstr_gamma_params_ebola$shape)
acf(pstr_gamma_params_ebola$scale)
get_Rhat(pstr_gamma_params_ebola)

acf(pstr_gamma_params_smallpox$shape)
acf(pstr_gamma_params_smallpox$scale)
get_Rhat(pstr_gamma_params_smallpox)


#####################
## plot posteriors ##
#####################

library(gridExtra)
# p_ebola <- plot_gamma_posterior(pstr_gamma_params_ebola, H=hscv_ebola)
# p_smallpox <- plot_gamma_posterior(pstr_gamma_params_smallpox, H=hscv_smallpox)
# p_mers <- plot_gamma_posterior(pstr_gamma_params_mers, H=hscv_mers)
# grid.arrange(p_ebola, p_smallpox, p_mers, ncol=1)
# 
# plot_all_regions <- plot_credible_regions(list(pstr_gamma_params_ebola,
#                                                pstr_gamma_params_mers,
#                                                pstr_gamma_params_smallpox), 
#                                           Hs=list(hscv_ebola, 
#                                                   hscv_mers,
#                                                   hscv_smallpox))

plot_all_regions <- plot_modified_credible_regions(list(pstr_gamma_params_ebola,
                                                        pstr_gamma_params_mers,
                                                        pstr_gamma_params_smallpox), 
                                                   kdes=list(kde_ebola, 
                                                             kde_mers,
                                                             kde_smallpox),
                                                   label_txt=c("Ebola", "MERS-CoV", "Smallpox"))

colors <- c("#1b9e77", "#d95f02", "#7570b3")
lighter_colors <- c("#8ecfbc", "#fdb174", "#b8b6d6")
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
