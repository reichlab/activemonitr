#######################################
## Title: active-monitoring global.R ##
## Author(s): Xuelian Li,            ##
##            Stephen A Lauer        ##
##            Nicholas G Reich       ##
## Date Created:  01/04/2016         ##
## Date Modified: 02/12/2020 XL      ##
#######################################
library(shiny)
library(Cairo)
library(dplyr)
library(ggplot2)
library(scales)
library(activeMonitr)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

# source("plot_risk.R")
theme_set(theme_bw(base_size = 18))

data(pstr_gamma_params_ebola)
data(pstr_gamma_params_mers)
data(pstr_gamma_params_smallpox)
data(boot_lnorm_params_covid)
data(bets_results_bootstrap)
data(boot_lnorm_params_covid_ucd)
data(kde_smallpox)
data(kde_ebola)
data(kde_mers)
data(kde_covid)
data(kde_covid_bets)
data(kde_covid_ucd)
