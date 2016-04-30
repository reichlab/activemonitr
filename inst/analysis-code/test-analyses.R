## test runs to figure out MCMC params
## Nicholas Reich
## April 2016

source('inst/analysis-code/inc-per-mcmc.R')

##########
## MERS ##
##########

## read in data
d <- read.csv('inst/raw-data/data_mers.csv')
d$IncP_max <- d$IncP_max+1

## MCMC setup
nsamp <- 60000 ## 110,000 samples
burnin <- 10000 ## remove 10,000 for burn-in
nthin <- 1
fit_mers <- inc_per_mcmc(dat=d, ip_cols = c("IncP_min", "IncP_max"),
                         dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                         theta0=log(runif(2, 2, 4)), proposal_sd=.1, verbose=TRUE)
## acceptance rate = 0.32 


##############
## smallpox ##
##############

d <- read.csv('inst/raw-data/smallpox-data.csv')
d$ip_min <- d$reported_incper
d$ip_max <- d$reported_incper + 1

## MCMC setup
nsamp <- 320000 ## 110,000 samples
burnin <- 20000 ## remove 10,000 for burn-in
nthin <- 6      ## thin to leave 50,000 samples per chain

## run the MCMC fits
fit_smallpox <- inc_per_mcmc(dat=d, ip_cols = c("ip_min", "ip_max"),
                             dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                             theta0=log(c(runif(1, 35, 45), runif(1, .2, .5))), 
                             proposal_sd=.02, verbose=TRUE,
                             log_scale=TRUE)
# Acceptance rate = 0.34