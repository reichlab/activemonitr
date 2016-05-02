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
nsamp <- 60000 
burnin <- 10000 
nthin <- 1
fit_mers <- inc_per_mcmc(dat=d, ip_cols = c("IncP_min", "IncP_max"),
                         dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                         theta0=log(runif(2, 2, 4)), proposal_sd=.1, verbose=TRUE)
## acceptance rate = 0.32 
# acf decreasing with lag


##############
## smallpox ##
##############

d <- read.csv('inst/raw-data/smallpox-data.csv')
d$ip_min <- d$reported_incper
d$ip_max <- d$reported_incper + 1

## MCMC setup
nsamp <- 320000 
burnin <- 20000 
nthin <- 6      

## run the MCMC fits
fit_smallpox <- inc_per_mcmc(dat=d, ip_cols = c("ip_min", "ip_max"),
                             dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                             theta0=log(c(runif(1, 35, 45), runif(1, .2, .5))), 
                             proposal_sd=.02, verbose=TRUE,
                             log_scale=TRUE)
# Acceptance rate = 0.34
# acf decreasing with lag


##############
## Ebola    ##
##############

d <- read.table('inst/raw-data/DURATION.INCUB.txt')
dcc <- d[complete.cases(d),]

nsamp <- 110000
burnin <- 10000
nthin <- 1

which_samp <- 1 ## the posterior draw to test on
fit_ebola <- inc_per_mcmc_on_posterior(dat=dcc, dist="G", nsamp=nsamp, burnin=burnin, nthin=nthin, 
                                       theta0=log(runif(2, 2, 4)), proposal_sd=.1, verbose=TRUE,
                                       which_sample=1)

# Acceptance rate = 0.33
# acf decreasing with lag

