#' Function for plotting risk of case escaping active monitoring
#'
#' @param pstr_data a data object with posterior distributions of gamma parameters (must have shape and scale column)
#' @param u numeric value or vector of assumed duration(s) of time between exposure and monitoring
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param return_data logical, whether or not to return the data used for plotting
#'
#' @return if return_data is specified, it returns the data.frame used to create the plot
#' @export
#'
#' @examples plot_risk(ebola_gamma_pstr, u=c(5, 10))
plot_risk <- function(pstr_data, 
                      u=7, 
                      phi=c(.0001, .001, .01),
                      durations=5:25,
                      return_data=FALSE) {
    require(ggplot2)
    require(grid)
    dat <- expand.grid(shape=median(pstr_data$shape), 
                       scale=median(pstr_data$scale), 
                       u=u, 
                       phi=phi,
                       d=durations)
    dat <- prob_of_missing_case(dat)
    p <- ggplot(dat, aes(d, p, group=factor(phi), color=factor(phi))) + 
        geom_line() + facet_grid(.~u) +
        scale_y_log10(breaks=c(1e-06, 1e-04, 1e-02), labels=c("1/1,000,000", "1/10,000", "1/100")) +
        scale_x_continuous(expand = c(0, 0)) +
        ylab("probability of symptoms outside of active monitoring") + 
        xlab("duration of active monitoring (days)") +
        scale_color_discrete(name=expression(phi),
                             breaks=c("0.01", "0.001", "1e-04"), 
                             labels=c("1/100", "1/1,000", "1/10,000")) +
        theme(panel.margin = unit(.7, "lines")) + ggtitle("stratified by days since exposure")
    print(p)
    if(return_data) return(dat)
}

#' Function for plotting risk and uncertainty of case escaping active monitoring
#'
#' @param pstr_data a data object with posterior distributions of gamma parameters (must have shape and scale column)
#' @param u numeric value or vector of assumed duration(s) of time between exposure and monitoring
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param return_data logical, whether or not to return the data used for plotting
#'
#' @return if return_data is specified, it returns the data.frame used to create the plot
#' @export
#'
#' @examples plot_risk_uncertainty(ebola_gamma_pstr)
plot_risk_uncertainty <- function(pstr_data, 
                                  max_u=14, 
                                  phi=c(.0001, .001, .01),
                                  durations=5:25,
                                  return_data=FALSE) {
    require(ggplot2)
    require(grid)
    ## calculate 99th percentile of distributions, to pick confidence bounds
    pstr_data <- pstr_data %>% 
        mutate(p99 = qgamma(p=.99, shape=shape, scale=scale)) 
    
    ## find percentiles of distributions, based on bounds
    alpha <- 0.05
    param_bounds <- arrange(pstr_data, p99)[round(nrow(pstr_data)*c(alpha, 1-alpha)),]
    
    ## make dataset 
    nreps <- 100 ## reps needed to stabilize uniform integration
    dat_sim_pst_param <- expand.grid(shape = param_bounds$shape,
                                     scale = param_bounds$scale,
                                     phi=phi,
                                     d=durations,
                                     reps=1:nreps)
    dat_sim_pst_param$u <- sample(1:max_u, size=nrow(dat_sim_pst_param), replace=TRUE)
    
    dat_sim_pst_param <- prob_of_missing_case(dat_sim_pst_param)
    dat_sim_pst_param_sum <- group_by(dat_sim_pst_param, d, phi) %>%
        summarize(p05 = quantile(p, prob=.05),
                  p50 = quantile(p, prob=.50),
                  p95 = quantile(p, prob=.95)) %>%
        ungroup()
    
    ## something like this will make labels appear right
    ## dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param$phi, levels=c(1e-04, 2e-03), labels=c("phi==1/10000", "phi==1/500"))
    ## for now, leaving out fancy formatting
    dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param_sum$phi)
    p <- ggplot(dat_sim_pst_param_sum, aes(x=d, color=phi_lab, fill=phi_lab)) + 
        #facet_grid(.~phi_lab, labeller=label_parsed) +
        facet_grid(.~phi_lab) +
        geom_line(aes(y=p50)) + 
        geom_ribbon(aes(ymin=p05, ymax=p95), alpha=.2, color=NA) + 
        scale_y_log10() +
        ylab("Pr(symptoms after AM)") + 
        xlab("duration of active monitoring (days)")    
    print(p)
    if(return_data) return(dat)
}


