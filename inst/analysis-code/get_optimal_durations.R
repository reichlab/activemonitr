## code for supplemental cost calculations

get_optimal_durations <- function(pstr_gamma_params, phis=c(1/10, 1/100, 1/1000, 1/10000)) {
    require(activeMonitr)
    require(dplyr)
    ## calculate per-day hazard of symptoms with other pathogens
    n_monitored <- 5379   ## per email from Neil 6/20/2016
    n_symptomatic <- 30   ## per email from Neil 6/20/2016, with conservative/high assumption
    s.21 <- 1-n_symptomatic*.3/n_monitored
    per_day_hazard <- 1-s.21^(1/21)
    
    ## $1.9m, 5321 monitored, 21 day duration
    per_day_cost <- 1900000/(n_monitored*21)
    
    cost_m <- c(10, 20)     ## per day cost of treatment
    cost_trt <- c(3e6, 5e6) ## cost of response to single case
    cost_exp <- c(0, 20e6)  ## cost of response to single case not captured by monitoring
    cost_falsepos <- c(10000,30000) ## cost of false positive testing
    
    cost_mat <- rbind(cost_m, cost_trt, cost_exp, cost_falsepos)
    
    gamma_params <- c(median = mean(pstr_gamma_params$median), 
                      shape = mean(pstr_gamma_params$shape),
                      scale = mean(pstr_gamma_params$scale))
    durs <- seq(.5, 10, by=.1)
    
    costs <- calc_monitoring_costs(durs = durs,
                                   probs_of_disease = phis,
                                   per_day_hazard = per_day_hazard,
                                   N = 100,
                                   cost_mat = cost_mat,
                                   gamma_params = gamma_params)
    
    costs$dur_median <- costs$dur*gamma_params['median']
    
    options("scipen"=10) 
    ## minimum costs 
    min_costs <- costs %>%
        group_by(phi) %>%
        summarize(min_cost = min(maxcost),
                  min_cost_dur = dur[which.min(maxcost)],
                  min_cost_dur_days = min_cost_dur * gamma_params['median']) %>%
        ungroup() %>%
        mutate(phi_lab = factor(as.character(MASS::fractions(phi, max.denominator = 1e10)))) %>%
        arrange(desc(min_cost_dur))
    return(min_costs)
}