#' Cost calculations for active monitoring scenarios
#' @param durs a vector of AM durations to consider, in multiples of median incubation period values
#' @param probs_of_disease a vector of probabilities representing the chances that different subgroups have of developing the disease of interest
#' @param per_day_hazard hazard to pass to the calculation of theta
#' @param N number of indiviuals for which to calculate the costs
#' @param cost_mat 4x2 matrix of costs where each named row corresponds to a cost, and the columns are the lower and upper bounds
#' @param gamma_params the median, shape and scale parameters for the assumed-gamma incubation period distribution
#' @param return_scalar logical, whether to return the scalar max cost value using the first value of durs and probs_of_disease
#' @return a length(probs_of_disease)xlength(durs) matrix of costs, one row per subgroup, one column per duration
#'
#' @export
calc_monitoring_costs <- function(durs, probs_of_disease, per_day_hazard, N, cost_mat, gamma_params, return_scalar=FALSE) {
  require(dplyr)
  ## checks
  if(any(probs_of_disease>1) | any(probs_of_disease<0)) stop("all probs_of_disease must be between 0 and 1")

  ## extract the costs into individual vectors
  cost_m <- cost_mat["cost_m",]     ## per day cost of treatment
  cost_trt <- cost_mat["cost_trt",] ## cost of response to single case
  cost_exp <- cost_mat["cost_exp",]  ## cost of response to single case not captured by monitoring
  cost_falsepos <- cost_mat["cost_falsepos",] ## cost of false positive testing
  
  ## extract incubation period parameters
  med_ip <- gamma_params["median"]
  shp <- gamma_params["shape"]
  scl <- gamma_params["scale"]
  
  phis <- probs_of_disease
  # out <- matrix(NA, nrow=length(phis), ncol=length(durs))
  # colnames(out) <- durs
  # rownames(out) <- phis
  num_out <- data_frame(phi=rep(NA, length(phis)*length(durs)),
                        dur=rep(NA, length(phis)*length(durs)),
                        mincost=rep(NA, length(phis)*length(durs)),
                        maxcost=rep(NA, length(phis)*length(durs)))
  k <- 0
  for(i in 1:length(phis)) {
    phi <- phis[i]
    for(j in 1:length(durs)) {
      k <- k+1
      dur <- durs[j]
      theta <- 1-(1-per_day_hazard)^dur ## prob of symptoms from other disease
      ## calculate costs
      c1 <- cost_m * dur * med_ip ## monitoring only
      c1a <- c1 + cost_falsepos   ## cost of monitoring + rule-out testing
      c2 <- c1 + cost_trt         ## cost of response to single case
      c3 <- c1 + c2 + cost_exp    ## cost of response to single case + exposure followups
      costs <- cbind(c1, c1a, c2, c3)
      ## calculate probabilities
      ip_prob <- pgamma(dur*med_ip, shape=shp, scale=scl)
      pi_vec <- c((1-phi)*(1-theta), 
                  (1-phi)*theta, 
                  phi*ip_prob, 
                  phi*(1-ip_prob))
      num_costs <- t(costs %*% pi_vec * N)
#      out[i,j] <- paste0("$", formatC(round(num_costs[1], -3)/1000, big.mark=",", format="d"), 
#                         "-$", formatC(round(num_costs[2], -3)/1000, big.mark=",", format="d"))
      num_out[k,] <- c(phi, dur, num_costs)
    }
  }
  if(return_scalar) num_out <- as.numeric(num_out[1,"maxcost"])
  return(num_out)
}
