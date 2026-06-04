#' Cost calculations for active monitoring scenarios
#' @param durs a vector of AM durations to consider, in multiples of median incubation period values
#' @param probs_of_disease a vector of probabilities representing the chances that different subgroups have of developing the disease of interest
#' @param per_day_hazard hazard to pass to the calculation of theta
#' @param N number of indiviuals for which to calculate the costs
#' @param cost_mat 4x2 matrix of costs where each named row corresponds to a cost, and the columns are the lower and upper bounds
#' @param dist character string specifying the parametric distribution of the incubation period; defaults to "gamma"
#' @param gamma_params the median, shape and scale parameters for the assumed-gamma incubation period distribution
#' @param return_scalar logical, whether to return the scalar max cost value using the first value of durs and probs_of_disease
#' @return a data.frame with one row per (probs_of_disease, durs) combination and
#'   columns \code{phi}, \code{dur}, \code{mincost}, \code{maxcost} (or, if
#'   \code{return_scalar=TRUE}, the scalar maxcost for the first combination)
#'
#' @importFrom stats pgamma plnorm
#' @export
calc_monitoring_costs <- function(durs, probs_of_disease, per_day_hazard, N, cost_mat, dist=c("gamma", "lnorm"), gamma_params, return_scalar=FALSE) {
    dist <- match.arg(dist)
    ## checks
    if(any(probs_of_disease>1) | any(probs_of_disease<0)) stop("all probs_of_disease must be between 0 and 1")

    ## extract the costs into individual vectors
    cost_m <- cost_mat["cost_m",]     ## per day cost of treatment
    cost_trt <- cost_mat["cost_trt",] ## cost of response to single case
    cost_exp <- cost_mat["cost_exp",]  ## cost of response to single case not captured by monitoring
    cost_falsepos <- cost_mat["cost_falsepos",] ## cost of false positive testing

    ## extract incubation period parameters
    med_ip <- gamma_params["median"]
    if(dist=="gamma"){
        shp <- gamma_params["shape"]
        scl <- gamma_params["scale"]
    }
    if(dist=="lnorm"){
        mlog <- gamma_params["meanlog"]
        slog <- gamma_params["sdlog"]
    }
  phis <- probs_of_disease
  n_out <- length(phis)*length(durs)
  phi_out <- numeric(n_out)
  dur_out <- numeric(n_out)
  mincost_out <- numeric(n_out)
  maxcost_out <- numeric(n_out)
  k <- 0
  for(i in seq_along(phis)) {
    phi <- phis[i]
    for(j in seq_along(durs)) {
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
      if(dist=="gamma")
          ip_prob <- pgamma(dur*med_ip, shape=shp, scale=scl)
      if(dist=="lnorm")
          ip_prob <- plnorm(dur*med_ip, meanlog=mlog, sdlog=slog)
      pi_vec <- c((1-phi)*(1-theta),
                  (1-phi)*theta,
                  phi*ip_prob,
                  phi*(1-ip_prob))
      num_costs <- t(costs %*% pi_vec * N) ## length-2: lower (min) and upper (max) cost
      phi_out[k] <- phi
      dur_out[k] <- dur
      mincost_out[k] <- num_costs[1]
      maxcost_out[k] <- num_costs[2]
    }
  }
  num_out <- data.frame(phi=phi_out, dur=dur_out,
                        mincost=mincost_out, maxcost=maxcost_out)
  if(return_scalar) num_out <- as.numeric(num_out[1, "maxcost"])
  return(num_out)
}
