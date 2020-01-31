#' Function for plotting risk of case escaping active monitoring
#'
#' @param pstr_data a data object with posterior distributions of gamma parameters (must have shape and scale column)
#' @param u numeric value or vector of assumed duration(s) of time between exposure and monitoring
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param return_data logical, whether or not to return the data used for plotting
#'
#' @return if return_plot is specified, it returns the grob
#' @export
#'
#' @examples plot_risk(ebola_gamma_pstr, u=c(5, 10))
plot_risk <- function(pstr_data,
                      u=7,
                      phi=c(.0001, .001, .01),
                      durations=5:25,
                      return_plot=FALSE) {
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
    if(return_plot) return(p)
}

#' Function for plotting risk and uncertainty of case escaping active monitoring
#'
#' @param pstr_data a data object with posterior distributions of gamma parameters (must have shape and scale column)
#' @param long_tail_pct numeric value between 0 and 1, which percentile should be chosen to represent the long tail of the distribution
#' @param alpha numeric value between 0 and 1, percentiles to use (with 1-alpha) when choosing the bounds for the shape and scale parameters
#' @param nreps numeric, number of reps needed to stabilize uniform integration
#' @param max_u numeric value, the maximum assumed duration of time between exposure and monitoring
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param yrange if not NULL, a vector of the ylim to plot
#' @param include_xlab logical, whether to include an x-axis label
#' @param include_legend logical, whether to include a legend
#' @param return_data logical, whether or not to return the data used for plotting
#' @param return_plot logical, whether or not to return the plotted grob
#'
#' @return if return_plot is specified, it returns the grob
#' @export
#'
#' @examples plot_risk_uncertainty(ebola_gamma_pstr)
plot_risk_uncertainty <- function(pstr_data,
                                  long_tail_pct=0.99,
                                  alpha=0.05,
                                  nreps=100,
                                  max_u=14,
                                  phi=c(.0001, .001, .01),
                                  durations=5:25,
                                  yrange=NULL,
                                  include_xlab=TRUE,
                                  include_legend=TRUE,
                                  return_data=FALSE,
                                  return_plot=FALSE) {
    require(ggplot2)
    require(grid)
    ## calculate 99th percentile of distributions, to pick confidence bounds
    pstr_data <- pstr_data %>%
        mutate(ltp = qgamma(p=long_tail_pct, shape=shape, scale=scale))

    ## find percentiles of distributions, based on bounds
    param_bounds <- arrange(pstr_data, ltp)[round(nrow(pstr_data)*c(alpha, 1-alpha)),]

    ## make dataset
    dat_sim_pst_param <- expand.grid(idx = param_bounds$idx,
                                     phi=phi,
                                     d=durations,
                                     reps=1:nreps) %>%
        left_join(param_bounds %>%
                      select(idx, shape, scale))
    dat_sim_pst_param$u <- sample(1:max_u, size=nrow(dat_sim_pst_param), replace=TRUE)

    dat_sim_pst_param <- prob_of_missing_case(dat_sim_pst_param)
    dat_sim_pst_param_sum <- group_by(dat_sim_pst_param, d, phi) %>%
        summarize(p05 = quantile(p, prob=.05),
                  p25 = quantile(p, prob=.25),
                  p50 = quantile(p, prob=.50),
                  p75 = quantile(p, prob=.75),
                  p95 = quantile(p, prob=.95)) %>%
        ungroup()

    ## something like this will make labels appear right
    if(identical(unique(dat_sim_pst_param_sum$phi), c(1e-04, 2e-03))) {
        dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param_sum$phi,
                                                levels=c(1e-04, 2e-03),
                                                labels=c("phi==1/10000", "phi==1/500"))
    } else {
        ## for now, leaving out fancy formatting
        dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param_sum$phi)
    }
    p <- ggplot(dat_sim_pst_param_sum, aes(x=d, color=phi_lab, fill=phi_lab)) +
        #facet_grid(.~phi_lab, labeller=label_parsed) +
        facet_grid(.~phi_lab, labeller=label_parsed) +
        geom_line(aes(y=p50)) +
        geom_ribbon(aes(ymin=p05, ymax=p95), alpha=.2, color=NA) +
        scale_y_log10() +
        ylab("Pr(symptoms after AM)") +
        theme(legend.justification=c(0,0), legend.position=c(0,0))

    ## formatting options for xlab
    if(include_xlab) {
        p <- p + xlab("duration of active monitoring (days)")
    } else {
        p <- p + xlab(NULL)
    }

    ## remove legend if specified
    if(!include_legend) {
        p <- p + guides(color=FALSE, fill=FALSE)
    }

    ## zoom on y axis if desired
    if(!is.null(yrange)) {
        p <- p + coord_cartesian(ylim=yrange)
    }

    print(p)
    out <- vector("list", 2)
    names(out) <- c("data", "plot")
    if(return_plot) {
        out[["plot"]] <- p
    }
    if(return_data) {
        out[["data"]] <- dat_sim_pst_param_sum
    }
    return(out)
}


