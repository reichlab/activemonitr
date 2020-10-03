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
    require(tidyr)
    require(dplyr)
    dat <- crossing(shape=median(pstr_data$shape),
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
#' @param dist character string specifying the parametric distribution of pstr_data, defaults to gamma
#' @param nsamp number of samples from posterior distribution upon which to base calculations
#' @param u numeric value or vector of assumed duration(s) of time between exposure and monitoring
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param ci_width numeric value between 0 and 1 indicating the nominal value for the eventually computed and displayed CI
#' @param yrange if not NULL, a vector of the ylim to plot
#' @param include_xlab logical, whether to include an x-axis label
#' @param include_legend logical, whether to include a legend
#' @param output_plot logical, whether or not to automatically output the plot
#' @param return_data logical, whether or not to return the data used for plotting
#' @param return_plot logical, whether or not to return the plotted grob
#'
#' @return if return_plot is specified, it returns the grob
#' @export
#'
#' @examples plot_risk_uncertainty(ebola_gamma_pstr)
plot_risk_uncertainty <- function(pstr_data,
                                  dist=c("gamma", "lnorm", "weibull"),
                                  nsamp=1000,
                                  u=runif(1000, 1, 14),
                                  phi=c(.0001, .001, .01),
                                  durations=5:25,
                                  ci_width=0.90,
                                  yrange=NULL,
                                  include_xlab=TRUE,
                                  include_legend=TRUE,
                                  output_plot=TRUE,
                                  return_data=FALSE,
                                  return_plot=FALSE) {
    require(ggplot2)
    require(tidyr)

    dist <- match.arg(dist)
    if(!(dist %in% c("gamma", "lnorm", "weibull")))
        stop('the only distributions supported at this time are "gamma", "lnorm", and "weibull"')
    if(dist %in% c("gamma", "weibull") & !("shape" %in% colnames(pstr_data)))
        stop('for the gamma or weibull distribution, please have a column named "shape"')
    if(dist=="gamma" & !("scale" %in% colnames(pstr_data)) & !("rate" %in% colnames(pstr_data)))
        stop('for the gamma distribution, please have a column named "scale" or "rate"')
    if(dist=="weibull" & !("scale" %in% colnames(pstr_data)))
        stop('for the weibull distribution, please have a column named "scale"')
    if(dist=="lnorm" & !("meanlog" %in% colnames(pstr_data)))
        stop('for the lnorm distribution, please have a column named "meanlog"')
    if(dist=="lnorm" & !("sdlog" %in% colnames(pstr_data)))
        stop('for the lnorm distribution, please have a column named "sdlog"')
    if(nrow(pstr_data)<nsamp)
        stop("number of samples needs to be less than or equal to than the number of draws from the posterior distribution")

    ## generate a sample of the posterior distribution from which to calculate
    pstr_samp <- pstr_data %>%
        sample_n(nsamp) %>%
        mutate(u = u) %>%
        crossing(d = durations,
                 phi = phi)

    dat_sim_pst_param <- prob_of_missing_case(pstr_samp, dist)
    dat_sim_pst_param_sum <- group_by(dat_sim_pst_param, d, phi) %>%
        summarize(ltp = quantile(p, prob=(1-ci_width)/2),
                  p50 = quantile(p, prob=.50),
                  utp = quantile(p, prob=1-(1-ci_width)/2)) %>%
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
        facet_grid(.~phi_lab, labeller=label_parsed) +
        geom_line(aes(y=p50)) +
        geom_ribbon(aes(ymin=ltp, ymax=utp), alpha=.2, color=NA) +
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

    if(output_plot)
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

#' Function for plotting risk and uncertainty of case escaping active monitoring
#'
#' @param dist character string specifying the parametric distribution (as in \code{\link[stats]{distributions}}), defaults to \code{"gamma"}
#' @param arg_list list of named numeric vectors to be plugged into distribution in \code{dist}
#' @param nsamp number of samples from posterior distribution upon which to base calculations
#' @param u numeric value or vector of assumed duration(s) of time between exposure and monitoring
#' @param udist character string specifying the parameteric distribution of assumed durations of time between exposure and monitoring (if missing \code{u}), defaults to \code{"unif"} (Uniform distribution)
#' @param uargs list of named numeric vectors to be plugged into distribution in \code{udist}, defaults to \code{list(min=1, max=14)}
#' @param phi probability a case becomes symptomatic
#' @param durations durations of active monitoring to plot
#' @param ci_width numeric value between 0 and 1 indicating the nominal value for the eventually computed and displayed CI
#' @param yrange if not NULL, a vector of the ylim to plot
#' @param include_xlab logical, whether to include an x-axis label
#' @param include_legend logical, whether to include a legend
#' @param output_plot logical, whether or not to automatically output the plot
#' @param return_data logical, whether or not to return the data used for plotting
#' @param return_plot logical, whether or not to return the plotted grob
#'
#' @return if return_plot is specified, it returns the grob
#' @export
#' @import gdist
#' @import ggplot2
#' @import tidyr
#'
#' @examples
#' ## load Ebola data and run
#' data(pstr_gamma_params_ebola)
#' plot_risk_gdist(dist="gamma",
#'                 arg_list=list(shape=pstr_gamma_params_ebola$shape,
#'                               scale=pstr_gamma_params_ebola$scale))
#'
plot_risk_gdist <- function(dist="gamma",
                            arg_list=list(shape=5.807,
                                          scale=0.948),
                            nsamp=1000,
                            u,
                            udist="unif",
                            uargs=list(min=1, max=14),
                            phi=c(.0001, .001, .01),
                            durations=5:25,
                            ci_width=0.90,
                            yrange=NULL,
                            include_xlab=TRUE,
                            include_legend=TRUE,
                            output_plot=TRUE,
                            return_data=FALSE,
                            return_plot=FALSE) {
    ## sample u's if missing
    if(missing(u)){
        u <- rdist(dist = udist, n = nsamp, arg_list = uargs)
    }
    ## generate a sample of the posterior distribution from which to calculate
    pstr_samp <- arg_list %>%
        as.data.frame() %>%
        slice_sample(n = nsamp) %>%
        mutate(u = u) %>%
        crossing(d = durations,
                 phi = phi) %>%
        mutate(q = d+u,
               lower.tail = FALSE)

    dat_sim_pst_param <- pdist(dist, arg_list=pstr_samp) * pstr_samp$phi
    dat_sim_pst_param_sum <- pstr_samp %>%
        mutate(p=dat_sim_pst_param) %>%
        group_by(d, phi) %>%
        summarize(ltp = quantile(p, prob=(1-ci_width)/2),
                  p50 = quantile(p, prob=.50),
                  utp = quantile(p, prob=1-(1-ci_width)/2)) %>%
        ungroup()

    ## something like this will make labels appear right
    if(identical(unique(dat_sim_pst_param_sum$phi), c(1e-04, 2e-03))) {
        dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param_sum$phi,
                                                levels=c(1e-04, 2e-03),
                                                labels=c("phi==1/10000",
                                                         "phi==1/500"))
    } else {
        ## for now, leaving out fancy formatting
        dat_sim_pst_param_sum$phi_lab <- factor(dat_sim_pst_param_sum$phi)
    }
    p <- ggplot(dat_sim_pst_param_sum, aes(x=d, color=phi_lab, fill=phi_lab)) +
        facet_grid(.~phi_lab, labeller=label_parsed) +
        geom_line(aes(y=p50)) +
        geom_ribbon(aes(ymin=ltp, ymax=utp), alpha=.2, color=NA) +
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

    if(output_plot)
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

##' make posterior likelihood plot of the median and 95th percentile of distribution
##'
##' @param mcmc_dfs the rbound outputs from inc_per_mcmc
##' @param kdes list of results from call to fit_kde
##' @param label_txt labels for points
##' @param colors colors for each region
##' @param show.legend logical, passed to geom_point
##' @param base.size size for font, to be passed to theme_bw()
##'
##' @return a plot
##' @export
##'
##' @examples plot_risk_uncertainty(ebola_gamma_pstr, kde_ebola, label_txt="Ebola", colors=c("#1b9e77", "#d95f02", "#7570b3"))
plot_modified_credible_regions <- function(mcmc_dfs, kdes, label_txt, colors, show.legend=FALSE, base.size=12) {
    require(ggplot2)
    require(reshape2)
    require(dplyr)

    ## set credible region data for KDE percentiles
    pstr_median <- data.frame(median=rep(NA, length(mcmc_dfs)),
                              p95=rep(NA, length(mcmc_dfs)),
                              disease = factor(label_txt,
                                               levels=label_txt, ordered=TRUE))

    ## make plot
    p <- ggplot() +  theme_bw() + xlab("median incubation period (days)") + ylab("95th percentile of incubation period (days)")
    for(i in 1:length(mcmc_dfs)) {
        pstr_median[i,"median"] <- median(mcmc_dfs[[i]]$median)
        pstr_median[i,"p95"] <- median(mcmc_dfs[[i]]$p95)
        p <- p + stat_contour(aes(x=Var1, y=Var2, z=value),
                              data=kdes[[i]][["contour_df"]],
                              breaks=kdes[[i]][["contour_level"]],
                              geom="polygon",
                              fill=colors[i], alpha=.6)
    }
    p <- p + geom_point(data=pstr_median, aes(x=median, y=p95, color=disease),
                        show.legend = show.legend) +
        theme_bw(base_size = base.size) +
        theme(legend.title=element_blank(),
              legend.position=c(1,1), legend.justification=c(1,1)) +
        scale_color_manual(values=colors)
    print(p)
    return(p)
}
