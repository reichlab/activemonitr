## incubation period MCMC for posterior distribution data
## Nicholas Reich
## January 2015

##' Runs MCMC for incubation period calculation on posterior data
##' 
##' Runs MCMC for a set of data (dat) that is in the form of samples from the posterior distribution.
##' Currently assumes that paramters require a exp() transformation.
##' 
##' @param dat data with columns as draws from the posterior, rows as observations
##' @param dist distribution, one of G, L, W
##' @param nsamp number of samples to use in the MCMC
##' @param burnin number of burnin samples to remove at end
##' @param nthin factor by which to thin by
##' @param theta0 a vector with starting values of the paramters (par1, par2), on proposal scale (i.e. log)
##' @param proposal_sd sd for the Gaussian proposal distribution
##' @param verbose whether to print results
##' @param which_sample if 0, the MCMC will sample from the posterior on each iteration; if an integer, the column number of the data to be used
inc_per_mcmc_on_posterior <- function(dat, dist, nsamp, burnin, nthin, theta0, proposal_sd, verbose=FALSE, which_sample=0) {
    if(!(which_sample%%1==0) | which_sample<0)
        stop("which_sample must be non-negative whole number")
    ## storage
    cnames <- c("par1", "par2", "accept")
    chain <- matrix(NA, nrow=nsamp, ncol=length(cnames))
    colnames(chain) <- cnames
    chain[1,1:2] <- theta0
    chain[1,"accept"] <- 0
    
    ## define parameter transformation function
    ## for dist = G or W exp of both 
    ## for dist = L, exp of just sdlog
    if(dist=="L"){
        par_trans <- function(x) c(x[1], exp(x[2]))
    } else {
        par_trans <- function(x) exp(x)
    }
                        
    for(i in 2:nsamp){
        current_pars <- chain[i-1,1:2]
        if(which_sample==0) {
            t <- dat[,sample(ncol(dat), 1)]
        } else {
            t <- dat[,which_sample]
        }
        proposal_pars <- current_pars + rnorm(2, mean=0, sd=proposal_sd)
        current_lik <- siclik(par_trans(current_pars), t, dist=dist)
        proposal_lik <- siclik(par_trans(proposal_pars), t, dist=dist)
        a <- min(proposal_lik/current_lik, 1)
        chain[i,"accept"] <- rbinom(1, 1, a)
        chain[i,1:2] <- chain[i, "accept"]*proposal_pars + (1-chain[i, "accept"])*current_pars
    }
    if(verbose) {
        layout(matrix(1:3, nrow=3))
        plot(chain[,1], type="l")
        plot(chain[,2], type="l")
        plot(chain[,1], chain[,2])
        print(paste("acceptance rate =", round(mean(chain[,"accept"]), 2)))
        #print(apply(chain[(burnin+1):nrow(chain),], MARGIN=2, FUN=mean))
    }
    idx <- seq(burnin+1, nrow(chain), by=nthin)
    chain <- chain[idx,]
    return(chain)
}

##' Runs MCMC for incubation period calculation
##' 
##' Runs MCMC for a set of data (dat) that is in the form of single interval censored data.
##' 
##' @param dat data with columns for min and max incubation periods rows as observations
##' @param ip_cols character vector of length 2 with names of the columns containing the minimum and maximum possible incubation periods per observation
##' @param dist distribution, one of G, L, W
##' @param nsamp number of samples to use in the MCMC
##' @param burnin number of burnin samples to remove at end
##' @param nthin factor by which to thin by
##' @param theta0 a vector with starting values of the paramters (par1, par2), on proposal scale (i.e. log)
##' @param proposal_sd sd for the Gaussian proposal distribution
##' @param verbose whether to print results
##' @param log_scale logical specifying whether to return likelihood on log-scale
inc_per_mcmc<- function(dat, ip_cols, dist, nsamp, burnin, nthin, theta0, 
                        proposal_sd, verbose=FALSE, log_scale=FALSE) {
    ## ensure that ip_cols values are in the column names of dat
    if(!(ip_cols[1] %in% colnames(dat)) | !(ip_cols[2] %in% colnames(dat)) )
        stop("ip_cols must be in colnames(dat)")
    
    ## storage
    cnames <- c("par1", "par2", "accept")
    chain <- matrix(NA, nrow=nsamp, ncol=length(cnames))
    colnames(chain) <- cnames
    chain[1,1:2] <- theta0
    chain[1,"accept"] <- 0
    
    ## define parameter transformation function
    ## for dist = G or W exp of both 
    ## for dist = L, exp of just sdlog
    if(dist=="L"){
        par_trans <- function(x) c(x[1], exp(x[2]))
    } else {
        par_trans <- function(x) exp(x)
    }
    
    for(i in 2:nsamp){
        current_pars <- chain[i-1,1:2]
        tmin <- dat[,ip_cols[1]]
        tmax <- dat[,ip_cols[2]]
        proposal_pars <- current_pars + rnorm(2, mean=0, sd=proposal_sd)
        current_lik <- siclik_two_val(par_trans(current_pars), 
                                      tmin=tmin, tmax=tmax, dist=dist,
                                      log_scale=log_scale)
        proposal_lik <- siclik_two_val(par_trans(proposal_pars), 
                                       tmin=tmin, tmax=tmax, dist=dist,
                                       log_scale=log_scale)
        a <- ifelse(log_scale,
                    min(exp(proposal_lik-current_lik), 1),
                    min(proposal_lik/current_lik, 1))
        chain[i,"accept"] <- rbinom(1, 1, a)
        chain[i,1:2] <- chain[i, "accept"]*proposal_pars + (1-chain[i, "accept"])*current_pars
    }
    if(verbose) {
        layout(matrix(1:3, nrow=3))
        plot(chain[,1], type="l")
        plot(chain[,2], type="l")
        plot(chain[,1], chain[,2])
        print(paste("acceptance rate =", round(mean(chain[,"accept"]), 2)))
        #print(apply(chain[(burnin+1):nrow(chain),], MARGIN=2, FUN=mean))
    }
    idx <- seq(burnin+1, nrow(chain), by=nthin)
    chain <- chain[idx,]
    return(chain)
}

##' calculates likelihood for single interval-censored data
##' 
##' We use a consistent (par1, par2) notation for each distribution, they
##' map in the following manner: \deqn{Log-normal(meanlog=par1, sdlog=par2)}
##' \deqn{Gamma(shape=par1, scale=par2)} \deqn{Weibull(shape=par1, scale=par2)}
##' 
##' @param pars a vector with pararmeter 1 and parameter 2 of distribution (see details)
##' @param t assumed SIC observation(s) of the incubation period (t, t+1)
##' @param dist single charachter specifying distribution to use: 
##'     W=weibull, G=gamma, L=log-normal
##' 
siclik <- function(pars, t, dist){
    par1 <- pars[1]
    par2 <- pars[2]
    ## calculates the SIC likelihood as the difference in CDFs
    if (dist =="W"){
        prod(pweibull(t+1, shape=par1, scale=par2) - pweibull(t, shape=par1, scale=par2))
    } else if (dist =="G") {
        prod(pgamma(t+1, shape=par1, scale=par2) - pgamma(t, shape=par1, scale=par2))
    } else if (dist == "L"){
        prod(plnorm(t+1, meanlog=par1, sdlog=par2) - plnorm(t, meanlog=par1, sdlog=par2))
    } else {
        stop("distribution not supported")
    }
}

##' calculates likelihood for single interval-censored data
##' 
##' We use a consistent (par1, par2) notation for each distribution, they
##' map in the following manner: \deqn{Log-normal(meanlog=par1, sdlog=par2)}
##' \deqn{Gamma(shape=par1, scale=par2)} \deqn{Weibull(shape=par1, scale=par2)}
##' 
##' @param pars a vector with pararmeter 1 and parameter 2 of distribution (see details)
##' @param tmin minimum value(s) of the incubation period
##' @param tmax maximum value(s) of the incubation period
##' @param dist single charachter specifying distribution to use: 
##'     W=weibull, G=gamma, L=log-normal
##' @param log_scale logical specifying whether to return likelihood on log-scale
##' 
siclik_two_val <- function(pars, tmin, tmax, dist, log_scale=FALSE){
    par1 <- pars[1]
    par2 <- pars[2]
    ## calculates the SIC likelihood as the difference in CDFs
    if (dist =="W"){
        liks <- pweibull(tmax, shape=par1, scale=par2) - pweibull(tmin, shape=par1, scale=par2)
    } else if (dist =="G") {
        liks <- pgamma(tmax, shape=par1, scale=par2) - pgamma(tmin, shape=par1, scale=par2)
    } else if (dist == "L"){
        liks <- plnorm(tmax, meanlog=par1, sdlog=par2) - plnorm(tmin, meanlog=par1, sdlog=par2)
    } else {
        stop("distribution not supported")
    }
    
    ## calculate lik on log scale or not
    if(log_scale) {
        lik <- sum(log(liks))
    } else {
        lik <- prod(liks)
    }
    return(lik)
}



##' make posterior likelihood plot
##' 
##' @param chain the output from inc_per_mcmc
##' @param dist the distribution assumed for the model output.
plot_posterior <- function(chain, dist) {
    require(coda)
    require(ggplot2)
    chain <- data.frame(chain)
    if(dist == "L") {
        chain[,1:2] <- cbind(exp(chain[,1]), exp(exp(chain[,2])))
        xlab_text <- "median"
        ylab_text <- "dispersion"
    } else {
        chain[,1:2] <- exp(chain[,1:2])
        xlab_text <- "shape"
        ylab_text <- "scale"
    }
    par1_median <- median(chain$par1)
    par2_median <- median(chain$par2)
    
    ## get contour line data
    nbin=100
    p1 <- seq(min(chain$par1), max(chain$par1), length.out=nbin)
    p2 <- seq(min(chain$par2), max(chain$par2), length.out=nbin)
    ptiles <- expand.grid(p1, p2)
    colnames(ptiles) <- c("p1", "p2")
    ptiles$p95 <- switch(dist,
                         W=qweibull(.95, shape=ptiles$p1, scale=ptiles$p2),
                         G=qgamma(.95, shape=ptiles$p1, scale=ptiles$p2),
                         L=qlnorm(.95, meanlog=log(ptiles$p1), log(sdlog=ptiles$p2)))
    
    p <- ggplot(chain, aes(x = par1, y = par2)) +
        #stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
        geom_point(alpha=I(.01)) +
        stat_ellipse(color="red", level=0.90)+
        stat_ellipse(color="red", level=0.95)+
        stat_contour(data=ptiles, aes(x=p1, y=p2, z=p95), breaks=c(21), color="blue") +
        annotate("point", x=par1_median, y=par2_median, shape=3, color="red") +
        theme_bw() + xlab(xlab_text) + ylab(ylab_text)
    print(p)
}

##' make posterior likelihood plot for gamma distribution
##' 
##' @param mcmc_df the rbound output from inc_per_mcmc
plot_gamma_posterior <- function(mcmc_df, H, p95.breaks=c(10, 15, 21)) {
    require(ggplot2)
    require(ks)
    require(reshape2)
    require(dplyr)
    shape_median <- median(mcmc_df$shape)
    scale_median <- median(mcmc_df$scale)
    
    ## get contour line data for p95 lines
    nbin=100
    p1 <- seq(min(mcmc_df$shape), max(mcmc_df$shape), length.out=nbin)
    p2 <- seq(min(mcmc_df$scale), max(mcmc_df$scale), length.out=nbin)
    ptiles <- expand.grid(p1, p2)
    colnames(ptiles) <- c("p1", "p2")
    ptiles$p95 <- qgamma(.95, shape=ptiles$p1, scale=ptiles$p2)
    
    ## get contour line data for KDE percentiles
    if(nrow(mcmc_df)>10000) {
        idx <- sample(1:nrow(mcmc_df), size=10000, replace=FALSE)
    } else {
        idx <- sample(1:nrow(mcmc_df), size=nrow(mcmc_df), replace=FALSE)
    }
    x <- mcmc_df[idx,1:2]
    fhat <- kde(x, H=H, compute.cont=TRUE)
    dimnames(fhat[['estimate']]) <- list(fhat[["eval.points"]][[1]], 
                                         fhat[["eval.points"]][[2]])
    contour_df <- melt(fhat[['estimate']])
    
    if(nrow(mcmc_df)>5000) {
        mcmc_df <- sample_n(mcmc_df, size=5000)
    }
    ## make plot
    p <- ggplot(mcmc_df, aes(x = shape, y = scale)) +
        geom_point(alpha=I(.05)) +
        stat_contour(data=ptiles, aes(x=p1, y=p2, z=p95, colour=..level..), breaks=p95.breaks) +
        annotate("point", x=shape_median, y=scale_median, shape=3, color="red") +
        geom_contour(aes(x=Var1, y=Var2, z=value), data=contour_df, 
                     breaks=fhat[["cont"]]["5%"], color="darkred") + 
        geom_contour(aes(x=Var1, y=Var2, z=value), data=contour_df, 
                     breaks=fhat[["cont"]]["10%"], color="red") +
        scale_x_log10() + scale_y_log10() +
        theme_bw() + xlab("shape") + ylab("scale")
    print(p)
    return(p)
}

##' make posterior likelihood plot for gamma distribution
##' 
##' @param mcmc_dfs the rbound outputs from inc_per_mcmc
##' @param Hs bandwidths matrices from inc_per_mcmc
##' @param p95.breaks contours to show of 95th percentiles
##' @params n_points max number of points to include in the KDE
plot_credible_regions <- function(mcmc_dfs, Hs, p95.breaks=c(10, 15, 21), n_points=10000) {
    require(ggplot2)
    require(ks)
    require(reshape2)
    require(dplyr)
    #shape_median <- median(mcmc_df$shape)
    #scale_median <- median(mcmc_df$scale)
    
    ## set plot range
    shape_range <- c(2.3, 47)
    scale_range <- c(.27, 4.5)
    
    ## colors
    colors <- c("#1b9e77", "#d95f02", "#7570b3")
    
    ## get contour line data for p95 lines
    nbin=100
    p1 <- seq(shape_range[1], shape_range[2], length.out=nbin)
    p2 <- seq(scale_range[1], scale_range[2], length.out=nbin)
    ptiles <- expand.grid(p1, p2)
    colnames(ptiles) <- c("p1", "p2")
    ptiles$p95 <- qgamma(.95, shape=ptiles$p1, scale=ptiles$p2)
    
    ## get contour line data for KDE percentiles
    contour_dfs <- vector("list", length(mcmc_dfs))
    contour_levels <- rep(0, length(mcmc_dfs))
    max_size <- n_points
    for(i in 1:length(mcmc_dfs)){
        if(nrow(mcmc_dfs[[i]])>max_size) {
            idx <- sample(1:nrow(mcmc_dfs[[i]]), size=max_size, replace=FALSE)
        } else {
            idx <- sample(1:nrow(mcmc_dfs[[i]]), size=nrow(mcmc_dfs[[i]]), replace=FALSE)
        }
        x <- mcmc_dfs[[i]][idx,1:2]
        fhat <- kde(x, H=Hs[[i]], compute.cont=TRUE)
        dimnames(fhat[['estimate']]) <- list(fhat[["eval.points"]][[1]], 
                                             fhat[["eval.points"]][[2]])
        contour_dfs[[i]] <- melt(fhat[['estimate']])
        contour_levels[i] <- fhat[["cont"]]["5%"]
    }
    
    ## make plot
    p <- ggplot(ptiles) +
        stat_contour(aes(x=p1, y=p2, z=p95), breaks=p95.breaks) +
        #annotate("point", x=shape_median, y=scale_median, shape=3, color="red") 
        scale_x_log10(breaks=c(2:5, 20, 40)) + scale_y_log10(breaks=c(.2, .5, 1:4)) +
        theme_bw() + xlab("shape") + ylab("scale")
    for(i in 1:length(mcmc_dfs)) {
        p <- p + stat_contour(aes(x=Var1, y=Var2, z=value), data=contour_dfs[[i]], 
                              breaks=contour_levels[i], geom="polygon", 
                              fill=colors[i], alpha=.6) 
    }
    print(p)
    return(p)
}


##' make posterior likelihood plot for gamma distribution
##' 
##' @param mcmc_dfs the rbound outputs from inc_per_mcmc
##' @param kdes list of results from call to fit_kde
##' @param label_txt labels for points
plot_modified_credible_regions <- function(mcmc_dfs, kdes, label_txt) {
    require(ggplot2)
    require(ks)
    require(reshape2)
    require(dplyr)

    ## colors
    colors <- c("#1b9e77", "#d95f02", "#7570b3")
    
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
    p <- p + geom_point(data=pstr_median, aes(x=median, y=p95, color=label_txt),
                        show.legend = FALSE) +
        scale_color_manual(values=colors)
    print(p)
    return(p)
}

## fit kde for use in smoothing
fit_kde <- function(mcmc_df, H, max_size) {
    require(reshape2)
    if(nrow(mcmc_df)>max_size) {
        idx <- sample(1:nrow(mcmc_df), size=max_size, replace=FALSE)
    } else {
        idx <- sample(1:nrow(mcmc_df), size=nrow(mcmc_df), replace=FALSE)
    }
    ## calculate median and p95
    x <- mcmc_df[idx, c("median", "p95")]
    fhat <- kde(x, H=H, compute.cont=TRUE)
    dimnames(fhat[['estimate']]) <- list(fhat[["eval.points"]][[1]], 
                                         fhat[["eval.points"]][[2]])
    contour_df <- melt(fhat[['estimate']])
    contour_level <- fhat[["cont"]]["5%"]
    return(list(contour_df=contour_df, contour_level=contour_level))
}
    
    

## plot densities
plot_gamma_densities <- function(mcmc_df, kde, n_to_plot, to=30, 
                                 plotcolor, label_txt, xaxs_lab=TRUE) {
    median_shape <- median(mcmc_df$shape)
    median_scale <- median(mcmc_df$scale)
    
    ## idx
    contour_df <- kde[["contour_df"]]
    credible_region_idx <- which(contour_df$value>kde[["contour_level"]])
    marginal_var1_ci <- range(contour_df$Var1[credible_region_idx])
    marginal_var2_ci <- range(contour_df$Var2[credible_region_idx])
    
    ## plot the first one
    xgam <- seq(0, to, by=.1)
    gam_dat <- data_frame(xgam, 
                          Fgam=dgamma(xgam, 
                                      shape=median_shape,
                                      scale=median_scale))
    p <- ggplot() + geom_line(data=gam_dat, aes(x=xgam, y=Fgam), color=plotcolor) +
        annotate("rect", 
                 xmin=marginal_var1_ci[1], xmax=marginal_var1_ci[2], 
                 ymin=-10, ymax=10,
                 alpha=.2) +
        annotate("rect", 
                 xmin=marginal_var2_ci[1], xmax=marginal_var2_ci[2], 
                 ymin=-10, ymax=10,
                 alpha=.2) 
    
    ## add samples
    for(i in 1:n_to_plot){
        shp <- as.numeric(mcmc_df[i,"shape"])
        scl <- as.numeric(mcmc_df[i,"scale"])
        gam_dat <- data_frame(xgam, 
                              Fgam=dgamma(xgam, shape=shp,
                                          scale=scl))
        p <- p + geom_line(data=gam_dat, aes(x=xgam, y=Fgam), 
                           color=plotcolor, alpha=I(5/n_to_plot))
    }
    # reimprint median
    gam_dat <- data_frame(xgam, 
                          Fgam=dgamma(xgam, shape=median_shape,
                                      scale=median_scale))
    p <- p + geom_line(data=gam_dat, aes(x=xgam, y=Fgam), color=plotcolor, size=1) +
        geom_text(aes(x=to-5, y=.2, label=label_txt)) +
        ylab("density") + coord_cartesian(ylim=c(0, .225))
    if(xaxs_lab) {
        p <- p + xlab("durtion of incubation period (days)")
    } else {
        p <- p + xlab(NULL)
    }
    print(p)
    return(p)
}

##' plot the distribution with MLE parameters
##' 
##' @param chain the output from inc_per_mcmc
##' @param dist the distribution assumed for the model output.
plot_ml_dist <- function(chain, dist, from=0, to=25) {
    par1_median <- exp(median(chain[,"par1"]))
    par2_median <- exp(median(chain[,"par2"]))
    x <- seq(from, to, length.out=1000)
    y <- switch(dist,
                W=dweibull(x, shape=par1_median, scale=par2_median),
                G=dgamma(x, shape=par1_median, scale=par2_median),
                L=dlnorm(x, meanlog=log(par1_median), sdlog=par2_median))
    par(mar=c(2,5,1,1))
    plot(x, y, las=1, type="l", bty="n",
         ylab="density", xlab="incubation period (in days)")
}


## from http://stackoverflow.com/questions/23437000/how-to-plot-a-contour-line-showing-where-95-of-values-fall-within-in-r-and-in
## not used anymore, superceded by stat_ellipse
getLevel <- function(x,y,prob=0.95) {
    kk <- MASS::kde2d(x,y)
    dx <- diff(kk$x[1:2])
    dy <- diff(kk$y[1:2])
    sz <- sort(kk$z)
    c1 <- cumsum(sz) * dx * dy
    approx(c1, sz, xout = 1 - prob)$y
}


#' Calculate robust bandwidths for plotting kernel density contours
#'
#' @param params output from inc_per_mcmc
#'
#' @return a matrix of bandwidths
get_robust_bandwidths <- function(params, cols=c("shape", "scale")) {
    require(ks)
    x <- params[, cols]
    Hscv(x)
}