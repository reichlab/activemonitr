#######################################
## Title: active-monitoring server.R ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ##
## Date Created:  12/27/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
shinyServer(function(input, output, session) {
    ## create plot of cost data
    output$plot_costs <-renderPlot({
        cost_m <- input$plot1_cost_per_day     ## per day cost of treatment
        cost_trt <- input$plot1_cost_per_case*1e6 ## cost of response to single case
        cost_exp <- c(0, input$plot1_secondary_cases*input$plot1_cost_per_case[2]*1e6)  ## cost of response to single case not captured by monitoring
        cost_falsepos <- 1000*input$plot1_cost_false_pos ## cost of false positive testing

        cost_mat <- rbind(cost_m, cost_trt, cost_exp, cost_falsepos)

        pstr_params <- switch(input$plot1_disease,
                              COVID1 = boot_lnorm_params_covid,
                              COVID2 = bets_results_bootstrap,
                              COVID3 = boot_lnorm_params_covid_ucd,
                              Ebola = pstr_gamma_params_ebola,
                              Mers = pstr_gamma_params_mers,
                              Smallpox = pstr_gamma_params_smallpox)

        if(input$plot1_disease %in% c("COVID1", "COVID3")){
            inc_dist <- "lnorm"
            gamma_params <- c(median = mean(pstr_params$median),
                              meanlog = mean(pstr_params$meanlog),
                              sdlog = mean(pstr_params$sdlog))
        } else if(input$plot1_disease=="COVID2"){
            inc_dist <- "gamma"
            gamma_params <- c(median = mean(pstr_params$median),
                              shape = mean(pstr_params$shape),
                              scale = mean(1/pstr_params$rate))
        } else{
            inc_dist <- "gamma"
            gamma_params <- c(median = mean(pstr_params$median),
                              shape = mean(pstr_params$shape),
                              scale = mean(pstr_params$scale))
        }
        durs <- seq(.1, 10, by=.1)
        phis <- as.numeric(input$plot1_prob_symptoms)

        costs <- calc_monitoring_costs(durs = durs,
                                       probs_of_disease = phis,
                                       per_day_hazard = 1/input$plot1_per_day_hazard_denom,
                                       N = 100,
                                       cost_mat = cost_mat,
                                       dist=inc_dist,
                                       gamma_params = gamma_params,
                                       return_scalar=FALSE)

        costs$phi_lab <- factor(as.character(MASS::fractions(costs$phi, max.denominator = 1e10)))
        costs$dur_median <- costs$dur*gamma_params['median']

        ## minimum costs
        min_costs <- costs %>%
            group_by(phi) %>%
            summarize(min_cost = min(maxcost),
                      min_cost_dur = dur[which.min(maxcost)],
                      min_cost_dur_days = min_cost_dur * gamma_params['median']) %>%
            ungroup() %>%
            mutate(phi_lab = factor(as.character(MASS::fractions(phi, max.denominator = 1e10))))

        ggplot(costs, aes(x=dur_median,
                          color=phi_lab, fill=phi_lab)) +
            geom_ribbon(aes(ymin=mincost, ymax=maxcost), alpha=.7) +
            scale_y_log10(labels=dollar,
                          name='Cost range of monitoring 100 individuals') +
            scale_x_continuous(name='Duration of active monitoring (in days)', expand=c(0,0)) +
            coord_cartesian(xlim=c(5, 43)) +
            scale_fill_brewer(palette="Dark2") +
            scale_color_brewer(palette="Dark2") +
            ## horizontal dashed line segments
            geom_segment(data=min_costs,
                         aes(x=3, xend=min_cost_dur_days,
                             y=min_cost, yend=min_cost, color=phi_lab),
                         linetype=2) +
            ## vertical dashed line segments
            geom_segment(data=min_costs,
                         aes(x=min_cost_dur_days, xend=min_cost_dur_days,
                             y=0, yend=min_cost, color=phi_lab),
                         linetype=2) +
            ## labels for line segments
            geom_text(data=min_costs, nudge_x = 1,
                      aes(x=min_cost_dur_days, y=1000,
                          label=paste(round(min_cost_dur_days),"d"))) +
            theme(legend.title=element_blank(), legend.position=c(1, 1), legend.justification=c(1, 1)) +
            ggtitle("Model-based cost range for monitoring 100 individuals") +
            annotate("text", x=12, y=2000,
                     label="dashed lines indicate an optimal \n duration of active monitoring")

    })

    ## create the plot2 of incubation period data
    output$plot_inc_per <-renderPlot({
        plot_modified_credible_regions(list(pstr_gamma_params_ebola,
                                            pstr_gamma_params_mers,
                                            pstr_gamma_params_smallpox,
                                            boot_lnorm_params_covid,
                                            bets_results_bootstrap,
                                            boot_lnorm_params_covid_ucd),
                                       kdes=list(kde_ebola,
                                                 kde_mers,
                                                 kde_smallpox,
                                                 kde_covid,
                                                 kde_covid_bets,
                                                 kde_covid_ucd),
                                       label_txt=c("Ebola", "MERS-CoV", "Smallpox", "COVID-19 (Lauer, Grantz)", "COVID-19 (Zhao)", "COVID-19 (McAloon)"),
                                       colors=cbbPalette,
                                       show.legend=TRUE,
                                       base.size=18)
    })


    ## create plot of undetected infections data
    output$plot_risk_uncertainty <-renderPlot({
        # browser()
        pstr_params <- switch(input$plot3_disease,
                              COVID1 = boot_lnorm_params_covid,
                              COVID2 = bets_results_bootstrap,
                              COVID3 = boot_lnorm_params_covid_ucd,
                              Ebola = pstr_gamma_params_ebola,
                              Mers = pstr_gamma_params_mers,
                              Smallpox = pstr_gamma_params_smallpox)
        ## set plot distribution parameters
        durs <- input$plot3_duration[1]:input$plot3_duration[2]
        phis <- as.numeric(input$plot3_prob_symptoms)
        plot_dist <- ifelse(input$plot3_disease %in% c("COVID1", "COVID3"),
                            "lnorm", "gamma")
        ## make risk plot
        p <- plot_risk_gdist(dist=plot_dist,
                             arg_list=pstr_params,
                             nsamp=1000,
                             udist="unif",
                             uargs=list(min=input$plot3_u[1],
                                        max=input$plot3_u[2]),
                             durations=durs,
                             phi=phis,
                             ci_width=input$plot3_ci,
                             output_plot=FALSE,
                             return_data=T,
                             return_plot=T)
        p_min <- max(c(10^(min(p$data$p50) %>% log10() %>% floor()), 1e-6))
        p_max <- 10^(max(p$data$p50) %>% log10() %>% ceiling())
        p$plot$data$phi_lab <- factor(as.character(MASS::fractions(p$plot$data$phi, max.denominator = 1e6)))
        p$plot +
            facet_grid(.~phi_lab) +
            scale_y_log10("Proportion of symptomatic infections that\ndevelop symptoms after active monitoring",
                          breaks=10^(log10(p_min):log10(p_max)),
                          labels=paste0("1/",
                                        formatC(10^(abs(log10(p_min):log10(p_max))),
                                                format="d",big.mark=","))) +
            scale_x_continuous("Duration of active monitoring, in days") +
            scale_color_discrete(guide=FALSE) +
            scale_fill_discrete(guide=FALSE) +
            coord_cartesian(ylim=c(p_min, p_max)) +
            theme(axis.text=element_text(color="black"),
                  strip.background=element_rect(fill="white"))

    })

    ## create table of undetected infections data
    output$tbl_risk_uncertainty <- renderDataTable({
        # browser()
        pstr_params <- switch(input$plot3_disease,
                              COVID1 = boot_lnorm_params_covid,
                              COVID2 = bets_results_bootstrap,
                              COVID3 = boot_lnorm_params_covid_ucd,
                              Ebola = pstr_gamma_params_ebola,
                              Mers = pstr_gamma_params_mers,
                              Smallpox = pstr_gamma_params_smallpox)

        durs <- input$plot3_duration[1]:input$plot3_duration[2]
        phis <- as.numeric(input$plot3_prob_symptoms)
        plot_dist <- ifelse(input$plot3_disease %in% c("COVID1", "COVID3"),
                            "lnorm", "gamma")

        p <- plot_risk_gdist(dist=plot_dist,
                             arg_list=pstr_params,
                             nsamp=1000,
                             udist="unif",
                             uargs=list(min=input$plot3_u[1],
                                        max=input$plot3_u[2]),
                             durations=durs,
                             phi=phis,
                             ci_width=input$plot3_ci,
                             output_plot=FALSE,
                             return_data=T,
                             return_plot=T)

        p$data %>%
            # filter(d %in% c(min(durs), round(median(durs)), max(durs))) %>%
            transmute(`Duration, in days` = d,
                      `Lower bound` = round(1e4*ltp,2),
                      `Median` = round(1e4*p50,2),
                      `Upper bound` = round(1e4*utp,2))

    }, options=list(searching=F, paginate=F,info=F,
                    pageLength=input$plot3_duration[2],
                    scroller=T, scrollY=300))


})

compute_data <- function(updateProgress = NULL) {
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))

    for (i in 1:10) {
        Sys.sleep(0.25)

        # Compute new row of data
        new_row <- data.frame(x = rnorm(1), y = rnorm(1))

        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
            text <- paste0("Computing data: ", round((i-1)*10,1), "%")
            updateProgress(detail = text)
        }

        # Add the new row of data
        dat <- rbind(dat, new_row)
    }

    dat
}

