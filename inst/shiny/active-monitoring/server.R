#######################################
## Title: active-monitoring server.R ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ## 
## Date Created:  12/27/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
shinyServer(function(input, output, session) {
  data_df <- reactive({
    ## choose data based on the type of disease
    if (input$plot1_radio=="Ebola"){
    data_df <- pstr_gamma_params_ebola
    }
    else if (input$plot1_radio=="Mers") {
    data_df<-pstr_gamma_params_mers  
    }
    else {
    data_df<-pstr_gamma_params_smallpox  
    }
    
    ## Output reactive dataframe
    data_df
  })
  
    ## create plot of cost data
    output$plot_costs <-renderPlot({
        cost_m <- input$plot1_cost_per_day     ## per day cost of treatment
        cost_trt <- c(3e6, 5e6) ## cost of response to single case
        cost_exp <- c(0, 20e6)  ## cost of response to single case not captured by monitoring
        cost_falsepos <- 1000*input$plot1_cost_false_pos ## cost of false positive testing
        
        cost_mat <- rbind(cost_m, cost_trt, cost_exp, cost_falsepos)
        
        pstr_params <- switch(input$plot1_disease,
                              Ebola = pstr_gamma_params_ebola,
                              Mers = pstr_gamma_params_mers,
                              Smallpox = pstr_gamma_params_smallpox)
                              
        gamma_params <- c(median = mean(pstr_params$median), 
                          shape = mean(pstr_params$shape),
                          scale = mean(pstr_params$scale))
        
        durs <- seq(.5, 5, by=.1)
        phis <- as.numeric(input$plot1_prob_symptoms)
        
        costs <- calc_monitoring_costs(durs = durs,
                                       probs_of_disease = phis,
                                       per_day_hazard = 1/input$plot1_per_day_hazard_denom,
                                       N = 100,
                                       cost_mat = cost_mat,
                                       gamma_params = gamma_params, 
                                       return_scalar=FALSE)
        
        costs$phi_lab <- factor(costs$phi, 
                                levels=MASS::fractions(unique(costs$phi), max.denominator = 1000000))
        
        ## minimum costs 
        min_costs <- costs %>%
            group_by(phi) %>%
            summarize(min_cost = min(maxcost),
                      min_cost_dur = dur[which.min(maxcost)],
                      min_cost_dur_days = min_cost_dur * gamma_params['median']) %>%
            ungroup() %>%
            mutate(phi_lab = factor(phi,
                                    levels=MASS::fractions(phi, max.denominator = 1000000)))

        ggplot(costs, aes(x=dur*gamma_params['median'], 
                          color=phi_lab, fill=phi_lab)) + 
            geom_ribbon(aes(ymin=mincost, ymax=maxcost), alpha=.7) + 
            scale_y_log10(labels=dollar,
                               name='Cost range of monitoring 100 individuals') +
            scale_x_continuous(name='Duration (in days)', expand=c(0,0)) +
            coord_cartesian(xlim=c(5, 43)) +
            scale_fill_brewer(palette="Dark2") +
            scale_color_brewer(palette="Dark2") +
            #scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
            #scale_color_manual(values=c("#e41a1c", "#377eb8")) +
            geom_segment(data=min_costs,
                         aes(x=3, xend=min_cost_dur_days, 
                             y=min_cost, yend=min_cost, color=phi_lab), 
                         linetype=2) +
            geom_segment(data=min_costs,
                         aes(x=min_cost_dur_days, xend=min_cost_dur_days, 
                             y=0, yend=min_cost, color=phi_lab), 
                         linetype=2) +
            # geom_segment(aes(x=3, xend=min_costs[1, "min_cost_dur_days"], 
            #                  y=min_costs[1, "min_cost"], yend=min_costs[1, "min_cost"]), 
            #              linetype=2, color="#377eb8") +
            # geom_segment(aes(x=min_costs[1, "min_cost_dur_days"], xend=min_costs[1, "min_cost_dur_days"], 
            #                  y=0, yend=min_costs[1, "min_cost"]), 
            #              linetype=2, color="#377eb8") +
            # geom_segment(aes(x=3, xend=min_costs[2, "min_cost_dur_days"], 
            #                  y=min_costs[2, "min_cost"], yend=min_costs[2, "min_cost"]), 
            #              linetype=2, color="#e41a1c") +
            # geom_segment(aes(x=min_costs[2, "min_cost_dur_days"], xend=min_costs[2, "min_cost_dur_days"], 
            #                  y=0, yend=min_costs[2, "min_cost"]), 
            #              linetype=2, color="#e41a1c") +
            theme(legend.title=element_blank(), legend.position=c(1, 1), legend.justification=c(1, 1)) +
          ggtitle("Model-based cost range for monitoring 100 individuals")
        
    })
    
    ## create the plot2 of incubation period data
    output$plot_inc_per <-renderPlot({
        data(kde_smallpox) 
        data(kde_ebola)
        data(kde_mers)
        colors <- c("#1b9e77", "#d95f02", "#7570b3")
        lighter_colors <- c("#8ecfbc", "#fdb174", "#b8b6d6")
        plot_modified_credible_regions(list(pstr_gamma_params_ebola,
                                            pstr_gamma_params_mers,
                                            pstr_gamma_params_smallpox), 
                                       kdes=list(kde_ebola, 
                                                 kde_mers,
                                                 kde_smallpox),
                                       label_txt=c("Ebola", "MERS-CoV", "Smallpox"),
                                       colors=colors, show.legend=TRUE, base.size=18)
    })
        
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

