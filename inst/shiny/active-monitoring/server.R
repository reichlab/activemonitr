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
    data_df <- ebola_gamma_pstr 
    }
    else if (input$plot1_radio=="Mers") {
    data_df<-mers_gamma_posterior  
    }
    else {
    data_df<-smallpox_gamma_posterior  
    }
    
    ## Output reactive dataframe
    data_df
  })
  
  ## create the plot1 of the ebola data
    output$plotEbo<-renderPlot({
    plot_df<-data_df()
    phi1 <- as.numeric(input$plot1_prob)
    durations1<-input$plot1_mdtime
    a<-durations1[1]
    b<-durations1[2]
    plot_risk_uncertainty(plot_df, phi=phi1, durations=a:b)
  })
})
