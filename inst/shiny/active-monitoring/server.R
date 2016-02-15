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
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Creating plot", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a closure to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute the new data, and pass in the updateProgress function so
    # that it can update the progress indicator.
    compute_data(updateProgress)
    plot_risk_uncertainty(plot_df, phi=phi1, durations=a:b)
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

