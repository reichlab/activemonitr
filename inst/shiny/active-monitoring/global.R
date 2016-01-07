#######################################
## Title: active-monitoring global.R ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ## 
## Date Created:  01/04/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
require(shiny)
require(dplyr)
require(googleCharts)
require(activeMonitr)
require(ggplot2)
require(grid)

load(file="ebola_gamma_pstr.rda")
ebola<-ebola_gamma_pstr
source("plot_risk.R", echo=TRUE)
source("prob_of_missing_case.R", echo=TRUE)
plot1_side_text <- conditionalPanel(
  condition="input.tabs == 'plot1'",
  h4("How to use this app:"),
  tags$br(),
  tags$ul(
    tags$li('Please select the type of disease for which you are interested in viewing.'),
    tags$br(),
    tags$li('Please select the probability that you infected by this disease for which you are interested in viewing.'),
    tags$br(),
    tags$li('For a given monitoring time period, you can compare the estimated risk of you geting this disease based on your choice.')
  ))

tags$hr()


plot2_side_text <- conditionalPanel(
  condition="input.tabs == 'plot2'",
  h4("How to use this app:"),
  tags$br(),
  tags$ul(
    tags$li('Please select the type of disease for which you are interested in viewing.'),
    tags$br(),
    tags$li('Please select the probability that you infected by this disease for which you are interested in viewing.'),
    tags$br(),
    tags$li('For a given monitoring time period, you can compare the estimated risk of you geting this disease based on your choice.')
  ))

tags$hr()

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.')))
)
