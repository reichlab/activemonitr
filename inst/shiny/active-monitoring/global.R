#######################################
## Title: active-monitoring global.R ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ##
## Date Created:  01/04/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
require(shiny)
require(dplyr)
require(ggplot2)
require(scales)
require(activeMonitr)

source("plot_risk.R")
theme_set(theme_bw(base_size = 18))

data("pstr_gamma_params_ebola")
data("pstr_gamma_params_mers")
data("pstr_gamma_params_smallpox")
data("boot_lnorm_params_ncov")


plot1_side_text <- conditionalPanel(
  condition="input.tabs == 'cost.plot'",
  p("Our model provides a range of expected costs for an active monitoring program, based on a series of assumptions.")
  #p("During potential pandemic scenarios, the US CDC has implemented active monitoring programs to track international travelers to minimize risk of disease transmission. You can use this web applet to explore the risks associated with different durations of active monitoring periods for Ebola, MERS-CoV, and Smallpox.")#,
#  tags$br(),
#   tags$ul(
#     tags$li('Please select the type of disease for which you are interested in viewing.'),
#     tags$br(),
#     tags$li('Please select the probability that you infected by this disease for which you are interested in viewing.'),
#     tags$br(),
#     tags$li('For a given monitoring time period, you can compare the estimated risk of you geting this disease based on your choice.')
#   )
)

tags$hr()


plot2_side_text <- conditionalPanel(
  condition="input.tabs == 'incper.plot'",
  h4("Overview"),
  p("The distribution of possible incubation periods is a key factor in determining the risk of developing symptoms after the active monitoring period ends. We have conducted detailed reanalysis of primary data on incubation periods of Ebola, MERS-CoV and Smallpox. The distributions of the incubation period best supported by the data are summarized in the figure to the right."),
  h4("Left panel"),
  p("We show 90% credible regions for the median and 95th percentile of the incubation period distribution for each disesae."),
  h4("Right panels"),
  p("We show the estimated distribution, with a random sample of other credible distributions shown in transparent lines."))

tags$hr()

info_side_text <- conditionalPanel(
  condition="input.tabs == 'overview'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.')))
)
