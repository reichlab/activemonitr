#######################################
## Title: active-monitoring ui.R     ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ## 
## Date Created:  12/27/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
shinyUI(fluidPage(
  title = "Duration of Active Monitoring",
  tags$head(includeCSS(file.path('www', 'style.css'))),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Slabo+27px")
  ),
  titlePanel("Determining Durations for Active Monitoring"),
  
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 plot1_side_text,
                 plot2_side_text,
                 info_side_text,
                 
                 ##in plot1, allow for Type of Infection Diseases, probability selection selection
                 conditionalPanel(
                   condition="input.tabs == 'plot1'",
                   sliderInput("plot1_cost_per_day", "Cost per person-day ($)",
                               min=0, max=100, value=c(10,20), sep=""),
                   sliderInput("plot1_cost_false_pos", "Cost of false positive ($000s)",
                               min=0, max=100, value=c(10,30), sep=""),
                   sliderInput("plot1_per_day_hazard_denom", "Per day false positive prob denom",
                               min=10, max=100000, value=10000),
                   radioButtons("plot1_disease", "Type of Infection Diseases",
                                c("Ebola" = "Ebola", "MERS-CoV" = "Mers",
                                  "Smallpox" = "Smallpox"
                                  ##"Custom(gama distribution)" ="Gamma"
                                  ),
                                selected="Ebola"),
                   ##conditionalPanel(
                     ##condition="input.plot1_radio=='Gamma'",
                     ##sliderInput("plot1_shape", "Shape",
                                ## min=0, max=30, value=c(5),
                                 ##sep=""),
                     ##sliderInput("plot1_scale", "Scale",
                                ## min=0, max=30, value=c(5),
                                 ##sep="")
                   ##),
                   # sliderInput("plot1_mdtime", "Monitoring Duration (days)",
                   #             min=0, max=30, value=c(5, 25),
                   #             sep=""),
                   checkboxGroupInput("plot1_prob_symptoms", "Probability a monitored individual develops symptoms",
                               c("1/10" = "0.1", "1/100" = "0.01",
                                 "1/1,000" = "0.001","1/10,000" ="0.0001"),
                               selected=c("0.001", "0.0001"))
                   
                 )
                 ),
 bootstrapPage(
   mainPanel(
         ## create tabs
     tabsetPanel(
       tabPanel("Cost Assessment",
                plotOutput("plot_costs"), 
                value="plot1"),
       
       ## graph2
       tabPanel("Incubation Periods",
                plotOutput("plot_inc_per"),
                #img(src ="plot2.png"),
                value="plot2"),
       
       
       tabPanel("More Information"
                
                ## add text about the variables
                #                  infor_main_text,
                
                
                
),
id="tabs"
)
)
)
)
)
)

