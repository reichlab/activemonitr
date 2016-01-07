#######################################
## Title: active-monitoring ui.R     ##
## Author(s): Xuelian Li,            ##
##            Nicholas G Reich       ## 
## Date Created:  12/27/2016         ##
## Date Modified: 01/04/2016 XL      ##
#######################################
shinyUI(fluidPage(
  title = "Demo of App for ebola_gamma_pstr monitoring time",
  tags$head(includeCSS(file.path('www', 'style.css'))),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Slabo+27px")
  ),
  titlePanel("App for ebola_gamma_pstr monitoring time"),
  
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 plot1_side_text,
                 plot2_side_text,
                 info_side_text,
                 
                 ##in plot1, allow for Type of Infection Diseases, probability selection selection
                 conditionalPanel(
                   condition="input.tabs == 'plot1'",
                   radioButtons("plot1_radio", "Type of Infection Diseases",
                                c("Ebola" = "Ebola", "MERS-CoV" = "Mers",
                                  "Smallpox" = "Smallpox","Custom(gama distribution)" ="Gamma"),
                                selected="Ebola"),
                   conditionalPanel(
                     condition="input.plot1_radio=='Gamma'",
                     sliderInput("plot1_shape", "Shape",
                                 min=0, max=30, value=c(5),
                                 sep=""),
                     sliderInput("plot1_scale", "Scale",
                                 min=0, max=30, value=c(5),
                                 sep="")
                   ),
                   sliderInput("plot1_mdtime", "Monitoring Duration",
                               min=0, max=30, value=c(5, 21),
                               sep=""),
                   checkboxGroupInput("plot1_prob", "Probability of Symptoms",
                               c("1/10" = "1/10", "1/100" = "1/100",
                                 "1/1000" = "1/1000","1/10000" ="1/10000"),
                               selected="1/10")
                   
                 )
                 ),
 bootstrapPage(
   mainPanel(
         ## create tabs
     tabsetPanel(
       tabPanel("plot1",
                ##plot1_main_text, 
                value="plot1"),
       
       ## graph2
       tabPanel("plot2",
                value="plot2"),
       
       
       tabPanel("More Infor"
                
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

