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
  titlePanel("App for ebola_gamma_pstr monitoring time"),
  ##fluidRow(id = "title-row",
           ##column(12,
                 ## h1("Demo of",
                     ##em(a("App for ebola_gamma_pstr monitoring time", href = "https://github.com/reichlab/activemonitr"))
                  ##),
                  ##h4(em("ebola_gamma_pstr monitoring time"), "lets you estimate the monitoring time needed for three infection diseases"),
                  ##div("Created by", a("Xuelian Li and Nicholas G Reich ", href = "https://github.com/nickreich"),
                     ## HTML("&bull;"),
                     ## "Code", a("on GitHub", href = "https://github.com/reichlab/activemonitr/tree/master/inst/shiny/active-monitoring")
                 ## )
          ## )
 ## ),
  
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
                                c("Ebola" = "Ebola", "Mers" = "Mers",
                                  "Pstr" = "Pstr","Gamma" ="Gamma"),
                                selected="Ebola"),
                   selectInput("plot1_prob", "Select Probability",
                               c("Ebola" = "Ebola", "Mers" = "Mers",
                                 "Pstr" = "Pstr","Gamma" ="Gamma"),
                               ## Multiple allows for multi-selection
                               multiple=TRUE)
                   
                 )
                 ),
 bootstrapPage(
   mainPanel(
         ## create tabs
     tabsetPanel(
       tabPanel("plot1",
                ## strong=bold, p=paragraph, em=emboss/italicised or bold italicized,
                ##plot1_main_text, 
                value="plot1"),
       
       ## summary tab
       tabPanel("plot2",
                ##dataTableOutput("summary"), value="summary",
                tags$style(type="text/css", '#summary tfoot {display:none;}'),
                value="plot2"),
       
       ## plot tab with google chart options
       
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

