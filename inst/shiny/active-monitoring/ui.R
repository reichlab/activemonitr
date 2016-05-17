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
  
  fluidPage(
    bootstrapPage(
      ## create tabs
      tabsetPanel(
        tabPanel("Model Overview",
                 value="overview"),
        tabPanel("Costs of Active Monitoring Programs",
                 plotOutput("plot_costs"), 
                 value="cost.plot"),
        tabPanel("Incubation Period Estimates",
                 plotOutput("plot_inc_per"),
                 value="incper.plot"),
        id="tabs", position="left")
    ),

    hr(), 
    
    fluidRow(
      
      ## text for overview panel
      conditionalPanel(
        condition="input.tabs == 'overview'",
        p("Active monitoring has shown promise as a tool in preventing and responding to outbreaks of pathogens that pose a grave threat to public health, such as", 
          a("the 2014 west Africa Ebola outbreak", href="http://www.cdc.gov/vhf/ebola/exposure/monitoring-and-movement-of-persons-with-exposure.html"),
          ". It could play an important role in containing outbreaks of rapidly spreading emerging pathogens, occurring either naturally or via a bioterrorist attack. 
          Individuals under active monitoring are typically asked to contact local health authorities every day for some number of days after their last potential exposure to report their health status."), 
        p("We developed a model that estimates the risks and costs associated with active monitoring programs. Our model provides a range of expected costs for an active monitoring program, 
          based on a series of assumptions about the disease of interest and the costs of the program.  
          The model is discussed in detail in a manuscript that is currently under review. The figure below provides an overview of the model schematic."),
        p("On the other tabs of this application, users may specify different model parameters to see how they impact the cost of active monitoring programs."),
        img(src='figure1-model-schema-v7.jpg', height='400px'),
        p("This app was developed by", a("Nicholas G Reich", href='https://reichlab.github.io/'),  "and Xuelian Li at UMass-Amherst Biostatistics.")
      ),
      
      ## user inputs for cost plot panel
      conditionalPanel(
        condition="input.tabs == 'cost.plot'",
        column(3,
               radioButtons("plot1_disease", "Pathogen",
                            c("Ebola" = "Ebola", "MERS-CoV" = "Mers",
                              "Smallpox" = "Smallpox"
                            ),
                            selected="Ebola")
        ),
        column(4, 
               checkboxGroupInput("plot1_prob_symptoms", "Probability a monitored individual develops symptoms",
                                  c("1/10" = "0.1", "1/100" = "0.01",
                                    "1/1,000" = "0.001","1/10,000" ="0.0001"),
                                  selected=c("0.001", "0.0001"))
        ),
        column(4, 
               h4("Cost inputs"),
               sliderInput("plot1_cost_per_day", "Cost per monitored person-day ($)",
                           min=0, max=100, value=c(10,20), sep=""),
               sliderInput("plot1_cost_false_pos", "Cost of a false positive ($000s)",
                           min=0, max=100, value=c(10,30), sep=""),
               sliderInput("plot1_per_day_hazard_denom", "1/(per day rate of false positive)",
                           min=10, max=100000, value=10000)
        )
      )
    )
  )
)
)

