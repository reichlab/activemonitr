#######################################
## Title: active-monitoring ui.R     ##
## Author(s): Xuelian Li,            ##
##            Stephen A Lauer        ##
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
          tabPanel("Overview",
                   value="overview"),
          tabPanel("Model",
                   value="model"),
          tabPanel("Incubation Period Estimates",
                   plotOutput("plot_inc_per"),
                   value="incper.plot"),
          tabPanel("Undetected Infections",
                   fluidRow(column(width=6, plotOutput("plot_risk_uncertainty")),
                            column(width=6,
                                   h5("Number of undetected symptomatic infections per 10,000 monitored"),
                                   dataTableOutput("tbl_risk_uncertainty"))),
                            value="undinf.plot"),
          tabPanel("Costs of Active Monitoring Programs",
                   plotOutput("plot_costs"),
                   value="cost.plot"),
          id="tabs")
    ),

    hr(),

    fluidRow(

        ## text for overview panel
        conditionalPanel(
            condition="input.tabs == 'overview'",
            h4("Overview"),
            p("Active monitoring has shown promise as a tool in preventing and responding to outbreaks of pathogens that pose a grave threat to public health, such as",
              a("the 2014 west Africa Ebola outbreak", href="http://www.cdc.gov/vhf/ebola/exposure/monitoring-and-movement-of-persons-with-exposure.html"),
              ". It could play an important role in containing outbreaks of rapidly spreading emerging pathogens, occurring either naturally or via a bioterrorist attack.
          Individuals under active monitoring are typically asked to contact local health authorities every day for some number of days after their last potential exposure to report their health status."),
            p("This web application allows you to directly observe the risks and costs associated with a variety of active-monitoring scenarios. Below, we provide a brief description of each tab."),
            h4("Model"),
            p("This tab provides an overview of the underlying model from",
              a("Quantifying the Risk and Cost of Active Monitoring for Infectious Diseases", href='https://www.nature.com/articles/s41598-018-19406-x'),
              "by Reich, et al. (2018)."),
            h4("Incubation Period Estimates"),
            p("This tab displays a figure with the incubation period estimates for the four disease scenarios (Ebola, MERS, Smallpox, and COVID-19) that are used throughout the app."),
            h4("Undetected Infections"),
            p("In this tab, you can choose an infectious disease scenario and specify the parameters of an active-monitoring program to see the proportion of symptomatic infections that would go undetected."),
            h4("Costs of Active Monitoring Programs"),
            p("In this tab, you can determine the costs associated with active monitoring and missing a case to see how those affect the overall cost of the program."),
            hr(),
            h4("Publications:"),
            p("Reich NG, Lessler J, Varma JK, and Vora NM. Quantifying the risk and cost of active monitoring for infectious diseases. Sci Rep. 2018;8(1):1093. doi:10.1038/s41598-018-19406-x."),
            p("Lauer SA, Grantz KH, Bi Q, Jones FK, Zheng Q, Meredith HR, Azman AS, Reich NG, and Lessler J. The incubation period of coronavirus disease 2019 (COVID-19) from publicly reported confirmed cases: estimation and application. Annals of Internal Medicine 172 (9), 577-582 (2020)."),
            p("Zhao Q, Ju N, Bacallado S, and Shah RD. BETS: The dangers of selection bias in early analyses of the coronavirus disease (COVID-19) pandemic. arXiv (2020)."),
            h4("Development"),
            p("This app was developed by",
              a("Nicholas G Reich", href='https://reichlab.github.io/'),
              "(UMass-Amherst Biostatistics),",
              a("Stephen A Lauer", href='https://salauer.github.io/'),
              "(Johns Hopkins Bloomberg SPH), and Xuelian Li. If you have any issues, please submit them to the associated",
              a("GitHub repository.", href='https://github.com/reichlab/activemonitr')),
        ),

        ## text for model panel
        conditionalPanel(
            condition="input.tabs == 'model'",
            p("We developed a model that estimates the risks and costs associated with active monitoring programs. Our model provides a range of expected costs for an active monitoring program,
          based on a series of assumptions about the disease of interest and the costs of the program.
          The model is discussed in detail in",
              a("Quantifying the Risk and Cost of Active Monitoring for Infectious Diseases", href='https://www.nature.com/articles/s41598-018-19406-x'),
              "by Reich, et al. (2018). The figure below provides an overview of the model schematic."),
            img(src='figure1-model-schema-v7.jpg', height='400px')),

      ## user inputs for cost plot panel
      conditionalPanel(
        condition="input.tabs == 'cost.plot'",
        column(3,
               radioButtons("plot1_disease", "Pathogen",
                            c("COVID-19 (Lauer, Grantz)" = "COVID1",
                              "COVID-19 (Zhao)" = "COVID2",
                              "COVID-19 (McAloon)" = "COVID3",
                              "Ebola" = "Ebola",
                              "MERS-CoV" = "Mers",
                              "Smallpox" = "Smallpox"
                            ),
                            selected="Ebola"),
               checkboxGroupInput("plot1_prob_symptoms",
                                  "Probability a monitored individual develops symptoms",
                                  c("Infected (1/1)" = "1",
                                    "Very high risk (1/10)" = "0.1",
                                    "High risk (1/100)" = "0.01",
                                    "Medium risk (1/1,000)" = "0.001",
                                    "Low risk (1/10,000)" ="0.0001"),
                                  selected=c("0.001", "0.0001"))
        ),
        column(4,
               sliderInput("plot1_secondary_cases", "Number of secondary cases",
                           min=0, max=25, value=4),
               sliderInput("plot1_cost_per_case", "Cost per case ($000,000s)",
                           min=0, max=20, value=c(3,5)),
               sliderInput("plot1_cost_per_day", "Cost per monitored person-day ($)",
                           min=0, max=100, value=c(10,20))
        ),
        column(4,
               sliderInput("plot1_cost_false_pos", "Cost of a false positive ($000s)",
                           min=0, max=100, value=c(10,30)),
               sliderInput("plot1_per_day_hazard_denom", "Expected number of monitored-person-days needed to have 1 hospitalized false positive",
                           min=10, max=10000, value=1000, step = 10)
        )
      ),

      ## text for incubation period panel
      conditionalPanel(
          condition="input.tabs == 'incper.plot'",
          p("The above figure shows the estimated incubation period parameters (points) and confidence regions. These estimates are based on obtained previously published incubation period observations on",
            a("152 cases of Ebola in Guinea,", href="http://www.ncbi.nlm.nih.gov/pubmed/25619149"),
            a("170 laboratory-confirmed cases of MERS-CoV in South Korea,", href="http://datadryad.org/resource/doi:10.5061/dryad.v3546"),
            a("362 cases of smallpox,", href="http://www.ncbi.nlm.nih.gov/pubmed/18178524"), "and two estimates of the COVID-19 incubation from",
            a("181 cases (Lauer, Grantz, et al.)", href="https://www.acpjournals.org/doi/10.7326/M20-0504"), "and",
            a("378 cases (Zhao et al.)", href="https://arxiv.org/pdf/2004.07743.pdf"),
            ". We fitted the observed data for Ebola, MERS-CoV, and smallpox to gamma probability distributions using Markov Chain Monte Carlo (MCMC) methods with the Metropolis-Hastings algorithm. The gamma distribution is one of several 'heavy-tailed' distributions often used to describe incubation periods, and aligns with assumptions made by previous researchers. The COVID-19 data were bootstrapped and fit to log-normal (Lauer, Grantz, et al.) and gamma (Zhao et al.) distributions.")
      ),

      ## user inputs for undetected infections plot panel
      conditionalPanel(
          condition="input.tabs == 'undinf.plot'",
          column(3,
                 radioButtons("plot3_disease", "Pathogen",
                              c("COVID-19 (Lauer, Grantz)" = "COVID1",
                                "COVID-19 (Zhao)" = "COVID2",
                                "COVID-19 (McAloon)" = "COVID3",
                                "Ebola" = "Ebola",
                                "MERS-CoV" = "Mers",
                                "Smallpox" = "Smallpox"),
                              selected="COVID1"),

          ),
          column(4,
                 selectInput("plot3_prob_symptoms",
                             "Probability a monitored individual develops symptoms",
                             c("Infected (1/1)" = "1",
                               "Very high risk (1/10)" = "0.1",
                               "High risk (1/100)" = "0.01",
                               "Medium risk (1/1,000)" = "0.001",
                               "Low risk (1/10,000)" ="0.0001"),
                             selected="0.01"),
                 sliderInput("plot3_u", "Days since infectious exposure",
                             min=0, max=30, value=c(1,5))),
          column(4,
                 sliderInput("plot3_duration", "Length of active-monitoring program",
                             min=0, max=100, value=c(1,28)),
                 sliderInput("plot3_ci", "Confidence interval width",
                             min=0.5, max=1, value=c(0.95), round=-1)

          )
      )
    )
  )
)
)

