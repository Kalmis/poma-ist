#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
if (!require(remotes)) install.packages('remotes')
if (!require(shiny)) remotes::install_version("shiny", "1.3.2", upgrade=FALSE)
if (!require(hms)) remotes::install_version("hms", "0.4.2", upgrade=FALSE)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Easy Money Simulation Engine"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel( 
        tabPanel("Simulation settings",
          h4("Simulation settings"),
          sliderInput("simulation_n",
                       "Number of simulations",
                       min = 10,
                       max = 10000,
                       value = 100),
          sliderInput("simulation_length_months",
                      "Number of simulated months",
                      min = 6,
                      max = 360,
                      value = 180),
          h4("Asset class weight (%) in portfolio"),
          numericInput("stock_weight",
                     "Stock:",
                     min = 1,
                     max = 100,
                     value = 25),
          numericInput("gov_bond_weight",
                     "Goverment bonds",
                     min = 1,
                     max = 100,
                     value = 25),
          numericInput("corp_bond_weight",
                     "Corporate bonds",
                     min = 1,
                     max = 100,
                     value = 25),
          numericInput("real_estate_weight",
                     "Real estates",
                     min = 1,
                     max = 100,
                     value = 25),
          numericInput("cash_weight",
                       "Cash leverage",
                       min = 1,
                       max = 100,
                       value = 0)
        ),
        tabPanel("Asset class settings",
          h4("Monthly volatilities"),
          numericInput("stock_sd",
                       "Stocks",
                       min = 0,
                       max = 100,
                       value = 4.237404),
          numericInput("gov_bond_sd",
                       "Goverment bonds",
                       min = 0,
                       max = 100,
                       value = 1.237999),
          numericInput("corp_bond_sd",
                       "Corporate bonds",
                       min = 0,
                       max = 100,
                       value = 1.392551),
          numericInput("real_estate_sd",
                       "Real estates",
                       min = 0,
                       max = 100,
                       value = 5.667676),
          h4("Monthly expected return %"),
          numericInput("stock_mean",
                       "Stocks",
                       min = 0,
                       max = 100,
                       value = 0.6811162),
          numericInput("gov_bond_mean",
                       "Goverment bonds",
                       min = 0,
                       max = 100,
                       value = 0.4094851),
          numericInput("corp_bond_mean",
                       "Corporate bonds",
                       min = 0,
                       max = 100,
                       value = 0.5102123),
          numericInput("real_estate_mean",
                       "Real estates",
                       min = 0,
                       max = 100,
                       value = 0.8012561),
          numericInput("cash_mean",
                       "Cash",
                       min = 0,
                       max = 100,
                       value = 0.0829538),
          fileInput("file1", "Choose correlation Excel File",
                    accept = c(
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                      ".xlsx")
          )
        ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2("All simulated price paths"),
       plotOutput("pathPlot"),
       h2("Percentile paths"),
       plotOutput("percentilePathPlot"),
       h2("Summary table"),
       verbatimTextOutput("percentileTable")
    )
  )
))
