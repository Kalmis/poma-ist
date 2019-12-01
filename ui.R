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
      h4("Simulation settings"),
      sliderInput("simulation_n",
                   "Number of simulations",
                   min = 10,
                   max = 10000,
                   value = 100),
      sliderInput("simulation_length_months",
                  "Number of simulated moths",
                  min = 6,
                  max = 120,
                  value = 12),
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
      h4("Volatilities"),
      numericInput("stock_sd",
                   "Stocks",
                   min = 0,
                   max = 1,
                   value = 0.04237404),
      numericInput("gov_bond_sd",
                   "Goverment bonds",
                   min = 0,
                   max = 1,
                   value = 0.01237999),
      numericInput("corp_bond_sd",
                   "Corporate bonds",
                   min = 0,
                   max = 1,
                   value = 0.01392551),
      numericInput("real_estate_sd",
                   "Real estates",
                   min = 0,
                   max = 1,
                   value = 0.05667676),
      h4("Expected return %"),
      numericInput("stock_mean",
                   "Stocks",
                   min = 0,
                   max = 1,
                   value = 0.6811162),
      numericInput("gov_bond_mean",
                   "Goverment bonds",
                   min = 0,
                   max = 1,
                   value = 0.4094851),
      numericInput("corp_bond_mean",
                   "Corporate bonds",
                   min = 0,
                   max = 1,
                   value = 0.5102123),
      numericInput("real_estate_mean",
                   "Real estates",
                   min = 0,
                   max = 1,
                   value = 0.8012561)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("pathPlot"),
       plotOutput("percentilePathPlot")
    )
  )
))
