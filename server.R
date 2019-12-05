#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(readxl)) install.packages('readxl')
if (!require(remotes)) install.packages('remotes')
if (!require(shiny)) remotes::install_version("shiny", "1.3.2", upgrade=FALSE)
if (!require(hms)) remotes::install_version("hms", "0.4.2", upgrade=FALSE)
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

#### Info ##############
# Syllabys info
# Our basic asset classes are stocks, corporate bonds, government
# bonds, and real estate.


# set dates
first.date <- '1995-07-01'
last.date <- '2019-10-01'

#### Help functions ####

filter_by_date_range <- function(df) {
  # Make sure date is date
  df$Date = as.Date(df$Date, format = '%Y-%m-%d')
  # Filter based on date column
  df = df[df$Date >= first.date,]
  df = df[df$Date <= last.date,]
  return(df)
}

calculate_adjusted_earnings <- function(df) {
  # Uses dplyr package to mutate the dataframe
  df = df %>%
    arrange(Date) %>%
    mutate( Adj.Earnings = Adj.Close / lag(Adj.Close) - 1  )
  return(df)
}

calculate_log_returns <- function(df) {
  # Uses dplyr package to mutate the dataframe
  df = df %>%
    arrange(Date) %>%
    mutate( Log_returns = log(Adj.Close) - log(lag(Adj.Close))  )
  return(df)
}

sim_returns_norm_dist <- function(mu,cov_matrix,n) {
  # Cholesky Decomposition
  A <- t(chol(cov_matrix))
  
  k <- length(cov_matrix[1,])
  X <- matrix(NA, nrow = n, ncol = k)
  mu <- rbind(mu)
  
  for (i in 1:n) {
    Z <- rnorm(k)
    X[i,1:k] <- t(A %*% Z + mu)
  }
  
  return(X)
}

create_price_paths <- function(returns) {
  
  n <- length(returns)
  price <- 100                              #Initial asset price 
  price_path <- numeric(n + 1)
  price_path[1] <- price
  
  for (i in 1:n) {
    price <- price * (1 + returns[i])
    price_path[i+1] <- price
  }
  
  return(price_path)
}

# S&P500 ticker ^GSPC
# Government Bond tickers ^LUATTRUU
# Corporate bond index ^SPBDT
# MSCI US REIT Index ^RMZ

# Read in csv files
stock <- read.table("data/^GSPC.csv", 
                    header = TRUE,
                    sep = ",")

gov_bond <- read.table("data/^LUATTRUU.csv", 
                       header = TRUE,
                       sep = ",")

corp_bond <- read.table("data/^SPBDT.csv", 
                        header = TRUE,
                        sep = ",")

real_estate <- read.table("data/^RMZ.csv", 
                          header = TRUE,
                          sep = ",")


# Calculate new columns
stock <- calculate_adjusted_earnings(stock)
stock <- calculate_log_returns(stock)

gov_bond <- calculate_adjusted_earnings(gov_bond)
gov_bond <- calculate_log_returns(gov_bond)

corp_bond <- calculate_adjusted_earnings(corp_bond)
corp_bond <- calculate_log_returns(corp_bond)

real_estate <- calculate_adjusted_earnings(real_estate)
real_estate <- calculate_log_returns(real_estate)


# Filter the range we are interested in
stock <- filter_by_date_range(stock)
gov_bond <- filter_by_date_range(gov_bond)
corp_bond <- filter_by_date_range(corp_bond)
real_estate <- filter_by_date_range(real_estate)


# Returns
stock.sd <- sd(stock$Adj.Earnings)
stock.mean <- mean(stock$Adj.Earnings)
stock.log.sd <-sd(stock$Log_returns)
stock.log.mean <- mean(stock$Log_returns)
stock.count <- nrow(stock)

gov_bond.sd <- sd(gov_bond$Adj.Earnings)
gov_bond.mean <- mean(gov_bond$Adj.Earnings)
gov_bond.log.sd <-sd(gov_bond$Log_returns)
gov_bond.log.mean <- mean(gov_bond$Log_returns)
gov_bond.count <- nrow(gov_bond)

corp_bond.sd <- sd(corp_bond$Adj.Earnings)
corp_bond.mean <- mean(corp_bond$Adj.Earnings)
corp_bond.log.sd <-sd(corp_bond$Log_returns)
corp_bond.log.mean <- mean(corp_bond$Log_returns)
corp_bond.count <- nrow(corp_bond)

real_estate.sd <- sd(real_estate$Adj.Earnings)
real_estate.mean <- mean(real_estate$Adj.Earnings)
real_estate.log.sd <-sd(real_estate$Log_returns)
real_estate.log.mean <- mean(real_estate$Log_returns)
real_estate.count <- nrow(real_estate)


# Histograms

binwidth = 0.01

stock_hist <- ggplot(stock, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  ylim(0, 110) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = stock.mean, sd = stock.sd) * binwidth * stock.count)



gov_bond_hist <- ggplot(gov_bond, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  ylim(0, 110) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = gov_bond.mean, sd = gov_bond.sd) * binwidth * gov_bond.count)

corp_bond_hist <- ggplot(corp_bond, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  ylim(0, 110) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = corp_bond.mean, sd = corp_bond.sd) * binwidth * corp_bond.count)

real_estate_hist <- ggplot(real_estate, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  ylim(0, 110) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = real_estate.mean, sd = real_estate.sd) * binwidth * real_estate.count)


#grid.arrange(stock_hist, nrow = 1)
grid.arrange(stock_hist, gov_bond_hist, corp_bond_hist,real_estate_hist, nrow = 1)

# Means ans standar deviations
result_matrix <- matrix(NA,4,2)
colnames(result_matrix) <- c("Mean","Sd")
rownames(result_matrix) <- c("Stocks","Gov.Bonds", "Corp.Bonds","Real Estate")
result_matrix[1,1] <- mean(stock$Adj.Earnings)
result_matrix[1,2] <- sd(stock$Adj.Earnings)
result_matrix[2,1] <- mean(gov_bond$Adj.Earnings)
result_matrix[2,2] <- sd(gov_bond$Adj.Earnings)
result_matrix[3,1] <- mean(corp_bond$Adj.Earnings)
result_matrix[3,2] <- sd(corp_bond$Adj.Earnings)
result_matrix[4,1] <- mean(real_estate$Adj.Earnings)
result_matrix[4,2] <- sd(real_estate$Adj.Earnings)









# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  validate_weights <- reactive({
    weight_sum = input$stock_weight + input$real_estate_weight + input$corp_bond_weight + input$gov_bond_weight + input$cash_weight
    validate(
      need(weight_sum == 100, label = "Sum of weights should be 100")
    )
  })
  
  percentile_paths <- reactive ({
    pppaths <- simulate_paths()
    simulation.n <- input$simulation_n
    simulation.length_months <- input$simulation_length_months
    simulation.length_month_plus_1 <- simulation.length_months + 1
    
    # Mean line
    m_line <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      m_line[i] <- mean(pppaths[i,])
    }
    

    # The 90% percentile
    per90 <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      per90[i] <- quantile(pppaths[i,],0.9)
    }
    # Legend

    # The 75% percentile
    per75 <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      per75[i] <- quantile(pppaths[i,],0.75)
    }
    # Legend

    
    # The 25% percentile
    per25 <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      per25[i] <- quantile(pppaths[i,],0.25)
    }
    # Legend

    # The 10% percentile
    per10 <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      per10[i] <- quantile(pppaths[i,],0.1)
    }

    return_list <- list(m_line=m_line, per90=per90, per75=per75, per25=per25, per10=per10)
    
  })
  
  simulate_paths <- reactive({ #### Inputs ############
    
    # Expected returns of different asset classes
    Mu <- rbind(log((1 + input$stock_mean/100)), 
                log((1 + input$real_estate_mean/100)), 
                log((1 + input$corp_bond_mean/100)), 
                log((1 + input$gov_bond_mean/100))) # given by user
    
    cash.mean <- input$cash_mean

    # Volatility estimates of different assets classes
    sd_vector <- c(input$stock_sd/100, 
                   input$real_estate_sd/100, 
                   input$corp_bond_sd/100, 
                   input$gov_bond_sd/100)# given by user

    # Correlations between different assets classes
    inFile <- input$file1
    
    correl_matrix <- cor(cbind(stock$Adj.Earnings,real_estate$Adj.Earnings,corp_bond$Adj.Earnings,gov_bond$Adj.Earnings)) # given by user
    
    if (!is.null(inFile)) {
      
      excel_correlation_data <- my_data <- read_excel(inFile$datapath)
      excel_correlation_data <- excel_correlation_data[,-1]
      correl_matrix[2,1] <- excel_correlation_data[[2,1]]
      correl_matrix[3,1] <- excel_correlation_data[[3,1]]
      correl_matrix[4,1] <- excel_correlation_data[[4,1]]
      correl_matrix[3,2] <- excel_correlation_data[[3,2]]
      correl_matrix[4,2] <- excel_correlation_data[[4,2]]
      correl_matrix[4,3] <- excel_correlation_data[[4,3]]
      
      correl_matrix[1, 2] <- excel_correlation_data[[2,1]]
      correl_matrix[1, 3] <- excel_correlation_data[[3,1]]
      correl_matrix[1, 4] <- excel_correlation_data[[4,1]]
      correl_matrix[2, 3] <- excel_correlation_data[[3,2]]
      correl_matrix[2, 4] <- excel_correlation_data[[4,2]]
      correl_matrix[3, 4] <- excel_correlation_data[[4,3]]
    }
    

    # Portfolio weights
    validate_weights()
    stock.weight <- input$stock_weight / 100
    real_estate.weight <- input$real_estate_weight / 100
    corp_bond.weight <- input$corp_bond_weight / 100
    gov_bond.weight <- input$gov_bond_weight / 100
    cash.weight <- input$cash_weight / 100
    
    weight_vector <- rbind(stock.weight,real_estate.weight,corp_bond.weight,gov_bond.weight, cash.weight)
    
    simulation.n <- input$simulation_n
    simulation.length_months <- input$simulation_length_months
    simulation.length_month_plus_1 <- simulation.length_months + 1
    
    #### Simulation ########
    
    # Simulating values from Multivariate Normal distribution
    # Disclaimer: it's known fact that stock market returns are not normally distributed...
    
    # covariance matrix
    cov_matrix <- diag(sd_vector)%*%correl_matrix%*%diag(sd_vector)

    # Simulating 100 different price paths for 12 month time interval for portfolio
    
    pppaths <- matrix(data = NA, nrow = simulation.length_month_plus_1, ncol = simulation.n) # portfolio price paths matrix
    colnames(pppaths) <- c(1:simulation.n)
    rownames(pppaths) <- c(0:simulation.length_months )
    
    cash_returns <- rep(cash.mean/100, simulation.length_months)
    
    for (i in 1:simulation.n) {
      sample <- sim_returns_norm_dist(Mu,cov_matrix,simulation.length_months )   # Simulating a sample of 12 montly returns (1 year) for all asset classes
      sample <- (exp(sample)-1)  
      sample <- cbind(sample, cash_returns)
      pf_returns <- sample%*%weight_vector                # Constructing monthly portfolio returns
      pppaths[,i]<- create_price_paths(pf_returns)        # creating 1 year cumulative price paths
    }
    return(pppaths)
    
  })
   
  output$pathPlot <- renderPlot({
    
    pppaths <- simulate_paths()
    simulation.n <- input$simulation_n
    simulation.length_months <- input$simulation_length_months
    simulation.length_month_plus_1 <- simulation.length_months + 1
    
    ### Price Paths ########
    
    # Plotting all the paths
    g_range <- range(min(pppaths[,])-5,max(pppaths[,])+5)
    plot(pppaths[,1], type = "l", xlim = c(0,simulation.length_months ), ylim=g_range, ann=FALSE, xaxt="n")
    axis(1, at=1:simulation.length_month_plus_1, lab=c(0:simulation.length_months ))
    box()
    
    for(i in 2:simulation.n) {
      lines(pppaths[,i],type = "l")
    }
    
    # Mean line
    m_line <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      m_line[i] <- mean(pppaths[i,])
    }
    
    lines(m_line,type = "l", col = "blue",lwd = 4, lty = 3)
    
    # The 95% Value at Risk (VaR95%)
    VaR95 <- numeric(simulation.length_month_plus_1)
    for (i in 1:simulation.length_month_plus_1) {
      VaR95[i] <- quantile(pppaths[i,],.05)
    }
    # Legend
    lines(VaR95,type = "l", col = "red",lwd = 3, lty = 6)
    legend("topleft",inset=.05,c("Average", "VaR95%"),fill=c("blue","red"), horiz=TRUE)
    
  })
  
  
  
  output$percentilePathPlot <- renderPlot({
    
    pppaths <- simulate_paths()
    percpaths <- percentile_paths()
    simulation.n <- input$simulation_n
    simulation.length_months <- input$simulation_length_months
    simulation.length_month_plus_1 <- simulation.length_months + 1
    
    ### Price Paths ########
    
    # Plotting all the paths
    g_range <- range(min(pppaths[,])-5,max(pppaths[,])+5)
    plot(1, type = "l", xlim = c(0,simulation.length_months ), ylim=g_range, ann=FALSE, xaxt="n")
    axis(1, at=1:simulation.length_month_plus_1, lab=c(0:simulation.length_months ))
    box()
    
    lines(percpaths$m_line,type = "l", col = "blue",lwd = 4, lty = 3)
    lines(percpaths$per90,type = "l", col = "green")
    lines(percpaths$per75,type = "l", col = "blue")
    lines(percpaths$per25,type = "l", col = "purple")
    lines(percpaths$per10,type = "l", col = "red")
    
    legend("topleft",inset=.05,c("Average", "90th percentile", "75th percentile", "25th percentile", "10th percentile"),
           col=c("blue","green", "blue", "purple", "red"), lty=c(3, 1, 1, 1, 1))
  })
  
  output$percentileTable <- renderPrint({
    percpaths <- percentile_paths()
    simulation.n <- input$simulation_n
    simulation.length_months <- input$simulation_length_months
    simulation.length_month_plus_1 <- simulation.length_months + 1
    
    result_matrix <- matrix(NA,5,2)
    colnames(result_matrix) <- c("Value at end","Return %")
    rownames(result_matrix) <- c("90th percentile","75th percentile", "50th percentile","25th percentile", "10th percentile")
    result_matrix[1,1] <- tail(percpaths$per90, n=1)
    result_matrix[2,1] <- tail(percpaths$per75, n=1)
    result_matrix[3,1] <- tail(percpaths$m_line, n=1)
    result_matrix[4,1] <- tail(percpaths$per25, n=1)
    result_matrix[5,1] <- tail(percpaths$per10, n=1)
    result_matrix[1,2] <- (tail(percpaths$per90, n=1) / 100 - 1 ) * 100
    result_matrix[2,2] <- (tail(percpaths$per75, n=1) / 100 - 1 ) * 100
    result_matrix[3,2] <- (tail(percpaths$m_line, n=1) / 100 - 1 ) * 100
    result_matrix[4,2] <- (tail(percpaths$per25, n=1) / 100 - 1 ) * 100
    result_matrix[5,2] <- (tail(percpaths$per10, n=1) / 100 - 1 ) * 100

    result_matrix
  })
  
})
