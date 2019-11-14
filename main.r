if (!require(ggplot2)) install.packages('ggplot2')
if (!require(dplyr)) install.packages('dplyr')
if (!require(gridExtra)) install.packages('gridExtra')
library(ggplot2)
library(dplyr)
library(gridExtra)

##### Instructions #####

# Set working directory to source file location
# Session -> Set working directory -> To source file location

#### Info ##############
# Syllabys info
# Our basic asset classes are stocks, corporate bonds, government
# bonds, and real estate.


# set dates
first.date <- '1980-01-01'
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


# S&P500 ticker https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC&.tsrc=fin-srch
# Government Bond tickers https://finance.yahoo.com/bonds
# Corporate bond index https://finance.yahoo.com/quote/%5ESPBDACPT/
# Dow Jones U.S. Real Estate Index

# Read in csv files
stock <- read.table("data/^GSPC.csv", 
                 header = TRUE,
                 sep = ",")

gov_bond <- read.table("data/^IRX.csv", 
                    header = TRUE,
                    sep = ",")

#corp_bond <- read.table("data/^SP500BDT.csv", 
#                    header = TRUE,
#                    sep = ",")

#real_estate <- read.table("data/^DJUSRE.csv", 
#                       header = TRUE,
#                       sep = ",")


# Calculate new columns
stock <- calculate_adjusted_earnings(stock)
gov_bond <- calculate_adjusted_earnings(gov_bond)
#corp_bond <- calculate_adjusted_earnings(stock)
#real_estate <- calculate_adjusted_earnings(gov_bond)

# Filter the range we are interested in
stock <- filter_by_date_range(stock)
gov_bond <- filter_by_date_range(gov_bond)
#corp_bond <- filter_by_date_range(stock)
#real_estate <- filter_by_date_range(gov_bond)

# Basic stat measures 
stock.sd <- sd(stock$Adj.Earnings)
stock.mean <- mean(stock$Adj.Earnings)
stock.count <- nrow(stock)

gov_bond.sd <- sd(gov_bond$Adj.Earnings)
gov_bond.mean <- mean(gov_bond$Adj.Earnings)
gov_bond.count <- nrow(gov_bond)

#corp_bond.sd <- sd(gov_bond$Adj.Earnings)
#corp_bond.mean <- mean(gov_bond$Adj.Earnings)
#corp_bond.count <- nrow(gov_bond)

#real_estate.sd <- sd(gov_bond$Adj.Earnings)
#real_estate.mean <- mean(gov_bond$Adj.Earnings)
#real_estate.count <- nrow(gov_bond)

# Histograms
binwidth = 0.01

stock_hist <- ggplot(stock, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = stock.mean, sd = stock.sd) * binwidth * stock.count)

gov_bond_hist <- ggplot(gov_bond, aes(x=Adj.Earnings)) +
  xlim(-4, 4) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = gov_bond.mean, sd = gov_bond.sd) * binwidth * gov_bond.count)

corp_bond_hist <- ggplot(corp_bond, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = corp_bond.mean, sd = corp_bond.sd) * binwidth * corp_bond.count)

real_estate_hist <- ggplot(real_estate, aes(x=Adj.Earnings)) +
  xlim(-0.15, 0.15) +
  geom_histogram(binwidth = binwidth) +
  stat_function(fun = function(x) 
    dnorm(x, mean = real_estate.mean, sd = real_estate.sd) * binwidth * real_estate.count)

#grid.arrange(stock_hist, gov_bond_hist, nrow = 1)
grid.arrange(stock_hist, gov_bond_hist, nrow = 1)
