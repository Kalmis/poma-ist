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
result_matrix





#### Inputs ############

# Expected returns of different asset classes
Mu <- rbind(stock.mean,real_estate.mean,corp_bond.mean,gov_bond.mean) # given by user

# Volatility estimates of different assets classes
sd_vector <- c(stock.sd,real_estate.sd,corp_bond.sd,gov_bond.sd)# given by user

# Correlations between different assets classes
correl_matrix <- cor(cbind(stock$Adj.Earnings,real_estate$Adj.Earnings,corp_bond$Adj.Earnings,gov_bond$Adj.Earnings)) # given by user

# Portfolio weights
stock.weight <- 0.25
real_estate.weight <- 0.25
corp_bond.weight <-0.25
gov_bond.weight <-0.25

weight_vector <- rbind(stock.weight,real_estate.weight,corp_bond.weight,gov_bond.weight)

#### Simulation ########

# Simulating values from Multivariate Normal distribution
# Disclaimer: it's known fact that stock market returns are not normally distributed...

# covariance matrix
cov_matrix <- diag(sd_vector)%*%correl_matrix%*%diag(sd_vector)

# Simulating 100 different price paths for 12 month time interval for portfolio

pppaths <- matrix(data = NA, nrow = 13, ncol = 100) # portfolio price paths matrix
colnames(pppaths) <- c(1:100)
rownames(pppaths) <- c(0:12)

for (i in 1:100) {
  sample <- sim_returns_norm_dist(Mu,cov_matrix,12)   # Simulating a sample of 12 montly returns (1 year) for all asset classes
  pf_returns <- sample%*%weight_vector                # Constructing monthly portfolio returns
  pppaths[,i]<- create_price_paths(pf_returns)        # creating 1 year cumulative price paths
}


### Price Paths ########

# Plotting all the paths
g_range <- range(min(pppaths[13,]),max(pppaths[13,]))
plot(pppaths[,1], type = "l", xlim = c(0,12), ylim=g_range, ann=FALSE, xaxt="n")
axis(1, at=1:13, lab=c(0:12))
box()

for(i in 2:100) {
  lines(pppaths[,i],type = "l")
}

# Mean line
m_line <- numeric(13)
for (i in 1:13) {
  m_line[i] <- mean(pppaths[i,])
}

lines(m_line,type = "l", col = "blue",lwd = 4, lty = 3)

# The 95% Value at Risk (VaR95%)
VaR95 <- numeric(13)
for (i in 1:13) {
  VaR95[i] <- quantile(pppaths[i,],.05)
}
# Legend
lines(VaR95,type = "l", col = "red",lwd = 3, lty = 6)
legend("topleft",inset=.05,c("Average", "VaR95%"),fill=c("blue","red"), horiz=TRUE)



##### Version 2 ####





#### Inputs ############

# Expected returns of different asset classes

# User inputs (expected returns given as annual level)

stock.er.a <- 0.05
real_estate.er.a <- 0.06
corp_bond.er.a <- 0.04
gov_bond.er.a <- 0.02


# expected returns transformed to monthly and logarithmic

stock.er.m.log <- log((1 + stock.er.a)^(1/12))
real_estate.er.m.log <- log((1 + real_estate.er.a)^(1/12))
corp_bond.er.m.log <- log((1 + corp_bond.er.a)^(1/12))
gov_bond.er.m.log <- log((1 + gov_bond.er.a)^(1/12))

Mu <- rbind(stock.er.m.log,real_estate.er.m.log,corp_bond.er.m.log,gov_bond.er.m.log)


# Volatility estimates of different assets classes (given as annual level)
stock.vol.a <- 0.15
real_estate.vol.a <- 0.20
corp_bond.vol.a <- 0.05
gov_bond.vol.a <- 0.04

# trasformation to monthly level
stock.vol.m <- stock.vol.a*sqrt(1/12)
real_estate.vol.m <- real_estate.vol.a*sqrt(1/12)
corp_bond.vol.m <- corp_bond.vol.a*sqrt(1/12)
gov_bond.vol.m <- gov_bond.vol.a*sqrt(1/12)

vol_vector <- c(stock.vol.m, real_estate.vol.m, corp_bond.vol.m, gov_bond.vol.m)

# Correlations between different assets classes (difference whether using returns or log returns not significant)
correl_matrix <- cor(cbind(stock$Log_returns,real_estate$Log_returns,corp_bond$Log_returns,gov_bond$Log_returns)) # can be given by user

# Portfolio weights
stock.weight <- 0.25
real_estate.weight <- 0.25
corp_bond.weight <-0.25
gov_bond.weight <-0.25

weight_vector <- rbind(stock.weight,real_estate.weight,corp_bond.weight,gov_bond.weight)

#### Simulation ########

# Simulating values from Multivariate Normal distribution

# covariance matrix
cov_matrix <- diag(vol_vector)%*%correl_matrix%*%diag(vol_vector)

# Simulating sim different price paths for tm month time interval for portfolio

tm <- 12 # Simulation period in moths
sim <- 10000 # number of simulations

pppaths <- matrix(data = NA, nrow = tm + 1, ncol = sim) # portfolio price paths matrix
colnames(pppaths) <- c(1:sim)
rownames(pppaths) <- c(0:tm)

for (i in 1:sim) {
  sample <- sim_returns_norm_dist(Mu,cov_matrix,tm)   # Simulating a sample of tm montly returnsfor all asset classes
  sample <- (exp(sample)-1)                           # Converting logarithmic returns to returns
  pf_returns <- sample%*%weight_vector                # Constructing monthly portfolio returns
  pppaths[,i]<- create_price_paths(pf_returns)        # creating 1 year cumulative price paths
}


### Price Paths ########

# Plotting all the paths
g_range <- range(min(pppaths[tm+1,]),max(pppaths[tm+1,]))
plot(pppaths[,1], type = "l", xlim = c(0,tm), ylim=g_range, ann=FALSE, xaxt="n")
axis(1, at=1:(tm+1), lab=c(0:tm))
box()

for(i in 2:sim) {
  lines(pppaths[,i],type = "l")
}

# Mean line
m_line <- numeric(tm+1)
for (i in 1:(tm+1)) {
  m_line[i] <- mean(pppaths[i,])
}

lines(m_line,type = "l", col = "blue",lwd = 4, lty = 3)

# The 95% Value at Risk (VaR95%)
VaR95 <- numeric(tm+1)
for (i in 1:(tm+1)) {
  VaR95[i] <- quantile(pppaths[i,],.05)
}
# Legend
lines(VaR95,type = "l", col = "red",lwd = 3, lty = 6)
legend("topleft",inset=.05,c("Average", "VaR95%"),fill=c("blue","red"), horiz=TRUE)

