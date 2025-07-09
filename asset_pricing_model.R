# Load Required Libraries
install.packages(c("quantmod", "PerformanceAnalytics", "tidyverse", "tseries"))
library(quantmod) # To download data
library(PerformanceAnalytics)
library(tidyverse)
library(tseries)
library(ggplot2)
library(gridExtra)
library(readxl)
library(lmtest)
library(car)
install.packages("gridExtra")
install.packages("readxl")
library(readxl)

####################################################################
#Importing Data
###################################################################
# Here, we are downloading data directly from web source(Yahoo Finance)
# Define the tickers for Indian stocks (from NSE)
tickers <- c("RELIANCE.NS", "TCS.NS", "INFY.NS")  # Example stocks

# Download data from Yahoo Finance
getSymbols(tickers, from = "2020-01-01", to = "2025-06-01", src = "yahoo")



####################################################################
#Preparing Data for Estimation
###################################################################

# Step 3: Calculate Daily Returns
# Calculate daily returns for each stock
returns <- na.omit(merge(
  dailyReturn(Cl(RELIANCE.NS)),
  dailyReturn(Cl(TCS.NS)),
  dailyReturn(Cl(INFY.NS))
))
colnames(returns) <- c("reliance","tcs","infosys")


####################################################################
## Construct a Portfolio
###################################################################

# Define weights for the portfolio
weights <- c(0.4, 0.3, 0.3)

# Calculate portfolio returns
portfolio_returns <- Return.portfolio(returns, weights = weights)

####################################################################
## Portfolio Metrics
###################################################################


# Portfolio mean and variance
portfolio_mean <- mean(portfolio_returns)
portfolio_variance <- var(portfolio_returns)
portfolio_sd <- sd(portfolio_returns)

# Print portfolio metrics
print("Portfolio Mean:")
print(portfolio_mean)
print("Portfolio Variance:")
print(portfolio_variance)
print("Portfolio Standard Deviation:")
print(portfolio_sd)


####################################################################
## Visualize Returns
###################################################################


# Plot individual stock returns

# Converting the xts object to data frame to use `ggplot2` function
plotdata <- fortify.zoo(returns)
# Create the first plot
plot1 <- ggplot(plotdata, aes(x = Index, y = reliance)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Log Returns of Reliance", x ="Time" , y = "Value") +
  theme_minimal()

# Create the second plot
plot2 <- ggplot(plotdata, aes(x =Index , y = tcs)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Log Returns of TCS", x = "Time", y = "Value") +
  theme_minimal()


# Create the third plot
plot3 <- ggplot(plotdata, aes(x =Index , y = infosys)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Log Returns of Infosys", x = "Time", y = "Value") +
  theme_minimal()

# Arrange the two plots side by side
grid.arrange(plot1, plot2, plot3, ncol = 1)


chart.TimeSeries(portfolio_returns, main = "Portfolio Returns")


####################################################################
## Calculating CAPM Beta
###################################################################


# Download market index data (e.g., NIFTY 50)
getSymbols("^NSEI", from = "2020-01-01", to = "2025-06-01", src = "yahoo")
market_returns <- dailyReturn(Cl(NSEI))


# Align dates for stock returns and market returns
aligned_returns <- merge(returns, market_returns, all = FALSE)

# Extract market returns after alignment
aligned_market_returns <- aligned_returns[, ncol(aligned_returns)]

# Function to calculate beta
beta <- function(stock_returns, market_returns) {
  cov(stock_returns, market_returns) / var(market_returns)
}

# Calculate betas for individual stocks
betas <- sapply(1:(ncol(aligned_returns) - 1), function(i) {
  beta(aligned_returns[, i], aligned_market_returns)
})

# Calculate portfolio beta
portfolio_beta <- sum(weights * betas)

# Display results
cat("Individual Betas:\n")
names(betas) <- colnames(returns)
print(betas)
cat("Portfolio Beta:", portfolio_beta, "\n")

#########################################
# CAPM beta: using a function
#########################################

# Calculate individual betas using CAPM.beta
individual_betas <- sapply(returns, function(stock_returns) {
  CAPM.beta(stock_returns, market_returns)
})

betareliance <- CAPM.beta(returns$reliance, market_returns)
betatcs <- CAPM.beta(returns$tcs, market_returns)
betainfosys <- CAPM.beta(returns$infosys, market_returns)
individual_betas <- cbind(betareliance,betatcs,betainfosys)

# Calculate portfolio returns again to ensure consistency
portfolio_returns <- Return.portfolio(returns, weights = weights)

# Calculate portfolio beta using CAPM.beta
portfolio_beta <- CAPM.beta(portfolio_returns, market_returns)

# Display results
cat("Individual Betas:\n")
names(individual_betas) <- colnames(returns)
print(individual_betas)
cat("Portfolio Beta:", portfolio_beta, "\n")



#########################################
# CAPM beta: using a regression
#########################################


# price data
cdata <- read_excel("E:\\Financial Econometrics\\Week 8 and 9 Case Studies\\data.xlsx", col_types = c("date", 
                                                                                                      "numeric", "numeric", "numeric", "numeric", 
                                                                                                      "numeric"))
#Calculating Return
rdata <- Return.calculate(cdata, method = "log")
# Removing Missing Values

rdata <- rdata[-1,]

# Calculating difference

rdata$TBR <- rdata$TBR/12
rdata$TCS <- rdata$TCS-rdata$TBR
rdata$Nifty50 <- rdata$Nifty50-rdata$TBR


# Calculating CAPM beta 


plot(rdata$Nifty50,rdata$TCS,pch=19)

lm_capm = lm(rdata$TCS ~ rdata$Nifty50, data = rdata)
summary(lm_capm)

linearHypothesis(lm_capm,c("rdata$Nifty50=1"))
linearHypothesis(lm_capm,hypothesis.matrix = diag(2),rhs = c(1,1))
linearHypothesis(lm_capm,c("(Intercept)=1","rdata$Nifty50=1"))


####################################################################
## Arbitrage Pricing theory
###################################################################



# Fit the APT model for each stock



models <- lm(rdata$TCS~rdata$Nifty50+rdata$WPI+rdata$OP)

summary(models)





####################################################################
## Test Market Efficiency Hypothesis (EMH)
###################################################################


# Perform ADF test for each stock
emh_test <- sapply(returns, function(x) adf.test(x, alternative = "stationary"))

# Print EMH test results
print("EMH Test Results (ADF Test):")
print(emh_test)

