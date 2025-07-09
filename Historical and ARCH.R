

############################################
#Install required packages
# Loading packages
###########################################

install.packages(c("quantmod", "TTR", "rugarch", "PerformanceAnalytics", "forecast"))

install.packages("dynlm")
install.packages("FinTS","broom")
install.packages("MTS")

# Load libraries
library(quantmod) # For downloading data
library(TTR) # getting data from Yahoo finance and other functions
library(PerformanceAnalytics) # Fore evaluation 
library(rugarch) # For estimating GARCH Model
library(forecast) # For forecasting 
library(tseries) # For Time seris / Unit root test
library(dynlm) # For dynlm function 
library(MTS) # for ARCH test
library(FinTS) #for function `ArchTest()`
library(broom) #for `glance(`) and `tidy()`
library(ggplot2) #for plotting


#########################################################
# Downloading data and cleaning the data
########################################################

# Downloading daily data for TCS company 
getSymbols("TCS.NS", src = "yahoo", from = "2015-01-01", to = "2024-11-01")

# Extract necessary columns
stock_data <- na.omit(TCS.NS)
head(stock_data)

# Compute daily log returns
rtcs <- dailyReturn(Cl(stock_data), type = "log")


########################################################
#Different methods to compute volatility
########################################################

# Compute annualized Close-to-Close volatility
vol_cc <- sd(rtcs, na.rm = TRUE) * sqrt(252) # Annualized using 252 trading days
cat("Annualized Close-to-Close Volatility:", vol_cc, "\n")



# Compute Parkinson's volatility
hl_range <- (log(Hi(stock_data)) - log(Lo(stock_data)))^2
vol_parkinson <- sqrt(sum(hl_range, na.rm = TRUE) / (4 * nrow(stock_data) * log(2))) * sqrt(252) # Annualized
cat("Annualized Parkinson's High-Low Volatility:", vol_parkinson, "\n")


# Compute Garman-Klass volatility
hl_squared <- (log(Hi(stock_data)) - log(Lo(stock_data)))^2
co_squared <- (log(Cl(stock_data)) - log(Op(stock_data)))^2
vol_gk <- sqrt(sum(0.5 * hl_squared - (2 * log(2) - 1) * co_squared, na.rm = TRUE) / nrow(stock_data)) * sqrt(252) # Annualized
cat("Annualized Garman-Klass Volatility:", vol_gk, "\n")


# Combine all volatility measures
vol_comparison <- data.frame(
    Method = c("Close-to-Close", "Parkinson's High-Low", "Garman-Klass"),
    Annualized_Volatility = c(vol_cc, vol_parkinson, vol_gk)
)

# Print the comparison
print(vol_comparison)
# In Close-to-Close intra-day trading has not taken consideration -> higher volatility
# In case of 'Parkinson" capture the intra-day
# German class take consideration of both high-low and close-to-close -> both intra-day and overnight structure

# Plot comparison

barplot(vol_comparison$Annualized_Volatility, 
        names.arg = vol_comparison$Method, 
        col = c("blue", "green", "red"), 
        main = "Comparison of Annualized Volatilities", 
        ylab = "Volatility", 
        ylim = c(0, max(vol_comparison$Annualized_Volatility) + 0.05))

####################################################################
#Conclusion
###################################################################

# The Close-to-Close volatility is slightly higher, as it does not account 
#for intraday price ranges or opening prices.
# The Parkinson’s High-Low method shows lower volatility due to its 
#focus on the high-low range, ignoring closing prices.
# The Garman-Klass method, being a hybrid, strikes a balance and 
#is more suitable for accurate volatility estimation.

###############################################################
# Prediction based on historical volatility Model
##############################################################


# Historical average volatility
historical_volatility <- sd(rtcs, na.rm = TRUE) * sqrt(252) # Annualized
cat("Historical Average Volatility:", historical_volatility, "\n")


# Simple Moving Average: Rolling 30-day standard deviation
sma_volatility <- rollapply(
    rtcs, 
    width = 30, 
    FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), # Annualized
    align = "right"
)


# Compute Exponential Moving Average volatility using TTR package
ema_volatility <- EMA(rtcs^2, n = 30) # Square returns
ema_volatility <- sqrt(ema_volatility) * sqrt(252) # Annualized


# Exponential weighted Moving Average volatility with a lambda decay factor
lambda <- 0.94
ewma_volatility <- sqrt(
    rollapply(
        rtcs^2, 
        width = 30, 
        FUN = function(x) sum(lambda^(length(x):1) * x) / sum(lambda^(length(x):1)),
        align = "right"
    )
) * sqrt(252) # Annualized



# Combine all volatilities
volatility_df <- data.frame(
    Date = index(sma_volatility),
    SMA = as.numeric(sma_volatility),
    EMA = as.numeric(ema_volatility),
    EWMA = as.numeric(ewma_volatility)
)

# Remove NAs
volatility_df <- na.omit(volatility_df)
head(volatility_df)


# Plot comparison of volatilities
plot(volatility_df$Date, volatility_df$SMA, type = "l", col = "blue", lwd = 2,
     main = "Comparison of Sample Volatilities",
     xlab = "Date", ylab = "Annualized Volatility",
     ylim = range(volatility_df[, 2:4]))
lines(volatility_df$Date, volatility_df$EMA, col = "red", lwd = 2)
lines(volatility_df$Date, volatility_df$EWMA, col = "green", lwd = 2)

# Add legend
legend("topright", legend = c("SMA", "EMA", "EWMA"),
       col = c("blue", "red", "green"), lwd = 2)




##############################################################################
# ARCH Model
#############################################################################

# We will use the same data
#Converting the data in time series
date <- index(rtcs)
# Visualize returns
plot(rtcs, main = "Daily Log Returns", col = "blue",lwd=1)

ggplot(rtcs, aes(y = daily.returns,x=date )) + geom_line(col = "blue",size=0.75) +
    labs(title = 'TCS Daily Stock Returns', y = "Return")


#ARCH Test
rtcsArchTest <- ArchTest(rtcs, lags=1, demean=TRUE)
rtcsArchTest

# p-value is close to 0, we reject the null hypothesishttp://127.0.0.1:19371/graphics/plot_zoom_png?width=286&height=182


# Specify an ARCH(1) model
arch_spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), # ARCH(1)
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)

# Fit the ARCH model
arch_fit <- ugarchfit(spec = arch_spec, data = rtcs)

###########################################################################
## Interpretation of ARCH(1) Model Results
###########################################################################

# 1. ARCH Effect Test (Pre-modeling):
# The ARCH LM test (lags = 1) returned a p-value close to 0, 
# indicating that we reject the null hypothesis of no ARCH effects.
# ⇒ The return series exhibits conditional heteroskedasticity,
#    and ARCH/GARCH modeling is appropriate.

# 2. ARCH(1) Model Fit Summary:
# - The model fitted is sGARCH(1,0), equivalent to an ARCH(1) model.
# - All parameters (mu, omega, alpha1) are statistically significant
#   under standard errors, suggesting a valid ARCH(1) fit.
# - However, under robust standard errors, parameters lose significance,
#   indicating potential model instability or misspecification.

# 3. Residual Diagnostics:
# - Ljung-Box test on standardized residuals shows significant p-values,
#   indicating residual autocorrelation still exists (not ideal).
# - Ljung-Box test on squared residuals yields high p-values (> 0.77),
#   suggesting the model has successfully captured volatility clustering.

# 4. Weighted ARCH LM Tests:
# - All p-values > 0.85 → Fail to reject H0 ⇒ No residual ARCH effects remain.
# ⇒ The ARCH(1) model has adequately removed conditional heteroskedasticity.

# 5. Nyblom Stability Test:
# - Joint and individual statistics exceed critical values for omega.
# ⇒ Indicates instability in model parameters over time — 
#    consider testing for structural breaks or using rolling window models.

# 6. Sign Bias Tests:
# - All tests (Sign, Negative, Positive) are highly significant (p ≈ 0),
#   showing asymmetry in how positive and negative shocks affect volatility.
# ⇒ Suggests a better fit may be obtained with asymmetric models like:
#    - EGARCH (Exponential GARCH)
#    - GJR-GARCH

# 7. Pearson Goodness-of-Fit:
# - Very high test statistics with p-values ≈ 0.
# ⇒ Indicates poor fit of the model to the distribution of returns.
# ⇒ Consider switching to a heavier-tailed distribution (e.g., t or skewed-t).

# Summary:
# ✅ ARCH(1) model captures volatility clustering well
# ❌ Serial correlation in residuals remains
# ❌ Model fails to account for asymmetry and shows parameter instability
# 🔄 Next Steps:
#     → Try EGARCH or GJR-GARCH
#     → Use Student-t distribution for better tail behavior
#     → Investigate parameter stability over time

###########################################################################


# Display the summary
print(arch_fit)
plot(arch_fit)

# Plot standardized residuals
plot(arch_fit, which = 10) # Residual diagnostics plot


# Check for autocorrelation in residuals
acf(residuals(arch_fit, standardize = TRUE), main = "ACF of Standardized Residuals")

arch_fit@fit

# Ljung-Box Test for no ARCH effects
Box.test(residuals(arch_fit, standardize = TRUE)^2, lag = 12, type = "Ljung-Box")

###########################################################################
## Ljung-Box Test on Squared Standardized Residuals
###########################################################################

# The Ljung-Box test was performed on the squared standardized residuals
# from the ARCH(1) model to check for any remaining ARCH effects.

# Result:
#   X-squared = 0.857, df = 12, p-value = 1

# Interpretation:
# - Since the p-value is much greater than 0.05, we fail to reject the null hypothesis.
# - This means there is **no significant autocorrelation in the squared residuals**.
# - ⇒ The ARCH(1) model has successfully captured the time-varying volatility.
# - ⇒ No further ARCH effects remain, indicating a good fit in terms of volatility dynamics.

# Note:
# - This test complements the Weighted ARCH LM test results, both confirming
#   that the model has removed volatility clustering from the residuals.

###########################################################################




# Extract the conditional variance
conditional_sigma <- sigma(arch_fit)
conditional_variance <- conditional_sigma^2
# Display the first few rows of the estimated variance
head(conditional_variance)

# The ARCH model estimates time-varying volatility (conditional variance).
# Here, we extract the conditional standard deviations (σ_t) and square them
# to obtain the conditional variances (σ_t²) for each time point.

# These values represent how the model believes variance (risk) evolves over time.

# You can plot them to visualize volatility clustering patterns in the returns.


# Plot the estimated conditional variance
plot(conditional_variance, type = "l", col = "blue", lwd = 2,
     main = "Estimated Conditional Variance from ARCH Model",
     xlab = "Time", ylab = "Variance")

###########################################################################
## Plotting Estimated Conditional Variance from ARCH(1) Model
###########################################################################

# The plot visualizes time-varying conditional variance (σ_t^2) estimated 
# by the ARCH(1) model over the sample period.

# Interpretation:
# - The variance is not constant and shows clear evidence of volatility clustering.
# - Large spikes correspond to major market events (e.g., COVID-19 in early 2020).
# - After each spike, volatility gradually declines — consistent with mean-reversion.
# - This justifies the use of ARCH models over constant-variance models.

# Overall, the ARCH model successfully captures time-dependent volatility behavior.


# Convert variance to standard deviation
conditional_volatility <- sqrt(conditional_variance)

# Convert conditional variance back to conditional standard deviation
# (also known as conditional volatility).
# This is useful for plotting and interpreting the magnitude of volatility over time.

# Plot the conditional volatility
plot(conditional_volatility, type = "l", col = "red", lwd = 2,
     main = "Estimated Conditional Volatility from ARCH Model",
     xlab = "Time", ylab = "Volatility")

###########################################################################
## Plotting Estimated Conditional Volatility from ARCH(1) Model
###########################################################################

# This plot displays the conditional standard deviation (σ_t) estimated
# by the ARCH(1) model over the sample period.

# Interpretation:
# - Conditional volatility is time-varying and shows clear clustering.
# - A major volatility spike is observed in early 2020 (likely due to COVID-19).
# - The model captures periods of elevated market uncertainty and reversion to normalcy.
# - Volatility levels remain elevated during turbulent times and lower in stable periods.

# This visualization supports the use of ARCH models in modeling financial time series
# where constant variance assumptions are invalid.


# Forecast 10-step ahead volatility
arch_forecast <- ugarchforecast(arch_fit, n.ahead = 10)

# Extract forecasted volatility
forecasted_vol <- sigma(arch_forecast)
print(forecasted_vol)

###########################################################################
## 10-Step Ahead Volatility Forecast (ARCH Model)
###########################################################################

# We use the ugarchforecast() function to forecast volatility 10 steps ahead.
# The resulting values are the model's predictions of conditional standard deviation
# (i.e., future volatility) for the next 10 time periods.

# Interpretation:
# - T+1 to T+10 represent the forecasted volatility from one to ten steps ahead.
# - The predicted volatilities are decreasing over time,
#   indicating the model expects risk to gradually subside.

# These forecasts are useful in:
# - Risk management (e.g., VaR, margin setting)
# - Option pricing and hedging
# - Financial decision-making under uncertainty

# Forecasted conditional volatilities:


################################################
# Using forecast matrices
################################################
arch_fit2 <- ugarchfit(spec = arch_spec,out.sample = 10,   data = rtcs)
arch_forecast2 <- ugarchforecast(arch_fit2, n.ahead = 10)
fpm(arch_forecast2)

###########################################################################
## Forecast Performance Metrics (FPM) for ARCH Model
###########################################################################

# We fit the ARCH(1) model using 10 observations as out-of-sample data
# and evaluate its 10-step ahead forecast performance using fpm().

# The forecast performance metrics are:
# - MSE (Mean Squared Error): 0.00247 → Lower is better
# - MAE (Mean Absolute Error): 0.0483 → Measures average error magnitude
# - DAC (Directional Accuracy): 0.5 → Model predicts the direction of change
#   correctly only 50% of the time (no better than random)
# - N: 10 out-of-sample forecasts evaluated

# Interpretation:
# While the error levels (MSE, MAE) are modest, the DAC of 0.5 suggests
# the model struggles to anticipate whether volatility will rise or fall.

# ⇒ Consider trying a more flexible model like GARCH(1,1) or EGARCH for
#    better directional forecasting of volatility.



