
#title: "SCMA 632 a6b "
#author: "Pramitt M. Patil"
#date: "2024-07-25"
#output: html_document


# Install an load necessary packag
install.packages("tseries")
install.packages("xts")
install.packages("forecast")
install.packages("rugarch")

library(rugarch)
library(tseries)
library(xts)
library(forecast)

install.packages("FinTS")
library(FinTS)

# Load the data
data <- read.csv("C://Users//prami//Desktop//SCMA 632//SCMA 632//assignments//A6b//NEST Historical Data.csv")
# Inspect the first few rows of the data
head(data)

# Convert the date column to Date type if necessary
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Load necessary library
library(xts)

# Sample data for demonstration (if not already defined)
data <- data.frame(
  Date = c("2024-07-01", "2024-07-02", "2024-07-03", NA, "2024-07-05"),
  Adj.Close = c(100, 101, 102, 103, 104)
)

# Convert Date column to Date format (if needed)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Check for NA, NaN, or Inf in Date
na_dates <- which(is.na(data$Date))
inf_dates <- which(is.infinite(data$Date))
nan_dates <- which(is.nan(data$Date))

# Print problematic indices
print(na_dates)  # Indices of NA dates
print(inf_dates) # Indices of Inf dates
print(nan_dates) # Indices of NaN dates

# Remove rows with NA, NaN, or Inf dates
valid_data <- data[!is.na(data$Date) & !is.infinite(data$Date) & !is.nan(data$Date), ]

# Convert to xts object using cleaned data
market <- xts(valid_data$Adj.Close, order.by = valid_data$Date)

# Print the xts object to verify
print(market)

# Calculate percentage returns
returns <- 100 * diff(log(market))  # log returns * 100
returns <- returns[!is.na(returns)]  # Remove NA values

# Plot the returns
plot(returns, main="Returns", ylab="Percentage Returns", xlab="Date")

# Fit an ARCH model to the cleaned returns
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                        distribution.model = "norm")
arch_fit <- ugarchfit(spec = arch_spec, data = returns)

# Print the summary of the fitted model
print(arch_fit)

# Plot the fitted model's conditional volatility
plot(arch_fit, which = 3)
arch_fit <- ugarchfit(spec = arch_spec, data = returns)

# Fit a GARCH model to the cleaned returns
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = returns)

# Print the summary of the fitted model
print(garch_fit)

# Plot the fitted model's conditional volatility
plot(garch_fit, which = 3)

# Forecast the volatility
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 90)

# Extract and print the forecasted variances
forecasted_variance <- garch_forecast@forecast$sigmaFor
tail(forecasted_variance, 3)

# Plot the forecasted volatility
plot(garch_forecast, which = 1)