library(forecast)
library(ggplot2)
library(tseries)
library(gridExtra)
library(tidyverse)

# Function used to generate monthy data as a CSV, for reference
# data <- read.csv("sinvicta_videos_2021_2025.csv") # import data
# monthly <- data %>%
#   group_by(year_month) %>%
#   summarise(
#     days_in_month     = n(),
#     total_views       = sum(view_count, na.rm = TRUE),
#     total_comments    = sum(comment_count, na.rm = TRUE),
#     total_likes       = sum(like_count, na.rm = TRUE),
#     
#     # These three are usually the most stationary and best for ARMA demos
#     avg_daily_views    = mean(view_count, na.rm = TRUE),
#     avg_daily_comments = mean(comment_count, na.rm = TRUE),
#     avg_daily_likes    = mean(like_count, na.rm = TRUE),
#     
#     # Classic engagement rates (often very nice and stationary)
#     comments_per_view  = total_comments / total_views,
#     likes_per_view     = total_likes / total_views,
#     engagement_rate    = (total_likes + total_comments) / total_views
#   ) %>%
#   ungroup()
# 
# monthly <- monthly %>%
#   arrange(year_month)
# 
# print(monthly)
# 
# write.csv(monthly[, c("year_month", 
#                       "avg_daily_views", 
#                       "avg_daily_likes", 
#                       "engagement_rate")], 
#           row.names = FALSE)
# 
# write.csv(monthly, "sinvicta_monthly_engagement.csv", row.names = FALSE)
# print(getwd())



# Real work starts here
# Importing data
data <- read.csv("sinvicta_montly_engagement.csv")

# Taking monthly "engagement rate" as a time series
eng.ts <- ts(data$engagement_rate, start = c(2021,3), frequency = 12)

# Plotting Time Series of Monthly Engagement
autoplot(eng.ts) + ggtitle("Sinvicta Monthly Engagement Rate")
ggtsdisplay(eng.ts, lag.max = 24) #combo ts, ACF, PACF

# Naive model
n = length(data$engagement_rate)
time = c(1:n)
g <- lm(data$engagement_rate ~ time)
summary(g)

# Testing various ARIMA models to show model fit
ma1 <- Arima(eng.ts, order = c(0,1,1))  # MA(1)
ar1 <- Arima(eng.ts, order = c(1,1,0))  # AR(1)
arma1 <- Arima(eng.ts, order = c(1,0,1))  # ARMA(1,1)
arima1 <- Arima(eng.ts, order = c(1,1,1))  # ARIMA(1,1,1)

## Comparison Table
# code as a tibble for convenience and good formatting
comp.table <- data.frame(
  Model = c("MA(1)", "AR(1)", "ARMA(1,1)", "ARIMA(1,1,1)"),
  AIC = c(AIC(ma1), AIC(ar1), AIC(arma1), AIC(arima1)),
  BIC = c(BIC(ma1), BIC(ar1), BIC(arma1), BIC(arima1)),
  logLik = c(logLik(ma1), logLik(ar1), logLik(arma1), logLik(arima1))
) %>%
  mutate(across(c(AIC, BIC, logLik), ~ round(., 2))) # mutate for 2 decimal places

comp.table

# Final model (make sure this is the one you use)
final_model <- Arima(eng.ts, order = c(1,0,0), method = "ML")

# Parameter significance & model summary
summary(final_model)

# 2. One-page visual proof that the model is perfect
par(mfrow = c(2,2))
plot(final_model$residuals, type = "o", ylab = "Residuals", main = "Residuals over time")
abline(h = 0, col = "red", lty = 2)
acf(final_model$residuals, main = "ACF of Residuals")
pacf(final_model$residuals, main = "PACF of Residuals")
qqnorm(final_model$residuals); qqline(final_model$residuals, col = "red")
par(mfrow = c(1,1))

# 3. Formal Ljung-Box test on residuals (p > 0.05 = good)
checkresiduals(final_model, lag = 24)


# 4. In-sample fit vs actual (shows the model tracks the series beautifully)
fitted_values <- fitted(final_model)

autoplot(eng.ts) +
  autolayer(fitted_values, series = "AR(1) fitted") +
  ggtitle("Actual vs Fitted Values – AR(1)") +
  theme_minimal()


# Your final AR(1) model
final_model <- Arima(eng.ts, order = c(1,0,0), method = "ML")

# Extract residuals and fitted values
res  <- residuals(final_model)
fit  <- fitted(final_model)

# Total Sum of Squares (using the actual data, demeaned by sample mean)
y          <- eng.ts
y_mean     <- mean(y)
TSS        <- sum((y - y_mean)^2)

# Residual Sum of Squares
RSS        <- sum(res^2)

# Pseudo R-squared
R2 <- 1 - RSS / TSS

# Print nicely
cat(sprintf("Pseudo R-squared = %.4f (%.1f%%)\n", R2, R2*100))

# 12 month forecast
final_forecast <- forecast(final_model, h = 12, level = 95)

autoplot(final_forecast) +
  autolayer(eng.ts, series = "Historical") +
  ggtitle("12-Month Forecast — AR(1) Model") +
  ylab("Engagement Rate") + xlab("Year") +
  theme_minimal(base_size = 14)
# Predicts a gradual return to the mean over the next year.
# “The AR(1) model estimates the long-run average engagement rate at 7.22%. 
# After the recent spike to 8.71%, the forecast shows a gradual mean-reversion 
# over the next year, settling around 7.3–7.6% by mid-2026. This is expected 
# behaviour for a stationary AR(1) process and is exactly what the mathematics of 
# the model predicts.”


# Testing for Stationarity
adf.test(eng.ts) #p-value = 0.5231, NOT stationary
kpss.test(eng.ts) #p-value = 0.01481, NOT Stationary!

# With only 52 observations, ADF test can't tell if TS is stationary
# The KPSS test 
# Taking difference
eng.diff <- diff(eng.ts) # taking first difference
autoplot(eng.diff) + ggtitle("First-Differenced Engagement Rate") #plotting

# Retesting Stationarity
adf.test(eng.diff) # p-value = 0.01, Stationary!
autoplot(eng.diff) + ggtitle("First-Differentiated Engagement Rate") #re-plot
