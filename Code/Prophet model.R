####################################################################################################333

library(prophet)


df_prophet <- agg[,c("day", "energy_sum")]
head(df_prophet)

df_prophet <- df_prophet %>% 
  rename(ds= day,
         y= energy_sum)

# Fit the model
model <- prophet(df_prophet, daily.seasonality=TRUE)

# Create future dates for prediction (e.g., forecast for the next 30 days)
future <- make_future_dataframe(model, periods = 60)

# Make predictions
forecast <- predict(model, future)

# Plot the forecast
plot(model, forecast)

# Plot the forecast components
prophet_plot_components(model, forecast)


df_prophet$ds <- as.Date(df_prophet$ds)
forecast$ds <- as.Date(forecast$ds)

# Check date ranges
range(df_prophet$ds)
range(forecast$ds)

# Merge actual data (df_prophet) with predictions (forecast)
df_eval <- merge(df_prophet, forecast[, c('ds', 'yhat')], by = 'ds')

# View the first few rows to check alignment
head(df_eval)

df_eval$residuals <- df_eval$y - df_eval$yhat

# R-squared
SST <- sum((df_eval$y - mean(df_eval$y))^2)
SSE <- sum(df_eval$residuals^2)
R2 <- 1 - (SSE / SST)

# MSE
MSE <- mean(df_eval$residuals^2)

# MAE
MAE <- mean(abs(df_eval$residuals))

# RMSE
RMSE <- sqrt(MSE)

# Print the results
cat("R-squared:", R2, "\n")
cat("MSE:", MSE, "\n")
cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")