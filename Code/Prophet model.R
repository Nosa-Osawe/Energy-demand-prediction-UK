####################################################################################################333
library(tidyverse)
library(prophet)
energy_agg <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")


df_prophet <- energy_agg[,c("day", "energy_sum")]
head(df_prophet)

df_prophet <- df_prophet %>% 
  rename(ds= day,
         y= energy_sum)

# Fit the model
model <- prophet(df_prophet, daily.seasonality=TRUE)

# Create future dates for prediction (e.g., forecast for the next 30 days)
future <- make_future_dataframe(model, periods = 182)

# Make predictions
forecast <- predict(model, future)

# Plot the forecast
plot(model, forecast)



# Load required library
library(ggplot2)

# Create a forecast plot using the Prophet model
forecast_plot <- plot(model, forecast)
forecast$ds <- as.Date(forecast$ds)
# Customize the axis titles
forecast_plot + 
  xlab("Date") + 
  ylab("Mean Energy Consumption per Household (KW/day)")+
   theme_bw()


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