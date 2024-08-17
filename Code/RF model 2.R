library(randomForest)
library(tidyverse)
library(Metrics)

agg_energy <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")

attach(agg_energy)
agg <- agg_energy %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max, -day, -X )
attach(agg)

agg <- agg %>% 
  select(-temperatureMin, -apparentTemperatureHigh, -apparentTemperatureMax,
         -apparentTemperatureLow, -temperatureHigh,- apparentTemperatureMin)


set.seed(123)
ind <- sample(2, nrow(agg), replace = TRUE, prob = c(0.7, 0.3))
rf_train <- agg[ind==1,]
rf_test <- agg[ind==2,]



best_mtry <- tuneRF(rf_train[,-which(names(rf_train) == "energy_sum")], 
                    rf_train$energy_sum,
                    ntreeTry = 500,  # Number of trees to try
                    stepFactor = 2,  # Increment factor for mtry
                    improve = 0.01,  # Minimum improvement to continue tuning
                    trace = TRUE,  # Print the progress
                    plot = TRUE)  # Plot the OOB error vs mtry

rf_energy <- randomForest(energy_sum~., data=rf_train,
                          ntree = 500,
                          mtry = 4,
                          importance = TRUE,
                          proximity = TRUE)
varImpPlot(rf_energy,
           sort = T,
           n.var = 6,
           main = "Top 6 - Variable Importance")


# Predict on the test set
rf_predictions <- as.numeric(predict(rf_energy, newdata = rf_test)) 

# Actual values
rf_actuals <- as.numeric(rf_test$energy_sum)

mse(rf_actuals, rf_predictions)
rmse(rf_actuals, rf_predictions)
mae(rf_actuals, rf_predictions)

# Compute R-squared (Coefficient of Determination)
ss_total <- sum((rf_actuals - mean(rf_actuals))^2)  # Total sum of squares
ss_res <- sum((rf_actuals - rf_predictions)^2)      # Residual sum of squares
r_squared <- 1 - (ss_res / ss_total)

###############################################################################

# PCA + RF

PCAbest_mtry <- tuneRF(rf_train[,-which(names(new_pcr_train) == "energy_sum")], 
                       rf_train$energy_sum,
                       ntreeTry = 500,  # Number of trees to try
                       stepFactor = 1.5,  # Increment factor for mtry
                       improve = 0.01,  # Minimum improvement to continue tuning
                       trace = TRUE,  # Print the progress
                       plot = TRUE)  # Plot the OOB error vs mtry


PCArf_energy <- randomForest(energy_sum~., data=new_pcr_train,
                          ntree = 500,
                          mtry = 4,
                          importance = TRUE,
                          proximity = TRUE)


varImpPlot(PCArf_energy,
           sort = T,
           n.var = 5,
           main = "Top 10 - Variable Importance")

# Predict on the test set
PCArf_predictions <- as.numeric(predict(PCArf_energy, newdata = new_pcr_test)) 

# Actual values
PCArf_actuals <- as.numeric(new_pcr_test$energy_sum)

mse(PCArf_actuals, PCArf_predictions)
rmse(PCArf_actuals, PCArf_predictions)
mae(PCArf_actuals, PCArf_predictions)

# Compute R-squared (Coefficient of Determination)
ss_total <- sum((PCArf_actuals - mean(PCArf_actuals))^2)  # Total sum of squares
ss_res <- sum((PCArf_actuals - PCArf_predictions)^2)      # Residual sum of squares
r_squared <- 1 - (ss_res / ss_total)
