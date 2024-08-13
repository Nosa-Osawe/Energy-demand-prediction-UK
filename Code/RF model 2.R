write.csv(agg, "C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")


view(agg)

agg <- agg %>%
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max )

library(randomForest)



set.seed(123)
ind <- sample(2, nrow(agg), replace = TRUE, prob = c(0.7, 0.3))
rf_train <- agg[ind==1,]
rf_test <- agg[ind==2,]

rf_energy <- randomForest(energy_sum~.-day, data=rf_train,
                          ntree = 500,
                          mtry = 3,
                          importance = TRUE,
                          proximity = TRUE)

best_mtry <- tuneRF(rf_train[,-which(names(rf_train) == "energy_sum")], 
                    rf_train$energy_sum,
                    ntreeTry = 500,  # Number of trees to try
                    stepFactor = 1.5,  # Increment factor for mtry
                    improve = 0.01,  # Minimum improvement to continue tuning
                    trace = TRUE,  # Print the progress
                    plot = TRUE)  # Plot the OOB error vs mtry


varImpPlot(rf_energy,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


# Predict on the test set
predictions <- predict(rf_energy, newdata = rf_test)

# Actual values
actuals <- rf_test$energy_sum

# Compute Mean Squared Error (MSE)
mse <- mean((predictions - actuals)^2)

# Compute Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Compute Mean Absolute Error (MAE)
mae <- mean(abs(predictions - actuals))

# Compute R-squared (Coefficient of Determination)
ss_total <- sum((actuals - mean(actuals))^2)  # Total sum of squares
ss_res <- sum((actuals - predictions)^2)      # Residual sum of squares
r_squared <- 1 - (ss_res / ss_total)

# Print results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")


