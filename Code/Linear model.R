# Linear model using all predictors without normalization
library(car)

agg_energy <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")

attach(agg_energy)
agg_lm <- agg_energy %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max, -day )


set.seed(123)
indlm <- sample(2, nrow(agg_lm), replace = TRUE, prob = c(0.7, 0.3))
lm_train1 <- agg_lm[indlm==1,]
lm_test1 <- agg_lm[indlm==2,]

# Build the models
lm1 <- lm(energy_sum ~., data = lm_train1)
summary(lm1)
vif(lm1)

lmPred1 <- predict(lm1, newdata = lm_test1)

# Actual values
lm_actual1 <- lm_test1$energy_sum

#  Mean Squared Error (MSE)
lm1_mse <- mean((lmPred1 - lm_actual1)^2)

# Compute Root Mean Squared Error (RMSE)
lm1_rmse <- sqrt(lm1_mse)

# Compute Mean Absolute Error (MAE)
lm1_mae <- mean(abs(lmPred1 - lm_actual1))

ss_total <- sum((lm_actual1 - mean(lm_actual1))^2)  # Total sum of squares
ss_res <- sum((lm_actual1 - lmPred1)^2)      # Residual sum of squares
lm_r2 <- 1 - (ss_res / ss_total)

lm1_mse
lm1_rmse
lm1_mae
#----------------------------------------------------------------------------------------------------------
 # Linear model using important predictors from RF model


lm_agg2 <- lm(energy_sum ~uvIndex+temperatureMax + apparentTemperatureMax
             +temperatureHigh + apparentTemperatureLow +temperatureLow
             + pressure + humidity +apparentTemperatureHigh, data = lm_train1)

summary(lm_agg2)


lmPred2 <- predict(lm_agg2, newdata = lm_test1)

# Actual values
lm_actual2 <- lm_test1$energy_sum

#  Mean Squared Error (MSE)
lm2_mse <- mean((lmPred2 - lm_actual2)^2)

# Compute Root Mean Squared Error (RMSE)
lm2_rmse <- sqrt(lm2_mse)

# Compute Mean Absolute Error (MAE)
lm2_mae <- mean(abs(lmPred2 - lm_actual2))

ss_total <- sum((lm_actual2 - mean(lm_actual2))^2)  # Total sum of squares
ss_res <- sum((lm_actual2 - lmPred2)^2)      # Residual sum of squares
lm2_r2 <- 1 - (ss_res / ss_total)

lm2_mse
lm2_rmse
lm2_mae







# ---------------------------------------------------------------------------------------------
EWH_combined <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\EWH.csv", 
                         stringsAsFactors = TRUE)

attach(EWH_combined)
view(EWH_combined)
glimpse(EWH_combined)

lm_EWH  <- lm(energy_sum~.-day-X, data = EWH_combined)
summary(lm_EWH) 

min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply Min-Max normalization to all numeric columns
EWH_normalized <- EWH_combined %>%
  mutate(across(where(is.numeric), min_max_normalize))

sum(is.na(EWH_normalized))


lm_agg <- lm(energy_sum ~. -day, data = EWH_normalized)

summary(lm_agg)
    
length(unique(EWH_normalized$day))



