install.packages("pls")
library(pls)
library(caret)
library(pls)

energy_agg <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")
view(energy_agg)

energy_agg <- energy_agg %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max, -day )
glimpse(energy_agg)

energy_agg <- energy_agg %>% 
  select(-X)

set.seed(123)
indpcr <- sample(2, nrow(energy_agg), replace = TRUE, prob = c(0.7, 0.3))
PCR_train <- energy_agg[indpcr==1,]
PCR_test <- energy_agg[indpcr==2,]


PCR_train <- as.data.frame(scale(PCR_train))
PCR_test <- as.data.frame(scale(PCR_test))


# Check for missing values
sum(is.na(PCR_train))
nearZeroVar(PCR_train, saveMetrics = TRUE) # No near Zero variance

sum(is.na(PCR_test))
nearZeroVar(PCR_test, saveMetrics = TRUE) # No near Zero variance

# Perform PCR
pcr_model <- pcr(energy_sum ~., 
                 data = PCR_train, scale = TRUE, validation = "CV")

summary(pcr_model)
pcr_loaad <-loadings(pcr_model)
print(pcr_loaad[,1:5])
# Determine optimal number of component

validationplot(pcr_model, val.type = "MSEP") 

pcr_predictions <- predict(pcr_model, ncomp = 7, newdata = PCR_test)


# Calculate RMSE
rmse <- sqrt(mean((PCR_test$energy_sum - pcr_predictions)^2))
# Calculate MSE
mse <- mean((PCR_test$energy_sum - pcr_predictions)^2)
# Calculate MAE
mae <- mean(abs(PCR_test$energy_sum - pcr_predictions))
# Calculate R-squared
sse <- sum((PCR_test$energy_sum - pcr_predictions)^2)  # Sum of Squared Errors
sst <- sum((PCR_test$energy_sum - mean(PCR_test$energy_sum))^2)  # Total Sum of Squares
r_squared <- 1 - (sse / sst)





