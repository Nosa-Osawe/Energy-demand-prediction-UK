library(tidyverse)
library(caret)
library(pls)
library(Metrics)
library(psych)

energy_agg <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")
glimpse(energy_agg)

energy_agg <- energy_agg %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max, -day )
glimpse(energy_agg)

energy_agg <- energy_agg %>% 
  select(-X)

set.seed(123)
indpcr <- sample(2, nrow(energy_agg), replace = TRUE, prob = c(0.7, 0.3))
PCR_train <- energy_agg[indpcr==1,]
PCR_test <- energy_agg[indpcr==2,]


PCR_train <- as.data.frame(scale(PCR_train)) # This is not so necessary since the PCR function can scale it all together
PCR_test <- as.data.frame(scale(PCR_test)) # Doing it twice has no ill-effect, too.


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
print(pcr_loaad[,1:7])
# Determine optimal number of component

validationplot(pcr_model, val.type = "MSEP") 

pcr_predictions <- predict(pcr_model, ncomp = 7, newdata = PCR_test)

# RMSE, MSE and MAE

mse(PCR_test$energy_sum, pcr_predictions)
rmse(PCR_test$energy_sum, pcr_predictions)
mae(PCR_test$energy_sum, pcr_predictions)
# Calculate R-squared
sse <- sum((PCR_test$energy_sum - pcr_predictions)^2)  # Sum of Squared Errors
sst <- sum((PCR_test$energy_sum - mean(PCR_test$energy_sum))^2)  # Total Sum of Squares
r_squared <- 1 - (sse / sst)

##########################################################################################

pcr.fit <- prcomp(~.- energy_sum, data = PCR_train, scale = TRUE)
summary(pcr.fit)

screeplot(pcr.fit, type = "lines", main = "Screeplot of PCR component")

?screeplot

transformtest <- as.data.frame(predict(pcr.fit, PCR_test)[,1:7])
glimpse(transformtest)

pcr.train <-as.data.frame(pcr.fit$x)
pcr.train <- pcr.train[,1:7]

new_pcr_train<- as.data.frame(cbind(PCR_train$energy_sum, pcr.train))
colnames(new_pcr_train)

colnames(new_pcr_train)[1] <- "energy_sum"

# new pcr train and test data from the PCR linear transformations
new_pcr_test <- as.data.frame(cbind(PCR_test$energy_sum, transformtest))
colnames(new_pcr_test)[1] <- "energy_sum"

new_pcr_train<-new_pcr_train

pcr_lm <- lm(energy_sum~., data = new_pcr_train)
summary(pcr_lm)

new_pcr_pred <- predict(pcr_lm, new_pcr_test)

rmse(new_pcr_test$energy_sum, new_pcr_pred)
mse(new_pcr_test$energy_sum, new_pcr_pred)
