 
library(tidyverse)


Energy_Daily <- "C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\daily_dataset\\daily_dataset"


Daily_csv_files <- list.files(path = Energy_Daily, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into one data frame
combined_Daily_data <- Daily_csv_files %>%
  lapply(read.csv) %>%   # Read each CSV file
  bind_rows()            # Combine them by rows into one data frame

# View the combined data frame
head(combined_Daily_data)
# view(combined_Daily_data)

sum(is.na(combined_Daily_data))

          # create a list of total NA and NA's per column
summary_na <- list(
  total_na = sum(is.na(combined_Daily_data)),
  na_per_column = colSums(is.na(combined_Daily_data))
)
print(summary_na)


combined_Daily_data <- combined_Daily_data %>% 
  select(-energy_std)

combined_Daily_data <- na.omit(combined_Daily_data)
sum(is.na(combined_Daily_data)) # super clean!!

# ---------------------------------------------------------------------------------------------------------
## weather data
weather <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\weather_daily_darksky.csv")
head(weather)



weather_date <- as.Date(weather$temperatureMaxTime)
head(weather_date)

weather <- cbind(weather_date,weather)


length(unique(weather$weather_date))

summary_weather <- weather %>% 
  group_by(weather_date) %>% 
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) 


sum(is.na(summary_weather))

rows_with_na <- summary_weather %>%
  filter(if_any(everything(), is.na))

colSums(is.na(summary_weather)) # for each column, find the number of NAs

summary_weather <- na.omit(summary_weather)    # Perfect!

#  ---------------------------------------------------------------------------------------------------------------


weather_categorical <- weather %>% 
  select(weather_date, icon, precipType,summary)
head(weather_categorical)

# Lets LEFT join summary_weather to weather_categorical using the weather_date

weather_categorical_unique <- weather_categorical %>%
  distinct(weather_date, .keep_all = TRUE)

combined_Weather <- summary_weather %>%
  left_join(weather_categorical_unique, by = "weather_date")


# View the resulting data frame
head(combined_Weather)

sum(is.na(combined_Weather))
length(unique(combined_Weather$weather_date))


# -- ----------------------- Back to daily energy data-------------------------------------------------

glimpse(combined_Daily_data)


combined_Daily_data$day <- as.Date(combined_Daily_data$day)

combined_Weather <- combined_Weather %>% 
  rename(day = weather_date)   

glimpse(combined_Weather)


Weather_energy <- combined_Daily_data %>%
  left_join(combined_Weather, by = "day")

glimpse(Weather_energy)


# create a list of total NA and NA's per column
Total_summary_na <- list(
  total_na = sum(is.na(Weather_energy)),
  na_per_column = colSums(is.na(Weather_energy)),
  na_per_row = rowSums(is.na(Weather_energy))
)
print(Total_summary_na)


glimpse(Weather_energy)

Weather_energy %>%  # count the number of rows with at least one NA
  filter(if_any(everything(), is.na)) %>% 
  nrow()   # just 57513 bunkums! Ha HA!

Weather_energy_clean<-na.omit(Weather_energy)

sum(is.na(Weather_energy_clean))  # so clean
glimpse(Weather_energy_clean)

house_info <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\informations_households.csv")
glimpse(house_info)

house_info_list <- list(
  stdorToU = unique(house_info$stdorToU),
  Acorn = unique(house_info$Acorn),
  acorn_group = unique(house_info$Acorn_grouped),
  file = unique(house_info$file)
)
print(house_info_list)


house_info %>%  
  group_by(Acorn_grouped) %>% 
  summarise(count = n())  # seems they had error in data entry

house_info %>%  
  group_by(Acorn, Acorn_grouped) %>% 
  filter(Acorn=="ACORN-U") %>% 
  summarise(count = n())

house_info %>%  
  group_by(Acorn_grouped, Acorn) %>% 
  summarise(count = n())     ### Looks like the entry was intentional, therefore NO error

length(unique(Weather_energy_clean$LCLid))
length(unique(house_info$LCLid))

# LEFT join the Weather_energy_clean to house_info by LCLid

Energy_weather_house.Info <- Weather_energy_clean %>%
  left_join(house_info, by = "LCLid")

glimpse(Energy_weather_house.Info)

sum(is.na(Energy_weather_house.Info)) # super clean!!
length(unique(Energy_weather_house.Info$LCLid))  # oh Nice!

###################################################################################################### 
# Creating a RF model to predict mean energy use per household 

library(randomForest)
library(caret)

set.seed(123)

# Re-sampling because the data set is too much for my laptop for running ML models

ind2 <- sample(2, nrow(Energy_weather_house.Info), replace = TRUE, prob = c(0.01, 0.99))
EWH <- Energy_weather_house.Info[ind2==1,] #



EWH <-EWH %>% 
  select(day,energy_sum, temperatureMax,windBearing, dewPoint, cloudCover, windSpeed,
         pressure, apparentTemperatureHigh, visibility,humidity, 
         apparentTemperatureLow, apparentTemperatureMax,uvIndex,temperatureLow,temperatureMin,
         temperatureHigh, apparentTemperatureMin, moonPhase, precipType, stdorToU,Acorn_grouped
         
  )




str(EWH)
ind3 <- sample(2, nrow(EWH), replace = TRUE, prob = c(0.7, 0.3))
EWH_train <- EWH[ind3==1,] # train
EWH_test  <- EWH[ind3==2,]  # Test


rf_EWH <- randomForest(energy_sum~. -day, data=EWH_train,
                       ntree = 500,
                        mtry = 5,
                          importance = TRUE,
                          proximity = TRUE)


varImpPlot(rf_EWH,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


# Predict on the test set
predictions.. <- predict(rf_EWH, newdata = EWH_test)

# Actual values
actuals.. <- EWH_test$energy_sum

# Compute Mean Squared Error (MSE)
mse <- mean((predictions.. - actuals..)^2)

# Compute Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Compute Mean Absolute Error (MAE)
mae <- mean(abs(predictions.. - actuals..))

# Compute R-squared (Coefficient of Determination)
ss_total <- sum((actuals.. - mean(actuals..))^2)  # Total sum of squares
ss_res <- sum((actuals.. - predictions..)^2)      # Residual sum of squares
r_squared <- 1 - (ss_res / ss_total)







###############################################################################################################3
# Creating a RF model to predict mean energy use per household using the daily aggregate (mean)

library(randomForest)


agg <- Rf.data %>%
  group_by(day) %>%
  summarise(across(where(is.numeric), 
                   mean, na.rm = TRUE))
str(agg)
view(agg)


set.seed(123)
ind <- sample(2, nrow(agg), replace = TRUE, prob = c(0.7, 0.3))
rf_train <- agg[ind==1,]
rf_test <- agg[ind==2,]

rf_energy <- randomForest(energy_sum~.-day, data=rf_train,
                          ntree = 200,
                          mtry = 8,
                          importance = TRUE,
                          proximity = TRUE)


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










