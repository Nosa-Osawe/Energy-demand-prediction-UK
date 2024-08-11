# Load necessary libraries
library(tidyverse)

# Specify the folder containing the CSV files
Energy_Daily <- "C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\daily_dataset\\daily_dataset"


Daily_csv_files <- list.files(path = Energy_Daily, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into one data frame
combined_Daily_data <- Daily_csv_files %>%
  lapply(read.csv) %>%   # Read each CSV file
  bind_rows()            # Combine them by rows into one data frame

# View the combined data frame
head(combined_Daily_data)
view(combined_Daily_data)

sum(is.na(combined_Daily_data))

summary_na <- list(
  total_na = sum(is.na(combined_Daily_data)),
  na_per_column = colSums(is.na(combined_Daily_data))
)
print(summary_na)


summary_na$na_per_column

# ------------------------------------------------------------------------------------------------------------------------------------------------------

Energy_Hourly <-"C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\halfhourly_dataset\\halfhourly_dataset"
Hourly_csv_files <- list.files(path = Energy_Hourly, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into one data frame
combined_hourly_data <- Hourly_csv_files %>%
  lapply(read.csv) %>%   # Read each CSV file
  bind_rows()            # Combine them by rows into one data frame

# View the combined data frame
head(combined_hourly_data)

length(combined_hourly_data$LCLid)
colnames(combined_hourly_data)

sum(is.na(combined_hourly_data))   # No NA found

# ------------------------------------------------------------------------------------------------------------------------------------------------------

hhblocks <- "C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\hhblock_dataset\\hhblock_dataset"


hhblocks_files <- list.files(path = hhblocks, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into one data frame
hhblocks_data <- hhblocks_files %>%
  lapply(read.csv) %>%   # Read each CSV file
  bind_rows()            

# View the combined data frame
head(hhblocks_data)

colnames(hhblocks_data)

sum(is.na(hhblocks_data))   # No NA found
length(hhblocks_data$hh_0)
# ------------------------------------------------------------------------------------------------------------------------------------------------------

head(combined_Daily_data)



Daily_energy <- combined_Daily_data %>% 
  select(LCLid, day, energy_mean, energy_sum)  # selecting the most useful columns

head(Daily_energy)
colnames(Daily_energy)
length(unique(Daily_energy$LCLid))


 Daily_energy %>% 
   group_by( day) %>% 
   summarise(
     mean_sum= mean(energy_mean),
     total_energy= sum(energy_sum)
   ) %>% 
   head()


