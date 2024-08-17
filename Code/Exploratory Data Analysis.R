library(tidyverse)
library(ggplot2)
library(gridExtra)



energy_agg <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")
glimpse(energy_agg)

aggg <- energy_agg %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max, -day )
glimpse(aggg)

aggg <- aggg %>% 
  select(-X)

plots <- list()

# Loop through all numeric variables in the dataset
for (col in names(aggg)) {
  if (is.numeric(aggg[[col]])) {
    p <- ggplot(aggg, aes_string(x = col)) +
      geom_histogram(
      aes(y = ..density..), 
        bins = 30, fill = "skyblue", color = "black") +
      geom_density(color = "red") +
     # ggtitle(paste("Histogram of", col)) +
      labs(y = NULL) +
      theme_classic()
    
    # Add the plot to the list
    plots[[col]] <- p
  }
}

# Arrange the plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))  # Adjust ncol to control the number of columns in the grid

# __________________________________________________________________________________-----

energy_agg <-read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\agg_energy.csv")
glimpse(energy_agg)

EDA <- energy_agg %>% 
  select(-energy_median, -energy_mean, -energy_count, -energy_min, -energy_max,-X )

glimpse(EDA)
attach(EDA)


EDA$day <- as.Date(EDA$day)



# Load required packages
library(ggplot2)
library(scales)



# Plot the time series with two variables
ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = temperatureMax, color = "Max. Temperature"), linewidth = 0.9, alpha = 0.8) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 30),  # Set limits for the left axis
  sec.axis = sec_axis(~./0.8, name = "Degree Celcius")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
       ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "Max. Temperature" = "red"))



# Hunidity

ggplot(EDA, aes(x = day)) +
 geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = humidity*10, color = "Humidity"), linewidth = 0.9, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(3, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./2, name = "(dg/m3)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "Humidity" = "blue"))


# pressure 

ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = windSpeed*1, color = "windSpeed"), linewidth = 0.9, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./1.5, name = "(m/s)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "green"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "windSpeed" = "green"))


# UV Index!
ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = uvIndex*scale_factor, color = "UV Index"), linewidth = 1.4, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./scale_factor, name = "(mW/m2)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "orange"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "UV Index" = "orange"))

scale_factor <- max(EDA$energy_sum) / max(EDA$uvIndex)



# Visibility
scale_factor <- max(EDA$energy_sum) / max(EDA$visibility)
# 
ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = visibility*scale_factor, color = "visibility"), linewidth = 1.4, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./scale_factor, name = "(meters)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkorange"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "visibility" = "darkorange"))


# Dewpoint
scale_factor <- max(EDA$energy_sum) / max(EDA$dewPoint)
# 
ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = dewPoint*scale_factor, color = "Dew Point"), linewidth = 1.4, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./scale_factor, name = "(Degree Celcius)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "brown"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "Dew Point" = "brown"))


# Cloud Cover
scale_factor <- max(EDA$energy_sum) / max(EDA$cloudCover)
# 
ggplot(EDA, aes(x = day)) +
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +
  geom_line(aes(y = cloudCover*scale_factor, color = "Cloud Cover"), linewidth = 1.4, alpha = 0.6) +
  # Scale for the two y-axes with limits
  scale_y_continuous(
    name = "(KW/Day)",
    limits = c(0, 16),  # Set limits for the left axis
    sec.axis = sec_axis(~./scale_factor, name = "(Degree Celcius)")  # Adjust scaling as needed
  ) +
  labs(x = "Date",
       color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkblue"),
    axis.title.y.left = element_text(color = "black")
  ) +
  scale_color_manual(values = c("Energy Consumed" = "black", "Cloud Cover" = "darkblue"))





