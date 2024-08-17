
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



# Load required packages
library(ggplot2)
library(scales)

# Create the time series plot
# Ensure 'date' is in date format
EDA$date <- as.Date(EDA$date)

# Plot the time series with two variables
ggplot(EDA, aes(x = day)) +
  # Plot energy_sum
  geom_line(aes(y = energy_sum, color = "Energy Consumed"), linewidth = 1) +

  geom_line(aes(y = temperatureMax, color = "Temperature"), linewidth = 0.7) +
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
  scale_color_manual(values = c("Energy Consumed" = "black", "Temperature" = "red"))

max(EDA$energy_sum)
max(temperatureMax)
