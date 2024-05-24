# Task and inputs
# Nadja Pfister

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

#_______________________________________________________________________________

# Task 1

# 1. BMI Calculation Function
# BMI = weight (kg) / (height (m) ^ 2)
calculate_bmi <- function(weight, height) {
  if (height <= 0 || weight <= 0) {
    stop("Height and weight must be positive values.")
  }
  bmi <- weight / (height ^ 2)
  return(bmi)
}

# 2. Celsius to Fahrenheit Conversion Function
# F = C * 9/5 + 32
celsius_to_fahrenheit <- function(celsius) {
  fahrenheit <- (celsius * 9/5) + 32
  return(fahrenheit)
}

# 3. Euclidean Distance Calculation Function
# distance = sqrt((x2 - x1)^2 + (y2 - y1)^2)
calculate_distance <- function(x1, y1, x2, y2) {
  distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(distance)
}

# Input for Function
# BMI
bmi <- calculate_bmi(weight = 55, height = 1.67)
print(paste("BMI:", bmi))

# Celsius to Fahrenheit
fahrenheit <- celsius_to_fahrenheit(celsius = 16)
print(paste("Fahrenheit:", fahrenheit))

# Euclidean Distance
distance <- calculate_distance(x1 = 0, y1 = 0, x2 = 3, y2 = 4)
print(paste("Distance:", distance))

#_______________________________________________________________________________

# Task 2

# Define the file path
file_path <- "C:/_Data/Master/PaT_24/week_5/Data/wildschwein_BE_2056.csv"

# Import the CSV file
wild_boar_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Display the first few rows and column names to understand the structure
head(wild_boar_data)
colnames(wild_boar_data)

# Convert the timestamp to POSIXct type
wild_boar_data$DatetimeUTC <- as.POSIXct(wild_boar_data$DatetimeUTC, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Filter the data for individuals Rosa and Sabi and for the timespan 01.04.2015 - 15.04.2015
filtered_data <- wild_boar_data %>%
  filter(TierName %in% c("Rosa", "Sabi") &
           DatetimeUTC >= as.POSIXct("2015-04-01 00:00:00", tz = "UTC") &
           DatetimeUTC <= as.POSIXct("2015-04-15 23:59:59", tz = "UTC"))

# Display the filtered data
head(filtered_data)

#_______________________________________________________________________________

# Task 3

# Round the minutes of DatetimeUTC to a multiple of 15 (00, 15, 30, 45) and store the values in a new column
filtered_data <- filtered_data %>%
  mutate(DatetimeRound = round_date(DatetimeUTC, unit = "15 minutes"))

# Display the modified data with the new rounded timestamp column
head(filtered_data)

#_______________________________________________________________________________

# Task 4

# Split the filtered_data into separate data.frames for each animal
rosa_data <- filtered_data %>% filter(TierName == "Rosa")
sabi_data <- filtered_data %>% filter(TierName == "Sabi")

# Join the datasets by the new DatetimeRound column
joined_data <- inner_join(rosa_data, sabi_data, by = "DatetimeRound", suffix = c(".Rosa", ".Sabi"))

# Calculate Euclidean distances between concurrent observations and store the values in a new column
joined_data <- joined_data %>%
  mutate(Distance = sqrt((E.Rosa - E.Sabi)^2 + (N.Rosa - N.Sabi)^2))

# Use a threshold on distance to determine if the animals are spatially close enough to constitute a meet
# We use 100 meters as the threshold
joined_data <- joined_data %>%
  mutate(Meet = Distance <= 100)

# Display the final dataset with distance and meet columns
head(joined_data)

#_______________________________________________________________________________

# Task 5

# Filter the joined dataset to only include meets
meets_data <- joined_data %>% filter(Meet == TRUE)

# Create the plot
plot <- ggplot() +
  geom_point(data = rosa_data, aes(x = E, y = N), color = "red", alpha = 0.5) +
  geom_point(data = sabi_data, aes(x = E, y = N), color = "blue", alpha = 0.5) +
  geom_point(data = meets_data, aes(x = (E.Rosa + E.Sabi) / 2, y = (N.Rosa + N.Sabi) / 2), color = "green", size = 2, alpha = 0.7) +
  labs(title = "Spatial Visualization of Wild Boar Meets",
       x = "Easting",
       y = "Northing") +
  xlim(min(c(rosa_data$E, sabi_data$E)) - 100, max(c(rosa_data$E, sabi_data$E)) + 100) +
  ylim(min(c(rosa_data$N, sabi_data$N)) - 100, max(c(rosa_data$N, sabi_data$N)) + 100) +
  theme_minimal()

# Display the plot
print(plot)

#_______________________________________________________________________________

# Task 6

# Create the 3D plot (Space-Time-Cube)
fig <- plot_ly() %>%
  add_trace(data = rosa_data, x = ~E, y = ~N, z = ~DatetimeUTC, type = 'scatter3d', mode = 'lines', name = 'Rosa', line = list(color = 'red')) %>%
  add_trace(data = sabi_data, x = ~E, y = ~N, z = ~DatetimeUTC, type = 'scatter3d', mode = 'lines', name = 'Sabi', line = list(color = 'blue')) %>%
  add_trace(data = meets_data, x = ~((E.Rosa + E.Sabi) / 2), y = ~((N.Rosa + N.Sabi) / 2), z = ~DatetimeRound, type = 'scatter3d', mode = 'markers', name = 'Meets', marker = list(color = 'green', size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Easting'),
                      yaxis = list(title = 'Northing'),
                      zaxis = list(title = 'Time')))

# Display the 3D plot
fig
