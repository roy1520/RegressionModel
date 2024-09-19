library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Dataset
set.seed(123) # For reproducibility
data <- tibble(
  trip_duration = rnorm(100, mean = 20, sd = 5), # Target variable
  distance = rnorm(100, mean = 5, sd = 2),       # Predictor variable
  time_of_day = sample(c("morning", "afternoon", "evening"), 100, replace = TRUE) # Categorical variable
)

# Factor declaration
data <- data %>%
  mutate(time_of_day = as.factor(time_of_day))

# Data preprocessing
data <- data %>%
  drop_na() %>% # Remove rows with NA values
  mutate(across(where(is.character), as.factor)) # Convert character columns to factors

# Building a linear regression model
model <- lm(trip_duration ~ distance + time_of_day, data = data)

#Check
summary(model)

# Prediction check (model)
predictions <- predict(model, newdata = data)

# Appying to the original data
data$predictions <- predictions

# Prediction check (data)
head(data)

# Best fit line
ggplot(data, aes(x = distance, y = trip_duration)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Trip Duration vs. Distance",
       x = "Distance",
       y = "Trip Duration") +
  theme_minimal()

# Error Plot
ggplot(data, aes(x = trip_duration, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "green") +
  labs(title = "Predictions vs. Actual Values",
       x = "Actual Trip Duration",
       y = "Predicted Trip Duration") +
  theme_minimal()


