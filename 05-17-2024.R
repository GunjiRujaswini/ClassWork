# Install and load necessary libraries
install.packages("tidyverse")
install.packages("caret")
install.packages("e1071")  # Required for caret package dependency

library(tidyverse)
library(caret)

# Set the working directory to where the file is located
setwd("C:/Users/gruja/Downloads")  # Change this to your file path

# Load the dataset
boston <- read.csv("boston.csv")

# View the first few rows of the dataset
head(boston)

# Check for missing values
sum(is.na(boston))

# Summary of the dataset
summary(boston)

# Scatter plot of MEDV vs RM
ggplot(boston, aes(x = RM, y = MEDV)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter plot of MEDV vs RM",
       x = "Average number of rooms per dwelling (RM)",
       y = "Median value of owner-occupied homes in $1000s (MEDV)")

# Correlation matrix
correlation_matrix <- cor(boston)
print(correlation_matrix)

# Building the linear regression model
linear_model <- lm(MEDV ~ ., data = boston)

# Summary of the linear regression model
summary(linear_model)

# Predicting the MEDV values
predictions <- predict(linear_model, boston)

# Calculating RMSE
rmse <- sqrt(mean((boston$MEDV - predictions)^2))
print(paste("RMSE: ", rmse))

# Plot actual vs predicted values
ggplot(boston, aes(x = MEDV, y = predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  labs(title = "Actual vs Predicted MEDV",
       x = "Actual MEDV",
       y = "Predicted MEDV")

# Optional: Polynomial regression model (e.g., adding a quadratic term for RM)
poly_model <- lm(MEDV ~ poly(RM, 2) + . - RM, data = boston)

# Summary of the polynomial regression model
summary(poly_model)

# Predicting the MEDV values with the polynomial model
poly_predictions <- predict(poly_model, boston)

# Calculating RMSE for the polynomial model
poly_rmse <- sqrt(mean((boston$MEDV - poly_predictions)^2))
print(paste("Polynomial RMSE: ", poly_rmse))
