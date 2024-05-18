library(ggplot2)

# Read a csv file
file_path <- "C:/Users/gruja/Downloads/Boston.csv"
data <- read.csv(file_path)
str(data)

# Linear Regression: Let's assume we are using 'lstat' (percentage of lower status population) to predict 'medv' (median value of owner-occupied homes in $1000s)
model_linear <- lm(medv ~ lstat, data=data)


# Predictions
data$pred_linear <- predict(model_linear)

# Plotting
plot(data$lstat, data$medv, main="Linear Regression: LSTAT vs MEDV",
     xlab="LSTAT", ylab="MEDV", col="blue", pch=16)
lines(data$lstat, data$pred_linear, col="red", lwd=2)
legend("topright", legend=c("Actual data", "Fitted line"), col=c("blue", "red"), pch=c(16, NA), lty=c(NA, 1))

# Polynomial Regression: Using 'lstat' to predict 'medv' with a degree 3 polynomial
model_poly <- lm(medv ~ poly(lstat, 3), data=data)  # Degree 3 polynomial

# Predictions
data <- data[order(data$lstat), ]  # Order the data by lstat
data$pred_poly <- predict(model_poly, newdata=data)

# Plotting
plot(data$lstat, data$medv, main="Polynomial Regression: LSTAT vs MEDV",
     xlab="LSTAT", ylab="MEDV", col="blue", pch=16)
lines(data$lstat, data$pred_poly, col="red", lwd=2)
legend("topright", legend=c("Actual data", "Fitted polynomial"), col=c("blue", "red"), pch=c(16, NA), lty=c(NA, 1))

# Create a binary variable 'medv_high' (1 if medv > median, else 0)
data$medv_high <- ifelse(data$medv > median(data$medv), 1, 0)

# Logistic Regression: Using 'lstat' to predict 'medv_high'
model_logistic <- glm(medv_high ~ lstat, data=data, family=binomial)

# Predictions
data$pred_logistic <- predict(model_logistic, type="response")
data$pred_class <- ifelse(data$pred_logistic > 0.5, 1, 0)

# Plotting
plot(data$lstat, data$medv_high, main="Logistic Regression: LSTAT vs MEDV_HIGH",
     xlab="LSTAT", ylab="MEDV_HIGH", col="blue", pch=16)
points(data$lstat, data$pred_class, col="red", pch=4)
legend("topright", legend=c("Actual data", "Predicted class"), col=c("blue", "red"), pch=c(16, 4))

# Check the structure of the data
str(data)
summary(data)

