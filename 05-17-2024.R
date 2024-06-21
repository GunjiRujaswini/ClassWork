library(ggplot2)

# Read a csv file
file_path <- "C:/Users/gruja/Downloads/Boston.csv"
data <- read.csv(file_path)
str(data)

# Linear Regression: Let's assume we are using 'lstat' (percentage of lower status population) to predict 'rm' (median value of owner-occupied homes in $1000s)
model_linear <- lm(rm ~ lstat, data=data)


# Predictions
data$pred_linear <- predict(model_linear)

# Plotting
plot(data$lstat, data$rm, main="Linear Regression: LSTAT vs rm",
     xlab="LSTAT", ylab="RM", col="blue", pch=16)
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

# Create a sequence of ages for prediction
age_seq <- seq(min(data$age), max(data$age), length.out = 100)

# Make predictions using the logistic regression model
pred_prob <- predict(model_logistic, newdata = data.frame(age = age_seq), type = "response")

# Convert predicted probabilities to predicted classes
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Plotting
plot(data$age, data$medv_high, main = "Logistic Regression: AGE vs MEDV_HIGH",
     xlab = "AGE", ylab = "MEDV_HIGH", col = "blue", pch = 16)
lines(age_seq, pred_prob, col = "red", lwd = 2)
legend("topright", legend = c("Actual data", "Predicted probability"), col = c("blue", "red"), pch = c(16, NA), lwd = c(NA, 2))



