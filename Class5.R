# Install necessary packages if not already installed
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

# Load the required libraries
library(ggplot2)
library(gridExtra)

# Read the CSV file for income data
inc <- read.csv("C:/Users/gruja/Downloads/Income.csv")

# Print the first few rows of the income data
head(inc)

# Create scatter plots to explore relationships between variables
# Age vs. Income
ggplot(inc, aes(x = Age, y = Income)) +
  geom_point() +
  labs(title = "Age vs. Income")

# Education vs. Income
ggplot(inc, aes(x = Education, y = Income)) +
  geom_point() +
  labs(title = "Education vs. Income")

# Gender vs. Income
ggplot(inc, aes(x = Gender, y = Income)) +
  geom_point() +
  labs(title = "Gender vs. Income")

# Fit a linear regression model with all variables
results <- lm(Income ~ Age + Education + Gender, data = inc)

# Print the model results
print(results)

# Print the summary of the model
summary(results)

# Remove Gender variable and fit a linear regression model
results2 <- lm(Income ~ Age + Education, data = inc)
print(results2)

# Generate data for linear relation
x <- runif(75, 0, 10)
x <- sort(x)
y <- 20 + 10 * x + rnorm(75, 0, 10)

# Plot the data
plot(x, y)

# Perform linear regression
lr <- lm(y ~ x)
print(lr)

# Draw the linear regression line
points(x, lr$coefficients[1] + lr$coefficients[2] * x, type = "l", col = 4)

# Generate data for non-linear relation
x <- runif(75, 0, 10)
x <- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75, 0, 20)

# Plot the data
plot(x, y)

# Perform linear regression
lr <- lm(y ~ x)
print(lr)

# Draw the linear regression line
points(x, lr$coefficients[1] + lr$coefficients[2] * x, type = "l", col = 4)

# Polynomial regression for non-linear data
poly <- loess(y ~ x)
fit <- predict(poly)

# Plot the data and regression line
plot(x, y)
points(x, fit, type = "l", col = 2)
points(x, lr$coefficients[1] + lr$coefficients[2] * x, type = "l", col = 4)

# Read the CSV file for churn data
churn <- read.csv("C:/Users/gruja/Downloads/churn.csv")

# Print the first few rows of churn data
head(churn)

# Count the number of churned instances
print(sum(churn$Churned))

# Explore churned customers
ch <- churn[churn$Churned == 1,]
ggplot(ch, aes(x = Churned_contacts)) +
  geom_histogram()

ggplot(ch, aes(x = Age)) +
  geom_histogram()

ggplot(ch, aes(x = Married)) +
  geom_histogram()

ggplot(ch, aes(x = Cust_years)) +
  geom_histogram()

# Explore non-churned customers
nch <- churn[churn$Churned == 0,]
ggplot(nch, aes(x = Churned_contacts)) +
  geom_histogram()

ggplot(nch, aes(x = Age)) +
  geom_histogram()

ggplot(nch, aes(x = Married)) +
  geom_histogram()

ggplot(nch, aes(x = Cust_years)) +
  geom_histogram()

# Logistic regression for churn prediction
Churn_logistic1 <- glm(Churned ~ Age + Married + Cust_years + Churned_contacts,
                       data = churn, family = binomial(link = "logit"))
print(Churn_logistic1)
summary(Churn_logistic1)

# Simplified logistic regression model
Churn_logistic2 <- glm(Churned ~ Age + Churned_contacts,
                       data = churn, family = binomial(link = "logit"))
print(Churn_logistic2)
summary(Churn_logistic2)

# Read demographics data using two different methods
stats <- read.csv("C:/Users/gruja/Downloads/Demographics.csv")
# Alternatively, set working directory
# setwd("C:/Users/gruja/OneDrive/Desktop/")
# stats <- read.csv("Demographics.csv")

# Explore the demographics data
nrow(stats) # number of rows
ncol(stats) # number of columns
head(stats) # first 6 rows
tail(stats) # last 6 rows
str(stats) # structure

# Summary statistics
summary(stats)

# Accessing data using $ sign
stats[3, 3] # row 3, column 3
stats[3, "Birth.rate"]
stats$Internet.users
stats[, "Internet.users"]
stats$Internet.users[3]
levels(stats$Income.Group)
summary(stats$Birth.rate)

# Creating new columns
stats$MyCalc <- stats$Birth.rate * stats$Internet.users
stats$xyz <- 1:5
stats$xyz <- NULL # Remove column
stats$MyCalc <- NULL

# Data filtering
stats[stats$Internet.users < 2,]
stats[stats$Birth.rate > 40 & stats$Internet.users < 2,]
stats[stats$Income.Group == "High income", ]
stats[stats$Country.Name == "Egypt", ]

# QuickPlot (qplot)
# One variable
ggplot(stats, aes(x = Birth.rate)) +
  geom_histogram()

ggplot(stats, aes(x = Internet.users)) +
  geom_histogram()

ggplot(stats, aes(x = Internet.users)) +
  geom_histogram(bins = 10)

# Two variables
ggplot(stats, aes(x = Income.Group, y = Birth.rate)) +
  geom_point()

ggplot(stats, aes(x = Income.Group, y = Internet.users)) +
  geom_point()

# Different sizes
ggplot(stats, aes(x = Income.Group, y = Birth.rate)) +
  geom_point(size = 10)

# Different colors
ggplot(stats, aes(x = Income.Group, y = Birth.rate, color = "blue")) +
  geom_point()

# Different types of charts
ggplot(stats, aes(x = Income.Group, y = Birth.rate)) +
  geom_boxplot()

ggplot(stats, aes(x = Income.Group, y = Birth.rate)) +
  geom_point()

# Creating a Data Frame from Vectors
Countries_2012_Dataset <- c("USA", "Canada", "UK", "Germany", "France")
Codes_2012_Dataset <- c("US", "CA", "UK", "DE", "FR")
Regions_2012_Dataset <- c("North America", "North America", "Europe", "Europe", "Europe")

# Create data frame
cont <- data.frame(Country = Countries_2012_Dataset, 
                   Code = Codes_2012_Dataset, 
                   Region = Regions_2012_Dataset)
print(cont)

# Merging Two Data Frames
merged <- merge(stats, cont, by.x = "Country.Code", by.y = "Code")
merged$Country <- NULL
print(merged)

# Plotting with ggplot
ggplot(merged, aes(x = Internet.users, y = Birth.rate, color = Region)) +
  geom_point()

