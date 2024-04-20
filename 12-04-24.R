#Harsharaj Challa

# Initialize variables
x <- -1
count <- 0

# if x is between -1 and 1
if (x >= -1 & x <= 1) {
  count <- count + 1  # Increment count if condition is true
} else {
  count <- count - 1  # Decrement count if condition is false
}
print(count)  # Output: 1

# WHILE LOOP

counter <- 1

# Loop through until counter reaches 12
while (counter < 12) {
  # Print the value of counter
  print(counter)
  # Increment counter by 1
  counter <- counter + 1
}

# FOR LOOP

# Loop through numbers from 1 to 5
for (i in 1:5) {
  # Print "Hello R" 5 times
  print("Hello R")
}


# Loop through numbers from 5 to 10
for (m in 5:10) {
  # Print the value of m
  print(m)
}


# Program to display square roots of numbers from 1 to 20

# Loop through numbers from 1 to 20
for (i in 1:20) {
  # Calculate the square root of each number
  square_root <- sqrt(i)
  # Print the square root
  cat("Square root of", i, "is", square_root, "\n")
}

# CREATING VECTORS

# Define vectors using various methods
myFirstVector <- c(2, 5, 7, 1, 4)
V <- 1:10
S <- -5:30
T <- 10:1
Mix <- c(3:6, 24, 0:4, 31, 9:7)

# Print vectors
print(myFirstVector)  # Output: 2 5 7 1 4
print(typeof(myFirstVector))  # Output: double
print(V)  # Output: 1 2 3 4 5 6 7 8 9 10
print(S)  # Output: -5 -4 -3 -2 -1 0 1 2 3 4 5 6 ... 30
print(T)  # Output: 10 9 8 7 6 5 4 3 2 1
print(Mix)  # Output: 3 4 5 6 24 0 1 2 3 4 31 9 8 7

# Creating vectors using seq function
A <- seq(1, 10)
B <- seq(1, 10, by = 2)
C <- seq(5, -5, by = -2)
D <- seq(1, 10, length = 20)
E <- seq(0, 1, length = 15)

# Print vectors
print(A)  # Output: 1 2 3 4 5 6 7 8 9 10
print(B)  # Output: 1 3 5 7 9
print(C)  # Output: 5 3 1 -1 -3 -5
print(D)  # Output: 1 1.473684 1.947368 2.421053 2.894737 ... 10
print(E)  # Output: 0 0.07142857 0.1428571 0.2142857 ... 1

# Creating vectors using rep function
A <- rep("Hello", 5)
B <- rep("A", 2)
C <- rep(1:5, 4)
D1 <- rep(c("A", "B", "C"), 3)
D2 <- rep(c("A", "B", "C"), each = 3)
D3 <- rep(c("A", "B", "C"), c(2, 1, 3))

# Print vectors
print(A)  # Output: "Hello" "Hello" "Hello" "Hello" "Hello"
print(B)  # Output: "A" "A"
print(C)  # Output: 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
print(D1)  # Output: "A" "B" "C" "A" "B" "C" "A" "B" "C"
print(D2)  # Output: "A" "A" "A" "B" "B" "B" "C" "C" "C"
print(D3)  # Output: "A" "A" "B" "C" "C" "C" "C"

# Convert mixed-mode vector to single mode
T <- c("Hello", 3, 67L, 6.8)
typeof(T)  # R converts them to a single mode (char)

# Vector Attributes
X <- c(3, 5, 8)
typeof(X)
length(X)

# Displaying partial vectors
vectorsw <- c("a", "b", "c", "d", "e")
vectorsw[1]
vectorsw[3]
vectorsw[-1]  # all except number 1
vectorsw[-3]
vectorsw[c(1, 3, 5)]  # get specific items
vectorsw[c(-2, -4)]  # same thing
vectorsw[c(-3:-5)]  # remove range 3 to 5
vectorsw[1:3]  # show 1 to 3

# Sorting a vector
A <- c(5, -2, 51, 42, -76, 42)
B <- sort(A)
A
B

# Arithmetic

# Addition of vectors
X <- c(1, 2, 3)
Y <- c(4, 5, 6)
Z <- X + Y
X
Y
Z

# Different length vectors addition
X <- c(1, 2, 3)
Y <- c(4, 5, 6, 7, 8, 9)
Z <- X + Y
X
Y
Z

# Calculate statistical measures of a vector
X <- c(34, 56, 22, 11, 65, 45)
Y <- mean(X)  # Mean
Z <- min(X)  # Minimum
W <- max(X)  # Maximum
S <- sum(X)  # Sum
V <- prod(X)  # Product
print(Y)
print(Z)
print(W)
print(S)
print(V)
# or
Y
Z
W
S
V

# Print numbers less than 50 from a vector
Tax <- c(32, 56, 44, 76, 89, 24)
for (i in 1:6)
  if (Tax[i] < 50)
    print(i)
for (i in Tax)
  if (i < 50)
    print(i)

# Define the temperatures in Fahrenheit
temperatures_fahrenheit <- c(45, 77, 20, 19, 101, 120, 212)

# Function to convert Fahrenheit to Celsius
fahrenheit_to_celsius <- function(temp_fahrenheit) {
  return ((temp_fahrenheit - 32) * 5 / 9)
}

# Convert temperatures from Fahrenheit to Celsius
temperatures_celsius <- fahrenheit_to_celsius(temperatures_fahrenheit)

# Display converted temperatures
cat("Temperatures in Celsius:", temperatures_celsius, "\n")

# Calculate and display highest, lowest, and average temperatures in Fahrenheit
highest_temp <- max(temperatures_fahrenheit)
lowest_temp <- min(temperatures_fahrenheit)
average_temp <- mean(temperatures_fahrenheit)

cat("Highest temperature in Fahrenheit:", highest_temp, "\n")
cat("Lowest temperature in Fahrenheit:", lowest_temp, "\n")
cat("Average temperature in Fahrenheit:", average_temp, "\n")