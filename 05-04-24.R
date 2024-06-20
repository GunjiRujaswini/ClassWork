
# addition operation
A <-5
B <-10
C <- A +B
C

# operations with variables
v1 <- 4.5
v2 <- 2
v3 <- v1 / v2
v3
v4 <- sqrt(v1)
v4

# for a text messages 
greet <-"Hello,"
name <-"Bob"
message <- paste(greet, name)
message

#Integers just to give a try
## Assigning a integer-point value to a variable
x <- 2L
typeof(x)
# Printing the value
print(x)

# Assigning a floating-point value to a variable
y1 <- 3.14
# Printing the value
print(y1)

# Assigning a double-point value to a variable
y <- 2.5
typeof(y)
# Printing the value
print(y)

#Assigning a complex-point value to a variable
z <- 2+3i
typeof(z)
# Printing the value
print(z)

#Assigning a character-point value to a variable
a <- "h"
typeof(a)
# Printing the value
print(a)

#Assigning a logical-point value to a variable
q <- T
typeof(q)
p <- F
typeof(p)
# Printing the value
print(p)
print(q)

# basics to learn R
a <- 5
b <- 3
c <- a + b
d <- a - b
e <- a * b
f <- a / b
a
b
c
d
e
f
# Printing the value
print(a)
print(b)
print(c)
print(d)
print(e)
print(f)


# logics to learn R
4 <5
10>100
4==5
result <- 4 < 5
result
typeof(result)

# another example
result <- 4 < 5
result
typeof(result)
result2 <- !(5>1)
result2
result | result2
result & result2
isTRUE(result)

#Assignment1

#Harsharaj Challa
# Define the numbers
numbers <- c(7, 12, 5)

# Calculate the average
average <- mean(numbers)

# Print the average
print(paste("The average of the numbers is:", average))

first_name <- "Harsharaj"
# Print the welcome message
print(paste("Welcome to R,",first_name))

# Define the variables
x <- 7
y <- 12
z <- 4

# Check if x is bigger than y and z
if (x > y) {
  print("x is bigger than y")
} else {
  print("x is not bigger than y")
}

if (x > z) {
  print("x is bigger than z")
} else {
  print("x is not bigger than z")
}
