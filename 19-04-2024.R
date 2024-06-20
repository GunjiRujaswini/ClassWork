N <-100000
a <-rnorm(N)
b <- rnorm(N)
c<- a * b
c
# Generate 5 random numbers from a standard normal distribution
X <- rnorm(5)
print("Random numbers from a standard normal distribution (mean=0, sd=1):")
plot(X)
hist(X)

# Generate 5 random numbers with mean 10 and sd 1
X <- rnorm(5, 10, 1)
print("Random numbers from N(mean=10, sd=1):")
plot(X)
hist(X)

# Another method to specify mean without explicitly stating it
X <- rnorm(5, 10)
print("Random numbers from N(mean=10, sd=1) using shorthand:")
plot(X)
hist(X)

# Explicitly specifying mean and sd
X <- rnorm(5, mean=10, sd=1)
print("Random numbers from N(mean=10, sd=1) with named arguments:")
plot(X)
hist(X)

# Reversing the order of named arguments
X <- rnorm(5, sd=1, mean=10)
plot(X)
hist(X)

# Specify only sd with default mean = 0
X <- rnorm(5, sd=1)
print("Random numbers from N(mean=0, sd=1) with explicit sd only:")
plot(X)
hist(X)

# Final print statement after all reassignments
print("Final content of X:")
plot(X)
hist(X)

# Generate the vector of numbers
x <-rnorm(100000)
# count how many are between -1 and 1
count <- 0
for (i in x)
if (i>-1 & i<1)
  count <- count+1
# Get and print the percentage, should be around 68%
percent <- count/length(x)
percent
x <-rnorm(80000)
plot(x)
hist(x)
head(x)
tail(x)
range(x)
summary(x)
boxplot(x)
