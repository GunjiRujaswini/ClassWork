#######################################################
# Section 5.5 An Example: Transactions in a Grocery Store
#######################################################

# Install necessary packages if not already installed
if (!requireNamespace("arules", quietly = TRUE)) {
  install.packages("arules")
}
if (!requireNamespace("arulesViz", quietly = TRUE)) {
  install.packages("arulesViz")
}

# Load the required libraries
library(arules)
library(arulesViz)

##########################################
# Section 5.5.1 The Groceries Dataset
##########################################

# Load the Groceries dataset
data(Groceries)

# Display summary of the Groceries dataset
summary(Groceries)

# Display the class of the Groceries dataset
class(Groceries)

# Display the first 20 grocery labels
Groceries@itemInfo[1:20,]

# Display the 10th to 20th transactions
apply(Groceries@data[, 10:20], 2, 
      function(r) paste(Groceries@itemInfo[r, "labels"], collapse=", "))

##########################################
# Section 5.5.2 Frequent Itemset Generation
##########################################

# Frequent 1-itemsets
itemsets1 <- apriori(Groceries, parameter=list(minlen=1, maxlen=1, support=0.02, target="frequent itemsets"))
summary(itemsets1)
inspect(head(sort(itemsets1, by="support"), 10))

# Frequent 2-itemsets
itemsets2 <- apriori(Groceries, parameter=list(minlen=2, maxlen=2, support=0.02, target="frequent itemsets"))
summary(itemsets2)
inspect(head(sort(itemsets2, by="support"), 10))

# Frequent 3-itemsets
itemsets3 <- apriori(Groceries, parameter=list(minlen=3, maxlen=3, support=0.02, target="frequent itemsets"))
inspect(sort(itemsets3, by="support"))

# Run Apriori without setting the maxlen parameter
itemsets_all <- apriori(Groceries, parameter=list(minlen=1, support=0.02, target="frequent itemsets"))

##########################################
# Section 5.5.3 Rule Generation and Visualization
##########################################

# Generate association rules
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.6, target="rules"))
summary(rules)

# Plot the rules
plot(rules, method="graph", control=list(type="items"))

# Filter rules with high confidence
confidentRules <- rules[quality(rules)$confidence > 0.9]

# Plot confident rules using a matrix plot
plot(confidentRules, method="matrix", measure=c("lift", "confidence"))

# Display rules with top lift scores
inspect(head(sort(rules, by="lift"), 10))

