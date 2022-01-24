# Title: Market Basket Analysis of Electronidex
# Last update: 2021.12
# File: Market_Basket_Analysis.R
# Project name: Market Basket Analysis of Electronidex

################
# Load packages
################
install.packages("arules")
install.packages("arulesViz")
install.packages("Matrix")

library("arules")
library("arulesViz")
library("Matrix")

###############
# Upload Data
###############
Tr <- read.transactions('ElectronidexTransactions2017.csv', 
                        format = 'basket', sep=',')
Tr
summary(Tr)


###################
# Evaluate the data
###################
inspect (Tr[1])  # View certain # of transactions
length (Tr)  # Number of transactions.
size (Tr)  # Number of items per transaction
LIST(Tr[1])  # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Tr) # To see the item labels

########################
# Visualize Your Dataset
########################
itemFrequencyPlot(Tr, topN=20, type='absolute')  # visualize the item frequencies
image(sample(Tr, 100))  # visualize certain number of the transactions

#############################
# Create some rules
# Apply the Apriori Algorithm
#############################
rules <- apriori(Tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1:5])
iMac_rules <- subset(rules, items %in% "iMac")  #see a certain item's rules
inspect(iMac_rules[1:10])  
is.redundant(rules)  # Check if there's any redundant rules
is.redundant(iMac_rules) 

###########################
# Visualization
###########################
plot(rules[1:50], method="graph", control=list(type="items")) 
plot(iMac_rules[1:50], method="graph", control=list(type="items")) 
