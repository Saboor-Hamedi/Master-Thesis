# Load the required libraries
library(readxl)
library(stats)

# Set the working directory
setwd("A:/MA. Program/Semester 4/Thesis/Analyzing Data/r_language")

# Read the data
mydata <- read_excel("education_1.xlsx")

# Perform logistic regression
model <- glm(label ~ sex, data = mydata, family = binomial)

# Print the summary of the logistic regression model
summary(model)
