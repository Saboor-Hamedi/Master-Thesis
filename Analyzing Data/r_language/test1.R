setwd("A:/MA. Program/Semester 4/Thesis/Analyzing Data/r_language")
library(readxl)
library(stats)
library(knitr)

mydata <- read_excel("education_1.xlsx")

model <- glm(label ~ sex, data = mydata, family = binomial(link = "logit"))

coefficients <- coef(model)
std_errors <- sqrt(diag(vcov(model)))
z_values <- coefficients / std_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Create a data frame to store the results
result <- data.frame(Coefficient = coefficients,
                     Std_Error = std_errors,
                     Z_Value = z_values,
                     P_Value = p_values)

# Print the result table using knitr::kable()
print(kable(result, format = "markdown"))
