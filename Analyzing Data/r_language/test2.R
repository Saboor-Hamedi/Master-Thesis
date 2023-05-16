library(readxl)
library(broom)
library(knitr)
library(kableExtra)
library(ggplot2)

# Set the working directory
setwd("A:/MA. Program/Semester 4/Thesis/Analyzing Data/r_language")
# Load your data
dataset <- read_excel("education_1.xlsx")

dataset$label <- factor(dataset$label, levels = c(0, 1))
# Perform logistic regression
logit_model <- glm(label ~ sex, data = dataset, family = binomial)
predictions <- predict(logit_model, type = "response")
# Extract coefficients, standard errors, z-values, and p-values

predictions_df <- data.frame(sex = dataset$sex, predicted_prob = predictions)

coefficients <- coef(logit_model)
std_errors <- sqrt(diag(vcov(logit_model)))
z_values <- coefficients / std_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Create a data frame for the coefficients
coefficients_df <- data.frame(
  Coefficient = coefficients,
  Std_Error = std_errors,
  z_Value = z_values,
  P_Value = p_values
)
print(coefficients_df)



#chart, predicted probabilities
ggplot(predictions_df, aes(x = sex, y = predicted_prob)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Sex", y = "Predicted Probability") +
  ggtitle("Logistic Regression: Predicted Probabilities") +
  theme_minimal()
