#NSSO data regression
library(dplyr)
setwd('D:\\R Studio')

# Loading the dataset
data = read.csv("NSSO68.csv")
unique(data$state_1)
# Subset data to state assigned
subset_data <- data %>%
  filter(state_1 == 'GUJ') %>%
  select(foodtotal_q, MPCE_MRP, MPCE_URP,Age,Meals_At_Home,Possess_ration_card,Education, No_of_Meals_per_day)
print(subset_data)

sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$MPCE_URP))
sum(is.na(subset_data$Age))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(data$Education))

impute_with_mean = function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Columns to impute
columns_to_impute = c("Education")

# Impute missing values with mean
data = impute_with_mean(data, columns_to_impute)

sum(is.na(data$Education))

# Fit the regression model
model = lm(foodtotal_q~ MPCE_MRP+MPCE_URP+Age+Meals_At_Home+Possess_ration_card+Education, data = subset_data)

# Print the regression results
print(summary(model))


library(car)
# Checking for multicollinearity using Variance Inflation Factor (VIF)
vif(model)

# Extracting the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}
# Print the equation
print(equation)



