## 1. Loading Libraries and Dataset file

library(dplyr)
library(ggplot2)
library(tidyverse)

file <- read.csv("/Users/jp/Desktop/ALY6015/AmesHousing.csv")
View(file)
str(file)

## 2. Performing EDA

summary(file)

#install.packages("skimr")
library(skimr)
skim(file)

## 3. Missing values and replacing them

# Replace empty values for numeric columns with the median of the column
file_clean_data <- file %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.) | . == "", 
                         median(as.numeric(na.omit(as.numeric(.))), na.rm = TRUE), 
                         as.numeric(.))))

# Verify the count of empty or missing values after cleaning
empty_or_na_count_1 <- sum(is.na(file))
print(paste("Total Empty or Missing Values before cleaning:", empty_or_na_count_1))
empty_or_na_count_2 <- sum(is.na(file_clean_data))
print(paste("Total empty or missing values after cleaning:", empty_or_na_count_2))

library(tidyverse)
missing_features <- file %>%
  summarise_all(~mean(is.na(.))) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "MissingPercentage") %>%
  filter(MissingPercentage > 0.05)

print(missing_features)

## 4. Correlation Matrix

##install.packages("corrplot")
library(corrplot)
library(dplyr)
correlation_matrix <- cor(select(file_clean_data, where(is.numeric)), use = "complete.obs")
print(correlation_matrix)

## 5. Correlation Plot

corrplot(correlation_matrix, method = "circle")

## 6. Scatter Plot

corr_saleprice <- cor(select(file_clean_data, where(is.numeric)), use = "complete.obs")["SalePrice", ]
highest_corr_saleprice <- names(sort(corr_saleprice, decreasing = TRUE)[2])
lowest_corr_saleprice <- names(which.min(corr_saleprice))
closest_05_corr_saleprice <- names(which.min(abs(corr_saleprice - 0.5)))

# Sort correlations in decreasing order, excluding SalePrice itself
top_6_features <- names(sort(corr_saleprice[-which(names(corr_saleprice) == "SalePrice")], decreasing = TRUE)[1:6])

# Print the top 6 features
top_6_features

print(paste("Variable with Highest Correlation Coefficient with SalePrice is:",highest_corr_saleprice))
print(paste("Variable with Lowest Correlation Coefficient with SalePrice is:",lowest_corr_saleprice))
print(paste("Variable with Closest Correlation Coefficient with SalePrice is:",closest_05_corr_saleprice))

library(ggplot2)

ggplot(file_clean_data, aes_string(x = highest_corr_saleprice, y = "SalePrice")) +
  geom_point() +
  labs(title = paste("Scatter Plot of", highest_corr_saleprice, "vs SalePrice"),
       x = highest_corr_saleprice, y = "SalePrice")

ggplot(file_clean_data, aes_string(x = lowest_corr_saleprice, y = "SalePrice")) +
  geom_point() +
  labs(title = paste("Scatter Plot of", lowest_corr_saleprice, "vs SalePrice"),
       x = lowest_corr_saleprice, y = "SalePrice")

ggplot(file_clean_data, aes_string(x = closest_05_corr_saleprice, y = "SalePrice")) +
  geom_point() +
  labs(title = paste("Scatter Plot of", closest_05_corr_saleprice, y = "SalePrice"),
       x = closest_05_corr_saleprice, y = "SalePrice")

## 7. Regression Model

regress_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
                    data = file_clean_data)
summary(regress_model)

## 8. Interpreting equation model

coefficients <- coef(regress_model)

equation <- paste0("SalePrice = ", round(coefficients[1], 2), " + ",
                   paste(
                     paste0(round(coefficients[-1], 2), " * ", names(coefficients[-1])), collapse = " + "
                   ))

cat("Regression Equation:", equation)

## 9. Plotting Regression Model

plot(regress_model)

## 10. Multicollinearity Check

##install.packages("car")
library(car)

vif_value <- vif(regress_model)
print(vif_value)

regress_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
           data = file_clean_data)
summary(regress_model)

coefficients <- coef(regress_model)

equation <- paste0("SalePrice = ", round(coefficients[1], 2), " + ",
                   paste(
                     paste0(round(coefficients[-1], 2), " * ", names(coefficients[-1])), collapse = " + "
                   ))

cat("Regression Equation:", equation)

print(vif(regress_model))

## 11. Outliers and Residuals

## Standardized residuals
standardized_residuals <- rstandard(regress_model)

# Potential outliers
outliers <- which(abs(standardized_residuals) > 3)
print(outliers)

plot(standardized_residuals, 
     main = "Standardized Residuals", 
     xlab = "Observation Index", 
     ylab = "Standardized Residuals",
     pch = 16, col = "cyan")
abline(h = c(-3, 3), col = "black", lty = 2)

## 12. Model Correction Attempt

file_clean_data <- file_clean_data[-c(outliers), ]

regress_model <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
                    data = file_clean_data)
summary(regress_model)

coefficients <- coef(regress_model)

equation <- paste0("SalePrice = ", round(coefficients[1], 2), " + ",
                   paste(
                     paste0(round(coefficients[-1], 2), " * ", names(coefficients[-1])), collapse = " + "
                   ))

cat("Regression Equation:", equation)

### Cross-Validation

##install.packages("caret")
library(caret)

set.seed(123)
training_index <- createDataPartition(file_clean_data$SalePrice, p = 0.8, list = FALSE)
training_data <- file_clean_data[training_index, ]
testing_data <- file_clean_data[-training_index, ]

training_control <- trainControl(method = "cv", number = 10)
model_cv <- train(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
                    data = file_clean_data)

print(model_cv)
predictions <- predict(model_cv, newdata = testing_data)

rmse_value <- sqrt(mean((predictions - testing_data$SalePrice)^2))
r_squared <- cor(predictions, testing_data$SalePrice)^2

n <- nrow(testing_data)
p <- length(model_cv$finalModel$coefficients) - 1
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r_squared))
print(paste("Adjusted R-squared:", adjusted_r_squared))

## 13. Subsets Regression Method

##install.packages("leaps")
library(leaps)

every_subsets <- regsubsets(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
                            data = file_clean_data)
summary(every_subsets)

sub_summary <- summary(every_subsets)

adj_r2 <- sub_summary$adjr2
best_models <- which(adj_r2 == max(adj_r2))
best_models

plot(every_subsets, scale = "adjr2")


# Perform all subsets regression
all_subsets <- regsubsets(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF,
                          data = file_clean_data,
                          nvmax = 5)

# View a summary of the results
all_subsets_summary <- summary(all_subsets)

# View metrics for each model size
all_subsets_summary

plot(all_subsets_summary$adjr2, type = "o", pch = 19,
     xlab = "Number of Predictors", ylab = "Adjusted R-squared",
     main = "Model Performance by Number of Predictors")

# Best 1-feature model
model_1 <- lm(SalePrice ~ Overall.Qual, data = file_clean_data)
summary(model_1)

# Best 2-feature model
model_2 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area, data = file_clean_data)
summary(model_2)

# Best 3-feature model
model_3 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars, data = file_clean_data)
summary(model_3)

# Best 4-feature model
model_4 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Total.Bsmt.SF, data = file_clean_data)
summary(model_4)

# Best 5-feature model
model_5 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Cars + Total.Bsmt.SF + X1st.Flr.SF, data = file_clean_data)
summary(model_5)

# Coefficients of the preferred model
coefficients <- coef(model_4)

# Construct the regression equation as a string
equation <- paste0(
  "SalePrice = ", round(coefficients[1], 2), 
  " + ", round(coefficients[2], 2), " * Overall.Qual",
  " + ", round(coefficients[3], 2), " * Gr.Liv.Area",
  " + ", round(coefficients[4], 2), " * Garage.Cars",
  " + ", round(coefficients[5], 2), " * Total.Bsmt.SF"
)

# Print the equation to the console
cat("Preferred Model Equation:\n", equation, "\n")

## 14. Model Comparison

# Calculate RMSE for Step 12 Model
predictions_step12 <- predict(model_cv, file_clean_data)
rmse_step12 <- sqrt(mean((predictions_step12 - file_clean_data$SalePrice)^2))
adj_r2_step12 <- summary(regress_model)$adj.r.squared

# Calculate RMSE for Step 13 Model
predictions_step13 <- predict(model_4, file_clean_data)
rmse_step13 <- sqrt(mean((predictions_step13 - file_clean_data$SalePrice)^2))
adj_r2_step13 <- summary(model_4)$adj.r.squared

# Print metrics for comparison
cat("Step 12 Model:\n")
cat("  Adjusted R^2:", round(adj_r2_step12, 4), "\n")
cat("  RMSE:", round(rmse_step12, 2), "\n")

cat("Step 13 Model:\n")
cat("  Adjusted R^2:", round(adj_r2_step13, 4), "\n")
cat("  RMSE:", round(rmse_step13, 2), "\n")