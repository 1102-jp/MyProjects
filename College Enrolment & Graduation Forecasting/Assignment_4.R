
library(Matrix)
library(ISLR)
View(College)

library(psych)
describe(College)
summary(College)

set.seed(20305)
library(caret)

College$Grad.Rate <- as.numeric(College$Grad.Rate)

## Splitting the dataset into training and testing
trainIndex <- sample(1:nrow(College), size = 0.8 * nrow(College), replace = FALSE)
caret_train <- College[trainIndex, ]
caret_test <- College[-trainIndex, ]

## install.packages("glmnet")
library(glmnet)
train_x <- model.matrix(Grad.Rate ~ ., data = caret_train)[,-1]
train_y <- caret_train$Grad.Rate

test_x <- model.matrix(Grad.Rate ~ ., data = caret_test)[,-1]
test_y <- caret_test$Grad.Rate

print(unique(train_y))
summary(train_y)

## Ridge regression
cv_ridge <- cv.glmnet(train_x, train_y, alpha = 0)

print(paste("Minimum Lambda for Ridge:", ridge_min_lambda <- cv_ridge$lambda.min))
print(paste("Lambda 1se for Ridge:", ridge_lambda_1se <- cv_ridge$lambda.1se))

plot(cv_ridge)
## Ridge plot
abline(v = log(cv_ridge$lambda.min), col = "cyan", lty = 4)
abline(v = log(cv_ridge$lambda.1se), col = "black", lty = 4)

ridge <- glmnet(train_x, train_y, alpha = 0, lambda = ridge_min_lambda)
print(coef_ridge <- coef(ridge))

## Ridge train rmse
predict_ridge_train <- predict(ridge, newx = train_x)
rmse_ridge_train <- sqrt(mean((train_y - predict_ridge_train)^2))
print(paste("RMSE for Ridge Training data:", rmse_ridge_train))

## Ridge test rmse
predict_ridge_test <- predict(ridge, newx = test_x)
rmse_ridge_test <- sqrt(mean((test_y - predict_ridge_test)^2))
print(paste("RMSE for Ridge Testing data:", rmse_ridge_test))

## Showing Ridge result in table
library(knitr)

ridge_result <- data.frame(
  Metric = c("Minimum Lambda", "1se Lambda", "Training RMSE", "Testing RMSE"),
  Value = c(ridge_min_lambda, ridge_lambda_1se, rmse_ridge_train, rmse_ridge_test)
)

kable(ridge_result, col.names = c("Metric", "Value"), caption = "Ridge Regression RMSE and Lambda Values")

library(glmnet)

set.seed(20305)
## Lasso regression
cv_lasso <- cv.glmnet(train_x, train_y, alpha = 1)

print(paste("Minimum Lambda for LASSO:", lasso_min_lambda <- cv_lasso$lambda.min))
print(paste("Lambda 1se for LASSO:", lasso_lambda_1se <- cv_lasso$lambda.1se))

plot(cv_lasso)
## Lasso plot
abline(v = log(cv_lasso$lambda.min), col = "cyan", lty = 4)
abline(v = log(cv_lasso$lambda.1se), col = "black", lty = 4)

lasso <- glmnet(train_x, train_y, alpha = 1, lambda = lasso_min_lambda)
print(coef_lasso <- coef(lasso))

## Lasso train rmse
predict_lasso_train <- predict(lasso, newx = train_x)
rmse_lasso_train <- sqrt(mean((train_y - predict_lasso_train)^2))
print(paste("RMSE for Lasso Training data:", rmse_lasso_train))

## Lasso test rmse
predict_lasso_test <- predict(lasso, newx = test_x)
rmse_lasso_test <- sqrt(mean((test_y - predict_lasso_test)^2))
print(paste("RMSE for LASSO Testing data:", rmse_lasso_test))

## Showing Lasso result in table
library(knitr)

lasso_result <- data.frame(
  Metric = c("Minimum Lambda", "1se Lambda", "Training RMSE", "Testing RMSE"),
  Value = c(lasso_min_lambda, lasso_lambda_1se, rmse_lasso_train, rmse_lasso_test)
)

kable(lasso_result, col.names = c("Metric", "Value"), caption = "Lasso Regression RMSE and Lambda Values")

library(glmnet)

set.seed(20305)
## Elastic Net regression
alpha_0.5 <- cv.glmnet(train_x, train_y, alpha = 0.5)

print(paste("Minimum Lambda for Elastic Net:", alpha_min_lambda <- alpha_0.5$lambda.min))
print(paste("Lambda 1se for Elastic Net:", alpha_lambda_1se <- alpha_0.5$lambda.1se))

plot(alpha_0.5)
## Elastic Net plot
abline(v = log(alpha_0.5$lambda.min), col = "cyan", lty = 0.4)
abline(v = log(alpha_0.5$lambda.1se), col = "black", lty = 0.4)

alpha <- glmnet(train_x, train_y, alpha = 0.5, lambda = alpha_min_lambda)
print(coef_lambda <- coef(alpha))

## Elasticnet train rmse
predict_alpha_train <- predict(alpha, newx = train_x)
rmse_alpha_train <- sqrt(mean((train_y - predict_alpha_train)^2))
print(paste("RMSE for Elastic Net Training data:", rmse_alpha_train))

## Elasticnet test rmse
predict_alpha_test <- predict(alpha, newx = test_x)
rmse_alpha_test <- sqrt(mean((test_y - predict_alpha_test)^2))
print(paste("RMSE for Elastic Net Testing data:", rmse_alpha_test))

## Showing ElasticNet result in table
library(knitr)

alpha_result <- data.frame(
  Metric = c("Minimum Lambda", "1se Lambda", "Training RMSE", "Testing RMSE"),
  Value = c(alpha_min_lambda, alpha_lambda_1se, rmse_alpha_train, rmse_alpha_test)
)

kable(alpha_result, col.names = c("Metric", "Value"), caption = "Alpha at 0.5 Regression RMSE and Lambda Values")

library(knitr)

combined_results <- data.frame(
  Model = c("Ridge", "LASSO", "ElasticNet (α=0.5)"),
  Training_RMSE = c(rmse_ridge_train, rmse_lasso_train, rmse_alpha_train),
  Testing_RMSE = c(rmse_ridge_test, rmse_lasso_test, rmse_alpha_test),
  Minimum_Lambda = c(ridge_min_lambda, lasso_min_lambda, alpha_min_lambda),
  Lambda_1se = c(ridge_lambda_1se, lasso_lambda_1se, alpha_lambda_1se)
)

kable(combined_results, col.names = c("Model", "Training RMSE", "Testing RMSE", "Minimum Lambda", "Lambda 1se"), 
      caption= "Comparison of Ridge, LASSO and ElasticNet")

## Stepwise selection
model <- lm(Grad.Rate ~ ., data = caret_train)
stepwise_model <- step(model, direction = "both" , trace = 0)
summary(stepwise_model)

stepwise_train <- predict(stepwise_model, newdata = caret_train)
stepwise_test <- predict(stepwise_model, newdata = caret_test)

## RMSE of Stepwise selection for Training and Testing
rmse_stepwise_train <- sqrt(mean((caret_train$Grad.Rate - stepwise_train)^2))
rmse_stepwise_test <- sqrt(mean((caret_test$Grad.Rate - stepwise_test)^2))

print(paste("Stepwise Training RMSE:", rmse_stepwise_train))
print(paste("Stepwise Testing RMSE:", rmse_stepwise_test))

## Comparing all 4 models RMSE for training and testing
library(knitr)

rmse <- data.frame(
  Model = c("Ridge Regression", "LASSO Regression", "ElasticNet (α=0.5)", "Stepwise Selection"),
  Training_RMSE = c(rmse_ridge_train, rmse_lasso_train, rmse_alpha_train, rmse_stepwise_train),
  Testing_RMSE = c(rmse_ridge_test, rmse_lasso_test, rmse_alpha_test, rmse_stepwise_test)
)

kable(rmse, col.names = c("Model", "Training RMSE", "Testing RMSE"), 
      caption= "Comparison of RMSE for Ridge, LASSO, ElasticNet, and Stepwise Selection")

train_predictions <- data.frame(
  Model = c("Ridge (Training)", "Lasso (Training)", "ElasticNet (α=0.5) (Training)", "Stepwise (Training)"),
  Predictions = c(predict_ridge_train, 
                  predict_lasso_train, 
                  predict_alpha_train, 
                  stepwise_train)
)

test_predictions <- data.frame(
  Model = c("Ridge (Testing)", "Lasso (Testing)", "ElasticNet (α=0.5) (Testing)", "Stepwise (Testing)"),
  Predictions = c(predict_ridge_test, 
                  predict_lasso_test, 
                  predict_alpha_test, 
                  stepwise_test)
)

combined_predictions <- rbind(train_predictions, test_predictions)

## Comparing all 4 models for training and testing
combined_predictions <- data.frame(
  Model = c("Ridge (Training)", "Ridge (Testing)",
            "Lasso (Training)", "Lasso (Testing)",
            "ElasticNet (α=0.5) (Training)", "ElasticNet (α=0.5) (Testing)",
            "Stepwise (Training)", "Stepwise (Testing)"),
  Predictions = c(tail(predict_ridge_train, 1), tail(predict_ridge_test, 1),
                  tail(predict_lasso_train, 1), tail(predict_lasso_test, 1),
                  tail(predict_alpha_train, 1), tail(predict_alpha_test, 1),
                  tail(stepwise_train, 1), tail(stepwise_test, 1))
)
print(combined_predictions)