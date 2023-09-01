###### Exercise 12.1 #######
# Set seed for reproducibility
set.seed(123)
# Generate independent variable X
X = runif(100, min = 1, max = 100) # 100 random uniform numbers between 1 and 100
# Generate some noise
noise = rnorm(100, mean = 0, sd = 10) # 100 random normal numbers with mean 0 and standard deviation 10
# Generate dependent variable Y
Y = 5 + 0.5 * X + noise 
# Combine X and Y into a data frame
data = data.frame(X, Y)

# Fit a simple linear regression model
model = lm(Y ~ X, data = data)
# Print the model summary
summary(model)

# Plot the data
plot(data$X, data$Y, main = "Simple Linear Regression", xlab = "X", ylab = "Y")
# Add the fitted regression line
abline(model, col = "red")



###### Exercise 12.2 #######
# Load the data
data(mtcars)

# Build the model
model = lm(mpg ~ cyl + hp + wt, data = mtcars)

# Print the summary of the model
summary(model)


###### Exercise 12.3 #######
# Create the dataset
set.seed(123)
x1 = rnorm(100)
x2 = -3 * x1 + rnorm(100)
y = 2 + x1 + x2 + rnorm(100)

df = data.frame(y = y, x1 = x1, x2 = x2)

# Single linear regression
single_reg = lm(y ~ x1, data = df)
summary(single_reg)

# Multiple linear regression
multi_reg = lm(y ~ x1 + x2, data = df)
summary(multi_reg)

###########
# Fit the model
model <- lm(mpg ~ qsec + am, data = mtcars)

# Display the summary of the model
summary(model)

# Convert am to categorical var
mtcars$am_cat = as.factor(mtcars$am)

# Fit the model
model <- lm(mpg ~ qsec + am_cat, data = mtcars)

# Display the summary of the model
summary(model)

# Load required library
library(ggplot2)

# Create new data frame for the predictions
newdata = data.frame(qsec = seq(min(mtcars$qsec), max(mtcars$qsec), length.out = 100),
                      am_cat = c(rep(0, 100), rep(1, 100)))
newdata$am_cat = as.factor(newdata$am_cat)

# Get predictions
newdata$mpg_pred = predict(model, newdata)

# Plot the data and the regression lines
ggplot(data = mtcars, aes(x = qsec, y = mpg, color = am_cat)) +
  geom_point() +
  geom_line(data = newdata, aes(y = mpg_pred)) +
  labs(title = "mpg vs qsec by Transmission Type",
       x = "Quarter Mile Time (qsec)",
       y = "Miles per Gallon (mpg)",
       color = "Transmission Type") +
  scale_color_discrete(labels = c("Automatic", "Manual")) +
  theme(text = element_text(size = 16),  # Default text size
        title = element_text(size = 15),  # Title size
        axis.title = element_text(size = 18),  # Axis title size
        legend.title = element_text(size = 16),  # Legend title size
        legend.text = element_text(size = 16), # Legend text size
        legend.position = "bottom")  # Legend position

# Adding interaction term
model_interaction <- lm(mpg ~ qsec * am_cat, data = mtcars)

# Print model summary
summary(model_interaction)

# Create scatter plot with two intersecting lines
ggplot(mtcars, aes(x = qsec, y = mpg, color = as.factor(am_cat))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # fit separate regression lines per group
  scale_color_discrete(name = "Transmission Type",
                       labels = c("Automatic", "Manual")) +
  labs(x = "Quarter mile time (seconds)",
       y = "Miles per gallon",
       title = "Separate regression lines fit for automatic and manual cars") +
  theme(text = element_text(size = 16),  
        title = element_text(size = 15), 
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) 


###### Exercise 12.4 #######
# Create a quadratic dataset
set.seed(1)
x = seq(-10, 10, by = 0.5)
y = x^2 + rnorm(length(x), sd = 5)

# Put it in a dataframe
df = data.frame(x = x, y = y)

# Plot the data
ggplot(df, aes(x = x, y = y)) + geom_point()

# Fit a linear model
lm1 <- lm(y ~ x, data = df)
summary(lm1)

# Fit a quadratic model
lm2 <- lm(y ~ x + I(x^2), data = df)
summary(lm2)

# Predicted values
df$linear_pred <- predict(lm1, newdata = df)
df$quadratic_pred <- predict(lm2, newdata = df)

# Plot the data and the models
ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = linear_pred), color = "blue", linetype = "dashed") +
  geom_line(aes(y = quadratic_pred), color = "red") +
  labs(title = "Scatter plot with linear and quadratic fits",
       x = "x",
       y = "y") +
  theme(text = element_text(size = 15)) +
  scale_color_discrete(name = "Model",
                       labels = c("Linear Model", "Quadratic Model")) +
  annotate("text", x = 0, y = 40, label = "Linear Model", color = "blue") +
  annotate("text", x = 6, y = 80, label = "Quadratic Model", color = "red")

#############
# Fit the original model
model_original = lm(mpg ~ hp, data = mtcars)

# Fit the log-transformed model
mtcars$log_mpg = log(mtcars$mpg)
model_log = lm(log_mpg ~ hp, data = mtcars)

# Predictions from the original model
mtcars$pred_original = predict(model_original, newdata = mtcars)

# Predictions from the log-transformed model (back-transformed to the original scale using exp)
mtcars$pred_log = exp(predict(model_log, newdata = mtcars))

library(tidyr)
library(dplyr)

# Reshape data to long format
df_long <- mtcars %>%
  gather(key = "Model", value = "Prediction", pred_original, pred_log)

# Create plot
ggplot(df_long, aes(x = hp, y = mpg)) +
  geom_point(data = mtcars, aes(x = hp, y = mpg)) +
  geom_line(aes(y = Prediction, color = Model)) +
  labs(
    x = "Horsepower (hp)",
    y = "Miles per gallon (mpg)",
    color = "Model"
  ) +
  scale_color_manual(values = c("pred_original" = "blue", "pred_log" = "red")) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.text = element_text(size = 16),  # control the font size of axis labels
    legend.text = element_text(size = 16)  # control the font size of legend text
  )



###########
# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n = 100 # number of observations
x = runif(n, -10, 10) # predictors
beta0 = 2 # intercept
beta1 = 3 # slope
epsilon = rnorm(n, 0, 2) # random error term
y = beta0 + beta1*x + epsilon # response variable

# Design matrix X
X = cbind(1, x)

# Calculate beta using closed-form solution
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
print(beta_hat)

# Fit linear regression model for comparison
model = lm(y ~ x)
print(coef(model))

############
# install the package if not already installed
if(!require(car)) install.packages('car')

# load the package
library(car)

# fit a linear model
model = lm(mpg ~ hp + wt + disp, data = mtcars)

# calculate VIF
vif_values = vif(model)
print(vif_values)

##############
# Load library
library(lmtest)

# Fit a simple linear regression model on mtcars dataset
model = lm(mpg ~ wt + hp, data = mtcars)

# Perform a Breusch-Pagan test to formally check for heteroskedasticity
bptest(model)


###### Exercise 12.5 #######
# install the package if not already installed
if(!require(glmnet)) install.packages('glmnet')
library(glmnet)

# Prepare data
data(mtcars)
X = as.matrix(mtcars[, -1]) # predictors
y = mtcars[, 1] # response

# Fit ridge regression model
set.seed(123) # for reproducibility
ridge_model = glmnet(X, y, alpha = 0)

# Use cross-validation to find the optimal lambda
cv_ridge = cv.glmnet(X, y, alpha = 0)
best_lambda = cv_ridge$lambda.min
best_lambda

# Fit a new ridge regression model using the optimal lambda
opt_ridge_model = glmnet(X, y, alpha = 0, lambda = best_lambda)

# Get coefficients
ridge_coefs = coef(opt_ridge_model)[-1]  # remove intercept
ridge_coefs

# Ordinary least squares regression
ols_model = lm(mpg ~ ., data = mtcars)

# Get coefficients
ols_coefs = coef(ols_model)[-1] # remove intercept
ols_coefs

# Plot
plot(1:length(ols_coefs), ols_coefs, type="b", col="blue", pch=19, xlab="Coefficient", ylab="Value", ylim=c(min(ols_coefs, ridge_coefs), max(ols_coefs, ridge_coefs)))
lines(1:length(ridge_coefs), ridge_coefs, type="b", col="red", pch=19)
legend("bottomright", legend=c("OLS", "Ridge"), col=c("blue", "red"), pch=19)

###### Exercise 12.5 #######
lasso_model = glmnet(X, y, alpha = 1)

# Use cross-validation to find the optimal lambda
cv_lasso = cv.glmnet(X, y, alpha = 1)
best_lambda = cv_lasso$lambda.min
best_lambda

# Fit a new lasso regression model using the optimal lambda
opt_lasso_model = glmnet(X, y, alpha = 1, lambda = best_lambda)

# Get coefficients
lasso_coefs = coef(opt_lasso_model)[-1]  # remove intercept
lasso_coefs

# Plot
plot(1:length(ols_coefs), ols_coefs, type="b", col="blue", pch=19, xlab="Coefficient", ylab="Value", ylim=c(min(ols_coefs, ridge_coefs), max(ols_coefs, ridge_coefs)))
lines(1:length(ridge_coefs), ridge_coefs, type="b", col="red", pch=19)
lines(1:length(lasso_coefs), lasso_coefs, type="b", col="green", pch=19)
legend("bottomright", legend=c("OLS", "Ridge", "Lasso"), col=c("blue", "red", "green"), pch=19)



