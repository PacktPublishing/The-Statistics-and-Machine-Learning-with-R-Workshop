# Create a range of equally spaced numbers between -10 and 10
x = seq(-10, 10, by = 0.1)

# Calculate the output value for each number using sigmoid function
sigmoid = 1 / (1 + exp(-x))

# Plot the sigmoid function
plot(x, sigmoid, type = "l", lwd = 2,
     main = "Sigmoid Function",
     xlab = "x",
     ylab = "f(x)",
     col = "red")

# Add grid lines
grid()



# Exercise 13.1 
# install the caret package if you haven't done so
install.packages("caret")

# load the caret package
library(caret)

# load the German Credit dataset
data(GermanCredit)
GermanCredit$Class_num = ifelse(GermanCredit$Class == "Bad", 1, 0)

lm_model = lm(Class_num ~ Duration, data=GermanCredit)
coefs = coefficients(lm_model)
intercept = coefs[1]
slope = coefs[2]

ggplot(GermanCredit, 
       aes(Duration, Class_num)) +
  geom_point() +
  geom_abline(intercept=intercept, slope=slope) +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 

ggplot(GermanCredit, 
       aes(Duration, Class_num)) +
  geom_point() +
  geom_abline(intercept=intercept, slope=slope) + 
  xlim(-30, 120) +
  ylim(-0.5, 1.5) +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 


glm_model = glm(Class_num ~ Duration, data=GermanCredit, family=binomial)
glm_model

ggplot(GermanCredit, 
       aes(Duration, Class_num)) +
  geom_point() +
  geom_abline(intercept=intercept, slope=slope) +
  geom_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family=binomial)
  ) +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 

# Get coefficients from logistic model
intercept_glm = coef(glm_model)[1]
slope_glm = coef(glm_model)[2]

# Generate sequence of x-values
x_values = seq(from = min(GermanCredit$Duration) - 150, 
                to = max(GermanCredit$Duration) + 150, 
                by = 0.1)

# Compute probabilities using logistic function
y_values = 1 / (1 + exp(-(intercept_glm + slope_glm * x_values)))

# Data frame for plot
plot_df = data.frame(x = x_values, y = y_values)

# Plot
ggplot() +
  geom_point(data = GermanCredit, aes(Duration, Class_num)) +
  geom_abline(intercept=intercept, slope=slope) +
  geom_line(data = plot_df, aes(x, y), color = "blue") +
  theme_minimal() +
  xlim(-30, 120) +
  ylim(-0.5, 1.5) +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 


# Exercise 13.2
library(tibble)
library(dplyr)

# making predictions
pred_df = tibble(
  Duration = seq(5, 80, 2)
)

pred_df = pred_df %>% 
  mutate(
    pred_prob = predict(glm_model, pred_df, type="response")
  )

ggplot() +
  geom_point(data = GermanCredit, aes(Duration, Class_num)) +
  geom_point(data = pred_df, aes(Duration, pred_prob) , color="blue") +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 
  
# getting the most likely outcome
pred_df = pred_df %>% 
  mutate(
    most_likely_outcome = round(pred_prob)
  )

ggplot() +
  geom_point(data = GermanCredit, aes(Duration, Class_num)) +
  geom_point(data = pred_df, aes(Duration, pred_prob) , color="blue") +
  geom_point(data = pred_df, aes(Duration, most_likely_outcome) , color="green") +
  theme(axis.title.x = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) 

#####
# calculate log odds using predicted probabilities
pred_df = pred_df %>% 
  mutate(
    log_odds = log(pred_prob / (1 - pred_prob))
  )

#####
# Create new data frame with all durations
new_data = data.frame(Duration = GermanCredit$Duration)
# Calculate predicted classes based on predicted probabilities
predicted_probs = predict(glm_model, new_data, type="response")

# Convert to binary outcomes
predicted_classes = ifelse(predicted_probs > 0.5, 1, 0) 

# Create confusion matrix
conf_matrix = table(predicted = predicted_classes, actual = GermanCredit$Class_num)
conf_matrix

# Accuracy
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", accuracy))

# Error rate
error_rate = 1 - accuracy
print(paste("Error rate: ", error_rate))

# Precision
precision = conf_matrix[2,2] / sum(conf_matrix[2,])
print(paste("Precision: ", precision))

# Recall / Sensitivity
recall = conf_matrix[2,2] / sum(conf_matrix[,2])
print(paste("Recall: ", recall))

# Specificity
specificity = conf_matrix[1,1] / sum(conf_matrix[,1])
print(paste("Specificity: ", specificity))


library(pROC)
# Calculate ROC curve
roc_obj = roc(GermanCredit$Class_num, predicted_probs)

# Plot ROC curve
plot(roc_obj)

# Calculate AUC
auc = auc(roc_obj)
print(paste("AUC: ", auc))


# Exercise 13.3
# split the raw dataset into training and test
set.seed(2)
index = sample(1:nrow(GermanCredit), nrow(GermanCredit)*0.7)
train = GermanCredit[index, ]
test = GermanCredit[-index, ]

# separate the minority and majority classes
table(train$Class_num)
minority_data = train[train$Class_num == 1,]
majority_data = train[train$Class_num == 0,]

# undersample the majority class
undersampled_majority = majority_data[sample(1:nrow(majority_data), nrow(minority_data)),]
# combine undersampled majority class and minority class
undersampled_data = rbind(minority_data, undersampled_majority)
table(undersampled_data$Class_num)

# oversample the minority class
oversampled_minority = minority_data[sample(1:nrow(minority_data), nrow(majority_data), replace = TRUE),]
# combine majority class and oversampled minority class
oversampled_data = rbind(majority_data, oversampled_minority)
table(oversampled_data$Class_num)

# fit logistic regression models on undersampled and oversampled data
undersampled_model = glm(Class_num ~ Duration, family = binomial(link = 'logit'), data = undersampled_data)
oversampled_model = glm(Class_num ~ Duration, family = binomial(link = 'logit'), data = oversampled_data)

# get the predicted probabilities on the test set
undersampled_pred = predict(undersampled_model, newdata = test, type = "response")
oversampled_pred = predict(oversampled_model, newdata = test, type = "response")

# apply threshold to convert the probabilities into binary classes
undersampled_pred_class = ifelse(undersampled_pred > 0.5, 1, 0)
oversampled_pred_class = ifelse(oversampled_pred > 0.5, 1, 0)

# calculate the confusion matrix
undersampled_cm = table(predicted = undersampled_pred_class, actual = test$Class_num)
oversampled_cm = table(predicted = oversampled_pred_class, actual = test$Class_num)

undersampled_cm
oversampled_cm

#####
library(glmnet)

# Create a matrix of predictors and a response vector
# For glmnet, we need to provide our data as matrices/vectors
X = GermanCredit[1:nrow(GermanCredit), 1:9]
y = GermanCredit$Class_num
# Define an alpha value: 0 for ridge, 1 for lasso, between 0 and 1 for elastic net
alpha_value = 1 # for lasso
# Run the glmnet model
fit = glmnet(X, y, family = "binomial", alpha = alpha_value)

# The resulting object contains the coefficients at each value of lambda
# Lambda is automatically chosen by glmnet, but you can also set it manually

# plot coefficient paths
plot(fit, xvar = "lambda", label = TRUE)


#####
library(nnet)

# convert gear to factor
mtcars$gear = as.factor(mtcars$gear)
table(mtcars$gear)

# fit the model
multinom_model = multinom(gear ~ mpg + hp + disp, data = mtcars)
 
# view summary of the model
summary(multinom_model)

# make prediction
predicted_gears = predict(multinom_model, newdata = mtcars)

# view the confusion matrix
table(Predicted = predicted_gears, Actual = mtcars$gear)





