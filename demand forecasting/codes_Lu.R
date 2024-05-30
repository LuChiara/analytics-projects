# load data

library(readr)
library(dplyr)

###############
#### Q2 a) ####
###############

# Read the data
data <- read_csv("/Users/chiaralu/Desktop/Courses/MGSC 670/Assignments/HW1/singleitemSKU88.csv")

# Verify the number of weeks in the dataset
num_weeks <- nrow(data)

# Create a seasonal factor (Week_factor) for 12 months (4 weeks each, repeated annually)
data$Week_factor <-  as.factor(rep(rep(seq(1,13),each=4),3))

# Split the data into training (first 2 years) and testing (last year)
train_data <- data[1:(2 * 52), ]
test_data <- data[(2 * 52 + 1):num_weeks, ]

# Fit the model
model <- lm(log(St) ~ factor(Week_factor) + I(Week) + log(Price), data = train_data)

# Display the summary of the model
summary(model)

# get r2 of test set 
model_test <- lm(log(St) ~ Week_factor + Week + log(Price), data = test_data)
r2 <- summary(model_test)$r.squared
r2

# Predict on the test set
test_data$log_pred <- predict(model, newdata = test_data)

# Convert log predictions to actual demand
test_data$predicted_demand <- exp(test_data$log_pred)

# Evaluate the model performance: Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs(test_data$St - test_data$predicted_demand) / test_data$St)

# Print MAPE
print(paste("MAPE: ", mape))


###############
#### Q2 b) ####
###############

model2 <- lm(log(St) ~ factor(Week_factor) + I(Week) + log(Price) +log(Price_1) +
             log(Price_2), data = train_data)

summary(model2)

# prediction
model2_test <- lm(log(St) ~ Week_factor + Week + log(Price) +log(Price_1) +
             log(Price_2), data = test_data)

r2_2 <- summary(model2_test)$r.squared
r2_2

test_data$log_pred2 <- predict(model2, newdata = test_data)
# convert log predictions to actual demand
test_data$predicted_demand2 <- exp(test_data$log_pred2)

# MAPE
mape2 <- mean(abs(test_data$St - test_data$predicted_demand2) / test_data$St)
print(paste("MAPE: ", mape2))

############
#### Q4 ####
############

# Fit the model
coefficients <- coef(model)

# Forecasting for Week 7
week <- 7
price_7 <- 1
log_price_7 <- log(price_7)
week_factor_7 <- data[data$Week == 7, "Week_factor"]

model_log_d7 <- coefficients['(Intercept)'] + 
  coefficients[paste0('factor(Week_factor)', week_factor_7)] + 
  coefficients['I(Week)'] * week + 
  coefficients['log(Price)'] * log_price_7

model_d7 <- exp(model_log_d7)
print(model_d7)


# Repeat for model 2
coefficients_2 <- coef(model2)

week <- 7
price_7 <- 1
price_6 <- 0.8
price_5 <- 0.8

log_price_7 <- log(price_7)
log_price_6 <- log(price_6)
log_price_5 <- log(price_5)

week_factor_7 <- Data[Data$Week == 7, "Week_factor"]

model2_log_d7 <- coefficients_2['(Intercept)'] + 
  coefficients_2[paste0('factor(Week_factor)', week_factor_7)] + 
  coefficients_2['I(Week)'] * week + 
  coefficients_2['log(Price)'] * log_price_7+
  coefficients_2['log(Price_1)'] * log_price_6 + 
  coefficients_2['log(Price_2)'] * log_price_5

model2_d7 <- exp(model2_log_d7)
print(model2_d7)


############
#### Q6 ####
############

# Load the data
multiple <- read_csv('/Users/chiaralu/Desktop/Courses/MGSC 670/Assignments/HW1/multipleitempart1.csv')

# Create seasonal factor
multiple$Season_factor <- as.factor(rep(seq(1, 13), length.out = nrow(multiple)))

# Split the data into training (first 2 years) and testing (last year)
num_weeks <- nrow(multiple)
train_data <- multiple[1:(2 * 52), ]  # Assuming 52 weeks per year
test_data <- multiple[(2 * 52 + 1):num_weeks, ]

# Initialize lists to store models, predictions, and MAPE values
models <- list()
predictions <- list()
mape_values <- numeric(5)

# Fit the model for each product
for (i in 1:5) {
  # Create dynamic variable names for demands and prices
  demand <- train_data[[paste0("S", i, "_t")]]
  price_now <- train_data[[paste0("PriceB", i)]]
  price_lag1 <- train_data[[paste0("PriceB", i, "_1")]]
  price_lag2 <- train_data[[paste0("PriceB", i, "_2")]]
  
  # Cross-price effects
  cross_price_effects <- NULL
  for (j in 1:5) {
    if (j != i) {
      cross_price_effects <- cbind(cross_price_effects, log(train_data[[paste0("PriceB", j)]]))
    }
  }
  colnames(cross_price_effects) <- paste0("log_priceB", setdiff(1:5, i))
  
  # Combine all data
  model_data <- data.frame(log_demand = log(demand), Season_factor = train_data$Season_factor, 
                           Week = train_data$Week, log_price_now = log(price_now), 
                           log_price_lag1 = log(price_lag1), log_price_lag2 = log(price_lag2), cross_price_effects)
  
  # Fit the model
  formula_str <- paste("log_demand ~ Season_factor + Week + log_price_now + log_price_lag1 + log_price_lag2 +",
                       paste(colnames(cross_price_effects), collapse = " + "))
  model <- lm(as.formula(formula_str), data = model_data)
  
  # Store the model
  models[[paste0("model_", i)]] <- model
  
  # Predict on the test data
  test_demand <- test_data[[paste0("S", i, "_t")]]
  test_price_now <- test_data[[paste0("PriceB", i)]]
  test_price_lag1 <- test_data[[paste0("PriceB", i, "_1")]]
  test_price_lag2 <- test_data[[paste0("PriceB", i, "_2")]]
  
  cross_price_effects_test <- NULL
  for (j in 1:5) {
    if (j != i) {
      cross_price_effects_test <- cbind(cross_price_effects_test, log(test_data[[paste0("PriceB", j)]]))
    }
  }
  colnames(cross_price_effects_test) <- paste0("log_priceB", setdiff(1:5, i))
  
  test_data_subset <- data.frame(Season_factor = test_data$Season_factor, Week = test_data$Week,
                                 log_price_now = log(test_price_now), log_price_lag1 = log(test_price_lag1),
                                 log_price_lag2 = log(test_price_lag2), cross_price_effects_test)
  
  predictions[[paste0("pred_", i)]] <- exp(predict(model, newdata = test_data_subset))
  
  # Calculate MAPE for the current product
  actual <- test_demand
  predicted <- predictions[[paste0("pred_", i)]]
  mape_values[i] <- mean(abs((actual - predicted) / actual)) * 100
}

# Display the models and predictions
models
predictions

# Display the MAPE values
mape_values

# Optionally, display the summaries of the models
for (i in 1:5) {
  print(summary(models[[paste0("model_", i)]]))
}



############
#### Q7 ####
############

# Load the data
multiple <- read_csv('/Users/chiaralu/Desktop/Courses/MGSC 670/Assignments/HW1/multipleitempart2.csv')
#View(multiple)

# Create seasonal factor
multiple$Season_factor <- as.factor(rep(seq(1, 13), length.out = nrow(multiple)))

# Split the data into training (first 2 years) and testing (last year)
num_weeks <- nrow(multiple)
train_data <- multiple[1:(2 * 52), ]  # Assuming 52 weeks per year
test_data <- multiple[(2 * 52 + 1):num_weeks, ]

# Initialize lists to store models, predictions, and MAPE values
models <- list()
predictions <- list()
mape_values <- numeric(5)

# Fit the model for each product
for (i in 1:5) {
  # Create dynamic variable names for demands and prices
  demand <- train_data[[paste0("S", i, "_t")]]
  price_now <- train_data[[paste0("PriceB", i)]]
  price_lag1 <- train_data[[paste0("PriceB", i, "_1")]]
  price_lag2 <- train_data[[paste0("PriceB", i, "_2")]]
  
  # Cross-price effects
  cross_price_effects <- NULL
  for (j in 1:5) {
    if (j != i) {
      cross_price_effects <- cbind(cross_price_effects, log(train_data[[paste0("PriceB", j)]]))
    }
  }
  colnames(cross_price_effects) <- paste0("log_priceB", setdiff(1:5, i))
  
  # Combine all data
  model_data <- data.frame(log_demand = log(demand), Season_factor = factor(train_data$Season_factor), 
                           Week = I(train_data$Week), log_price_now = log(price_now), 
                           log_price_lag1 = log(price_lag1), log_price_lag2 = log(price_lag2), cross_price_effects)
  
  # Fit the model
  formula_str <- paste("log_demand ~ Season_factor + Week + log_price_now + log_price_lag1 + log_price_lag2 +",
                       paste(colnames(cross_price_effects), collapse = " + "))
  model <- lm(as.formula(formula_str), data = model_data)
  
  # Store the model
  models[[paste0("model_", i)]] <- model
  
  # Predict on the test data
  test_demand <- test_data[[paste0("S", i, "_t")]]
  test_price_now <- test_data[[paste0("PriceB", i)]]
  test_price_lag1 <- test_data[[paste0("PriceB", i, "_1")]]
  test_price_lag2 <- test_data[[paste0("PriceB", i, "_2")]]
  
  cross_price_effects_test <- NULL
  for (j in 1:5) {
    if (j != i) {
      cross_price_effects_test <- cbind(cross_price_effects_test, log(test_data[[paste0("PriceB", j)]]))
    }
  }
  colnames(cross_price_effects_test) <- paste0("log_priceB", setdiff(1:5, i))
  
  test_data_subset <- data.frame(Season_factor = factor(test_data$Season_factor), Week = I(test_data$Week),
                                 log_price_now = log(test_price_now), log_price_lag1 = log(test_price_lag1),
                                 log_price_lag2 = log(test_price_lag2), cross_price_effects_test)
  
  predictions[[paste0("pred_", i)]] <- exp(predict(model, newdata = test_data_subset))
  
  # Calculate MAPE for the current product
  actual <- test_demand
  predicted <- predictions[[paste0("pred_", i)]]
  mape_values[i] <- mean(abs((actual - predicted) / actual)) * 100
}

# Display the models and predictions
models
predictions

# Display the MAPE values
mape_values

# Optionally, display the summaries of the models
for (i in 1:5) {
  print(summary(models[[paste0("model_", i)]]))
}


############
#### Q8 ####
############

# Run Q 6 block again

# Initialize a list to store the predicted demands
predicted_demands <- list()

# Define the constant prices
prices <- c(1, 1, 1, 1, 1)

# Loop over the two price scenarios for product 1
for (p17 in c(1, 0.7)) {
  # Update the price of product 1
  prices[1] <- p17
  
  # Loop over the five products
  for (i in 1:5) {
    # Create a new data frame for the prediction
    new_data <- data.frame(Season_factor = factor(1, levels = levels(train_data$Season_factor)),
                           Week = 1,
                           log_price_now = log(prices[i]),
                           log_price_lag1 = log(1),  # Assuming the lagged price is 1
                           log_price_lag2 = log(1))  # Assuming the lagged price is 1
    
    # Add the cross-price effects
    for (j in 1:5) {
      if (j != i) {
        new_data[[paste0("log_priceB", j)]] <- log(prices[j])
      }
    }
    
    # Predict the log demand using the corresponding model
    log_demand <- predict(models[[paste0("model_", i)]], newdata = new_data)
    
    # Exponentiate to get the predicted demand
    predicted_demands[[paste0("d", i, "7_p17_", p17)]] <- exp(log_demand)
  }
}

# Print the predicted demands
predicted_demands

