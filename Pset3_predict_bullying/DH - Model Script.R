library(tidyverse)
library(caret)
library(randomForest)
library(FNN)
library(class)

train <- read_csv("cleaned_data.csv")
test <- read_csv("cleaned_test_data.csv")

# Add bully_high variable to the training set
train <- train %>%
  mutate(bully_high = ifelse(bully >= 2.5, 1, 0))

# one-hot encoding numeric values for the categorical variables for KNN model
# parent_1 and 2 have natural orders for encoding
# gender, school_adults_value, parent_qual, and qual are nominal variables, 
# so we will use arbitrary numeric encoding for them (this is a limitation in this model)
encode_categoricals <- function(df) {
  df %>%
    mutate(
      parent_1 = dplyr::recode(parent_1,
                               "Less than high school"                                                    = 1,
                               "High school or GED"                                                       = 2,
                               "Two-year degree (like an associate's degree or trade school degree)"      = 3,
                               "Some college (they went to a four-year college but didn't finish)"        = 4,
                               "Finished a four-year college"                                             = 5,
                               "Finished a graduate degree (like a master's degree, PhD, MD, or a law degree)" = 6,
                               "I'm not sure"                                                             = 7,
                               "This does not apply to me"                                                = 7
      ),
      parent_2 = dplyr::recode(parent_2,
                               "Less than high school"                                                    = 1,
                               "High school or GED"                                                       = 2,
                               "Two-year degree (like an associate's degree or trade school degree)"      = 3,
                               "Some college (they went to a four-year college but didn't finish)"        = 4,
                               "Finished a four-year college"                                             = 5,
                               "Finished a graduate degree (like a master's degree, PhD, MD, or a law degree)" = 6,
                               "I'm not sure"                                                             = 7,
                               "This does not apply to me"                                                = 7
      )
    ) %>%
    mutate(
      gender = dplyr::recode(gender,
                             "Male"                      = 1,
                             "Female"                    = 2,
                             "Another way:"              = 3,
                             "I choose not to identify." = 3
      )
    ) %>%
    mutate(
      school_adults_value = dplyr::recode(school_adults_value,
                                          "Academics" = 1,
                                          "Caring"    = 2,
                                          "Athletics" = 3,
                                          "Arts"      = 4,
                                          "Other"     = 5
      )
    ) %>%
    mutate(
      parent_qual = dplyr::recode(parent_qual,
                                  "ach"   = 1,
                                  "care"  = 2,
                                  "happy" = 3
      ),
      qual = dplyr::recode(qual,
                           "ach"   = 1,
                           "care"  = 2,
                           "happy" = 3
      )
    )
}

# Step 10: Train/validation split
set.seed(80107)
train_index <- sample(1:nrow(train), size = 0.8 * nrow(train))
model_train <- train[ train_index, ]
model_val   <- train[-train_index, ]

# Create X matrices
model_train_X <- model_train %>% select(-student_id, -bully, -bully_high)
model_val_X   <- model_val   %>% select(-student_id, -bully, -bully_high)
test_X        <- test  %>% select(-student_id)


# adding numerically encoded values to the data frames for KNN model
knn_train_X <- model_train_X %>% encode_categoricals()
knn_val_X   <- model_val_X   %>% encode_categoricals()
knn_test_X  <- test_X        %>% encode_categoricals()

#scaling the data for KNN model
preProcValues <- preProcess(knn_train_X, method = c("center", "scale"))

knn_train_scaled <- predict(preProcValues, knn_train_X)
knn_val_scaled <- predict(preProcValues, knn_val_X)
knn_test_scaled <- predict(preProcValues, knn_test_X)

# Step 11: Train KNN model
# creating 10-fold cross validation
cv_control_reg <- trainControl(
  method          = "cv",
  number          = 10,
  verboseIter      = TRUE
)

train_control <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter      = TRUE
)

k_grid <- expand.grid(k = c(3, 5, 7, 10, 15, 20, 25, 30, 50))

# KNN Regression (predicted_bully_level)
knn_reg_train <- knn_train_scaled %>%
  mutate(bully = model_train$bully )

knn_reg_model <- train(
  bully ~ .,
  data = knn_reg_train,
  method = "knn",
  trControl = cv_control_reg,
  tuneGrid = k_grid,
  metric = "RMSE"
)  

# finding the best k and RMSE for the KNN regression model
print(knn_reg_model)
plot(knn_reg_model)

knn_reg_preds <- predict(knn_reg_model, newdata = knn_val_scaled)

postResample(pred = knn_reg_preds, obs = model_val$bully)

knn_class_train <- knn_train_scaled %>%
  mutate(bully_high = as.factor(ifelse(model_train$bully_high == 1, "yes", "no")))

knn_class_model <- train(
  bully_high ~ .,
  data = knn_class_train,
  method = "knn",
  trControl = train_control,
  tuneGrid = k_grid,
  metric = "ROC"
)

#predict on validation
knn_class_preds <- predict(knn_class_model, newdata = knn_val_scaled)

#confusion matrix
confusionMatrix(knn_class_preds, as.factor(ifelse(model_val$bully_high == 1, "yes", "no")))

# finding the best k and ROC for the KNN classification model
print(knn_class_model)
plot(knn_class_model)

# the curve did not become flat or dip, so we will try more k values to see if we can find a better model
k_grid_extended <- expand.grid(k = c(50, 75, 100, 125, 150))

knn_class_model_extended <- train(
  bully_high ~ .,
  data = knn_class_train,
  method = "knn",
  trControl = train_control,
  tuneGrid = k_grid_extended,
  metric = "ROC"
)
plot(knn_class_model_extended)

# creating a full grid with the extended k values to find the best model
k_grid_full <- expand.grid(k = c(3, 5, 7, 10, 15, 20, 25, 30, 50, 75, 100, 125, 150))

knn_class_model <- train(
  bully_high ~ .,
  data      = knn_class_train,
  method    = "knn",
  trControl = train_control,
  tuneGrid  = k_grid_full,
  metric    = "ROC"
)

plot(knn_class_model)

knn_class_preds <- predict(knn_class_model, newdata = knn_val_scaled)

confusionMatrix(knn_class_preds, 
                as.factor(ifelse(model_val$bully_high == 1, "yes", "no")))

rf_importance_model <- randomForest(
  bully ~ .,
  data       = model_train %>% select(-student_id, -bully_high),
  ntree      = 500,
  importance = TRUE,
  na.action  = na.omit
)

varImpPlot(rf_importance_model, 
           n.var = 10,
           main  = "Top 10 Most Important Variables")

# Get baseline RMSE on validation set
baseline_preds <- predict(knn_reg_model, newdata = knn_val_scaled)
baseline_rmse  <- sqrt(mean((model_val$bully - baseline_preds)^2))

# For each variable, shuffle it and measure RMSE increase
vars <- names(knn_val_scaled)

importance_scores <- sapply(vars, function(var) {
  permuted        <- knn_val_scaled
  permuted[[var]] <- sample(permuted[[var]])  # shuffle one variable
  perm_preds      <- predict(knn_reg_model, newdata = permuted)
  perm_rmse       <- sqrt(mean((model_val$bully - perm_preds)^2))
  return(perm_rmse - baseline_rmse)           # higher = more important
})

# Scale to 0-100 and plot top 20
data.frame(Variable   = names(importance_scores),
           Importance = importance_scores) %>%
  arrange(desc(Importance)) %>%
  head(20) %>%
  mutate(Importance = (Importance / max(Importance)) * 100) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Predictors (KNN)",
       x     = "",
       y     = "Importance (scaled)") +
  theme_minimal()

# KNN Regression test predictions
test_reg_preds <- predict(knn_reg_model, newdata = knn_test_scaled)

# Get probability scores for test set
knn_class_probs <- predict(knn_class_model, newdata = knn_test_scaled, type = "prob")
test_risk_preds <- knn_class_probs[, "yes"]

# Get probability scores on VALIDATION set
knn_val_probs <- predict(knn_class_model, newdata = knn_val_scaled, type = "prob")

# Training set rate
cat("Training set bully_high rate:", mean(train$bully_high), "\n")

# Validation set rate
cat("Validation set bully_high rate:", mean(model_val$bully_high), "\n")

# placing thresholds to flag risk
thresholds <- seq(0.01, 0.20, by = 0.01)

threshold_results <- sapply(thresholds, function(t) {
  preds  <- ifelse(knn_val_probs[, "yes"] >= t, 1, 0)
  actual <- model_val$bully_high
  fn_rate <- sum(preds == 0 & actual == 1) / sum(actual == 1)
  fp_rate <- sum(preds == 1 & actual == 0) / sum(actual == 0)
  c(threshold = t, FNR = fn_rate, FPR = fp_rate)
})

as.data.frame(t(threshold_results))

for (t in c(0.06, 0.07, 0.08, 0.09, 0.10)) {
  rate <- mean(test_risk_preds >= t)
  cat("Threshold:", t, "→ Predicted high rate:", round(rate, 4), "\n")
}

# creating submission dataframe using the 0.09 threshold to 
# match the bully_high rates of the train and validate sets
submission <- data.frame(
  student_id            = test$student_id,
  predicted_bully_level = test_reg_preds,
  predicted_bully_risk  = test_risk_preds,
  predicted_bully_high  = ifelse(test_risk_preds >=0.09, 1, 0)
)

# What is the prediction percentage for bully_high
cat("Predicted bully_high rate:", mean(submission$predicted_bully_high), "\n")

# VERIFY! 
table(submission$predicted_bully_high)

write_csv(submission, "LAMBDA_KNN_student_predictions.csv")

#checking prediction file
source("check_predictions_function.R")
check_prediction_file_format("LAMBDA_KNN_student_predictions.csv")
