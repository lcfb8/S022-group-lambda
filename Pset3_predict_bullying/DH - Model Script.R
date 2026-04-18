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
# Gender, school_adults_value, parent_qual, and qual are nominal variables, 
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


# adding one-hot encoded values back into the data frames for KNN model
knn_train_X <- model_train_X %>% encode_categoricals()
knn_val_X   <- model_val_X   %>% encode_categoricals()
knn_test_X  <- test_X        %>% encode_categoricals()

#scaling the data for KNN model
preProcValues <- preProcess(knn_train_X, method = c("center", "scale"))

knn_train_scaled <- predict(preProcValues, knn_train_X)
knn_val_scaled <- predict(preProcValues, knn_val_X)
knn_test_scaled <- predict(preProcValues, knn_test_X)

# Step 11: Train KNN model
# creating 5-fold cross validation
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

