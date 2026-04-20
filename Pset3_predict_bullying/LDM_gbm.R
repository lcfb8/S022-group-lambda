# Pset 3: Predicting Bullying
# Authors: Lambda team
# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)
library(doParallel)
library(gbm)


train_all <- read.csv("cleaned_data.csv")

## Data exploration and cleaning
# To answer  Writeup and Discussion Questions (Part I)
skim(train_all)

nrow(train_all)

hist(train_all$bully)

mean_high_low_bully <- train_all %>%
  mutate(bully_group = if_else(bully >= 2.5, ">= 2.5", "< 2.5")) %>%
  group_by(bully_group) %>%
  summarise(
    mean_bully = mean(bully, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )



#####
# Overall clean-up

# Some of the code below is from Lab10, but we will modify it to fit our needs.

# see if any variables in our dataset have close to zero variability 
# (and thus should be dropped from the analysis)
nzv_to_drop <- nearZeroVar(train_all, saveMetrics = TRUE) %>%
  filter( nzv == TRUE )

train_clean <- train_all %>%
  dplyr::select(-race_amerind, -feel_safer_other_rank, -student_id)

# Convert all character predictors to factors
train_clean <- train_clean %>%
  mutate(across(where(is.character), as.factor))

# check variable types
map_chr(train_clean, typeof)

type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")

#####
# Splitting data into train and test
# set a seed for reproducibility
set.seed(80107)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(train_clean$bully,
                                  p = .8, # proportion of data for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- train_clean %>% 
  mutate(temp_id = 1:nrow(train_clean))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  dplyr::select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

# compare these to `test` and `train`:
summary(train$bully)
summary(test$bully)

# After realizing how imbalanced our data was, we started to look into models 
# that could better deal with the rare and high scores of bully. We learned 
# that rf averages out those high values, so we decided to try gradient boosting
# machines (gbm) instead, which can capture more complex relationships in the 
# data and may be better at predicting the higher bully scores.

# we ran multiple combinations of parameters, we could go on testing others, but 
# the below ones seemed good enough and predicted 1.5% of the test data better 
# than rf, which is a nice improvement. The test RMSE from rf was 0.399066, with
# gbm the test RMSE is 0.386752. So, to best of our knowledge this might be
# the best model we can get with the data we have.

tune_grid_gbm_v2 <- expand.grid(
  n.trees           = c(1000, 2000, 3000, 5000),  
  interaction.depth = c(7, 9, 11),                
  shrinkage         = c(0.001, 0.005, 0.01),      
  n.minobsinnode    = c(10, 20, 30)              
)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

set.seed(80107)

gbm_mod_v2 <- train(
  bully ~ .,
  data = train,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = tune_grid_gbm_v2,
  metric = "RMSE",
  verbose = FALSE
)

stopCluster(cl)


gbm_mod_v2$bestTune

plot(gbm_mod_v2)

X <- model.matrix(bully ~ ., data = train)
ncol(X) - 1  # predictors (excluding intercept)

cv_mod_results <- gbm_mod_v2$results

# retrieve importance (by default, this is scaled from 0-100)
cv_mod_imp <- varImp(gbm_mod_v2)

cv_mod_imp <- cv_mod_imp$importance # get the df only

cv_mod_imp <- cv_mod_imp %>% rownames_to_column("var")

# The most important predictors are:
# e_safety_score	100.00	
# discrimination	98.82	
# school_rules	48.46	
# belong_score	17.82	
# p_safety_score	16.04	

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(y = "Variable", x = "Importance (scaled)") +
  theme_bw()

# had to ask help from chatgpt to display only the top 20 predictors:
cv_mod_imp <- varImp(gbm_mod_v2)$importance %>%
  rownames_to_column("var") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 20)

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(title = "Top 20 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


train_reg <- train %>%
  mutate(rf_pred = predict(gbm_mod_v2, train)) 

# caret has a built-in RMSE function, but it's also easy to calculate by hand:
RMSE(train_reg$bully, train_reg$rf_pred)
sqrt(mean((train_reg$bully - train_reg$rf_pred)^2))

# train RMSE  0.3395108

# now for test data:
test_reg <- test %>%
  mutate(rf_pred = predict(gbm_mod_v2, test))

RMSE(test_reg$bully, test_reg$rf_pred)
sqrt(mean((test_reg$bully - test_reg$rf_pred)^2))

# test RMSE 0.386752 (random forest test RMSE 0.399066)
#####
# Fill in the csv file with our predictions
new_raw <- read.csv("cleaned_test_data.csv") %>%
  mutate(across(where(is.character), as.factor))
rf_reg <- gbm_mod_v2

# Align factor levels to what the regression model saw during training 
# (suggestion from Claude)
for (v in names(rf_reg$xlevels)) {
  if (v %in% names(new_raw)) {
    new_raw[[v]] <- as.character(new_raw[[v]])
    new_raw[[v]][!new_raw[[v]] %in% rf_reg$xlevels[[v]]] <- NA
    new_raw[[v]] <- factor(new_raw[[v]], levels = rf_reg$xlevels[[v]])
  }
}

predicted_bully_level <- predict(rf_reg, newdata = new_raw)

pred_out <- data.frame(
  student_id = new_raw$student_id,
  predicted_bully_level = as.numeric(predicted_bully_level)
)

preds <- write.csv(pred_out, "LDM_gbm_predictions.csv", 
                   row.names = FALSE)

preds <- read.csv("LDM_gbm_predictions.csv")


