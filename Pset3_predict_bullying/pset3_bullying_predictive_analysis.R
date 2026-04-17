# Pset 3: Predicting Bullying using cross validation - random forest
# Authors: Lambda team
# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)


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



##############
# Overall clean-up

# Most of the code below is from Lab10, but we will modify it to fit our needs.

# see if any variables in our dataset have close to zero variability 
# (and thus should be dropped from the analysis)
nzv_to_drop <- nearZeroVar(train_all, saveMetrics = TRUE) %>%
  filter( nzv == TRUE )


# check variable types
map_chr(train_all, typeof)

# some of the varibales are written text, so we'll drop all the variables with 
# "text", also drop the nzv == TRUE rows: race_amerind and sm_yikyak


train_clean <- train_all %>%
  select(-race_amerind, - feel_safer_other_rank, -student_id)

type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")


# set a seed for reproducibility
set.seed(80107)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(train_clean$bully,
                                  p = .5, # proportion of data to use for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- train_clean %>% 
  mutate(temp_id = 1:nrow(train_clean))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  select(-temp_id)

# compare these to `test` and `train`:
summary(train$bully)
summary(test$bully)

# results are very similar! :)


modelLookup(model = "knn")

# what is the tuning parameter for rf?

modelLookup(model = "rf")

# Reset the seed
set.seed(80107)

ctrl <- trainControl(method = "cv", number = 10)

# now we train the model using cross-validation:
cv_mod <- train(
  bully ~ ., data = train, method = "rf", trControl = ctrl, ntree = 200, tuneLength = 10
)

# 
cv_mod
# 
saveRDS(cv_mod, file = "cv_mod_regression.rds")

# I have saved the model to make this a bit quicker for us in lab:
cv_mod = readRDS("cv_mod_regression.rds")

# get a summary 
cv_mod

cv_mod_results <- cv_mod$results

ggplot(cv_mod_results, aes(x = mtry, y = RMSE)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

# 97 seems to be the best mtry value: 0.3927957
# but we'll have to choose variables or create composite to decrease the number of
# predictors.

# retrieve importance (by default, this is scaled from 0-100)
cv_mod_imp <- varImp(cv_mod)

cv_mod_imp <- cv_mod_imp$importance # get the df only

cv_mod_imp <- cv_mod_imp %>% rownames_to_column("var")

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(y = "Variable", x = "Importance (scaled)") +
  theme_bw()

# had to ask help from chatgpt to display only the top 20 predictors:
cv_mod_imp <- varImp(cv_mod)$importance %>%
  rownames_to_column("var") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 20)

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(title = "Top 20 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


train <- train %>%
  mutate(rf_pred = predict(cv_mod, train)) 

# caret has a built-in RMSE function, but it's also easy to calculate by hand:
RMSE(train$bully, train$rf_pred)
sqrt(mean((train$bully - train$rf_pred)^2))

# [1] 0.1676409

# now for test data:
test <- test %>%
  mutate(rf_pred = predict(cv_mod, test))

RMSE(test$bully, test$rf_pred)
sqrt(mean((test$bully - test$rf_pred)^2))