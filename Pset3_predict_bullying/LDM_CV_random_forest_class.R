# Pset 3: Predicting Bullying
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

train_clean <- train_all %>%
  select(-race_amerind, -feel_safer_other_rank, -student_id)

# Convert all character predictors to factors
train_clean <- train_clean %>%
  mutate(across(where(is.character), as.factor))


# check variable types
map_chr(train_all, typeof)

type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")


# convert bully to a binary variable:
train_rfc <- train_all %>%
  mutate(
    bully_bin = if_else(bully >= 2.5, 1L, 0L)  # 1 = high, 0 = low
  ) %>%
  select(-bully)

# set a seed for reproducibility
set.seed(80107)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(train_rfc$bully_bin,
                                  p = .8, # proportion of data to use for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- train_rfc %>% 
  mutate(temp_id = 1:nrow(train_rfc))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  select(-temp_id)

# compare these to `test` and `train`:
table(train$bully_bin)
prop.table(table(train$bully_bin))

table(test$bully_bin)
prop.table(table(test$bully_bin))
# results are very similar! :)


# what is the tuning parameter for rf?

modelLookup(model = "rf")

# Reset the seed
set.seed(80107)

ctrl <- trainControl(method = "cv", number = 10)

# now we train the model using cross-validation:
cv_mod <- train(
  bully_bin ~ ., data = train, method = "rf", trControl = ctrl, ntree = 200, tuneLength = 10
)

# 
cv_mod

cv_mod_results <- cv_mod$results

ggplot(cv_mod_results, aes(x = mtry, y = RMSE)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

# 38 seems to be the best mtry value: 0.4016325, R^2 0.3678621, MAE: 0.2828903

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

# now for test data:
test <- test %>%
  mutate(rf_pred = predict(cv_mod, test))

RMSE(test$bully, test$rf_pred)
sqrt(mean((test$bully - test$rf_pred)^2))