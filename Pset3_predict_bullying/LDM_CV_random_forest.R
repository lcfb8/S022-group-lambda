# Pset 3: Predicting Bullying
# Authors: Lambda team
# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)
library(doParallel)


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
  select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  select(-temp_id)

# compare these to `test` and `train`:
summary(train$bully)
summary(test$bully)

# what is the tuning parameter for rf?
modelLookup(model = "rf")

#####
# Train model with cross-validation
# Reset the seed
set.seed(80107)

# parallel run this heavy model training
cl <- makePSOCKcluster(parallel::detectCores() - 1)  
registerDoParallel(cl)

# multiple parameters combinations have been tested, these are the ones
# we landed on, test RMSE didn't seem to improve with the tweaks we made.
# we think this might be due to our data being very imbalanced.

grid <- expand.grid(
  mtry = c(5, 10, 15, 20, 25, 30),
  splitrule = c("variance", "extratrees"),
  min.node.size = c(3, 5, 10, 20)     
)

ctrl <- trainControl(
  method = "cv", 
  number = 10,
  savePredictions = "final",
  verboseIter = FALSE      
)
# we saw on Luke's website that he uses ranger as an alternative to rf 
cv_mod_reg <- train(
  bully ~ ., 
  data = train,
  method = "ranger",
  trControl = ctrl,
  num.trees = 1000,       
  tuneGrid = grid,
  metric = "RMSE",
  importance = "permutation"  
)
stopCluster(cl)

#####
# Check results

# expand grid was a suggestion from chatgpt
grid <- expand.grid(
mtry = c(2, 5, 10, 15, 20, 25, 28, 30, 32, 35, 40, 45, 50))

# CV setup (keep 10-fold, or use repeatedcv)
ctrl <- trainControl(method = "cv", number = 10)
# ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

cv_mod_reg <- train(
   bully ~ ., 
   data = train,
   method = "rf",
   trControl = ctrl,
   ntree = 500,
   tuneGrid = grid,
   metric = "RMSE"
 )
 
cv_mod_reg
cv_mod_reg$bestTune
plot(cv_mod_reg)

X <- model.matrix(bully ~ ., data = train)
ncol(X) - 1  # predictors (excluding intercept)

cv_mod_results <- cv_mod_reg$results

ggplot(cv_mod_results, aes(x = mtry, y = RMSE)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

# retrieve importance (by default, this is scaled from 0-100)
cv_mod_imp <- varImp(cv_mod_reg)

cv_mod_imp <- cv_mod_imp$importance # get the df only

cv_mod_imp <- cv_mod_imp %>% rownames_to_column("var")

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(y = "Variable", x = "Importance (scaled)") +
  theme_bw()

# had to ask help from chatgpt to display only the top 20 predictors:
cv_mod_imp <- varImp(cv_mod_reg)$importance %>%
  rownames_to_column("var") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 20)

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(title = "Top 20 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


train_reg <- train %>%
  mutate(rf_pred = predict(cv_mod_reg, train)) 

# caret has a built-in RMSE function, but it's also easy to calculate by hand:
RMSE(train_reg$bully, train_reg$rf_pred)
sqrt(mean((train_reg$bully - train_reg$rf_pred)^2))

# train RMSE 0.1595064
# now for test data:
test_reg <- test %>%
  mutate(rf_pred = predict(cv_mod_reg, test))

RMSE(test_reg$bully, test_reg$rf_pred)
sqrt(mean((test_reg$bully - test_reg$rf_pred)^2))
# test RMSE 0.399066

#####
# Fill in the csv file with our predictions
new_raw <- read.csv("cleaned_test_data.csv") %>%
  mutate(across(where(is.character), as.factor))
rf_reg <- cv_mod_reg

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

preds <- write.csv(pred_out, "rf_reg_predictions_ranger.csv", 
                   row.names = FALSE)

preds <- read.csv("rf_reg_predictions_ranger.csv")

