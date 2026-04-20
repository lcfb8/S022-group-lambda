
# Pset 3: Predicting Bullying
# Authors: Lambda team


# Packages 
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)
library(MASS)           


# Load Data 
train_all <- read.csv("cleaned_data.csv")
test_all  <- read.csv("cleaned_test_data.csv")   #test set for submission


skim(train_all)
nrow(train_all)

mean_high_low_bully <- train_all %>%
  mutate(bully_group = if_else(bully >= 2.5, ">= 2.5", "< 2.5")) %>%
  group_by(bully_group) %>%
  summarise(
    mean_bully = mean(bully, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  )


# Check near-zero variance variables
nzv_to_drop <- nearZeroVar(train_all, saveMetrics = TRUE) %>%
  filter(nzv == TRUE)
print(nzv_to_drop)

# Drop columns not used in training
train_clean <- train_all %>%
  dplyr::select(-race_amerind, -feel_safer_other_rank, -student_id)

# Convert character columns to factors
train_clean <- train_clean %>%
  mutate(across(where(is.character), as.factor))

# Inspect variable types
type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")
print(type)


set.seed(80107)

trainIndex <- createDataPartition(train_clean$bully,
                                  p      = .8,
                                  list   = FALSE,
                                  times  = 1)

full <- train_clean %>%
  mutate(temp_id = row_number())

train <- full %>% filter( temp_id %in% trainIndex) %>% dplyr::select(-temp_id)
test  <- full %>% filter(!temp_id %in% trainIndex) %>% dplyr::select(-temp_id)

summary(train$bully)
summary(test$bully)

# Binarize outcome — "high" = bully score ≥ 2.5
trainb <- train %>%
  mutate(bully = factor(if_else(bully >= 2.5, "high", "low")))

testb <- test %>%
  mutate(bully = factor(if_else(bully >= 2.5, "high", "low")))


# Cross-validation control
set.seed(80107)

ctrl <- trainControl(
  method          = "repeatedcv",
  number          = 10,
  repeats         = 3,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Stepwise selection
full_model <- glm(bully ~ ., data = trainb, family = binomial())

step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)
AIC(step_model)
# the AIC here is 1475.915

# Train M1: logistic regression
set.seed(80107)


M1 <- train(
  bully ~ grade + gender + race_latinx + race_other + race_none +
    parent_1 + feel_safer_clear + feel_safer_consistent + feel_safer_other +
    feel_safer_consistent_rank + feel_safer_relats_rank + feel_safer_counselor_rank +
    parent_qual + sm_ever_stalker + sm_ever_bad_post + sm_ever_got_mean +
    sm_ever_trouble + sm_ever_stalked + tell_friend + tell_family +
    school_values_red + sm_more_helpful_red + sm_attitude_score +
    e_safety_score + parent_support_score + sm_help_score + sm_concern_score +
    discrimination + school_rules,
  data      = trainb,
  method    = "glm",
  family    = binomial(),
  trControl = ctrl,
  metric    = "ROC"
)

AIC(M1$finalModel)
BIC(M1$finalModel)
summary(M1$finalModel)

# Variables to keep (matching model formula + student_id)
vars_to_keep <- c(
  "student_id",
  "grade", "gender",
  "race_latinx", "race_other", "race_none",
  "parent_1",
  "feel_safer_clear", "feel_safer_consistent", "feel_safer_other",
  "feel_safer_consistent_rank", "feel_safer_relats_rank", "feel_safer_counselor_rank",
  "parent_qual",
  "sm_ever_stalker", "sm_ever_bad_post", "sm_ever_got_mean",
  "sm_ever_trouble", "sm_ever_stalked",
  "tell_friend", "tell_family",
  "school_values_red", "sm_more_helpful_red",
  "sm_attitude_score", "e_safety_score", "parent_support_score",
  "sm_help_score", "sm_concern_score",
  "discrimination", "school_rules"
)

# Guard: stop early if any column is missing (I asked Claude how to match the variables chosen in M1 with the ones in the test set for submission)
missing_vars <- vars_to_keep[!vars_to_keep %in% colnames(test_all)]
if (length(missing_vars) > 0) {
  stop("Missing columns in test_all: ", paste(missing_vars, collapse = ", "))
} else {
  cat("✅ All required columns found in test_all\n")
}

test_all_clean <- test_all %>%
  dplyr::select(all_of(vars_to_keep)) %>%
  mutate(across(where(is.character), as.factor))


write.csv(
  test_all_clean %>% dplyr::select(student_id, bully_score),
  "cleaned_test_data_with_scores.csv",
  row.names = FALSE
)

preds <- read.csv("cleaned_test_data_with_scores.csv")
head(preds)

# calculate AUC in case it's needed
library(pROC)

test_probs <- predict(M1, newdata = testb, type = "prob")

roc_curve <- roc(
  response  = testb$bully,          #
  predictor = test_probs[["high"]], 
  levels    = c("low", "high")      
)

# Print AUC
auc(roc_curve)
# AUC 0.8844905

