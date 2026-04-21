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

mean_high_low_bully

##############
# Overall clean-up

# Most of the code below is from Lab10, and has been modified to fit our needs.

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


# convert bully to a binary variable:
train_rfc <- train_clean %>%
  mutate(
    bully_bin = if_else(bully >= 2.5, 1L, 0L)  # 1 = high, 0 = low
  ) %>%
  dplyr::select(-bully)


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
  dplyr::select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

train$bully_bin <- factor(train$bully_bin, levels = c(0,1), labels = c("low","high"))
test$bully_bin  <- factor(test$bully_bin,  levels = c(0,1), labels = c("low","high"))

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

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)


cv_mod <- train(
  bully_bin ~ ., data = train, method = "rf", trControl = ctrl, ntree = 500, tuneLength = 10
)


class(train$bully_bin)
str(train$bully_bin)
# 
cv_mod

cv_mod_results <- cv_mod$results

ggplot(cv_mod_results, aes(x = mtry, y = ROC)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

max(cv_mod$results)

# AUC at the best tuned mtry
cv_mod$results[cv_mod$results$mtry == cv_mod$bestTune$mtry, "ROC"]

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


new_raw <- read.csv("cleaned_test_data.csv") %>%
  mutate(across(where(is.character), as.factor))

# Make sure bully_bin is not present (it usually won't be in test file)
new_raw <- new_raw %>% select(-any_of("bully_bin"))

rf_cls <- cv_mod
# Align factor levels to training model
for (v in names(rf_cls$xlevels)) {
  if (v %in% names(new_raw)) {
    new_raw[[v]] <- as.character(new_raw[[v]])
    new_raw[[v]][!new_raw[[v]] %in% rf_cls$xlevels[[v]]] <- NA
    new_raw[[v]] <- factor(new_raw[[v]], levels = rf_cls$xlevels[[v]])
  }
}

# Predicted probability of being "high"
p_mat <- predict(rf_cls, newdata = new_raw, type = "prob")
predicted_bully_risk <- p_mat[, "high"]

#### Delete this in the end ####
# 0/1 classification using 0.5 cutoff (change if you chose a different threshold)
# predicted_bully_high <- ifelse(predicted_bully_risk >= 0.5, 1L, 0L)

# Get probability scores on validation set
rf_val_probs <- predict(rf_cls, newdata = test, type = "prob")

# Threshold analysis
thresholds <- seq(0.01, 0.20, by = 0.01)

threshold_results <- sapply(thresholds, function(t) {
  preds   <- ifelse(rf_val_probs[, "high"] >= t, 1, 0)
  actual  <- ifelse(test$bully_bin == "high", 1, 0)
  fn_rate <- sum(preds == 0 & actual == 1) / sum(actual == 1)
  fp_rate <- sum(preds == 1 & actual == 0) / sum(actual == 0)
  c(threshold = t, FNR = fn_rate, FPR = fp_rate)
})

as.data.frame(t(threshold_results))

# Check predicted rate at different thresholds
for (t in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) {
  rate <- mean(predicted_bully_risk >= t)
  cat("Threshold:", t, "→ Predicted high rate:", round(rate, 4), "\n")
}

# extending threeshold to better match training and validation rates (run analysis first to pick the right one)
for (t in c(0.15, 0.20, 0.25, 0.30, 0.35, 0.40)) {
  rate <- mean(predicted_bully_risk >= t)
  cat("Threshold:", t, "→ Predicted high rate:", round(rate, 4), "\n")
}

# Use chosen threshold (run analysis first to pick the right one)
predicted_bully_high <- ifelse(predicted_bully_risk >= 0.15, 1L, 0L)

# verify!
table(predicted_bully_high)
cat("Predicted high rate:", mean(predicted_bully_high), "\n")
cat("Training high rate:", mean(train$bully_bin == "high"), "\n")


# Output with required column names
pred_out <- data.frame(
  student_id = new_raw$student_id,
  predicted_bully_level = NA_real_,
  predicted_bully_risk = as.numeric(predicted_bully_risk),
  predicted_bully_high = as.integer(predicted_bully_high)
)

write.csv(pred_out, "rf_class_predictions.csv", row.names = FALSE)
results <- read.csv("rf_class_predictions.csv")