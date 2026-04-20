library(glmnet)
library(pROC)

train_all <- read.csv("cleaned_data.csv")

train_slg <- train_all %>%
  mutate(
    bully_bin = if_else(bully >= 2.5, 1L, 0L)  # 1 = high, 0 = low
  )

train_slg <- train_slg %>%
  select(-student_id, -race_amerind, -feel_safer_other_rank) %>%
  mutate(across(where(is.character), as.factor))

# Outcome (binary)
y <- train_slg$bully_bin

# Design matrix: use everything EXCEPT bully and bully_bin
X <- model.matrix(~ . - bully - bully_bin, data = train_slg)[, -1]

set.seed(80107)


cvfit_auc <- cv.glmnet(
  X, y,
  family = "binomial",
  alpha = 1, #lasso           
  nfolds = 10,
  type.measure = "auc"           
)


# Sparse Logistic Regression AUC
auc_min <- round(max(cvfit_auc$cvm), 4)

auc_1se <- round(
  cvfit_auc$cvm[cvfit_auc$lambda == cvfit_auc$lambda.1se], 4
)


# How many predictors kept at each lambda?
n_coef_min <- sum(coef(cvfit_auc, s = "lambda.min") != 0) - 1  # -1 for intercept
n_coef_1se <- sum(coef(cvfit_auc, s = "lambda.1se") != 0) - 1


cvfit
plot(cvfit)        # CV error vs log(lambda)


coef_min  <- coef(cvfit, s = "lambda.min")
coef_1se  <- coef(cvfit, s = "lambda.1se")

coef_1se  # many coefficients will be exactly 0

nz_coefs <- coef_1se[coef_1se != 0]
nz_coefs