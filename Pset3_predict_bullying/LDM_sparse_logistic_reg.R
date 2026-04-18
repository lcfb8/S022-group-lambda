library(glmnet)

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


cvfit <- cv.glmnet(
  X, y,
  family = "binomial", # for logistic regression
  alpha = 1,        # 1 = LASSO; 0 = ridge; (0,1) = elastic net
  nfolds = 10
)

cvfit
plot(cvfit)        # CV error vs log(lambda)


coef_min  <- coef(cvfit, s = "lambda.min")
coef_1se  <- coef(cvfit, s = "lambda.1se")

coef_1se  # many coefficients will be exactly 0

nz_coefs <- coef_1se[coef_1se != 0]
nz_coefs

# probabilities for "1" class
p_hat <- predict(cvfit, newx = X, s = "lambda.1se", type = "response")

# hard classifications using 0.5 threshold
y_hat <- ifelse(p_hat >= 0.5, 1, 0)
mean(y_hat == y)  # training accuracy (use proper train/test or CV for performance)