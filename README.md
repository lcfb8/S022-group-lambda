# S022-group-lambda

---

## Data

- **Training data:** `cleaned_data.csv`
  - Survey responses from students
  - Outcome variable: `bully` (continuous scale 1--4)
  - Binary threshold: `bully >= 2.5` = high risk

- **Test data:** `cleaned_test_data.csv`
  - Held-out data with no bully scores
  - Used for final predictions only

---

## Models

### Regression (predicting continuous bully score)

| Model | Validation RMSE | Selected |
|---|---|---|
| KNN Regression | 0.446 | No |
| Ranger Random Forest | 0.399 | Yes |
| Ridge/Lasso/Elastic Net | 0.412 / 0.416 | No |
| LOESS | 0.416 | No |
| Linear Regression | 0.401 | No |

### Classification (predicting high vs low risk)

| Model | AUC | Selected |
|---|---|---|
| KNN Classification | N/A | No |
| Random Forest | 0.879 | No |
| Sparse Logistic Regression | 0.892 | No |
| Logistic Regression | 0.911 | No |
| Stepwise Logistic Regression | 0.913 | Yes |

---

## Key Findings

- Top predictors across models:
  `e_safety_score`, `discrimination`, `school_rules`
- All three are composite variables validated via 
  Cronbach's alpha (all > 0.90) and corrected 
  item-total correlations (all > 0.66)
- Class imbalance (~4.6% high-risk students) was 
  addressed using case weights in regression models
- We prioritize minimizing false negatives over 
  false positives given the cost of missing 
  at-risk students

---

## Reproducibility

All models use `set.seed(80107)` for reproducibility.

### Required Packages

```r
install.packages(c(
  "tidyverse",
  "caret",
  "skimr",
  "MASS",
  "pROC",
  "gbm",
  "ranger",
  "glmnet",
  "randomForest",
  "knitr",
  "psych"       # for Cronbach's alpha
))