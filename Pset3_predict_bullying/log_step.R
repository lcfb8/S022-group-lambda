library( tidyverse )
library( glmnet )
library( caret )
library( skimr )
library( ModelMetrics )

# Forward/backward stepwise selection (based in part on ch 24 of the class 
# website)  and linear and logistic regression

# I put this code through chatGPT once I had written it, and incorporated chat's
# suggestions to remove the numeric encoding (just use factor variables), split
# on bully_high rather than bully for logistic regression, and update scaling

set.seed(80107)

cleaned_data = read.csv("cleaned_data.csv")

#remove student_id
cleaned_data <- cleaned_data[,-1]

# create a binary outcome for bully
reg_data = cleaned_data %>% 
  mutate(bully_high = ifelse(bully >=2.5, 1,0)) %>% 
  mutate(across(where(is.character), as.factor))

# now we'll create our train and validate datasets
trainIndex <- createDataPartition(reg_data$bully_high,
                                  p = .8, # the share of data in the training set
                                  list = FALSE # puts our output in matrix form
)

reg_data <- reg_data %>% 
  mutate(temp_id = 1:nrow(reg_data))

# Use those indices to split the data into training and test sets
reg_train <- reg_data %>% 
  filter(temp_id %in% trainIndex) %>% 
  dplyr::select(-temp_id)

# Assign everything else to the test dataset 
reg_test <- reg_data %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

# Scale all numeric variables except our outcome. ChatGPT helped me use the 
# train scale parameters for the test data to scale evenly

predictor_cols <- reg_train %>% 
  select(-bully, -bully_high) %>% 
  select(where(is.numeric)) %>% 
  names()

train_means <- reg_train %>% 
  summarise(across(all_of(predictor_cols), mean))

train_sds <- reg_train %>% 
  summarise(across(all_of(predictor_cols), sd))

reg_train <- reg_train %>% 
  mutate(across(all_of(predictor_cols),
                ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))

reg_test <- reg_test %>% 
  mutate(across(all_of(predictor_cols),
                ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))


# Baseline linear model (OLS) - continuous outcome (based on ch 24 webpage)
model_linear <- lm(bully ~ .-bully_high, data = reg_train)

reg_test$y_lm = predict( model_linear, newdata = reg_test)

#calculate RMSE
sqrt(mean((reg_test$bully-reg_test$y_lm)^2))

#check out the coefficients
coefs_lm = coef(model_linear)

coefs_lm <- data.frame(
  Variable = rownames(as.matrix(coefs_lm)),
  Coefficient = as.numeric(coefs_lm))

coefs_lm %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

############
# now let's run logistic regression with a binary outcome
model_log <- glm(bully_high ~ .-bully, data = reg_train, family = binomial)

summary(model_log)
AIC(model_log)

#predict. chatGPT helped me figure out the type="response" part of this
reg_test$y_log = predict( model_log, newdata = reg_test, type = "response")

# let's see what these predictions look like
reg_test %>% 
  ggplot(aes(age,y_log))+
  #color points by by bully_high
  geom_point(aes(color = factor(bully_high)))

#calculate RMSE (not great for classification/risk)
sqrt(mean((reg_test$bully_high-reg_test$y_log)^2))

#calculate AUC
auc(reg_test$bully_high,reg_test$y_log)

#check out the coefficients
coefs_log = coef(model_log)

coefs_log <- data.frame(
  Variable = rownames(as.matrix(coefs_log)),
  Coefficient = as.numeric(coefs_log))

coefs_log %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

# chatGPT told me how to run forward/backward stepwise regression based on
# the logstic regression I just did. Much simpler than the complex
# forward/backward stepwise code I had written out! this runs through all
# the models and picks the one with the lowest AIC
model_step = step(model_log, direction = "both", trace = 0)

summary(model_step)
AIC(model_step)

# let's predict
reg_test$y_step = predict( model_step, 
                           newdata = reg_test, type = "response")

# calculate AUC
auc(reg_test$bully_high, reg_test$y_step)

# add a confusion matrix
reg_test$pred_class <- ifelse(reg_test$y_step > 0.5, 1, 0)
caret::confusionMatrix(factor(reg_test$pred_class), 
                       factor(reg_test$bully_high))

# check out the coefficients
coefs_step = coef(model_step)

coefs_step <- data.frame(
  Variable = rownames(as.matrix(coefs_step)),
  Coefficient = as.numeric(coefs_step))

coefs_step %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

################################################
# now let's run the model on our test data
holdout_data <- read_csv("cleaned_test_data.csv")

# we have to do some of the same cleaning we did for our training data
#remove student_id
holdout_test <- holdout_data[,-1]

# make the character variables factors
holdout_test <- holdout_data %>%
  mutate(across(where(is.character), as.factor))

# align factor levels with training data (chatGPT suggestion)
for (col in names(reg_train)) {
  if (is.factor(reg_train[[col]])) {
    holdout_test[[col]] <- factor(holdout_test[[col]],
                                  levels = levels(reg_train[[col]]))
  }
}

# scale our data using the parameters we used for training data
holdout_test <- holdout_test %>%
  mutate(across(all_of(predictor_cols),
                ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))

holdout_test$predicted_bully_risk = predict( model_step, 
                           newdata = holdout_test, type = "response")

hist(holdout_test$predicted_bully_risk)

holdout_test %>% 
  filter(predicted_bully_risk >0.5) %>% 
  nrow()


#create a new dataframe called preds
preds <- data.frame(student_id = holdout_data$student_id,
                    predicted_bully_risk = holdout_test$predicted_bully_risk)

write.csv(preds, "log_step_risk_predictions.csv", 
                   row.names = FALSE)



