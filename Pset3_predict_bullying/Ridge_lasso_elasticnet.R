library(tidyverse)
library(skimr)
library(caret)     
library(glmnet) 
library(gridExtra)
library(ModelMetrics)

## Based on Lab 11

set.seed(80107)

cleaned_data = read.csv("cleaned_data.csv")

#remove student_id
cleaned_data <- cleaned_data[,-1]

# dim(cleaned_data)

# skim(cleaned_data)

# Create a data partition such that 80% of the data is used for training and 
# 20% for testing

trainIndex <- createDataPartition(cleaned_data$bully,
                                  p = .8, # the share of data in the training set
                                  list = FALSE # puts our output in matrix form
)

cleaned_data <- cleaned_data %>% 
  mutate(temp_id = 1:nrow(cleaned_data))

# Use those indices to split the data into training and test sets
train <- cleaned_data %>% 
  filter(temp_id %in% trainIndex) %>% 
  dplyr::select(-temp_id)


# Assign everything else to the test dataset 
test <- cleaned_data %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

# Check how many rows are in each set
nrow(train)
nrow(test)

# Convert predictors into a model matrix for glmnet
# model.matrix() automatically creates dummy variables for categorical predictors
# math ~ . means: predict math using all other variables in the dataset
# [,-1] removes the intercept column because glmnet handles the intercept internally
x_train_unscaled <- model.matrix(bully ~ ., train)[, -1]
y_train <- train$bully

x_test_unscaled <- model.matrix(bully ~ ., test)[, -1]
y_test <- test$bully

# Standardize the predictors in the training set
x_train <- scale(x_train_unscaled)

# Standardize the test set using the SAME means and standard deviations from 
# the training set
x_test <- scale(
  x_test_unscaled,
  center = attr(x_train, "scaled:center"),
  scale = attr(x_train, "scaled:scale")
)

###### now we'll test ridge, lasso, and elastic net all in one
# using 5 folds for cv since our dataset only has 5% bully >= 2.5

train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE 
)

# Define a set of alpha values to try. We'll include 0 and 1 to test ridge and
# lasso along with the other values for elastic net

alpha_grid <- seq(0, 1, by = 0.1) 
lambda_grid <- 10 ^ seq(4, -4, length = 100)

set.seed(80107)

rle_model <- train(
  x = x_train,
  y = y_train,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda_grid),
  metric = "RMSE"
)


# Plot RMSE across tuning values
plot(rle_model, xTrans = log)

# View the best alpha
best_alpha <- rle_model$bestTune$alpha
best_alpha

# View what caret says is the best lambda (lowest RMSE)
best_lambda_caret <- rle_model$bestTune$lambda
best_lambda_caret

# ChatGPT helped me understand that caret and glmnet packages have different
# ways of finding the best lambda. Let's look at the glmnet version
cv.out <- cv.glmnet(x_train, y_train, alpha=best_alpha)
plot(cv.out)

cv.out

#what is the Rsquared for rle_model with best_alpha
rle_model$results %>% 
  filter(alpha == best_alpha) %>% 
  arrange(RMSE) %>% 
  head(1)

# lambda min has ~69 nonzero coefs and lambda 1se has ~25 nonzero coefs
# lambda min of 0.01 matches caret, but lambda 1se of 0.08 is different

# let's store this best lambda (1se) as well
best_lambda_glmnet <- cv.out$lambda.1se
best_lambda_glmnet
log(best_lambda_glmnet)

# Extract coefficients at the best lambda for both methods
coefs_caret <- coef(rle_model$finalModel, 
                    alpha = best_alpha, s = best_lambda_caret)

coefs_glmnet <- coef(rle_model$finalModel, 
                    alpha = best_alpha, s = best_lambda_glmnet)

coefs_caret <- coefs_caret[which(coefs_caret != 0),]

coefs_glmnet <- coefs_glmnet[which(coefs_glmnet != 0),]

coefs_cdf <- data.frame(
  Variable = rownames(as.matrix(coefs_caret)),
  Coefficient = as.numeric(coefs_caret)
)

coefs_gdf <- data.frame(
  Variable = rownames(as.matrix(coefs_glmnet)),
  Coefficient = as.numeric(coefs_glmnet)
)


# Inspect largest coefficients
coefs_cdf %>%
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

coefs_gdf %>%
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

# Now lets run our best model on the testing data
# First let's try the caret version (best lambda minimizes RMSE)
rle.pred.caret = predict(rle_model, newdata=x_test)

# How variable are our predictions?
summary( rle.pred.caret )
sd( rle.pred.caret )

# The RMSE
sqrt( mean((rle.pred.caret-y_test)^2) )

# Now let's repeat with the glmnet version using lambda 1se
rle.pred.glmnet = predict(rle_model$finalModel, 
                          s = best_lambda_glmnet, newx=x_test)


# How variable are our predictions?
summary( rle.pred.glmnet )
sd( rle.pred.glmnet )

# The RMSE
sqrt( mean((rle.pred.glmnet-y_test)^2) )

# both of these RMSEs are around 0.41

# Let's look at a plot

#put both sets of predictions into the test df
test_plot <- test %>% 
  mutate(y_pred_caret = rle.pred.caret, y_pred_glmnet = rle.pred.glmnet)

test_plot <- test_plot %>% 
  pivot_longer(cols = c(y_pred_glmnet,y_pred_caret, bully), names_to = "model", 
               values_to = "y")

test_plot %>% 
  ggplot(aes(age,y, col = model))+
  geom_point()+
  geom_jitter()

test_plot %>% 
  ggplot(aes(model,y)) +
  geom_boxplot()


#########################################################################

# Forward/backward stepwise selection (based on ch 24 of the class website)

# Baseline linear model (OLS)

model_linear <- lm(bully ~ ., data = train)

test$y_hat = predict( model_linear, newdata = test)

#calculate RMSE
sqrt(mean((test$bully-test$y_hat)^2))

#check out the coefficients
coefs_lm = coef(model_linear)

coefs_lm <- data.frame(
  Variable = rownames(as.matrix(coefs_lm)),
  Coefficient = as.numeric(coefs_lm)
)

coefs_lm %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

# Stepwise selection: forward

library(MASS)

# set up simplest and most complex to consider:
# `~ 1` is the "intercept only" model.  Our max model 
# has everything (but no interactions).
mod_simple <- lm(bully ~ 1, data = train)
mod_max <- lm(bully ~ ., data = train)

# use forward stepwise selection to pick an optimal model (in terms of AIC)
mod_forward <- stepAIC(mod_simple,
                       scope = list(lower = formula(mod_simple),
                                    upper = formula(mod_max)),
                       direction = "forward",
                       trace = 0 )

summary(mod_forward)

#We can examine how many coefficients we zeroed out with this approach:
# Total number of coefficients (minus intercept)
length( coef( model_linear ) ) - 1

length(coef(mod_forward)) - 1

test$y_hat = predict( mod_forward, newdata = test)

#let's check out the coefficients
coefs_forward = coef(mod_forward)

coefs_forward <- data.frame(
  Variable = rownames(as.matrix(coefs_forward)),
  Coefficient = as.numeric(coefs_forward)
)

coefs_forward %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

#calculate rmse
sqrt(mean((test$bully-test$y_hat)^2))

#let's try backward 
#chatGPT helped me understand how to code this since it isn't on the website

mod_backward <- stepAIC(mod_max,
                       direction = "backward",
                       trace = 0 )

summary(mod_backward)

#We can examine how many coefficients we zeroed out with this approach:
# Total number of coefficients (minus intercept)
length( coef( model_linear ) ) - 1

length(coef(mod_backward)) - 1

test$y_hat = predict( mod_backward, newdata = test)


#let's check out the coefficients
coefs_backward= coef(mod_backward)

coefs_backward <- data.frame(
  Variable = rownames(as.matrix(coefs_backward)),
  Coefficient = as.numeric(coefs_backward)
)

coefs_backward %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(10)

#calculate rmse
sqrt(mean((test$bully-test$y_hat)^2))

#compare AIC for all models
AIC(model_linear)
AIC(mod_forward)
AIC(mod_backward)

#forward model has slightly fewer nonzero coefficients and slightly lower AIC,
#so let's go with that

test$y_hat = predict( mod_forward, newdata = test)

table(test$y_hat >= 2.5)

table(test$bully >= 2.5)

test %>% 
  ggplot(aes(bully,y_hat))+
  geom_point()

#########################################################################
# now let's take our top 3 predictors and try loess, why not
 #based on the college_and_ses_with_loess script from class

dat = train
head( dat )

dat = dat %>% 
  mutate(bully_high = ifelse(bully >= 2.5,1,0))

dat_test = test %>% 
  mutate(bully_high = ifelse(bully >=2.5,1,0)) 

# use loess with multiple variables to predict bully based on e_safety_score,
# discrimination, and school_rules

llp = loess( bully ~ e_safety_score + 
               discrimination + school_rules, data=dat )

summary(llp)

dat$pcol = predict( llp, newdata=dat )

qplot( e_safety_score, pcol, data = dat ) +
  geom_smooth()
qplot( discrimination, pcol, data = dat ) +
  geom_smooth()
qplot( school_rules, pcol, data = dat ) +
  geom_smooth()

rmse(llp, data = dat)

# let's run it on the test data now

dat_test$pcol = predict( llp, newdata=dat_test )

qplot( e_safety_score, pcol, data = dat_test ) +
  geom_smooth()
qplot( discrimination, pcol, data = dat_test ) +
  geom_smooth()
qplot( school_rules, pcol, data = dat_test ) +
  geom_smooth()

rmse(llp, data = dat_test)

dat_test %>% 
  ggplot(aes(pcol))+
  geom_histogram()

dat_test %>% 
  ggplot(aes(bully,pcol))+
  geom_point()

dat_test %>% 
  summarise( bully_pct = mean(bully >= 2.5), pcol_pct = mean(pcol >= 2.5) )

summary(dat_test$pcol)


#####

llp_binary = loess( bully_high ~ e_safety_score + 
               discrimination + school_rules, data=dat )

dat$pcol = predict( llp_binary, newdata=dat )

rmse(llp_binary, data = dat)

# let's run it on the test data now

dat_test$pcol = predict( llp_binary, newdata=dat_test )

rmse(llp_binary, data = dat_test)

# from Luciana's logistic regression code: probabilities for "1" class
# hard classifications using 0.5 threshold
dat_test$y_hat <- ifelse(dat_test$pcol >= 0.5, 1, 0)

dat_test %>% 
  summarize( mean(y_hat == bully_high))

table(dat_test$y_hat)
# predicted 8 out of 1533 students above the 2.5 threshold for bullying
table(dat_test$bully_high)
# in reality there are 70
# :(

