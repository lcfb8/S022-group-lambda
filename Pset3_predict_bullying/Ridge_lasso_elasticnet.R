library(tidyverse)
library(skimr)
library(caret)     
library(glmnet) 
library(gridExtra)

## Based on Lab 11

set.seed(80107)

cleaned_data = read.csv("cleaned_data.csv")

#remove student_id
cleaned_data <- cleaned_data[,-1]

dim(cleaned_data)

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
  select(-temp_id)

# Assign everything else to the test dataset 
test <- cleaned_data %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  select(-temp_id)

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

train_control <- trainControl(
  method = "cv",
  number = 10,
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
sqrt( mean((rle.pred-y_test)^2) )

# Now let's repeat with the glmnet version using lambda 1se
rle.pred.glmnet = predict(rle_model$finalModel, 
                          s = best_lambda_glmnet, newx=x_test)


# How variable are our predictions?
summary( rle.pred.glmnet )
sd( rle.pred.glmnet )

# The RMSE
sqrt( mean((rle.pred.glmnet-y_test)^2) )

# both of these RMSEs are quite low

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

#now let's take our top 3 predictors and try loess, why not
#based on the college_and_ses_with_loess script from class

dat = train
head( dat )

qplot( dat$e_safety_score )

dat = dat %>% 
  mutate(bully_high = ifelse(bully >= 2.5,1,0))

test = test %>% 
  mutate(bully_high = ifelse(bully >=2.5,1,0))


##
## Looking at probability of =>2.5 bully score given e_safety_score
##

table( dat$bully_high )


ggplot( dat, aes( e_safety_score, bully_high ) ) +
  geom_point()
#wow this does not look promising

summary( dat$e_safety_score )
q = quantile( dat$e_safety_score, c( 0.10, 0.90 ) )
q


ll = loess( bully_high ~ e_safety_score, data=dat )
ll


# We can predict using our model, using it like a function.  
# Here, for e_safety_score = 2, we have this:
predict( ll, 2 )


preds = tibble( e_safety_score = seq( 1, 4, length.out=100 ),
                fitted = predict( ll, e_safety_score ) )
preds


ggplot( preds, aes( e_safety_score, fitted ) ) +
  geom_line()


# using binning
dat <- dat %>% mutate( cut = cut( e_safety_score, 10 ) ) %>%
  group_by( cut ) %>%
  mutate( binned = mean( bully_high ) )

ggplot( dat, aes( e_safety_score, binned ) ) +
  geom_line() +
  geom_line( data=preds, aes(e_safety_score,fitted), col="red" )



####  Look at loess lines for different subsets of folks  #####
# identify where most of the data is

ggplot( data=dat, aes( e_safety_score, bully_high ) ) +
  geom_point() +
  geom_smooth( data=filter( dat, gender=="Female" ), col="purple", se=FALSE ) +
  geom_smooth( data=filter( dat, gender=="Male" ), col="darkgreen", se=FALSE ) +
  geom_smooth( data=filter( dat, gender=="Another way:" ), col="orange", se=FALSE )+
  geom_vline( xintercept = q, lwd=1, lty=2 )


##### Loess with multiple variables   ####

ggplot( dat, aes( discrimination, school_rules ) ) +
  geom_point( alpha=0.2 )

# use loess with multiple variables to predict bully_high based on e_safety_score,
# discrimination, and school_rules

llp = loess( bully_high ~ e_safety_score + 
               discrimination + school_rules, data=dat )

dat$pcol = predict( llp, newdata=dat )

qplot( e_safety_score, pcol, data = dat ) +
  geom_smooth()
qplot( discrimination, pcol, data = dat ) +
  geom_smooth()
qplot( school_rules, pcol, data = dat ) +
  geom_smooth()

rmse(llp, data = dat)

# let's run it on the test data now

test$pcol = predict( llp, newdata=test )

qplot( e_safety_score, pcol, data = test ) +
  geom_smooth()
qplot( discrimination, pcol, data = test ) +
  geom_smooth()
qplot( school_rules, pcol, data = test ) +
  geom_smooth()

rmse(llp, data = test)

test %>% 
  ggplot(aes(pcol))+
  geom_histogram()

