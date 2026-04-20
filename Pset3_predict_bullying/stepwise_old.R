# Stepwise selection
library( tidyverse )
library( MASS )
library( glmnet )
library( ModelMetrics )


set.seed(80107)

cleaned_data = read.csv("cleaned_data.csv")

#remove student_id
cleaned_data <- cleaned_data[,-1]

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
