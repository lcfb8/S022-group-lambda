library(tidyverse)
library(skimr)
library(caret)
library(ModelMetrics)


# let's take our top 3 predictors from lasso (discrimination, e_safety_score,
# school_rules) and try loess, why not
# based on the college_and_ses_with_loess script from class

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
