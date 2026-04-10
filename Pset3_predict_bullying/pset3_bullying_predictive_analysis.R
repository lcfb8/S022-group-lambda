# Pset 3: Predicting Bullying
# Authors: Lambda team


# There are several machine learning methods you might consider.
# Here is a list of options we have discussed in class, in the order we learned about them:
#   
# * KNN classification and regression
# * Classification or regression trees
# * Classification or regression random forests
# * Forward/backward stepwise regression (treating the binary outcome as a continuous one)
# * Ridge regression
# * The Lasso (sparse regression) 

# * 10% of your grade will be whether your code is submitted, nicely formatted, and easy to read.

# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)


survey <- read.csv("data/student_survey_data.csv")
skim(survey)

nrow(survey)
# survey_clean = na.omit( survey )
# nrow(survey_clean)
# if we drop all n.a. we end up from 7674 students, we end up with 2

# So we'll drop n.a. as we go or use imputation? or mice?

# Most of the code below is from Lab10, but we will modify it to fit our needs.

# see if any variables in our dataset have close to zero variability 
nearZeroVar(survey, saveMetrics = TRUE)

# check variable types
map_chr(survey, typeof)

# some of the varibales are written text, so we'll drop all the variables with 
# "text" in the name, since they won't be useful for our analysis, drop student_id
# and finished

#survey_clean <- survey %>%
#  select(-contains("text"))

survey_clean <- survey %>%
  select(-student_id, -finished, -ends_with("_text"))

type <- map_chr(survey_clean, typeof) %>%
  enframe(name = "var", value = "type")

# examine any missingness
sum(complete.cases(survey_clean)) == nrow(survey_clean)

survey_clean %>%
  skim()

# there's lots of missing data, so we should decide what to do with it.
####
# The following chunk of code comes from mice_to_impute.R (provided in the data folder)
library( mice )


# Let's remove the long text stuff
classes = map_chr( survey_clean, class )
table( classes )
names(survey_clean)[ classes == "character" ]

sample( survey_clean$talk_text, 30 )
survey_clean = dplyr::select( survey_clean, !ends_with("_text" ) )

classes = map_chr( survey_clean, class )
table( classes )
names(survey_clean)[ classes == "character" ]
sample( survey_clean$school_values_red, 20 )
sample( survey_clean$parents_know, 20 )

# convert character to factor
survey_clean <- survey_clean %>% mutate( across( where( is.character ), factor ) )
survey_clean

levels <- map_dbl( survey_clean, function( x ) { length( unique( x ) ) } )
levels[ classes == "character" ]

# These all look like reasonable categorical variables.  So we now
# want to impute missing values.



#### Doing imputation #####

# First, mice will automatically detect and handle different types data if you
# don't tell it not to.

# We can see what it wants to do:
# To do this, use mice with maxit=0 to get what the defaults are first, and
# then we can easily change them and run mice for real.
imp <- mice(survey_clean, maxit = 0 )
imp$loggedEvents
table( imp$method )
meths <- imp$method

# The following settings would do simpler imputation methods.  The
# first samples a random value.  The second does mean imputation.

# meths[ classes == "character" ] = "sample"
# meths[ meths == "pmm" ] = "mean"

table( meths, classes )

which( meths == "" )
skimr::skim( survey_clean$tell_principal )
# It is setting method to "" for those with no missing data


# KEY LINE: create imputed data set
imp <- mice(survey_clean, m = 1, maxit = 1, method=meths)     

# This will default to 5 chained equation passes.  maxit specifies the
# number of iterations.  For the regression methods, first pass gets
# imputed values, then we are using those to better impute everything,
# and so on.  5 seems to make people happy.

imp$loggedEvents

full = mice::complete(imp)  # get the imputed data set
full = as_tibble(full)

nrow(full)

saveRDS( full, "imputed_data.rds" )


# Explore and check
table( survey_clean$esafe3, useNA = "always" )
table( full$esafe3, useNA = "always" )

table( survey_clean$gender, useNA = "always" )
table( full$gender, useNA = "always" )

table( survey_clean$age, useNA = "always" )
table( full$age, useNA = "always" )




####

full %>%
  is.na() %>%
  colSums()



# set a seed for reproducibility
set.seed(0330)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(full$bully,
                                  p = .5, # proportion of data to use for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- full %>% 
  mutate(temp_id = 1:nrow(survey_clean))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  select(-temp_id)

# compare these to `test` and `train`:
summary(train$bully)
summary(test$bully)

# results are very similar! :)


modelLookup(model = "knn")

# what is the tuning parameter for rf?

modelLookup(model = "rf")

# Reset the seed
set.seed(0330)

ctrl <- trainControl(method = "cv", number = 10)

# now we train the model using cross-validation:
cv_mod <- train(
  bully ~ ., data = train, method = "rf", trControl = ctrl, ntree = 200, tuneLength = 10
)

# 
cv_mod
# 
saveRDS(cv_mod, file = "data/cv_mod_regression.rds")

# I have saved the model to make this a bit quicker for us in lab:
cv_mod = readRDS("data/cv_mod_regression.rds")

# get a summary 
cv_mod

cv_mod_results <- cv_mod$results

ggplot(cv_mod_results, aes(x = mtry, y = RMSE)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

# 97 seems to be the best mtry value: 0.3927957
# but we'll have to choose variables or create composite to decrease the number of
# predictors.

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
  labs(title = "Top 50 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


train <- train %>%
  mutate(rf_pred = predict(cv_mod, train)) 

# caret has a built-in RMSE function, but it's also easy to calculate by hand:
RMSE(train$bully, train$rf_pred)
sqrt(mean((train$bully - train$rf_pred)^2))

# [1] 0.1676409

# now for test data:
test <- test %>%
  mutate(rf_pred = predict(cv_mod, test))

RMSE(test$bully, test$rf_pred)
sqrt(mean((test$bully - test$rf_pred)^2))

# 0.3941006