## data_challenge_skeleton.R
## Data Challenge: Prediction Competition
## Your goal: predict Y for the 10 test points in each dataset.

library(tidyverse)

# ── Load data ──────────────────────────────────────────────────────────────────
# Training data: X and Y are both observed
trainA <- read_csv("data/datasetA.csv", col_types = cols())
trainB <- read_csv("data/datasetB.csv", col_types = cols())
trainC <- read_csv("data/datasetC.csv", col_types = cols())

# Test data: only X is given — you must predict Y
testA <- read_csv("data/testA.csv", col_types = cols())
testB <- read_csv("data/testB.csv", col_types = cols())
testC <- read_csv("data/testC.csv", col_types = cols())


# ── Helper: format your predictions for the Google Form ───────────────────────
# Pass a test dataset that has your predicted Y values in a column called "Y".
# The function sorts by X and prints a comma-separated list to paste into the form.
format_answer <- function(test_with_predictions) {
    cat( "\n\n" )
    cat( "** Your Predictions **\n" )
    stopifnot("Y" %in% names(test_with_predictions))
    out <- test_with_predictions |>
        arrange(X) |>
        mutate(Y = round(Y, 2)) |>
        pull(Y)
    cat( "Paste the following into the google form:\n" )
    cat( "Form location: https://forms.gle/FcdkEx4Pyd6C6m9P9\n" )
    cat(paste(out, collapse = ", "), "\n")
}


# ── Dataset A ─────────────────────────────────────────────────────────────────

trainA
testA

# This is maybe not a good prediction
# M = lm( Y ~ X, trainA )
# testA$Y = predict( M, newdata = testA )

# Let's see what the data looks like

trainA %>% 
    ggplot(aes( X, Y )) +
    geom_point() 
    geom_smooth( method = "loess", span = 0.5, se = FALSE )+
    geom_smooth( method = lm, se = FALSE, col = "red")

# The data looks like noise. How can we predict anything from this??

# Let's make a function for test/train splits
# We'll test out different test/train splits for loess and linear models

train_split_lm = function(data, pct){
N = nrow(data)
val_rows = sample( N, N*pct )

val = data[ val_rows, ]
real_train = data[ -val_rows, ]

M = lm( Y ~ X, real_train )
val$Yhat = predict( M, newdata = val )

val %>%
    summarize( rmse = sqrt( mean( (Yhat - Y)^2 ) ) )

}

train_split_loess = function(data, pct){
    N = nrow(data)
    val_rows = sample( N, N*pct )
    
    val = data[ val_rows, ]
    real_train = data[ -val_rows, ]
    
    M = loess( Y ~ X, real_train )
    val$Yhat = predict( M, newdata = val )
    
    val %>%
        summarize( rmse = sqrt( mean( ((Yhat - Y)^2), na.rm = TRUE ) ) )
    #had to add na.rm = TRUE because I kept randomly getting NAs, but
    #I'm not sure why? didn't get NA for the linear model
}

# let's test out the function for a 50/50 split
train_split_loess(trainA, 0.5)
train_split_lm(trainA, 0.5)

# let's make a list of splits from 10% to 50% 

splits <- seq(from = 0.1, to = 0.5, by = 0.1)

#run the train_split_loess and train_split_lm function for every value of 
#pct in splits, and replicate this 1000 times
#copilot helped me figure out some of this code

splits_rmse_lm <- replicate(1000, map_dfr(splits, ~train_split_lm(trainA, .x)), 
          simplify = TRUE)

splits_rmse_loess <- 
    replicate(1000, map_dfr(splits, ~train_split_loess(trainA, .x)), 
                         simplify = TRUE)

#convert these to a tibble (necessary)?

splits_rmse_lm <- tibble(splits_rmse_lm)
splits_rmse_loess <- tibble(splits_rmse_loess)


#in the splits_rmse tibbles, separate the lists in the splits_rmse column into 
# five columns labeled 0.1, 0.2, 0.3, 0.4, and 0.5
# copilot helped me with this

splits_rmse_lm <- splits_rmse_lm %>% 
    unnest_wider(col = splits_rmse_lm, names_sep = "pct") %>% 
 
    #rename the second through sixth columns to be 0.1, 0.2, 0.3, 0.4, and 0.5
    rename( "0.1" = splits_rmse_lmpct1,
            "0.2" = splits_rmse_lmpct2,
            "0.3" = splits_rmse_lmpct3,
            "0.4" = splits_rmse_lmpct4,
            "0.5" = splits_rmse_lmpct5 )

splits_rmse_loess <- splits_rmse_loess %>% 
    unnest_wider(col = splits_rmse_loess, names_sep = "pct") %>% 
    
    #rename the second through sixth columns to be 0.1, 0.2, 0.3, 0.4, and 0.5
    #I'm sorry, these names are very ugly and go against best practices
    rename( "0.1" = splits_rmse_loesspct1,
            "0.2" = splits_rmse_loesspct2,
            "0.3" = splits_rmse_loesspct3,
            "0.4" = splits_rmse_loesspct4,
            "0.5" = splits_rmse_loesspct5 )

summary(splits_rmse_lm)
summary(splits_rmse_loess)


#OK it seems like a 10/90 split generates the lowest RMSE for both linear and 
#Loess models. Is this a good thing? 0.1 also gives the widest range of rmses.
# I'll go with it. According to my results, mean rmse is usuallly 
# lower for linear (14ish) than for loess (15ish)

# Now let's see which span is best for the loess models
# We can create the split using our code from above

# I have not set a seed on purpose, so I can rerun this and see the variety
# among the results

    N = nrow(trainA)
    val_rows = sample( N, N*0.1 )
    
    val_A = trainA[ val_rows, ]
    real_trainA = trainA[ -val_rows, ]
    
#Now we have our training data split into two sets: real_trainA (90%)
#and 10% to test (val_A)
 

#Let's see which span is best for loess models using our test/train split data
#from trainA. The code below is from lab 8

span_rmse <- function(span = 0.5) {
   
    train <- real_trainA
    test <- val_A
    
    # fit the model with the given span on the TRAINING data
    model <- loess( Y ~ X, data = train, span = span)
    
    # get the predictions with the model on the TESTING data
    test$predictions <- predict(model, newdata = test)
    train$predictions <- predict(model)
    
    # calculate the error for each observation in the test data
    rmse_test <- sqrt(mean((test$predictions - test$Y)^2, na.rm = TRUE))
    rmse_train <- sqrt(mean((train$predictions - train$Y)^2))
    rmse_list = tibble(span = span,
                       test_rmse = rmse_test, 
                       train_rmse = rmse_train)
    #got some NAs for rmse_test a few times so I added na.rm = TRUE
    
    return(rmse_list)
}


# test the function
span_rmse(0.5) 


# now we will create a vector of spans to try 
spans <- seq(from = 0.1, to = 2, by = .05)

# run the function on each value of the spans vector
rmse <- map_dfr(spans, span_rmse) # run the function on each of the spans

#got a bunch of warning messaging about "pseudoinverse used at 29", 
#"neighborhood radius 8," "reciprocal condition number 5.2015e-17"? I think
#loess models don't cope with random selection so well?

# get the best span to minimize RMSE
best.span <- rmse %>% 
    slice_min(test_rmse) %>% 
    pull(span)

best.span

# on running this multiple times I get a wide range of results for the best span
# and the lowest test_rmse. Which I think is just demonstrating that the data 
# is very noisy and the 10/90 test/train split leads to a lot of variation


# indicate whether spans result in under or overfit models
rmse <- rmse %>%
    mutate(span.fit = ifelse(span == best.span, "Best",
                             ifelse(span > best.span, "Underfit", "Overfit")))

# plot the rmses and spans
p5 <- rmse %>% 
    ggplot(aes(x = span, y = test_rmse)) +
    geom_line( color = "grey" ) +
    geom_point( aes(color = span.fit) ) +
    labs(x = "Span", y = "RMSE", color = "Quality of span",
         title = "Span performance on Testing data RMSE")

p5

#ok every time I run this I get vastly different results which I also think
#is because our data is noise. Often, it keeps telling me the best span is 2, 
# which is basically just a linear model. 

# compare train and test rmse
rmsesL <- pivot_longer(rmse, cols = c(train_rmse, test_rmse), 
                       names_to = "dataset",
                       values_to = "rmse")


p6 <- rmsesL %>% 
    ggplot(aes( span, rmse, col = dataset) ) +
    geom_line() +
    labs(x = "Span", y = "RMSE", color = "Dataset",
         title = "Comparing span performance on training vs. test data RMSE") +
    scale_color_manual(values = c("red", "blue"),
                       labels = c("Test RMSE", "Train RMSE"))

p6

#ok this seems weird. But also, the data in trainA is kind of garbage. So
#maybe this is not the best way to practice? 

#anyway, let's try to predict for testA using the loess model with span = 2

trainA %>% 
    ggplot(aes(X,Y)) +
    geom_point() +
    geom_smooth(method = "loess", span = 2, se = FALSE, col = "red") 

#the internet says to add surface = "direct" to extrapolate NAs
M = loess(Y ~ X, trainA, control = 
                    loess.control(surface = "direct"), span = 2)

#calculate RMSE for this model

trainA$predictions <- predict(M)
rmse_train <- sqrt(mean((trainA$predictions - trainA$Y)^2))

round(rmse_train, digits = 2)

#use the model to predict Y values for the testA dataset

testA$Y = predict(M, newdata = testA)

testA

testA %>% 
    ggplot(aes(X,Y))+
    geom_point()

# should it really look like this? 

# Let's try overfitting the model with span = 0.1 
# because garbage in, garbage out

M = loess(Y ~ X, trainA, control = 
              loess.control(surface = "direct"), span = 0.1)

# calculate RMSE for this model

trainA$predictions <- predict(M)
rmse_train <- sqrt(mean((trainA$predictions - trainA$Y)^2))
round(rmse_train, digits = 2)

testA$Y = predict(M, newdata = testA)

testA

testA %>% 
    ggplot(aes(X,Y))+
    geom_point()

# This looks like noise, just like trainA does. Is that what we want? We need
# to decide which to go with

# Once you have your analysis, print out your predictions
format_answer( testA )

# Now cut and paste the above into the google form!


# ── Dataset B ─────────────────────────────────────────────────────────────────

# THIS IS GOING TO BE SO MUCH BETTER BECAUSE WE DON'T HAVE GARBAGE DATA YAY
# Most code is just adapted from part A

trainB
testB

# Let's see what the data looks like

trainB %>% 
    ggplot(aes( X, Y )) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.5, se = FALSE )

# Does not look like it's just noise!! this will obviously be better with loess
# than a linear model

# We'll test out different test/train splits for loess using the function we
# built for part A (code is the same as part A but with trainB data and only
# testing on loess)

train_split_loess(trainB, 0.5)

splits <- seq(from = 0.1, to = 0.5, by = 0.1)

splits_rmse_loess <- 
    replicate(1000, map_dfr(splits, ~train_split_loess(trainB, .x)), 
              simplify = TRUE)

splits_rmse_loess <- tibble(splits_rmse_loess)

splits_rmse_loess

splits_rmse_loess <- splits_rmse_loess %>% 
    unnest_wider(col = splits_rmse_loess, names_sep = "pct") %>% 
    
    #rename the second through sixth columns to be 0.1, 0.2, 0.3, 0.4, and 0.5
    rename( "0.1" = splits_rmse_loesspct1,
            "0.2" = splits_rmse_loesspct2,
            "0.3" = splits_rmse_loesspct3,
            "0.4" = splits_rmse_loesspct4,
            "0.5" = splits_rmse_loesspct5 )

summary(splits_rmse_loess)


# Mean RMSEs are very similar for all the splits. 10/90 is slightly lower,
# so I'll go with that. Is it an issue that 10/90 split shows a much wider
# range of RMSEs, meaning it varies much more in terms of ability to predict?

# Now let's see which span is best for the loess models

# We can create the split using our code from above

N = nrow(trainB)
val_rows = sample( N, N*0.1 )

val_B = trainB[ val_rows, ]
real_trainB = trainB[ -val_rows, ]

# Can I make a function for this so I can replicate it 1000 times, 
# to take into account variation from random sampling?



tt_rep = function(data){
    
    N = nrow(data)
    val_rows = sample( N, N*0.1 )
    
    val_B <- data[ val_rows, ]
    real_trainB <- data[ -val_rows, ]
    
    test_train = bind_rows(val_B, real_trainB)
    
    test_train
}

tt_rep(trainB)

#Now we have our function to split the training data into two sets and put 
#it back together again. We'll split it apart again in the next function. This
#allows us to run many trials (monte carlo simluation-ish) to find the best span


#Let's see which span is best for loess models using our test/train split data
#from trainB. The code below is adapted from lab 8

span_rmse <- function(span) {
    
    test_train <- tt_rep(trainB)
    
    test <- test_train[1:10,]
    train <- test_train[11:100,]
    
    # fit the model with the given span on the TRAINING data
    model <- loess( Y ~ X, data = train, span = span)
    
    # get the predictions with the model on the TESTING data
    test$predictions <- predict(model, newdata = test)
    train$predictions <- predict(model)
    
    # calculate the error for each observation in the test data
    rmse_test <- sqrt(mean((test$predictions - test$Y)^2, na.rm = TRUE))
    rmse_train <- sqrt(mean((train$predictions - train$Y)^2))
    rmse_list = tibble(span = span,
                       test_rmse = rmse_test, 
                       train_rmse = rmse_train)
    
    return(rmse_list)
}


# test the function
span_rmse(0.5) 


# now we will create a vector of spans to try 
spans <- seq(from = 0.1, to = 2, by = .05)

# I asked chatGPT to help me write the following code to replicate the function 
# 10 times and put the output into a tibble (I was using replicate before,
# but it led to a mess of nested vectors)

rmse <- map_dfr(1:100, \(i) {
    map_dfr(spans, span_rmse) %>%
        mutate(rep = i)
})

# in rmse, mutate rep so it's a factor
rmse <- rmse %>% 
    mutate(rep = as.factor(rep))

rmse %>% 
    ggplot(aes(span, test_rmse))+
    geom_point() +
    geom_smooth()



# get the best span to minimize RMSE
best.span <- rmse %>% 
    slice_min(test_rmse) %>% 
    pull(span)

best.span

# I keep getting very different spans every time I run it because of the 
# 90/10 split. Maybe there's too much variation and a 50/50 split would be better

# ran this a few times, best span appears to be around 0.6

# indicate whether spans result in under or overfit models
rmse <- rmse %>%
    mutate(span.fit = ifelse(span == best.span, "Best",
                             ifelse(span > best.span, "Underfit", "Overfit")))

# plot the rmses and spans
p5 <- rmse %>% 
    ggplot(aes(x = span, y = test_rmse)) +
    geom_line( color = "grey" ) +
    geom_point( aes(color = span.fit) ) +
    labs(x = "Span", y = "RMSE", color = "Quality of span",
         title = "Span performance on Testing data RMSE")

p5

# compare train and test rmse
rmsesL <- pivot_longer(rmse, cols = c(train_rmse, test_rmse), 
                       names_to = "dataset",
                       values_to = "rmse")


p6 <- rmsesL %>% 
    ggplot(aes( span, rmse, col = dataset) ) +
    geom_line() +
    labs(x = "Span", y = "RMSE", color = "Dataset",
         title = "Comparing span performance on training vs. test data RMSE") +
    scale_color_manual(values = c("red", "blue"),
                       labels = c("Test RMSE", "Train RMSE"))

p6


# Let's predict for testB using the loess model with span = 0.6

trainB %>% 
    ggplot(aes(X,Y)) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.6, se = FALSE) 

#the internet says to add surface = "direct" to extrapolate NAs
M = loess(Y ~ X, trainB, span = 0.6, control = 
              loess.control(surface = "direct"))

#caculate rmse for this model
trainB$predictions <- predict(M)
rmse_train <- sqrt(mean((trainB$predictions - trainB$Y)^2))
round(rmse_train, digits = 2)

testB$Y = predict(M, newdata = testB)

testB

testB %>% 
    ggplot(aes(X,Y))+
    geom_point()

# this looks pretty good

# Once you have your analysis, print out your predictions
format_answer( testB )

# Now cut and paste the above into the google form!


# ── Dataset C ─────────────────────────────────────────────────────────────────

# Most code is just adapted from parts A and B

trainC
testC

# Let's see what the data looks like

trainC %>% 
    ggplot(aes( X, Y )) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.5, se = FALSE ) +
    geom_smooth( method = "lm", col = "red", se = FALSE)

# Looks pretty linear, but could maybe also use loess. I'll try both.

# We'll test out different test/train splits for loess and linear using the 
# function created in part A

# let's test out the function for a 50/50 split
train_split_loess(trainC, 0.5)
train_split_lm(trainC, 0.5)

# let's make a list of splits from 10% to 50% 

splits <- seq(from = 0.1, to = 0.5, by = 0.1)

#run the train_split_loess and train_split_lm function for every value of 
#pct in splits, and replicate this 1000 times
#copilot helped me figure out some of this code

#if this code is running super slowly, change the 1000 to 100
splits_rmse_lm <- replicate(1000, map_dfr(splits, ~train_split_lm(trainC, .x)), 
                            simplify = TRUE)

splits_rmse_loess <- 
    replicate(1000, map_dfr(splits, ~train_split_loess(trainC, .x)), 
              simplify = TRUE)

#convert these to a tibble (necessary)?

splits_rmse_lm <- tibble(splits_rmse_lm)
splits_rmse_loess <- tibble(splits_rmse_loess)

splits_rmse_loess

#in the splits_rmse tibbles, separate the lists in the splits_rmse column into 
# five columns labeled 0.1, 0.2, 0.3, 0.4, and 0.5

splits_rmse_lm <- splits_rmse_lm %>% 
    unnest_wider(col = splits_rmse_lm, names_sep = "pct") %>% 
    
    #rename the second through sixth columns to be 0.1, 0.2, 0.3, 0.4, and 0.5
    rename( "0.1" = splits_rmse_lmpct1,
            "0.2" = splits_rmse_lmpct2,
            "0.3" = splits_rmse_lmpct3,
            "0.4" = splits_rmse_lmpct4,
            "0.5" = splits_rmse_lmpct5 )

splits_rmse_loess <- splits_rmse_loess %>% 
    unnest_wider(col = splits_rmse_loess, names_sep = "pct") %>% 
    
    #rename the second through sixth columns to be 0.1, 0.2, 0.3, 0.4, and 0.5
    #I'm sorry, these names are very ugly and go against best practices
    rename( "0.1" = splits_rmse_loesspct1,
            "0.2" = splits_rmse_loesspct2,
            "0.3" = splits_rmse_loesspct3,
            "0.4" = splits_rmse_loesspct4,
            "0.5" = splits_rmse_loesspct5 )

summary(splits_rmse_lm)
summary(splits_rmse_loess)


# Like for B, mean RMSEs are very similar for all the splits. 
# 10/90 is slightly lower, so I'll go with that. 

# Now let's see which span is best for the loess models

# We can create the split using our code from above

N = nrow(trainC)
val_rows = sample( N, N*0.1 )

val_C = trainC[ val_rows, ]
real_trainC = trainC[ -val_rows, ]

#Now we have our training data split into two sets: real_trainC (90%)
#and 10% to test (val_C)


#Let's see which span is best for loess models using our test/train split data
#from trainC. The code below is from lab 8

span_rmse <- function(span = 0.5) {
    
    train <- real_trainC
    test <- val_C
    
    # fit the model with the given span on the TRAINING data
    model <- loess( Y ~ X, data = train, span = span)
    
    # get the predictions with the model on the TESTING data
    test$predictions <- predict(model, newdata = test)
    train$predictions <- predict(model)
    
    # calculate the error for each observation in the test data
    rmse_test <- sqrt(mean((test$predictions - test$Y)^2))
    rmse_train <- sqrt(mean((train$predictions - train$Y)^2))
    rmse_list = tibble(span = span,
                       test_rmse = rmse_test, 
                       train_rmse = rmse_train)
    
    return(rmse_list)
}


# test the function
span_rmse(0.5) 

# now we will create a vector of spans to try 
spans <- seq(from = 0.1, to = 2, by = .05)

# run the function on each value of the spans vector
rmse <- map_dfr(spans, span_rmse) # run the function on each of the spans

# got a lot of warnings hm..

# get the best span to minimize RMSE
best.span <- rmse %>% 
    slice_min(test_rmse) %>% 
    pull(span)

best.span


# ran this multiple times and got a bunch of warnings and a bunch of different 
# "best" spans, which makes me think loess might not be the best method

# Let's predict for testC using a linear model

trainC %>% 
    ggplot(aes(X,Y)) +
    geom_point() +
    geom_smooth(method = lm) 

M = lm(Y ~ X, trainC)

#caculate rmse for this model
trainC$predictions <- predict(M)
rmse_train <- sqrt(mean((trainC$predictions - trainC$Y)^2))
round(rmse_train, digits = 2)

testC$Y = predict(M, newdata = testC)

testC

testC %>% 
    ggplot(aes(X,Y))+
    geom_point()

# this looks very linear. I think that's a good thing?

# Once you have your analysis, print out your predictions
format_answer( testC )

# Now cut and paste the above into the google form!


