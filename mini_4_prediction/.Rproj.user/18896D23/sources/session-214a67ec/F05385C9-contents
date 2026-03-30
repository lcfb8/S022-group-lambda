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

## set seed
set.seed(1234)


n <- nrow(trainA)
train_ids <- sample(seq_len(n),
                    size = floor(0.9 * n),
                    replace = FALSE)

train_90 <- trainA[train_ids, ]
test_10  <- trainA[-train_ids, ]


p2 <- ggplot(train_90, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "", 
       title = "Training data")

p3 <- ggplot(test_10, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "",
       title = "Testing data")

grid.arrange(p2, p3, ncol = 2)

span_m1 <- .5
span_m2 <- .8
span_m3 <- 1.4

mA1 <- loess(Y ~ X, data = train_90, span = span_m1)
mA2 <- loess(Y ~ X, data = train_90, span = span_m2)
mA3 <- loess(Y ~ X, data = train_90, span = span_m3)
Mlm = lm( Y ~ X, train_90 )


train_90 %>%
  ggplot(aes(y = Y, x = X)) +
  geom_point() +
  geom_smooth(span = span_m1, method = "loess", se = FALSE, aes(color = "Span 0.5")) + 
  geom_smooth(span = span_m2, method = "loess", se = FALSE, aes(color = "Span 0.8")) + 
  geom_smooth(span = span_m3, method = "loess", se = FALSE, aes(color = "Span 1.4")) + 
  geom_smooth(aes(color = "Linear model"), method = "lm", se = FALSE) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "orange"),
                     name = "Model")

train_90 <- train_90 %>%
  mutate(pred_m1 = predict(mA1),
         pred_m2 = predict(mA2),
         pred_m3 = predict(mA3),
         pred_mlm = predict(Mlm))


# get the error for each observation
train_90 <- train_90 %>%
  mutate(error_m1 = Y - pred_m1,
         error_m2 = Y - pred_m2,
         error_m3 = Y - pred_m3,
         error_mlm = Y - pred_mlm)

# calculate the RMSE for each model
m1_rmse_train <- sqrt(mean(train_90$error_m1^2))
m2_rmse_train <- sqrt(mean(train_90$error_m2^2))
m3_rmse_train <- sqrt(mean(train_90$error_m3^2))
mlm_rmse_train <- sqrt(mean(train_90$error_mlm^2))

# compare train RMSE
train_rmse <- tibble(span = c( span_m1, span_m2, span_m3, "linear" ),
                     train_rmse = c( m1_rmse_train, m2_rmse_train, 
                                     m3_rmse_train, mlm_rmse_train)
)

knitr::kable(train_rmse, digits = 2)

# Once you have your analysis, print out your predictions
format_answer( train_90 )

# Best span:

# create a function that takes a span and returns the RMSE on the test data 
# after fitting a loess on the training data with the given span 

get_rmse <- function(span = 0.9) {
  
  test <- test_10
  train <- train_90
  
  # fit the model with the given span on the TRAINING data
  model <- loess(Y ~ X, data = train, span = span)
  
  # get the predictions with the model on the TESTING data
  test$predictions <- predict(model, newdata = test)
  train$predictions <- predict(model)
  
  # keep only rows with non-missing predictions and Y
  idx_test  <- !is.na(test$predictions)  & !is.na(test$Y)
  idx_train <- !is.na(train$predictions) & !is.na(train$Y)
  
  # calculate the error for each observation in the test data
  rmse_test <- sqrt(mean((test$predictions[idx_test] - test$Y[idx_test])^2))
  rmse_train <- sqrt(mean((train$predictions[idx_train] - train$Y[idx_train])^2))
  rmse_list = tibble(span = span,
                     test_rmse = rmse_test, 
                     train_rmse = rmse_train)
  
  return(rmse_list)
}

# test to make sure it matches
get_rmse(0.9) 

get_rmse(1.5) 

# now we will create a vector of spans to try 
spans <- seq(from = 0.5, to = 2, by = .05)

# run the function on each value of the spans vector
rmse <- map_dfr(spans, get_rmse) # run the function on each of the spans

# get the best span to minimize RMSE
best.span <- rmse %>% 
  slice_min(test_rmse) %>% 
  pull(span)

best.span

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


#### not sure what to do with this:

# pick rows for validation
N = nrow(trainA)
val_rows = sample( N, N*0.5 )

val_A = trainA[ val_rows, ]
real_trainA = trainA[ -val_rows, ]

M = lm( Y ~ X, real_trainA )
val_A$Yhat = predict( M, newdata = val_A )

val_A %>%
    summarize( rmse = sqrt( mean( (Yhat - Y)^2 ) ) )



# ── Dataset B ─────────────────────────────────────────────────────────────────

testB$Y = 0

trainB
testB

## set seed
set.seed(1234)


n <- nrow(trainB)
train_ids <- sample(seq_len(n),
                    size = floor(0.9 * n),
                    replace = FALSE)

train_90B <- trainB[train_ids, ]
test_10B  <- trainB[-train_ids, ]


p2B <- ggplot(train_90B, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "", 
       title = "Training data")

p3B <- ggplot(test_10B, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "",
       title = "Testing data")

grid.arrange(p2B, p3B, ncol = 2)

span_m1 <- .5
span_m2 <- .8
span_m3 <- 1.4

mB1 <- loess(Y ~ X, data = train_90B, span = span_m1)
mB2 <- loess(Y ~ X, data = train_90B, span = span_m2)
mB3 <- loess(Y ~ X, data = train_90B, span = span_m3)
MBlm = lm( Y ~ X, train_90B )


train_90B %>%
  ggplot(aes(y = Y, x = X)) +
  geom_point() +
  geom_smooth(span = span_m1, method = "loess", se = FALSE, aes(color = "Span 0.5")) + 
  geom_smooth(span = span_m2, method = "loess", se = FALSE, aes(color = "Span 0.8")) + 
  geom_smooth(span = span_m3, method = "loess", se = FALSE, aes(color = "Span 1.4")) + 
  geom_smooth(aes(color = "Linear model"), method = "lm", se = FALSE) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "orange"),
                     name = "Model")

train_90B <- train_90B %>%
  mutate(pred_m1 = predict(mB1),
         pred_m2 = predict(mB2),
         pred_m3 = predict(mB3),
         pred_mlm = predict(MBlm))


# get the error for each observation
train_90B <- train_90B %>%
  mutate(error_m1 = Y - pred_m1,
         error_m2 = Y - pred_m2,
         error_m3 = Y - pred_m3,
         error_mlm = Y - pred_mlm)

# calculate the RMSE for each model
m1_rmse_train <- sqrt(mean(train_90B$error_m1^2))
m2_rmse_train <- sqrt(mean(train_90B$error_m2^2))
m3_rmse_train <- sqrt(mean(train_90B$error_m3^2))
mlm_rmse_train <- sqrt(mean(train_90B$error_mlm^2))

# compare train RMSE
train_rmse <- tibble(span = c( span_m1, span_m2, span_m3, "linear" ),
                     train_rmse = c( m1_rmse_train, m2_rmse_train, 
                                     m3_rmse_train, mlm_rmse_train)
)

knitr::kable(train_rmse, digits = 2)

# Once you have your analysis, print out your predictions
format_answer( train_90B )

# Best span:

# create a function that takes a span and returns the RMSE on the test data 
# after fitting a loess on the training data with the given span 

get_rmse <- function(span = 0.9) {
  
  test <- test_10B
  train <- train_90B
  
  # fit the model with the given span on the TRAINING data
  model <- loess(Y ~ X, data = train, span = span)
  
  # get the predictions with the model on the TESTING data
  test$predictions <- predict(model, newdata = test)
  train$predictions <- predict(model)
  
  # keep only rows with non-missing predictions and Y
  idx_test  <- !is.na(test$predictions)  & !is.na(test$Y)
  idx_train <- !is.na(train$predictions) & !is.na(train$Y)
  
  # calculate the error for each observation in the test data
  rmse_test <- sqrt(mean((test$predictions[idx_test] - test$Y[idx_test])^2))
  rmse_train <- sqrt(mean((train$predictions[idx_train] - train$Y[idx_train])^2))
  rmse_list = tibble(span = span,
                     test_rmse = rmse_test, 
                     train_rmse = rmse_train)
  
  return(rmse_list)
}

# test to make sure it matches
get_rmse(0.9) 

get_rmse(1.5) 

# now we will create a vector of spans to try 
spans <- seq(from = 0.5, to = 2, by = .05)

# run the function on each value of the spans vector
rmse <- map_dfr(spans, get_rmse) # run the function on each of the spans

# get the best span to minimize RMSE
best.span <- rmse %>% 
  slice_min(test_rmse) %>% 
  pull(span)

best.span

# indicate whether spans result in under or overfit models
rmse <- rmse %>%
  mutate(span.fit = ifelse(span == best.span, "Best",
                           ifelse(span > best.span, "Underfit", "Overfit")))

# plot the rmses and spans
p5B <- rmse %>% 
  ggplot(aes(x = span, y = test_rmse)) +
  geom_line( color = "grey" ) +
  geom_point( aes(color = span.fit) ) +
  labs(x = "Span", y = "RMSE", color = "Quality of span",
       title = "Span performance on Testing data RMSE")

p5B


format_answer(test_10B)




# ── Dataset C ─────────────────────────────────────────────────────────────────

testC$Y = 0


trainC
testC

## set seed
set.seed(1234)


n <- nrow(trainC)
train_ids <- sample(seq_len(n),
                    size = floor(0.9 * n),
                    replace = FALSE)

train_90C <- trainC[train_ids, ]
test_10C  <- trainC[-train_ids, ]


p2C <- ggplot(train_90C, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "", 
       title = "Training data")

p3C <- ggplot(test_10C, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "",
       y = "",
       title = "Testing data")

grid.arrange(p2C, p3C, ncol = 2)

span_m1 <- .5
span_m2 <- .8
span_m3 <- 1.4

mC1 <- loess(Y ~ X, data = train_90C, span = span_m1)
mC2 <- loess(Y ~ X, data = train_90C, span = span_m2)
mC3 <- loess(Y ~ X, data = train_90C, span = span_m3)
MClm = lm( Y ~ X, train_90C )


train_90C %>%
  ggplot(aes(y = Y, x = X)) +
  geom_point() +
  geom_smooth(span = span_m1, method = "loess", se = FALSE, aes(color = "Span 0.5")) + 
  geom_smooth(span = span_m2, method = "loess", se = FALSE, aes(color = "Span 0.8")) + 
  geom_smooth(span = span_m3, method = "loess", se = FALSE, aes(color = "Span 1.4")) + 
  geom_smooth(aes(color = "Linear model"), method = "lm", se = FALSE) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "orange"),
                     name = "Model")

train_90C <- train_90C %>%
  mutate(pred_m1 = predict(mC1),
         pred_m2 = predict(mC2),
         pred_m3 = predict(mC3),
         pred_mlm = predict(MClm))


# get the error for each observation
train_90C <- train_90C %>%
  mutate(error_m1 = Y - pred_m1,
         error_m2 = Y - pred_m2,
         error_m3 = Y - pred_m3,
         error_mlm = Y - pred_mlm)

# calculate the RMSE for each model
m1_rmse_train <- sqrt(mean(train_90C$error_m1^2))
m2_rmse_train <- sqrt(mean(train_90C$error_m2^2))
m3_rmse_train <- sqrt(mean(train_90C$error_m3^2))
mlm_rmse_train <- sqrt(mean(train_90C$error_mlm^2))

# compare train RMSE
train_rmse <- tibble(span = c( span_m1, span_m2, span_m3, "linear" ),
                     train_rmse = c( m1_rmse_train, m2_rmse_train, 
                                     m3_rmse_train, mlm_rmse_train)
)

knitr::kable(train_rmse, digits = 2)

# Once you have your analysis, print out your predictions
format_answer( train_90C )

# Best span:

# create a function that takes a span and returns the RMSE on the test data 
# after fitting a loess on the training data with the given span 

get_rmse <- function(span = 0.9) {
  
  test <- test_10C
  train <- train_90C
  
  # fit the model with the given span on the TRAINING data
  model <- loess(Y ~ X, data = train, span = span)
  
  # get the predictions with the model on the TESTING data
  test$predictions <- predict(model, newdata = test)
  train$predictions <- predict(model)
  
  # keep only rows with non-missing predictions and Y
  idx_test  <- !is.na(test$predictions)  & !is.na(test$Y)
  idx_train <- !is.na(train$predictions) & !is.na(train$Y)
  
  # calculate the error for each observation in the test data
  rmse_test <- sqrt(mean((test$predictions[idx_test] - test$Y[idx_test])^2))
  rmse_train <- sqrt(mean((train$predictions[idx_train] - train$Y[idx_train])^2))
  rmse_list = tibble(span = span,
                     test_rmse = rmse_test, 
                     train_rmse = rmse_train)
  
  return(rmse_list)
}

# test to make sure it matches
get_rmse(0.9) 

get_rmse(1.5) 

# now we will create a vector of spans to try 
spans <- seq(from = 0.5, to = 2, by = .05)

# run the function on each value of the spans vector
rmse <- map_dfr(spans, get_rmse) # run the function on each of the spans

# get the best span to minimize RMSE
best.span <- rmse %>% 
  slice_min(test_rmse) %>% 
  pull(span)

best.span

# indicate whether spans result in under or overfit models
rmse <- rmse %>%
  mutate(span.fit = ifelse(span == best.span, "Best",
                           ifelse(span > best.span, "Underfit", "Overfit")))

# plot the rmses and spans
p5C <- rmse %>% 
  ggplot(aes(x = span, y = test_rmse)) +
  geom_line( color = "grey" ) +
  geom_point( aes(color = span.fit) ) +
  labs(x = "Span", y = "RMSE", color = "Quality of span",
       title = "Span performance on Testing data RMSE")

p5C

format_answer(test_10C)











