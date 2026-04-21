# Pset 3: Predicting Bullying
# Authors: Lambda team

# Cleaning Script ---------------------------------------------------------

# Training Data Script ====
#### Phase 1: SETUP ####
# Step 1
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)
library(mice)

# Step 2: Load in data
survey <- read_csv("data/student_survey_data.csv")

#### Phase 2: DATA EXPLORATION ####
# Step 3: skim data
skimr::skim(survey)

# visualize bullying
hist(survey$bully, main = "Distribution of Bully Scores", xlab = "Bully Score")

# identifying how common it is to have a high bullying score (>=2.5)
mean(survey$bully >= 2.5, na.rm = TRUE) # ~4.5%

# checking to remove NAs
nrow(survey)
survey_na_drop = na.omit( survey )
nrow(survey_na_drop) # 0 observations!

#### Phase 3: DATA CLEANING ####

# Step 4 
# based on skimr, the text variables are not needed so I will drop them
survey_clean <- survey %>%
  select(-ends_with("_text"))

names(survey_clean) # now only has 180 variables after dropping text variables

colSums(is.na(survey_clean))

# Remove additional variables that we have decided to omit from our analysis

survey_clean <- survey_clean %>% 
  select(-starts_with("use_sm_")) %>%
  select(-c(finished,parents_know,parents_know_red,school_values,
            sm_more_helpful))

# Now we are at 157 variables

# Step 5: we need to reverse code and recode some variables before we do imputation

# Reverse coding negative variables to include in composite scores
# Reverse coding belong 9-11 so that higher scores indicate more belongingness, 
# to be consistent with the other belong variables
survey_clean <- survey_clean %>%
  mutate(
    belong9_r  = (1 + 5) - belong9,
    belong10_r = (1 + 5) - belong10,
    belong11_r = (1 + 5) - belong11,
    support6_r = (1 + 5) - support6
  )

# Reverse coding support6
survey_clean <- survey_clean %>%
  mutate(
    support6_r = (min(support6, na.rm = TRUE) +
                    max(support6, na.rm = TRUE)) - support6
  )
# Reverse coding sm_harder
survey_clean <- survey_clean %>%
  mutate(
    sm_less_real_r = (min(sm_less_real, na.rm = TRUE) +
                        max(sm_less_real, na.rm = TRUE)) - sm_less_real,
    sm_harder_r = (min(sm_harder, na.rm = TRUE) +
                     max(sm_harder, na.rm = TRUE)) - sm_harder
  )

# Verifying reverse coding worked — the correlation between original and reversed should be exactly -1
cor(survey_clean$belong9,  survey_clean$belong9_r,  use = "complete.obs")
cor(survey_clean$support6, survey_clean$support6_r, use = "complete.obs")

survey_clean <- survey_clean %>% 
  select(-c(support6,belong10,belong11,belong9,sm_less_real,sm_harder))

# make grade factor and age numeric

survey_clean <- survey_clean %>%
  mutate(grade = as.factor(str_remove(grade, "th"))) %>%
  mutate(age = as.numeric(age))

#This forces “other” in age to become NA. But only 18 people chose “other” and
#most of them then gave a joke age in age_text so this seems ok

#Q9 school values
# We removed school_values which has yes/no/I'm not sure. Keep school_values_red 
# which is just Yes/No, but recode to 0,1

survey_clean <- survey_clean %>%
  mutate(school_values_red =
           recode(school_values_red,
                  "Yes" = "1",
                  "No" = "0"))

#Q14 ranking ways kids would feel safer
# For the first ~100 rows, NA often actually means "not ranked". Let's 
# update this before reversing the ranking and turning all not ranked to 0
safe_ranks <- survey_clean %>%
  select(student_id, feel_safer_clear_rank:feel_safer_other_rank) %>%
  filter(if_any(-student_id, ~ !is.na(.))) %>%
  filter(if_any(-student_id, ~ is.na(.))) %>%
  mutate(across(-student_id, ~ ifelse(is.na(.), 0, .)))

rerank <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "Not ranked" ~ 0,
    x == "1" ~ 3,
    x == "3" ~ 1,
    TRUE ~ as.numeric(x)
  )
}

safe_ranks <- safe_ranks %>%
  mutate(across(-student_id, rerank))

survey_clean <- survey_clean %>%
  mutate(across(feel_safer_clear_rank:feel_safer_other_rank, as.numeric))

survey_clean <- rows_update(survey_clean, safe_ranks, by = "student_id")

#Q19 and Q20 about top values

#Select the top rank only, combine into one column called “qual”
#Have to combine before mice, otherwise we get NAs because mice doesn't 
#understand that the columns can only have 1,2,3 (it might give someone
#2,2,3 for example)

top_val = survey_clean %>%
  select(student_id, ach, care, happy)

top_val = top_val %>%
  pivot_longer(ach:happy, names_to = "qual", values_to = "rank")

top_val = top_val %>%
  filter(rank == 1)%>% 
  select(-rank)%>%
  group_by(student_id) %>%
  slice(1) %>%
  ungroup()


survey_clean = survey_clean %>%
  left_join(top_val, by = "student_id") %>%
  select(-c(ach:happy)) 


survey_clean$qual = as_factor(survey_clean$qual)


#Q20: Recode the data to match the answers in Q19

survey_clean <- survey_clean %>%
  mutate(parent_qual =
           recode(parent_qual,
                  "Achieving at a high level" = "ach",
                  "Happiness (feeling good most of the time)" = "happy",
                  "Caring about others" = "care"))

#Q29 recode to numeric

survey_clean <- survey_clean %>%
  mutate(adults_worry = recode(adults_worry,
                               "Not at all" = "0",
                               "A little bit" = "1",
                               "Moderately" = "2",
                               "A lot" = "3"))


#Q37 sm_more_helpful - remove but keep sm_more_helpful_red which is just yes/no

survey_clean <- survey_clean %>%
  mutate(sm_more_helpful_red =
           recode(sm_more_helpful_red,
                  "Yes" = "1",
                  "No" = "0"))


# Step 6: Now it's time for imputation!!!!!!! Using the helper script and lab7

# make sure any values that are just empty are actually NAs
survey_clean <- survey_clean %>% 
  mutate(across(where(is.character) | where(is.numeric), 
                ~ifelse(. == '', NA, .)))

# convert character to factor (from Luciana's code)
survey_clean <- survey_clean %>% mutate( across( where( is.character ), factor ) )


# First, mice will automatically detect and handle different types data if you
# don't tell it not to.

# We can see what it wants to do:
# To do this, use mice with maxit=0 to get what the defaults are first, and
# then we can easily change them and run mice for real.
imp <- mice(survey_clean, maxit = 0 )
imp$loggedEvents
table( imp$method )
meths <- imp$method

# Create imputed data set
imp <- mice(survey_clean, m = 1, maxit = 5, method=meths)     

# This will default to 5 chained equation passes.  maxit specifies the
# number of iterations.  For the regression methods, first pass gets
# imputed values, then we are using those to better impute everything,
# and so on.  5 seems to make people happy.

#LB NOTE: SO DOES THIS MEAN WE SHOULD DO MAXIT = 5, OR 1?? 

imp$loggedEvents

full = mice::complete(imp)  # get the imputed data set
full = as_tibble(full)
full

saveRDS( full, "imputed_survey_data.rds" )


# Explore and check
table( survey_clean$esafe3, useNA = "always" )
table( full$esafe3, useNA = "always" )

table( survey_clean$gender, useNA = "always" )
table( full$gender, useNA = "always" )

table( survey_clean$age, useNA = "always" )
table( full$age, useNA = "always" )

table(is.na(full))


#YAY the new dataset doesn't have any NAs

survey_clean <- full

survey_clean <- survey_clean %>%
  mutate(
    adults_worry        = as.numeric(as.character(adults_worry)),
    school_values_red   = as.numeric(as.character(school_values_red)),
    sm_more_helpful_red = as.numeric(as.character(sm_more_helpful_red))
  )

table(is.na(survey_clean))

#### Creating composite scores ####

make_composite <- function(data, cols, score_name) {
  data %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(student_id) %>%
    summarise("{score_name}" :=mean(value, na.rm = TRUE))
}

range(survey_clean$belong9_r,  na.rm = TRUE)
range(survey_clean$support6_r, na.rm = TRUE)


# Q17: Support Composite
support_vars <- c("support1", "support2", "support3", "support4",
                  "support5", "support6_r", "support7", "support8")

support_composite <- make_composite(survey_clean, support_vars, "support_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("support1", "support2", "support3", "support4",
                   "support5", "support6_r",
                   "support7", "support8"))) %>%
  left_join(support_composite, by = "student_id")

# Q18: Belongingness Composite8: 
belong_vars <- c("belong1", "belong2", "belong3", "belong4", "belong5",
                 "belong6", "belong7", "belong8", "belong9_r", "belong10_r", "belong11_r")

belong_composite <- make_composite(survey_clean, belong_vars, "belong_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("belong1", "belong2", "belong3", "belong4", "belong5",
                   "belong6", "belong7", "belong8", "belong9_r",
                   "belong10_r", "belong11_r"))) %>%
  left_join(belong_composite, by = "student_id")

# Q22: social media composite
sm_vars <- c("sm_facebook", "sm_instagram", "sm_pinterest", "sm_snapchat", 
             "sm_tumblr", "sm_twitter", "sm_vine", "sm_yikyak",
             "sm_youtube", "sm_other")
sm_composite <- make_composite(survey_clean, sm_vars, "sm_platforms")
survey_clean <- survey_clean %>%
  select(-all_of(c("sm_facebook", "sm_instagram", "sm_pinterest", "sm_snapchat", 
                   "sm_tumblr", "sm_twitter", "sm_vine", "sm_yikyak",
                   "sm_youtube", "sm_other"))) %>%
  left_join(sm_composite, by = "student_id")

# Q36: Social Media Relationship Composite
sm_attitude_vars <- c("sm_less_real_r", "sm_relats_same",
                      "sm_easier",      "sm_harder_r",
                      "sm_more_open")

sm_attitude_composite <- make_composite(survey_clean, sm_attitude_vars, 
                                        "sm_attitude_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("sm_less_real_r", "sm_relats_same", "sm_easier",
                   "sm_harder_r", "sm_more_open"))) %>%
  left_join(sm_attitude_composite, by = "student_id")

# Q12L: Physical Safety Composite
p_safety_vars <- c("psafe1", "psafe2", "psafe3", "psafe4", 
                   "psafe5", "psafe6", "psafe7")
p_safety_composite <- make_composite(survey_clean, p_safety_vars, "p_safety_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("psafe1", "psafe2", "psafe3", "psafe4", "psafe5",
                   "psafe6", "psafe7"))) %>%
  left_join(p_safety_composite, by = "student_id")

# Q12R: Emotional Safety Composite
e_safety_vars <- c("esafe1", "esafe2", "esafe3", "esafe4", 
                   "esafe5", "esafe6", "esafe7")
e_safety_composite <- make_composite(survey_clean, e_safety_vars, "e_safety_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("esafe1", "esafe2", "esafe3", "esafe4", "esafe5",
                   "esafe6", "esafe7"))) %>%
  left_join(e_safety_composite, by = "student_id")

# Q26: Parental Talk Composite
parent_talk_vars <- c("talk_appropriate", "talk_privacy", "talk_avoid_hurt",
                      "talk_show_support", "talk_take_action", "talk_share_belief",
                      "talk_uncomfortable", "talk_connect")
parent_talk_composite <- make_composite(survey_clean, parent_talk_vars, "parent_support_score")
survey_clean <- survey_clean %>%
  select(-all_of(c("talk_appropriate", "talk_privacy", "talk_avoid_hurt",
                   "talk_show_support", "talk_take_action", "talk_share_belief",
                   "talk_uncomfortable", "talk_connect"))) %>%
  left_join(parent_talk_composite, by = "student_id")

# Q39: School Talk Composite
school_talk_vars <- c("school_appropriate", "school_privacy", "school_avoid_hurt",
                      "school_show_support", "school_take_action", "school_share_belief",
                      "school_uncomfortable", "school_connect")
school_talk_composite <- make_composite(survey_clean, school_talk_vars, "school_support_score")                      
survey_clean <- survey_clean %>%
  select(-all_of(c("school_appropriate", "school_privacy", "school_avoid_hurt",
                   "school_show_support", "school_take_action", "school_share_belief",
                   "school_uncomfortable", "school_connect"))) %>%
  left_join(school_talk_composite, by = "student_id")

# Q33: Social Media Help Composite
sm_help_vars <- c("sm_help_connect", "sm_help_laugh", "sm_help_opinions",
                  "sm_help_plan", "sm_help_encourage", "sm_help_closer",
                  "sm_help_show_care", "sm_help_solve_problems", "sm_help_reach_out")
sm_help_composite <- make_composite(survey_clean, sm_help_vars, "sm_help_score")
survey_clean <- survey_clean %>%
  select(-all_of(c("sm_help_connect", "sm_help_laugh", "sm_help_opinions",
                   "sm_help_plan", "sm_help_encourage", "sm_help_closer",
                   "sm_help_show_care", "sm_help_solve_problems", "sm_help_reach_out"))) %>%
  left_join(sm_help_composite, by = "student_id")

# Q34: Social Media Concern Composite
sm_concern_vars <- c("sm_concern_left_out", "sm_concern_start_fight", "sm_concern_mean",
                     "sm_concern_show_off", "sm_concern_not_real","sm_concern_cause_drama",
                     "sm_concern_misunderstand", "sm_concern_stalk")
sm_concern_composite <- make_composite(survey_clean, sm_concern_vars, "sm_concern_score")    
survey_clean <- survey_clean %>%
  select(-all_of(c("sm_concern_left_out", "sm_concern_start_fight", "sm_concern_mean",
                   "sm_concern_show_off", "sm_concern_not_real","sm_concern_cause_drama",
                   "sm_concern_misunderstand", "sm_concern_stalk"))) %>%
  left_join(sm_concern_composite, by = "student_id")   

# Q16: Discrimination Climate
discrimination <- c("disc_race", "disc_lang", "disc_wealth", "disc_religion", 
                    "disc_sex_or", "disc_gender", "disc_disab", "disc_phys_ap", 
                    "disc_country")
discrimination_comp <- make_composite(survey_clean, discrimination, "discrimination")
survey_clean <- survey_clean %>%
  select(-all_of(c("disc_race", "disc_lang", "disc_wealth", "disc_religion", 
                   "disc_sex_or", "disc_gender", "disc_disab", "disc_phys_ap", 
                   "disc_country"))) %>%
  left_join(discrimination_comp, by = "student_id")

# Q21: School Fairness & Rules
rules <- c("rules1", "rules2", "rules3", "rules4", "rules5", "rules6",
           "rules7", "rules8", "rules9")
rules_comp <- make_composite(survey_clean, rules, "school_rules")
survey_clean <- survey_clean %>%
  select(-all_of(c("rules1", "rules2", "rules3", "rules4", "rules5", "rules6",
                   "rules7", "rules8", "rules9"))) %>%
  left_join(rules_comp, by = "student_id")

skim(survey_clean)

# create csv for cleaned survey data 
write_csv(survey_clean, "cleaned_data.csv")


# Test Data Script ====
# Re-using the code above on the test data

#### Phase 1: SETUP ####
# Step 2: Load in data
test <- read_csv("data/student_test_data.csv")

#### Phase 2: DATA EXPLORATION ####
# Step 3: skim data
skimr::skim(test)

#### Phase 3: DATA CLEANING ####

# Step 4 #
# based on skimr, the text variables are not needed so I will drop them
test_clean <- test %>%
  select(-ends_with("_text"))

names(test_clean) # now only has 180 variables after dropping text variables

colSums(is.na(test_clean))

# Remove additional variables that we have decided to omit from our analysis

test_clean <- test_clean %>% 
  select(-starts_with("use_sm_")) %>%
  select(-c(finished,parents_know,parents_know_red,school_values,
            sm_more_helpful))

# Now we are at 157 variables

# Step 5: we need to reverse code and recode some variables before we do imputation

# Reverse coding negative variables to include in composite scores
# Reverse coding belong 9-11 so that higher scores indicate more belongingness, 
# to be consistent with the other belong variables
test_clean <- test_clean %>%
  mutate(
    belong9_r  = (1 + 5) - belong9,
    belong10_r = (1 + 5) - belong10,
    belong11_r = (1 + 5) - belong11,
    support6_r = (1 + 5) - support6
  )

# Reverse coding support6
test_clean <- test_clean %>%
  mutate(
    support6_r = (min(support6, na.rm = TRUE) +
                    max(support6, na.rm = TRUE)) - support6
  )
# Reverse coding sm_harder
test_clean <- test_clean %>%
  mutate(
    sm_less_real_r = (min(sm_less_real, na.rm = TRUE) +
                        max(sm_less_real, na.rm = TRUE)) - sm_less_real,
    sm_harder_r = (min(sm_harder, na.rm = TRUE) +
                     max(sm_harder, na.rm = TRUE)) - sm_harder
  )

# Verifying reverse coding worked — the correlation between original and reversed should be exactly -1
cor(test_clean$belong9,  test_clean$belong9_r,  use = "complete.obs")
cor(test_clean$support6, test_clean$support6_r, use = "complete.obs")

test_clean <- test_clean %>% 
  select(-c(support6,belong10,belong11,belong9,sm_less_real,sm_harder))

# make grade factor and age numeric

test_clean <- test_clean %>%
  mutate(grade = as.factor(str_remove(grade, "th"))) %>%
  mutate(age = as.numeric(age))

#This forces “other” in age to become NA. But only 18 people chose “other” and
#most of them then gave a joke age in age_text so this seems ok

#Q9 school values
# We removed school_values which has yes/no/I'm not sure. Keep school_values_red 
# which is just Yes/No, but recode to 0,1

test_clean <- test_clean %>%
  mutate(school_values_red =
           recode(school_values_red,
                  "Yes" = "1",
                  "No" = "0"))

#Q14 ranking ways kids would feel safer
# For the first ~100 rows, NA often actually means "not ranked". Let's 
# update this before reversing the ranking and turning all not ranked to 0
safe_ranks <- test_clean %>%
  select(student_id, feel_safer_clear_rank:feel_safer_other_rank) %>%
  filter(if_any(-student_id, ~ !is.na(.))) %>%
  filter(if_any(-student_id, ~ is.na(.))) %>%
  mutate(across(-student_id, ~ ifelse(is.na(.), 0, .)))

rerank <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "Not ranked" ~ 0,
    x == "1" ~ 3,
    x == "3" ~ 1,
    TRUE ~ as.numeric(x)
  )
}

safe_ranks <- safe_ranks %>%
  mutate(across(-student_id, rerank))

test_clean <- test_clean %>%
  mutate(across(feel_safer_clear_rank:feel_safer_other_rank, as.numeric))

test_clean <- rows_update(test_clean, safe_ranks, by = "student_id")

#Q19 and Q20 about top values

#Select the top rank only, combine into one column called “qual”
#Have to combine before mice, otherwise we get NAs because mice doesn't 
#understand that the columns can only have 1,2,3 (it might give someone
#2,2,3 for example)

top_val = test_clean %>%
  select(student_id, ach, care, happy)

top_val = top_val %>%
  pivot_longer(ach:happy, names_to = "qual", values_to = "rank")

top_val = top_val %>%
  filter(rank == 1)%>% 
  select(-rank)


test_clean = test_clean %>%
  left_join(top_val, by = "student_id") %>%
  select(-c(ach:happy)) 


test_clean$qual = as_factor(test_clean$qual)


#Q20: Recode the data to match the answers in Q19

test_clean <- test_clean %>%
  mutate(parent_qual =
           recode(parent_qual,
                  "Achieving at a high level" = "ach",
                  "Happiness (feeling good most of the time)" = "happy",
                  "Caring about others" = "care"))

#Q29 recode to numeric

test_clean <- test_clean %>%
  mutate(adults_worry = recode(adults_worry,
                               "Not at all" = "0",
                               "A little bit" = "1",
                               "Moderately" = "2",
                               "A lot" = "3"))


#Q37 sm_more_helpful - remove but keep sm_more_helpful_red which is just yes/no

test_clean <- test_clean %>%
  mutate(sm_more_helpful_red =
           recode(sm_more_helpful_red,
                  "Yes" = "1",
                  "No" = "0"))


# Step 6: Now it's time for imputation!!!!!!! Using the helper script and lab7

library( mice )


# make sure any values that are just empty are actually NAs
test_clean <- test_clean %>% 
  mutate(across(where(is.character) | where(is.numeric), 
                ~ifelse(. == '', NA, .)))

# convert character to factor (from Luciana's code)
test_clean <- test_clean %>% mutate( across( where( is.character ), factor ) )


# First, mice will automatically detect and handle different types data if you
# don't tell it not to.

# We can see what it wants to do:
# To do this, use mice with maxit=0 to get what the defaults are first, and
# then we can easily change them and run mice for real.
imp <- mice(test_clean, maxit = 0 )
imp$loggedEvents
table( imp$method )
meths <- imp$method

# Create imputed data set
imp <- mice(test_clean, m = 1, maxit = 5, method=meths)     

# This will default to 5 chained equation passes.  maxit specifies the
# number of iterations.  For the regression methods, first pass gets
# imputed values, then we are using those to better impute everything,
# and so on.  5 seems to make people happy.

imp$loggedEvents

full = mice::complete(imp)  # get the imputed data set
full = as_tibble(full)
full

saveRDS( full, "imputed_test_data.rds" )


# Explore and check
table( test_clean$esafe3, useNA = "always" )
table( full$esafe3, useNA = "always" )

table( test_clean$gender, useNA = "always" )
table( full$gender, useNA = "always" )

table( test_clean$age, useNA = "always" )
table( full$age, useNA = "always" )

table(is.na(full))


#YAY the new dataset doesn't have any NAs

test_clean <- full

test_clean <- test_clean %>%
  mutate(
    adults_worry        = as.numeric(as.character(adults_worry)),
    school_values_red   = as.numeric(as.character(school_values_red)),
    sm_more_helpful_red = as.numeric(as.character(sm_more_helpful_red))
  )

table(is.na(test_clean))

#### Creating composite scores ####

range(test_clean$belong9_r,  na.rm = TRUE)
range(test_clean$support6_r, na.rm = TRUE)


# Q17: Support Composite
support_vars <- c("support1", "support2", "support3", "support4",
                  "support5", "support6_r", "support7", "support8")

support_composite <- make_composite(test_clean, support_vars, "support_score")

test_clean <- test_clean %>%
  select(-all_of(c("support1", "support2", "support3", "support4",
                   "support5", "support6_r",
                   "support7", "support8"))) %>%
  left_join(support_composite, by = "student_id")

# Q18: Belongingness Composite8: 
belong_vars <- c("belong1", "belong2", "belong3", "belong4", "belong5",
                 "belong6", "belong7", "belong8", "belong9_r", "belong10_r", "belong11_r")

belong_composite <- make_composite(test_clean, belong_vars, "belong_score")

test_clean <- test_clean %>%
  select(-all_of(c("belong1", "belong2", "belong3", "belong4", "belong5",
                   "belong6", "belong7", "belong8", "belong9_r",
                   "belong10_r", "belong11_r"))) %>%
  left_join(belong_composite, by = "student_id")

# Q22: social media composite
sm_vars <- c("sm_facebook", "sm_instagram", "sm_pinterest", "sm_snapchat", 
             "sm_tumblr", "sm_twitter", "sm_vine", "sm_yikyak",
             "sm_youtube", "sm_other")
sm_composite <- make_composite(test_clean, sm_vars, "sm_platforms")
test_clean <- test_clean %>%
  select(-all_of(c("sm_facebook", "sm_instagram", "sm_pinterest", "sm_snapchat", 
                   "sm_tumblr", "sm_twitter", "sm_vine", "sm_yikyak",
                   "sm_youtube", "sm_other"))) %>%
  left_join(sm_composite, by = "student_id")

# Q36: Social Media Relationship Composite
sm_attitude_vars <- c("sm_less_real_r", "sm_relats_same",
                      "sm_easier",      "sm_harder_r",
                      "sm_more_open")

sm_attitude_composite <- make_composite(test_clean, sm_attitude_vars, 
                                        "sm_attitude_score")

test_clean <- test_clean %>%
  select(-all_of(c( "sm_less_real_r","sm_relats_same", "sm_easier",
                    "sm_harder_r", "sm_more_open"))) %>%
  left_join(sm_attitude_composite, by = "student_id")

# Q12L: Physical Safety Composite
p_safety_vars <- c("psafe1", "psafe2", "psafe3", "psafe4", 
                   "psafe5", "psafe6", "psafe7")
p_safety_composite <- make_composite(test_clean, p_safety_vars, "p_safety_score")

test_clean <- test_clean %>%
  select(-all_of(c("psafe1", "psafe2", "psafe3", "psafe4", "psafe5",
                   "psafe6", "psafe7"))) %>%
  left_join(p_safety_composite, by = "student_id")

# Q12R: Emotional Safety Composite
e_safety_vars <- c("esafe1", "esafe2", "esafe3", "esafe4", 
                   "esafe5", "esafe6", "esafe7")
e_safety_composite <- make_composite(test_clean, e_safety_vars, "e_safety_score")

test_clean <- test_clean %>%
  select(-all_of(c("esafe1", "esafe2", "esafe3", "esafe4", "esafe5",
                   "esafe6", "esafe7"))) %>%
  left_join(e_safety_composite, by = "student_id")

# Q26: Parental Talk Composite
parent_talk_vars <- c("talk_appropriate", "talk_privacy", "talk_avoid_hurt",
                      "talk_show_support", "talk_take_action", "talk_share_belief",
                      "talk_uncomfortable", "talk_connect")
parent_talk_composite <- make_composite(test_clean, parent_talk_vars, "parent_support_score")
test_clean <- test_clean %>%
  select(-all_of(c("talk_appropriate", "talk_privacy", "talk_avoid_hurt",
                   "talk_show_support", "talk_take_action", "talk_share_belief",
                   "talk_uncomfortable", "talk_connect"))) %>%
  left_join(parent_talk_composite, by = "student_id")

# Q39: School Talk Composite
school_talk_vars <- c("school_appropriate", "school_privacy", "school_avoid_hurt",
                      "school_show_support", "school_take_action", "school_share_belief",
                      "school_uncomfortable", "school_connect")
school_talk_composite <- make_composite(test_clean, school_talk_vars, "school_support_score")                      
test_clean <- test_clean %>%
  select(-all_of(c("school_appropriate", "school_privacy", "school_avoid_hurt",
                   "school_show_support", "school_take_action", "school_share_belief",
                   "school_uncomfortable", "school_connect"))) %>%
  left_join(school_talk_composite, by = "student_id")

# Q33: Social Media Help Composite
sm_help_vars <- c("sm_help_connect", "sm_help_laugh", "sm_help_opinions",
                  "sm_help_plan", "sm_help_encourage", "sm_help_closer",
                  "sm_help_show_care", "sm_help_solve_problems", "sm_help_reach_out")
sm_help_composite <- make_composite(test_clean, sm_help_vars, "sm_help_score")
test_clean <- test_clean %>%
  select(-all_of(c("sm_help_connect", "sm_help_laugh", "sm_help_opinions",
                   "sm_help_plan", "sm_help_encourage", "sm_help_closer",
                   "sm_help_show_care", "sm_help_solve_problems", "sm_help_reach_out"))) %>%
  left_join(sm_help_composite, by = "student_id")

# Q34: Social Media Concern Composite
sm_concern_vars <- c("sm_concern_left_out", "sm_concern_start_fight", "sm_concern_mean",
                     "sm_concern_show_off", "sm_concern_not_real","sm_concern_cause_drama",
                     "sm_concern_misunderstand", "sm_concern_stalk")
sm_concern_composite <- make_composite(test_clean, sm_concern_vars, "sm_concern_score")    
test_clean <- test_clean %>%
  select(-all_of(c("sm_concern_left_out", "sm_concern_start_fight", "sm_concern_mean",
                   "sm_concern_show_off", "sm_concern_not_real","sm_concern_cause_drama",
                   "sm_concern_misunderstand", "sm_concern_stalk"))) %>%
  left_join(sm_concern_composite, by = "student_id")   

# Q16: Discrimination Climate
discrimination <- c("disc_race", "disc_lang", "disc_wealth", "disc_religion", 
                    "disc_sex_or", "disc_gender", "disc_disab", "disc_phys_ap", 
                    "disc_country")
discrimination_comp <- make_composite(test_clean, discrimination, "discrimination")
test_clean <- test_clean %>%
  select(-all_of(c("disc_race", "disc_lang", "disc_wealth", "disc_religion", 
                   "disc_sex_or", "disc_gender", "disc_disab", "disc_phys_ap", 
                   "disc_country"))) %>%
  left_join(discrimination_comp, by = "student_id")

# Q21: School Fairness & Rules
rules <- c("rules1", "rules2", "rules3", "rules4", "rules5", "rules6",
           "rules7", "rules8", "rules9")
rules_comp <- make_composite(test_clean, rules, "school_rules")
test_clean <- test_clean %>%
  select(-all_of(c("rules1", "rules2", "rules3", "rules4", "rules5", "rules6",
                   "rules7", "rules8", "rules9"))) %>%
  left_join(rules_comp, by = "student_id")

skim(test_clean)

write_csv(test_clean, "cleaned_test_data.csv")


# Ranger Script -----------------------------------------------------------

# Pset 3: Predicting Bullying
# Authors: Lambda team
# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)
library(doParallel)


train_all <- read.csv("cleaned_data.csv")

## Data exploration and cleaning
# To answer  Writeup and Discussion Questions (Part I)
skim(train_all)

nrow(train_all)

hist(train_all$bully)

mean_high_low_bully <- train_all %>%
  mutate(bully_group = if_else(bully >= 2.5, ">= 2.5", "< 2.5")) %>%
  group_by(bully_group) %>%
  summarise(
    mean_bully = mean(bully, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )



#### Overall clean-up ####

# Most of the code below is from Lab10, but we will modify it to fit our needs.

# see if any variables in our dataset have close to zero variability 
# (and thus should be dropped from the analysis)
nzv_to_drop <- nearZeroVar(train_all, saveMetrics = TRUE) %>%
  filter( nzv == TRUE )

train_clean <- train_all %>%
  dplyr::select(-race_amerind, -feel_safer_other_rank, -student_id)

# Convert all character predictors to factors
train_clean <- train_clean %>%
  mutate(across(where(is.character), as.factor))

# check variable types
map_chr(train_clean, typeof)

type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")

#### Splitting data into train and test ####
# set a seed for reproducibility
set.seed(80107)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(train_clean$bully,
                                  p = .8, # proportion of data for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- train_clean %>% 
  mutate(temp_id = 1:nrow(train_clean))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  dplyr::select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

# compare these to `test` and `train`:
summary(train$bully)
summary(test$bully)

# what is the tuning parameter for rf?
modelLookup(model = "rf")
modelLookup(model = "ranger")

#### Train model with cross-validation ####
# Reset the seed
set.seed(80107)

# parallel run this heavy model training
cl <- makePSOCKcluster(parallel::detectCores() - 1)  
registerDoParallel(cl)

# multiple parameters combinations have been tested, these are the ones
# we landed on, test RMSE didn't seem to improve with the tweaks we made.
# we think this might be due to our data being very imbalanced.

grid <- expand.grid(
  mtry = c(5, 10, 15, 20, 25, 30),
  splitrule = c("variance", "extratrees"),
  min.node.size = c(3, 5, 10, 20)     
)

ctrl <- trainControl(
  method = "cv", 
  number = 10,
  savePredictions = "final",
  verboseIter = FALSE      
)
# we saw on Luke's website that he uses ranger as an alternative to rf 
cv_mod_reg <- train(
  bully ~ ., 
  data = train,
  method = "ranger",
  trControl = ctrl,
  num.trees = 1000,       
  tuneGrid = grid,
  metric = "RMSE",
  importance = "permutation"  
)
stopCluster(cl)

#### Check results ####

cv_mod_reg
cv_mod_reg$bestTune
plot(cv_mod_reg)

X <- model.matrix(bully ~ ., data = train)
ncol(X) - 1  # predictors (excluding intercept)

cv_mod_results <- cv_mod_reg$results

ggplot(cv_mod_results, aes(x = mtry, y = RMSE)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

# retrieve importance (by default, this is scaled from 0-100)
cv_mod_imp <- varImp(cv_mod_reg)

cv_mod_imp <- cv_mod_imp$importance # get the df only

cv_mod_imp <- cv_mod_imp %>% rownames_to_column("var")

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(y = "Variable", x = "Importance (scaled)") +
  theme_bw()

# had to ask help from chatgpt to display only the top 20 predictors:
cv_mod_imp <- varImp(cv_mod_reg)$importance %>%
  rownames_to_column("var") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 20)

ggplot(cv_mod_imp, aes(x = Overall, y = reorder(var, Overall))) +
  geom_col(fill = "blue") +
  labs(title = "Top 20 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


train_reg <- train %>%
  mutate(rf_pred = predict(cv_mod_reg, train)) 

# caret has a built-in RMSE function, but it's also easy to calculate by hand:
RMSE(train_reg$bully, train_reg$rf_pred)
sqrt(mean((train_reg$bully - train_reg$rf_pred)^2))

# train RMSE 0.1595064
# now for test data:
test_reg <- test %>%
  mutate(rf_pred = predict(cv_mod_reg, test))

RMSE(test_reg$bully, test_reg$rf_pred)
sqrt(mean((test_reg$bully - test_reg$rf_pred)^2))
# test RMSE 0.399066

#### Fill in the csv file with our predictions ####
new_raw <- read.csv("cleaned_test_data.csv") %>%
  mutate(across(where(is.character), as.factor))
rf_reg <- cv_mod_reg

# Align factor levels to what the regression model saw during training 
# (suggestion from Claude)
for (v in names(rf_reg$xlevels)) {
  if (v %in% names(new_raw)) {
    new_raw[[v]] <- as.character(new_raw[[v]])
    new_raw[[v]][!new_raw[[v]] %in% rf_reg$xlevels[[v]]] <- NA
    new_raw[[v]] <- factor(new_raw[[v]], levels = rf_reg$xlevels[[v]])
  }
}

predicted_bully_level <- predict(rf_reg, newdata = new_raw)

pred_out <- data.frame(
  student_id = new_raw$student_id,
  predicted_bully_level = as.numeric(predicted_bully_level)
)

write.csv(pred_out, "final_rangder_random_forest_prediction.csv", 
                   row.names = FALSE)

preds <- read.csv("final_rangder_random_forest_prediction.csv")


# Random Forest Classification Script  ------------------------------------

# Pset 3: Predicting Bullying
# Authors: Lambda team
# packages
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)


train_all <- read.csv("cleaned_data.csv")

## Data exploration and cleaning
# To answer  Writeup and Discussion Questions (Part I)
skim(train_all)

nrow(train_all)

hist(train_all$bully)

mean_high_low_bully <- train_all %>%
  mutate(bully_group = if_else(bully >= 2.5, ">= 2.5", "< 2.5")) %>%
  group_by(bully_group) %>%
  summarise(
    mean_bully = mean(bully, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

mean_high_low_bully

##### Overall clean-up ####

# Most of the code below is from Lab10, and has been modified to fit our needs.

# see if any variables in our dataset have close to zero variability 
# (and thus should be dropped from the analysis)
nzv_to_drop <- nearZeroVar(train_all, saveMetrics = TRUE) %>%
  filter( nzv == TRUE )

train_clean <- train_all %>%
  dplyr::select(-race_amerind, -feel_safer_other_rank, -student_id)

# Convert all character predictors to factors
train_clean <- train_clean %>%
  mutate(across(where(is.character), as.factor))


# check variable types
map_chr(train_clean, typeof)

type <- map_chr(train_clean, typeof) %>%
  enframe(name = "var", value = "type")


# convert bully to a binary variable:
train_rfc <- train_clean %>%
  mutate(
    bully_bin = if_else(bully >= 2.5, 1L, 0L)  # 1 = high, 0 = low
  ) %>%
  dplyr::select(-bully)


# set a seed for reproducibility
set.seed(80107)

# Create an index of rows for a training set with half of the data
trainIndex <- createDataPartition(train_rfc$bully_bin,
                                  p = .8, # proportion of data to use for training
                                  list = FALSE, #results will be in matrix form
                                  times = 1) # number of partitions to create)

# Create a temporary id to subset the data according to the trainIndex
full <- train_rfc %>% 
  mutate(temp_id = 1:nrow(train_rfc))

train <- full %>% 
  filter(temp_id %in% trainIndex) %>% 
  dplyr::select(-temp_id)

test <- full %>% 
  filter(!(temp_id %in% trainIndex)) %>% 
  dplyr::select(-temp_id)

train$bully_bin <- factor(train$bully_bin, levels = c(0,1), labels = c("low","high"))
test$bully_bin  <- factor(test$bully_bin,  levels = c(0,1), labels = c("low","high"))

# compare these to `test` and `train`:
table(train$bully_bin)
prop.table(table(train$bully_bin))

table(test$bully_bin)
prop.table(table(test$bully_bin))

# results are very similar! :)


# what is the tuning parameter for rf?

modelLookup(model = "rf")

# Reset the seed
set.seed(80107)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)


cv_mod <- train(
  bully_bin ~ ., data = train, method = "rf", trControl = ctrl, ntree = 500, tuneLength = 10
)


class(train$bully_bin)
str(train$bully_bin)
# 
cv_mod

cv_mod_results <- cv_mod$results

ggplot(cv_mod_results, aes(x = mtry, y = ROC)) +
  geom_point(col = "blue") +
  geom_line(col = "blue") +
  theme_bw()

max(cv_mod$results)

# AUC at the best tuned mtry
cv_mod$results[cv_mod$results$mtry == cv_mod$bestTune$mtry, "ROC"]

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
  labs(title = "Top 20 predictors", y = NULL, x = "Importance (scaled)") +
  theme_bw()


new_raw <- read.csv("cleaned_test_data.csv") %>%
  mutate(across(where(is.character), as.factor))

# Make sure bully_bin is not present (it usually won't be in test file)
new_raw <- new_raw %>% select(-any_of("bully_bin"))

rf_cls <- cv_mod
# Align factor levels to training model
for (v in names(rf_cls$xlevels)) {
  if (v %in% names(new_raw)) {
    new_raw[[v]] <- as.character(new_raw[[v]])
    new_raw[[v]][!new_raw[[v]] %in% rf_cls$xlevels[[v]]] <- NA
    new_raw[[v]] <- factor(new_raw[[v]], levels = rf_cls$xlevels[[v]])
  }
}

# Predicted probability of being "high"
p_mat <- predict(rf_cls, newdata = new_raw, type = "prob")
predicted_bully_risk <- p_mat[, "high"]

#### Delete this in the end ####
# 0/1 classification using 0.5 cutoff (change if you chose a different threshold)
# predicted_bully_high <- ifelse(predicted_bully_risk >= 0.5, 1L, 0L)

# Get probability scores on validation set
rf_val_probs <- predict(rf_cls, newdata = test, type = "prob")

# Threshold analysis
thresholds <- seq(0.01, 0.20, by = 0.01)

threshold_results <- sapply(thresholds, function(t) {
  preds   <- ifelse(rf_val_probs[, "high"] >= t, 1, 0)
  actual  <- ifelse(test$bully_bin == "high", 1, 0)
  fn_rate <- sum(preds == 0 & actual == 1) / sum(actual == 1)
  fp_rate <- sum(preds == 1 & actual == 0) / sum(actual == 0)
  c(threshold = t, FNR = fn_rate, FPR = fp_rate)
})

as.data.frame(t(threshold_results))

# Check predicted rate at different thresholds
for (t in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.10)) {
  rate <- mean(predicted_bully_risk >= t)
  cat("Threshold:", t, "→ Predicted high rate:", round(rate, 4), "\n")
}

# extending threeshold to better match training and validation rates (run analysis first to pick the right one)
for (t in c(0.15, 0.20, 0.25, 0.30, 0.35, 0.40)) {
  rate <- mean(predicted_bully_risk >= t)
  cat("Threshold:", t, "→ Predicted high rate:", round(rate, 4), "\n")
}

# Use chosen threshold (run analysis first to pick the right one)
predicted_bully_high <- ifelse(predicted_bully_risk >= 0.15, 1L, 0L)

# verify!
table(predicted_bully_high)
cat("Predicted high rate:", mean(predicted_bully_high), "\n")
cat("Training high rate:", mean(train$bully_bin == "high"), "\n")


# Output with required column names
pred_out <- data.frame(
  student_id = new_raw$student_id,
  predicted_bully_level = NA_real_,
  predicted_bully_risk = as.numeric(predicted_bully_risk),
  predicted_bully_high = as.integer(predicted_bully_high)
)

write.csv(pred_out, "rf_class_predictions.csv", row.names = FALSE)
results <- read.csv("rf_class_predictions.csv")


# Stepwise Regression Script ----------------------------------------------

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

cleaned_data <- read_csv("cleaned_data.csv")

#remove student_id
cleaned_data <- cleaned_data[,-1]

# create a binary outcome for bully
reg_data = cleaned_data %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(bully_high = ifelse(bully >=2.5, 1,0)) 


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

#### Logistic Regression with binary outcome ####
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

#### Testing on holdout data ####
# now let's run the model on our test data
holdout_data <- read_csv("cleaned_test_data.csv")

# we have to do some of the same cleaning we did for our training data
# make the character variables factors
holdout_test <- holdout_data %>%
  select(-student_id) %>% 
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


#create a new dataframe called preds
preds <- data.frame(student_id = holdout_data$student_id,
                    predicted_bully_risk = holdout_test$predicted_bully_risk)

write.csv(preds, "log_step_risk_predictions.csv", 
          row.names = FALSE)


# Merge Data Script -------------------------------------------------------

library( tidyverse )

rf_class <- read_csv("rf_class_predictions.csv")

rf_ranger <- read_csv("final_ranger_random_forest_prediction.csv")

log_step_risk <- read_csv("log_step_risk_predictions.csv")

rf_class = rf_class %>% 
  select(student_id,predicted_bully_high)

dim(rf_class)
dim(rf_ranger)
dim(log_step_risk)

#merge (by student_id) rf_class, rf_ranger, and log_step_risk into one dataset 
#called student_test_data.csv 
lambda_student_predictions <- rf_class %>% 
  left_join(rf_ranger, by = "student_id") %>% 
  left_join(log_step_risk, by = "student_id")

dim(lambda_student_predictions)

lambda_student_predictions %>% 
  mutate(predicted_bully_high = as.factor(predicted_bully_high)) %>% 
  ggplot(aes(predicted_bully_level, predicted_bully_risk,
             col=predicted_bully_high)) +
  geom_point()

write.csv(lambda_student_predictions, "lambda_student_predictions.csv", 
          row.names = FALSE)

source( "check_predictions_function.R" )
check_prediction_file_format( "lambda_student_predictions.csv" )


