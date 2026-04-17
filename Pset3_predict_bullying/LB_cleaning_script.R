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
    belong9_r  = (min(belong9,  na.rm = TRUE) +
                    max(belong9,  na.rm = TRUE)) - belong9,
    belong10_r = (min(belong10, na.rm = TRUE) +
                    max(belong10, na.rm = TRUE)) - belong10,
    belong11_r = (min(belong11, na.rm = TRUE) +
                    max(belong11, na.rm = TRUE)) - belong11
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
  select(-rank)


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

library( mice )


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
imp <- mice(survey_clean, m = 1, maxit = 1, method=meths)     

# This will default to 5 chained equation passes.  maxit specifies the
# number of iterations.  For the regression methods, first pass gets
# imputed values, then we are using those to better impute everything,
# and so on.  5 seems to make people happy.

#LB NOTE: SO DOES THIS MEAN WE SHOULD DO MAXIT = 5, OR 1?? 

imp$loggedEvents

full = mice::complete(imp)  # get the imputed data set
full = as_tibble(full)
full

saveRDS( full, "imputed_data.rds" )


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

table(is.na(survey_clean))

#######################
# Creating composite scores

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

#DYLAN I UPDATED THESE TO THE _r VERSIONS IS THAT OK
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
  select(-all_of(c( "sm_relats_same", "sm_easier",
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


#### Re-using the code above on the test data ####
# Step 2: Load in data
test <- read_csv("data/student_test_data.csv")

#### Phase 2: DATA EXPLORATION ####
# Step 3: skim data
skimr::skim(test)


# Step 4 
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
    belong9_r  = (min(belong9,  na.rm = TRUE) +
                    max(belong9,  na.rm = TRUE)) - belong9,
    belong10_r = (min(belong10, na.rm = TRUE) +
                    max(belong10, na.rm = TRUE)) - belong10,
    belong11_r = (min(belong11, na.rm = TRUE) +
                    max(belong11, na.rm = TRUE)) - belong11
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
imp <- mice(test_clean, m = 1, maxit = 1, method=meths)     

# This will default to 5 chained equation passes.  maxit specifies the
# number of iterations.  For the regression methods, first pass gets
# imputed values, then we are using those to better impute everything,
# and so on.  5 seems to make people happy.

#LB NOTE: SO DOES THIS MEAN WE SHOULD DO MAXIT = 5, OR 1?? 

imp$loggedEvents

full = mice::complete(imp)  # get the imputed data set
full = as_tibble(full)
full

saveRDS( full, "imputed_data.rds" )


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

table(is.na(test_clean))

#######################
# Creating composite scores

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
  select(-all_of(c( "sm_relats_same", "sm_easier",
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
