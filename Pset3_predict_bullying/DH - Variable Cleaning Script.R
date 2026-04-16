#### Phase 1: SETUP ####
# Step 1
library(tidyverse)
library(caret)
library(skimr)
library(dplyr)

# Step 2
survey <- read_csv("data/student_survey_data.csv")
test <- read_csv("data/student_test_data.csv")

#### Phase 2: DATA EXPLORATION ####
# Step 3
skimr::skim(survey)
skimr::skim(test)

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

# Step 5
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

range(survey_clean$belong9,  na.rm = TRUE)
range(survey_clean$support6, na.rm = TRUE)

# Reverse coding negative variables to include in composite scores
# Reverse coding belong 9-11 so that higher scores indicate more belongingness, to be consistent with the other belong variables
survey_clean <- survey_clean %>%
  mutate(
    belong9_r  = (min(belong9,  na.rm = TRUE) +
                    max(belong9,  na.rm = TRUE)) - belong9,
    belong10_r = (min(belong10, na.rm = TRUE) +
                    max(belong10, na.rm = TRUE)) - belong10,
    belong11_r = (min(belong11, na.rm = TRUE) +
                    max(belong11, na.rm = TRUE)) - belong11
  )
# Reverse coding survey_clean
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

# Creating composite scores
# Q17: Support Composite
support_vars <- c("support1", "support2", "support3", "support4",
                  "support5", "support6_r", "support7", "support8")

support_composite <- make_composite(survey_clean, support_vars, "support_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("support1", "support2", "support3", "support4",
                   "support5", "support6", "support6_r",
                   "support7", "support8"))) %>%
  left_join(support_composite, by = "student_id")

# Q18: Belongingness Composite8: 
belong_vars <- c("belong1", "belong2", "belong3", "belong4", "belong5",
                 "belong6", "belong7", "belong8", "belong9_r", "belong10_r", "belong11_r")

belong_composite <- make_composite(survey_clean, belong_vars, "belong_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("belong1", "belong2", "belong3", "belong4", "belong5",
                   "belong6", "belong7", "belong8", "belong9", "belong9_r",
                   "belong10", "belong10_r", "belong11", "belong11_r"))) %>%
  left_join(belong_composite, by = "student_id")

# Q36: Social Media Relationship Composite
sm_attitude_vars <- c("sm_less_real_r", "sm_relats_same",
                      "sm_easier",      "sm_harder_r",
                      "sm_more_open")

sm_attitude_composite <- make_composite(survey_clean, sm_attitude_vars, "sm_attitude_score")

survey_clean <- survey_clean %>%
  select(-all_of(c("sm_less_real", "sm_relats_same", "sm_easier",
                   "sm_harder", "sm_harder_r", "sm_more_open"))) %>%
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