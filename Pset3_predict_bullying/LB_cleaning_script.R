library( tidyverse )

survey = read_csv("data/student_survey_data.csv")

survey$school_values_red

#remove all of the columns we won't be using
survey = survey %>%
  select(-contains("text")) %>%
  select(-c(finished,parents_know))

#make grade and age numeric
survey <- survey %>%
  mutate(grade = as.numeric(str_remove(grade, "th"))) %>%
  mutate(age = as.numeric(age))

#This forces “other” in age to become NA. But only 18 people chose “other” and
#most of them then gave a joke age in age_text so this seems ok


#Q14 ranking ways kids would feel safer

safe_ranks <- survey %>%
  select(student_id, feel_safer_clear_rank:feel_safer_other_rank) %>%
  filter(if_any(-student_id, ~ !is.na(.))) %>%
  filter(if_any(-student_id, ~ is.na(.))) %>%
  mutate(across(-student_id, ~ ifelse(is.na(.), 0, .)))

safe_ranks

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

safe_ranks

survey <- survey %>%
  mutate(across(feel_safer_clear_rank:feel_safer_other_rank, as.numeric))

survey <- rows_update(survey, safe_ranks, by = "student_id")

#Q19 and Q20 about top values

#Select the top rank only, combine into one column called “qual”

ach = survey %>%
  select(student_id, ach, care, happy)

ach = ach %>%
  pivot_longer(ach:happy, names_to = "qual", values_to = "rank")

ach = ach %>%
  filter(rank == 1) %>%
  select(student_id,qual)

survey = survey %>%
  left_join(ach, by = "student_id") %>%
  select(-c(ach:happy))


#Q20: Recode the data to match the answers in Q19

survey <- survey %>%
  mutate(parent_qual =
           recode(parent_qual,
                  "Achieving at a high level" = "ach",
                  "Happiness (feeling good most of the time)" = "happy",
                  "Caring about others" = "care"
                  ))


#Q29 recode to numeric

survey <- survey %>%
  mutate(adults_worry = recode(adults_worry,
                               "Not at all" = "0",
                               "A little bit" = "1",
                               "Moderately" = "2",
                               "A lot" = "3"))






