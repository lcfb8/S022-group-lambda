library(tidyverse)
library(psych)
library(caret)
library(skimr)
library(dplyr)
library(factoextra)

survey_clean <- read.csv("student_survey_data.csv")
### composite by groups 
##############
# Q21 School Fairness & Rules #

# look at distributions
survey_clean %>%
  dplyr::select(rules1:rules9) %>%
  summary()

# check missing values
survey_clean %>%
  dplyr::select(rules1:rules9) %>%
  is.na() %>%
  colSums()


rules_data <- survey_clean %>%
  dplyr::select(rules1:rules9) %>%
  drop_na()

pca_rules <- prcomp(rules_data, scale = TRUE)

# how much does 1 component explain?
get_eigenvalue(pca_rules)

# scree plot (here you have to see a big drop after the first component to justify using a composite )
fviz_eig(pca_rules, addlabels = TRUE)


# chronbach's alpha for rules variables
alpha_rules <- survey_clean %>%
  dplyr::select(rules1:rules9) %>%
  psych::alpha()

# see overall alpha, it should be above 0.7 to be considered "good" reliability, but this is just a rule of thumb.
print(alpha_rules$total$raw_alpha)


# only run this if steps 3 and 4 look good!
survey_clean <- survey_clean %>%
  mutate(
    rules_composite = rowMeans(
      dplyr::select(., rules1:rules9),
      na.rm = TRUE
    )
  )

# check the new variable
survey_clean %>%
  summarise(
    mean   = mean(rules_composite, na.rm = TRUE),
    sd     = sd(rules_composite, na.rm = TRUE),
    min    = min(rules_composite, na.rm = TRUE),
    max    = max(rules_composite, na.rm = TRUE)
  )

get_eigenvalue(pca_rules)


# How each question correlates with the total scale, r.drop should be over 0.3 
print(alpha_rules$item.stats)

# Visualize item means
barplot(alpha_rules$item.stats$mean,
        names.arg = rownames(alpha_rules$item.stats),
        col = "steelblue",
        main = "Average Score Per Question",
        ylab = "Mean Score",
        las = 2)

#######################################################################
# Q16 Discrimination Climate #

# look at distributions
survey_clean %>%
  dplyr::select(disc_race,
         disc_lang,
         disc_wealth,
         disc_religion,
         disc_sex_or,
         disc_gender,
         disc_disab,
         disc_phys_ap,
         disc_country) %>%
  summary()

# check missing values
survey_clean %>%
  dplyr::select(disc_race,
         disc_lang,
         disc_wealth,
         disc_religion,
         disc_sex_or,
         disc_gender,
         disc_disab,
         disc_phys_ap,
         disc_country) %>%
  is.na() %>%
  colSums()


disc_data <- survey_clean %>%
  dplyr::select(disc_race,
         disc_lang,
         disc_wealth,
         disc_religion,
         disc_sex_or,
         disc_gender,
         disc_disab,
         disc_phys_ap,
         disc_country) %>%
  drop_na()

pca_disc <- prcomp(disc_data, scale = TRUE)

# how much does 1 component explain?
get_eigenvalue(pca_disc)

# scree plot
fviz_eig(pca_disc, addlabels = TRUE)


# chronbach's alpha for rules variables
alpha_disc <- survey_clean %>%
  dplyr::select(disc_race,
         disc_lang,
         disc_wealth,
         disc_religion,
         disc_sex_or,
         disc_gender,
         disc_disab,
         disc_phys_ap,
         disc_country) %>%
  psych::alpha()

# see overall alpha, it should be above 0.7 to be considered "good" reliability,
# but this is just a rule of thumb.

print(alpha_disc$total$raw_alpha)


# only run this if steps 3 and 4 look good!
survey_clean <- survey_clean %>%
  mutate(
    disc_composite = rowMeans(
      dplyr::select(., disc_race,
             disc_lang,
             disc_wealth,
             disc_religion,
             disc_sex_or,
             disc_gender,
             disc_disab,
             disc_phys_ap,
             disc_country),
      na.rm = TRUE
    )
  )

# check the new variable
survey_clean %>%
  summarise(
    mean   = mean(disc_composite, na.rm = TRUE),
    sd     = sd(disc_composite, na.rm = TRUE),
    min    = min(disc_composite, na.rm = TRUE),
    max    = max(disc_composite, na.rm = TRUE)
  )

get_eigenvalue(pca_disc)


# How each question correlates with the total scale, r.drop should be over 0.3 
print(alpha_disc$item.stats)

# Visualize item means
barplot(alpha_disc$item.stats$mean,
        names.arg = rownames(alpha_disc$item.stats),
        col = "steelblue",
        main = "Average Score Per Question",
        ylab = "Mean Score",
        las = 2)


#######################################################################
# Q12R: Emotional Safety Composite

# e_safety_score composite variable


# look at distributions
survey_clean %>%
  dplyr::select(esafe1:esafe7) %>%
  summary()

# check missing values
survey_clean %>%
  dplyr::select(esafe1:esafe7) %>%
  is.na() %>%
  colSums()


e_safety_data <- survey_clean %>%
  dplyr::select(esafe1:esafe7) %>%
  drop_na()

pca_e_safety <- prcomp(e_safety_data, scale = TRUE)

# how much does 1 component explain?
get_eigenvalue(pca_e_safety)

# scree plot (here you have to see a big drop after the first component to justify using a composite )
fviz_eig(pca_e_safety, addlabels = TRUE)


# chronbach's alpha for rules variables
alpha_e_safety <- survey_clean %>%
  dplyr::select(esafe1:esafe7) %>%
  psych::alpha()

# see overall alpha, it should be above 0.7 to be considered "good" reliability, but this is just a rule of thumb.
print(alpha_e_safety$total$raw_alpha)


# only run this if steps 3 and 4 look good!
survey_clean <- survey_clean %>%
  dplyr::mutate(
    e_safety_composite = rowMeans(
      dplyr::select(., esafe1:esafe7),
      na.rm = TRUE
    )
  )

# check the new variable
survey_clean %>%
  summarise(
    mean   = mean(e_safety_composite, na.rm = TRUE),
    sd     = sd(e_safety_composite, na.rm = TRUE),
    min    = min(e_safety_composite, na.rm = TRUE),
    max    = max(e_safety_composite, na.rm = TRUE)
  )

get_eigenvalue(pca_e_safety)


# How each question correlates with the total scale, r.drop should be over 0.3 
print(alpha_e_safety$item.stats)

# Visualize item means
barplot(alpha_e_safety$item.stats$mean,
        names.arg = rownames(alpha_e_safety$item.stats),
        col = "steelblue",
        main = "Average Score Per Question",
        ylab = "Mean Score",
        las = 2)
