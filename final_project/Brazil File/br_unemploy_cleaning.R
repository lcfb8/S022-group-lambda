library( tidyverse )
library( skimr )
library(lubridate)

unemploy_brazil_raw <- read_csv("brazil_unemployment_2012_2025.csv")

unemploy_brazil = unemploy_brazil_raw %>% 
  separate_wider_delim(dates, delim = ";", names_sep = ".")

#the first row of unemploy_brazil has the column names
colnames(unemploy_brazil) = unemploy_brazil[1,]

unemploy_brazil = unemploy_brazil[-1,]

unemploy_brazil = unemploy_brazil %>% 
  pivot_longer(cols = 1:168, names_to = "dates", values_to = "rate")

unemploy_brazil = unemploy_brazil %>% 
  separate_wider_delim(dates, delim = " ", names_sep = ".")

unemploy_brazil = unemploy_brazil %>% 
  rename("year" = "dates.2") %>% 
  mutate(rate = as.numeric(rate)) %>% 
  select(-dates.1)

unemploy_brazil = unemploy_brazil %>% 
  group_by(year) %>% 
  summarize(rate = mean(rate))

unemploy_brazil



