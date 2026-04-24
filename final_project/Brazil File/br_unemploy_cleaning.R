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

#########
#ok let's look at 2002-2016

brun_2002_16_raw <- read_csv("brun_2002_2016.csv")

head(brun_2002_16_raw)

brun_2002_16 = brun_2002_16_raw %>% 
  separate_wider_delim(1, delim = ";", names_sep = ".")

head(brun_2002_16)

colnames(brun_2002_16) = brun_2002_16[1,]

brun_2002_16 = brun_2002_16 %>% 
  select(!c(cols = 1:2)) %>% 
  slice(4)

dim(brun_2002_16)

brun_2002_16 = brun_2002_16 %>% 
  pivot_longer(cols = 1:168,
    names_to = "dates", values_to = "rate")

brun_2002_16 = brun_2002_16 %>% 
  separate_wider_delim(dates, delim = " ", names_sep = ".")

brun_2002_16 = brun_2002_16 %>% 
  rename("year" = "dates.2") %>% 
  mutate(rate = as.numeric(rate)) %>% 
  select(-dates.1)

brun_2002_16 = brun_2002_16 %>% 
  group_by(year) %>% 
  summarize(rate = mean(rate))

brun_2002_16

#########
#ok let's look at 1991-2002

brun_1991_02_raw <- read_csv("brun_1991_2002.csv")

head(brun_1991_02_raw)

brun_1991_02 = brun_1991_02_raw[7,]

brun_1991_02 = brun_1991_02[,-1]

brun_1991_02 = brun_1991_02 %>% 
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(cols = 1:143,
               names_to = "dates", values_to = "rate")


brun_1991_02 = brun_1991_02 %>%
separate_wider_delim(dates, delim = " ", names_sep = ".")

brun_1991_02 = brun_1991_02 %>% 
  rename("year" = "dates.2") %>% 
  select(-dates.1)

brun_1991_02 = brun_1991_02 %>% 
  group_by(year) %>% 
  summarize(rate = mean(rate))

brun_1991_02

#################
#let's check out our 3 datasets
brun_1991_02
brun_2002_16
unemploy_brazil

#claude help for unemployment rates

install.packages( "WDI" )
library(WDI)

# World Bank indicator for unemployment
WDI(country = "BR", 
    indicator = "SL.UEM.TOTL.ZS",  # unemployment % of labor force
    start = 2002, 
    end = 2002)
