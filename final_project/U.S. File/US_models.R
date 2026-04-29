library( tidyverse )
library( broom )

major_df <- read.csv('~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/S022-group-lambda/final_project/U.S. File/major_final_df.csv')

head(major_df)

#pivot wider major_df so we can run the loess model below with gdp_trils and unemploy

dat = major_df %>% 
  pivot_wider(names_from = "econ", values_from = "rate") %>% 
  #collapse rows with the same values
  group_by(BACHELORS, YEAR) %>% 
  mutate(gdp_trils = max(gdp_trils, na.rm = TRUE)) %>% 
  mutate(unemploy = max(unemploy, na.rm = TRUE)) %>% 
  #keep the groupings so there aren't duplicates in the new df
  ungroup() %>%
  distinct()

dat


M_loess <- loess(Total ~ gdp_trils + unemploy, data = major_df, span = 0.2)

#fix the below code 

ggplot(brazil_all, aes(unemploy, total_conc, group = area, color = area))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.5, se = FALSE)

#what's the r squared for M_loess
summary(M_loess)$r.squared
