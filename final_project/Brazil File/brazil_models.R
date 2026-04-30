library( tidyverse )
library( broom )

brazil_ed <- read_csv("brazil_ed_recoded.csv")
alt_1996 <- read_csv("brazil_1996_alt.csv")
brazil_edprop <- read_csv("brazil_ed_proportions.csv")
brazil_all <- read_csv("brazil_all.csv")

#let's make an inverse variable for unemployment

brazil_all = brazil_all %>% 
  mutate(employ = 100-unemploy)


M_loess <- loess(total_conc ~ employ + gdp, data = brazil_all, span = 0.2)

#fix the below code 

ggplot(brazil_all, aes(unemploy, total_conc, group = area, color = area))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.5, se = FALSE)

#what's the r squared for M_loess
summary(M_loess)$r.squared
