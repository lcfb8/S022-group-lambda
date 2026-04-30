library( tidyverse )
library( broom )

brazil_ed <- read_csv("brazil_ed_recoded.csv")
alt_1996 <- read_csv("brazil_1996_alt.csv")
brazil_edprop <- read_csv("brazil_ed_proportions.csv")
brazil_all <-read_csv("brazil_all.csv")



M_loess <- loess(total_conc ~ unemploy + gdp, data = brazil_all, span = 0.2)

head(brazil_all)


brazil_all %>% 
  mutate(area = as.factor(area)) %>% 
  ggplot(aes(unemploy,total_conc,group = area, color = area))+
  geom_point()+
  geom_smooth()
