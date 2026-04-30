library( tidyverse )
library( broom )
library( WDI )

major <- read.csv('~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/S022-group-lambda/final_project/U.S. File/major_ALL.csv')

head(major)

major %>% 
  filter(BACHELORS !="Grand total") %>% 
  ggplot(aes(unemploy, Total, group = BACHELORS, color = BACHELORS))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.5, se = FALSE)

major %>% 
  filter(BACHELORS !="Grand total") %>% 
  ggplot(aes(gdp, Total, group = BACHELORS, color = BACHELORS))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.5, se = FALSE)

major_prop = major %>% 
  #calculate the proportion of Total, Total_Men, and Total_Women for each YEAR and BACHELORS
  group_by(YEAR) %>%
   mutate(prop_total = Total / Total[BACHELORS == "Grand total"],
          prop_men = Total_Men / Total_Men[BACHELORS == "Grand total"],
          prop_women = Total_Women / Total_Women[BACHELORS == "Grand total"]) %>%
  select(-c(X,GDP,UNRATE)) %>% 
  ungroup()

major_prop %>% 
  filter(BACHELORS != "Grand total") %>% 
  ggplot(aes(YEAR,prop_total,color = BACHELORS))+
  geom_point()+
  geom_line()


#let's look at WDI economic indicators

unemploy = data.frame(WDI(country = "US", 
                            indicator = "SL.UEM.TOTL.ZS",  # unemployment % of labor force
                            start = 1995, 
                            end = 2024))

 unemploy = unemploy %>% 
  select(year,SL.UEM.TOTL.ZS) %>% 
  rename("unemploy" = SL.UEM.TOTL.ZS)


# US GDP
gdp = data.frame(WDI(country = "US", 
                            indicator = "NY.GDP.MKTP.CD",  # GDP in current US dollars
                            start = 1995, 
                            end = 2024))

gdp = gdp %>% 
  select(year, NY.GDP.MKTP.CD) %>% 
  rename( "gdp" = NY.GDP.MKTP.CD)


#plot unemploy and gdp
unemploy %>% 
  ggplot(aes(year, unemploy))+
  geom_point()+
  geom_line()
 
gdp %>% 
  ggplot(aes(year, log(gdp)))+
  geom_point()+
  geom_line()

#does WDI have percent change in GDP over time?


gdp_growth = data.frame(WDI(country = "US", 
                     indicator = "NY.GDP.MKTP.KD.ZG",  # GDP growth (annual %)
                     start = 1995, 
                     end = 2024))


gdp_growth = gdp_growth %>% 
  rename("growth" = NY.GDP.MKTP.KD.ZG) %>% 
  select(4:5)

gdp_growth %>% 
  ggplot(aes(year,growth)) +
  geom_point()+
  geom_line()
  

econ = full_join(gdp_growth, unemploy, by = "year")

econ = pivot_longer(econ, cols = c(growth,unemploy), 
                    names_to = "thing", values_to = "rate")

econ %>% 
  mutate(thing = factor(thing)) %>%
  ggplot(aes(year,rate, group = thing, col = thing))+
  geom_point()+
  geom_line()
