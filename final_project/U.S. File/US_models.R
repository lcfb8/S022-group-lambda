library( tidyverse )
library( broom )
library( WDI )

major <- read.csv('~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/S022-group-lambda/final_project/U.S. File/major_ALL.csv')

head(major)

M_loess <- loess(Total ~ gdp_trils + unemploy, data = dat, span = 0.2)

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

econ
econ %>% 
  mutate(thing = factor(thing)) %>% 
  ggplot(aes(year,rate, group = thing, col = thing))+
  geom_point()+
  geom_line()
