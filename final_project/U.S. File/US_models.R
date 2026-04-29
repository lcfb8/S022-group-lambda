library( tidyverse )
library( broom )
library( WDI )

major_df <- read.csv('~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/S022-group-lambda/final_project/U.S. File/major_final_df.csv')

head(major_df)

#pivot wider major_df so we can run the loess model below with gdp_trils and unemploy

dat = major_df %>%
  select(-X) %>%  # drop row index
  pivot_wider(names_from = "econ", values_from = "rate")

dat


M_loess <- loess(Total ~ gdp_trils + unemploy, data = dat, span = 0.2)

dat %>% 
  filter(BACHELORS !="Grand total") %>% 
  ggplot(aes(unemploy, Total, group = BACHELORS, color = BACHELORS))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.5, se = FALSE)

dat %>% 
  filter(BACHELORS !="Grand total") %>% 
  ggplot(aes(gdp_trils, Total, group = BACHELORS, color = BACHELORS))+
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
  ggplot(aes(year, gdp))+
  geom_point()+
  geom_line()
