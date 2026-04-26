library( tidyverse )
library( skimr )
library( WDI )


# Brazil unemployment
desemprego = data.frame(WDI(country = "BR", 
        indicator = "SL.UEM.TOTL.ZS",  # unemployment % of labor force
        start = 1995, 
        end = 2024))

desemprego = desemprego %>% 
  select(year,SL.UEM.TOTL.ZS) %>% 
  rename("unemploy" = SL.UEM.TOTL.ZS)


# Brazil GDP
gdp_brazil = data.frame(WDI(country = "BR", 
      indicator = "NY.GDP.MKTP.CD",  # GDP in current US dollars
      start = 1995, 
      end = 2024))

gdp_brazil = gdp_brazil %>% 
  select(year, NY.GDP.MKTP.CD) %>% 
  rename( "gdp" = NY.GDP.MKTP.CD)


#combined brazil education data

br1995_99 = read_csv("1995_1999_clean.csv")
br2000_08 = read_csv("2000_2008_clean.csv")
br2009_13 = read_csv("censo_2009_13.csv")
br2014_24 = read_csv("censo_2014_24.csv")

br1995_99 = br1995_99 %>% 
  rename( "area" = NO_CINE_AREA_GERAL,
          "ano" = NU_ANO_CENSO,
          "total_conc" = QT_CONC,
          "total_fem" =  QT_CONC_FEM,
          "total_masc" = QT_CONC_MASC)

br2000_08 = br2000_08 %>% 
  rename( "area" = NO_CINE_AREA_GERAL,
          "ano" = NU_ANO_CENSO,
          "total_conc" = QT_CONC,
          "total_fem" =  QT_CONC_FEM,
          "total_masc" = QT_CONC_MASC)

brazil_ed = bind_rows(br1995_99,br2000_08,br2009_13,br2014_24)

brazil_ed = brazil_ed %>% 
  rename( "year" = ano)

brazil = full_join(brazil_ed, desemprego, by = "year")
brazil = full_join(brazil, gdp_brazil, by = "year")

brazil

write_csv(brazil, "brazil_all.csv")

#############
#OK LET'S PLOT!

#no scientific notation
options(scipen = 999)

brazil = read_csv("brazil_all.csv")

brazil = brazil %>% 
  mutate(gdp_trils = gdp/100000000000) 

brazilL = brazil %>% 
  select(-gdp) %>% 
  pivot_longer(cols = c(gdp_trils,unemploy), 
               names_to = "econ", values_to = "rate")

head(brazilL)
brazilL %>% 
  ggplot(aes(year, rate, col = econ)) +
  geom_line()






