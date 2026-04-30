library( tidyverse )

#combining the brazil education data

br1995_99 = read_csv("1995_1999_clean.csv")
br2000_08 = read_csv("2000_2008_clean.csv")
br2009_13 = read_csv("censo_2009_13.csv")
br2014_24 = read_csv("censo_2014_24.csv")


brazil_ed = bind_rows(br1995_99,br2000_08,br2009_13,br2014_24)


brazil_ed = brazil_ed %>% 
  rename( "area" = NO_CINE_AREA_GERAL,
          "year" = NU_ANO_CENSO,
          "total_conc" = QT_CONC,
          "total_fem" =  QT_CONC_FEM,
          "total_masc" = QT_CONC_MASC) %>% 
  filter(area != "Programas ou Cursos Gerais")


table(brazil_ed$area)

#which major categories can we combine?
brazil_ed %>% 
  filter(str_detect(area, "direito")) %>%
  #filter(area == "Negócios, administração e direito") %>% 
  ggplot(aes(year, total_conc, col = area))+
  geom_point()
#these don't overlap, look ok to combine

brazil_ed %>% 
  filter(str_detect(area, "matemática") 
         | str_detect(area, "computação")
         | str_detect(area, "Computação")) %>%
  ggplot(aes(year, total_conc, col = area))+
  geom_point()

brazil_ed %>% 
  filter(str_detect(area, "sociais")) %>%
  ggplot(aes(year, total_conc, col = area))+
  geom_point()

table(brazil_ed$area)

brazil_ed %>% 
  filter(area %in% c("Ciências naturais, matemática e estatística",
                     "Ciências sociais, comunicação e informação", 
                     "Ciências sociais, negócios e direito",
                     "Ciências, matemática e computação",
                     "Computação e Tecnologias da Informação e Comunicação (TIC)",
                     "Negócios, administração e direito")) %>% 
  ggplot(aes(year, total_conc, col = area))+
  geom_point()


# combine some columns that should be in the same category
brazil_ed = brazil_ed %>% 
  mutate(area = case_when(
    area == "Artes e humanidades" ~ "Artes e humanidades",
    area == "Humanidades e artes" ~ "Artes e humanidades",
    TRUE ~ area
  )) %>%
  mutate(area = case_when(
    area == "Agricultura e veterinária" ~ "Agricultura, silvicultura, pesca e veterinária",
    area == "Agricultura, silvicultura, pesca e veterinária" ~ "Agricultura, silvicultura, pesca e veterinária",
    TRUE ~ area
  )) %>%
  mutate(area = case_when(
    area == "Saúde e bem estar social" ~ "Saúde e bem-estar",
    area == "Saúde e bem-estar" ~ "Saúde e bem-estar",
    TRUE ~ area
  ))


# the mash up of science, math, comp sci, social sciences, and business is messy
# we need to align with the US categories

# Other/Unknown = Agricultura, silvicultura, pesca e veterinária 
# Arts & Humanities = Artes e humanidades 
# Business & Management = Negócios, administração e direito + Servicios + Ciências sociais, negócios e direito
# Behavioral & Social Sciences = Ciências sociais, comunicação e informação 
# Health & Medical = Saúde e bem-estar 
# Education = Educação 
# Engineering = Engenharia, produção e construção + Computação e Tecnologias da Informação e Comunicação
# Natural Sciences = Ciências naturais, matemática e estatística + Ciências, matemática e computação


#copilot help write code for the combined variables above in brazil_ed. When we combine
#columns, if they both have data in them, this data should be summed For example,
#in the new column "Science, math, IT, and social science," the total_fem for 2012
#should be 45748. 

brazil_ed = brazil_ed %>% 
  mutate(area = case_when(
    area %in% c("Ciências naturais, matemática e estatística",
                "Ciências, matemática e computação")
                 ~ "Natural Sciences",
    area %in% c("Ciências sociais, negócios e direito",
                "Negócios, administração e direito", "Serviços") ~ 
      "Business & Management",
    area %in% c("Engenharia, produção e construção", 
    "Computação e Tecnologias da Informação e Comunicação (TIC)") ~ "Engineering",
    TRUE ~ area
  )) %>%
  group_by(year, area) %>%
  summarise(
    total_conc = sum(total_conc),
    total_fem = sum(total_fem),
    total_masc = sum(total_masc),
    .groups = "drop"
  )

table(brazil_ed$area)

brazil_ed = brazil_ed %>% 
  mutate(area = recode(area,
                       "Agricultura, silvicultura, pesca e veterinária" = "Other/Unknown",
                       "Artes e humanidades" = "Arts & Humanities",
                       "Educação"= "Education",
                       "Ciências sociais, comunicação e informação"= "Social & Behavioral Sciences",
                       "Saúde e bem-estar"= "Health & Medical"))


#Claude helped me add the totals
brazil_ed = brazil_ed %>%
  group_by(year) %>%
  summarise(
    area = "Total",
    total_conc = sum(total_conc, na.rm = TRUE),
    total_fem  = sum(total_fem,  na.rm = TRUE),
    total_masc = sum(total_masc, na.rm = TRUE)
  ) %>%
  bind_rows(brazil_ed) %>%          # combine with original data
  arrange(year, area)               # sort so "Total" appears with each year

brazil_ed

write_csv(brazil_ed, "brazil_ed_recoded.csv")

brazil_ed %>%
  mutate(check = total_fem + total_masc == total_conc) %>%
  filter(check == FALSE)


# let's make a dataset that has proportions of total_conc, total_fem, and total_masc for each area (divide by Total for each year) rather than totals

brazil_edprop = brazil_ed %>%
  group_by(year) %>%
  mutate(
    prop_conc = total_conc / total_conc[area == "Total"],
    prop_fem = total_fem / total_fem[area == "Total"],
    prop_masc = total_masc / total_masc[area == "Total"]
  ) %>%
  ungroup()

head(brazil_edprop)

write_csv(brazil_edprop, "brazil_ed_proportions.csv")

#############################################
########################################################
# Brazil economic data
#unemployment
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


######
#combine education and economic data (if we need?)
brazil = full_join(brazil_ed, desemprego, by = "year")

brazil = full_join(brazil, gdp_brazil, by = "year")

brazil

write_csv(brazil, "brazil_all.csv")
  
