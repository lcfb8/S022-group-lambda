library( tidyverse )

#combining the brazil education data

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
  filter(area != "Programas ou Cursos Gerais")

brazil_ed = brazil_ed %>% 
  rename( "year" = ano)

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


#the mash up of science, math, comp sci, social sciences, and business is messy
#here's a temporary solution 

#Keep as is, these are good:
#Agricultura, silvicultura, pesca e veterinária 
#Artes e humanidades 
#Educação 
#Engenharia, produção e construção 
#Saúde e bem-estar 
#Serviços 

#combine:
# NEW: Science, math, IT, and social science = 
# Ciências naturais, matemática e estatística + 
# Ciências, matemática e computação +
# Computação e Tecnologias da Informação e Comunicação (TIC) +
# Ciências sociais, comunicação e informação 

#NEW: Business, administration and law = 
# Ciências sociais, negócios e direito +
# Negócios, administração e direito 

#copilot write code for the combined variables above in brazil_ed. When we combine
#columns, if they both have data in them, this data should be summed For example,
#in the new column "Science, math, IT, and social science," the total_fem for 2012
#should be 45748. 

brazil_ed = brazil_ed %>% 
  mutate(area = case_when(
    area %in% c("Ciências naturais, matemática e estatística",
                "Ciências, matemática e computação",
                "Computação e Tecnologias da Informação e Comunicação (TIC)",
                "Ciências sociais, comunicação e informação") ~ "Science, math, IT, & social science",
    area %in% c("Ciências sociais, negócios e direito",
                "Negócios, administração e direito") ~ "Business, administration & law",
    TRUE ~ area
  )) %>%
  group_by(year, area) %>%
  summarise(
    total_conc = sum(total_conc),
    total_fem = sum(total_fem),
    total_masc = sum(total_masc),
    .groups = "drop"
  )

brazil_ed = brazil_ed %>% 
  mutate(area = recode(area,
                       "Agricultura, silvicultura, pesca e veterinária" = "Agriculture & veterinary",
                       "Artes e humanidades" = "Arts & humanities",
                       "Educação"= "Education",
                       "Engenharia, produção e construção"= "Engineering & construction",
                       "Saúde e bem-estar"= "Health & medicine",
                       "Serviços" = "Hospitality"))


brazil_ed %>% 
  ggplot(aes(year,total_conc, col = area))+
  geom_point()

write_csv(brazil_ed, "brazil_ed_recoded.csv")
