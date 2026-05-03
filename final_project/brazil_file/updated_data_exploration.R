library( tidyverse )
library( skimr )
library( stringr )
library( WDI )
library( readr )


brazil_ed <- read_csv("~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/Final project/MICRODADOS/all_years/data/processed/master_graduations.csv")
alt_1996 <- read_csv("~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/S022-group-lambda/final_project/brazil_file/brazil_1996_alt.csv")

alt_1996 = alt_1996 %>% 
  rename("cine_area_name" = "NO_CINE_AREA_GERAL",
         "grads_total" = "QT_CONC",
         "grads_female" = "QT_CONC_FEM",
         "grads_male" = "QT_CONC_MASC",
         "year"= "NU_ANO_CENSO") %>% 
  mutate(cine_area_name = recode(cine_area_name,
                    "Agricultura, silvicultura, pesca e veterinária" = 
                      "Agricultura, Silvicultura, Pesca E Veterinária",
                    "Artes e humanidades" = "Artes E Humanidades",
                    "Ciências naturais, matemática e estatística" = 
                      "Ciências Naturais, Matemática E Estatística",
                    "Engenharia, produção e construção" = 
                      "Engenharia, Produção E Construção",
                    "Negócios, administração e direito" = 
                      "Negócios, Administração E Direito",
                    "Saúde e bem-estar" = "Saúde E Bem-Estar"
                    )) %>% 
  filter(cine_area_name != "Total")


alt_1996

names(brazil_ed)

head(brazil_ed)

brazil_ed %>% 
  group_by(cine_area_name,year) %>% 
  ggplot(aes(year,grads_total, group = cine_area_name, col = cine_area_name))+
  geom_point()+
  geom_line()

dim(brazil_ed)

head(brazil_ed$data_quality_flag)

unique(brazil_ed$degree_type)

brazil_totals = brazil_ed %>% 
  filter(cine_area_name != "Programas Básicos") %>% 
  filter(degree_type %in% c("licenciatura", "bacharelado")) %>%
  filter(year != 1996) %>% 
  bind_rows(alt_1996) %>% 
  group_by(cine_area_name, year) %>% 
  summarize(grads_female = sum(grads_female),
            grades_male = sum(grads_male),
            grads_total = sum(grads_total))


brazil_totals %>% 
  ggplot(aes(year,grads_total, col = cine_area_name))+
  geom_point()+
  geom_line()
