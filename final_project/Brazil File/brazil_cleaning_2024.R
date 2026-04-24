library( tidyverse )
library( readxl )
library( skimr )
library( readr )

#cleaning script for Brazil higher education census
#variables we want: NU_ANO_CENSO, NO_CURSO, NO_CINE_AREA_GERAL, QT_CONC,
# QT_CONC_FEM, QT_CONC_MASC


#read 2024 census
censo_2024 = read_delim(
  "microdados_censo_da_educacao_superior_2024/dados/MICRODADOS_CADASTRO_CURSOS_2024.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

#select the variables we want; remove Programas básicos (college prep programs)
censo_2024 = censo_2024 %>% 
  select(c(NU_ANO_CENSO, NO_CURSO, NO_CINE_AREA_GERAL, 
           QT_CONC, QT_CONC_FEM, QT_CONC_MASC)) %>% 
  filter(NO_CINE_AREA_GERAL != "Programas básicos")

#let's take a look
head(censo_2024)

#how many NAs in censo_2024?
table(is.na(censo_2024))
#yay there are no NAs




