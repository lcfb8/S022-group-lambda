
#I put all of my code in Claude and asked it to tidy it up. Tidied code below;
#original code is further down after that. My original code involved looking
#at each year individually to explore the data, check for NAs, etc.

library(tidyverse)
library(glue)

# ── Helper functions ──────────────────────────────────────────────────────────

# Build file path per year — folder structure changed across releases
get_censo_path = function(year) {
  base = glue("microdados_censo_da_educacao_superior_{year}")
  file = glue("MICRODADOS_CADASTRO_CURSOS_{year}.CSV")
  
  subfolder = if (year >= 2023) {
    "dados"
  } else if (year == 2022) {
    "microdados_educacao_superior_2022/dados"
  } else if (year == 2021) {
    "Microdados do Censo da Educacao Superior 2021/dados"
  } else {
    # Folder names in 2020 and earlier use garbled encoding for "Educação"
    glue("Microdados do Censo da Educa‡ֶo Superior {year}/dados")
  }
  
  glue("{base}/{subfolder}/{file}")
}

# Read a single census year from disk
read_censo = function(year) {
  read_delim(
    get_censo_path(year),
    delim = ";",
    locale = locale(encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )
}

# Select relevant columns, drop prep programs, and remove NAs
# NAs in QT_CONC* are from distance-learning courses with no completion data
clean_census = function(data) {
  data %>%
    select(NU_ANO_CENSO, NO_CURSO, NO_CINE_AREA_GERAL,
           QT_CONC, QT_CONC_FEM, QT_CONC_MASC) %>%
    filter(NO_CINE_AREA_GERAL != "Programas básicos") %>%
    na.omit()
}

# Summarize degree completions by year x field of study
summarize_census = function(data) {
  data %>%
    summarise(
      .by        = c(NU_ANO_CENSO, NO_CINE_AREA_GERAL),
      QT_CONC = sum(QT_CONC),
      QT_CONC_FEM  = sum(QT_CONC_FEM),
      QT_CONC_MASC = sum(QT_CONC_MASC)
    )
}

# Full pipeline: read → clean → summarize
process_years = function(years) {
  map(years, read_censo) %>%
    bind_rows() %>%
    clean_census() %>%
    summarize_census()
}

# ── Process and save ──────────────────────────────────────────────────────────

out_dir = "../S022-group-lambda/final_project/Brazil File"

censo_2014_24 = process_years(2014:2024)
write_csv(censo_2014_24, file.path(out_dir, "censo_2014_24.csv"))

censo_2009_13 = process_years(2009:2013)
write_csv(censo_2009_13, file.path(out_dir, "censo_2009_13.csv"))

#######################################
#here's my original cleaning script for Brazil higher education census
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

#let's look at censo_2024 a bit more

censo_2024 %>% 
  group_by(NO_CINE_AREA_GERAL) %>% 
  summarise(total = sum(QT_CONC))

#make an updated version of censo_2024 based on the groups of NO_CINE_AREA_GERAL which also includes NU_ANO_CENSO
censo_2024S = censo_2024 %>% 
  group_by(NO_CINE_AREA_GERAL) %>% 
  summarize(total_conc = sum(QT_CONC), 
            total_fem = sum(QT_CONC_FEM), 
            total_masc = sum(QT_CONC_MASC),
            ano = mean(NU_ANO_CENSO) ) %>% 
  rename("area" = NO_CINE_AREA_GERAL)
  

censo_2024S


#let's make these functions and see if we can run them on the others

clean_census = function(data){
  data %>% 
    select(c(NU_ANO_CENSO, NO_CURSO, NO_CINE_AREA_GERAL, 
             QT_CONC, QT_CONC_FEM, QT_CONC_MASC)) %>% 
    filter(NO_CINE_AREA_GERAL != "Programas básicos")
}

concise_census = function(data){
  data %>% 
    group_by(NO_CINE_AREA_GERAL) %>% 
    summarize(total_conc = sum(QT_CONC), 
              total_fem = sum(QT_CONC_FEM), 
              total_masc = sum(QT_CONC_MASC),
              ano = unique(NU_ANO_CENSO)) %>% 
    rename("area" = NO_CINE_AREA_GERAL)
}


########
censo_2023 = read_delim(
  "microdados_censo_da_educacao_superior_2023/dados/MICRODADOS_CADASTRO_CURSOS_2023.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2023 = clean_census(censo_2023)

dim(censo_2023)

censo_2023S = concise_census(censo_2023)

censo_2023S

######
censo_2022 = read_delim(
  "microdados_censo_da_educacao_superior_2022/microdados_educacao_superior_2022/dados/MICRODADOS_CADASTRO_CURSOS_2022.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2022 = clean_census(censo_2022)

dim(censo_2022)

censo_2022S = concise_census(censo_2022)

censo_2022S


############
censo_2021 = read_delim(
  "microdados_censo_da_educacao_superior_2021/Microdados do Censo da Educacao Superior 2021/dados/MICRODADOS_CADASTRO_CURSOS_2021.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2021 = clean_census(censo_2021)

dim(censo_2021)

censo_2021S = concise_census(censo_2021)

censo_2021S

###########

censo_2020 = read_delim(
  "microdados_censo_da_educacao_superior_2020/Microdados do Censo da Educa‡ֶo Superior 2020/dados/MICRODADOS_CADASTRO_CURSOS_2020.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2020 = clean_census(censo_2020)

dim(censo_2020)

censo_2020S = concise_census(censo_2020)

censo_2020S

###########

censo_2019 = read_delim(
  "microdados_censo_da_educacao_superior_2019/Microdados do Censo da Educa‡ֶo Superior 2019/dados/MICRODADOS_CADASTRO_CURSOS_2019.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

dim(censo_2019)

#uh oh our concise_census function produces all NAs!!! let's check for NAs
table(is.na(censo_2019))
# :o so many NAs

#cl<-makePSOCKcluster(parallel::detectCores()-1)
#registerDoParallel(cl)

#skim(censo_2019)

#stopCluster(cl)

#there are 4531 NAs in the CONC categories we're interested in. What are they?

check_censo_2019 = censo_2019 %>% 
  filter(is.na(QT_CONC))

#skim(check_censo_2019)

unique(check_censo_2019$TP_GRAU_ACADEMICO)

unique(censo_2019$TP_DIMENSAO)

#OK so these are all distance learning courses based in Brazil (not int'l)
# we only have data on people enrolling, not completing degrees
#I think we can na.rm these

censo_2019 = clean_census(censo_2019)

#remove all NAs from censo_2019
censo_2019 = na.omit(censo_2019)

censo_2019S = concise_census(censo_2019)

censo_2019S
###########
censo_2018 = read_delim(
  "microdados_censo_da_educacao_superior_2018/Microdados do Censo da Educa‡ֶo Superior 2018/dados/MICRODADOS_CADASTRO_CURSOS_2018.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2018 = clean_census(censo_2018)

dim(censo_2018)

#check for NAs
table(is.na(censo_2018))
#we have NAs :(
#same issue as 2019 - it's all the online classes. let's remove

censo_2018 = na.omit(censo_2018)

censo_2018S = concise_census(censo_2018)

censo_2018S

###########
censo_2017 = read_delim(
  "microdados_censo_da_educacao_superior_2017/Microdados do Censo da Educa‡ֶo Superior 2017/dados/MICRODADOS_CADASTRO_CURSOS_2017.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2017 = clean_census(censo_2017)

dim(censo_2017)
#wow this dataset is a lot smaller hmm

#check for NAs
table(is.na(censo_2017))
#we have NAs :(

#skim(censo_2017)

censo_2017 %>% 
  filter(is.na(QT_CONC)) %>% 
  summarize(mean = mean(TP_DIMENSAO))

censo_2017 = na.omit(censo_2017)

censo_2017S = concise_census(censo_2017)

censo_2017S

####################
censo_2016 = read_delim(
  "microdados_censo_da_educacao_superior_2016/Microdados do Censo da Educa‡ֶo Superior 2016/dados/MICRODADOS_CADASTRO_CURSOS_2016.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

dim(censo_2016)
#from 700k observations in 2024 to under 100k observations in 2016

#check for NAs
table(is.na(censo_2016))
#we have NAs :(

#skim(censo_2016)

censo_2016 %>% 
  filter(is.na(QT_CONC)) %>% 
  summarize(mean = mean(TP_DIMENSAO))

censo_2016 = clean_census(censo_2016)

censo_2016 = na.omit(censo_2016)

censo_2016S = concise_census(censo_2016)

censo_2016S

####################
censo_2015 = read_delim(
  "microdados_censo_da_educacao_superior_2015/Microdados do Censo da Educa‡ֶo Superior 2015/dados/MICRODADOS_CADASTRO_CURSOS_2015.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

#check for NAs
table(is.na(censo_2015))
#we have NAs :(

#skim(censo_2015)

censo_2015 %>% 
  filter(is.na(QT_CONC)) %>% 
  summarize(mean = mean(TP_DIMENSAO))

censo_2015 = clean_census(censo_2015)

censo_2015 = na.omit(censo_2015)

censo_2015S = concise_census(censo_2015)

censo_2015S
####################
censo_2014 = read_delim(
  "microdados_censo_da_educacao_superior_2014/Microdados do Censo da Educa‡ֶo Superior 2014/dados/MICRODADOS_CADASTRO_CURSOS_2014.CSV", 
  delim = ";", locale = locale(encoding = "ISO-8859-1"))

censo_2014 = na.omit(censo_2014)

censo_2014 = clean_census(censo_2014)

censo_2014S = concise_census(censo_2014)

censo_2014S
########################
#Let's put these 10 together before moving on to 2013

censo_2014_24 = bind_rows(censo_2014S, censo_2015S, censo_2016S, censo_2017S, censo_2018S, 
             censo_2019S, censo_2020S, censo_2021S, censo_2022S, censo_2023S, censo_2024S)

censo_2014_24

#save censo_2014_24 as a csv and save it in ../S022-group-lambda/final_project/Brazil File

write_csv(censo_2014_24, "../S022-group-lambda/final_project/Brazil File/censo_2014_24.csv")

#yay

#######################
#can we get even fancier with our functions

read_censo = function(year){
  read_delim(
    glue("microdados_censo_da_educacao_superior_{year}/Microdados do Censo da Educa‡ֶo Superior {year}/dados/MICRODADOS_CADASTRO_CURSOS_{year}.CSV"),
    delim = ";", locale = locale(encoding = "ISO-8859-1")
  )
}

#omg can we nest and map all our functions

years = c(2009:2013)

censo_2009_13all = 
  map(years, read_censo)

censo_2009_13 = bind_rows(censo_2009_13all)

#skim(censo_2009_13)

censo_2009_13 = clean_census(censo_2009_13)

censo_2009_13 = na.omit(censo_2009_13)

censo_2009_13 = censo_2009_13 %>% 
  summarise(.by = c(NU_ANO_CENSO, NO_CINE_AREA_GERAL), 
            total_conc = sum(QT_CONC), 
            total_fem = sum(QT_CONC_FEM), 
            total_masc = sum(QT_CONC_MASC),
            ano = unique(NU_ANO_CENSO)) %>% 
  rename("area" = NO_CINE_AREA_GERAL) %>% 
  select(-NU_ANO_CENSO)

censo_2009_13

write_csv(censo_2009_13, "../S022-group-lambda/final_project/Brazil File/censo_2009_13.csv")


