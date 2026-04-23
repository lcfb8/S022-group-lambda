

library( tidyverse )
library( readxl )
library( skimr )
library( readr )

tab_1.01 <- read_csv("data/1.01.csv")
tab_2.01 <- read_csv("data/2.01.csv")
tab_2.02 <- read_csv("data/2.02.csv")
tab_2.03 <- read_csv("data/2.03.csv")
tab_2.04 <- read_csv("data/2.04.csv")
tab_2.05 <- read_csv("data/2.05.csv")
tab_3.01 <- read_csv("data/3.01.csv")
tab_3.02 <- read_excel("data/3.02.xlsx")
tab_4.02 <- read_excel("data/4.02.xlsx")

unemploy_brazil <- read_csv("brazil_unemployment_2012_2025.csv")


censo_2024 <- read_excel(
    "data/tabelas_de_divulgacao_censo_da_educacao_superior_2024.xls")

censo_2024

tab_1.01

tab_1.01L = pivot_longer(tab_1.01, cols = !1, 
                         names_to = "type", values_to = "count")

tab_1.01L %>% 
    ggplot(aes(ano, count, col = type)) +
    geom_point() +
    geom_line() +
    labs(title = "1.01 Número de Instituições de Educação Superior, por 
    Organização Acadêmica e Categoria Administrativa – Brasil – 2014-2024")
    

tab_2.01


tab_2.01L = pivot_longer(tab_2.01, cols = !1, 
                         names_to = "type", values_to = "count")

tab_2.01L %>% 
    ggplot(aes(ano, count, col = type))+
    geom_point()+
    geom_line()+
    labs(title = "2.01 Número de Cursos de Graduação, por Modalidade de Ensino 
         e por Grau Acadêmico – Brasil – 2014-2024")

tab_2.02

tab_2.02L = pivot_longer(tab_2.02, cols = c(total, uni, cenuni, fac, ifcefet), 
                                     names_to = "type", values_to = "count")

tab_2.02L = pivot_longer(tab_2.02L, 
                        cols = c(delta_total, delta_uni, delta_cenuni, 
                                 delta_fac, delta_ifcefet), 
                                  names_to = "delta_type", values_to = "delta")
tab_2.02L %>% 
    ggplot(aes(ano, count, col = type)) +
    geom_point()+
    geom_line()+
    labs(title = "2.02 Variação e Número de Cursos de Graduação Presencial de 
         Educação Tecnológica, por Organização Acadêmica – Brasil – 2014-2024")


tab_2.02L$delta = as.numeric(tab_2.02L$delta)


tab_2.02L %>% 
    ggplot(aes(ano, delta, col = delta_type)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Changes in cursos de graduação presencial de educação 
         tecnológica, por organização acadêmica")

tab_2.03L = pivot_longer(tab_2.03, cols = 2:9, 
                         names_to = "year", values_to = "count")

tab_2.03L

tab_2.03LB <- tab_2.03L %>% 
    filter(region == "brasil") %>% 
    filter(year >= 2018) %>% 
    select(area, year, count,type)

head(tab_2.03LB)

tab_2.03LB %>% 
    ggplot(aes(as.numeric(year), count, col = type, group = type)) +
    facet_wrap(~area) +
    geom_point() + 
    geom_line()+
    labs(title = "Número de Ingressos e Concluintes de Cursos de Graduação para 
    cada 10,000 habitantes, segundo a Área Geral do Curso – OCDE 2017/2021 &
         Brasil – 2018-2024")

tab_2.04 = tab_2.04[1:2]

tab_2.04

tab_2.04 = tab_2.04 %>% 
    arrange(matriculas)

tab_2.04

tab_2.04 %>% 
    ggplot(aes(x = reorder(curso, matriculas), y = log(matriculas))) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") + #rotate labels 90 degrees
    labs(
        title = "Número e Frequência de Matrículas de Graduação em Licenciatura, 
        segundo os Cursos de Graduação em Licenciatura com 15 Maiores Números 
        de Matrículas – Brasil – 2024",
        x = "Curso",
        y = "log(Matriculas)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# tab_2.05 <- tab_2.05[-2]

tab_2.05

#in tab_2.05 in the ano column, each 10 rows should list the same year, beginning with 2014 and going to 2024, increasing by 1 year every ten rows
tab_2.05$ano = rep(2014:2024, each = 10)

tab_2.05M = tab_2.05[1:5]
tab_2.05I = tab_2.05[c("ano", "pos", "ingres_curso","ingres_count","ingres_pct")]
tab_2.05C = tab_2.05[c("ano", "pos","conc_curso","conc_count","conc_pct")]

tab_2.05M
tab_2.05I
tab_2.05C

tab_2.05M$type = "matriculas"
tab_2.05I$type = "ingressantes"
tab_2.05C$type = "concluintes"


tab_2.05I = tab_2.05I %>% 
    rename("count" = "ingres_count", "pct" = "ingres_pct", "curso" = "ingres_curso")

tab_2.05C =tab_2.05C %>% 
    rename("count" = "conc_count", "pct" = "conc_pct", "curso" = "conc_curso")

tab_2.05M =tab_2.05M %>% 
    rename("count" = "matric_count", "pct" = "matric_pct", "curso" = "matric_curso")

tab_2.05all = bind_rows(tab_2.05M, tab_2.05I, tab_2.05C)

tab_2.05all

tab_2.05all %>% 
    group_by(curso, type) %>% 
    ggplot(aes(ano, count, col = curso, shape = type))+
    facet_wrap(~type)+
    geom_point() +
    geom_line()+
    labs(title = "2.05 Os 10 maiores Cursos de Graduação em Relação ao Número 
         de Matrículas, de Ingressantes e de Concluintes (Classificação Cine 
         Brasil) – Brasil – 2014-2024")

tab_3.01

tab_3.01$ifcefet_pres = as.numeric(tab_3.01$ifcefet_pres)
tab_3.01$ifcefet_dist = as.numeric(tab_3.01$ifcefet_dist)

tab_3.01_pres = tab_3.01[1:7]

#make a new df called tab_3.01_dist which is columns 1,2,and 8:12 of tab_3.01
tab_3.01_dist = tab_3.01[c(1,2,8:12)]

tab_3.01_pres = tab_3.01_pres %>% 
    pivot_longer(cols = 3:7, names_to = "type", values_to = "count")

tab_3.01_pres$pres_dist = "pres"

tab_3.01_dist$pres_dist = "dist"
tab_3.01_dist = tab_3.01_dist %>% 
    pivot_longer(cols = 3:7, names_to = "type", values_to = "count" )

tab_3.01_dist

tab_3.01 = bind_rows(tab_3.01_dist, tab_3.01_pres)

tab_3.01 = tab_3.01 %>% 
    filter(!is.na(categoria))


#converting NAs to 0
tab_3.01[is.na(tab_3.01)]<-0


tab_3.01$type <- tab_3.01$type %>% 
    recode("total_dist" = "total",
           "uni_dist" = "uni", 
           "cenuni_dist" = "cenuni",
           "fac_dist" = "fac",
           "ifcefet_dist" = "ifcefet", 
           "total_pres" = "total", 
           "uni_pres" = "uni", 
           "cenuni_pres" = "cenuni", 
           "fac_pres" = "fac", 
           "ifcefet_pres" = "ifcefet")

tab_3.01 = tab_3.01 %>% 
    filter(!is.na(categoria))


tab_3.01 %>% 
    ggplot(aes(ano, count/10000, group = categoria, col = categoria)) +
    facet_grid(type~pres_dist) +
    geom_point()+
    geom_line()+
    labs(title = "Número de Matrículas em Cursos de Graduação, por Organização 
         Acadêmica e Modalidade de Ensino, segundo a Categoria Administrativa – 
         Brasil – 2014-2024")


tab_3.02 = tab_3.02 %>% 
    filter(!is.na(grau))


tab_3.02 #in the "Ano" column, every five rows should be the same year. So row
# 1 says 2014; rows 2, 3, 4, and 5 should also have 2014 in the "Ano" column 
# then row 6 should say 2015, and rows 7, 8, 9, and 10 should also say 2015 in the "Ano" column, and so on until row 55 which should say 2024 in the "Ano" column

tab_3.02$ano = rep(2014:2024, each = 5)

names(tab_3.02)

tab_3.02 = tab_3.02 %>% 
    pivot_longer(3:16, names_to = "type", values_to = "count")

tab_3.02 = tab_3.02 %>% 
    separate(type, into = c("inst", "pub_priv", "pres_dist"), sep = "_")

tab_3.02 %>% 
    filter(inst == "total",pub_priv == "priv", pres_dist == "pres")

tab_3.02 %>% 
    filter(pres_dist == "pres") %>% 
    filter(!(inst == "total")) %>%
    filter(!(grau == "Não aplicável")) %>% 
    ggplot(aes(ano,count/10000, group = grau, col = grau)) +
    facet_wrap(~inst)+
    geom_point()+
    geom_line()


tab_4.02$Ano[1:90]= rep(2014:2023, each = 9)

tab_4.02 = tab_4.02 %>% 
    filter(!is.na(cat_admin))

tab_4.02L = tab_4.02 %>% 
    pivot_longer(3:6, names_to = "type", values_to = "count")

tab_4.02L %>% 
    ggplot(aes(Ano, count, group = cat_admin, col = cat_admin))+
    facet_wrap(~type)+
    geom_point()+
    geom_line()


unemploy_brazil <- read_csv("brazil_unemployment_2012_2025.csv")

head(unemploy_brazil)

unemploy_brazil = unemploy_brazil %>% 
  separate_wider_delim(dates, delim = ";", names_sep = ".")

#the first row of unemploy_brazil has the column names
colnames(unemploy_brazil) = unemploy_brazil[1,]

unemploy_brazil = unemploy_brazil[-1,]

unemploy_brazil = unemploy_brazil %>% 
  pivot_longer(cols = 1:168, names_to = "dates", values_to = "rate")

library(lubridate)

unemploy_brazil = unemploy_brazil %>% 
  separate_wider_delim(dates, delim = " ", names_sep = ".")

unemploy_brazil = unemploy_brazil %>% 
  rename("month" = "dates.1",
         "year" = "dates.2")

unemploy_brazil

#create a new column in unemploy_brazil called "month" that is made up of the 
#second term in the column "months". so for example, if we're looking at row 6 of
#months, "jun-jul-ago", the data in "month" would be "jul"
unemploy_brazil = unemploy_brazil %>% 
  separate_wider_delim(month, delim = "-", names_sep = ".")

unemploy_brazil = unemploy_brazil %>% 
  mutate(m_y = paste(month.3, year, sep = "-"))

unemploy_brazil #update the values in m_y with the following: 
# jan = 1, fev = 2, mar = 3, abr = 4, mai = 5, jun = 6, jul = 7, ago = 8, set = 9, out = 10, nov = 11, dez = 12
unemploy_brazil = unemploy_brazil %>% 
  mutate(m_y = case_when(
    month.3 == "jan" ~ paste(1, year, sep = "-"),
    month.3 == "fev" ~ paste(2, year, sep = "-"),
    month.3 == "mar" ~ paste(3, year, sep = "-"),
    month.3 == "abr" ~ paste(4, year, sep = "-"),
    month.3 == "mai" ~ paste(5, year, sep = "-"),
    month.3 == "jun" ~ paste(6, year, sep = "-"),
    month.3 == "jul" ~ paste(7, year, sep = "-"),
    month.3 == "ago" ~ paste(8, year, sep = "-"),
    month.3 == "set" ~ paste(9, year, sep = "-"),
    month.3 == "out" ~ paste(10, year, sep = "-"),
    month.3 == "nov" ~ paste(11, year, sep = "-"),
    month.3 == "dez" ~ paste(12, year, sep = "-")
  ))

unemploy_brazil$m_y = my(unemploy_brazil$m_y)


unemploy_brazil$rate = parse_number(unemploy_brazil$rate)


unemploy_brazil %>% 
  ggplot(aes(m_y,rate))+
  geom_point() +
  geom_line()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 


censo_2024S = read_csv("microdados_1000.csv")

head(censo_2024S)

#which columns of censo_2024S contain only NAs?
colnames(censo_2024S)[sapply(censo_2024S, function(x) all(is.na(x)))]

censo_2024S = censo_2024S %>% 
  select(!c("NO_REGIAO","CO_REGIAO","NO_UF","SG_UF","CO_UF", 
            "NO_MUNICIPIO","CO_MUNICIPIO","IN_CAPITAL"))

table(censo_2024S$NO_CINE_AREA_GERAL)

#make a bar plot of censo_2024S$NO_CINE_AREA_GERAL
censo_2024S %>%  
  ggplot(aes(x = NO_CINE_AREA_GERAL)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency of NO_CINE_AREA_GERAL in censo_2024S")


censo_2024S %>%  
  ggplot(aes(x = QT_ING_FEM, y = QT_ING_MASC)) +
  facet_wrap(~NO_CINE_AREA_GERAL)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

skim(censo_2024S)

table(censo_2024S$QT_SIT_FALECIDO)


censo_2024S %>% 
  select(NO_CURSO) %>% 
  arrange(n()) %>% 
  table()

censo_2024 = read_delim("microdados_censo_da_educacao_superior_2024/dados/MICRODADOS_CADASTRO_CURSOS_2024.CSV", delim = ";")

head(censo_2024)

skim(censo_2024)

table(censo_2024[31])

#select only the variables that we want to look at
censo_2024V = censo_2024 %>% 
  select(c(1, 2, 5, 11, 12, 16, 17, 19, 22, 24, 26, 27, 28, 29, 40, 48, 49, 50, 
           78, 79, 80, 97, 98, 99, 126, 136, 146, 211:218))

head(censo_2024V)

names(censo_2024V)

table(censo_2024V$NO_CINE_AREA_GERAL)

censo_2024V %>% 
  filter(NO_CINE_AREA_GERAL == "Programas b\xe1sicos") %>% 
  select(-c(NO_REGIAO,SG_UF,TP_ORGANIZACAO_ACADEMICA,TP_REDE)) %>% 
  sample(NO_CURSO)

censo_2024V %>% 
  select(NO_CURSO,TP_GRAU_ACADEMICO,NO_CINE_AREA_GERAL) %>% 
  sample()

censo_2024V <- censo_2024V %>% 
  group_by(NO_CINE_AREA_GERAL) %>% 
  mutate(QT_CONC_FEM_CURSO = sum(QT_CONC_FEM))

censo_2024V %>% 
  ggplot(aes(NO_CINE_AREA_GERAL, QT_CONC_FEM_CURSO)) +
  geom_point()


colnames(censo_2024V)[sapply(censo_2024V, function(x) all(is.na(x)))]

#do any of the columns in censo_2024 only contain 0s?
colnames(censo_2024V)[sapply(censo_2024V, function(x) all(x == 0))]







