
library(readxl)
library(tidyverse)
library(dplyr)

# open all data sets
df2010 <- read_excel("indicadores_trajetoria_educacao_superior_2010_2019.xlsx") 
df2011 <- read_excel("indicadores_trajetoria_educacao_superior_2011_2020.xlsx")
df2012 <- read_excel("indicadores_trajetoria_educacao_superior_2012_2021.xlsx")
df2013 <- read_excel("indicadores_trajetoria_educacao_superior_2013_2022.xlsx")
df2014 <- read_excel("indicadores_trajetoria_educacao_superior_2014_2023.xlsx")
df2015 <- read_excel("indicadores_trajetoria_educacao_superior_2015_2024.xlsx")
df2016 <- read_excel("indicadores_trajetoria_educacao_superior_2016_2024.xlsx")
df2017 <- read_excel("indicadores_trajetoria_educacao_superior_2017_2024.xlsx")
df2018 <- read_excel("indicadores_trajetoria_educacao_superior_2018_2024.xlsx")
df2019 <- read_excel("indicadores_trajetoria_educacao_superior_2019_2024.xlsx")
df2020 <- read_excel("indicadores_trajetoria_educacao_superior_2020_2024.xlsx")

# combine all data sets into one
dfall <- bind_rows(df2010, df2011, df2012, df2013, df2014, df2015, df2016, df2017, 
                df2018, df2019, df2020)
dim(dfall)
head(dfall)


# remove the first 5 rows
dfall <- dfall[-c(1:7),]

# convert the first row into column names
colnames(dfall) <- dfall[1,]
dfall <- dfall[-1,]

dfallclean <- dfall

# clean entires rows that has "Código da Instituição" 

df_clean <- dfallclean %>%
  filter(!if_any(everything(), ~ .x == "Código da Instituição"))  %>%
  filter(!if_any(everything(), ~ .x == "NO_CINE_AREA_GERAL"))

# filter by areas NO_CINE_AREA_GERAL
artes <- df_clean %>% filter( CO_CINE_AREA_GERAL == "02" | 
                       CO_CINE_AREA_GERAL == "03")  


class(df_clean$CO_CINE_AREA_GERAL)


artes <- df_clean %>% filter( NO_CINE_ROTULO == "Música" |
                       NO_CINE_ROTULO == "Artes Cênicas" | 
                       NO_CINE_ROTULO == "Artes Visuais")
                


# plot the number of students by year (NU_ANO_INGRESSO) and area 
# (NO_CINE_AREA_GERAL)
ggplot(artes, aes(x = NU_ANO_INGRESSO, fill = NO_CINE_AREA_GERAL)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Students by Year and Area",
       x = "Year of Admission",
       y = "Number of Students",
       fill = "Area") +
  theme_minimal()


# the same for students in stem areas

stem <- df_clean %>% filter( CO_CINE_AREA_GERAL == "08" )  

# plot the number of students by year (NU_ANO_INGRESSO) and area
ggplot(stem, aes(x = NU_ANO_INGRESSO, fill = CO_CINE_AREA_GERAL)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Students by Year and Area",
       x = "Year of Admission",
       y = "Number of Students",
       fill = "Area") +
  theme_minimal()


# convert QT_INGRESSO into numeric varibles
# I was running out of memory with the code I had, ChatGPT suggested
# the below workaround
# 1) Convert types directly, without piping
df_clean$NU_ANO_INGRESSO  <- as.factor(df_clean$NU_ANO_INGRESSO)
df_clean$QT_INGRESSANTE   <- as.numeric(df_clean$QT_INGRESSANTE)

# 2) Then filter rows once
df_clean <- df_clean[ 
  !is.na(df_clean$QT_INGRESSANTE) &
    !is.na(df_clean$NO_CINE_AREA_GERAL) &
    !is.na(df_clean$NU_ANO_INGRESSO),
]


# change names of values:

df_clean$NO_CINE_AREA_GERAL <- as.character(df_clean$NO_CINE_AREA_GERAL)

# 1) Define translation vector: names = original Portuguese, values = English
cine_map <- c(
  # Education
  "Educação"                                           = "Education",
  
  # Arts/Humanities
  "Artes e humanidades"                                = "Arts & Humanities",
  
  # Social sciences / communication / info (two variants)
  "Ciências sociais, jornalismo e informação"          = "Social Sci., Journalism & Info",
  "Ciências sociais, comunicação e informação"         = "Social Sci., Communication & Info",
  
  # Business/admin/law
  "Negócios, administração e direito"                  = "Business, Admin & Law",
  
  # Natural sciences / math / stats
  "Ciências naturais, matemática e estatística"        = "Natural Sci., Math & Stats",
  
  # ICT / computing (two variants)
  "Tecnologia da informação e comunicação (TIC)"       = "ICT",
  "Computação e Tecnologias da Informação e Comunicação (TIC)" =
    "Computing & ICT",
  
  # Engineering / production / construction
  "Engenharia, produção e construção"                  = "Engineering, Manufacturing & Construction",
  
  # Agriculture / forestry / fisheries / vet
  "Agricultura, silvicultura, pesca e veterinária"     = "Agriculture, Forestry, Fisheries & Vet",
  
  # Health & welfare (two variants)
  "Saúde e bem‑estar"                                  = "Health & Welfare",
  "Saúde e bem-estar"                                  = "Health & Welfare",
  
  # Services
  "Serviços"                                           = "Services"
)

df_clean$NO_CINE_AREA_GERAL <- as.character(df_clean$NO_CINE_AREA_GERAL)
df_clean$CINE_AREA_EN <- cine_map[df_clean$NO_CINE_AREA_GERAL]

# Check for any remaining unmatched labels
unique(df_clean$NO_CINE_AREA_GERAL[is.na(df_clean$CINE_AREA_EN)])
# 2) Coerce to character if it's a factor
df_clean$NO_CINE_AREA_GERAL <- as.character(df_clean$NO_CINE_AREA_GERAL)

# 3) Create new column using the map
df_clean$CINE_AREA_EN <- cine_map[df_clean$NO_CINE_AREA_GERAL]

# 4) Optional: see if anything didn't match
unique(df_clean$NO_CINE_AREA_GERAL[is.na(df_clean$CINE_AREA_EN)])

unmatched <- unique(df_clean$NO_CINE_AREA_GERAL[is.na(df_clean$CINE_AREA_EN)])
unmatched
length(unmatched)

# proport of students in each area by year


df_plot_prop <- df_plot %>%
  group_by(NU_ANO_INGRESSO) %>%
  mutate(prop = QT / sum(QT)) %>%
  ungroup()


# had to manually assign colors to each CINE area because the default palette 
# was so confusing

manual_cols <- c(
  "Education"                               = "#1b9e77",
  "Arts & Humanities"                       = "#d95f02",
  "Social Sci., Journalism & Info"          = "#7570b3",
  "Social Sci., Communication & Info"       = "#e7298a",
  "Business, Admin & Law"                   = "#66a61e",
  "Natural Sci., Math & Stats"              = "#e6ab02",
  "ICT"                                     = "#a6761d",
  "Computing & ICT"                         = "#666666",
  "Engineering, Manufacturing & Construction" = "#1f78b4",
  "Agriculture, Forestry, Fisheries & Vet"  = "#b2df8a",
  "Health & Welfare"                        = "#fb9a99",
  "Services"                                = "#8dd3c7"
)

ggplot(df_plot_prop,
       aes(x = NU_ANO_INGRESSO,
           y = prop,
           color = CINE_AREA_EN,
           group = CINE_AREA_EN)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = manual_cols) +
  labs(
    title = "Proportion of Ingressantes by CINE Area and Year",
    x     = "Year of Admission (NU_ANO_INGRESSO)",
    y     = "Proportion of QT_INGRESSANTE",
    color = "CINE Area"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


