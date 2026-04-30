####### 2000 ######
### join!####

# Load raw files
dat_2000_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2000_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE, stringsAsFactors = FALSE, sep = "|", fileEncoding = "latin1"
)

print(dat_2000_to_merge$C0811)

dat_2000_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2000_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE, stringsAsFactors = FALSE, sep = "|", fileEncoding = "latin1"
)


clean_graduates_2000 <- function(dat_raw, year, output_path) {
  # turns fem and masc NAs into zero
  dat_raw$C0813[is.na(dat_raw$C0813)]   <- 0
  dat_raw$C0811[is.na(dat_raw$C0811)]   <- 0
  dat_raw$C0821[is.na(dat_raw$C0821)]   <- 0
  dat_raw$C0823[is.na(dat_raw$C0823)]   <- 0
  dat_raw$C0812[is.na(dat_raw$C0812)]   <- 0
  dat_raw$C0814[is.na(dat_raw$C0814)]   <- 0
  dat_raw$C0822[is.na(dat_raw$C0822)]   <- 0
  dat_raw$C0824[is.na(dat_raw$C0824)]   <- 0

  # Step 1: Select and rename columns
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NOMEAREAGERAL,
    QT_CONC_FEM        = dat_raw$C0811 + dat_raw$C0813 + dat_raw$C0821 + dat_raw$C0823,
    QT_CONC_MASC       = dat_raw$C0812 + dat_raw$C0814 + dat_raw$C0822 + dat_raw$C0824
  )
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save
  write.csv(dat_agg, output_path, row.names = FALSE)
  message("Done! Saved: ", output_path)
  
  return(dat_agg)  
}

# ---- Run for presencial 2000 ----
clean_2000 <- clean_graduates_2000(
  dat_raw     = dat_2000_to_merge,
  year        = 2000,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean.csv"
)


# ---- function for online learning ----
clean_graduates_online <- function(dat_raw, year, output_path) {
  # Step 2: Replace NAs with 0
  dat_raw$C2211[is.na(dat_raw$C2211)]   <- 0
  dat_raw$C2212[is.na(dat_raw$C2212)] <- 0
  
  # Step 1: Select and rename columns
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NOMEAREAGERAL,
    QT_CONC_FEM        = dat_raw$C2211,
    QT_CONC_MASC       = dat_raw$C2212
  )
  
 
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save
  write.csv(dat_agg, output_path, row.names = FALSE)
  message("Done! Saved: ", output_path)
  
  return(dat_agg)  
}


# ---- Run for distancia ----
clean_2000_dis <- clean_graduates_online(
  dat_raw     = dat_2000_to_merge_dis,
  year        = 2000,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean_dis.csv"
)


clean_2000_merged <- rbind(clean_2000, clean_2000_dis)

write.csv(
  clean_2000_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean.csv",
  row.names = FALSE
)

# Step 2: Aggregate again to sum across presencial + distancia
clean_2000_final <- aggregate(
  cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
  data = clean_2000_combined,
  FUN  = sum
)

# Step 3: Add year back
clean_2000_final$NU_ANO_CENSO <- 2000

# Step 4: Save
write.csv(
  clean_2000_final,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean_final.csv",
  row.names = FALSE
)

####### 2001 ######
### merge!####

dat_2001_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2001_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# ---- Run for presencial ----
clean_2001 <- clean_graduates_2000(
  dat_raw     = dat_2001_to_merge,
  year        = 2001,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean.csv"
)


dat_2001_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2001_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for distancia ----
clean_2001_dis <- clean_graduates_online(
  dat_raw     = dat_2001_to_merge_dis,
  year        = 2001,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean_dis.csv"
)


clean_2001_merged <- rbind(clean_2001, clean_2001_dis)

clean_2001_merged <- clean_2001_merged[clean_2001_merged$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]

write.csv(
  clean_2001_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2001_clean.csv",
  row.names = FALSE
)


####### 2002 ######
### join!####


dat_2002_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2002_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


clean_graduates_2002 <- function(dat_raw, year, output_path) {
  # turns fem and masc NAs into zero
  dat_raw$C8211[is.na(dat_raw$C8211)]   <- 0
  dat_raw$C8212[is.na(dat_raw$C8212)]   <- 0
  dat_raw$C8213[is.na(dat_raw$C8213)]   <- 0
  dat_raw$C8214[is.na(dat_raw$C8214)]   <- 0
  dat_raw$C8221[is.na(dat_raw$C8221)]   <- 0
  dat_raw$C8222[is.na(dat_raw$C8222)]   <- 0
  dat_raw$C8223[is.na(dat_raw$C8223)]   <- 0
  dat_raw$C8224[is.na(dat_raw$C8224)]   <- 0
  
  # Step 1: Select and rename columns
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NOMEAREAGERAL,
    QT_CONC_FEM        = dat_raw$C8211 + dat_raw$C8213 + dat_raw$C8221 + dat_raw$C8223,
    QT_CONC_MASC       = dat_raw$C8212 + dat_raw$C8214 + dat_raw$C8222 + dat_raw$C8224
  )
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save
  write.csv(dat_agg, output_path, row.names = FALSE)
  message("Done! Saved: ", output_path)
  
  return(dat_agg)  
}


# ---- Run for presencial ----
clean_2002 <- clean_graduates_2002(
  dat_raw     = dat_2002_to_merge,
  year        = 2002,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_clean.csv"
)


clean_2002 <- clean_2002[clean_2002$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]





 
#### online learning 2002

# C2222 - mas
# C2221 - fem
# C2211 - fem
# C2212 - masc



dat_2002_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2002_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# ---- Run for distancia ----
clean_2002_dis <- clean_graduates_online(
  dat_raw     = dat_2002_to_merge_dis,
  year        = 2002,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2002_clean_dis.csv"
)


clean_2002_merged <- rbind(clean_2002, clean_2002_dis)

clean_2002_merged <- clean_2002_merged[clean_2002_merged$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]

write.csv(
  clean_2002_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2002_clean.csv",
  row.names = FALSE
)



####### 2003 ######
### join!####


dat_2003_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2003_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)




# ---- Run for presencial ----
clean_2003 <- clean_graduates_2002(
  dat_raw     = dat_2003_to_merge,
  year        = 2003,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2003_clean.csv"
)


clean_2003 <- clean_2003[clean_2003$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]



dat_2003_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2003_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# ---- Run for distancia ----
clean_2003_dis <- clean_graduates_online(
  dat_raw     = dat_2003_to_merge_dis,
  year        = 2003,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2003_clean_dis.csv"
)


clean_2003_merged <- rbind(clean_2003, clean_2003_dis)


write.csv(
  clean_2003_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2003_clean.csv",
  row.names = FALSE
)




####### 2004 ######
### join!####

dat_2004_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2004_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for presencial ----
clean_2004 <- clean_graduates_2002(
  dat_raw     = dat_2004_to_merge,
  year        = 2004,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2004_clean.csv"
)


clean_2004 <- clean_2004[clean_2004$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]



dat_2004_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2004_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for distancia ----
clean_2004_dis <- clean_graduates_online(
  dat_raw     = dat_2004_to_merge_dis,
  year        = 2004,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2004_clean_dis.csv"
)


clean_2004_merged <- rbind(clean_2004, clean_2004_dis)


write.csv(
  clean_2004_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2004_clean.csv",
  row.names = FALSE
)

####### 2005 ######
### join!####


dat_2005_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2005_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# ---- Run for presencial ----
clean_2005 <- clean_graduates_2002(
  dat_raw     = dat_2005_to_merge,
  year        = 2005,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2005_clean.csv"
)


clean_2005 <- clean_2005[clean_2005$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]



dat_2005_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2005_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for distancia ----
clean_2005_dis <- clean_graduates_online(
  dat_raw     = dat_2005_to_merge_dis,
  year        = 2005,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2005_clean_dis.csv"
)


clean_2005_merged <- rbind(clean_2005, clean_2005_dis)


write.csv(
  clean_2005_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2005_clean.csv",
  row.names = FALSE
)

####### 2006 ######
### join!####

dat_2006_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2006_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for presencial ----
clean_2006 <- clean_graduates_2002(
  dat_raw     = dat_2006_to_merge,
  year        = 2006,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2006_clean.csv"
)


clean_2006 <- clean_2006[clean_2006$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]



dat_2006_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2006_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for distancia ----
clean_2006_dis <- clean_graduates_online(
  dat_raw     = dat_2006_to_merge_dis,
  year        = 2006,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2006_clean_dis.csv"
)


clean_2006_merged <- rbind(clean_2006, clean_2006_dis)


write.csv(
  clean_2006_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2006_clean.csv",
  row.names = FALSE
)


####### 2007 ######
### join!####

dat_2007_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2007_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

clean_graduates_2007 <- function(dat_raw, year, output_path) {
  # turns fem and masc NAs into zero
  dat_raw$C8111[is.na(dat_raw$C8111)]   <- 0
  dat_raw$C8112[is.na(dat_raw$C8112)]   <- 0
  dat_raw$C8113[is.na(dat_raw$C8113)]   <- 0
  dat_raw$C8114[is.na(dat_raw$C8114)]   <- 0
  dat_raw$C8121[is.na(dat_raw$C8121)]   <- 0
  dat_raw$C8122[is.na(dat_raw$C8122)]   <- 0
  dat_raw$C8123[is.na(dat_raw$C8123)]   <- 0
  dat_raw$C8124[is.na(dat_raw$C8124)]   <- 0
  
  # Step 1: Select and rename columns
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NOMEAREAGERAL,
    QT_CONC_FEM        = dat_raw$C8111 + dat_raw$C8113 + dat_raw$C8121 + dat_raw$C8123,
    QT_CONC_MASC       = dat_raw$C8112 + dat_raw$C8114 + dat_raw$C8122 + dat_raw$C8124
  )
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save
  write.csv(dat_agg, output_path, row.names = FALSE)
  message("Done! Saved: ", output_path)
  
  return(dat_agg)  
}

# ---- Run for presencial ----
clean_2007 <- clean_graduates_2007(
  dat_raw     = dat_2007_to_merge,
  year        = 2007,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2007_clean.csv"
)


clean_2007 <- clean_2007[clean_2007$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]






dat_2007_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2007_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# function for online

clean_graduates_online_07 <- function(dat_raw, year, output_path) {
  # Step 2: Replace NAs with 0
  dat_raw$C165011[is.na(dat_raw$C165011)]   <- 0
  dat_raw$C165012[is.na(dat_raw$C165012)] <- 0
  dat_raw$C165021[is.na(dat_raw$C165021)]   <- 0
  dat_raw$C165022[is.na(dat_raw$C165022)] <- 0
  
  # Step 1: Select and rename columns
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NOMEAREAGERAL,
    QT_CONC_FEM        = dat_raw$C165011 + dat_raw$C165021,
    QT_CONC_MASC       = dat_raw$C165012 + dat_raw$C165022
  )
  
  
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save
  write.csv(dat_agg, output_path, row.names = FALSE)
  message("Done! Saved: ", output_path)
  
  return(dat_agg)  
}





# ---- Run for distancia ----
clean_2007_dis <- clean_graduates_online_07(
  dat_raw     = dat_2007_to_merge_dis,
  year        = 2007,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2007_clean_dis.csv"
)


clean_2007_merged <- rbind(clean_2007, clean_2007_dis)


write.csv(
  clean_2007_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2007_clean.csv",
  row.names = FALSE
)


####### 2008 ######
### join!####

dat_2008_to_merge <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2008_merge_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


# ---- Run for presencial ----
clean_2008 <- clean_graduates_2007(
  dat_raw     = dat_2008_to_merge,
  year        = 2008,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2008_clean.csv"
)


clean_2008 <- clean_2008[clean_2008$NO_CINE_AREA_GERAL != "Programas ou Cursos Gerais", ]



dat_2008_to_merge_dis <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/2008_merge_GRADUACAO_DISTANCIA.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)


clean_2008_dis <- clean_graduates_online_07(
  dat_raw     = dat_2008_to_merge_dis,
  year        = 2008,
  output_path = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2008_clean_dis.csv"
)


clean_2008_merged <- rbind(clean_2008, clean_2008_dis)


write.csv(
  clean_2008_merged,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2008_clean.csv",
  row.names = FALSE
)

#### merge duplicate rows by YEAR and AREA

# Lista de todos os anos
years <- 2000:2008

for (yr in years) {
  
  # Ler o CSV
  file_path <- paste0("/Users/lucianadiasdemacedo/Downloads/S022/Final_project/", yr, "_clean.csv")
  dat <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Agregar duplicatas somando por área e ano
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL + NU_ANO_CENSO,
    data = dat,
    FUN  = sum
  )
  
  # Salvar por cima do CSV original
  write.csv(dat_agg, file_path, row.names = FALSE)
  
  message("Done! ", yr)
}

# Verificar um para confirmar
head(read.csv("/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2008_clean.csv"))


# Ler e juntar todos os anos
years <- 2000:2008

dat_2000_2008 <- do.call(rbind, lapply(years, function(yr) {
  file_path <- paste0("/Users/lucianadiasdemacedo/Downloads/S022/Final_project/", yr, "_clean.csv")
  read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
}))

# Verificar
dim(dat_2000_2008)
unique(dat_2000_2008$NU_ANO_CENSO)  # deve mostrar 2000 a 2008

# Salvar
write.csv(
  dat_2000_2008,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/2000_2008_clean.csv",
  row.names = FALSE
)
