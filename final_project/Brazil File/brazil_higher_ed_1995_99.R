#Brazilian graduation data from 1995-1999


####### 1995 ######
dat_1995 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/1995_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|"
)

area_map_1995 <- c(
  "CIENCIAS AGRARIAS"           = "Agricultura, silvicultura, pesca e veterinária",
  "CIENCIAS BIOLOGICAS"         = "Ciências naturais, matemática e estatística",
  "CIENCIAS DA SAUDE"           = "Saúde e bem-estar",
  "CIENCIAS EXATAS E DA TERRA"  = "Ciências naturais, matemática e estatística",
  "CIENCIAS HUMANAS"            = "Artes e humanidades",
  "CIENCIAS SOCIAIS APLICADAS"  = "Negócios, administração e direito",
  "ENGENHARIA / TECNOLOGIA"     = "Engenharia, produção e construção",
  "LINGUISTICA, LETRAS E ARTES" = "Artes e humanidades"
  # CICLO BASICO COMUM -> fica como NA e é dropado
)

# Aplicar mapeamento

dat_1995$NO_CINE_AREA <- area_map_1995[dat_1995$NO_AREA_CONHE]

# Dropar CICLO BASICO COMUM (ficou NA)
clean_1995 <- dat_1995[!is.na(dat_1995$NO_CINE_AREA), ]

# Confirmar
cat("NAs restantes em clean_1995:", sum(is.na(clean_1995$NO_CINE_AREA)), "\n")
unique(clean_1995$NO_CINE_AREA)

clean_1995$QT_DIPLO_1SEM_FEMI[is.na(clean_1995$QT_DIPLO_1SEM_FEMI)]   <- 0
clean_1995$QT_DIPLO_2SEM_FEMI[is.na(clean_1995$QT_DIPLO_2SEM_FEMI)] <- 0
clean_1995$QT_DIPLO_1SEM_MASC[is.na(clean_1995$QT_DIPLO_1SEM_MASC)]   <- 0
clean_1995$QT_DIPLO_2SEM_MASC[is.na(clean_1995$QT_DIPLO_2SEM_MASC)] <- 0

# Step 2: Keep only the desired columns, merge semesters, and add year
dat_1995_clean <- data.frame(
  NU_ANO_CENSO  = 1995,
  NO_CINE_AREA_GERAL = clean_1995$NO_CINE_AREA,
  QT_CONC_FEM = clean_1995$QT_DIPLO_1SEM_FEMI + clean_1995$QT_DIPLO_2SEM_FEMI,
  QT_CONC_MASC = clean_1995$QT_DIPLO_1SEM_MASC + clean_1995$QT_DIPLO_2SEM_MASC
)

dat_1995_clean$QT_CONC <- dat_1995_clean$QT_CONC_FEM + dat_1995_clean$QT_CONC_MASC

dat_1995_agg <- aggregate(
  cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
  data = dat_1995_clean,
  FUN  = sum
)


# Step 4: Save as comma-separated CSV
write.csv(
  dat_1995_agg,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/1995_clean.csv",
  row.names = FALSE
)


clean_1995 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/1995_clean.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

####### function for 1995 and 1996 ######


clean_graduates <- function(dat_clean, year, output_dir) {
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back after aggregation
  dat_agg$NU_ANO_CENSO <- year

  # Step 6: Save clean CSV
  output_path <- paste0(output_dir, "/", year, "_clean.csv")
  write.csv(dat_agg, output_path, row.names = FALSE)
  
  message("Done! Saved: ", output_path)
  
  return(dat_agg)
}




########


dat_1996 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/1996_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|"
)


area_map_1996 <- c(
  "CIENCIAS AGRARIAS"           = "Agricultura, silvicultura, pesca e veterinária",
  "CIENCIAS BIOLOGICAS"         = "Ciências naturais, matemática e estatística",
  "CIENCIAS DA SAUDE"           = "Saúde e bem-estar",
  "CIENCIAS EXATAS E DA TERRA"  = "Ciências naturais, matemática e estatística",
  "CIENCIAS HUMANAS"            = "Artes e humanidades",
  "CIENCIAS SOCIAIS APLICADAS"  = "Negócios, administração e direito",
  "ENGENHARIA / TECNOLOGIA"     = "Engenharia, produção e construção",
  "LINGUISTICA, LETRAS E ARTES" = "Artes e humanidades"
  # CICLO BASICO COMUM -> fica como NA e é dropado
)

# Aplicar mapeamento

dat_1996$NO_CINE_AREA <- area_map_1996[dat_1996$NO_AREA_CONHE]

# Dropar CICLO BASICO COMUM (ficou NA)
clean_1996 <- dat_1996[!is.na(dat_1996$NO_CINE_AREA), ]

# Confirmar
cat("NAs restantes em clean_1995:", sum(is.na(clean_1996$NO_CINE_AREA)), "\n")
unique(clean_1996$NO_CINE_AREA)

clean_1996$QT_DIPLO_1SEM_FEMI[is.na(clean_1996$QT_DIPLO_1SEM_FEMI)]   <- 0
clean_1996$QT_DIPLO_2SEM_FEMI[is.na(clean_1996$QT_DIPLO_2SEM_FEMI)] <- 0
clean_1996$QT_DIPLO_1SEM_MASC[is.na(clean_1996$QT_DIPLO_1SEM_MASC)]   <- 0
clean_1996$QT_DIPLO_2SEM_MASC[is.na(clean_1996$QT_DIPLO_2SEM_MASC)] <- 0

# Step 2: Keep only the desired columns, merge semesters, and add year
dat_1996_clean <- data.frame(
  NU_ANO_CENSO  = 1996,
  NO_CINE_AREA_GERAL = clean_1996$NO_CINE_AREA,
  QT_CONC_FEM = clean_1996$QT_DIPLO_1SEM_FEMI + clean_1996$QT_DIPLO_2SEM_FEMI,
  QT_CONC_MASC = clean_1996$QT_DIPLO_1SEM_MASC + clean_1996$QT_DIPLO_2SEM_MASC
)


# Run function 1996
clean_1996 <- clean_graduates(
  dat_clean  = dat_1996_clean,
  year       = 1996,
  output_dir = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project"
)


######### one more separate function, because every year the variables are different!!!!

###### function for 1997



clean_graduates_1997 <- function(dat_clean, year, output_dir) {
  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back after aggregation
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save clean CSV
  output_path <- paste0(output_dir, "/", year, "_clean.csv")
  write.csv(dat_agg, output_path, row.names = FALSE)
  
  message("Done! Saved: ", output_path)
  
  return(dat_agg)
}


####### 1997 ######

dat_1997 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/1997_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# Aplicar mapeamento de áreas
dat_1997$NO_CINE_AREA_GERAL <- area_map_1996[dat_1997$NO_AREA_CONHECIMENTO]

# Dropar CICLO BASICO COMUM (ficou NA)
clean_1997 <- dat_1997[!is.na(dat_1997$NO_CINE_AREA_GERAL), ]

clean_1997$QT_FEM_DIP[is.na(clean_1997$QT_FEM_DIP)]   <- 0
clean_1997$QT_MASC_DIP[is.na(clean_1997$QT_MASC_DIP)] <- 0



# Criar colunas QT_CONC_FEM e QT_CONC_MASC
clean_1997$QT_CONC_FEM  <- clean_1997$QT_FEM_DIP
clean_1997$QT_CONC_MASC <- clean_1997$QT_MASC_DIP

# Rodar função
clean_1997 <- clean_graduates_1997(
  dat_clean  = clean_1997,
  year       = 1997,
  output_dir = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project"
)


###### function for 1998 and 1999 #####

clean_graduates_199X <- function(dat_raw, year, output_dir) {
  
  dat_raw$C0811[is.na(dat_raw$C0811)]   <- 0
  dat_raw$C0813[is.na(dat_raw$C0813)] <- 0
  dat_raw$C0812[is.na(dat_raw$C0812)]   <- 0
  dat_raw$C0814[is.na(dat_raw$C0814)] <- 0
  
  # Step 1: Keep only the desired columns, merge semesters, and add year
  dat_clean <- data.frame(
    NU_ANO_CENSO       = year,
    NO_CINE_AREA_GERAL = dat_raw$NO_AREA,
    QT_CONC_FEM        = dat_raw$C0811 + dat_raw$C0813 ,
    QT_CONC_MASC       = dat_raw$C0812 + dat_raw$C0814
  )

  
  # Step 3: Sum all graduates
  dat_clean$QT_CONC <- dat_clean$QT_CONC_FEM + dat_clean$QT_CONC_MASC
  
  # Step 4: Aggregate by area
  dat_agg <- aggregate(
    cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ NO_CINE_AREA_GERAL,
    data = dat_clean,
    FUN  = sum
  )
  
  # Step 5: Add year back after aggregation
  dat_agg$NU_ANO_CENSO <- year
  
  # Step 6: Save clean CSV
  output_path <- paste0(output_dir, "/", year, "_clean.csv")
  write.csv(dat_agg, output_path, row.names = FALSE)
  
  message("Done! Saved: ", output_path)
  
  return(dat_agg)
}

####### 1998 ######

dat_1998 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/1998_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# Run function 1998
clean_1998 <- clean_graduates_199X(
  dat_raw    = dat_1998,
  year       = 1998,
  output_dir = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project"
)

####### 1999 ######

dat_1999 <- read.csv(
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/microdados_1995_2013/1999_GRADUACAO_PRESENCIAL.CSV",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = "|",
  fileEncoding = "latin1"
)

# Run function 1998
clean_1999 <- clean_graduates_199X(
  dat_raw    = dat_1999,
  year       = 1999,
  output_dir = "/Users/lucianadiasdemacedo/Downloads/S022/Final_project"
)


####### 1998 and 1999 need to have their general areas merged 

clean_1998$NO_CINE_AREA_GERAL

# Tabela de mapeamento: área antiga -> área nova
area_map <- c(
  
  # Negócios, administração e direito
  "Administração"                             = "Negócios, administração e direito",
  "Administração de cooperativas"             = "Negócios, administração e direito",
  "Administração de recursos humanos"         = "Negócios, administração e direito",
  "Administração rural"                       = "Negócios, administração e direito",
  "Ciências contábeis"                        = "Negócios, administração e direito",
  "Ciências gerenciais"                       = "Negócios, administração e direito",
  "Direito"                                   = "Negócios, administração e direito",
  "Economia"                                  = "Negócios, administração e direito",
  "Formação de executivos"                    = "Negócios, administração e direito",
  "Gestão de negócios"                        = "Negócios, administração e direito",
  "Marketing e propaganda"                    = "Negócios, administração e direito",
  "Mercadologia (marketing)"                  = "Negócios, administração e direito",
  "Negócios imobiliários"                     = "Negócios, administração e direito",
  "Planejamento administrativo"               = "Negócios, administração e direito",
  "Qualidade total"                           = "Negócios, administração e direito",
  "Secretariado"                              = "Negócios, administração e direito",
  "Secretariado executivo"                    = "Negócios, administração e direito",
  "Vendas em varejo"                          = "Negócios, administração e direito",
  
  # Educação
  "Ciência da educação"                       = "Educação",
  "Educação artística"                        = "Educação",
  "Educação física"                           = "Educação",
  "Formação de professor de disciplinas profissionalizantes do ensino médio" = "Educação",
  "Pedagogia"                                 = "Educação",
  
  # Engenharia, produção e construção
  "Agrimensura"                               = "Engenharia, produção e construção",
  "Arquitetura e urbanismo"                   = "Engenharia, produção e construção",
  "Automação industrial"                      = "Engenharia, produção e construção",
  "Ciência aeronáutica"                       = "Engenharia, produção e construção",
  "Construção civil"                          = "Engenharia, produção e construção",
  "Controle e automação"                      = "Engenharia, produção e construção",
  "Eletricidade"                              = "Engenharia, produção e construção",
  "Eletrônica"                                = "Engenharia, produção e construção",
  "Eletrônica industrial"                     = "Engenharia, produção e construção",
  "Engenharia"                                = "Engenharia, produção e construção",
  "Engenharia agrícola"                       = "Engenharia, produção e construção",
  "Engenharia cartográfica"                   = "Engenharia, produção e construção",
  "Engenharia de alimentos"                   = "Engenharia, produção e construção",
  "Engenharia de pesca"                       = "Engenharia, produção e construção",
  "Gestão da produção"                        = "Engenharia, produção e construção",
  "Indústria têxtil"                          = "Engenharia, produção e construção",
  "Indústrias de laticínios (industriais)"    = "Engenharia, produção e construção",
  "Manutenção de equipamentos eletrônicos"    = "Engenharia, produção e construção",
  "Manutenção mecânica"                       = "Engenharia, produção e construção",
  "Mecânica"                                  = "Engenharia, produção e construção",
  "Montagem, torneamento e usinagem de metais"= "Engenharia, produção e construção",
  "Navegação fluvial"                         = "Engenharia, produção e construção",
  "Produção industrial"                       = "Engenharia, produção e construção",
  "Química industrial"                        = "Engenharia, produção e construção",
  "Saneamento básico"                         = "Engenharia, produção e construção",
  "Tecnologia ambiental"                      = "Engenharia, produção e construção",
  "Tecnologia de alimentos"                   = "Engenharia, produção e construção",
  "Tecnologia de madeira"                     = "Engenharia, produção e construção",
  "Tecnologia em eletrotécnica"               = "Engenharia, produção e construção",
  "Tecnologia química"                        = "Engenharia, produção e construção",
  "Topografia"                                = "Engenharia, produção e construção",
  
  # Serviços
  "Economia doméstica"                        = "Serviços",
  "Hotelaria"                                 = "Serviços",
  "Recreação e lazer"                         = "Serviços",
  "Turismo"                                   = "Serviços",
  "Turismo e hotelaria"                       = "Serviços",
  
  # Ciências sociais, comunicação e informação
  "Arqueologia"                               = "Ciências sociais, comunicação e informação",
  "Arquivologia"                              = "Ciências sociais, comunicação e informação",
  "Biblioteconomia"                           = "Ciências sociais, comunicação e informação",
  "Ciência política"                          = "Ciências sociais, comunicação e informação",
  "Ciências sociais"                          = "Ciências sociais, comunicação e informação",
  "Comunicação social (redação e conteúdo)"   = "Ciências sociais, comunicação e informação",
  "Estudos sociais"                           = "Ciências sociais, comunicação e informação",
  "Jornalismo"                                = "Ciências sociais, comunicação e informação",
  "Publicidade e propaganda"                  = "Ciências sociais, comunicação e informação",
  "Relações internacionais"                   = "Ciências sociais, comunicação e informação",
  
  # Computação e TIC
  "Análise de sistemas"                       = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Ciência da computação"                     = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Informática (ciência da computação)"       = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Matemática computacional (informática)"    = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Processamento de dados"                    = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Redes de computadores"                     = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Sistemas de comunicação sem fio"           = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Tecnologia da informação"                  = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Tecnologia digital"                        = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  "Telecomunicações"                          = "Computação e Tecnologias da Informação e Comunicação (TIC)",
  
  # Saúde e bem-estar
  "Enfermagem e obstetrícia"                  = "Saúde e bem-estar",
  "Farmácia"                                  = "Saúde e bem-estar",
  "Fisioterapia"                              = "Saúde e bem-estar",
  "Fonoaudiologia"                            = "Saúde e bem-estar",
  "Medicina"                                  = "Saúde e bem-estar",
  "Musicoterapia"                             = "Saúde e bem-estar",
  "Naturologia"                               = "Saúde e bem-estar",
  "Nutrição"                                  = "Saúde e bem-estar",
  "Odontologia"                               = "Saúde e bem-estar",
  "Psicologia"                                = "Saúde e bem-estar",
  "Psicomotricidade"                          = "Saúde e bem-estar",
  "Radiologia"                                = "Saúde e bem-estar",
  "Terapia ocupacional"                       = "Saúde e bem-estar",
  
  # Agricultura, silvicultura, pesca e veterinária
  "Agronomia"                                 = "Agricultura, silvicultura, pesca e veterinária",
  "Ciências agrárias"                         = "Agricultura, silvicultura, pesca e veterinária",
  "Engenharia florestal"                      = "Agricultura, silvicultura, pesca e veterinária",
  "Fruticultura"                              = "Agricultura, silvicultura, pesca e veterinária",
  "Horticultura"                              = "Agricultura, silvicultura, pesca e veterinária",
  "Medicina veterinária"                      = "Agricultura, silvicultura, pesca e veterinária",
  "Tecnologia agronômica"                     = "Agricultura, silvicultura, pesca e veterinária",
  "Viticultura"                               = "Agricultura, silvicultura, pesca e veterinária",
  "Zootecnia"                                 = "Agricultura, silvicultura, pesca e veterinária",
  
  # Ciências naturais, matemática e estatística
  "Astronomia"                                = "Ciências naturais, matemática e estatística",
  "Ciência atuarial"                          = "Ciências naturais, matemática e estatística",
  "Ciências biológicas"                       = "Ciências naturais, matemática e estatística",
  "Estatística"                               = "Ciências naturais, matemática e estatística",
  "Física"                                    = "Ciências naturais, matemática e estatística",
  "Geofísica"                                 = "Ciências naturais, matemática e estatística",
  "Geografia"                                 = "Ciências naturais, matemática e estatística",
  "Geologia"                                  = "Ciências naturais, matemática e estatística",
  "Matemática"                                = "Ciências naturais, matemática e estatística",
  "Meteorologia"                              = "Ciências naturais, matemática e estatística",
  "Oceanologia"                               = "Ciências naturais, matemática e estatística",
  "Química"                                   = "Ciências naturais, matemática e estatística",
  "Química de alimentos"                      = "Ciências naturais, matemática e estatística",
  "Química de polímeros"                      = "Ciências naturais, matemática e estatística",
  
  # Artes e humanidades
  "Artes cênicas"                             = "Artes e humanidades",
  "Artes e educação"                          = "Artes e humanidades",
  "Artes plásticas"                           = "Artes e humanidades",
  "Artes visuais"                             = "Artes e humanidades",
  "Belas artes"                               = "Artes e humanidades",
  "Cerâmica (artesanal)"                      = "Artes e humanidades",
  "Dança (arte)"                              = "Artes e humanidades",
  "Decoração de interiores"                   = "Artes e humanidades",
  "Desenho e plástica"                        = "Artes e humanidades",
  "Desenho industrial (artístico)"            = "Artes e humanidades",
  "Design"                                    = "Artes e humanidades",
  "Estudos religiosos"                        = "Artes e humanidades",
  "Filosofia"                                 = "Artes e humanidades",
  "Fotografia"                                = "Artes e humanidades",
  "História"                                  = "Artes e humanidades",
  "Interpretação teatral"                     = "Artes e humanidades",
  "Letras"                                    = "Artes e humanidades",
  "Lingüística (línguas)"                     = "Artes e humanidades",
  "Moda"                                      = "Artes e humanidades",
  "Museologia"                                = "Artes e humanidades",
  "Música"                                    = "Artes e humanidades",
  "Paisagismo"                                = "Artes e humanidades",
  "Produção cultural"                         = "Artes e humanidades",
  "Teologia"                                  = "Artes e humanidades",
  "Tradutor e intérprete"                     = "Artes e humanidades"
)



# Corrigir itens errados no area_map
area_map["Serviço social"]      <- "Artes e humanidades"
area_map["Engenharia florestal"] <- "Engenharia, produção e construção"
# Adicionar novos mapeamentos
area_map["Administração em comércio exterior"]                           <- "Negócios, administração e direito"
area_map["Aqüicultura"]                                                  <- "Agricultura, silvicultura, pesca e veterinária"
area_map["Artes gráficas"]                                               <- "Artes e humanidades"
area_map["Biologia molecular"]                                           <- "Ciências naturais, matemática e estatística"
area_map["Cinematografia"]                                               <- "Artes e humanidades"
area_map["Comunicação visual"]                                           <- "Artes e humanidades"
area_map["Empreendedorismo"]                                             <- "Negócios, administração e direito"
area_map["Formação de professor de disciplinas do setor de serviços"]   <- "Educação"
area_map["Formação de professor do ensino fundamental"]                  <- "Educação"
area_map["Sistemas de informação"]                                       <- "Computação e Tecnologias da Informação e Comunicação (TIC)"

# As que serão dropadas ficam como NA (sem mapeamento)
# Agroindústria, Ciências, Formação militar, Gestão da informação,
# Gestão do lazer, Irrigação e drenagem, Manutenção de aparelhos médico-hospitalares,
# Normal superior, Processos industriais, Segurança pública,
# Tecnologia mecatrônica, Telemática

# Reaplicar o mapeamento atualizado
clean_1998$AREA_GERAL <- area_map[clean_1998$NO_CINE_AREA_GERAL]
clean_1999$AREA_GERAL <- area_map[clean_1999$NO_CINE_AREA_GERAL]

# Dropar todas as linhas com NA
clean_1998 <- clean_1998[!is.na(clean_1998$AREA_GERAL), ]
clean_1999 <- clean_1999[!is.na(clean_1999$AREA_GERAL), ]

# Confirmar que não sobrou nenhum NA
cat("NAs restantes em clean_1998:", sum(is.na(clean_1998$AREA_GERAL)), "\n")
cat("NAs restantes em clean_1999:", sum(is.na(clean_1999$AREA_GERAL)), "\n")

# Ver áreas únicas para confirmar
unique(clean_1998$AREA_GERAL)
unique(clean_1999$AREA_GERAL)


clean_1999 <- aggregate(
  cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ AREA_GERAL,
  data = clean_1999,
  FUN  = sum
)

clean_1998 <- aggregate(
  cbind(QT_CONC_FEM, QT_CONC_MASC, QT_CONC) ~ AREA_GERAL,
  data = clean_1998,
  FUN  = sum
)

names(clean_1998)[names(clean_1998) == "AREA_GERAL"] <- "NO_CINE_AREA_GERAL"
names(clean_1999)[names(clean_1999) == "AREA_GERAL"] <- "NO_CINE_AREA_GERAL"
##########
# merge data all data sets from 1995-1999

# Verificar colunas de cada dataset
colnames(clean_1995)
colnames(clean_1996)
colnames(clean_1997)
colnames(clean_1998)
colnames(clean_1999)

# Adicione apenas nos datasets que estão faltando, com o ano correto
clean_1995$NU_ANO_CENSO <- 1995
clean_1998$NU_ANO_CENSO <- 1998
clean_1999$NU_ANO_CENSO <- 1999

dat_all <- rbind(clean_1995, clean_1996, clean_1997, clean_1998, clean_1999)

write.csv(
  dat_all,
  "/Users/lucianadiasdemacedo/Downloads/S022/Final_project/1995_1999_clean.csv",
  row.names = FALSE
)
