# ================================================================
# INEP Censo da Educação Superior
# Graduation by Gender and Field — Complete Pipeline
# Grad years: 1995–2024 (all 30 years present; 1996 sparse)
# ================================================================

library(tidyverse)

DATA_DIR <- "~/Documents/HKS/Spring 2026/EDU S022 Stats & Data Science/Final project/MICRODADOS/all_years"

# ================================================================
# SECTION 1: HELPERS
# ================================================================

to_int <- function(x) suppressWarnings(as.integer(str_trim(x)))

find_col <- function(df, candidates) {
  found <- intersect(candidates, names(df))
  if (length(found) == 0) return(NULL)
  found[1]
}

sum_cols_safe <- function(df, col_names) {
  existing <- intersect(col_names, names(df))
  if (length(existing) == 0) return(rep(0L, nrow(df)))
  rowSums(
    sapply(df[existing], function(x) coalesce(to_int(x), 0L)),
    na.rm = TRUE
  )
}

read_pre2009 <- function(path) {
  read_delim(path, delim = "|",
             locale    = locale(encoding = "latin1"),
             col_types = cols(.default = col_character()),
             trim_ws   = TRUE, name_repair = "minimal")
}

read_post2009 <- function(path) {
  read_delim(path, delim = ";",
             locale    = locale(encoding = "latin1"),
             col_types = cols(.default = col_character()),
             trim_ws   = TRUE, name_repair = "minimal")
}

# ================================================================
# SECTION 2: FILE DISCOVERY
# ================================================================

all_files <- tibble(
  path = list.files(DATA_DIR, pattern = "\\.CSV$",
                    full.names = TRUE, ignore.case = TRUE)
) %>%
  mutate(
    filename = basename(path),
    year     = as.integer(str_extract(filename, "\\d{4}")),
    era      = case_when(
      str_detect(filename, "GRADUACAO_PRESENCIAL") ~ "pre2009",
      str_detect(filename, "CADASTRO_CURSOS")      ~ "post2009",
      TRUE                                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(era)) %>%
  arrange(year)

# ================================================================
# SECTION 3: LOOKUP TABLES
# ================================================================

# ---- 3a: Old MEC area lookup ----
raw_2004 <- read_pre2009(all_files %>% filter(year == 2004) %>% pull(path))

area_lookup_old <- raw_2004 %>%
  select(
    area_code_3dig  = CODAREADETALHADA,
    area_code_2dig  = CODAREAESPECIFICA,
    area_code_1dig  = CODAREAGERAL,
    area_name_3dig  = NOMEAREADETALHADA,
    area_name_2dig  = NOMEAREAESPECIFIC,
    area_name_broad = NOMEAREAGERAL
  ) %>%
  filter(!is.na(area_code_3dig), str_trim(area_code_3dig) != "") %>%
  mutate(across(everything(), str_squish)) %>%
  distinct(area_code_3dig, .keep_all = TRUE) %>%
  # Codes absent from 2004 + fix for "010"
  bind_rows(tribble(
    ~area_code_3dig, ~area_code_1dig, ~area_code_2dig,
    ~area_name_broad,                          ~area_name_2dig,
    ~area_name_3dig,
    "215", "2", "21",
    "Humanidades E Artes",                      "Artes",
    "Artesanato",
    "620", "6", "62",
    "Agricultura E Veterinária",                "Agricultura, Silvicultura E Pesca",
    "Agricultura, Silvicultura E Recursos Pesqueiros",
    "761", "7", "76",
    "Saúde E Bem-Estar Social",                 "Serviços Sociais",
    "Atendimento À Criança E Serviços Para Os Jovens",
    "853", "8", "85",
    "Serviços",                                 "Serviços Pessoais",
    "Serviços Comunitários De Saneamento",
    "010", "1", "01",
    "Educação",                                 "Educação",
    "Educação Geral"
  )) %>%
  distinct(area_code_3dig, .keep_all = TRUE) %>%
  mutate(
    cine_area_code = case_when(
      area_code_3dig == "010"                  ~ "01",
      area_code_1dig == "1"                    ~ "01",
      area_code_1dig == "2"                    ~ "02",
      area_code_2dig %in% c("31", "32")        ~ "03",
      area_code_2dig %in% c("34", "38")        ~ "04",
      area_code_2dig %in% c("42", "44", "46")  ~ "05",
      area_code_2dig == "48"                   ~ "06",
      area_code_1dig == "5"                    ~ "07",
      area_code_1dig == "6"                    ~ "08",
      area_code_1dig == "7"                    ~ "09",
      area_code_1dig == "8"                    ~ "10",
      TRUE                                     ~ NA_character_
    ),
    cine_area_name = case_when(
      cine_area_code == "01" ~ "Educação",
      cine_area_code == "02" ~ "Artes E Humanidades",
      cine_area_code == "03" ~ "Ciências Sociais, Comunicação E Informação",
      cine_area_code == "04" ~ "Negócios, Administração E Direito",
      cine_area_code == "05" ~ "Ciências Naturais, Matemática E Estatística",
      cine_area_code == "06" ~ "Computação E Tecnologias Da Informação E Comunicação (TIC)",
      cine_area_code == "07" ~ "Engenharia, Produção E Construção",
      cine_area_code == "08" ~ "Agricultura, Silvicultura, Pesca E Veterinária",
      cine_area_code == "09" ~ "Saúde E Bem-Estar",
      cine_area_code == "10" ~ "Serviços",
      TRUE                   ~ NA_character_
    )
  ) %>%
  arrange(area_code_1dig, area_code_2dig, area_code_3dig)

# ---- 3b: NO_AREA free-text lookup (grad years 1997–1998) ----
no_area_lookup <- tribble(
  ~NO_AREA,                                                                    ~cine_area_code,
  # 01 Educação
  "Pedagogia",                                                                 "01",
  "Educação física",                                                           "01",
  "Educação artística",                                                        "01",
  "Ciência da educação",                                                       "01",
  "Normal superior",                                                           "01",
  "Artes e educação",                                                          "01",
  "Estudos sociais",                                                           "01",
  "Formação de professor de disciplinas profissionalizantes do ensino médio",  "01",
  "Formação de professor de disciplinas do setor de serviços",                 "01",
  "Formação de professor do ensino fundamental",                               "01",
  "Recreação e lazer",                                                         "01",
  "Gestão do lazer",                                                           "01",
  "Psicomotricidade",                                                          "01",
  # 02 Artes e Humanidades
  "Letras",                                                                    "02",
  "Filosofia",                                                                 "02",
  "História",                                                                  "02",
  "Música",                                                                    "02",
  "Artes cênicas",                                                             "02",
  "Artes plásticas",                                                           "02",
  "Artes visuais",                                                             "02",
  "Belas artes",                                                               "02",
  "Dança (arte)",                                                              "02",
  "Estudos religiosos",                                                        "02",
  "Teologia",                                                                  "02",
  "Lingüística (línguas)",                                                     "02",
  "Linguística (línguas)",                                                     "02",
  "Tradutor e intérprete",                                                     "02",
  "Desenho e plástica",                                                        "02",
  "Moda",                                                                      "02",
  "Artes gráficas",                                                            "02",
  "Produção cultural",                                                         "02",
  "Interpretação teatral",                                                     "02",
  "Fotografia",                                                                "02",
  "Cinematografia",                                                            "02",
  "Comunicação visual",                                                        "02",
  "Decoração de interiores",                                                   "02",
  "Desenho industrial (artístico)",                                            "02",
  "Design",                                                                    "02",
  # 03 Ciências Sociais, Comunicação e Informação
  "Ciências sociais",                                                          "03",
  "Comunicação social (redação e conteúdo)",                                   "03",
  "Jornalismo",                                                                "03",
  "Ciência política",                                                          "03",
  "Relações internacionais",                                                   "03",
  "Arqueologia",                                                               "03",
  "Publicidade e propaganda",                                                  "03",
  "Arquivologia",                                                              "03",
  "Museologia",                                                                "03",
  "Biblioteconomia",                                                           "03",
  "Marketing e propaganda",                                                    "03",
  "Geografia",                                                                 "03",
  "Gestão da informação",                                                      "03",
  # 04 Negócios, Administração e Direito
  "Administração",                                                             "04",
  "Direito",                                                                   "04",
  "Economia",                                                                  "04",
  "Ciências contábeis",                                                        "04",
  "Secretariado executivo",                                                    "04",
  "Secretariado",                                                              "04",
  "Turismo",                                                                   "04",
  "Hotelaria",                                                                 "04",
  "Turismo e hotelaria",                                                       "04",
  "Mercadologia (marketing)",                                                  "04",
  "Formação de executivos",                                                    "04",
  "Gestão de negócios",                                                        "04",
  "Administração rural",                                                       "04",
  "Administração de cooperativas",                                             "04",
  "Administração de recursos humanos",                                         "04",
  "Administração em comércio exterior",                                        "04",
  "Ciência atuarial",                                                          "04",
  "Negócios imobiliários",                                                     "04",
  "Qualidade total",                                                           "04",
  "Gestão da produção",                                                        "04",
  "Economia doméstica",                                                        "04",
  "Planejamento administrativo",                                               "04",
  "Ciências gerenciais",                                                       "04",
  "Vendas em varejo",                                                          "04",
  "Empreendedorismo",                                                          "04",
  # 05 Ciências Naturais, Matemática e Estatística
  "Matemática",                                                                "05",
  "Física",                                                                    "05",
  "Química",                                                                   "05",
  "Ciências biológicas",                                                       "05",
  "Ciências",                                                                  "05",
  "Estatística",                                                               "05",
  "Geologia",                                                                  "05",
  "Astronomia",                                                                "05",
  "Oceanologia",                                                               "05",
  "Meteorologia",                                                              "05",
  "Geofísica",                                                                 "05",
  "Biologia molecular",                                                        "05",
  "Química industrial",                                                        "05",
  "Química de alimentos",                                                      "05",
  "Química de polímeros",                                                      "05",
  "Tecnologia química",                                                        "05",
  # 06 Computação e TIC
  "Ciência da computação",                                                     "06",
  "Processamento de dados",                                                    "06",
  "Análise de sistemas",                                                       "06",
  "Informática (ciência da computação)",                                       "06",
  "Sistemas de informação",                                                    "06",
  "Redes de computadores",                                                     "06",
  "Telecomunicações",                                                          "06",
  "Tecnologia digital",                                                        "06",
  "Tecnologia da informação",                                                  "06",
  "Sistemas de comunicação sem fio",                                           "06",
  "Matemática computacional (informática)",                                    "06",
  "Automação industrial",                                                      "06",
  "Controle e automação",                                                      "06",
  "Telemática",                                                                "06",
  "Tecnologia mecatrônica",                                                    "06",
  "Eletrônica",                                                                "06",
  "Eletrônica industrial",                                                     "06",
  "Eletricidade",                                                              "06",
  "Manutenção de equipamentos eletrônicos",                                    "06",
  # 07 Engenharia, Produção e Construção
  "Engenharia",                                                                "07",
  "Arquitetura e urbanismo",                                                   "07",
  "Construção civil",                                                          "07",
  "Mecânica",                                                                  "07",
  "Engenharia cartográfica",                                                   "07",
  "Engenharia de alimentos",                                                   "07",
  "Manutenção mecânica",                                                       "07",
  "Manutenção de aparelhos médico-hospitalares",                               "07",
  "Montagem, torneamento e usinagem de metais",                                "07",
  "Tecnologia em eletrotécnica",                                               "07",
  "Produção industrial",                                                       "07",
  "Agrimensura",                                                               "07",
  "Topografia",                                                                "07",
  "Tecnologia ambiental",                                                      "07",
  "Indústria têxtil",                                                          "07",
  "Fabricação de móveis",                                                      "07",
  "Cerâmica (artesanal)",                                                      "07",
  "Irrigação e drenagem (construção)",                                         "07",
  "Processos industriais",                                                     "07",
  "Ciência aeronáutica",                                                       "07",
  "Tecnologia agronômica",                                                     "07",
  "Tecnologia de madeira",                                                     "07",
  # 08 Agricultura, Silvicultura, Pesca e Veterinária
  "Agronomia",                                                                 "08",
  "Medicina veterinária",                                                      "08",
  "Zootecnia",                                                                 "08",
  "Fruticultura",                                                              "08",
  "Horticultura",                                                              "08",
  "Viticultura",                                                               "08",
  "Engenharia florestal",                                                      "08",
  "Engenharia agrícola",                                                       "08",
  "Engenharia de pesca",                                                       "08",
  "Agroindústria",                                                             "08",
  "Aqüicultura",                                                               "08",
  "Paisagismo",                                                                "08",
  "Indústrias de laticínios (industriais)",                                    "08",
  "Tecnologia de alimentos",                                                   "08",
  "Ciências agrárias",                                                         "08",
  # 09 Saúde e Bem-Estar
  "Medicina",                                                                  "09",
  "Enfermagem e obstetrícia",                                                  "09",
  "Fisioterapia",                                                              "09",
  "Odontologia",                                                               "09",
  "Farmácia",                                                                  "09",
  "Fonoaudiologia",                                                            "09",
  "Nutrição",                                                                  "09",
  "Terapia ocupacional",                                                       "09",
  "Psicologia",                                                                "09",
  "Radiologia",                                                                "09",
  "Saúde e segurança no trabalho",                                             "09",
  "Musicoterapia",                                                             "09",
  "Tecnologia de aparelhos auditivos",                                         "09",
  "Tecnologia em prótese",                                                     "09",
  "Tecnologia oftálmica",                                                      "09",
  "Naturologia",                                                               "09",
  "Serviço social",                                                            "09",
  # 10 Serviços
  "Navegação fluvial",                                                         "10",
  "Segurança pública",                                                         "10",
  "Formação militar",                                                          "10",
  "Saneamento básico",                                                         "10",
  # 00 Programas Básicos
  "Básicos / Programas Gerais",                                                "00"
) %>%
  mutate(
    NO_AREA = str_squish(NO_AREA),
    cine_area_name = case_when(
      cine_area_code == "00" ~ "Programas Básicos",
      cine_area_code == "01" ~ "Educação",
      cine_area_code == "02" ~ "Artes E Humanidades",
      cine_area_code == "03" ~ "Ciências Sociais, Comunicação E Informação",
      cine_area_code == "04" ~ "Negócios, Administração E Direito",
      cine_area_code == "05" ~ "Ciências Naturais, Matemática E Estatística",
      cine_area_code == "06" ~ "Computação E Tecnologias Da Informação E Comunicação (TIC)",
      cine_area_code == "07" ~ "Engenharia, Produção E Construção",
      cine_area_code == "08" ~ "Agricultura, Silvicultura, Pesca E Veterinária",
      cine_area_code == "09" ~ "Saúde E Bem-Estar",
      cine_area_code == "10" ~ "Serviços"
    )
  ) %>%
  distinct(NO_AREA, .keep_all = TRUE)

# ---- 3c: CINE Brasil lookup ----
cine_lookup <- map_dfr(
  all_files %>% filter(era == "post2009") %>% pull(path) %>%
    set_names(all_files %>% filter(era == "post2009") %>% pull(year)),
  ~ read_post2009(.x) %>%
    select(any_of(c("CO_CINE_AREA_GERAL",      "NO_CINE_AREA_GERAL",
                    "CO_CINE_AREA_ESPECIFICA",  "NO_CINE_AREA_ESPECIFICA",
                    "CO_CINE_AREA_DETALHADA",   "NO_CINE_AREA_DETALHADA"))),
  .id = "source_year"
) %>%
  mutate(source_year = as.integer(source_year)) %>%
  filter(!is.na(CO_CINE_AREA_DETALHADA), CO_CINE_AREA_DETALHADA != "") %>%
  arrange(desc(source_year)) %>%
  distinct(CO_CINE_AREA_DETALHADA, .keep_all = TRUE) %>%
  select(-source_year) %>%
  rename(
    cine_area_code = CO_CINE_AREA_GERAL,
    cine_area_name = NO_CINE_AREA_GERAL,
    area_code_2dig = CO_CINE_AREA_ESPECIFICA,
    area_name_2dig = NO_CINE_AREA_ESPECIFICA,
    area_code_3dig = CO_CINE_AREA_DETALHADA,
    area_name_3dig = NO_CINE_AREA_DETALHADA
  ) %>%
  mutate(
    across(everything(), ~ str_squish(str_to_title(.))),
    cine_area_name = if_else(
      cine_area_code == "06",
      "Computação E Tecnologias Da Informação E Comunicação (TIC)",
      cine_area_name
    )
  ) %>%
  arrange(cine_area_code, area_code_2dig, area_code_3dig)

# Save lookups
dir.create("data/lookup",    showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(area_lookup_old, "data/lookup/area_lookup_old_system.csv")
write_csv(no_area_lookup,  "data/lookup/no_area_lookup.csv")
write_csv(cine_lookup,     "data/lookup/cine_lookup.csv")

# ================================================================
# SECTION 4: PRE-2009 CLEANER
# ================================================================

clean_pre2009 <- function(path, year) {
  
  # 1997 file: QT_FEM_DIP is NOT annual graduates — skip
  # 1997 graduates are recovered from the 1998 file (which reports year - 1)
  if (year == 1997) {
    message("Skipping 1997 file — 1997 graduates recovered from 1998 file")
    return(NULL)
  }
  
  raw <- read_pre2009(path)
  
  # ---- Shared column name variants ----
  inst_col   <- find_col(raw, c("MASCARA", "IES"))
  course_col <- find_col(raw, c("CD_CURSO", "CURSO", "CO_CURSO"))
  name_col   <- find_col(raw, c("NO_CURSO", "NOMEDOCURSO", "NOME_CURSO"))
  uf_col     <- find_col(raw, c("SG_UF_CURSO", "SIGLA_UF_CURSO", "SIGLA_UF", "SG_UF"))
  
  # ---- Degree type — if/else because conditions are scalar ----
  degree_type <- if ("IN_BACHARELADO" %in% names(raw)) {
    # 1995–1996 style: 1/0 flags
    case_when(
      raw[["IN_BACHARELADO"]]  == "1" ~ "bacharelado",
      raw[["IN_LICENC_PLENA"]] == "1" ~ "licenciatura",
      raw[["IN_LICENC_CURTA"]] == "1" ~ "licenciatura_curta",
      raw[["IN_TECNOLOGO"]]    == "1" ~ "tecnologo",
      TRUE                            ~ "outro"
    )
  } else if ("EH_BACHARELADO" %in% names(raw)) {
    # 2000–2008 style: S/N flags
    case_when(
      raw[["EH_BACHARELADO"]] == "S" ~ "bacharelado",
      raw[["EH_LICENCPLENA"]] == "S" ~ "licenciatura",
      raw[["EH_LICCURTA"]]    == "S" ~ "licenciatura_curta",
      raw[["EH_TECNO"]]       == "S" ~ "tecnologo",
      TRUE                           ~ "outro"
    )
  } else {
    # 1998–1999: degree type not available
    rep(NA_character_, nrow(raw))
  }
  
  # ---- 2001: special case — contains BOTH 2000 and 2001 graduates ----
  if (year == 2001) {
    
    # Pre-compute grad vectors for both years
    gf_2000 <- sum_cols_safe(raw, c("C1311", "C1313", "C1321", "C1323"))
    gm_2000 <- sum_cols_safe(raw, c("C1312", "C1314", "C1322", "C1324"))
    gf_2001 <- sum_cols_safe(raw, c("C8111", "C8113", "C8121", "C8123"))
    gm_2001 <- sum_cols_safe(raw, c("C8112", "C8114", "C8122", "C8124"))
    
    # Build shared base columns once
    base <- raw %>%
      mutate(
        inst_id        = str_pad(str_trim(.data[[inst_col]]), 4, pad = "0"),
        course_code    = str_trim(.data[[course_col]]),
        course_name    = str_squish(str_to_title(.data[[name_col]])),
        area_code      = str_trim(AREACURSO),
        area_code_3dig = str_sub(str_trim(AREACURSO), 1, 3),
        area_free_text = NA_character_,
        state_uf       = str_to_upper(str_trim(.data[[uf_col]])),
        degree_type    = degree_type
      ) %>%
      select(inst_id, course_code, course_name, area_code, area_code_3dig,
             area_free_text, state_uf, degree_type)
    
    rows_2000 <- base %>%
      mutate(year = 2000L, file_year = 2001L,
             grads_female = gf_2000, grads_male = gm_2000,
             grads_total  = gf_2000 + gm_2000) %>%
      filter(grads_total > 0)
    
    rows_2001 <- base %>%
      mutate(year = 2001L, file_year = 2001L,
             grads_female = gf_2001, grads_male = gm_2001,
             grads_total  = gf_2001 + gm_2001) %>%
      filter(grads_total > 0)
    
    return(
      bind_rows(rows_2000, rows_2001) %>%
        select(year, file_year, inst_id, course_code, course_name,
               area_code, area_code_3dig, area_free_text,
               state_uf, degree_type, grads_female, grads_male, grads_total)
    )
  }
  
  # ---- All other pre-2009 years ----
  
  # Graduate columns + actual graduate year
  if (year %in% c(1995, 1996)) {
    actual_grad_year <- year
    grads_female <- sum_cols_safe(raw, c("QT_DIPLO_1SEM_FEMI", "QT_DIPLO_2SEM_FEMI"))
    grads_male   <- sum_cols_safe(raw, c("QT_DIPLO_1SEM_MASC", "QT_DIPLO_2SEM_MASC"))
    
  } else if (year %in% c(1998, 1999)) {
    actual_grad_year <- year - 1          # 1998 file → 1997 grads; 1999 file → 1998 grads
    grads_female <- sum_cols_safe(raw, c("C0811", "C0813"))
    grads_male   <- sum_cols_safe(raw, c("C0812", "C0814"))
    
  } else if (year == 2000) {
    actual_grad_year <- year - 1          # 2000 file → 1999 grads
    grads_female <- sum_cols_safe(raw, c("C1311", "C1313"))
    grads_male   <- sum_cols_safe(raw, c("C1312", "C1314"))
    
  } else if (year %in% 2002:2008) {
    actual_grad_year <- year
    grads_female <- sum_cols_safe(raw, c("C8111", "C8113", "C8121", "C8123"))
    grads_male   <- sum_cols_safe(raw, c("C8112", "C8114", "C8122", "C8124"))
    
  } else {
    message("Year ", year, ": unexpected — skipping")
    return(NULL)
  }
  
  # Area classification
  if ("AREACURSO" %in% names(raw)) {
    area_code      <- str_trim(raw[["AREACURSO"]])
    area_code_3dig <- str_sub(area_code, 1, 3)
    area_free_text <- NA_character_
  } else if ("NO_AREA" %in% names(raw)) {
    area_code      <- NA_character_
    area_code_3dig <- NA_character_
    area_free_text <- str_squish(raw[["NO_AREA"]])
  } else {
    area_code      <- NA_character_
    area_code_3dig <- NA_character_
    area_free_text <- NA_character_
  }
  
  raw %>%
    mutate(
      year           = actual_grad_year,
      file_year      = !!year,
      inst_id        = str_pad(str_trim(.data[[inst_col]]), 4, pad = "0"),
      course_code    = str_trim(.data[[course_col]]),
      course_name    = str_squish(str_to_title(.data[[name_col]])),
      area_code      = area_code,
      area_code_3dig = area_code_3dig,
      area_free_text = area_free_text,
      state_uf       = str_to_upper(str_trim(.data[[uf_col]])),
      degree_type    = degree_type,
      grads_female   = grads_female,
      grads_male     = grads_male,
      grads_total    = grads_female + grads_male
    ) %>%
    filter(grads_total > 0) %>%
    select(year, file_year, inst_id, course_code, course_name,
           area_code, area_code_3dig, area_free_text,
           state_uf, degree_type, grads_female, grads_male, grads_total)
}

# ================================================================
# SECTION 5: POST-2009 CLEANER
# ================================================================

clean_post2009 <- function(path, year) {
  
  raw <- read_post2009(path) %>%
    filter(str_trim(TP_MODALIDADE_ENSINO) == "1")   # presencial only
  
  uf_col <- find_col(raw, c("SG_UF_CURSO", "SG_UF"))
  
  raw %>%
    mutate(
      year           = to_int(NU_ANO_CENSO),
      file_year      = !!year,
      inst_id        = str_pad(str_trim(CO_IES), 4, pad = "0"),
      course_code    = str_trim(CO_CURSO),
      course_name    = str_squish(str_to_title(NO_CURSO)),
      area_code      = str_trim(CO_CINE_AREA_DETALHADA),
      area_code_3dig = str_trim(CO_CINE_AREA_DETALHADA),
      area_free_text = NA_character_,
      state_uf       = str_to_upper(str_trim(.data[[uf_col]])),
      degree_type    = case_when(
        str_trim(TP_GRAU_ACADEMICO) == "1" ~ "bacharelado",
        str_trim(TP_GRAU_ACADEMICO) == "2" ~ "licenciatura",
        str_trim(TP_GRAU_ACADEMICO) == "3" ~ "tecnologo",
        TRUE                               ~ "outro"
      ),
      grads_female   = coalesce(to_int(QT_CONC_FEM),  0L),
      grads_male     = coalesce(to_int(QT_CONC_MASC), 0L),
      grads_total    = grads_female + grads_male
    ) %>%
    filter(grads_total > 0) %>%
    select(year, file_year, inst_id, course_code, course_name,
           area_code, area_code_3dig, area_free_text,
           state_uf, degree_type, grads_female, grads_male, grads_total)
}

# ================================================================
# SECTION 6: RUN CLEANING FUNCTIONS
# ================================================================

pre2009_data <- map2_dfr(
  all_files %>% filter(era == "pre2009") %>% pull(path),
  all_files %>% filter(era == "pre2009") %>% pull(year),
  clean_pre2009
)

post2009_data <- map2_dfr(
  all_files %>% filter(era == "post2009") %>% pull(path),
  all_files %>% filter(era == "post2009") %>% pull(year),
  clean_post2009
)

cat("Pre-2009 rows: ", nrow(pre2009_data), "\n")
cat("Post-2009 rows:", nrow(post2009_data), "\n")

# ================================================================
# SECTION 7: JOIN AREA LOOKUPS
# ================================================================

pre2009_structured <- pre2009_data %>%
  filter(!is.na(area_code_3dig)) %>%
  left_join(
    area_lookup_old %>% select(area_code_3dig, cine_area_code, cine_area_name),
    by           = "area_code_3dig",
    relationship = "many-to-one"
  )

pre2009_text <- pre2009_data %>%
  filter(!is.na(area_free_text)) %>%
  mutate(area_free_text_clean = str_squish(area_free_text)) %>%
  left_join(
    no_area_lookup %>% select(NO_AREA, cine_area_code, cine_area_name),
    by = c("area_free_text_clean" = "NO_AREA")
  ) %>%
  select(-area_free_text_clean)

post2009_joined <- post2009_data %>%
  left_join(
    cine_lookup %>% select(area_code_3dig, cine_area_code, cine_area_name),
    by           = "area_code_3dig",
    relationship = "many-to-one"
  )

# ================================================================
# SECTION 8: COMBINE, FILTER TECNÓLOGO, FINALISE
# ================================================================

master <- bind_rows(pre2009_structured, pre2009_text, post2009_joined) %>%
  # Remove tecnólogo where degree_type is known; keep NA rows (1997–1998 files)
  filter(is.na(degree_type) | degree_type != "tecnologo") %>%
  mutate(
    grads_total       = coalesce(grads_female, 0L) + coalesce(grads_male, 0L),
    area_classified   = !is.na(cine_area_code),
    data_quality_flag = case_when(
      year == 1996 ~ "sparse — known reporting gap (51 courses only)",
      TRUE         ~ "ok"
    )
  ) %>%
  filter(grads_total > 0) %>%
  arrange(year, inst_id, course_code)

# ================================================================
# SECTION 9: VALIDATION
# ================================================================

cat("\n=== Annual totals (compare to INEP Sinopse) ===\n")
master %>%
  group_by(year) %>%
  summarise(
    n_courses      = n(),
    grads_f        = sum(grads_female, na.rm = TRUE),
    grads_m        = sum(grads_male,   na.rm = TRUE),
    total          = sum(grads_total,  na.rm = TRUE),
    pct_female     = round(100 * grads_f / total, 1),
    pct_classified = round(100 * mean(area_classified), 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

cat("\n=== Year coverage ===\n")
tibble(year = 1995:2024) %>%
  mutate(
    in_data = year %in% unique(master$year),
    note    = case_when(
      year == 1996 & in_data  ~ "OK — sparse (51 courses only)",
      !in_data                ~ "MISSING — investigate",
      TRUE                    ~ "OK"
    )
  ) %>%
  print(n = 30)

cat("\n=== Unmatched area codes (pre-2009 structured) ===\n")
pre2009_structured %>%
  filter(is.na(cine_area_code)) %>%
  count(year, area_code_3dig, sort = TRUE) %>%
  print(n = 20)

cat("\n=== Unmatched NO_AREA values (free-text years) ===\n")
pre2009_text %>%
  filter(is.na(cine_area_code)) %>%
  count(year, area_free_text, sort = TRUE) %>%
  print(n = 20)

cat("\n=== Graduates by CINE area x year ===\n")
master %>%
  filter(!is.na(cine_area_code)) %>%
  group_by(year, cine_area_code, cine_area_name) %>%
  summarise(
    grads_f = sum(grads_female, na.rm = TRUE),
    grads_m = sum(grads_male,   na.rm = TRUE),
    total   = sum(grads_total,  na.rm = TRUE),
    pct_f   = round(100 * grads_f / total, 1),
    .groups = "drop"
  ) %>%
  arrange(cine_area_code, year) %>%
  print(n = 300)

# ================================================================
# SECTION 10: SAVE
# ================================================================

write_csv(master, "data/processed/master_graduations.csv")

message("\nDone.")
message(nrow(master), " course-year rows | ",
        n_distinct(master$year), " grad years | ",
        round(100 * mean(master$area_classified), 1), "% area-classified")
