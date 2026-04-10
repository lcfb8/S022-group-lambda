library(tidyverse)
library(dplyr)
library(readr)


# Read in the data for each year
df1980 <- read_csv("~/Exploratory Analysis/1980/ef1980_acp.csv")
df1986 <- read_csv("~/Exploratory Analysis/1986/ef1986_acp.csv")
df1994 <- read_csv("~/Exploratory Analysis/1994/ef1994_acp.csv")
df1996 <- read_csv("~/Exploratory Analysis/1996/ef96_acp.csv")
df1998 <- read_csv("~/Exploratory Analysis/1998/ef98_acp.csv")
df2000 <- read_csv("~/Exploratory Analysis/2000/ef2000cp.csv")
df2002 <- read_csv("~/Exploratory Analysis/2002/ef2002cp.csv")
df2004 <- read_csv("~/Exploratory Analysis/2004/ef2004cp_rv.csv")
df2006 <- read_csv("~/Exploratory Analysis/2006/ef2006cp_rv.csv")
df2008 <- read_csv("~/Exploratory Analysis/2008/ef2008cp_rv.csv")
df2010 <- read_csv("~/Exploratory Analysis/2010/ef2010cp_rv.csv")
df2012 <- read_csv("~/Exploratory Analysis/2012/ef2012cp_rv.csv")
df2014 <- read_csv("~/Exploratory Analysis/2014/ef2014cp_rv.csv")
df2016 <- read_csv("~/Exploratory Analysis/2016/ef2016cp_rv.csv")
df2018 <- read_csv("~/Exploratory Analysis/2018/ef2018cp_rv.csv") 
df2020 <- read_csv("~/Exploratory Analysis/2020/ef2020cp_rv.csv")
df2022 <- read_csv("~/Exploratory Analysis/2022/ef2022cp_rv.csv")
df2024 <- read_csv("~/Exploratory Analysis/2024/ef2024cp.csv")
GDP_UNRATE <- read_csv("~/Exploratory Analysis/GDP & UNRATE/GDP_UNRATE.csv")

# ── Add year columns ──────────────────────────────────────────────────────────
df1980 <- df1980 %>% mutate(year = 1980)
df1986 <- df1986 %>% mutate(year = 1986)
df1994 <- df1994 %>% mutate(year = 1994)
df1996 <- df1996 %>% mutate(year = 1996)
df1998 <- df1998 %>% mutate(year = 1998)
df2000 <- df2000 %>% mutate(year = 2000)
df2002 <- df2002 %>% mutate(year = 2002)
df2004 <- df2004 %>% mutate(year = 2004)
df2006 <- df2006 %>% mutate(year = 2006)
df2008 <- df2008 %>% mutate(year = 2008)
df2010 <- df2010 %>% mutate(year = 2010)
df2012 <- df2012 %>% mutate(year = 2012)
df2014 <- df2014 %>% mutate(year = 2014)
df2016 <- df2016 %>% mutate(year = 2016)
df2018 <- df2018 %>% mutate(year = 2018)
df2020 <- df2020 %>% mutate(year = 2020)
df2022 <- df2022 %>% mutate(year = 2022)
df2024 <- df2024 %>% mutate(year = 2024)


# Define the columns you actually need
keep_cols <- c(
  "year", "EFCIPLEV", "UNITID",
  "EFTOTLT", "EFTOTLM", "EFTOTLW",
  "EFAIANT", "EFAIANM", "EFAIANW",
  "EFASIAT", "EFASIAM", "EFASIAW",
  "EFBKAAT", "EFBKAAM", "EFBKAAW",
  "EFHISPT", "EFHISPM", "EFHISPW",
  "EFNHPIT", "EFNHPIM", "EFNHPIW",
  "EFWHITT", "EFWHITM", "EFWHITW",
  "EF2MORT", "EF2MORM", "EF2MORW",
  "EFUNKNT", "EFUNKNM", "EFUNKNW",
  "EFNRALT", "EFNRALM", "EFNRALW"
)


# ── 1980 clean ────────────────────────────────────────────────────────────────
df1980_clean <- df1980 %>%
  rename_with(toupper) %>%
  filter(LINE == 29) %>% 
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFTOTLM = EFRACE15, EFTOTLW  = EFRACE16,
    EFCIPLEV = FIELD
  ) %>%
  mutate(
    year     = 1980,           # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(      # FIX 1: was just renaming FIELD, now recodes to standard codes
      EFCIPLEV == 900  ~ 201,     # Engineering
      EFCIPLEV == 400  ~ 301,     # Biological Sciences
      EFCIPLEV == 1900 ~ 501,     # Physical Sciences
      EFCIPLEV == 500  ~ 601,     # Business
      EFCIPLEV == 1400 ~ 716,     # Law
      EFCIPLEV == 1204 ~ 816,     # Dentistry
      EFCIPLEV == 1206 ~ 916,     # Medicine
      TRUE ~ NA_real_
    ),
    EFTOTLT  = GEFM15 + GEFW16,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = NA, EFUNKNM = NA, EFUNKNW = NA,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── 1986 clean ────────────────────────────────────────────────────────────────
df1986_clean <- df1986 %>%
  rename_with(toupper) %>%
  filter(LINE == 29) %>% 
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFCIPLEV = CIPCODE
  ) %>%
  mutate(
    year     = 1986,           # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(      # FIX 2: was renaming CIPCODE (character) → now numeric codes
      EFCIPLEV == "14.00" ~ 201, # Engineering
      EFCIPLEV == "26.00" ~ 301, # Biological Sciences
      EFCIPLEV == "27.00" ~ 401, # Mathematics
      EFCIPLEV == "40.00" ~ 501, # Physical Sciences
      EFCIPLEV == "06.00" ~ 601, # Business
      EFCIPLEV == "22.00" ~ 716, # Law
      EFCIPLEV == "18.04" ~ 816, # Dentistry
      EFCIPLEV == "18.10" ~ 916, # Medicine
      TRUE ~ NA_real_
    ),
    EFTOTLT  = EFTOTLM + EFTOTLW,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = NA, EFUNKNM = NA, EFUNKNW = NA,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── 1994 clean ────────────────────────────────────────────────────────────────
df1994_clean <- df1994 %>%
  rename_with(toupper) %>%
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFUNKNM = EFRACE13, EFUNKNW = EFRACE14,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFCIPLEV = CIPCODE
  ) %>%
  filter(is.na(SECTION) | SECTION == "" | SECTION == 0) %>%
  mutate(
    year     = 1994,  # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(
      EFCIPLEV == "13.0000" ~ 101,
      EFCIPLEV == "14.0000" ~ 201,
      EFCIPLEV == "26.0000" ~ 301,
      EFCIPLEV == "27.0000" ~ 401,
      EFCIPLEV == "40.0000" ~ 501,
      EFCIPLEV == "52.0000" ~ 601,
      EFCIPLEV == "22.0101" ~ 716,
      EFCIPLEV == "51.0401" ~ 816,
      EFCIPLEV == "51.1201" ~ 916,
      TRUE ~ NA_real_
    ),
    EFTOTLT  = EFTOTLM + EFTOTLW,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = EFUNKNM + EFUNKNW,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── 1996 clean ────────────────────────────────────────────────────────────────
df1996$cipcode <- as.numeric(df1996$cipcode)  # Ensure CIPCODE is numeric for consistent recoding
df1996_clean <- df1996 %>%
  rename_with(toupper) %>%
  filter(LINE == 29) %>%  
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFUNKNM = EFRACE13, EFUNKNW = EFRACE14,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFCIPLEV = CIPCODE
  ) %>%  # FIX 3: Convert CIPCODE to numeric before recoding
  mutate(
    year     = 1996,  # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(
      EFCIPLEV == "13" ~ 101, # removed 0's to run case_when on numeric EFCIPLEV
      EFCIPLEV == "14" ~ 201,
      EFCIPLEV == "26" ~ 301,
      EFCIPLEV == "27" ~ 401,
      EFCIPLEV == "40" ~ 501,
      EFCIPLEV == "52" ~ 601,
      EFCIPLEV == "22.0101" ~ 716,
      EFCIPLEV == "51.0401" ~ 816,
      EFCIPLEV == "51.1201" ~ 916,
      TRUE ~ NA_real_
    ),
    EFTOTLT  = EFTOTLM + EFTOTLW,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = EFUNKNM + EFUNKNW,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── 1998 clean ────────────────────────────────────────────────────────────────
df1998_clean <- df1998 %>%
  rename_with(toupper) %>%
  filter(LINE == 29) %>%
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFUNKNM = EFRACE13, EFUNKNW = EFRACE14,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFCIPLEV = CIPCODE
  )  %>% 
  mutate(
    year     = 1998,  # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(
      EFCIPLEV == "13" ~ 101,
      EFCIPLEV == "14" ~ 201,
      EFCIPLEV == "26" ~ 301,
      EFCIPLEV == "27" ~ 401,
      EFCIPLEV == "40" ~ 501,
      EFCIPLEV == "52" ~ 601,
      EFCIPLEV == "22.0101" ~ 716,
      EFCIPLEV == "51.0401" ~ 816,
      EFCIPLEV == "51.1201" ~ 916,
      TRUE ~ NA_real_
    ),
    EFTOTLT  = EFTOTLM + EFTOTLW,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = EFUNKNM + EFUNKNW,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── 2000 clean ────────────────────────────────────────────────────────────────
df2000_clean <- df2000 %>%
  rename_with(toupper) %>%
  filter(LINE == 29) %>% 
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFUNKNM = EFRACE13, EFUNKNW = EFRACE14,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFCIPLEV = CIPCODE
  ) %>%
  mutate(
    year     = 2000,  # FIX 5: restore year lost by toupper
    EFCIPLEV = case_when(
      EFCIPLEV == "13" ~ 101,
      EFCIPLEV == "14" ~ 201,
      EFCIPLEV == "26" ~ 301,
      EFCIPLEV == "27" ~ 401,
      EFCIPLEV == "40" ~ 501,
      EFCIPLEV == "52" ~ 601,
      EFCIPLEV == "22.0101" ~ 716,
      EFCIPLEV == "51.0401" ~ 816,
      EFCIPLEV == "51.1201" ~ 916,
      TRUE ~ NA_real_
    ),
    EFTOTLT  = EFTOTLM + EFTOTLW,
    EFNRALT  = EFNRALM + EFNRALW,
    EFBKAAT  = EFBKAAM + EFBKAAW,
    EFAIANT  = EFAIANM + EFAIANW,
    EFASIAT  = EFASIAM + EFASIAW,
    EFHISPT  = EFHISPM + EFHISPW,
    EFWHITT  = EFWHITM + EFWHITW,
    EFUNKNT  = EFUNKNM + EFUNKNW,
    EFNHPIT  = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT  = NA, EF2MORM = NA, EF2MORW = NA
  )%>%
  select(any_of(keep_cols))

# ── Combine early years ───────────────────────────────────────────────────────
early_df <- bind_rows(
  df1980_clean,
  df1986_clean,
  df1994_clean,
  df1996_clean,
  df1998_clean,
  df2000_clean
) %>%
  mutate(EFCIPLEV = as.numeric(EFCIPLEV))  # Ensure consistent numeric type

# ── 2002-2006 old_df ──────────────────────────────────────────────────────────
# FIX: Removed first (duplicate) old_df definition that included 1998-2006
df2002 <- df2002 %>% rename_with(toupper) %>% mutate(year = 2002)
df2004 <- df2004 %>% rename_with(toupper) %>% mutate(year = 2004)
df2006 <- df2006 %>% rename_with(toupper) %>% mutate(year = 2006)

old_df <- bind_rows(df2002, df2004, df2006) %>%
  filter(LSTUDY == 1) %>%
  rename(
    EFNRALM = EFRACE01, EFNRALW = EFRACE02,
    EFBKAAM = EFRACE03, EFBKAAW = EFRACE04,
    EFAIANM = EFRACE05, EFAIANW = EFRACE06,
    EFASIAM = EFRACE07, EFASIAW = EFRACE08,
    EFHISPM = EFRACE09, EFHISPW = EFRACE10,
    EFWHITM = EFRACE11, EFWHITW = EFRACE12,
    EFUNKNM = EFRACE13, EFUNKNW = EFRACE14,
    EFTOTLM = EFRACE15, EFTOTLW = EFRACE16,
    EFNRALT = EFRACE17, EFBKAAT = EFRACE18,
    EFAIANT = EFRACE19, EFASIAT = EFRACE20,
    EFHISPT = EFRACE21, EFWHITT = EFRACE22,
    EFUNKNT = EFRACE23, EFTOTLT = EFRACE24
  ) %>%
  mutate(
    EFNHPIT = NA, EFNHPIM = NA, EFNHPIW = NA,
    EF2MORT = NA, EF2MORM = NA, EF2MORW = NA
  )

# ── 2008 clean ────────────────────────────────────────────────────────────────
df2008 <- df2008 %>%
  rename_with(toupper) %>%
  mutate(
    year    = 2008,     # FIX 6: restore year lost by toupper
    EFAIANT = DVEFAIT, EFAIANM = DVEFAIM, EFAIANW = DVEFAIW,
    EFASIAT = DVEFAPT, EFASIAM = DVEFAPM, EFASIAW = DVEFAPW,
    EFBKAAT = DVEFBKT, EFBKAAM = DVEFBKM, EFBKAAW = DVEFBKW,
    EFHISPT = DVEFHST, EFHISPM = DVEFHSM, EFHISPW = DVEFHSW,
    EFWHITT = DVEFWHT, EFWHITM = DVEFWHM, EFWHITW = DVEFWHW,
    EFNHPIT = NA, EFNHPIM = NA, EFNHPIW = NA
  )

# ── Combine all years ─────────────────────────────────────────────────────────
# FIX 7: early_df was created but never included in combined_df
combined_df <- bind_rows(
  early_df,   # 1980, 1986, 1994, 1996, 1998, 2000
  old_df,     # 2002, 2004, 2006
  df2008,
  df2010,
  df2012,
  df2014,
  df2016,
  df2018,
  df2020,
  df2022,
  df2024
)

# ── Select variables ──────────────────────────────────────────────────────────
filtered_df <- combined_df %>% select(
  year, EFCIPLEV,
  EFTOTLT, EFTOTLM, EFTOTLW,
  EFAIANT, EFAIANM, EFAIANW,
  EFASIAT, EFASIAM, EFASIAW,
  EFBKAAT, EFBKAAM, EFBKAAW,
  EFHISPT, EFHISPM, EFHISPW,
  EFNHPIT, EFNHPIM, EFNHPIW,
  EFWHITT, EFWHITM, EFWHITW,
  EF2MORT, EF2MORM, EF2MORW,
  EFUNKNT, EFUNKNM, EFUNKNW,
  EFNRALT, EFNRALM, EFNRALW
)

# ── Rename variables ──────────────────────────────────────────────────────────
filtered_df <- filtered_df %>%
  rename(
    Major        = EFCIPLEV,
    Grand_total  = EFTOTLT,  Total_men    = EFTOTLM,  Total_women  = EFTOTLW,
    AIAN_total   = EFAIANT,  AIAN_men     = EFAIANM,  AIAN_women   = EFAIANW,
    A_total      = EFASIAT,  A_men        = EFASIAM,  A_women      = EFASIAW,
    B_total      = EFBKAAT,  B_men        = EFBKAAM,  B_women      = EFBKAAW,
    H_total      = EFHISPT,  H_men        = EFHISPM,  H_women      = EFHISPW,
    NHPI_total   = EFNHPIT,  NHPI_men     = EFNHPIM,  NHPI_women   = EFNHPIW,
    W_total      = EFWHITT,  W_men        = EFWHITM,  W_women      = EFWHITW,
    TwoMor_total = EF2MORT,  TwoMor_men   = EF2MORM,  TwoMor_women = EF2MORW,
    UNK_total    = EFUNKNT,  UNK_men      = EFUNKNM,  UNK_women    = EFUNKNW,
    NR_total     = EFNRALT,  NR_men       = EFNRALM,  NR_women     = EFNRALW
  )

# ── Summarise by year and major ───────────────────────────────────────────────
final_df <- filtered_df %>%
  filter(Major %in% c(101, 201, 301, 401, 501, 601, 716, 816, 916)) %>%
  group_by(year, Major) %>%
  summarise(
    Grand_total  = sum(Grand_total,  na.rm = TRUE),
    Total_men    = sum(Total_men,    na.rm = TRUE),
    Total_women  = sum(Total_women,  na.rm = TRUE),
    AIAN_total   = sum(AIAN_total,   na.rm = TRUE),
    AIAN_men     = sum(AIAN_men,     na.rm = TRUE),
    AIAN_women   = sum(AIAN_women,   na.rm = TRUE),
    A_total      = sum(A_total,      na.rm = TRUE),
    A_men        = sum(A_men,        na.rm = TRUE),
    A_women      = sum(A_women,      na.rm = TRUE),
    B_total      = sum(B_total,      na.rm = TRUE),
    B_men        = sum(B_men,        na.rm = TRUE),
    B_women      = sum(B_women,      na.rm = TRUE),
    H_total      = sum(H_total,      na.rm = TRUE),
    H_men        = sum(H_men,        na.rm = TRUE),
    H_women      = sum(H_women,      na.rm = TRUE),
    NHPI_total   = sum(NHPI_total,   na.rm = TRUE),
    NHPI_men     = sum(NHPI_men,     na.rm = TRUE),
    NHPI_women   = sum(NHPI_women,   na.rm = TRUE),
    W_total      = sum(W_total,      na.rm = TRUE),
    W_men        = sum(W_men,        na.rm = TRUE),
    W_women      = sum(W_women,      na.rm = TRUE),
    TwoMor_total = sum(TwoMor_total, na.rm = TRUE),
    TwoMor_men   = sum(TwoMor_men,   na.rm = TRUE),
    TwoMor_women = sum(TwoMor_women, na.rm = TRUE),
    UNK_total    = sum(UNK_total,    na.rm = TRUE),
    UNK_men      = sum(UNK_men,      na.rm = TRUE),
    UNK_women    = sum(UNK_women,    na.rm = TRUE),
    NR_total     = sum(NR_total,     na.rm = TRUE),
    NR_men       = sum(NR_men,       na.rm = TRUE),
    NR_women     = sum(NR_women,     na.rm = TRUE),
    .groups = "drop"
  )

final_df <- final_df %>%
  left_join(GDP_UNRATE, by = c("year" = "Year")) 

write_csv(final_df, "~/Exploratory Analysis/final_df.csv")

# color blind friendly options
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000", "#999999")

library(RColorBrewer)


# Plotting the change in major over years
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education",
                                   "Engineering",
                                   "Biological Sciences",
                                   "Mathematics",
                                   "Physical Sciences",
                                   "Business Management",
                                   "Law",
                                   "Dentistry",
                                   "Medicine"))) %>%
  ggplot(aes(x = year, y = Grand_total, color = Major, group = Major)) +
  geom_line(linewidth = 1.5) +
  geom_point(size=4) +                         # Optional: adds points at each year
  labs(title = "Trends in Grand Total by Major Over Time",
       x = "Year",
       y = "Grand Total") +
  theme_minimal() +
  theme(legend.title = element_blank())

# creating a data frame in long format to plot the change in ethnicity
ethnicity_long <- final_df %>%
  select(year, Major, ends_with("_total")) %>%  # Keep only the total columns
  pivot_longer(
    cols = ends_with("_total"),
    names_to = "Ethnicity",
    values_to = "Total"
  ) %>%
  mutate(
    Ethnicity = factor(Ethnicity,
                       levels = c("Grand_total", "AIAN_total", "A_total", "B_total",
                                  "H_total", "NHPI_total", "W_total",
                                  "TwoMor_total", "UNK_total", "NR_total"),
                       labels = c("Grand Total", "AIAN",
                                  "Asian", "Black",
                                  "Hispanic", "NHPI",
                                  "White", "Two or More Races",
                                  "Unknown", "Non-Resident")),
    Major = factor(Major,
                   levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                   labels = c("Education", "Engineering", "Biological Sciences",
                              "Mathematics", "Physical Sciences", "Business Management",
                              "Law", "Dentistry", "Medicine"))
  )

ethnicity_long %>%
  ggplot(aes(x = year, y = Total, color = Major, group = Major)) +
  geom_line(linewidth=1) +
  geom_point() +
  facet_wrap(~ Ethnicity, scales = "free_y") +  # free_y since counts vary widely by group
  labs(title = "Enrollment Trends by Ethnicity and Major Over Time",
       x = "Year",
       y = "Total Enrollment",
       color = "Major") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),   # Facet label size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle years for readability
  )

# creating a data frame in long format to plot the change in major by gender
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  pivot_longer(cols = c(Total_men, Total_women),
               names_to = "Gender",
               values_to = "Total") %>%
  ggplot(aes(x = year, y = Total, color = Gender, group = Gender)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "Gender Gap by Major Over Time",
       x = "Year", y = "Total Enrollment") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Percent Change in Enrollment from 2008 Baseline
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  group_by(Major) %>%
  mutate(pct_change = (Grand_total - first(Grand_total)) / first(Grand_total) * 100) %>%
  ggplot(aes(x = year, y = pct_change, color = Major, group = Major)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = okabe_ito) +    # Okabe-Ito for 9 majors
  labs(title = "Percent Change in Enrollment from 2002 Baseline",
       x = "Year", y = "Percent Change (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

#  Enrollment vs UNRATE Over Time
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = Grand_total, color = Major, group = Major),
            linewidth = 1.2) +
  geom_line(aes(y = UNRATE * max(Grand_total) / max(UNRATE)),
            color = "black", linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Total Enrollment",
    labels = scales::comma,
    sec.axis = sec_axis(
      ~ . * max(final_df$UNRATE) / max(final_df$Grand_total),
      name = "Unemployment Rate (%)"
    )
  ) +
  scale_color_manual(values = okabe_ito) +
  labs(title = "Enrollment by Major vs Unemployment Rate Over Time",
       x = "Year",
       caption = "Dashed line = Unemployment Rate (UNRATE)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black")
  )

# Scatter: UNRATE vs Enrollment by Major
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = UNRATE, y = Grand_total, color = Major)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = okabe_ito) +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "Unemployment Rate vs Enrollment by Major",
       x = "Unemployment Rate - UNRATE (%)",
       y = "Total Enrollment") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Scatter: GDP vs Enrollment by Major
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = GDP, y = Grand_total, color = Major)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-12,
                                                    suffix = "T")) +
  scale_color_manual(values = okabe_ito) +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "GDP vs Enrollment by Major",
       x = "GDP (Trillions USD)",
       y = "Total Enrollment") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Percent Change: Enrollment vs GDP and UNRATE
econ_long <- final_df %>%
  group_by(Major) %>%
  mutate(
    enrollment_pct = (Grand_total - first(Grand_total)) / first(Grand_total) * 100,
    gdp_pct        = (GDP - first(GDP)) / first(GDP) * 100,
    unrate_pct     = (UNRATE - first(UNRATE)) / first(UNRATE) * 100
  ) %>%
  ungroup() %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  select(year, Major, enrollment_pct, gdp_pct, unrate_pct) %>%
  pivot_longer(cols = c(enrollment_pct, gdp_pct, unrate_pct),
               names_to = "Metric",
               values_to = "Pct_Change") %>%
  mutate(Metric = factor(Metric,
                         levels = c("enrollment_pct", "gdp_pct", "unrate_pct"),
                         labels = c("Enrollment", "GDP", "UNRATE")))

econ_long %>%
  ggplot(aes(x = year, y = Pct_Change, color = Metric, group = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#0072B2", "#009E73", "#D55E00"),
                     labels = c("Enrollment", "GDP", "UNRATE")) +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "Percent Change from Baseline: Enrollment vs GDP and UNRATE",
       x = "Year",
       y = "Percent Change (%)",
       color = "Metric") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#  Correlation Heatmap: Enrollment vs GDP and UNRATE
corr_df <- final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  group_by(Major) %>%
  summarise(
    Enrollment_GDP    = cor(Grand_total, GDP,    use = "complete.obs"),
    Enrollment_UNRATE = cor(Grand_total, UNRATE, use = "complete.obs")
  ) %>%
  pivot_longer(cols = c(Enrollment_GDP, Enrollment_UNRATE),
               names_to = "Correlation",
               values_to = "Value") %>%
  mutate(Correlation = recode(Correlation,
                              "Enrollment_GDP"    = "Enrollment vs GDP",
                              "Enrollment_UNRATE" = "Enrollment vs UNRATE"))

corr_df %>%
  ggplot(aes(x = Correlation, y = Major, fill = Value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Value, 2)), size = 3.5) +
  scale_fill_gradient2(low      = "#D55E00",
                       mid      = "white",
                       high     = "#0072B2",
                       midpoint = 0,
                       limits   = c(-1, 1)) +
  labs(title = "Correlation Between Enrollment and Economic Indicators by Major",
       x = NULL, y = NULL, fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


# Make sure UNRATE and GDP exist and have values
final_df %>%
  select(year, GDP, UNRATE) %>%
  distinct() %>%
  arrange(year)



#### Recession Section ####

# Key US recessions within your data range
recessions <- data.frame(
  start = c(1980, 1981, 1990, 2001, 2007, 2020),
  end   = c(1980, 1982, 1991, 2001, 2009, 2020),
  label = c("1980\nRecession",
            "1981-82\nRecession",
            "1990-91\nRecession",
            "2001\nDot-com",
            "2007-09\nGreat Recession",
            "2020\nCOVID")
)

# Enrollment Trends with Recession Shading
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = year, y = Grand_total, color = Major, group = Major)) +
  # Add recession shading FIRST so lines appear on top
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  # Add recession labels
  geom_text(data = recessions,
            aes(x = (start + end) / 2, y = Inf, label = label),
            inherit.aes = FALSE,
            vjust = 1.5, size = 2.5, color = "red") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = okabe_ito) +
  labs(title = "Enrollment by Major with US Recession Periods",
       x = "Year",
       y = "Total Enrollment",
       color = "Major",
       caption = "Red shaded areas = recession periods") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Faceted by Major with Recession Shading
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = year, y = Grand_total)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  geom_line(aes(color = Major, group = Major), linewidth = 1.2) +
  geom_point(aes(color = Major), size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = okabe_ito) +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "Enrollment by Major During Recession Periods",
       x = "Year",
       y = "Total Enrollment",
       caption = "Red shaded areas = recession periods") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Shows how the SHARE of enrollment shifts during recessions
final_df %>%
  group_by(year) %>%
  mutate(
    enrollment_share = Grand_total / sum(Grand_total) * 100
  ) %>%
  ungroup() %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = year, y = enrollment_share, fill = Major)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.10) +
  geom_area(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = okabe_ito) +
  labs(title = "Share of Total Enrollment by Major Over Time",
       subtitle = "Red shaded areas = recession periods",
       x = "Year",
       y = "Enrollment Share (%)",
       fill = "Major") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
