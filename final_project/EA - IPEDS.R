library(tidyverse)
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

# Add year column in each data frame
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

# Combine the data frames into one
# combined_df <- bind_rows(df1980, 
                         #df1986, 
                         #df1994,
                         #df1996,
                         #df1998,
                         #df2000,
                         #df2002,
                         #df2004,
                         #df2006,
                         #df2008,
                         #df2010, 
                         #df2012, 
                         #df2014, 
                         #df2016, 
                         #df2018, 
                         #df2020, 
                         #df2022, 
                         #df2024)

combined_df <- bind_rows(df2010, 
                         df2012, 
                         df2014, 
                         df2016, 
                         df2018, 
                         df2020, 
                         df2022, 
                         df2024)

# Keeping only the variables I want in the large data fram
filtered_df <- combined_df %>% select(year, 
                                   EFCIPLEV, 
                                   EFTOTLT,
                                   EFTOTLM,
                                   EFTOTLW,
                                   EFAIANT,
                                   EFAIANM,
                                   EFAIANW,
                                   EFASIAT,
                                   EFASIAM,
                                   EFASIAW,
                                   EFBKAAT,
                                   EFBKAAM,
                                   EFBKAAW,
                                   EFHISPT,
                                   EFHISPM,
                                   EFHISPW,
                                   EFNHPIT,
                                   EFNHPIM,
                                   EFNHPIW,
                                   EFWHITT,
                                   EFWHITM,
                                   EFWHITW,
                                   EF2MORT,
                                   EF2MORM,
                                   EF2MORW,
                                   EFUNKNT,
                                   EFUNKNM,
                                   EFUNKNW,
                                   EFNRALT,
                                   EFNRALM,
                                   EFNRALW,
)

# renaming variables to be more descriptive
filtered_df <- filtered_df %>%
  rename(
    Major=EFCIPLEV,
    Grand_total=EFTOTLT,
    Total_men=EFTOTLM,
    Total_women=EFTOTLW,
    AIAN_total =EFAIANT,
    AIAN_men =EFAIANM,
    AIAN_women=EFAIANW,
    A_total=EFASIAT,
    A_men =EFASIAM,
    A_women =EFASIAW,
    B_total =EFBKAAT,
    B_men =EFBKAAM,
    B_women=EFBKAAW,
    H_total =EFHISPT,
    H_men =EFHISPM,
    H_women =EFHISPW,
    NHPI_total=EFNHPIT,
    NHPI_men =EFNHPIM,
    NHPI_women=EFNHPIW,
    W_total =EFWHITT,
    W_men =EFWHITM,
    W_women=EFWHITW,
    TwoMor_total =EF2MORT,
    TwoMor_men =EF2MORM,
    TwoMor_women =EF2MORW,
    UNK_total=EFUNKNT,
    UNK_men=EFUNKNM,
    UNK_women=EFUNKNW,
    NR_total=EFNRALT,
    NR_men=EFNRALM,
    NR_women=EFNRALW
)
    
# The data fram included totals from each institution. I wanted to focus on the majors, so I 
# filtered the data frame to include only the major categories and then grouped by year and 
# major to get the sum of each variable for each major category in each year.
final_df <- filtered_df %>%
  filter(Major %in% c(101, 201, 301, 401, 501, 601, 716, 816, 916)) %>%
  group_by(year, Major) %>%
  summarise(
    Grand_total = sum(Grand_total, na.rm = TRUE),
    Total_men = sum(Total_men, na.rm = TRUE),
    Total_women = sum(Total_women, na.rm = TRUE),
    AIAN_total = sum(AIAN_total, na.rm = TRUE),
    AIAN_men = sum(AIAN_men, na.rm = TRUE),
    AIAN_women = sum(AIAN_women, na.rm = TRUE),
    A_total = sum(A_total, na.rm = TRUE),
    A_men = sum(A_men, na.rm = TRUE),
    A_women = sum(A_women, na.rm = TRUE),
    B_total = sum(B_total, na.rm = TRUE),
    B_men = sum(B_men, na.rm = TRUE),
    B_women = sum(B_women, na.rm = TRUE),
    H_total = sum(H_total, na.rm = TRUE),
    H_men = sum(H_men, na.rm = TRUE),
    H_women = sum(H_women, na.rm = TRUE),
    NHPI_total = sum(NHPI_total, na.rm = TRUE),
    NHPI_men = sum(NHPI_men, na.rm = TRUE),
    NHPI_women = sum(NHPI_women, na.rm = TRUE),
    W_total = sum(W_total, na.rm = TRUE),
    W_men = sum(W_men, na.rm = TRUE),
    W_women = sum(W_women, na.rm = TRUE),
    TwoMor_total = sum(TwoMor_total, na.rm = TRUE),
    TwoMor_men = sum(TwoMor_men, na.rm = TRUE),
    TwoMor_women = sum(TwoMor_women, na.rm = TRUE),
    UNK_total = sum(UNK_total, na.rm = TRUE),
    UNK_men = sum(UNK_men, na.rm = TRUE),
    UNK_women = sum(UNK_women, na.rm = TRUE),
    NR_total = sum(NR_total, na.rm = TRUE),
    NR_men = sum(NR_men, na.rm = TRUE),
    NR_women = sum(NR_women, na.rm = TRUE),
    .groups = "drop"
  )

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

# Proportional Diversity Stacked Bar Chart
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  pivot_longer(cols = c(AIAN_total, A_total, B_total, H_total,
                        NHPI_total, W_total, TwoMor_total, UNK_total, NR_total),
               names_to = "Ethnicity",
               values_to = "Total") %>%
  ggplot(aes(x = factor(year), y = Total, fill = Ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ Major) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = okabe_ito,       # Okabe-Ito handles up to 9 colors
                    labels = c("AIAN", "Asian", "Black", "Hispanic",
                               "NHPI", "White", "Two or More",
                               "Unknown", "Non-Resident")) +
  labs(title = "Proportional Ethnic Diversity by Major Over Time",
       x = "Year", y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap of Enrollment by Major and Year
final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  ggplot(aes(x = factor(year), y = Major, fill = Grand_total)) +
  geom_tile() +
  scale_fill_viridis_c(option = "mako") +    # viridis options: "viridis", "magma", "plasma", "mako"
  labs(title = "Enrollment Heatmap by Major and Year",
       x = "Year", y = "Major", fill = "Total Enrollment") +
  theme_minimal()

# Percent Change in Enrollment from 2010 Baseline
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
  labs(title = "Percent Change in Enrollment from 2010 Baseline",
       x = "Year", y = "Percent Change (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")


final_df %>%
  mutate(Major = factor(Major,
                        levels = c(101, 201, 301, 401, 501, 601, 716, 816, 916),
                        labels = c("Education", "Engineering", "Biological Sciences",
                                   "Mathematics", "Physical Sciences", "Business",
                                   "Law", "Dentistry", "Medicine"))) %>%
  pivot_longer(cols = c(AIAN_total, A_total, B_total, H_total,
                        NHPI_total, W_total, TwoMor_total, UNK_total, NR_total),
               names_to = "Ethnicity",
               values_to = "Total") %>%
  mutate(Ethnicity = factor(Ethnicity,
                            levels = c("AIAN_total", "A_total", "B_total", "H_total",
                                       "NHPI_total", "W_total", "TwoMor_total",
                                       "UNK_total", "NR_total"),
                            labels = c("AIAN", "Asian", "Black",
                                       "Hispanic", "NHPI", "White",
                                       "Two or More", "Unknown", "Non-Resident"))) %>%
  group_by(Ethnicity, Major) %>%
  mutate(pct_change = (Total - first(Total)) / first(Total) * 100) %>%
  ggplot(aes(x = year, y = pct_change, color = Ethnicity, group = Ethnicity)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = okabe_ito) +
  facet_wrap(~ Major, scales = "free_y") +
  labs(title = "Percent Change in Enrollment by Ethnicity from 2010 Baseline",
       x = "Year",
       y = "Percent Change (%)",
       color = "Ethnicity") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))
