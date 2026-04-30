library( tidyverse )
library( skimr )
library( stringr )
library( WDI )
library( readr )


brazil_ed <- read_csv("brazil_ed_recoded.csv")
alt_1996 <- read_csv("brazil_1996_alt.csv")
brazil_edprop <- read_csv("brazil_ed_proportions.csv")

#replace 1996 data with 1996 alt data (potentially more accurate totals but
#no split by gender)

alt_1996 = alt_1996 %>% 
  rename("area" = "NO_CINE_AREA_GERAL",
         "total_conc" = "QT_CONC",
         "total_fem" = "QT_CONC_FEM",
         "total_masc" = "QT_CONC_MASC",
         "year"= "NU_ANO_CENSO") %>% 
  mutate(area = recode(area,
                "Agricultura, silvicultura, pesca e veterinária" = "Other/Unknown",
                "Artes e humanidades" = "Arts & Humanities",
                "Educação"= "Education",
                "Ciências sociais, comunicação e informação"= "Social & Behavioral Sciences",
                "Saúde e bem-estar"= "Health & Medical",
                "Ciências naturais, matemática e estatística" = "Natural Sciences",
                "Engenharia, produção e construção" = "Engineering",
                "Negócios, administração e direito" = "Business & Management"))

alt_1996

brazil_ed = brazil_ed %>% 
  filter(year != 1996) %>% 
  bind_rows(alt_1996)



# color blind friendly options
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000", "#999999")

library(RColorBrewer)


# Key economic recessions 
recessions <- data.frame(
  start = c(1999, 2007, 2014, 2020),
  end   = c(1999, 2009, 2016, 2020),
  label = c("1999\n'Samba effect'",
            "2007-09\nGreat Recession",
            "2014-16\nBrazil Econ Crisis",
            "2020\nCOVID")
)

# total line chart
brazil_ed %>%
  filter(area != "Total") %>% 
  ggplot(aes(x = year, y = total_conc/1000, color = area)) +
  scale_color_brewer(palette = "Set2") +
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
  geom_point()+
  geom_line(linewidth = 1) +
  
  labs(
    title   = "Bachelor's Degrees Awarded by Major Category",
    x       = "Year",
    y       = "Total Degrees (in thousands)",
    color   = "Major Category"
  ) +
  theme_minimal()

# Proportion line chart

brazil_edprop %>%
  filter(area != "Total") %>% 
  filter(year >1999) %>% 
  ggplot(aes(x = year, y = prop_conc, color = area)) +
  scale_color_brewer(palette = "Set2") +
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
  geom_point()+
  geom_line(linewidth = 1) +
  
  labs(
    title   = "Proportion of Bachelor's Degrees Awarded by Major Category",
    x       = "Year",
    y       = "Proportion of Total Degrees",
    color   = "Major Category"
  ) +
  theme_minimal()

# Let's try stacked area
brazil_ed %>%
  filter(area != "Total") %>% 
  ggplot(aes(x = year, y = total_conc/1000, fill = area)) +
  geom_area()+
  labs(
    title   = "Bachelor's Degrees Awarded by Major Category",
    x       = "Year",
    y       = "Total Degrees (in thousands)",
    color   = "Major Category"
  ) +
  theme_minimal()


#Now with proportions

brazil_edprop %>% 
  filter(area != "Total") %>% 
  ggplot(aes(year,prop_conc,fill = area))+
  geom_area()



# Faceted by Gender
brazil_ed %>%
  pivot_longer(
    cols      = c(total_masc, total_fem),
    names_to  = "Gender",
    values_to = "Count"
  ) %>%
  mutate(Gender = if_else(Gender == "total_masc", "Men", "Women")) %>%
  mutate(area = factor(area, levels = c("Arts & Humanities",
                                            "Business & Management","Education", 
                                            "Engineering", "Health & Medical",
                                            "Natural Sciences",
                                            "Social & Behavioral Sciences",
                                            "Other/Unknown","Total"))) %>% 
  ggplot(aes(x = year, y = Count, color = Gender)) +
  
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Men" = "#2166ac", "Women" = "#d6604d")) +
  facet_wrap(~ area, scale = "free_y") +
  
  labs(
    title    = "Bachelor's Degrees by Major and Gender in Brazil",
    subtitle = "Red shaded areas indicate economic recessions",
    x        = "Year",
    y        = "Degrees Awarded",
    color    = "Gender",
    caption   = "Note: The Brazil recessions indicated are: 1999: the 'Samba Effect', 2007-09: The Great Recession, 
2014-16: The Brazilian Economic Crisis, 2020: COVID"
  ) +
  theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    plot.caption     = element_text(hjust = 0,        # left align
                                    size  = 11,        # font size
                                    color = "gray40", # gray color
                                    face  = "italic"
                                    ))



# Faceted by Gender proportionally
brazil_edprop %>%
  pivot_longer(
    cols      = c(prop_masc, prop_fem),
    names_to  = "Gender",
    values_to = "Proportion"
  ) %>%
  mutate(Gender = if_else(Gender == "prop_masc", "Men", "Women")) %>%
  mutate(area = as_factor(area)) %>% 
  ggplot(aes(x = year, y = Proportion, color = Gender)) +
  
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  
  geom_line(linewidth = 0.8) +
  
  scale_color_manual(values = c("Men" = "#2166ac", "Women" = "#d6604d")) +
  scale_y_continuous(labels = scales::comma) +
  
  facet_wrap(~ area, scales = "free_y") +
  
  labs(
    title    = "Proportion of Bachelor's Degrees by Major and Gender",
    subtitle = "Red shaded areas indicate Brazil recessions",
    x        = "Year",
    y        = "Degrees Awarded",
    color    = "Gender"
  ) +
  theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom"
  )


# % Women Over Time by Major
brazil_ed %>%
  mutate(pct_women = total_fem / (total_masc + total_fem) * 100) %>%
  mutate(area = as_factor(area)) %>%
  ggplot(aes(x = year, y = pct_women, color = area)) +
  
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(linewidth = 1.25) +
  
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(40, 90)) +
  
  labs(
    title    = "Percentage of Women Earning Bachelor's Degrees by Major",
    subtitle = "Dashed line = 50% | Red shaded areas indicate US recessions",
    x        = "Year",
    y        = "% Women",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

# Gender Ratio Heatmap (Year × Major)
brazil_ed %>%
  mutate(pct_women = total_fem / (total_masc + total_fem) * 100) %>%
  
  ggplot(aes(x = year, y = area, fill = pct_women)) +
  
  geom_tile() +
  
  scale_fill_gradient2(
    low      = "#2166ac",   # blue = more men
    mid      = "white",
    high     = "#d6604d",   # red = more women
    midpoint = 50,
    labels   = scales::percent_format(scale = 1)
  ) +
  
  labs(
    title = "Gender Ratio Heatmap by Major and Year",
    subtitle = "Blue = More Men | Red = More Women | White = 50/50",
    x    = "Year",
    y    = "",
    fill = "% Women"
  ) +
  theme_minimal() +
  theme(
    axis.text.y  = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# Indexed Growth Chart (Base Year = 1995)
brazil_ed %>%
  group_by(area) %>%
  mutate(index = total_conc / total_conc[year == min(year)] * 100) %>%
  ungroup() %>%
  
  ggplot(aes(x = year, y = index, color = area)) +
  
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title    = "Indexed Growth of Bachelor's Degrees by Major",
    subtitle = "Base year = 1995 (100) | Red shaded areas indicate Brazil recessions",
    x        = "Year",
    y        = "Index (1995 = 100)",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



#no scientific notation
options(scipen = 999)

brazil = read_csv("brazil_all.csv")

brazil = brazil %>% 
  mutate(gdp_100bil = gdp/100000000000) 

brazilL = brazil %>% 
  select(-gdp) %>% 
  pivot_longer(cols = c(gdp_100bil,unemploy), 
               names_to = "econ", values_to = "rate")

#this looks better when GDP is in 100s of billions
brazilL %>% 
  ggplot(aes(year, rate, col = econ)) +
  geom_line()

##############

# Unemployment Rate vs. Total Degrees
brazil %>%
  group_by(year, unemploy) %>%
  summarise(Total = sum(total_conc), .groups = "drop") %>%
  
  ggplot(aes(x = unemploy, y = Total)) +
  
  geom_point(aes(color = year), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  
  scale_color_viridis_c() +
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    title    = "Unemployment Rate vs. Total Bachelor's Degrees Awarded",
    subtitle = "Each point represents one year",
    x        = "Unemployment Rate (%)",
    y        = "Total Degrees Awarded",
    color    = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#GDP vs. Degree Counts by Major
brazil %>%
  ggplot(aes(x = gdp_trils, y = total_conc, color = area)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  
  facet_wrap(~ area, scales = "free_y") +
  
  labs(
    title    = "GDP vs. Bachelor's Degrees Awarded by Major",
    subtitle = "Each point represents one year",
    x        = "GDP (USD trillions)",
    y        = "Total Degrees Awarded",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold")
  )

#what is going on with those arts & humanities outliers?

brazil %>% 
  filter(area == "Arts & humanities") %>% 
  filter(year>1997) %>% 
  ggplot(aes(year,total_conc,col = area))+
  geom_point()

