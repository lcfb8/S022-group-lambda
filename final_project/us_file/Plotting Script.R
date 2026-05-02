rm(list = ls())

# Loading packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

major_df <- read.csv("C:/Users/dylan/Documents/GitHub/S022-group-lambda/final_project/us_file/us_major_all.csv")
major_trill <- read.csv("C:/Users/dylan/Documents/GitHub/S022-group-lambda/final_project/us_file/us_major_trill.csv")

# Color blind friendly palette
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000", "#999999")

# Recession reference data
recessions <- data.frame(
  start = c(2001, 2007, 2020),
  end   = c(2001, 2009, 2020),
  label = c("2001\nDot-com", "2007-09\nGreat Recession", "2020\nCOVID")
)

# ── Chart 1: Total Degrees by Major ───────────────────────────────────────────
major_trill %>%
  filter(area != "Grand total",
         econ == "gdp_trils") %>%
  ggplot(aes(x = year, y = Total, color = area)) +
  scale_color_brewer(palette = "Set2") +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  geom_text(data = recessions,
            aes(x = (start + end) / 2, y = Inf, label = label),
            inherit.aes = FALSE,
            vjust = 1.5, size = 2.5, color = "red") +
  geom_line(linewidth = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Bachelor's Degrees Awarded by Major Category",
    x     = "Year",
    y     = "Total Degrees",
    color = "Major Category"
  ) +
  theme_minimal()

# ── Chart 2: Degrees by Major and Gender ──────────────────────────────────────
gender_plot <- major_trill %>%
  filter(econ == "gdp_trils") %>%
  pivot_longer(
    cols      = c(Total_Men, Total_Women),
    names_to  = "Gender",
    values_to = "Count"
  ) %>%
  mutate(Gender    = if_else(Gender == "Total_Men", "Men", "Women"),
         area = factor(area, 
                            levels = c("Arts & Humanities",
                                       "Business & Management",
                                       "Education",
                                       "Engineering",
                                       "Health & Medical",
                                       "Natural Sciences",
                                       "Social & Behavioral Sciences",
                                       "Other/Unknown",
                                       "Grand total"))) %>%
  ggplot(aes(x = year, y = Count, color = Gender)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Men" = "#2166ac", "Women" = "#d6604d")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ area, scales = "free_y") +
  labs(
    title    = "Bachelor's Degrees by Major and Gender in the U.S.",
    subtitle = "Red shaded areas indicate US recessions",
    x        = "Year",
    y        = "Degrees Awarded",
    color    = "Gender",
    caption = "Note: The US recessions indicated are: 2001: The dot-com recession, 2007-09: The Great Recession, 2020: COVID"
  ) +
  theme_minimal() +
  theme(strip.text       = element_text(face = "bold"),
        legend.position  = "bottom",
        plot.caption     = element_text(hjust = 0,
                                        size  = 12,
                                        color = "gray40",
                                        face  = "italic"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
  )
ggsave("gender_plot.png", gender_plot, bg = "transparent")

# ── Chart 3: % Women Over Time ─────────────────────────────────────────────────
major_trill %>%
  filter(econ == "gdp_trils",
         area != "Grand total") %>%
  mutate(pct_women = Total_Women / Total * 100,
         area = factor(area, 
                            levels = c("Arts & Humanities",
                                       "Business & Management",
                                       "Education",
                                       "Engineering",
                                       "Health & Medical",
                                       "Natural Sciences",
                                       "Social & Behavioral Sciences",
                                       "Other/Unknown"))) %>%
  ggplot(aes(x = year, y = pct_women, color = area)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(linewidth = 1.25) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 90)) +
  labs(
    title    = "Percentage of Women Earning Bachelor's Degrees by Major",
    subtitle = "Dashed line = 50% | Red shaded areas indicate US recessions",
    x        = "Year",
    y        = "% Women",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── Chart 4: Gender Ratio Heatmap ─────────────────────────────────────────────
major_trill %>%
  filter(econ == "gdp_trils",
         area != "Grand total") %>%
  mutate(pct_women = Total_Women / Total * 100) %>%
  ggplot(aes(x = year, y = area, fill = pct_women)) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#d6604d",
    midpoint = 50,
    labels   = scales::percent_format(scale = 1)
  ) +
  labs(
    title    = "Gender Ratio Heatmap by Major and Year",
    subtitle = "Blue = More Men | Red = More Women | White = 50/50",
    x        = "Year",
    y        = "",
    fill     = "% Women"
  ) +
  theme_minimal() +
  theme(axis.text.y      = element_text(face = "bold"),
        legend.position  = "bottom",
        legend.key.width = unit(2, "cm"))

# ── Chart 5: Indexed Growth ────────────────────────────────────────────────────
major_trill %>%
  filter(econ == "gdp_trils",
         area != "Grand total") %>%
  group_by(area) %>%
  mutate(index = Total / Total[year == min(year)] * 100) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = index, color = area)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "red", alpha = 0.15) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Indexed Growth of Bachelor's Degrees by Major",
    subtitle = "Base year = 1995 (100) | Red shaded areas indicate US recessions",
    x        = "Year",
    y        = "Index (1995 = 100)",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── Chart 6: Unemployment Rate vs. Total Degrees ──────────────────────────────
major_trill %>%
  filter(econ == "unemploy",
         area != "Grand total") %>%
  group_by(year, rate) %>%
  summarise(Total = sum(Total), .groups = "drop") %>%
  ggplot(aes(x = rate, y = Total)) +
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

# ── Chart 7: GDP vs. Degree Counts by Major ───────────────────────────────────
major_trill %>%
  filter(econ == "gdp_trils",
         area != "Grand total") %>%
  ggplot(aes(x = rate, y = Total, color = area)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~ area, scales = "free_y") +
  labs(
    title    = "GDP vs. Bachelor's Degrees Awarded by Major",
    subtitle = "Each point represents one year",
    x        = "GDP (USD Trillions)",
    y        = "Total Degrees Awarded",
    color    = "Major Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text      = element_text(face = "bold"))
