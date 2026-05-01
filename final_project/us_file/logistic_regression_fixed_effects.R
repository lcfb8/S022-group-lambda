# ============================================================
# Major choice over the business cycle (simple version)
#
# One OLS per field:
#   share(t) = a + b*unemp(t-L) + c*gdp_growth(t-L) + d*t + error
#
#   b > 0 : field grows when unemployment was high L years ago
#   c > 0 : field grows when GDP growth was high L years ago
#   d     : linear trend (absorbs long-run drift in the field's share)
#
# Running separate regressions per field is equivalent to a pooled
# model with field fixed effects + field-specific slopes on every term.
# ============================================================

# 0) PACKAGES ---------------------------------------------------
library(tidyverse)
library(broom)        # tidy() turns lm() output into a plain data frame

# 1) SETTINGS ---------------------------------------------------
DATA_PATH <- "major_ALL.csv"
LAG       <- 4        # years between macro conditions and graduation
# (students affected by the economy when they enroll)

# 2) LOAD DATA --------------------------------------------------
raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
names(raw) <- tolower(names(raw))

# "Grand total" row holds annual totals + macro variables
totals <- raw %>%
  filter(bachelors == "Grand total") %>%
  transmute(
    year    = as.integer(year),
    tot_all = as.numeric(total),
    tot_men = as.numeric(total_men),
    tot_wom = as.numeric(total_women),
    unemp   = as.numeric(unemploy),
    gdp     = as.numeric(gdp)
  ) %>%
  arrange(year)

# All other rows are individual fields
fields <- raw %>%
  filter(bachelors != "Grand total") %>%
  transmute(
    year  = as.integer(year),
    field = bachelors,
    n_all = as.numeric(total),
    n_men = as.numeric(total_men),
    n_wom = as.numeric(total_women)
  )

# 3) BUILD PANEL: shares + lagged macro -------------------------

macro <- totals %>%
  mutate(
    gdp_growth = 100 * (log(gdp) - log(lag(gdp))),   # % change in real GDP
    unemp_lag  = lag(unemp,      LAG),                # unemployment at enrollment
    gdpg_lag   = lag(gdp_growth, LAG)                 # GDP growth at enrollment
  ) %>%
  dplyr::select(year, tot_all, tot_men, tot_wom, unemp_lag, gdpg_lag)

panel <- fields %>%
  left_join(macro, by = "year") %>%
  filter(!is.na(unemp_lag), !is.na(gdpg_lag)) %>%    # drop years lost to lag
  mutate(
    share_all = n_all / tot_all,
    share_men = n_men / tot_men,
    share_wom = n_wom / tot_wom,
    t = year - min(year)    # time trend starting at 0
  )

cat("Estimation period:", min(panel$year), "to", max(panel$year), "\n")
cat("Number of fields :", n_distinct(panel$field), "\n")

# Quick sanity check — shares should sum to 1.000 each year
share_sums <- panel %>%
  group_by(year) %>%
  summarise(sum_all = round(sum(share_all, na.rm = TRUE), 3), .groups = "drop")
cat("\nShare sums per year (should all equal 1.000):\n")
print(share_sums)

# 4) RUN ONE REGRESSION PER FIELD -------------------------------
# conf.int = TRUE uses the t-distribution (correct for ~20 obs per field)

run_models <- function(share_col) {
  panel %>%
    mutate(y = .data[[share_col]]) %>%
    group_by(field) %>%
    group_modify(~
                   tidy(lm(y ~ unemp_lag + gdpg_lag + t, data = .x), conf.int = TRUE)
    ) %>%
    ungroup() %>%
    filter(term %in% c("unemp_lag", "gdpg_lag")) %>%
    dplyr::select(field, term, estimate, conf.low, conf.high, p.value)
}

results_all <- run_models("share_all")
results_men <- run_models("share_men")
results_wom <- run_models("share_wom")

# Print unemployment sensitivities, ranked
cat("\nOverall — unemployment sensitivity (ranked):\n")
results_all %>%
  filter(term == "unemp_lag") %>%
  arrange(desc(estimate)) %>%
  print(n = Inf)

sum(results_all$estimate[results_all$term == "unemp_lag"])
# ≈ 0.000  ✓

# 5) PLOT -------------------------------------------------------

coef_plot <- function(results, var, title) {
  x_label <- if (var == "unemp_lag") "unemployment rate" else "GDP growth rate"
  results %>%
    filter(term == var) %>%
    ggplot(aes(
      x    = reorder(field, estimate),
      y    = estimate,
      ymin = conf.low,
      ymax = conf.high
    )) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange() +
    coord_flip() +
    labs(
      title    = title,
      subtitle = paste0("OLS per field | linear trend | lag = ", LAG, " yrs | 95% CI"),
      x        = NULL,
      y        = paste0("Coefficient on lagged ", x_label, " (Δ field share)")
    ) +
    theme_minimal(base_size = 11)
}

print(coef_plot(results_all, "unemp_lag",
                "Fields that gain share when unemployment was high (overall)"))

print(coef_plot(results_all, "gdpg_lag",
                "Fields that gain share when GDP growth was high (overall)"))

# 6) SAVE -------------------------------------------------------
write.csv(results_all, "results_all.csv", row.names = FALSE)
write.csv(results_men, "results_men.csv", row.names = FALSE)
write.csv(results_wom, "results_wom.csv", row.names = FALSE)