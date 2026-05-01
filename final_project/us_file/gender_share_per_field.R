# ============================================================
# Women's share of each field over the business cycle
#
# TWO complementary outcomes:
#
# (A) CHOICE SHARE (across fields — what your original code had)
#     share_wom(f,t) = women in field f / all women graduates
#     "Do women shift toward/away from field f ?"
#
# (B) WITHIN-FIELD FEMALE SHARE  ← NEW FOCUS
#     prop_wom(f,t)  = women in field f / all graduates in field f
#     "Does field f become more/less female?"
#
# Same model for both:
#   outcome(f,t) = a + b*unemp(t-L) + c*gdp_growth(t-L) + d*t + e
#
#   b > 0 : field becomes MORE female / women flow IN when
#           unemployment was high L years ago
#   b < 0 : field becomes LESS female / women flow OUT
# ============================================================


# 0) PACKAGES ---------------------------------------------------
library(tidyverse)
library(broom)


# 1) SETTINGS ---------------------------------------------------
DATA_PATH <- "major_ALL.csv"
LAG       <- 8          # enrollment-to-graduation lag (years)


# 2) LOAD DATA --------------------------------------------------
raw        <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
names(raw) <- tolower(names(raw))

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

fields <- raw %>%
  filter(bachelors != "Grand total") %>%
  transmute(
    year  = as.integer(year),
    field = bachelors,
    n_all = as.numeric(total),
    n_men = as.numeric(total_men),
    n_wom = as.numeric(total_women)
  )


# 3) BUILD PANEL ------------------------------------------------
macro <- totals %>%
  mutate(
    gdp_growth = 100 * (log(gdp) - log(lag(gdp))),
    unemp_lag  = lag(unemp,      LAG),
    gdpg_lag   = lag(gdp_growth, LAG)
  ) %>%
  dplyr::select(year, tot_all, tot_men, tot_wom, unemp_lag, gdpg_lag)

panel <- fields %>%
  left_join(macro, by = "year") %>%
  filter(!is.na(unemp_lag), !is.na(gdpg_lag)) %>%
  mutate(
    # (A) Choice shares — who graduates WHERE relative to own gender total
    share_wom = n_wom / tot_wom,   # women's choice share
    share_men = n_men / tot_men,   # men's choice share
    share_all = n_all / tot_all,   # overall choice share
    
    # (B) Within-field female proportion  ← main new outcome
    prop_wom  = n_wom / (n_wom + n_men),
    
    # (C) Gender gap in choice share (women minus men)
    # positive = women are over-represented relative to their numbers
    gap_share = share_wom - share_men,
    
    t = year - min(year)           # linear time trend (starts at 0)
  )

cat("Estimation period:", min(panel$year), "–", max(panel$year), "\n")
cat("Number of fields :", n_distinct(panel$field), "\n")


# 4) REGRESSION FUNCTIONS ---------------------------------------

# (A) OLS per field — same as original code
#     Works for any share-type outcome (columns as strings)
run_ols <- function(outcome_col) {
  panel %>%
    mutate(y = .data[[outcome_col]]) %>%
    group_by(field) %>%
    group_modify(~
                   tidy(lm(y ~ unemp_lag + gdpg_lag + t, data = .x),
                        conf.int = TRUE)
    ) %>%
    ungroup() %>%
    filter(term %in% c("unemp_lag", "gdpg_lag")) %>%
    dplyr::select(field, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(outcome = outcome_col)
}

# (B) Quasi-binomial GLM per field — RECOMMENDED for prop_wom
#     Uses raw counts → automatically weights large fields more heavily
#     Coefficients are on the log-odds scale; we report AMEs below
run_qbinom <- function() {
  panel %>%
    group_by(field) %>%
    group_modify(~{
      fit <- glm(
        cbind(n_wom, n_men) ~ unemp_lag + gdpg_lag + t,
        data   = .x,
        family = quasibinomial(link = "logit")
      )
      tidy(fit, conf.int = TRUE)
    }) %>%
    ungroup() %>%
    filter(term %in% c("unemp_lag", "gdpg_lag")) %>%
    dplyr::select(field, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(outcome = "prop_wom (logit, counts)")
}


# 5) RUN MODELS -------------------------------------------------
res_within_ols   <- run_ols("prop_wom")    # within-field female share (OLS)
res_choice_wom   <- run_ols("share_wom")   # women's choice share (OLS)
res_choice_men   <- run_ols("share_men")   # men's choice share   (OLS)
res_gap          <- run_ols("gap_share")   # choice-share gap     (OLS)
res_within_binom <- run_qbinom()           # within-field (quasi-binomial ✓)


# 6) PRINT RANKED RESULTS ---------------------------------------
cat("\n══════════════════════════════════════════════════════════\n")
cat("  WITHIN-FIELD FEMALE SHARE — unemployment sensitivity\n")
cat("  (OLS | ranked by coefficient)\n")
cat("══════════════════════════════════════════════════════════\n")
res_within_ols %>%
  filter(term == "unemp_lag") %>%
  arrange(desc(estimate)) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x, 5))) %>%
  print(n = Inf)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  WITHIN-FIELD FEMALE SHARE — unemployment sensitivity\n")
cat("  (Quasi-binomial / log-odds | ranked by coefficient)\n")
cat("══════════════════════════════════════════════════════════\n")
res_within_binom %>%
  filter(term == "unemp_lag") %>%
  arrange(desc(estimate)) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x, 5))) %>%
  print(n = Inf)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  WOMEN'S CHOICE SHARE — unemployment sensitivity (ranked)\n")
cat("══════════════════════════════════════════════════════════\n")
res_choice_wom %>%
  filter(term == "unemp_lag") %>%
  arrange(desc(estimate)) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x, 5))) %>%
  print(n = Inf)


# 7) COEFFICIENT PLOTS ------------------------------------------

coef_plot <- function(results, var, title, subtitle = NULL,
                      point_col = "steelblue") {
  x_lab <- if (var == "unemp_lag") "lagged unemployment rate"
  else                   "lagged GDP growth rate"
  sub   <- subtitle %||%
    paste0("OLS per field | lag = ", LAG, " yrs | 95% CI")
  results %>%
    filter(term == var) %>%
    mutate(sig = ifelse(conf.low > 0 | conf.high < 0,
                        "CI excludes 0", "CI includes 0")) %>%
    ggplot(aes(x    = reorder(field, estimate),
               y    = estimate,
               ymin = conf.low,
               ymax = conf.high,
               colour = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(size = 0.7, linewidth = 0.7) +
    coord_flip() +
    scale_colour_manual(
      values = c("CI excludes 0" = point_col, "CI includes 0" = "grey65")
    ) +
    labs(title    = title,
         subtitle = sub,
         x        = NULL,
         y        = paste0("Coefficient on ", x_lab),
         colour   = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}


# ── Plot 1: Within-field female share ~ unemployment (OLS) ─────
p1 <- coef_plot(
  res_within_ols, "unemp_lag",
  title      = "Within-field female share and unemployment",
  subtitle   = paste0("Outcome: women / (women+men) in field | ",
                      "OLS per field | lag = ", LAG, " yrs | 95% CI"),
  point_col  = "steelblue"
)
print(p1)

# ── Plot 2: Within-field female share ~ unemployment (logit) ───
p2 <- coef_plot(
  res_within_binom, "unemp_lag",
  title     = "Within-field female share and unemployment (log-odds)",
  subtitle  = paste0("Outcome: cbind(n_wom, n_men) | ",
                     "Quasi-binomial logit | lag = ", LAG, " yrs | 95% CI"),
  point_col = "darkred"
)
print(p2)

# ── Plot 3: Women's choice share ~ unemployment ─────────────────
p3 <- coef_plot(
  res_choice_wom, "unemp_lag",
  title     = "Women's major-choice share and unemployment",
  subtitle  = paste0("Outcome: women in field / all women graduates | ",
                     "OLS | lag = ", LAG, " yrs | 95% CI"),
  point_col = "darkorange"
)
print(p3)

# ── Plot 4: Side-by-side women vs men choice share ──────────────
combined <- bind_rows(
  res_choice_wom %>% mutate(gender = "Women"),
  res_choice_men %>% mutate(gender = "Men")
)

p4 <- combined %>%
  filter(term == "unemp_lag") %>%
  mutate(sig = ifelse(conf.low > 0 | conf.high < 0,
                      "CI excludes 0", "CI includes 0")) %>%
  ggplot(aes(x      = reorder(field, estimate),
             y      = estimate,
             ymin   = conf.low,
             ymax   = conf.high,
             colour = sig,
             shape  = gender)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 0.7, linewidth = 0.7) +
  coord_flip() +
  scale_colour_manual(
    values = c("CI excludes 0" = "steelblue", "CI includes 0" = "grey65")
  ) +
  labs(title    = "Choice-share response to unemployment — Women vs Men",
       subtitle = paste0("OLS per field | lag = ", LAG, " yrs | 95% CI"),
       x        = NULL,
       y        = "Coefficient on lagged unemployment (Δ field share)",
       colour   = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p4)

# ── Plot 5: Time series of within-field female share ────────────
p5 <- panel %>%
  ggplot(aes(x = year, y = prop_wom, colour = field)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(title    = "Within-field female share over time",
       subtitle = "Dashed line = 50% | one line per field",
       x = "Year", y = "Women / (Women + Men)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

print(p5)

# ── Plot 6: Faceted time series ─────────────────────────────────
p6 <- panel %>%
  ggplot(aes(x = year, y = prop_wom)) +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ field, scales = "free_y") +
  labs(title    = "Within-field female share by major",
       subtitle = "Dashed = 50%  |  y-axis free per panel",
       x = "Year", y = "Women / (Women + Men)") +
  theme_minimal(base_size = 10)

print(p6)


# 8) SAVE -------------------------------------------------------
write.csv(res_within_ols,   "results_within_female_share_OLS.csv",   row.names = FALSE)
write.csv(res_within_binom, "results_within_female_share_logit.csv", row.names = FALSE)
write.csv(res_choice_wom,   "results_women_choice_share.csv",        row.names = FALSE)
write.csv(res_choice_men,   "results_men_choice_share.csv",          row.names = FALSE)
write.csv(res_gap,          "results_gender_gap_choice_share.csv",   row.names = FALSE)

cat("\n✔ All results saved.\n")