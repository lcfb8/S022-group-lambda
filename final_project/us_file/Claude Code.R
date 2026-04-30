# ============================================================
# FULL PIPELINE:
# Step 1g: Detrend economic variables
# Step 1:  Lasso → lag selection
# Step 2:  Fractional Logit → inference (95% CIs, no p-values)
# Step 3:  Block Bootstrap → final CIs
# ============================================================


# ── 0. PACKAGES ──────────────────────────────────────────────
packages <- c("tidyverse", "glmnet", "lmtest", "sandwich",
              "modelsummary", "ggplot2", "patchwork", "broom")

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)


# ── 1. LOAD & PREPARE DATA ───────────────────────────────────
df_raw <- read.csv("major_ALL.csv", stringsAsFactors = FALSE)

# 1a. Pull grand total for share calculation
grand_total <- df_raw %>%
  filter(BACHELORS == "Grand total") %>%
  select(YEAR, grand_total = Total)

# 1b. Individual majors only
df <- df_raw %>%
  filter(BACHELORS != "Grand total") %>%
  left_join(grand_total, by = "YEAR") %>%
  rename(year        = YEAR,
         major       = BACHELORS,
         total       = Total,
         total_men   = Total_Men,
         total_women = Total_Women)

# 1c. Major share (bounded [0,1] — our outcome)
df <- df %>%
  mutate(major_share = total / grand_total)

# 1d. GDP growth rate (removes non-stationarity in nominal GDP)
df <- df %>%
  arrange(major, year) %>%
  group_by(major) %>%
  mutate(gdp_growth = (gdp - lag(gdp)) / lag(gdp) * 100) %>%
  ungroup()

# 1e. Create raw lags 1–4 for both predictors + time trend
df <- df %>%
  arrange(major, year) %>%
  group_by(major) %>%
  mutate(
    unemp_lag1 = lag(unemploy,   1),
    unemp_lag2 = lag(unemploy,   2),
    unemp_lag3 = lag(unemploy,   3),
    unemp_lag4 = lag(unemploy,   4),
    gdpg_lag1  = lag(gdp_growth, 1),
    gdpg_lag2  = lag(gdp_growth, 2),
    gdpg_lag3  = lag(gdp_growth, 3),
    gdpg_lag4  = lag(gdp_growth, 4)
  ) %>%
  ungroup() %>%
  mutate(time_trend = year - min(year))

# 1f. Drop NAs caused by lagging
df_clean <- df %>% na.omit()

cat("✔ Rows after cleaning:", nrow(df_clean),
    "| Majors:", n_distinct(df_clean$major),
    "| Years:",  n_distinct(df_clean$year), "\n")


# ──────────────────────────────────────────────────────────────
# 1g. DETREND ECONOMIC VARIABLES
#     Removes linear trend from unemployment & GDP growth so
#     only cyclical deviations (recessions/booms) remain.
#     Resolves near-perfect collinearity with time_trend.
# ──────────────────────────────────────────────────────────────

# One row per year for macro variables
macro_yearly <- df_clean %>%
  select(year, unemploy, gdp_growth, time_trend) %>%
  distinct() %>%
  arrange(year)

# Regress each on time_trend — residuals = cyclical component
unemp_trend_model <- lm(unemploy   ~ time_trend, data = macro_yearly)
gdpg_trend_model  <- lm(gdp_growth ~ time_trend, data = macro_yearly)

macro_yearly <- macro_yearly %>%
  mutate(
    unemploy_cycle = residuals(unemp_trend_model),
    gdpg_cycle     = residuals(gdpg_trend_model)
  )

# Visualise raw vs. detrended unemployment
recessions <- tibble(start = c(2001, 2007, 2020),
                     end   = c(2002, 2009, 2021))

macro_long <- macro_yearly %>%
  select(year, unemploy, unemploy_cycle) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         unemploy       = "Raw Unemployment",
                         unemploy_cycle = "Cyclical Component (detrended)"))

p_detrend <- ggplot(macro_long, aes(x = year, y = value, colour = series)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.3, inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c("Raw Unemployment"               = "grey60",
                                 "Cyclical Component (detrended)" = "steelblue")) +
  labs(title    = "Unemployment: Raw vs. Detrended Cyclical Component",
       subtitle = "Shaded = recession periods | Cyclical component used in model",
       x = "Year", y = "%", colour = NULL) +
  theme_minimal(base_size = 13)

print(p_detrend)

# Merge cyclical variables back into panel
df_clean <- df_clean %>%
  left_join(macro_yearly %>% select(year, unemploy_cycle, gdpg_cycle),
            by = "year")

# Create lags of cyclical variables
df_clean <- df_clean %>%
  arrange(major, year) %>%
  group_by(major) %>%
  mutate(
    ucyc_lag1 = lag(unemploy_cycle, 1),
    ucyc_lag2 = lag(unemploy_cycle, 2),
    ucyc_lag3 = lag(unemploy_cycle, 3),
    ucyc_lag4 = lag(unemploy_cycle, 4),
    gcyc_lag1 = lag(gdpg_cycle,     1),
    gcyc_lag2 = lag(gdpg_cycle,     2),
    gcyc_lag3 = lag(gdpg_cycle,     3),
    gcyc_lag4 = lag(gdpg_cycle,     4)
  ) %>%
  ungroup() %>%
  na.omit()

# Confirm collinearity is resolved — should be ~0
cor_check <- df_clean %>%
  select(time_trend, ucyc_lag3, gcyc_lag3) %>%
  distinct() %>%
  cor()

cat("\n── Correlation with time_trend (should be ~0) ──────────\n")
print(round(cor_check, 3))

cat("\n✔ Rows after detrending:", nrow(df_clean),
    "| Majors:", n_distinct(df_clean$major),
    "| Years:",  n_distinct(df_clean$year), "\n")


# ============================================================
# STEP 1: LASSO FOR LAG SELECTION
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 1: LASSO LAG SELECTION\n")
cat("════════════════════════════════════════════════════════\n")

# Build model matrix
# Major dummies + time_trend: penalty = 0 (protected, never removed)
# Cyclical lag variables:     penalty = 1 (subject to Lasso shrinkage)
major_dummies <- model.matrix(~ factor(major) + time_trend - 1,
                              data = df_clean)
lag_vars      <- as.matrix(df_clean %>%
                             select(ucyc_lag1, ucyc_lag2,
                                    ucyc_lag3, ucyc_lag4,
                                    gcyc_lag1, gcyc_lag2,
                                    gcyc_lag3, gcyc_lag4))

X_lasso <- cbind(major_dummies, lag_vars)
y_lasso <- df_clean$major_share

n_protected     <- ncol(major_dummies)
n_lag_vars      <- ncol(lag_vars)
penalty_factors <- c(rep(0, n_protected), rep(1, n_lag_vars))

# Time-series CV folds — respects time ordering, no future data leakage
years_clean <- sort(unique(df_clean$year))
n_years     <- length(years_clean)
min_train   <- floor(n_years * 0.6)

cv_folds <- lapply(min_train:(n_years - 1), function(k) {
  list(
    train = which(df_clean$year %in% years_clean[1:k]),
    test  = which(df_clean$year %in% years_clean[k + 1])
  )
})

cat("✔ Time-series CV:", length(cv_folds), "folds\n",
    " Training window grows from", min_train, "to", n_years - 1, "years\n")

# CV over lambda grid to find optimal shrinkage
lambda_grid <- 10^seq(-4, 0, length.out = 100)

cv_mse <- sapply(lambda_grid, function(lam) {
  mean(sapply(cv_folds, function(fold) {
    fit  <- glmnet(X_lasso[fold$train, ], y_lasso[fold$train],
                   alpha          = 1,
                   lambda         = lam,
                   penalty.factor = penalty_factors,
                   standardize    = TRUE)
    pred <- predict(fit, newx = X_lasso[fold$test, ], s = lam)
    mean((y_lasso[fold$test] - pred)^2)
  }))
})

best_lambda <- lambda_grid[which.min(cv_mse)]
cat("✔ Best lambda (min CV-MSE):", round(best_lambda, 6), "\n")

# Fit final Lasso at best lambda
lasso_final <- glmnet(X_lasso, y_lasso,
                      alpha          = 1,
                      lambda         = best_lambda,
                      penalty.factor = penalty_factors,
                      standardize    = TRUE)

# Extract lag coefficients only
lasso_coefs <- coef(lasso_final)
lag_names   <- rownames(lasso_coefs)[
  grepl("ucyc_lag|gcyc_lag", rownames(lasso_coefs))
]

lasso_lag_coefs <- data.frame(
  variable    = lag_names,
  coefficient = as.numeric(lasso_coefs[lag_names, ])
) %>% mutate(retained = coefficient != 0)

cat("\n── Lasso Lag Coefficients ──────────────────────────────\n")
print(lasso_lag_coefs)

# Select best lag per predictor type
surviving_lags <- lasso_lag_coefs %>% filter(retained)

best_unemp_lag <- surviving_lags %>%
  filter(grepl("ucyc", variable)) %>%
  slice_max(abs(coefficient), n = 1) %>%
  pull(variable) %>% str_extract("[0-9]") %>% as.integer()

best_gdpg_lag <- surviving_lags %>%
  filter(grepl("gcyc", variable)) %>%
  slice_max(abs(coefficient), n = 1) %>%
  pull(variable) %>% str_extract("[0-9]") %>% as.integer()

# Fallback to lag 3 if nothing survives (theoretical prior)
if (length(best_unemp_lag) == 0) {
  best_unemp_lag <- 3
  cat("⚠ No unemployment lag survived — defaulting to lag 3\n")
}
if (length(best_gdpg_lag) == 0) {
  best_gdpg_lag <- 3
  cat("⚠ No GDP growth lag survived — defaulting to lag 3\n")
}

cat("\n✔ Selected lags:\n")
cat("  Unemployment cycle: lag", best_unemp_lag, "\n")
cat("  GDP growth cycle:   lag", best_gdpg_lag,  "\n")

# Lasso coefficient path plot (lag variables only)
lasso_path <- glmnet(X_lasso, y_lasso,
                     alpha          = 1,
                     penalty.factor = penalty_factors,
                     standardize    = TRUE)

lag_col_idx <- which(colnames(X_lasso) %in%
                       c("ucyc_lag1","ucyc_lag2","ucyc_lag3","ucyc_lag4",
                         "gcyc_lag1","gcyc_lag2","gcyc_lag3","gcyc_lag4"))

path_df <- as.data.frame(t(as.matrix(lasso_path$beta[lag_col_idx, ]))) %>%
  mutate(log_lambda = log(lasso_path$lambda)) %>%
  pivot_longer(-log_lambda, names_to = "variable", values_to = "coefficient")

p_lasso <- ggplot(path_df,
                  aes(x = log_lambda, y = coefficient, colour = variable)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = log(best_lambda),
             linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("text", x = log(best_lambda),
           y = max(path_df$coefficient) * 0.9,
           label = "Selected λ", hjust = -0.1, size = 3.5) +
  labs(title    = "Lasso Coefficient Path — Cyclical Lag Variables Only",
       subtitle = "Major fixed effects & time trend protected from penalisation",
       x        = "log(λ)  [← less shrinkage    more shrinkage →]",
       y        = "Coefficient",
       colour   = "Variable") +
  theme_minimal(base_size = 13)

print(p_lasso)


# ============================================================
# STEP 2: FRACTIONAL LOGIT — PRIMARY INFERENCE MODEL
# Uses HC3 robust SEs (not clustered — only 8 majors)
# Reports 95% CIs throughout instead of p-values
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 2: FRACTIONAL LOGIT (PAPKE-WOOLDRIDGE)\n")
cat("════════════════════════════════════════════════════════\n")

unemp_selected <- paste0("ucyc_lag", best_unemp_lag)
gdpg_selected  <- paste0("gcyc_lag", best_gdpg_lag)

flogit_model <- glm(
  as.formula(paste("major_share ~",
                   unemp_selected, "+", gdpg_selected,
                   "+ time_trend + factor(major)")),
  data   = df_clean,
  family = quasibinomial(link = "logit")
)

# HC3 robust SEs (observation-level; clustering not feasible with 8 groups)
robust_se <- coeftest(flogit_model,
                      vcov = vcovHC(flogit_model, type = "HC3"))

# 95% CIs from robust SEs
robust_ci <- coefci(flogit_model,
                    vcov  = vcovHC(flogit_model, type = "HC3"),
                    level = 0.95)

# Clean table: estimates + CIs for key predictors only (exclude major dummies)
ci_table <- as.data.frame(robust_ci) %>%
  rownames_to_column("term") %>%
  rename(CI_lower = `2.5 %`, CI_upper = `97.5 %`) %>%
  mutate(estimate = coef(flogit_model)) %>%
  filter(term %in% c(unemp_selected, gdpg_selected, "time_trend")) %>%
  mutate(
    significant = ifelse(CI_lower > 0 | CI_upper < 0, "✔ Yes", "✘ No"),
    across(c(estimate, CI_lower, CI_upper), ~round(.x, 6))
  ) %>%
  select(term, estimate, CI_lower, CI_upper, significant)

cat("\n── Model Estimates with 95% Confidence Intervals ───────\n")
print(ci_table)


# ── Average Marginal Effects (AMEs) with 95% CIs ─────────────
# AME = beta * mean(p_hat * (1 - p_hat))
# CI  = CI_bound * mean(p_hat * (1 - p_hat))  [delta method scaling]

p_hat       <- predict(flogit_model, type = "response")
mean_factor <- mean(p_hat * (1 - p_hat), na.rm = TRUE)
ame_fn      <- function(beta) beta * mean_factor

ame_ci_table <- as.data.frame(robust_ci) %>%
  rownames_to_column("term") %>%
  rename(CI_lower = `2.5 %`, CI_upper = `97.5 %`) %>%
  filter(term %in% c(unemp_selected, gdpg_selected)) %>%
  mutate(
    AME      = coef(flogit_model)[term] * mean_factor * 100,
    CI_lower = CI_lower * mean_factor * 100,
    CI_upper = CI_upper * mean_factor * 100,
    sig      = ifelse(CI_lower > 0 | CI_upper < 0,
                      "✔ Significant", "✘ Not significant"),
    term     = recode(term,
                      !!unemp_selected := paste0("Unemployment cycle (lag ",
                                                 best_unemp_lag, ")"),
                      !!gdpg_selected  := paste0("GDP growth cycle (lag ",
                                                 best_gdpg_lag,  ")"))
  ) %>%
  mutate(across(c(AME, CI_lower, CI_upper), ~round(.x, 4))) %>%
  select(term, AME, CI_lower, CI_upper, sig)

cat("\n── Average Marginal Effects with 95% CIs (pp) ───────────\n")
print(ame_ci_table)


# ── Major-Specific Fractional Logit with 95% CIs ─────────────
major_results <- map_dfr(unique(df_clean$major), function(m) {
  sub  <- df_clean %>% filter(major == m)
  form <- as.formula(paste("major_share ~",
                           unemp_selected, "+",
                           gdpg_selected, "+ time_trend"))
  tryCatch({
    fit  <- glm(form, data = sub, family = quasibinomial(link = "logit"))
    p_h  <- predict(fit, type = "response")
    mf   <- mean(p_h * (1 - p_h), na.rm = TRUE)
    
    # 95% CIs using HC3 robust SEs
    ci_m <- coefci(fit,
                   vcov  = vcovHC(fit, type = "HC3"),
                   level = 0.95)
    
    tibble(
      major    = m,
      term     = c("Unemployment", "GDP Growth"),
      AME      = c(coef(fit)[unemp_selected] * mf * 100,
                   coef(fit)[gdpg_selected]  * mf * 100),
      CI_lower = c(ci_m[unemp_selected, "2.5 %"]  * mf * 100,
                   ci_m[gdpg_selected,  "2.5 %"]  * mf * 100),
      CI_upper = c(ci_m[unemp_selected, "97.5 %"] * mf * 100,
                   ci_m[gdpg_selected,  "97.5 %"] * mf * 100),
      sig      = ifelse(CI_lower > 0 | CI_upper < 0, "✔ Yes", "✘ No")
    )
  }, error = function(e) NULL)
})

cat("\n── Major-Specific AMEs with 95% CIs ─────────────────────\n")
print(major_results %>%
        mutate(across(c(AME, CI_lower, CI_upper), ~round(.x, 4))) %>%
        arrange(term, AME))


# ============================================================
# STEP 3: BLOCK BOOTSTRAP FOR FINAL CONFIDENCE INTERVALS
# ============================================================
# Resamples blocks of consecutive years (block size = 4)
# to respect temporal autocorrelation with small N (~25 years)
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 3: BLOCK BOOTSTRAP (B = 1000 iterations)\n")
cat("════════════════════════════════════════════════════════\n")

set.seed(42)
B          <- 1000
block_size <- 4
years_vec  <- sort(unique(df_clean$year))
n_years    <- length(years_vec)

# Draw one block-bootstrapped sample
block_bootstrap_sample <- function(df, years, block_sz) {
  start_positions <- 1:(length(years) - block_sz + 1)
  n_blocks_needed <- ceiling(length(years) / block_sz)
  selected_starts <- sample(start_positions,
                            size    = n_blocks_needed,
                            replace = TRUE)
  boot_years <- unlist(lapply(selected_starts, function(s) {
    years[s:min(s + block_sz - 1, length(years))]
  }))[1:length(years)]
  
  map_dfr(seq_along(boot_years), function(i) {
    df %>%
      filter(year == boot_years[i]) %>%
      mutate(boot_year = years[i])
  })
}

# Bootstrap loop — store AMEs from each iteration
boot_results <- map_dfr(1:B, function(b) {
  tryCatch({
    boot_data <- block_bootstrap_sample(df_clean, years_vec, block_size)
    
    fit <- glm(
      as.formula(paste("major_share ~",
                       unemp_selected, "+", gdpg_selected,
                       "+ time_trend + factor(major)")),
      data   = boot_data,
      family = quasibinomial(link = "logit")
    )
    
    p_h <- predict(fit, type = "response")
    mf  <- mean(p_h * (1 - p_h), na.rm = TRUE)
    
    tibble(
      boot      = b,
      ame_unemp = coef(fit)[unemp_selected] * mf * 100,
      ame_gdpg  = coef(fit)[gdpg_selected]  * mf * 100
    )
  }, error = function(e) NULL)
}, .progress = TRUE)

cat("\n✔ Successful bootstrap iterations:", nrow(boot_results), "of", B, "\n")

# Percentile 95% CIs from bootstrap distribution
ci_unemp <- quantile(boot_results$ame_unemp, c(0.025, 0.975), na.rm = TRUE)
ci_gdpg  <- quantile(boot_results$ame_gdpg,  c(0.025, 0.975), na.rm = TRUE)

# Point estimates from main model
ame_unemp <- coef(flogit_model)[unemp_selected] * mean_factor * 100
ame_gdpg  <- coef(flogit_model)[gdpg_selected]  * mean_factor * 100

# Final results table
final_table <- tibble(
  Predictor = c(paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")),
  AME       = round(c(ame_unemp, ame_gdpg), 4),
  CI_lower  = round(c(ci_unemp[1], ci_gdpg[1]), 4),
  CI_upper  = round(c(ci_unemp[2], ci_gdpg[2]), 4),
  Sig       = c(
    ifelse(ci_unemp[1] > 0 | ci_unemp[2] < 0, "✔ Significant", "✘ Not significant"),
    ifelse(ci_gdpg[1]  > 0 | ci_gdpg[2]  < 0, "✔ Significant", "✘ Not significant")
  )
)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FINAL RESULTS (AME in percentage points)\n")
cat("  95% CIs from block bootstrap\n")
cat("══════════════════════════════════════════════════════════\n")
print(final_table)


# ============================================================
# VISUALISATIONS
# ============================================================

# ── Plot 1: Detrended unemployment (already printed above) ────


# ── Plot 2: Lasso coefficient path (already printed above) ───


# ── Plot 3: Bootstrap distribution of AMEs ───────────────────
boot_long <- boot_results %>%
  pivot_longer(c(ame_unemp, ame_gdpg),
               names_to  = "predictor",
               values_to = "ame") %>%
  mutate(predictor = recode(predictor,
                            ame_unemp = paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                            ame_gdpg  = paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")))

p_boot <- ggplot(boot_long, aes(x = ame, fill = predictor)) +
  geom_histogram(bins = 40, colour = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title    = "Block Bootstrap Distribution of Average Marginal Effects",
       subtitle = paste0("B = ", B, " iterations | Block size = ",
                         block_size, " years"),
       x = "AME (percentage points)", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_boot)


# ── Plot 4: Major-specific AMEs with 95% CI whiskers ─────────
plot_major_ame <- function(term_name, point_colour) {
  major_results %>%
    filter(term == term_name) %>%
    ggplot(aes(x      = reorder(major, AME),
               y      = AME,
               ymin   = CI_lower,
               ymax   = CI_upper,
               colour = sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(size = 0.8, linewidth = 0.8) +
    coord_flip() +
    scale_colour_manual(values = c("✔ Yes" = point_colour,
                                   "✘ No"  = "grey65")) +
    labs(title    = paste("Effect of", term_name, "on Major Share"),
         subtitle = "Point = AME | Whiskers = 95% CI | Colour = CI excludes zero",
         x        = NULL,
         y        = "Average Marginal Effect (percentage points)",
         colour   = "CI excludes 0") +
    theme_minimal(base_size = 12)
}

print(plot_major_ame("Unemployment", "steelblue") /
        plot_major_ame("GDP Growth",   "darkgreen"))


# ── Plot 5: Final summary — AME + block bootstrap 95% CIs ────
p_final <- final_table %>%
  ggplot(aes(x      = Predictor,
             y      = AME,
             ymin   = CI_lower,
             ymax   = CI_upper,
             colour = Sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(size = 1.2, linewidth = 1) +
  coord_flip() +
  scale_colour_manual(values = c("✔ Significant"     = "steelblue",
                                 "✘ Not significant" = "grey60")) +
  labs(title    = "Final Model: AMEs with 95% Block Bootstrap CIs",
       subtitle = paste0("Fractional Logit | Lasso-selected lags | ",
                         "Outcome: major share of total graduates"),
       x        = NULL,
       y        = "Average Marginal Effect (percentage points)",
       colour   = NULL) +
  theme_minimal(base_size = 13)

print(p_final)