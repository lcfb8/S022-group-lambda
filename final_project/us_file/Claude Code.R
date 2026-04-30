# ============================================================
# FULL PIPELINE (CORRECTED):
# Step 1g: Detrend economic variables         ← NEW
# Step 1:  Lasso → lag selection              ← updated variable names
# Step 2:  Fractional Logit → inference       ← updated variable names
# Step 3:  Block Bootstrap → CIs              ← no changes
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

# 1d. GDP growth rate
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
    unemp_lag1 = lag(unemploy,    1),
    unemp_lag2 = lag(unemploy,    2),
    unemp_lag3 = lag(unemploy,    3),
    unemp_lag4 = lag(unemploy,    4),
    gdpg_lag1  = lag(gdp_growth,  1),
    gdpg_lag2  = lag(gdp_growth,  2),
    gdpg_lag3  = lag(gdp_growth,  3),
    gdpg_lag4  = lag(gdp_growth,  4)
  ) %>%
  ungroup() %>%
  mutate(time_trend = year - min(year))

# 1f. Drop NAs caused by lagging
df_clean <- df %>% na.omit()

cat("✔ Rows after cleaning:", nrow(df_clean),
    "| Majors:", n_distinct(df_clean$major),
    "| Years:",  n_distinct(df_clean$year), "\n")


# ──────────────────────────────────────────────────────────────
# 1g. DETREND ECONOMIC VARIABLES (FIX FOR COLLINEARITY)       ←
#     Remove the linear trend from unemployment & GDP growth   ←
#     so only cyclical deviations (recessions/booms) remain.   ←
#     This resolves near-perfect collinearity with time_trend. ←
# ──────────────────────────────────────────────────────────────

# One row per year for macro variables
macro_yearly <- df_clean %>%
  select(year, unemploy, gdp_growth, time_trend) %>%
  distinct() %>%
  arrange(year)

# Regress each on time_trend; residuals = cyclical component
unemp_trend_model <- lm(unemploy   ~ time_trend, data = macro_yearly)
gdpg_trend_model  <- lm(gdp_growth ~ time_trend, data = macro_yearly)

macro_yearly <- macro_yearly %>%
  mutate(
    unemploy_cycle = residuals(unemp_trend_model),
    gdpg_cycle     = residuals(gdpg_trend_model)
  )

# Visualise raw vs. detrended unemployment
macro_long <- macro_yearly %>%
  select(year, unemploy, unemploy_cycle) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         unemploy       = "Raw Unemployment",
                         unemploy_cycle = "Cyclical Component (detrended)"))

recessions <- tibble(start = c(2001, 2007, 2020),
                     end   = c(2002, 2009, 2021))

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

# Confirm collinearity is resolved
cor_check <- df_clean %>%
  select(time_trend, ucyc_lag3, gcyc_lag3) %>%
  distinct() %>%
  cor()

cat("\n── Correlation with time_trend (should be ~0 now) ──────\n")
print(round(cor_check, 3))

cat("✔ Rows after detrending:", nrow(df_clean),
    "| Majors:", n_distinct(df_clean$major),
    "| Years:",  n_distinct(df_clean$year), "\n")


# ============================================================
# STEP 1: LASSO FOR LAG SELECTION
# (same structure as before — now uses ucyc/gcyc variables)
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 1: LASSO LAG SELECTION\n")
cat("════════════════════════════════════════════════════════\n")

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

# Time-series CV folds
years_clean <- sort(unique(df_clean$year))
n_years     <- length(years_clean)
min_train   <- floor(n_years * 0.6)

cv_folds <- lapply(min_train:(n_years - 1), function(k) {
  list(
    train = which(df_clean$year %in% years_clean[1:k]),
    test  = which(df_clean$year %in% years_clean[k + 1])
  )
})

cat("✔ Time-series CV:", length(cv_folds), "folds\n")

# CV over lambda grid
lambda_grid <- 10^seq(-4, 0, length.out = 100)

cv_mse <- sapply(lambda_grid, function(lam) {
  mean(sapply(cv_folds, function(fold) {
    fit  <- glmnet(X_lasso[fold$train, ], y_lasso[fold$train],
                   alpha = 1, lambda = lam,
                   penalty.factor = penalty_factors,
                   standardize = TRUE)
    pred <- predict(fit, newx = X_lasso[fold$test, ], s = lam)
    mean((y_lasso[fold$test] - pred)^2)
  }))
})

best_lambda <- lambda_grid[which.min(cv_mse)]
cat("✔ Best lambda:", round(best_lambda, 6), "\n")

# Fit final Lasso
lasso_final <- glmnet(X_lasso, y_lasso,
                      alpha          = 1,
                      lambda         = best_lambda,
                      penalty.factor = penalty_factors,
                      standardize    = TRUE)

# Extract lag coefficients
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

# Select best lag per predictor
surviving_lags <- lasso_lag_coefs %>% filter(retained)

best_unemp_lag <- surviving_lags %>%
  filter(grepl("ucyc", variable)) %>%
  slice_max(abs(coefficient), n = 1) %>%
  pull(variable) %>% str_extract("[0-9]") %>% as.integer()

best_gdpg_lag <- surviving_lags %>%
  filter(grepl("gcyc", variable)) %>%
  slice_max(abs(coefficient), n = 1) %>%
  pull(variable) %>% str_extract("[0-9]") %>% as.integer()

if (length(best_unemp_lag) == 0) {
  best_unemp_lag <- 3
  cat("⚠ Defaulting unemployment to lag 3\n")
}
if (length(best_gdpg_lag) == 0) {
  best_gdpg_lag <- 3
  cat("⚠ Defaulting GDP growth to lag 3\n")
}

cat("\n✔ Selected lags:\n")
cat("  Unemployment cycle: lag", best_unemp_lag, "\n")
cat("  GDP growth cycle:   lag", best_gdpg_lag,  "\n")

# Lasso coefficient path plot
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
             linetype = "dashed", colour = "black") +
  labs(title    = "Lasso Coefficient Path — Cyclical Lag Variables Only",
       subtitle = "Major fixed effects & time trend protected from penalisation",
       x = "log(λ)", y = "Coefficient", colour = "Variable") +
  theme_minimal(base_size = 13)

print(p_lasso)


# ============================================================
# STEP 2: FRACTIONAL LOGIT — PRIMARY INFERENCE MODEL
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

print(summary(flogit_model))

# Cluster-robust SEs
robust_se <- coeftest(flogit_model,
                      vcov = vcovHC(flogit_model,
                                    type = "HC3"))
cat("\n── Robust SEs ───────────────────────────────────────────\n")
print(robust_se)

# AMEs
p_hat  <- predict(flogit_model, type = "response")
ame_fn <- function(beta) mean(beta * p_hat * (1 - p_hat), na.rm = TRUE)

ame_unemp <- ame_fn(coef(flogit_model)[unemp_selected])
ame_gdpg  <- ame_fn(coef(flogit_model)[gdpg_selected])

cat("\n── Average Marginal Effects ─────────────────────────────\n")
cat(sprintf("Unemployment cycle (lag %d): %+.5f → %+.3f pp\n",
            best_unemp_lag, ame_unemp, ame_unemp * 100))
cat(sprintf("GDP growth cycle   (lag %d): %+.5f → %+.3f pp\n",
            best_gdpg_lag,  ame_gdpg,  ame_gdpg  * 100))

# Major-specific models
major_results <- map_dfr(unique(df_clean$major), function(m) {
  sub  <- df_clean %>% filter(major == m)
  form <- as.formula(paste("major_share ~",
                           unemp_selected, "+",
                           gdpg_selected, "+ time_trend"))
  tryCatch({
    fit   <- glm(form, data = sub, family = quasibinomial(link = "logit"))
    p_h   <- predict(fit, type = "response")
    se_r  <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
    ame_f <- function(b) mean(b * p_h * (1 - p_h), na.rm = TRUE)
    tibble(
      major   = m,
      term    = c("Unemployment", "GDP Growth"),
      AME     = c(ame_f(coef(fit)[unemp_selected]),
                  ame_f(coef(fit)[gdpg_selected])),
      p.value = c(se_r[unemp_selected, "Pr(>|z|)"],
                  se_r[gdpg_selected,  "Pr(>|z|)"])
    )
  }, error = function(e) NULL)
})


# ============================================================
# STEP 3: BLOCK BOOTSTRAP FOR CONFIDENCE INTERVALS
# (no changes — variable names update automatically)
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 3: BLOCK BOOTSTRAP (B = 1000 iterations)\n")
cat("════════════════════════════════════════════════════════\n")

set.seed(42)
B          <- 1000
block_size <- 4
years_vec  <- sort(unique(df_clean$year))
n_years    <- length(years_vec)

block_bootstrap_sample <- function(df, years, block_sz) {
  start_positions <- 1:(length(years) - block_sz + 1)
  n_blocks_needed <- ceiling(length(years) / block_sz)
  selected_starts <- sample(start_positions,
                            size = n_blocks_needed, replace = TRUE)
  boot_years <- unlist(lapply(selected_starts, function(s) {
    years[s:min(s + block_sz - 1, length(years))]
  }))[1:length(years)]
  map_dfr(seq_along(boot_years), function(i) {
    df %>% filter(year == boot_years[i]) %>%
      mutate(boot_year = years[i])
  })
}

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
    tibble(
      boot      = b,
      ame_unemp = ame_fn(coef(fit)[unemp_selected]),
      ame_gdpg  = ame_fn(coef(fit)[gdpg_selected])
    )
  }, error = function(e) NULL)
}, .progress = TRUE)

cat("✔ Successful bootstrap iterations:", nrow(boot_results), "of", B, "\n")

# 95% percentile CIs
ci_unemp <- quantile(boot_results$ame_unemp, c(0.025, 0.975), na.rm = TRUE)
ci_gdpg  <- quantile(boot_results$ame_gdpg,  c(0.025, 0.975), na.rm = TRUE)

cat("\n── Block Bootstrap 95% CIs (percentage points) ─────────\n")
cat(sprintf("Unemployment cycle (lag %d): %+.3f pp  [%+.3f, %+.3f]\n",
            best_unemp_lag,
            ame_unemp * 100, ci_unemp[1] * 100, ci_unemp[2] * 100))
cat(sprintf("GDP growth cycle   (lag %d): %+.3f pp  [%+.3f, %+.3f]\n",
            best_gdpg_lag,
            ame_gdpg * 100, ci_gdpg[1] * 100, ci_gdpg[2] * 100))

# Final results table
final_table <- tibble(
  Predictor = c(paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")),
  AME       = round(c(ame_unemp, ame_gdpg) * 100, 4),
  CI_lower  = round(c(ci_unemp[1], ci_gdpg[1]) * 100, 4),
  CI_upper  = round(c(ci_unemp[2], ci_gdpg[2]) * 100, 4),
  Sig       = c(
    ifelse(ci_unemp[1] > 0 | ci_unemp[2] < 0, "✔ Significant", "✘ Not significant"),
    ifelse(ci_gdpg[1]  > 0 | ci_gdpg[2]  < 0, "✔ Significant", "✘ Not significant")
  )
)

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FINAL RESULTS (AME in percentage points)\n")
cat("══════════════════════════════════════════════════════════\n")
print(final_table)


# ── VISUALISATIONS ────────────────────────────────────────────

# Bootstrap distribution
boot_long <- boot_results %>%
  pivot_longer(c(ame_unemp, ame_gdpg),
               names_to = "predictor", values_to = "ame") %>%
  mutate(predictor = recode(predictor,
                            ame_unemp = paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                            ame_gdpg  = paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")))

p_boot <- ggplot(boot_long, aes(x = ame * 100, fill = predictor)) +
  geom_histogram(bins = 40, colour = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~predictor, scales = "free_x") +
  labs(title    = "Block Bootstrap Distribution of AMEs",
       subtitle = paste0("B = ", B, " | Block size = ", block_size, " years"),
       x = "AME (percentage points)", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_boot)

# Major-specific AME plots
plot_major_ame <- function(term_name, fill_col) {
  major_results %>%
    filter(term == term_name) %>%
    mutate(sig = ifelse(p.value < 0.05, "p < 0.05", "p ≥ 0.05"),
           AME = AME * 100) %>%
    ggplot(aes(x = reorder(major, AME), y = AME, fill = sig)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c("p < 0.05" = fill_col, "p ≥ 0.05" = "grey75")) +
    labs(title = paste("Effect of", term_name, "by Major"),
         x = NULL, y = "AME (pp)", fill = NULL) +
    theme_minimal(base_size = 12)
}

print(plot_major_ame("Unemployment", "steelblue") /
        plot_major_ame("GDP Growth",   "darkgreen"))

# Final summary plot
p_final <- final_table %>%
  ggplot(aes(x = Predictor, y = AME,
             ymin = CI_lower, ymax = CI_upper, colour = Sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(size = 1.2, linewidth = 1) +
  coord_flip() +
  scale_colour_manual(values = c("✔ Significant"     = "steelblue",
                                 "✘ Not significant" = "grey60")) +
  labs(title    = "Final Model: AMEs with 95% Block Bootstrap CIs",
       subtitle = "Outcome: Major share of total bachelor's graduates",
       x = NULL, y = "AME (percentage points)", colour = NULL) +
  theme_minimal(base_size = 13)

print(p_final)