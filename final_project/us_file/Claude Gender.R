# ============================================================
# FULL PIPELINE WITH GENDER SPLIT:
# Step 0:  Packages
# Step 1:  Load & prepare data (overall + by gender)
# Step 1g: Detrend economic variables
# Step 2:  Lasso → lag selection (run once, applied to all)
# Step 3:  Fractional Logit → inference (overall, men, women)
# Step 4:  Block Bootstrap → final CIs (overall, men, women)
# Step 5:  Gender comparison visualisations
# ============================================================


# ── 0. PACKAGES ──────────────────────────────────────────────
packages <- c("tidyverse", "glmnet", "lmtest", "sandwich",
              "modelsummary", "ggplot2", "patchwork", "broom")

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)


# ── 1. LOAD & PREPARE DATA ───────────────────────────────────
df_raw <- read.csv("major_ALL.csv", stringsAsFactors = FALSE)

# 1a. Pull grand totals for overall, men, and women separately
grand_total <- df_raw %>%
  filter(BACHELORS == "Grand total") %>%
  select(YEAR,
         grand_total       = Total,
         grand_total_men   = Total_Men,
         grand_total_women = Total_Women)

# 1b. Individual majors only
df <- df_raw %>%
  filter(BACHELORS != "Grand total") %>%
  left_join(grand_total, by = "YEAR") %>%
  rename(year        = YEAR,
         major       = BACHELORS,
         total       = Total,
         total_men   = Total_Men,
         total_women = Total_Women)

# 1c. Major shares — overall, men, women
#     Each share = "of all [group] graduates, what % chose this major?"
df <- df %>%
  mutate(
    share_overall = total       / grand_total,
    share_men     = total_men   / grand_total_men,
    share_women   = total_women / grand_total_women
  )

# 1d. GDP growth rate
df <- df %>%
  arrange(major, year) %>%
  group_by(major) %>%
  mutate(gdp_growth = (gdp - lag(gdp)) / lag(gdp) * 100) %>%
  ungroup()

# 1e. Raw lags 1–4 + time trend
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

# 1f. Drop NAs
df_clean <- df %>% na.omit()

cat("✔ Rows after cleaning:", nrow(df_clean),
    "| Majors:", n_distinct(df_clean$major),
    "| Years:",  n_distinct(df_clean$year), "\n")


# ──────────────────────────────────────────────────────────────
# 1g. DETREND ECONOMIC VARIABLES
#     Removes linear trend so only cyclical deviations remain.
#     Resolves collinearity with time_trend.
# ──────────────────────────────────────────────────────────────

macro_yearly <- df_clean %>%
  select(year, unemploy, gdp_growth, time_trend) %>%
  distinct() %>%
  arrange(year)

unemp_trend_model <- lm(unemploy   ~ time_trend, data = macro_yearly)
gdpg_trend_model  <- lm(gdp_growth ~ time_trend, data = macro_yearly)

macro_yearly <- macro_yearly %>%
  mutate(
    unemploy_cycle = residuals(unemp_trend_model),
    gdpg_cycle     = residuals(gdpg_trend_model)
  )

# Visualise raw vs. detrended
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

# Merge cyclical variables back + create cyclical lags
df_clean <- df_clean %>%
  left_join(macro_yearly %>% select(year, unemploy_cycle, gdpg_cycle),
            by = "year") %>%
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

# Confirm collinearity resolved
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
# STEP 2: LASSO FOR LAG SELECTION
# Run once on overall share — lags apply to all three models
# (overall, men, women share the same economic time series)
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 2: LASSO LAG SELECTION (using overall share)\n")
cat("════════════════════════════════════════════════════════\n")

major_dummies <- model.matrix(~ factor(major) + time_trend - 1,
                              data = df_clean)
lag_vars      <- as.matrix(df_clean %>%
                             select(ucyc_lag1, ucyc_lag2,
                                    ucyc_lag3, ucyc_lag4,
                                    gcyc_lag1, gcyc_lag2,
                                    gcyc_lag3, gcyc_lag4))

X_lasso <- cbind(major_dummies, lag_vars)
y_lasso <- df_clean$share_overall       # lag selection based on overall

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

lasso_final <- glmnet(X_lasso, y_lasso,
                      alpha          = 1,
                      lambda         = best_lambda,
                      penalty.factor = penalty_factors,
                      standardize    = TRUE)

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

cat("\n✔ Selected lags (applied to all three models):\n")
cat("  Unemployment cycle: lag", best_unemp_lag, "\n")
cat("  GDP growth cycle:   lag", best_gdpg_lag,  "\n")

unemp_selected <- paste0("ucyc_lag", best_unemp_lag)
gdpg_selected  <- paste0("gcyc_lag", best_gdpg_lag)

# Lasso path plot
lasso_path <- glmnet(X_lasso, y_lasso,
                     alpha = 1, penalty.factor = penalty_factors,
                     standardize = TRUE)

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
  annotate("text", x = log(best_lambda),
           y = max(path_df$coefficient) * 0.9,
           label = "Selected λ", hjust = -0.1, size = 3.5) +
  labs(title    = "Lasso Coefficient Path — Cyclical Lag Variables Only",
       subtitle = "Major fixed effects & time trend protected from penalisation",
       x = "log(λ)", y = "Coefficient", colour = "Variable") +
  theme_minimal(base_size = 13)

print(p_lasso)


# ============================================================
# STEP 3: FRACTIONAL LOGIT — OVERALL, MEN, WOMEN
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 3: FRACTIONAL LOGIT — ALL THREE MODELS\n")
cat("════════════════════════════════════════════════════════\n")

# ── Helper: run fractional logit + extract CIs + AMEs ────────
run_flogit_full <- function(outcome_var, data = df_clean) {
  
  formula <- as.formula(
    paste(outcome_var, "~",
          unemp_selected, "+", gdpg_selected,
          "+ time_trend + factor(major)")
  )
  
  fit <- glm(formula, data = data, family = quasibinomial(link = "logit"))
  
  # HC3 robust SEs
  robust_ci <- coefci(fit,
                      vcov  = vcovHC(fit, type = "HC3"),
                      level = 0.95)
  
  # AME scaling factor
  p_hat       <- predict(fit, type = "response")
  mean_factor <- mean(p_hat * (1 - p_hat), na.rm = TRUE)
  
  # CI table for key predictors
  ci_table <- as.data.frame(robust_ci) %>%
    rownames_to_column("term") %>%
    rename(CI_lower = `2.5 %`, CI_upper = `97.5 %`) %>%
    filter(term %in% c(unemp_selected, gdpg_selected, "time_trend")) %>%
    mutate(
      estimate    = coef(fit)[term],
      AME         = estimate * mean_factor * 100,
      AME_CI_low  = CI_lower * mean_factor * 100,
      AME_CI_high = CI_upper * mean_factor * 100,
      sig         = ifelse(AME_CI_low > 0 | AME_CI_high < 0,
                           "✔ Significant", "✘ Not significant"),
      across(c(estimate, CI_lower, CI_upper,
               AME, AME_CI_low, AME_CI_high), ~round(.x, 5))
    ) %>%
    select(term, estimate, CI_lower, CI_upper,
           AME, AME_CI_low, AME_CI_high, sig)
  
  list(model = fit, ci_table = ci_table, mean_factor = mean_factor)
}

# Run for all three outcomes
model_overall <- run_flogit_full("share_overall")
model_men     <- run_flogit_full("share_men")
model_women   <- run_flogit_full("share_women")

cat("\n── Overall ─────────────────────────────────────────────\n")
print(model_overall$ci_table)

cat("\n── Men ─────────────────────────────────────────────────\n")
print(model_men$ci_table)

cat("\n── Women ───────────────────────────────────────────────\n")
print(model_women$ci_table)


# ── Major-specific models for all three outcomes ──────────────
run_major_flogit <- function(outcome_var, data = df_clean) {
  map_dfr(unique(data$major), function(m) {
    sub  <- data %>% filter(major == m)
    form <- as.formula(paste(outcome_var, "~",
                             unemp_selected, "+",
                             gdpg_selected, "+ time_trend"))
    tryCatch({
      fit  <- glm(form, data = sub, family = quasibinomial(link = "logit"))
      p_h  <- predict(fit, type = "response")
      mf   <- mean(p_h * (1 - p_h), na.rm = TRUE)
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
}

major_overall <- run_major_flogit("share_overall") %>% mutate(group = "Overall")
major_men     <- run_major_flogit("share_men")     %>% mutate(group = "Men")
major_women   <- run_major_flogit("share_women")   %>% mutate(group = "Women")

# Combine into one table
major_all <- bind_rows(major_overall, major_men, major_women) %>%
  mutate(group = factor(group, levels = c("Overall", "Men", "Women")))

cat("\n── Major-Specific AMEs (all groups) ─────────────────────\n")
print(major_all %>%
        mutate(across(c(AME, CI_lower, CI_upper), ~round(.x, 4))) %>%
        arrange(term, major, group))


# ============================================================
# STEP 4: BLOCK BOOTSTRAP — OVERALL, MEN, WOMEN
# ============================================================

cat("\n════════════════════════════════════════════════════════\n")
cat("  STEP 4: BLOCK BOOTSTRAP (B = 1000 iterations)\n")
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

# Bootstrap for one outcome variable
run_bootstrap <- function(outcome_var) {
  map_dfr(1:B, function(b) {
    tryCatch({
      boot_data <- block_bootstrap_sample(df_clean, years_vec, block_size)
      fit <- glm(
        as.formula(paste(outcome_var, "~",
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
  }, .progress = FALSE)
}

cat("Bootstrapping overall share...\n")
boot_overall <- run_bootstrap("share_overall")
cat("Bootstrapping men's share...\n")
boot_men     <- run_bootstrap("share_men")
cat("Bootstrapping women's share...\n")
boot_women   <- run_bootstrap("share_women")

cat("✔ Bootstrap complete\n",
    " Overall:", nrow(boot_overall), "| Men:", nrow(boot_men),
    "| Women:", nrow(boot_women), "successful iterations\n")

# Extract 95% CIs for each group
extract_boot_ci <- function(boot_df, group_name,
                            model_obj, outcome_var) {
  ci_u <- quantile(boot_df$ame_unemp, c(0.025, 0.975), na.rm = TRUE)
  ci_g <- quantile(boot_df$ame_gdpg,  c(0.025, 0.975), na.rm = TRUE)
  mf   <- model_obj$mean_factor
  
  tibble(
    group     = group_name,
    Predictor = c(paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                  paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")),
    AME       = round(c(
      coef(model_obj$model)[unemp_selected] * mf * 100,
      coef(model_obj$model)[gdpg_selected]  * mf * 100), 4),
    CI_lower  = round(c(ci_u[1], ci_g[1]), 4),
    CI_upper  = round(c(ci_u[2], ci_g[2]), 4),
    Sig       = c(
      ifelse(ci_u[1] > 0 | ci_u[2] < 0, "✔ Significant", "✘ Not significant"),
      ifelse(ci_g[1] > 0 | ci_g[2] < 0, "✔ Significant", "✘ Not significant")
    )
  )
}

final_overall <- extract_boot_ci(boot_overall, "Overall", model_overall)
final_men     <- extract_boot_ci(boot_men,     "Men",     model_men)
final_women   <- extract_boot_ci(boot_women,   "Women",   model_women)

final_table <- bind_rows(final_overall, final_men, final_women) %>%
  mutate(group = factor(group, levels = c("Overall", "Men", "Women")))

cat("\n══════════════════════════════════════════════════════════\n")
cat("  FINAL RESULTS (AME in percentage points)\n")
cat("  95% CIs from block bootstrap\n")
cat("══════════════════════════════════════════════════════════\n")
print(final_table)


# ============================================================
# STEP 5: VISUALISATIONS
# ============================================================

# ── Plot 1: Already printed — detrended unemployment ─────────


# ── Plot 2: Already printed — Lasso path ─────────────────────


# ── Plot 3: Bootstrap distributions — all three groups ───────
boot_combined <- bind_rows(
  boot_overall %>% mutate(group = "Overall"),
  boot_men     %>% mutate(group = "Men"),
  boot_women   %>% mutate(group = "Women")
) %>%
  pivot_longer(c(ame_unemp, ame_gdpg),
               names_to = "predictor", values_to = "ame") %>%
  mutate(
    predictor = recode(predictor,
                       ame_unemp = paste0("Unemployment cycle (lag ", best_unemp_lag, ")"),
                       ame_gdpg  = paste0("GDP growth cycle (lag ",   best_gdpg_lag,  ")")),
    group = factor(group, levels = c("Overall", "Men", "Women"))
  )

p_boot <- ggplot(boot_combined, aes(x = ame, fill = group)) +
  geom_histogram(bins = 35, colour = "white", alpha = 0.8,
                 position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  facet_grid(group ~ predictor, scales = "free_x") +
  scale_fill_manual(values = c("Overall" = "grey50",
                               "Men"     = "steelblue",
                               "Women"   = "coral")) +
  labs(title    = "Block Bootstrap Distribution of AMEs by Gender",
       subtitle = paste0("B = ", B, " iterations | Block size = ",
                         block_size, " years"),
       x = "AME (percentage points)", y = "Count", fill = "Group") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p_boot)


# ── Plot 4: Final summary — gender comparison with CIs ───────
p_final <- final_table %>%
  mutate(group = factor(group, levels = c("Women", "Men", "Overall"))) %>%
  ggplot(aes(x      = group,
             y      = AME,
             ymin   = CI_lower,
             ymax   = CI_upper,
             colour = group,
             shape  = Sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(size = 1, linewidth = 0.9,
                  position = position_dodge(width = 0.4)) +
  facet_wrap(~Predictor, scales = "free_y") +
  coord_flip() +
  scale_colour_manual(values = c("Overall" = "grey40",
                                 "Men"     = "steelblue",
                                 "Women"   = "coral")) +
  scale_shape_manual(values = c("✔ Significant"     = 19,
                                "✘ Not significant" = 1)) +
  labs(title    = "Final Model: AMEs by Gender with 95% Block Bootstrap CIs",
       subtitle = "Outcome: Major share of same-gender graduates | Filled = CI excludes zero",
       x        = NULL,
       y        = "Average Marginal Effect (percentage points)",
       colour   = "Group",
       shape    = "CI excludes 0") +
  theme_minimal(base_size = 13)

print(p_final)


# ── Plot 5: Major-specific AMEs by gender — side by side ─────
plot_gender_major <- function(term_name) {
  major_all %>%
    filter(term == term_name) %>%
    ggplot(aes(x      = reorder(major, AME),
               y      = AME,
               ymin   = CI_lower,
               ymax   = CI_upper,
               colour = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(size = 0.7, linewidth = 0.7,
                    position = position_dodge(width = 0.6)) +
    coord_flip() +
    scale_colour_manual(values = c("Overall" = "grey40",
                                   "Men"     = "steelblue",
                                   "Women"   = "coral")) +
    labs(title    = paste("Effect of", term_name, "on Major Share by Gender"),
         subtitle = "Point = AME | Whiskers = 95% CI | Dodge = gender group",
         x        = NULL,
         y        = "AME (percentage points)",
         colour   = "Group") +
    theme_minimal(base_size = 12)
}

print(plot_gender_major("Unemployment") /
        plot_gender_major("GDP Growth"))


# ── Plot 6: Major share trends over time — men vs women ──────
df_trend <- df_raw %>%
  filter(BACHELORS != "Grand total") %>%
  left_join(grand_total, by = "YEAR") %>%
  rename(year        = YEAR,
         major       = BACHELORS,
         total_men   = Total_Men,
         total_women = Total_Women) %>%
  mutate(
    share_men   = total_men   / grand_total_men,
    share_women = total_women / grand_total_women
  ) %>%
  pivot_longer(c(share_men, share_women),
               names_to  = "gender",
               values_to = "share") %>%
  mutate(gender = recode(gender,
                         share_men   = "Men",
                         share_women = "Women"))

p_trends <- ggplot(df_trend,
                   aes(x = year, y = share * 100,
                       colour = gender, linetype = gender)) +
  geom_rect(data = recessions,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~major, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("Men" = "steelblue", "Women" = "coral")) +
  scale_linetype_manual(values = c("Men" = "solid", "Women" = "dashed")) +
  labs(title    = "Major Share Over Time by Gender",
       subtitle = "Share = % of same-gender graduates | Shaded = recessions",
       x = "Year", y = "Share (%)",
       colour = "Gender", linetype = "Gender") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_trends)