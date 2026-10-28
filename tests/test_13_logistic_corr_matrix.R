# tests/test_13_logistic_corr_matrix.R
# Test Suite for StatLab Binary Logistic Regression & Correlation Matrix Heatmap Engine
# Tests R/13_logistic_corr_matrix.R

cat("\n=== Testing StatLab Logistic Regression & Correlation Matrix Engine (R/13_logistic_corr_matrix.R) ===\n")

# ---------------------------------------------------------------------------
# Test Data 1: Shrimp Survival Binary Response (DO and TAN predictors)
# ---------------------------------------------------------------------------
set.seed(42)
n_shrimps <- 40
do_vals <- round(stats::runif(n_shrimps, 3.0, 7.5), 1)
tan_vals <- round(stats::runif(n_shrimps, 0.05, 0.85), 2)
# True log-odds: higher DO increases survival, higher TAN decreases survival
log_odds <- -1.5 + 1.2 * do_vals - 4.5 * tan_vals
prob_surv <- 1 / (1 + exp(-log_odds))
status_surv <- ifelse(stats::runif(n_shrimps) < prob_surv, 1L, 0L)

df_logistic <- data.frame(
  Status_Sintasan = status_surv,
  DO_mgL = do_vals,
  TAN_ppm = tan_vals
)

# ---------------------------------------------------------------------------
# Test 1: Binary Logistic Regression
# ---------------------------------------------------------------------------
cat("--- 1. Testing Binary Logistic Regression ---\n")

res_logit <- sl_analyze_binary_logistic(df_logistic, list(
  roles = list(
    response_col = "Status_Sintasan",
    predictor_cols = c("DO_mgL", "TAN_ppm")
  ),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_logit), "Binary logistic returns a list")
assert_true(!is.null(res_logit$main$coefficients), "Coefficients list present")
assert_true(!is.null(res_logit$main$pseudo_r2_nagelkerke), "Nagelkerke pseudo R2 calculated")
assert_true(!is.null(res_logit$main$auc), "ROC AUC calculated")
assert_true(res_logit$main$auc >= 0.5 && res_logit$main$auc <= 1.0, "AUC in valid range [0.5, 1.0]")

# Verify Odds Ratios: DO should have OR > 1 (protective), TAN should have OR < 1 (risk factor)
coef_df <- res_logit$main$coefficients
do_idx <- which(coef_df$variable == "DO_mgL")
tan_idx <- which(coef_df$variable == "TAN_ppm")
assert_true(length(do_idx) == 1L, "DO_mgL found in coefficients")
assert_true(length(tan_idx) == 1L, "TAN_ppm found in coefficients")
assert_true(coef_df$odds_ratio[do_idx] > 1.0, "DO has positive odds ratio (> 1.0)")
assert_true(coef_df$odds_ratio[tan_idx] < 1.0, "TAN has risk odds ratio (< 1.0)")

# Verify tables and plots
assert_true(length(res_logit$tables_apa) >= 2L, "At least 2 APA tables (Coefficients & Model Summary)")
assert_true(length(res_logit$plots) >= 1L, "ROC curve plot generated")
assert_true(grepl("• \\*\\*Rasio Odds", res_logit$interpretation$id), "Key points format present in Logistic Regression")

# ---------------------------------------------------------------------------
# Test Data 2: Water Quality Parameters for Correlation Matrix
# ---------------------------------------------------------------------------
df_water <- data.frame(
  DO = c(6.5, 6.2, 5.8, 5.1, 4.8, 4.2, 7.0, 6.8, 5.5, 4.9),
  pH = c(7.8, 7.7, 7.6, 7.4, 7.2, 7.0, 8.0, 7.9, 7.5, 7.3),
  Suhu = c(28.5, 29.0, 29.5, 30.2, 31.0, 31.5, 28.0, 28.2, 29.8, 30.5),
  Amonia = c(0.08, 0.12, 0.18, 0.35, 0.52, 0.68, 0.05, 0.07, 0.22, 0.41)
)

# ---------------------------------------------------------------------------
# Test 2: Multivariate Correlation Matrix & 300-DPI Heatmap
# ---------------------------------------------------------------------------
cat("--- 2. Testing Correlation Matrix & Heatmap ---\n")

res_corr <- sl_analyze_correlation_matrix(df_water, list(
  method = "pearson",
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_corr), "Correlation matrix returns a list")
assert_true(!is.null(res_corr$main$r_matrix), "Correlation matrix present")
assert_true(!is.null(res_corr$main$p_matrix), "P-value matrix present")

# Verify diagonal is 1.0
diag_vals <- diag(res_corr$main$r_matrix)
assert_near(diag_vals, rep(1.0, 4), tol = 1e-8, "Diagonal of correlation matrix is 1.0")

# DO and Amonia should have strong negative correlation
r_do_amonia <- res_corr$main$r_matrix["DO", "Amonia"]
assert_true(r_do_amonia < -0.80, "DO and Amonia have strong negative correlation (r < -0.80)")

# Verify tables and Heatmap plot
assert_true(length(res_corr$tables_apa) >= 1L, "At least 1 APA table for correlation matrix")
assert_true(length(res_corr$plots) >= 1L, "Publication-grade Correlation Heatmap plot generated")
assert_true(grepl("• \\*\\*Korelasi Terkuat", res_corr$interpretation$id), "Key points format present in Correlation Analysis")

# ---------------------------------------------------------------------------
# Test 3: Dispatcher Routing
# ---------------------------------------------------------------------------
cat("--- 3. Testing Dispatcher Routing ---\n")

disp_logit <- sl_dispatch("run_analysis", list(
  analysis_type = "binary_logistic",
  data = df_logistic,
  opts = list(
    roles = list(response_col = "Status_Sintasan", predictor_cols = c("DO_mgL", "TAN_ppm")),
    include_plots = FALSE
  )
))
assert_true(is.character(disp_logit), "binary_logistic dispatched")

disp_corr <- sl_dispatch("run_analysis", list(
  analysis_type = "correlation_matrix",
  data = df_water,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_corr), "correlation_matrix dispatched")

cat("=== All Logistic Regression & Correlation Matrix Tests Passed ===\n")
