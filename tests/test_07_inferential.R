# tests/test_07_inferential.R
# Test Suite for Two-Way ANOVA, Regression, and Chi-Square in pure Base R

cat("\n=== Testing StatLab Inferential Modules (Two-Way ANOVA, Regression, Chi-Square) ===\n")

# Source prerequisite modules if needed
if (!exists("sl_analyze_regression", mode = "function")) {
  r_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  for (f in r_files) source(f)
}

# ---------------------------------------------------------------------------
# Test 1: Two-Way ANOVA
# ---------------------------------------------------------------------------
cat("--- 1. Testing Two-Way ANOVA ---\n")

df_2way <- data.frame(
  Suhu = factor(rep(c("Rendah", "Tinggi"), each = 6)),
  Salinitas = factor(rep(c("15_ppt", "30_ppt"), each = 3, times = 2)),
  Laju_Pertumbuhan = c(1.8, 2.0, 1.9, 2.5, 2.3, 2.4, 3.2, 3.4, 3.1, 3.9, 4.1, 3.8)
)

res_2way <- sl_analyze_twoway_anova(df_2way, list(alpha = 0.05, include_plots = FALSE))

assert_true(!is.null(res_2way$main$factor_a), "Factor A present")
assert_true(!is.null(res_2way$main$factor_b), "Factor B present")
assert_true(!is.null(res_2way$main$interaction), "Interaction present")
assert_true(res_2way$main$factor_a$p < 0.001, "Factor A significant")
assert_true(res_2way$main$factor_b$p < 0.001, "Factor B significant")
assert_true(length(res_2way$tables_apa) >= 2L, "Two-Way ANOVA has at least 2 tables")

# ---------------------------------------------------------------------------
# Test 2: Linear Regression & Correlation
# ---------------------------------------------------------------------------
cat("--- 2. Testing Linear Regression & Correlation ---\n")

df_reg <- data.frame(
  Salinitas = c(5, 10, 15, 20, 25, 30, 35, 40),
  Sintasan = c(95.0, 92.5, 88.0, 84.5, 78.0, 71.5, 64.0, 52.0)
)

ref_lm <- lm(Sintasan ~ Salinitas, data = df_reg)
ref_sum <- summary(ref_lm)
ref_r2 <- ref_sum$r.squared
ref_f <- ref_sum$fstatistic[1L]

res_reg <- sl_analyze_regression(df_reg, list(alpha = 0.05, include_plots = TRUE))

assert_near(res_reg$main$r_squared, ref_r2, 1e-8, "R-squared parity")
assert_near(res_reg$main$f_statistic, ref_f, 1e-8, "F statistic parity")
assert_true(length(res_reg$tables_apa) >= 2L, "Regression has at least 2 tables (ANOVA + Coefficients)")
assert_true(length(res_reg$plots) >= 1L, "Scatter plot generated")

# ---------------------------------------------------------------------------
# Test 3: Chi-Square Test of Independence
# ---------------------------------------------------------------------------
cat("--- 3. Testing Chi-Square Test of Independence ---\n")

df_chi <- data.frame(
  Kolam_A = c(45, 5),
  Kolam_B = c(30, 15),
  Kolam_C = c(52, 8)
)

ref_chi <- chisq.test(as.matrix(df_chi), correct = FALSE)
res_chi <- sl_analyze_chisq(df_chi, list(alpha = 0.05))

assert_near(res_chi$main$chi_statistic, as.double(ref_chi$statistic), 1e-8, "Chi-Square statistic parity")
assert_near(res_chi$main$p_value, as.double(ref_chi$p.value), 1e-8, "Chi-Square p-value parity")
assert_true(length(res_chi$tables_apa) >= 1L, "Chi-Square summary table present")

# ---------------------------------------------------------------------------
# Test 4: Curve Estimation & Non-Linear Regression
# ---------------------------------------------------------------------------
cat("--- 4. Testing Curve Estimation & Non-Linear Models ---\n")

df_quad <- data.frame(
  Dosis = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
  Pertumbuhan = c(1.2, 2.1, 2.8, 3.1, 3.0, 2.5, 1.7)
)

res_curve_quad <- sl_analyze_curve_estimation(df_quad, list(alpha = 0.05, include_plots = TRUE, model = "all"))
assert_true(!is.null(res_curve_quad$main$best_model), "Best model identified")
assert_true(res_curve_quad$main$best_model == "quadratic", "Quadratic identified as best model for parabolic data")
assert_true(!is.null(res_curve_quad$main$vertex), "Vertex optimum calculated")
assert_true(res_curve_quad$main$vertex$x_opt > 3.5 && res_curve_quad$main$vertex$x_opt < 4.5, "Optimum X vertex in expected range")
assert_true(res_curve_quad$main$vertex$nature == "maximum", "Vertex is maximum / peak")
assert_true(length(res_curve_quad$tables_apa) >= 3L, "At least 3 APA tables generated")

# Allometric Power test
df_allo <- data.frame(
  Panjang = c(10, 12, 14, 16, 18, 20),
  Bobot = c(15.2, 25.8, 41.2, 61.5, 87.0, 119.5)
)
res_curve_allo <- sl_analyze_curve_estimation(df_allo, list(alpha = 0.05, include_plots = FALSE, model = "power"))
assert_true(!is.null(res_curve_allo$main$allometric), "Allometric test evaluated")
assert_true(abs(res_curve_allo$main$allometric$b - 3.0) < 0.2, "Allometric exponent b near 3.0")
assert_true(!is.null(res_curve_allo$main$allometric$p_value), "Allometric p-value present")

# ---------------------------------------------------------------------------
# Test 5: Dispatcher Routing for all inferential analyses
# ---------------------------------------------------------------------------
cat("--- 5. Testing Dispatcher Routing ---\n")

disp_2way <- sl_dispatch("run_analysis", list(analysis_type = "twoway_anova", data = df_2way, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_2way), "twoway_anova dispatched")

disp_reg <- sl_dispatch("run_analysis", list(analysis_type = "regression", data = df_reg, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_reg), "regression dispatched")

disp_chi <- sl_dispatch("run_analysis", list(analysis_type = "chisquare_indep", data = df_chi, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_chi), "chisquare_indep dispatched")

disp_curve <- sl_dispatch("run_analysis", list(analysis_type = "curve_estimation", data = df_quad, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_curve), "curve_estimation dispatched")

cat("=== All Inferential Engine Tests Passed ===\n")
