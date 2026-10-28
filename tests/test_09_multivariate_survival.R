# tests/test_09_multivariate_survival.R
# Test Suite for Repeated Measures ANOVA, MANOVA, Kaplan-Meier, and PCA/HCA in pure Base R

cat("\n=== Testing StatLab Advanced Modules (RM-ANOVA, MANOVA, Kaplan-Meier, PCA/HCA) ===\n")

# Source prerequisite modules if needed
if (!exists("sl_analyze_repeated_measures", mode = "function")) {
  r_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  for (f in r_files) source(f)
}

# ---------------------------------------------------------------------------
# Test 1: Repeated Measures ANOVA
# ---------------------------------------------------------------------------
cat("--- 1. Testing Repeated Measures ANOVA ---\n")

df_rm <- data.frame(
  Minggu_0 = c(10.2, 11.5, 9.8, 12.0, 10.5),
  Minggu_2 = c(15.4, 16.8, 14.9, 17.2, 15.8),
  Minggu_4 = c(22.1, 23.5, 21.0, 24.2, 22.8),
  Minggu_6 = c(30.5, 32.0, 29.8, 33.1, 31.2)
)

res_rm <- sl_analyze_repeated_measures(df_rm, list(alpha = 0.05, include_plots = TRUE))

assert_true(!is.null(res_rm$main$f_statistic), "RM-ANOVA F-statistic present")
assert_true(res_rm$main$f_statistic > 500, "RM-ANOVA F-statistic strong effect")
assert_true(res_rm$main$p_value < 0.001, "RM-ANOVA p-value highly significant")
assert_true(!is.null(res_rm$main$epsilon), "Greenhouse-Geisser epsilon present")
assert_true(res_rm$main$epsilon > 0 && res_rm$main$epsilon <= 1.0, "Epsilon within [0, 1]")
assert_true(length(res_rm$tables_apa) >= 3L, "RM-ANOVA has 3 APA tables")
assert_true(length(res_rm$plots) >= 1L, "RM-ANOVA profile growth plot generated")

# ---------------------------------------------------------------------------
# Test 2: MANOVA (Multivariate ANOVA)
# ---------------------------------------------------------------------------
cat("--- 2. Testing MANOVA ---\n")

df_manova <- data.frame(
  Perlakuan = factor(rep(c("Kontrol", "Dosis_A", "Dosis_B"), each = 4)),
  Bobot = c(12.1, 12.5, 11.9, 12.3, 15.2, 15.8, 14.9, 15.5, 18.2, 18.9, 18.0, 18.5),
  Panjang = c(8.2, 8.5, 8.0, 8.3, 10.1, 10.5, 9.9, 10.3, 12.2, 12.7, 12.0, 12.4)
)

res_manova <- sl_analyze_manova(df_manova, list(alpha = 0.05))

assert_true(!is.null(res_manova$main$pillai), "Pillai's trace present")
assert_true(!is.null(res_manova$main$wilks), "Wilks' lambda present")
assert_true(res_manova$main$pillai > 0.8, "Strong Pillai trace")
assert_true(res_manova$main$p_value < 0.001, "MANOVA p-value highly significant")
assert_true(length(res_manova$tables_apa) >= 2L, "MANOVA has at least 2 tables (Summary + Univariate)")

# ---------------------------------------------------------------------------
# Test 3: Kaplan-Meier Survival Analysis & Log-Rank Test
# ---------------------------------------------------------------------------
cat("--- 3. Testing Kaplan-Meier & Log-Rank Test ---\n")

df_km <- data.frame(
  Waktu_Jam = c(12, 24, 36, 48, 60, 72, 84, 96, 20, 30, 45, 55, 65, 80, 90, 96),
  Status_Sensor = c(1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0),
  Perlakuan = factor(c(rep("Probiotik", 8), rep("Kontrol", 8)))
)

res_km <- sl_analyze_kaplan_meier(df_km, list(alpha = 0.05, include_plots = TRUE))

assert_true(!is.null(res_km$main$chisq), "Log-Rank Chi-Square statistic present")
assert_true(!is.null(res_km$main$p_value), "Log-Rank p-value present")
assert_true(length(res_km$tables_apa) >= 1L, "Survival summary table present")
assert_true(length(res_km$plots) >= 1L, "Kaplan-Meier survival curve plot generated")

# ---------------------------------------------------------------------------
# Test 4: PCA & HCA
# ---------------------------------------------------------------------------
cat("--- 4. Testing PCA & HCA ---\n")

df_pca <- data.frame(
  DO = c(6.5, 7.2, 5.8, 6.9, 7.5, 6.1, 5.5, 7.0),
  pH = c(7.8, 8.1, 7.5, 8.0, 8.2, 7.7, 7.4, 7.9),
  Suhu = c(28.5, 29.0, 28.0, 28.8, 29.2, 28.2, 27.9, 28.7),
  Amonia = c(0.02, 0.01, 0.05, 0.02, 0.01, 0.04, 0.06, 0.02)
)

res_pca <- sl_analyze_pca_hca(df_pca, list(alpha = 0.05, include_plots = TRUE))

assert_true(length(res_pca$tables_apa) >= 2L, "PCA variance and loadings tables present")
assert_true(length(res_pca$plots) >= 1L, "PCA biplot generated")

# ---------------------------------------------------------------------------
# Test 5: Dispatcher Routing for all advanced modules
# ---------------------------------------------------------------------------
cat("--- 5. Testing Dispatcher Routing ---\n")

disp_rm <- sl_dispatch("run_analysis", list(analysis_type = "repeated_measures", data = df_rm, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_rm), "repeated_measures dispatched")

disp_manova <- sl_dispatch("run_analysis", list(analysis_type = "manova", data = df_manova, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_manova), "manova dispatched")

disp_km <- sl_dispatch("run_analysis", list(analysis_type = "kaplan_meier", data = df_km, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_km), "kaplan_meier dispatched")

disp_pca <- sl_dispatch("run_analysis", list(analysis_type = "pca_hca", data = df_pca, opts = list(include_plots = FALSE)))
assert_true(is.character(disp_pca), "pca_hca dispatched")

cat("=== All Advanced Engine Tests Passed ===\n")
