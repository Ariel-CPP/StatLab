# tests/test_10_nonparametric.R
# Test suite for StatLab Non-Parametric Suite (pure Base R)
# Kruskal-Wallis, Mann-Whitney U, Wilcoxon Signed-Rank, and Friedman tests

cat("\n=== Testing StatLab Non-Parametric Suite (R/10_nonparametric.R) ===\n")

# ---------------------------------------------------------------------------
# Test 1: Kruskal-Wallis Test
# ---------------------------------------------------------------------------
cat("--- 1. Testing Kruskal-Wallis Test ---\n")

df_kw_long <- data.frame(
  Perlakuan = factor(rep(c("Kontrol", "Dosis_A", "Dosis_B"), each = 4)),
  Bakteri_CFU = c(120, 150, 135, 140, 60, 75, 80, 70, 20, 25, 15, 30)
)

res_kw <- sl_analyze_kruskal_wallis(df_kw_long, list(
  roles = list(mode = "long", response_cols = "Bakteri_CFU", group_col = "Perlakuan"),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_kw), "Kruskal-Wallis returns a list")
assert_true(!is.null(res_kw$main$h_statistic), "H-statistic present")
assert_true(res_kw$main$df == 2L, "df is 2 for 3 groups")
assert_true(res_kw$main$p_value < 0.01, "Kruskal-Wallis p-value highly significant")
assert_true(length(res_kw$tables_apa) >= 2L, "KW has at least 2 tables (Summary + Post-Hoc)")
assert_true(length(res_kw$plots) >= 2L, "KW generates at least 2 plots (Boxplot + Mean Rank)")
assert_true(grepl("• \\*\\*Signifikansi Kruskal-Wallis\\*\\*", res_kw$interpretation$id), "Key points format in KW")

# Test wide mode for KW
df_kw_wide <- data.frame(
  Kontrol = c(120, 150, 135, 140),
  Dosis_A = c(60, 75, 80, 70),
  Dosis_B = c(20, 25, 15, 30)
)
res_kw_wide <- sl_analyze_kruskal_wallis(df_kw_wide, list(
  roles = list(mode = "wide", response_cols = c("Kontrol", "Dosis_A", "Dosis_B")),
  alpha = 0.05,
  include_plots = FALSE
))
assert_near(res_kw_wide$main$h_statistic, res_kw$main$h_statistic, tol = 1e-8, msg = "KW wide and long match")

# ---------------------------------------------------------------------------
# Test 2: Mann-Whitney U Test (Two Independent Samples)
# ---------------------------------------------------------------------------
cat("--- 2. Testing Mann-Whitney U Test ---\n")

df_mwu <- data.frame(
  Kontrol = c(12.5, 11.8, 13.0, 12.2, 11.5, 12.8),
  Probiotik = c(15.2, 16.0, 15.8, 16.5, 17.0, 15.5)
)

res_mwu <- sl_analyze_mann_whitney(df_mwu, list(
  roles = list(mode = "wide", response_cols = c("Kontrol", "Probiotik")),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_mwu), "Mann-Whitney returns a list")
assert_true(!is.null(res_mwu$main$u_statistic), "U-statistic present")
assert_true(!is.null(res_mwu$main$w_statistic), "W-statistic present")
assert_true(!is.null(res_mwu$main$z_score), "Z-score present")
assert_true(res_mwu$main$p_value < 0.01, "Mann-Whitney p-value significant")
assert_true(!is.null(res_mwu$main$effect_size_r), "Effect size r present")
assert_true(length(res_mwu$tables_apa) >= 1L, "MWU has summary table")
assert_true(length(res_mwu$plots) >= 1L, "MWU has plot")

# ---------------------------------------------------------------------------
# Test 3: Wilcoxon Signed-Rank Test (Paired Samples)
# ---------------------------------------------------------------------------
cat("--- 3. Testing Wilcoxon Signed-Rank Test ---\n")

df_wilcox_paired <- data.frame(
  Sebelum = c(185, 192, 178, 205, 190, 210, 198, 182),
  Sesudah = c(165, 170, 175, 180, 172, 185, 178, 168)
)

res_pair <- sl_analyze_wilcoxon_signed(df_wilcox_paired, list(
  roles = list(mode = "wide", response_cols = c("Sebelum", "Sesudah")),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_pair), "Wilcoxon Signed returns a list")
assert_true(!is.null(res_pair$main$v_statistic), "V-statistic present")
assert_true(!is.null(res_pair$main$p_value), "p-value present")
assert_true(res_pair$main$p_value < 0.05, "Wilcoxon signed p-value significant")
assert_true(length(res_pair$tables_apa) >= 1L, "Wilcoxon signed has table")
assert_true(length(res_pair$plots) >= 1L, "Wilcoxon signed has plot")

# ---------------------------------------------------------------------------
# Test 4: Friedman Test (Non-Parametric Repeated Measures)
# ---------------------------------------------------------------------------
cat("--- 4. Testing Friedman Test ---\n")

df_friedman <- data.frame(
  Minggu_0 = c(10.2, 9.8, 10.5, 11.0, 9.5),
  Minggu_2 = c(12.5, 11.9, 12.8, 13.2, 12.0),
  Minggu_4 = c(15.8, 15.2, 16.0, 16.5, 15.0),
  Minggu_6 = c(18.5, 17.8, 19.0, 19.5, 18.2)
)

res_friedman <- sl_analyze_friedman(df_friedman, list(
  roles = list(mode = "wide", response_cols = c("Minggu_0", "Minggu_2", "Minggu_4", "Minggu_6")),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_friedman), "Friedman returns a list")
assert_true(!is.null(res_friedman$main$chi_square), "Chi-Square statistic present")
assert_true(res_friedman$main$df == 3L, "df is 3 for 4 time points")
assert_true(res_friedman$main$p_value < 0.01, "Friedman p-value significant")
assert_true(length(res_friedman$tables_apa) >= 2L, "Friedman has at least 2 tables (Summary + Post-Hoc)")
assert_true(length(res_friedman$plots) >= 1L, "Friedman has trajectory plot")

# ---------------------------------------------------------------------------
# Test 5: Dispatcher Routing
# ---------------------------------------------------------------------------
cat("--- 5. Testing Dispatcher Routing ---\n")

disp_kw <- sl_dispatch("run_analysis", list(
  analysis_type = "kruskal_wallis",
  data = df_kw_wide,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_kw), "kruskal_wallis dispatched")

disp_mwu <- sl_dispatch("run_analysis", list(
  analysis_type = "mann_whitney",
  data = df_mwu,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_mwu), "mann_whitney dispatched")

disp_ws <- sl_dispatch("run_analysis", list(
  analysis_type = "wilcoxon_signed",
  data = df_wilcox_paired,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_ws), "wilcoxon_signed dispatched")

disp_fr <- sl_dispatch("run_analysis", list(
  analysis_type = "friedman",
  data = df_friedman,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_fr), "friedman dispatched")

cat("=== All Non-Parametric Suite Tests Passed ===\n")
