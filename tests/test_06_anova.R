# tests/test_06_anova.R
# Test Suite for StatLab One-Way ANOVA Engine (pure Base R)
# Validates against Desktop R base::aov, stats::TukeyHSD, stats::oneway.test,
# and agricolae::duncan.test (oracle) to <= 1e-8.

cat("\n=== Testing StatLab One-Way ANOVA Engine (R/06_anova.R) ===\n")

# Source prerequisite modules if needed
if (!exists("sl_analyze_anova", mode = "function")) {
  r_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  for (f in r_files) source(f)
}

# ---------------------------------------------------------------------------
# Test 1: Sweetpotato standard dataset verification
# ---------------------------------------------------------------------------
cat("--- 1. Testing ANOVA on Standard Sweetpotato Data ---\n")

sweetpotato <- data.frame(
  yield = c(21.7, 28.5, 23.0, 10.6, 14.9, 13.1, 28.0, 41.8, 39.2, 32.1, 40.4, 38.2),
  virus = factor(c("cc", "cc", "cc", "fc", "fc", "fc", "ff", "ff", "ff", "oo", "oo", "oo"))
)

# Reference R aov
ref_aov <- aov(yield ~ virus, data = sweetpotato)
ref_sum <- summary(ref_aov)[[1L]]
ref_ss_between <- ref_sum[["Sum Sq"]][1L]
ref_ss_within  <- ref_sum[["Sum Sq"]][2L]
ref_df_between <- ref_sum[["Df"]][1L]
ref_df_within  <- ref_sum[["Df"]][2L]
ref_ms_between <- ref_sum[["Mean Sq"]][1L]
ref_ms_within  <- ref_sum[["Mean Sq"]][2L]
ref_f_val      <- ref_sum[["F value"]][1L]
ref_p_val      <- ref_sum[["Pr(>F)"]][1L]

res <- sl_analyze_anova(sweetpotato, list(
  roles = list(mode = "long", group_col = "virus", response_cols = "yield"),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(!is.null(res$main$anova_table), "ANOVA table must be present in result")
assert_near(res$main$anova_table$ss_between, ref_ss_between, 1e-8, "SS between parity")
assert_near(res$main$anova_table$ss_within, ref_ss_within, 1e-8, "SS within parity")
assert_true(res$main$anova_table$df_between == ref_df_between, "df between parity")
assert_true(res$main$anova_table$df_within == ref_df_within, "df within parity")
assert_near(res$main$anova_table$ms_between, ref_ms_between, 1e-8, "MS between parity")
assert_near(res$main$anova_table$ms_within, ref_ms_within, 1e-8, "MS within parity")
assert_near(res$main$anova_table$f_statistic, ref_f_val, 1e-8, "F statistic parity")
assert_near(res$main$anova_table$p_value, ref_p_val, 1e-8, "p-value parity")

# ---------------------------------------------------------------------------
# Test 2: Post Hoc Letters (Tukey & Duncan) Parity
# ---------------------------------------------------------------------------
cat("--- 2. Testing Post Hoc Letters (Tukey & Duncan) ---\n")

# In sweetpotato:
# Groups by yield: oo (36.9) a, ff (36.3) a, cc (24.4) b, fc (12.9) c
assert_true(!is.null(res$posthoc), "Posthoc must be present")
assert_true(!is.null(res$posthoc$tukey), "Tukey must be present")
assert_true(!is.null(res$posthoc$duncan), "Duncan must be present")

# Check Duncan letters vs agricolae oracle if agricolae is installed
if (requireNamespace("agricolae", quietly = TRUE)) {
  oracle_d <- agricolae::duncan.test(ref_aov, "virus", alpha = 0.05, console = FALSE)
  for (grp_name in rownames(oracle_d$groups)) {
    expected_letter <- as.character(oracle_d$groups[grp_name, "groups"])
    actual_letter <- res$posthoc$duncan$group_letters[[grp_name]]
    assert_true(identical(actual_letter, expected_letter),
      sprintf("Duncan letter for group %s matches agricolae (%s == %s)", grp_name, actual_letter, expected_letter))
  }
}

# ---------------------------------------------------------------------------
# Test 3: Table Structure (Tabel Sidik Ragam & Rataan Perlakuan)
# ---------------------------------------------------------------------------
cat("--- 3. Testing Journal APA Tables Structure ---\n")

assert_true(length(res$tables_apa) >= 2L, "At least 2 APA tables (ANOVA Summary + Treatment Means) required")

# Table 1: ANOVA Summary
tbl_anova <- res$tables_apa[[1L]]
assert_true(tbl_anova$id == "anova_summary", "Table 1 must be anova_summary")
assert_true(length(tbl_anova$headers$id) >= 8L, "Table 1 headers must have at least 8 columns")
assert_true(length(tbl_anova$rows$id) == 3L, "Table 1 must have 3 rows (Between, Error, Total)")

# Table 2: Treatment Means & Letters
tbl_means <- res$tables_apa[[2L]]
assert_true(tbl_means$id == "treatment_means", "Table 2 must be treatment_means")
assert_true(length(tbl_means$rows$id) == 4L, "Table 2 must have 4 group rows")

# ---------------------------------------------------------------------------
# Test 4: Wide Mode Support (Tilapia Growth Feed Trial)
# ---------------------------------------------------------------------------
cat("--- 4. Testing Wide Mode Feed Trial ---\n")

wide_feed <- data.frame(
  Kontrol = c(12.4, 12.1, 12.8, 12.5),
  Pakan_A = c(14.2, 14.8, 14.5, 14.1),
  Pakan_B = c(16.5, 16.9, 16.2, 16.6),
  Pakan_C = c(15.1, 15.4, 15.0, 15.3)
)

res_wide <- sl_analyze_anova(wide_feed, list(
  roles = list(mode = "wide", response_cols = names(wide_feed)),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(res_wide$main$anova_table$p_value < 0.001, "Feed trial must be highly significant (p < 0.001)")
assert_true(length(res_wide$plots) >= 1L, "At least 1 plot generated")
assert_true(nzchar(res_wide$plots[[1L]]$png_data_url), "PNG data URL must be non-empty")

# ---------------------------------------------------------------------------
# Test 5: Dispatcher Routing
# ---------------------------------------------------------------------------
cat("--- 5. Testing Dispatcher Routing ---\n")

disp_json <- sl_dispatch("run_analysis", list(
  analysis_type = "oneway_anova",
  data = wide_feed,
  opts = list(alpha = 0.05, include_plots = FALSE)
))

assert_true(is.character(disp_json) && nchar(disp_json) > 100, "Dispatcher returns valid JSON string")
parsed <- jsonlite::fromJSON(disp_json)
assert_true(parsed$meta$analysis_type == "oneway_anova", "Parsed analysis_type matches oneway_anova")
assert_true(!is.null(parsed$main$anova_table), "Parsed ANOVA table is non-null")

cat("=== All One-Way ANOVA Engine Tests Passed ===\n")
