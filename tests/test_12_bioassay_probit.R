# tests/test_12_bioassay_probit.R
# Test Suite for StatLab Bioassay & Dose-Response Engine (R/12_bioassay_probit.R)
# Tests LC10, LC50, LC90 estimation via Probit and Logit binomial GLM models.

cat("\n=== Testing StatLab Bioassay & Dose-Response Engine (R/12_bioassay_probit.R) ===\n")

# ---------------------------------------------------------------------------
# Test Data: Pathogen Challenge Bioassay (Vibrio parahaemolyticus in Penaeus vannamei)
# ---------------------------------------------------------------------------
df_bioassay <- data.frame(
  Konsentrasi_ppm = c(0, 10, 25, 50, 100, 200),
  Mortalitas_Ekor = c(0, 2, 6, 11, 16, 20),
  Total_Uji = c(20, 20, 20, 20, 20, 20)
)

# ---------------------------------------------------------------------------
# Test 1: Standard Probit Analysis
# ---------------------------------------------------------------------------
cat("--- 1. Testing Probit LC50 Analysis ---\n")

res_probit <- sl_analyze_probit_lc50(df_bioassay, list(
  roles = list(
    dose_col = "Konsentrasi_ppm",
    dead_col = "Mortalitas_Ekor",
    total_col = "Total_Uji"
  ),
  link = "probit",
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_probit), "Probit returns a list")
assert_true(!is.null(res_probit$main$lc50), "LC50 is calculated")
assert_true(!is.null(res_probit$main$lc10), "LC10 is calculated")
assert_true(!is.null(res_probit$main$lc90), "LC90 is calculated")

# Verify monotonic order of lethal concentrations
assert_true(res_probit$main$lc10 < res_probit$main$lc50, "LC10 < LC50")
assert_true(res_probit$main$lc50 < res_probit$main$lc90, "LC50 < LC90")

# Verify sensible LC50 range (for this data, 50% mortality of 20 is 10, between 25 and 50 ppm)
assert_true(res_probit$main$lc50 > 25 && res_probit$main$lc50 < 60, "LC50 is in realistic range [25, 60] ppm")

# Verify 95% Confidence Interval
assert_true(res_probit$main$lc50_ci_low < res_probit$main$lc50, "LC50 CI Low < LC50")
assert_true(res_probit$main$lc50_ci_high > res_probit$main$lc50, "LC50 CI High > LC50")

# Verify slope and chi-square goodness of fit
assert_true(res_probit$main$slope > 0, "Slope is positive (higher dose -> higher mortality)")
assert_true(!is.null(res_probit$main$chisq_stat), "Goodness of fit Chi-Square stat present")
assert_true(!is.null(res_probit$main$chisq_p), "Goodness of fit p-value present")

# Verify APA Tables structure
assert_true(length(res_probit$tables_apa) >= 2L, "Bioassay has at least 2 APA tables (Model Parameters & LC Summary)")
assert_true(length(res_probit$plots) >= 1L, "Bioassay has sigmoidal response curve plot")

# Verify Key Points formatting
assert_true(grepl("• \\*\\*Lethal Concentration", res_probit$interpretation$id), "Poin kunci LC50 present in interpretation")

# ---------------------------------------------------------------------------
# Test 2: Logit Analysis Option
# ---------------------------------------------------------------------------
cat("--- 2. Testing Logit LC50 Analysis ---\n")

res_logit <- sl_analyze_probit_lc50(df_bioassay, list(
  roles = list(
    dose_col = "Konsentrasi_ppm",
    dead_col = "Mortalitas_Ekor",
    total_col = "Total_Uji"
  ),
  link = "logit",
  alpha = 0.05,
  include_plots = FALSE
))

assert_true(is.list(res_logit), "Logit returns a list")
assert_true(!is.null(res_logit$main$lc50), "Logit LC50 is calculated")
# Logit and Probit LC50 should be very close (within 10%)
lc50_diff_pct <- abs(res_logit$main$lc50 - res_probit$main$lc50) / res_probit$main$lc50 * 100
assert_true(lc50_diff_pct < 10, "Logit and Probit LC50 are consistent within 10%")

# ---------------------------------------------------------------------------
# Test 3: Wide Format Style (Columns: Konsentrasi, Mati, Hidup)
# ---------------------------------------------------------------------------
cat("--- 3. Testing Alive Column Variant ---\n")

df_alive <- data.frame(
  Dosis = c(0, 10, 25, 50, 100, 200),
  Mati = c(0, 2, 6, 11, 16, 20),
  Hidup = c(20, 18, 14, 9, 4, 0)
)

res_alive <- sl_analyze_probit_lc50(df_alive, list(
  roles = list(
    dose_col = "Dosis",
    dead_col = "Mati",
    alive_col = "Hidup"
  ),
  include_plots = FALSE
))
assert_near(res_alive$main$lc50, res_probit$main$lc50, tol = 1e-6, "Alive column variant matches Total column LC50")

# ---------------------------------------------------------------------------
# Test 4: Dispatcher Routing
# ---------------------------------------------------------------------------
cat("--- 4. Testing Dispatcher Routing ---\n")

disp_probit <- sl_dispatch("run_analysis", list(
  analysis_type = "probit_lc50",
  data = df_bioassay,
  opts = list(
    roles = list(dose_col = "Konsentrasi_ppm", dead_col = "Mortalitas_Ekor", total_col = "Total_Uji"),
    include_plots = FALSE
  )
))
assert_true(is.character(disp_probit), "probit_lc50 dispatched")

disp_probit_alias <- sl_dispatch("run_analysis", list(
  analysis_type = "probit",
  data = df_bioassay,
  opts = list(
    roles = list(dose_col = "Konsentrasi_ppm", dead_col = "Mortalitas_Ekor", total_col = "Total_Uji"),
    include_plots = FALSE
  )
))
assert_true(is.character(disp_probit_alias), "probit alias dispatched")

cat("=== All Bioassay & Dose-Response Engine Tests Passed ===\n")
