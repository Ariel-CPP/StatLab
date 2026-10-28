# tests/test_01_data_prep.R

# 1. Grubbs (1969) mass-spectrometer dataset vs outliers::grubbs.test to 1e-8
g_data <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)
out_res <- sl_detect_outliers(g_data, groups = factor(rep("G1", 8)), source_rows = 1:8, alpha = 0.05)
ref_g <- outliers::grubbs.test(g_data, type = 10, two.sided = TRUE)
assert_near(out_res$per_group[["G1"]]$grubbs$G, unname(ref_g$statistic["G"]), 1e-8, "Grubbs G vs CRAN outliers")
assert_near(out_res$per_group[["G1"]]$grubbs$p_value, ref_g$p.value, 1e-8, "Grubbs p vs CRAN outliers")
assert_true(out_res$per_group[["G1"]]$grubbs$is_outlier, "245.57 flagged by Grubbs")
assert_true(8L %in% out_res$flagged$source_row && any(out_res$flagged$by_iqr), "245.57 flagged by IQR too")

# 2. Review Focus #4: Jagged wide columns (Treatment A has 3 obs + 2 blanks, B has 5 obs)
df_wide <- data.frame(
  Pakan_A = c("10.2", "11.0", "10.8", "", ""),
  Pakan_B = c("14.1", "14.5", "13.9", "14.2", "14.8"),
  stringsAsFactors = FALSE
)
prep_w <- sl_prepare_groups(df_wide, mode = "wide", response_cols = c("Pakan_A", "Pakan_B"))
assert_true(prep_w$n_valid == 8L, "Jagged wide keeps all 3 + 5 = 8 valid replicates")
assert_true(sum(prep_w$groups == "Pakan_B") == 5L, "Pakan_B retains rows 4 and 5")

# 3. Review Focus #3: Dirty missing tokens ("NA", "-") vs unexpected text ("mati") in long mode
df_long <- data.frame(
  Perlakuan = c("K", "K", "K", "K", "P1", "P1", "P1"),
  Bobot = c("12.5", "NA", "-", "mati", "15.0", "15.4", "14.9"),
  stringsAsFactors = FALSE
)
prep_l <- sl_prepare_groups(df_long, mode = "long", response_cols = "Bobot", group_col = "Perlakuan")
assert_true(prep_l$n_valid == 4L, "4 numeric obs kept")
assert_true(prep_l$n_missing == 3L, "2 missing + 1 coerced counted in missing")
assert_true(identical(prep_l$coerced_non_numeric$rows, 4L), "Row 4 ('mati') flagged as unexpected text")

# 4. Explicit opt-in outlier exclusion (never silent)
prep_ex <- sl_prepare_groups(
  data.frame(Perlakuan = rep("G1", 8), Y = g_data),
  mode = "long", response_cols = "Y", group_col = "Perlakuan", exclude_rows = 8L
)
assert_true(prep_ex$n_valid == 7L && nrow(prep_ex$excluded_outliers) == 1L &&
            prep_ex$excluded_outliers$source_row[1] == 8L, "Explicit row 8 exclusion recorded")

# 5. Review Focus #2: Constant group (sd == 0) in outlier detection does not crash
const_out <- sl_detect_outliers(c(100, 100, 100, 100), groups = factor(rep("S100", 4)))
assert_true(identical(const_out$per_group[["S100"]]$grubbs$valid, FALSE), "Constant group Grubbs graceful")
assert_true(nrow(const_out$flagged) == 0L, "Constant group flags 0 outliers")

# --- Expanded Oracle & Edge-Case Tests ---

# 6. Additional Grubbs oracle checks across low-p, borderline-p, and high-p (both q <= 0.5 and q > 0.5)
data(iris)
data(ToothGrowth)

grubbs_oracle_cases <- list(
  low_tail_outlier = c(-35.2, 10.1, 10.4, 9.8, 10.2, 10.0, 9.9),
  n3_outlier       = c(10.0, 10.001, 50.0),
  n3_even_high_p   = c(10.0, 10.1, 10.2),
  seq5_high_p      = as.numeric(1:5),
  sym4_high_q      = c(-1.0, -1.0, 1.0, 1.0),
  setosa_pw_p018   = iris$Petal.Width[iris$Species == "setosa"],
  setosa_sw_p093   = iris$Sepal.Width[iris$Species == "setosa"],
  setosa_sl_p937   = iris$Sepal.Length[iris$Species == "setosa"],
  versic_sl_q898   = iris$Sepal.Length[iris$Species == "versicolor"],
  virgin_pw_p984   = iris$Petal.Width[iris$Species == "virginica"],
  tg_dose05        = ToothGrowth$len[ToothGrowth$dose == 0.5],
  tg_dose1_q779    = ToothGrowth$len[ToothGrowth$dose == 1.0],
  tg_dose2         = ToothGrowth$len[ToothGrowth$dose == 2.0]
)

for (case_nm in names(grubbs_oracle_cases)) {
  vec <- grubbs_oracle_cases[[case_nm]]
  ours <- sl_detect_outliers(vec, groups = factor(rep(case_nm, length(vec))), alpha = 0.05)
  ref  <- outliers::grubbs.test(vec, type = 10, two.sided = TRUE)
  g_obj <- ours$per_group[[case_nm]]$grubbs
  assert_true(isTRUE(g_obj$valid), paste("Grubbs valid for", case_nm))
  assert_near(g_obj$G, unname(ref$statistic["G"]), 1e-8, paste("Grubbs G parity:", case_nm))
  assert_near(g_obj$p_value, ref$p.value, 1e-8, paste("Grubbs p parity:", case_nm))
}

# Verify lower-tail outlier suspect_row and suspect_value
low_out_res <- sl_detect_outliers(grubbs_oracle_cases$low_tail_outlier, source_rows = 101:107, alpha = 0.05)
assert_true(
  identical(low_out_res$per_group[["all"]]$grubbs$suspect_row, 101L) &&
    isTRUE(low_out_res$per_group[["all"]]$grubbs$is_outlier),
  "Lower-tail outlier at custom source_row 101 flagged with groups=NULL"
)
assert_near(low_out_res$per_group[["all"]]$grubbs$suspect_value, -35.2, 1e-12, "Lower-tail suspect_value")

# Alpha sensitivity check on iris setosa Petal.Width (two-sided p ~= 0.018026)
pw_set <- iris$Petal.Width[iris$Species == "setosa"]
out_a05 <- sl_detect_outliers(pw_set, alpha = 0.05)
out_a01 <- sl_detect_outliers(pw_set, alpha = 0.01)
assert_true(out_a05$per_group[["all"]]$grubbs$is_outlier, "setosa Petal.Width flagged at alpha=0.05")
assert_true(!out_a01$per_group[["all"]]$grubbs$is_outlier, "setosa Petal.Width NOT flagged at alpha=0.01")

# Bimodal inlier sample (G < 1 SD, q = 1): p_value matches outliers::grubbs.test, but never falsely flagged as outlier
bimod_vec <- c(rep(10, 5), rep(12, 5))
bimod_res <- sl_detect_outliers(bimod_vec, alpha = 0.05)
bimod_ref <- outliers::grubbs.test(bimod_vec, type = 10, two.sided = TRUE)
assert_near(bimod_res$per_group[["all"]]$grubbs$G, unname(bimod_ref$statistic["G"]), 1e-8, "Bimodal G parity")
assert_near(bimod_res$per_group[["all"]]$grubbs$p_value, bimod_ref$p.value, 1e-8, "Bimodal p parity")
assert_true(!bimod_res$per_group[["all"]]$grubbs$is_outlier, "Bimodal inlier sample not falsely flagged as outlier")

# 7. Tiny groups (n = 1, n = 2) and multi-group IQR + Grubbs non-destructive behavior
tiny_df <- data.frame(
  G = c("N1", "N2", "N2", "Normal5", "Normal5", "Normal5", "Normal5", "Normal5"),
  X = c(42.0, 10.0, 12.0, 20.1, 20.3, 19.9, 20.0, 55.0),
  stringsAsFactors = FALSE
)
prep_tiny <- sl_prepare_groups(tiny_df, mode = "long", response_cols = "X", group_col = "G")
assert_true(identical(levels(prep_tiny$groups), c("N1", "N2", "Normal5")), "Factor levels preserve appearance order")
out_tiny <- sl_detect_outliers(prep_tiny$values, prep_tiny$groups, prep_tiny$source_rows, alpha = 0.05)
assert_true(!out_tiny$per_group[["N1"]]$grubbs$valid && nzchar(out_tiny$per_group[["N1"]]$grubbs$note_key),
            "n=1 Grubbs invalid with note_key")
assert_true(!out_tiny$per_group[["N2"]]$grubbs$valid && nzchar(out_tiny$per_group[["N2"]]$grubbs$note_key),
            "n=2 Grubbs invalid with note_key")
assert_true(out_tiny$per_group[["Normal5"]]$grubbs$valid && out_tiny$per_group[["Normal5"]]$grubbs$is_outlier,
            "Normal5 outlier 55.0 flagged")
assert_true(identical(out_tiny$flagged$source_row, 8L) &&
            out_tiny$flagged$by_iqr[1] && out_tiny$flagged$by_grubbs[1],
            "Row 8 flagged once with both by_iqr and by_grubbs TRUE")

# Verify 1.5*IQR bounds match Q1 - 1.5*IQR and Q3 + 1.5*IQR (type = 7)
n5_vals <- c(20.1, 20.3, 19.9, 20.0, 55.0)
q13 <- stats::quantile(n5_vals, probs = c(0.25, 0.75), names = FALSE, type = 7)
iqr_ref <- q13[2] - q13[1]
assert_near(out_tiny$per_group[["Normal5"]]$iqr_bounds,
            c(q13[1] - 1.5 * iqr_ref, q13[2] + 1.5 * iqr_ref),
            1e-12, "1.5*IQR bounds exact")

# 8. Wide mode internal missing vs trailing NA/blank, non-alphabetical groups, Indonesian comma decimals & warning-free coercion
warn_count <- 0L
withCallingHandlers(
  {
    df_wide_dirty <- data.frame(
      Kontrol = c(" 12,5 ", "", "13,0", "rusak", "", ""),
      Artemia = c("1.234,5", "N/A", "1.240,0", "1.238,2", "n/a", ""),
      stringsAsFactors = FALSE
    )
    pw_dirty <- sl_prepare_groups(df_wide_dirty, mode = "wide", response_cols = c("Kontrol", "Artemia"))
  },
  warning = function(w) {
    warn_count <<- warn_count + 1L
    invokeRestart("muffleWarning")
  }
)
assert_true(warn_count == 0L, "Zero coercion warnings emitted on dirty wide data")
# Kontrol occupied up to row 4 (2 valid, 1 internal blank row 2, 1 coerced row 4; rows 5-6 trailing blanks skipped)
# Artemia occupied up to row 5 (3 valid, 2 missing tokens row 2 & row 5; row 6 trailing blank skipped)
assert_true(identical(pw_dirty$n_raw_obs, 9L), "Wide n_raw_obs counts 4 + 5 occupied slots, skipping trailing blanks")
assert_true(identical(pw_dirty$n_valid, 5L), "Wide n_valid == 2 + 3 == 5")
assert_true(identical(pw_dirty$n_missing, 4L), "Wide n_missing == 2 + 2 == 4 internal missing/coerced")
assert_true(identical(pw_dirty$coerced_non_numeric$count, 1L) &&
            identical(pw_dirty$coerced_non_numeric$rows, 4L) &&
            identical(pw_dirty$coerced_non_numeric$cols, "Kontrol"),
            "Coerced cell in wide mode records count, row 4, col Kontrol")
assert_true(identical(levels(pw_dirty$groups), c("Kontrol", "Artemia")),
            "Non-alphabetical wide column order Kontrol -> Artemia preserved")
assert_near(pw_dirty$values, c(12.5, 13.0, 1234.5, 1240.0, 1238.2), 1e-12,
            "Indonesian comma & thousands-dot parsed accurately")

# Numeric wide data.frame with trailing NAs vs internal NA
df_wide_num <- data.frame(
  Kontrol = c(10.1, NA_real_, 10.5, NA_real_, NA_real_),
  Pakan_1 = c(15.2, 15.8, 16.0, 15.5, 15.9)
)
pw_num <- sl_prepare_groups(df_wide_num, mode = "wide", response_cols = c("Kontrol", "Pakan_1"))
assert_true(identical(pw_num$n_raw_obs, 8L) && identical(pw_num$n_valid, 7L) && identical(pw_num$n_missing, 1L),
            "Numeric wide trailing NAs skipped while internal NA counted in n_missing")

# Missing group_col in long mode counted in n_missing
df_long_gmiss <- data.frame(
  Grp = c("A", "", NA_character_, "B", "-"),
  Val = c("10", "20", "30", "40", "mati"),
  stringsAsFactors = FALSE
)
pl_gmiss <- sl_prepare_groups(df_long_gmiss, mode = "long", response_cols = "Val", group_col = "Grp")
assert_true(identical(pl_gmiss$n_raw_obs, 5L) &&
            identical(pl_gmiss$n_valid, 2L) &&
            identical(pl_gmiss$n_missing, 3L) &&
            identical(pl_gmiss$coerced_non_numeric$rows, 5L),
            "Missing group_col rows excluded listwise and counted once per row in n_missing")

# 9. Empty inputs, Latin-1 NBSP safety, wide exclude_rows, long group_col=NULL, JSON round-trip & Pure Base-R check
prep_empty <- sl_prepare_groups(data.frame(A = numeric(0), B = numeric(0)), mode = "wide", response_cols = c("A", "B"))
assert_true(identical(prep_empty$n_raw_obs, 0L) && identical(prep_empty$n_valid, 0L) &&
            identical(prep_empty$n_missing, 0L) && nrow(prep_empty$excluded_outliers) == 0L,
            "0-row wide data.frame handled cleanly")

out_empty <- sl_detect_outliers(numeric(0))
assert_true(length(out_empty$per_group) == 0L && nrow(out_empty$flagged) == 0L,
            "0-length sl_detect_outliers handled cleanly")

nbsp_str <- "12,5\xa0"
Encoding(nbsp_str) <- "latin1"
prep_nbsp <- sl_prepare_groups(data.frame(Y = c(nbsp_str, "13,0"), stringsAsFactors = FALSE),
                               mode = "long", response_cols = "Y", group_col = NULL)
assert_true(identical(prep_nbsp$n_valid, 2L) && identical(levels(prep_nbsp$groups), "Y"),
            "Latin-1 NBSP string & long-mode group_col=NULL handled without PCRE UTF-8 error")
assert_near(prep_nbsp$values, c(12.5, 13.0), 1e-12, "Latin-1 NBSP value parsed")

prep_w_ex <- sl_prepare_groups(df_wide, mode = "wide", response_cols = c("Pakan_A", "Pakan_B"), exclude_rows = 5L)
assert_true(identical(prep_w_ex$n_valid, 7L) && nrow(prep_w_ex$excluded_outliers) == 1L &&
            identical(prep_w_ex$excluded_outliers$group[1], "Pakan_B"),
            "Wide mode exclude_rows excludes occupied row 5 of Pakan_B and records it")

out_json <- sl_to_json(out_res)
out_parsed <- jsonlite::fromJSON(out_json, simplifyVector = FALSE)
assert_true(is.list(out_parsed$flagged) && length(out_parsed$flagged) == 1L &&
            identical(out_parsed$flagged[[1]]$source_row, 8L) &&
            is.list(out_parsed$per_group$G1$iqr_bounds) &&
            length(out_parsed$per_group$G1$iqr_bounds) == 2L,
            "sl_detect_outliers serializes cleanly via sl_to_json with boxed iqr_bounds")

r01_lines <- readLines("R/01_data_prep.R", encoding = "UTF-8", warn = FALSE)
r01_code <- paste(sub("#.*$", "", r01_lines), collapse = "\n")
assert_true(!grepl("(outliers|jsonlite|nortest|car)::|\\b(library|require|requireNamespace)\\s*\\(", r01_code),
            "R/01_data_prep.R uses strictly base and stats functions with zero external packages")
