# tests/test_04_descriptive.R

# 0. RFC-4648 Section 10 test vectors for pure-R sl_base64_encode
rfc_vectors <- list(
  list(plain = "",       b64 = ""),
  list(plain = "f",      b64 = "Zg=="),
  list(plain = "fo",     b64 = "Zm8="),
  list(plain = "foo",    b64 = "Zm9v"),
  list(plain = "foob",   b64 = "Zm9vYg=="),
  list(plain = "fooba",  b64 = "Zm9vYmE="),
  list(plain = "foobar", b64 = "Zm9vYmFy")
)
for (tv in rfc_vectors) {
  raw_in <- charToRaw(tv$plain)
  got <- sl_base64_encode(raw_in)
  assert_true(identical(got, tv$b64), sprintf("RFC-4648 base64('%s') == '%s'", tv$plain, tv$b64))
}
all_bytes_b64 <- sl_base64_encode(as.raw(0:255))
assert_true(
  nchar(all_bytes_b64) == 344L && grepl("^[A-Za-z0-9+/]+={0,2}$", all_bytes_b64),
  "sl_base64_encode encodes all 256 byte values cleanly"
)

# 1. Dispatcher healthcheck
hc <- jsonlite::fromJSON(sl_dispatch("healthcheck", list()), simplifyVector = FALSE)
assert_true(isTRUE(hc$ok) && identical(hc$engine, "StatLab"), "Dispatcher healthcheck ok")

# 2. Main iris descriptive run via sl_dispatch (with 300-DPI PNG plot capture)
data(iris)
res_json <- sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = iris[, c("Sepal.Length", "Species")],
  roles = list(mode = "long", response_cols = "Sepal.Length", group_col = "Species"),
  opts = list(alpha = 0.05, exclude_rows = integer(0), include_plots = TRUE)
))
res <- jsonlite::fromJSON(res_json, simplifyVector = FALSE)

# Check meta & top-level contract keys
assert_true(identical(res$meta$analysis_type, "descriptive"), "meta analysis_type")
assert_true(identical(res$meta$n_raw, 150L) && identical(res$meta$n_valid, 150L) &&
            identical(res$meta$n_missing, 0L) && identical(res$meta$n_excluded_outliers, 0L),
            "meta sample counts for iris")
assert_true(identical(unlist(res$meta$group_names), c("setosa", "versicolor", "virginica")),
            "meta group_names array")
assert_true(is.null(res$posthoc) && is.null(res$effect_size) &&
            is.list(res$alternatives) && length(res$alternatives) == 0L,
            "posthoc=null, effect_size=null, alternatives=[]")

# Check setosa exact descriptive numbers against base R & SPSS/Excel G1, G2 formulas
setosa <- iris$Sepal.Length[iris$Species == "setosa"]
g_set <- res$main$groups[[1]]
assert_true(identical(g_set$group, "setosa") && identical(g_set$n, 50L) &&
            identical(g_set$n_missing, 0L), "setosa n=50, n_missing=0")
assert_near(g_set$mean, mean(setosa), 1e-8, "setosa mean")
assert_near(g_set$sd, sd(setosa), 1e-8, "setosa sd")
assert_near(g_set$se, sd(setosa) / sqrt(50), 1e-8, "setosa se")
assert_near(g_set$median, median(setosa), 1e-8, "setosa median")
q_ref <- quantile(setosa, probs = c(0.25, 0.75), names = FALSE, type = 7)
assert_near(g_set$q1, q_ref[1], 1e-8, "setosa Q1")
assert_near(g_set$q3, q_ref[2], 1e-8, "setosa Q3")
assert_near(g_set$iqr, IQR(setosa), 1e-8, "setosa IQR")
assert_near(g_set$min, min(setosa), 1e-8, "setosa min")
assert_near(g_set$max, max(setosa), 1e-8, "setosa max")
assert_near(g_set$cv_pct, (sd(setosa) / mean(setosa)) * 100, 1e-8, "setosa CV%")

n_s <- length(setosa)
z_s <- (setosa - mean(setosa)) / sd(setosa)
g1_ref <- (n_s / ((n_s - 1) * (n_s - 2))) * sum(z_s^3)
g2_ref <- ((n_s * (n_s + 1)) / ((n_s - 1) * (n_s - 2) * (n_s - 3))) * sum(z_s^4) -
  (3 * (n_s - 1)^2) / ((n_s - 2) * (n_s - 3))
assert_near(g_set$skewness, g1_ref, 1e-8, "setosa SPSS/Excel G1 skewness")
assert_near(g_set$kurtosis, g2_ref, 1e-8, "setosa SPSS/Excel G2 excess kurtosis")

ci_ref <- t.test(setosa, conf.level = 0.95)$conf.int
assert_near(g_set$ci_low, ci_ref[1], 1e-8, "setosa 95% CI low")
assert_near(g_set$ci_high, ci_ref[2], 1e-8, "setosa 95% CI high")

# 3. Check plots are non-empty 300-DPI PNG data URLs
assert_true(length(res$plots) >= 2L, "At least Q-Q and Boxplot generated")
for (plt in res$plots) {
  assert_true(nzchar(plt$id) && nzchar(plt$title$id) && nzchar(plt$title$en),
              paste("Plot metadata complete:", plt$id))
  assert_true(startsWith(plt$png_data_url, "data:image/png;base64,iVBOR"),
              paste("Valid PNG header in base64:", plt$id))
  assert_true(is.numeric(plt$width_in) && plt$width_in > 0 &&
              is.numeric(plt$height_in) && plt$height_in > 0,
              paste("Plot dimensions valid:", plt$id))
}

# 4. Check APA & SPSS Full Tables + KaTeX Formulas
assert_true(length(res$tables_apa) >= 1L, "Layer-1 APA table present")
apa1 <- res$tables_apa[[1]]
assert_true(length(apa1$rows$id) == 3L && length(apa1$rows$en) == 3L, "APA table has 3 species rows")
assert_true(any(grepl(",", unlist(apa1$rows$id))) && !any(grepl("[0-9]\\.[0-9]", unlist(apa1$rows$id))),
            "APA ID rows use Indonesian decimal comma without English dot decimals")
assert_true(any(grepl("[0-9]\\.[0-9]", unlist(apa1$rows$en))),
            "APA EN rows use English decimal dot")

full_ids <- vapply(res$tables_full, function(tb) tb$id, character(1L))
assert_true(all(c("descriptives_full", "normality_full", "homogeneity_full", "outliers_full") %in% full_ids),
            "All 4 SPSS-style Layer-2 full tables present")

assert_true(length(res$formulas) >= 3L && all(vapply(res$formulas, function(f) {
  nzchar(f$label$id) && nzchar(f$label$en) && nzchar(f$latex)
}, logical(1L))), "KaTeX formulas present with bilingual labels")

# 5. Check bilingual prose & Indonesian decimal comma vs English dot
assert_true(grepl("setosa", res$interpretation$id) && grepl(",", res$interpretation$id),
            "ID prose uses group name + comma")
assert_true(!grepl("[0-9]\\.[0-9]", res$interpretation$id),
            "ID prose contains zero English dot-decimals")
assert_true(grepl("setosa", res$interpretation$en) && grepl("\\.", res$interpretation$en),
            "EN prose uses group name + dot")

# 6. Check General Conclusion compilation (normal & Spec §6.2 assumption-violated items)
gc_json <- sl_dispatch("general_conclusion", list(
  items = list(
    list(
      title = "Bobot Ikan",
      analysis_type = "descriptive",
      chosen_test = "Statistik Deskriptif",
      effective_test = "Statistik Deskriptif",
      assumption_violated = res$assumptions$normality_violated || res$assumptions$homogeneity_violated,
      summary_id = res$interpretation$id,
      summary_en = res$interpretation$en
    ),
    list(
      title = "Pertumbuhan Nila",
      analysis_type = "anova_1way",
      chosen_test = "ANOVA Satu Arah",
      effective_test = "Kruskal-Wallis",
      assumption_violated = TRUE,
      summary_id = "Perlakuan pakan berbeda nyata (p = 0,004).",
      summary_en = "Feed treatments differed significantly (p = 0.004)."
    )
  ),
  lang = "id"
))
gc <- jsonlite::fromJSON(gc_json, simplifyVector = FALSE)
assert_true(nzchar(gc$id) && nzchar(gc$en), "General conclusion bilingual output even when lang='id'")
assert_true(grepl("Kruskal-Wallis", gc$id) && grepl("ANOVA Satu Arah", gc$id) &&
            grepl("Kruskal-Wallis", gc$en) && grepl("ANOVA Satu Arah", gc$en),
            "Spec §6.2: General conclusion names both effective_test and chosen_test on violation")

# 7. Review Focus #5: Zero-mean vector produces null cv_pct in JSON without crashing
df_zero <- data.frame(G = rep("Anomali", 5), X = c(-2, -1, 0, 1, 2))
z_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = df_zero,
  roles = list(mode = "long", response_cols = "X", group_col = "G"),
  opts = list(alpha = 0.05, include_plots = FALSE)
)), simplifyVector = FALSE)
assert_true(is.null(z_res$main$groups[[1]]$cv_pct), "Zero-mean CV% serializes cleanly to null")
assert_true(is.list(z_res$plots) && length(z_res$plots) == 0L, "include_plots=FALSE returns empty array []")

# 8. Alpha = 0.01 CI check (99% confidence interval)
res_a01 <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = iris[, c("Sepal.Length", "Species")],
  roles = list(mode = "long", response_cols = "Sepal.Length", group_col = "Species"),
  opts = list(alpha = 0.01, exclude_rows = integer(0), include_plots = FALSE)
)), simplifyVector = FALSE)
ci99_ref <- t.test(setosa, conf.level = 0.99)$conf.int
assert_near(res_a01$main$groups[[1]]$ci_low, ci99_ref[1], 1e-8, "setosa 99% CI low (alpha=0.01)")
assert_near(res_a01$main$groups[[1]]$ci_high, ci99_ref[2], 1e-8, "setosa 99% CI high (alpha=0.01)")

# 9. Review Focus #2: Constant group c(100, 100, 100) + tiny group n=2 WITH include_plots = TRUE
warn_edge <- 0L
const_res <- NULL
single_const_res <- NULL
withCallingHandlers(
  {
    const_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
      analysis_type = "descriptive",
      data = list(
        Kontrol_100 = c(100, 100, 100),
        Perlakuan_A = c(85.2, 88.4, 91.0, 86.5, 42.0),
        Kecil_N2    = c(75.0, 79.0)
      ),
      roles = list(mode = "wide", response_cols = c("Kontrol_100", "Perlakuan_A", "Kecil_N2")),
      opts = list(alpha = 0.05, include_plots = TRUE)
    )), simplifyVector = FALSE)

    single_const_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
      analysis_type = "descriptive",
      data = list(SR_Konstan = c(100, 100, 100)),
      roles = list(mode = "wide", response_cols = "SR_Konstan"),
      opts = list(alpha = 0.05, include_plots = TRUE)
    )), simplifyVector = FALSE)
  },
  warning = function(w) {
    warn_edge <<- warn_edge + 1L
    invokeRestart("muffleWarning")
  }
)
assert_true(warn_edge == 0L, "Zero R warnings on constant c(100,100,100), jagged wide list, and n=2 with plots")
g_c100 <- const_res$main$groups[[1]]
assert_true(identical(g_c100$n, 3L) && g_c100$sd == 0 && g_c100$se == 0 &&
            g_c100$ci_low == 100 && g_c100$ci_high == 100 &&
            is.null(g_c100$skewness) && is.null(g_c100$kurtosis),
            "Constant group c(100,100,100) stats & CI=100 without t.test crash")
assert_true(identical(const_res$main$groups[[2]]$n, 5L) &&
            identical(const_res$main$groups[[3]]$n, 2L) &&
            identical(const_res$meta$n_missing, 0L),
            "Jagged wide named-list preserves all replicates with 0 false missing")
assert_true(length(const_res$plots) >= 2L &&
            startsWith(const_res$plots[[1]]$png_data_url, "data:image/png;base64,iVBOR") &&
            length(single_const_res$plots) >= 2L &&
            startsWith(single_const_res$plots[[1]]$png_data_url, "data:image/png;base64,iVBOR"),
            "Plots render cleanly to valid PNGs even for constant c(100,100,100) groups")

# 10. Review Focus #3: Coerced non-numeric text cell emits bilingual warning naming row index
df_dirty <- data.frame(
  Perlakuan = c("K", "K", "K", "K", "P1", "P1", "P1"),
  Bobot = c("12,5", "13,0", "NA", "mati", "15,0", "15,4", "14,9"),
  stringsAsFactors = FALSE
)
dirty_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = df_dirty,
  roles = list(mode = "long", response_cols = "Bobot", group_col = "Perlakuan"),
  opts = list(alpha = 0.05, include_plots = FALSE)
)), simplifyVector = FALSE)
assert_true(any(grepl("4", unlist(dirty_res$warnings$id))) &&
            any(grepl("4", unlist(dirty_res$warnings$en))),
            "Coerced non-numeric cell ('mati' at row 4) named in bilingual warnings")
assert_true(identical(dirty_res$main$groups[[1]]$n, 2L) &&
            identical(dirty_res$main$groups[[1]]$n_missing, 2L),
            "Per-group n_missing tracks both 'NA' and coerced 'mati' in group K")

# 11. Static zero-CRAN check across all R/*.R modules
r_all <- sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))
assert_true(length(r_all) >= 7L, "All 7 Phase-1 R modules exist in R/")
for (rf in r_all) {
  lines <- readLines(rf, encoding = "UTF-8", warn = FALSE)
  code_only <- paste(sub("#.*$", "", lines), collapse = "\n")
  has_forbidden <- grepl(
    "(jsonlite|base64enc|outliers|nortest|car|e1071|ggplot2)::|\\b(library|require|requireNamespace)\\s*\\(",
    code_only
  )
  assert_true(!has_forbidden, paste("Zero external/CRAN package calls in", basename(rf)))
}

# 12. Performance (< 15 ms for 60 KB), jsonlite::base64_enc oracle, and device/RNG hygiene
raw_60kb <- as.raw((0:61439) %% 256L)
invisible(sl_base64_encode(raw_60kb[1:12]))
t0 <- proc.time()[["elapsed"]]
for (iter_b64 in 1:10) {
  b64_60kb <- sl_base64_encode(raw_60kb)
}
elapsed_ms <- ((proc.time()[["elapsed"]] - t0) / 10) * 1000
ref_b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(raw_60kb))
assert_true(identical(b64_60kb, ref_b64), "sl_base64_encode matches jsonlite::base64_enc on 60 KB binary payload")
assert_true(elapsed_ms < 15, sprintf("sl_base64_encode 60 KB < 15 ms budget (%.2f ms)", elapsed_ms))

# Error inside sl_capture_plot never leaks open graphics devices and preserves existing caller device
outer_tmp <- tempfile(fileext = ".svg")
grDevices::svg(outer_tmp, width = 4, height = 3)
outer_dev <- grDevices::dev.cur()
devs_before <- grDevices::dev.list()

err_caught <- tryCatch({
  sl_capture_plot(function() stop("simulated plot error"))
  FALSE
}, error = function(e) TRUE)
assert_true(err_caught, "sl_capture_plot propagates plot_fn error")
assert_true(identical(grDevices::dev.list(), devs_before) &&
            identical(grDevices::dev.cur(), outer_dev),
            "sl_capture_plot cleans up leaked device on error and restores outer active device")

ok_cap <- sl_capture_plot(function() {
  sl_plot_qq(c(0, 0, 0, 0, 5), title = "Tied Quartiles", lang = "en", outlier_idx = 5L)
})
assert_true(startsWith(ok_cap$png_data_url, "data:image/png;base64,iVBOR") &&
            identical(grDevices::dev.cur(), outer_dev),
            "sl_capture_plot restores outer active device after successful capture")
grDevices::dev.off(outer_dev)
if (file.exists(outer_tmp)) unlink(outer_tmp)

# RNG seed immutability across plot generation
set.seed(20261028L)
seed_before <- .Random.seed
invisible(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = iris[1:30, c("Sepal.Length", "Species")],
  roles = list(mode = "long", response_cols = "Sepal.Length", group_col = "Species"),
  opts = list(alpha = 0.05, include_plots = TRUE)
)))
assert_true(identical(.Random.seed, seed_before), "Plot capture never mutates global .Random.seed")

# 13. Non-destructive outlier flagging vs explicit opt-in exclude_rows + direct R overflow safety
g_out_df <- data.frame(
  Perlakuan = rep("Uji_Grubbs", 8),
  Y = c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57),
  stringsAsFactors = FALSE
)
keep_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = g_out_df,
  roles = list(mode = "long", response_cols = "Y", group_col = "Perlakuan"),
  opts = list(alpha = 0.05, exclude_rows = integer(0), include_plots = FALSE)
)), simplifyVector = FALSE)
assert_true(identical(keep_res$meta$n_valid, 8L) &&
            length(keep_res$outliers$flagged) == 1L &&
            identical(keep_res$outliers$flagged[[1]]$source_row, 8L),
            "Outlier at row 8 flagged but retained when exclude_rows is empty")

excl_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = g_out_df,
  roles = list(mode = "long", response_cols = "Y", group_col = "Perlakuan"),
  opts = list(alpha = 0.05, exclude_rows = 8L, include_plots = FALSE)
)), simplifyVector = FALSE)
assert_true(identical(excl_res$meta$n_valid, 7L) &&
            identical(excl_res$meta$n_excluded_outliers, 1L) &&
            grepl("8", excl_res$interpretation$id) &&
            grepl("8", excl_res$interpretation$en),
            "Explicit exclude_rows=8L recorded in meta and bilingual interpretation")

# Direct R call on extreme overflow values returns NA_real_ (not Inf/NaN) in group summary fields
ovf_direct <- sl_analyze_descriptive(
  data.frame(Big = c(-1e200, 0, 1e200, 2e200)),
  list(mode = "wide", response_cols = "Big", include_plots = FALSE)
)
g_ovf <- ovf_direct$main$groups[[1]]
assert_true(is.na(g_ovf$sd) && is.na(g_ovf$se) && is.na(g_ovf$ci_low) && is.na(g_ovf$skewness),
            "Extreme overflow sd/se/ci normalized to NA_real_ in direct R output")

# Multi-group (> 6 groups) plot branch + empty general_conclusion items
df_7grp <- data.frame(
  G = rep(paste0("Kolam_", 1:7), each = 4),
  DO = seq(4.5, 7.2, length.out = 28),
  stringsAsFactors = FALSE
)
res_7grp <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = df_7grp,
  roles = list(mode = "long", response_cols = "DO", group_col = "G"),
  opts = list(alpha = 0.05, include_plots = TRUE, lang = "en")
)), simplifyVector = FALSE)
assert_true(length(res_7grp$plots) == 3L &&
            startsWith(res_7grp$plots[[1]]$png_data_url, "data:image/png;base64,iVBOR"),
            "> 6 groups pooled diagnostic plots render cleanly")

gc_empty <- jsonlite::fromJSON(sl_dispatch("general_conclusion", list(items = list())), simplifyVector = FALSE)
assert_true(nzchar(gc_empty$id) && nzchar(gc_empty$en), "Empty general_conclusion returns clean bilingual notice")

# n = 1 group + n = 0 empty-vector direct plot calls
res_n1 <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = list(Tunggal = 42.5, Kelompok_B = c(10.1, 11.2, 10.8, 11.0)),
  roles = list(mode = "wide", response_cols = c("Tunggal", "Kelompok_B")),
  opts = list(alpha = 0.05, include_plots = TRUE)
)), simplifyVector = FALSE)
assert_true(identical(res_n1$main$groups[[1]]$n, 1L) &&
            is.null(res_n1$main$groups[[1]]$sd) &&
            is.null(res_n1$main$groups[[1]]$ci_low) &&
            length(res_n1$plots) == 3L &&
            startsWith(res_n1$plots[[1]]$png_data_url, "data:image/png;base64,iVBOR"),
            "n=1 group runs cleanly with null sd/CI and valid 300-DPI PNG plots")

empty_plot_cap <- sl_capture_plot(function() {
  graphics::par(mfrow = c(1L, 3L))
  sl_plot_qq(numeric(0), title = "Empty Q-Q", lang = "id")
  sl_plot_hist_density(numeric(0), title = "Empty Hist", xlab = "X", lang = "id")
  sl_plot_box_jitter(numeric(0), character(0), title = "Empty Box", ylab = "Y", lang = "id")
})
assert_true(startsWith(empty_plot_cap$png_data_url, "data:image/png;base64,iVBOR"),
            "n=0 empty vectors render informative placeholder plots without error")

# 14. Fix Round 1 regressions:
# (a) Wide-mode overlapping row indices (outlier at row 1 in ONLY ONE column must NOT flag row 1 of other columns)
df_wide_row1 <- data.frame(
  Kontrol     = c(84.5, 85.0, 86.2, 85.8, 84.9, 85.4),
  Perlakuan_A = c(42.0, 86.1, 88.4, 89.2, 87.5, 88.0),
  Perlakuan_B = c(90.1, 91.2, 89.8, 90.5, 91.0, 90.3)
)
prep_wr1 <- sl_prepare_groups(
  df_wide_row1,
  mode = "wide",
  response_cols = c("Kontrol", "Perlakuan_A", "Perlakuan_B")
)
out_wr1 <- sl_detect_outliers(prep_wr1$values, prep_wr1$groups, prep_wr1$source_rows, alpha = 0.05)
assert_true(
  nrow(out_wr1$flagged) == 1L &&
    identical(out_wr1$flagged$group[1], "Perlakuan_A") &&
    identical(out_wr1$flagged$source_row[1], 1L),
  "Wide row-1 fixture flags strictly Perlakuan_A row 1"
)

mask_wr1 <- .sl_flagged_outlier_mask(prep_wr1, out_wr1)
assert_true(
  is.logical(mask_wr1) &&
    length(mask_wr1) == length(prep_wr1$values) &&
    sum(mask_wr1) == 1L &&
    identical(as.character(prep_wr1$groups[mask_wr1]), "Perlakuan_A") &&
    identical(prep_wr1$source_rows[mask_wr1], 1L) &&
    !any(mask_wr1[prep_wr1$groups %in% c("Kontrol", "Perlakuan_B")]),
  "Wide-mode outlier mask matches (group, source_row) pair without bleeding into Kontrol/Perlakuan_B row 1"
)

# Verify actual rendered box-jitter graphics stream draws '#1' once (only for Perlakuan_A), not 3 times
tmp_pdf_box <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf_box, width = 6.5, height = 4.2, compress = FALSE)
box_meta <- sl_plot_box_jitter(
  values = prep_wr1$values,
  groups = prep_wr1$groups,
  title = "Wide Row-1 Outlier Check",
  ylab = "SR (%)",
  lang = "en",
  outlier_rows = out_wr1$flagged$source_row,
  source_rows = prep_wr1$source_rows,
  outlier_groups = out_wr1$flagged$group
)
grDevices::dev.off()
pdf_box_lines <- grep("Tj|TJ", readLines(tmp_pdf_box, warn = FALSE), useBytes = TRUE, value = TRUE)
pdf_box_txt <- paste(pdf_box_lines, collapse = "\n")
if (file.exists(tmp_pdf_box)) unlink(tmp_pdf_box)

hash1_hits <- gregexpr("(#1)", pdf_box_txt, fixed = TRUE, useBytes = TRUE)[[1]]
hash1_count <- if (identical(hash1_hits[1], -1L)) 0L else length(hash1_hits)
assert_true(
  !is.null(box_meta) &&
    sum(box_meta$outlier_mask) == 1L &&
    identical(box_meta$groups[box_meta$outlier_mask], "Perlakuan_A") &&
    identical(hash1_count, 1L),
  "sl_plot_box_jitter renders crimson '#1' label strictly once (for Perlakuan_A only)"
)

# Verify per-group Q-Q panels in wide mode only mark Perlakuan_A (0 outliers in Kontrol & Perlakuan_B)
tmp_pdf_qq <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf_qq, width = 7.5, height = 3.6, compress = FALSE)
graphics::par(mfrow = c(1L, 3L))
qq_counts <- integer(3L)
g_chr_wr1 <- as.character(prep_wr1$groups)
lvls_wr1 <- levels(prep_wr1$groups)
for (j in seq_along(lvls_wr1)) {
  idx_j <- which(g_chr_wr1 == lvls_wr1[j])
  qq_meta_j <- sl_plot_qq(
    prep_wr1$values[idx_j],
    title = paste0("Q-Q: ", lvls_wr1[j]),
    lang = "en",
    outlier_idx = which(mask_wr1[idx_j])
  )
  qq_counts[j] <- sum(qq_meta_j$outlier_mask)
}
grDevices::dev.off()
if (file.exists(tmp_pdf_qq)) unlink(tmp_pdf_qq)
assert_true(
  identical(qq_counts, c(0L, 1L, 0L)),
  "Wide-mode Q-Q panels flag 0 outliers in Kontrol, 1 in Perlakuan_A, 0 in Perlakuan_B"
)

# Full end-to-end sl_dispatch on 7 wide columns (>6 pooled Q-Q branch) with row-1 outlier in Kolam_2 only
wide_7col <- list(
  Kolam_1 = c(80.1, 80.5, 81.0, 80.8, 80.4, 80.7),
  Kolam_2 = c(35.0, 80.4, 80.9, 81.1, 80.6, 80.8),
  Kolam_3 = c(81.0, 81.3, 80.9, 81.2, 81.5, 81.1),
  Kolam_4 = c(79.8, 80.2, 80.0, 79.9, 80.3, 80.1),
  Kolam_5 = c(82.0, 82.4, 81.9, 82.2, 82.1, 82.3),
  Kolam_6 = c(80.5, 80.9, 80.7, 80.6, 81.0, 80.8),
  Kolam_7 = c(81.2, 81.6, 81.4, 81.3, 81.7, 81.5)
)
res_w7 <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = wide_7col,
  roles = list(mode = "wide", response_cols = names(wide_7col)),
  opts = list(alpha = 0.05, include_plots = TRUE, lang = "id")
)), simplifyVector = FALSE)
assert_true(
  length(res_w7$outliers$flagged) == 1L &&
    identical(res_w7$outliers$flagged[[1]]$group, "Kolam_2") &&
    identical(res_w7$outliers$flagged[[1]]$source_row, 1L) &&
    length(res_w7$plots) == 3L,
  "7-column wide analysis with row-1 outlier in Kolam_2 completes cleanly with plots"
)

# (b) sl_plot_hist_density subtitle for n = 1 must NOT claim 'SD = 0 (constant)'
tmp_pdf_h1 <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf_h1, width = 6.5, height = 4.2, compress = FALSE)
h1_en <- sl_plot_hist_density(c(42.5), title = "Single EN", xlab = "Weight", lang = "en")
h1_id <- sl_plot_hist_density(c(42.5), title = "Single ID", xlab = "Bobot", lang = "id")
grDevices::dev.off()
pdf_h1_lines <- grep("Tj|TJ", readLines(tmp_pdf_h1, warn = FALSE), useBytes = TRUE, value = TRUE)
pdf_h1_txt <- paste(pdf_h1_lines, collapse = "\n")
if (file.exists(tmp_pdf_h1)) unlink(tmp_pdf_h1)

assert_true(
  !grepl("SD = 0", pdf_h1_txt, fixed = TRUE, useBytes = TRUE) &&
    grepl("n = 1", pdf_h1_txt, fixed = TRUE, useBytes = TRUE) &&
    !is.null(h1_en) && identical(h1_en$subtitle, "n = 1 (single observation)") &&
    !is.null(h1_id) && identical(h1_id$subtitle, "n = 1 (observasi tunggal)"),
  "sl_plot_hist_density labels n=1 as 'n = 1' rather than 'SD = 0'"
)

tmp_pdf_hc <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf_hc, width = 6.5, height = 4.2, compress = FALSE)
hc_en <- sl_plot_hist_density(c(100, 100, 100), title = "Const EN", xlab = "SR", lang = "en")
hc_id <- sl_plot_hist_density(c(100, 100, 100), title = "Const ID", xlab = "SR", lang = "id")
grDevices::dev.off()
pdf_hc_lines <- grep("Tj|TJ", readLines(tmp_pdf_hc, warn = FALSE), useBytes = TRUE, value = TRUE)
pdf_hc_txt <- paste(pdf_hc_lines, collapse = "\n")
if (file.exists(tmp_pdf_hc)) unlink(tmp_pdf_hc)

sd0_hits <- gregexpr("SD = 0", pdf_hc_txt, fixed = TRUE, useBytes = TRUE)[[1]]
sd0_count <- if (identical(sd0_hits[1], -1L)) 0L else length(sd0_hits)
assert_true(
  identical(sd0_count, 2L) &&
    identical(hc_en$subtitle, "SD = 0 (constant)") &&
    identical(hc_id$subtitle, "SD = 0 (konstan)"),
  "sl_plot_hist_density still labels n >= 2 constant vectors as 'SD = 0 (constant)' / 'SD = 0 (konstan)'"
)
