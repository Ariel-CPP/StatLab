# R/07_ttest.R
# StatLab Independent & Paired Two-Sample t-Test Engine (pure Base R)
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

#' Independent Two-Sample t-Test (Student, Welch & Mann-Whitney U)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_ttest_independent <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  mode <- if (!is.null(roles$mode) && roles$mode %in% c("wide", "long")) roles$mode else "wide"
  response_cols <- if (!is.null(roles$response_cols)) as.character(roles$response_cols) else character(0L)
  group_col <- if (!is.null(roles$group_col) && nzchar(as.character(roles$group_col[[1L]]))) as.character(roles$group_col[[1L]]) else NULL
  exclude_rows <- if (!is.null(opts$exclude_rows)) as.integer(opts$exclude_rows) else integer(0L)

  if (length(response_cols) == 0L) {
    if (mode == "wide") {
      num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
      response_cols <- if (length(num_cols) >= 2L) num_cols[1L:2L] else names(df)[1L:min(2L, length(names(df)))]
    } else {
      cols <- names(df)
      group_col <- if (!is.null(group_col)) group_col else cols[1L]
      num_cols <- cols[cols != group_col]
      response_cols <- if (length(num_cols) > 0L) num_cols[1L] else cols[2L]
    }
  }

  prep <- sl_prepare_groups(
    df = df,
    mode = mode,
    response_cols = response_cols,
    group_col = group_col,
    exclude_rows = exclude_rows
  )

  grp_levels <- levels(prep$groups)
  if (length(grp_levels) != 2L) {
    stop(sprintf("Independent t-test requires exactly 2 groups (found %d: %s). For 3+ groups, use ANOVA.",
      length(grp_levels), paste(grp_levels, collapse = ", ")))
  }

  g1_nm <- grp_levels[1L]
  g2_nm <- grp_levels[2L]
  x1 <- prep$values[prep$groups == g1_nm]
  x2 <- prep$values[prep$groups == g2_nm]
  n1 <- length(x1)
  n2 <- length(x2)

  if (n1 < 2L || n2 < 2L) {
    stop("Independent t-test requires at least 2 observations in each group.")
  }

  resp_label <- if (mode == "long") response_cols[1L] else sprintf("%s vs %s", g1_nm, g2_nm)

  m1 <- mean(x1, na.rm = TRUE)
  m2 <- mean(x2, na.rm = TRUE)
  sd1 <- stats::sd(x1, na.rm = TRUE)
  sd2 <- stats::sd(x2, na.rm = TRUE)
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  diff_mean <- m1 - m2

  # Assumptions: Normality & Homogeneity
  assumptions <- sl_check_assumptions(
    values = prep$values,
    groups = prep$groups,
    alpha = alpha,
    require_homogeneity = TRUE
  )
  outliers <- sl_detect_outliers(prep$values, prep$groups, prep$source_rows, alpha = alpha)

  # 1. Student t-test (Equal Variance)
  t_student <- tryCatch(stats::t.test(x1, x2, var.equal = TRUE, conf.level = 1.0 - alpha), error = function(e) NULL)
  
  # Pooled SD & Cohen's d
  df_student <- n1 + n2 - 2L
  s_pooled <- sqrt(((n1 - 1L) * sd1^2 + (n2 - 1L) * sd2^2) / df_student)
  cohen_d <- if (is.finite(s_pooled) && s_pooled > 0) diff_mean / s_pooled else 0.0

  # 2. Welch t-test (Unequal Variance)
  t_welch <- tryCatch(stats::t.test(x1, x2, var.equal = FALSE, conf.level = 1.0 - alpha), error = function(e) NULL)

  # 3. Mann-Whitney U / Wilcoxon Rank Sum
  mwu <- tryCatch(stats::wilcox.test(x1, x2, exact = FALSE), error = function(e) NULL)

  # Variance Ratio F-test
  f_test <- tryCatch(stats::var.test(x1, x2, conf.level = 1.0 - alpha), error = function(e) NULL)

  # =========================================================================
  # Build Tables
  # =========================================================================

  # Table 1: Primary Comparison Table
  t_headers_id <- sl_box_vec(c("Metode Uji", "Statistik", "db", "P-Value", "Beda Rataan", "95% CI Bawah", "95% CI Atas", "Effect Size (d)", "Signifikansi"))
  t_headers_en <- sl_box_vec(c("Test Method", "Statistic", "df", "P-Value", "Mean Difference", "Lower 95% CI", "Upper 95% CI", "Effect Size (d)", "Significance"))

  rows_id <- list()
  rows_en <- list()

  # Student
  if (!is.null(t_student)) {
    p_stu <- t_student$p.value
    sig_stu <- if (p_stu < 0.01) "**" else if (p_stu < 0.05) "*" else "tn"
    sig_stu_en <- if (sig_stu == "tn") "ns" else sig_stu

    rows_id[[1L]] <- sl_box_vec(c(
      "Uji-t Student (Varians Sama)",
      sprintf("t = %s", sl_fmt_num(t_student$statistic, 4L, "id")),
      as.character(df_student),
      sl_fmt_p(p_stu, "id"),
      sl_fmt_num(diff_mean, 4L, "id"),
      sl_fmt_num(t_student$conf.int[1L], 4L, "id"),
      sl_fmt_num(t_student$conf.int[2L], 4L, "id"),
      sl_fmt_num(cohen_d, 4L, "id"),
      sig_stu
    ))

    rows_en[[1L]] <- sl_box_vec(c(
      "Student's t-test (Equal Variance)",
      sprintf("t = %s", sl_fmt_num(t_student$statistic, 4L, "en")),
      as.character(df_student),
      sl_fmt_p(p_stu, "en"),
      sl_fmt_num(diff_mean, 4L, "en"),
      sl_fmt_num(t_student$conf.int[1L], 4L, "en"),
      sl_fmt_num(t_student$conf.int[2L], 4L, "en"),
      sl_fmt_num(cohen_d, 4L, "en"),
      sig_stu_en
    ))
  }

  # Welch
  if (!is.null(t_welch)) {
    p_wel <- t_welch$p.value
    sig_wel <- if (p_wel < 0.01) "**" else if (p_wel < 0.05) "*" else "tn"
    sig_wel_en <- if (sig_wel == "tn") "ns" else sig_wel

    rows_id[[2L]] <- sl_box_vec(c(
      "Uji-t Welch (Varians Heterogen / Robust)",
      sprintf("t = %s", sl_fmt_num(t_welch$statistic, 4L, "id")),
      sl_fmt_num(t_welch$parameter, 2L, "id"),
      sl_fmt_p(p_wel, "id"),
      sl_fmt_num(diff_mean, 4L, "id"),
      sl_fmt_num(t_welch$conf.int[1L], 4L, "id"),
      sl_fmt_num(t_welch$conf.int[2L], 4L, "id"),
      sl_fmt_num(cohen_d, 4L, "id"),
      sig_wel
    ))

    rows_en[[2L]] <- sl_box_vec(c(
      "Welch's t-test (Unequal Variance / Robust)",
      sprintf("t = %s", sl_fmt_num(t_welch$statistic, 4L, "en")),
      sl_fmt_num(t_welch$parameter, 2L, "en"),
      sl_fmt_p(p_wel, "en"),
      sl_fmt_num(diff_mean, 4L, "en"),
      sl_fmt_num(t_welch$conf.int[1L], 4L, "en"),
      sl_fmt_num(t_welch$conf.int[2L], 4L, "en"),
      sl_fmt_num(cohen_d, 4L, "en"),
      sig_wel_en
    ))
  }

  # Mann-Whitney U
  if (!is.null(mwu)) {
    p_mwu <- mwu$p.value
    sig_mwu <- if (p_mwu < 0.01) "**" else if (p_mwu < 0.05) "*" else "tn"
    sig_mwu_en <- if (sig_mwu == "tn") "ns" else sig_mwu

    rows_id[[3L]] <- sl_box_vec(c(
      "Uji Mann-Whitney U (Non-Parametrik)",
      sprintf("W = %s", sl_fmt_num(mwu$statistic, 2L, "id")),
      "\u2014",
      sl_fmt_p(p_mwu, "id"),
      sl_fmt_num(diff_mean, 4L, "id"),
      "\u2014",
      "\u2014",
      "\u2014",
      sig_mwu
    ))

    rows_en[[3L]] <- sl_box_vec(c(
      "Mann-Whitney U Test (Non-Parametric)",
      sprintf("W = %s", sl_fmt_num(mwu$statistic, 2L, "en")),
      "\u2014",
      sl_fmt_p(p_mwu, "en"),
      sl_fmt_num(diff_mean, 4L, "en"),
      "\u2014",
      "\u2014",
      "\u2014",
      sig_mwu_en
    ))
  }

  tbl_primary <- list(
    id = "ttest_summary",
    title = list(
      id = sprintf("Tabel 1. Ringkasan Uji Komparasi Dua Sampel Independen (%s vs %s)", g1_nm, g2_nm),
      en = sprintf("Table 1. Two-Sample Independent Comparisons Summary (%s vs %s)", g1_nm, g2_nm)
    ),
    headers = list(id = t_headers_id, en = t_headers_en),
    rows = list(id = rows_id, en = rows_en),
    note = list(
      id = "Jika ragam terbukti heterogen (Levene p < 0,05), gunakan Uji-t Welch. Jika normalitas terlanggar, gunakan Mann-Whitney U.",
      en = "If variance is heterogeneous (Levene p < 0.05), use Welch's t-test. If normality is violated, use Mann-Whitney U."
    )
  )

  # Table 2: Descriptive Summary
  desc_headers_id <- sl_box_vec(c("Kelompok", "N", "Rataan", "Standar Deviasi (SD)", "Standar Error (SE)", "Median", "IQR"))
  desc_headers_en <- sl_box_vec(c("Group", "N", "Mean", "Standard Deviation (SD)", "Standard Error (SE)", "Median", "IQR"))

  q1_1 <- stats::quantile(x1, 0.25, na.rm = TRUE)
  q3_1 <- stats::quantile(x1, 0.75, na.rm = TRUE)
  q1_2 <- stats::quantile(x2, 0.25, na.rm = TRUE)
  q3_2 <- stats::quantile(x2, 0.75, na.rm = TRUE)

  desc_rows_id <- list(
    sl_box_vec(c(g1_nm, as.character(n1), sl_fmt_num(m1, 2L, "id"), sl_fmt_num(sd1, 2L, "id"), sl_fmt_num(se1, 2L, "id"), sl_fmt_num(stats::median(x1), 2L, "id"), sl_fmt_num(q3_1 - q1_1, 2L, "id"))),
    sl_box_vec(c(g2_nm, as.character(n2), sl_fmt_num(m2, 2L, "id"), sl_fmt_num(sd2, 2L, "id"), sl_fmt_num(se2, 2L, "id"), sl_fmt_num(stats::median(x2), 2L, "id"), sl_fmt_num(q3_2 - q1_2, 2L, "id")))
  )
  desc_rows_en <- list(
    sl_box_vec(c(g1_nm, as.character(n1), sl_fmt_num(m1, 2L, "en"), sl_fmt_num(sd1, 2L, "en"), sl_fmt_num(se1, 2L, "en"), sl_fmt_num(stats::median(x1), 2L, "en"), sl_fmt_num(q3_1 - q1_1, 2L, "en"))),
    sl_box_vec(c(g2_nm, as.character(n2), sl_fmt_num(m2, 2L, "en"), sl_fmt_num(sd2, 2L, "en"), sl_fmt_num(se2, 2L, "en"), sl_fmt_num(stats::median(x2), 2L, "en"), sl_fmt_num(q3_2 - q1_2, 2L, "en")))
  )

  tbl_desc <- list(
    id = "ttest_groups",
    title = list(
      id = "Tabel 2. Statistik Deskriptif Kedua Kelompok Pengamatan",
      en = "Table 2. Group Descriptive Statistics"
    ),
    headers = list(id = desc_headers_id, en = desc_headers_en),
    rows = list(id = desc_rows_id, en = desc_rows_en),
    note = list(id = "Nilai statistik dasar per kelompok perlakuan.", en = "Baseline group descriptive values.")
  )

  tables_apa <- list(tbl_primary, tbl_desc)
  tables_full <- list()

  # =========================================================================
  # Plots (Bar Chart & Boxplot)
  # =========================================================================
  plots <- list()
  if (include_plots) {
    p_bar <- sl_capture_plot(function() {
      sl_plot_bar_letters(
        means = c(m1, m2),
        ses = c(se1, se2),
        labels = c(g1_nm, g2_nm),
        letters = c(if (t_welch$p.value < alpha) "a" else "a", if (t_welch$p.value < alpha) "b" else "a"),
        ylab = resp_label,
        title = sprintf("Perbandingan Rataan %s vs %s", g1_nm, g2_nm),
        lang = plot_lang
      )
    }, width_in = 6.0, height_in = 4.2, res = 300)

    plots[[1L]] <- list(
      id = "ttest_barchart",
      title = list(id = "Diagram Batang Komparasi Rataan", en = "Mean Comparison Bar Chart"),
      png_data_url = p_bar$png_data_url,
      width_in = p_bar$width_in,
      height_in = p_bar$height_in
    )
  }

  # =========================================================================
  # Interpretation
  # =========================================================================
  eff_p <- if (assumptions$homogeneity_violated && !is.null(t_welch)) t_welch$p.value else t_student$p.value
  eff_t <- if (assumptions$homogeneity_violated && !is.null(t_welch)) t_welch$statistic else t_student$statistic
  eff_df <- if (assumptions$homogeneity_violated && !is.null(t_welch)) t_welch$parameter else df_student
  eff_method <- if (assumptions$homogeneity_violated) "Uji-t Welch" else "Uji-t Student"

  interp_id <- if (eff_p < 0.05) {
    higher_grp <- if (m1 > m2) g1_nm else g2_nm
    lower_grp  <- if (m1 > m2) g2_nm else g1_nm
    diff_abs   <- abs(diff_mean)
    sprintf(
      "Hasil analisis menggunakan %s menunjukkan terdapat perbedaan yang signifikan secara statistik antara kelompok %s dan %s pada parameter %s (t(%s) = %s, p = %s; Cohen's d = %s). Kelompok %s memiliki rataan yang lebih tinggi sebesar %s dibandingkan %s (%s).",
      eff_method, g1_nm, g2_nm, resp_label,
      sl_fmt_num(eff_df, 1L, "id"), sl_fmt_num(eff_t, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(cohen_d, 2L, "id"),
      higher_grp, sl_fmt_num(max(m1, m2), 2L, "id"), lower_grp, sl_fmt_num(min(m1, m2), 2L, "id")
    )
  } else {
    sprintf(
      "Hasil analisis menggunakan %s menunjukkan tidak terdapat perbedaan yang signifikan secara statistik antara kelompok %s dan %s pada parameter %s (t(%s) = %s, p = %s; Cohen's d = %s). Perbedaan nilai rataan kedua kelompok masih berada dalam rentang variasi acak sampling.",
      eff_method, g1_nm, g2_nm, resp_label,
      sl_fmt_num(eff_df, 1L, "id"), sl_fmt_num(eff_t, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(cohen_d, 2L, "id")
    )
  }

  interp_en <- if (eff_p < 0.05) {
    higher_grp <- if (m1 > m2) g1_nm else g2_nm
    lower_grp  <- if (m1 > m2) g2_nm else g1_nm
    sprintf(
      "Analysis via %s indicated a statistically significant difference between %s and %s for %s, t(%s) = %s, p = %s (Cohen's d = %s). Group %s demonstrated a significantly higher mean (%s) than %s (%s).",
      if (assumptions$homogeneity_violated) "Welch's t-test" else "Student's t-test",
      g1_nm, g2_nm, resp_label,
      sl_fmt_num(eff_df, 1L, "en"), sl_fmt_num(eff_t, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(cohen_d, 2L, "en"),
      higher_grp, sl_fmt_num(max(m1, m2), 2L, "en"), lower_grp, sl_fmt_num(min(m1, m2), 2L, "en")
    )
  } else {
    sprintf(
      "Analysis via %s indicated no statistically significant difference between %s and %s for %s, t(%s) = %s, p = %s (Cohen's d = %s). The observed mean difference is consistent with random sampling variability.",
      if (assumptions$homogeneity_violated) "Welch's t-test" else "Student's t-test",
      g1_nm, g2_nm, resp_label,
      sl_fmt_num(eff_df, 1L, "en"), sl_fmt_num(eff_t, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(cohen_d, 2L, "en")
    )
  }

  formulas <- list(
    list(
      title = list(id = "Statistik Uji-t Student (Varians Sama)", en = "Student's t-Statistic"),
      latex = "t = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_p \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}}}, \\quad s_p = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1 + n_2 - 2}}"
    ),
    list(
      title = list(id = "Effect Size Cohen's d", en = "Cohen's d Effect Size"),
      latex = "d = \\frac{\\bar{X}_1 - \\bar{X}_2}{s_p}"
    )
  )

  meta <- list(
    analysis_type = "independent_t",
    analysis_label = list(id = "Uji-t Dua Sampel Independen", en = "Two-Sample Independent t-Test"),
    response_label = resp_label,
    mode = mode,
    alpha = alpha,
    n_raw = as.integer(prep$n_raw_obs),
    n_valid = as.integer(prep$n_valid),
    n_missing = as.integer(prep$n_missing),
    n_excluded_outliers = as.integer(nrow(prep$excluded_outliers)),
    excluded_rows = sl_box_vec(as.integer(prep$excluded_outliers$source_row)),
    group_names = sl_box_vec(c(g1_nm, g2_nm)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = assumptions,
    outliers = outliers,
    main = list(
      student = list(statistic = t_student$statistic, df = df_student, p_value = t_student$p.value, cohen_d = cohen_d),
      welch = list(statistic = t_welch$statistic, df = t_welch$parameter, p_value = t_welch$p.value),
      groups = list(
        list(group = g1_nm, n = n1, mean = m1, sd = sd1, se = se1),
        list(group = g2_nm, n = n2, mean = m2, sd = sd2, se = se2)
      )
    ),
    posthoc = NULL,
    effect_size = list(cohen_d = cohen_d),
    alternatives = list(welch = t_welch, mann_whitney = mwu),
    tables_apa = tables_apa,
    tables_full = tables_full,
    formulas = formulas,
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Paired Two-Sample t-Test Engine (Before vs After)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_ttest_paired <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  cols <- names(df)[vapply(df, is.numeric, logical(1L))]
  if (length(cols) < 2L) {
    cols <- names(df)[1L:min(2L, length(names(df)))]
  }
  if (length(cols) < 2L) {
    stop("Paired t-test requires at least 2 paired numeric columns (e.g. Sebelum and Sesudah).")
  }

  col_before <- cols[1L]
  col_after  <- cols[2L]

  v1 <- as.numeric(df[[col_before]])
  v2 <- as.numeric(df[[col_after]])

  valid_idx <- which(is.finite(v1) & is.finite(v2))
  if (length(valid_idx) < 3L) {
    stop("Paired t-test requires at least 3 paired observations with valid numbers.")
  }

  x1 <- v1[valid_idx]
  x2 <- v2[valid_idx]
  n <- length(x1)
  d <- x2 - x1 # Sesudah - Sebelum

  m1 <- mean(x1)
  m2 <- mean(x2)
  md <- mean(d)
  sd_d <- stats::sd(d)
  se_d <- sd_d / sqrt(n)
  cohen_dz <- if (is.finite(sd_d) && sd_d > 0) md / sd_d else 0.0

  # Paired t-test
  t_pair <- stats::t.test(x2, x1, paired = TRUE, conf.level = 1.0 - alpha)
  wilc_pair <- tryCatch(stats::wilcox.test(x2, x1, paired = TRUE, exact = FALSE), error = function(e) NULL)

  # Normality of differences
  diff_sw <- sl_shapiro_test(d)
  diff_lillie <- sl_lilliefors_test(d)

  # Table 1: Primary Paired Summary
  p_t <- t_pair$p.value
  sig_mark <- if (p_t < 0.01) "**" else if (p_t < 0.05) "*" else "tn"
  sig_mark_en <- if (sig_mark == "tn") "ns" else sig_mark

  headers_id <- sl_box_vec(c("Metode Uji", "Statistik", "db", "P-Value", "Rataan Selisih (d)", "SE Selisih", "95% CI Bawah", "95% CI Atas", "Signifikansi"))
  headers_en <- sl_box_vec(c("Test Method", "Statistic", "df", "P-Value", "Mean Difference (d)", "SE Difference", "Lower 95% CI", "Upper 95% CI", "Significance"))

  rows_id <- list(
    sl_box_vec(c(
      "Uji-t Berpasangan (Paired t-test)",
      sprintf("t = %s", sl_fmt_num(t_pair$statistic, 4L, "id")),
      as.character(n - 1L),
      sl_fmt_p(p_t, "id"),
      sl_fmt_num(md, 4L, "id"),
      sl_fmt_num(se_d, 4L, "id"),
      sl_fmt_num(t_pair$conf.int[1L], 4L, "id"),
      sl_fmt_num(t_pair$conf.int[2L], 4L, "id"),
      sig_mark
    ))
  )
  rows_en <- list(
    sl_box_vec(c(
      "Paired Samples t-test",
      sprintf("t = %s", sl_fmt_num(t_pair$statistic, 4L, "en")),
      as.character(n - 1L),
      sl_fmt_p(p_t, "en"),
      sl_fmt_num(md, 4L, "en"),
      sl_fmt_num(se_d, 4L, "en"),
      sl_fmt_num(t_pair$conf.int[1L], 4L, "en"),
      sl_fmt_num(t_pair$conf.int[2L], 4L, "en"),
      sig_mark_en
    ))
  )

  if (!is.null(wilc_pair)) {
    p_w <- wilc_pair$p.value
    sig_w <- if (p_w < 0.01) "**" else if (p_w < 0.05) "*" else "tn"
    sig_w_en <- if (sig_w == "tn") "ns" else sig_w

    rows_id[[2L]] <- sl_box_vec(c(
      "Uji Wilcoxon Signed-Rank (Non-Parametrik)",
      sprintf("V = %s", sl_fmt_num(wilc_pair$statistic, 2L, "id")),
      "\u2014",
      sl_fmt_p(p_w, "id"),
      sl_fmt_num(md, 4L, "id"),
      "\u2014", "\u2014", "\u2014",
      sig_w
    ))
    rows_en[[2L]] <- sl_box_vec(c(
      "Wilcoxon Signed-Rank Test (Non-Parametric)",
      sprintf("V = %s", sl_fmt_num(wilc_pair$statistic, 2L, "en")),
      "\u2014",
      sl_fmt_p(p_w, "en"),
      sl_fmt_num(md, 4L, "en"),
      "\u2014", "\u2014", "\u2014",
      sig_w_en
    ))
  }

  tbl_primary <- list(
    id = "paired_ttest_summary",
    title = list(
      id = sprintf("Tabel 1. Ringkasan Uji-t Berpasangan (%s vs %s)", col_before, col_after),
      en = sprintf("Table 1. Paired Samples t-Test Summary (%s vs %s)", col_before, col_after)
    ),
    headers = list(id = headers_id, en = headers_en),
    rows = list(id = rows_id, en = rows_en),
    note = list(
      id = sprintf("Selisih d = %s - %s. Koefisien Cohen's d_z = %s.", col_after, col_before, sl_fmt_num(cohen_dz, 2L, "id")),
      en = sprintf("Difference d = %s - %s. Cohen's d_z = %s.", col_after, col_before, sl_fmt_num(cohen_dz, 2L, "en"))
    )
  )

  interp_id <- if (p_t < 0.05) {
    arah <- if (md > 0) "peningkatan" else "penurunan"
    sprintf(
      "Hasil uji-t berpasangan menunjukkan terdapat perbedaan yang signifikan secara statistik antara %s dan %s (t(%d) = %s, p = %s; Cohen's d_z = %s). Terjadi %s nilai rataan sebesar %s (rataan awal: %s, rataan akhir: %s).",
      col_before, col_after, n - 1L, sl_fmt_num(t_pair$statistic, 3L, "id"), sl_fmt_p(p_t, "id"), sl_fmt_num(cohen_dz, 2L, "id"),
      arah, sl_fmt_num(abs(md), 2L, "id"), sl_fmt_num(m1, 2L, "id"), sl_fmt_num(m2, 2L, "id")
    )
  } else {
    sprintf(
      "Hasil uji-t berpasangan menunjukkan tidak terdapat perbedaan yang signifikan secara statistik antara %s dan %s (t(%d) = %s, p = %s; Cohen's d_z = %s). Perubahan rataan sebesar %s tidak melampaui batas variasi acak.",
      col_before, col_after, n - 1L, sl_fmt_num(t_pair$statistic, 3L, "id"), sl_fmt_p(p_t, "id"), sl_fmt_num(cohen_dz, 2L, "id"),
      sl_fmt_num(md, 2L, "id")
    )
  }

  interp_en <- if (p_t < 0.05) {
    direction <- if (md > 0) "an increase" else "a decrease"
    sprintf(
      "Paired t-test analysis indicated a statistically significant difference between %s and %s, t(%d) = %s, p = %s (Cohen's d_z = %s). There was %s in the mean response of %s (initial: %s, final: %s).",
      col_before, col_after, n - 1L, sl_fmt_num(t_pair$statistic, 3L, "en"), sl_fmt_p(p_t, "en"), sl_fmt_num(cohen_dz, 2L, "en"),
      direction, sl_fmt_num(abs(md), 2L, "en"), sl_fmt_num(m1, 2L, "en"), sl_fmt_num(m2, 2L, "en")
    )
  } else {
    sprintf(
      "Paired t-test analysis indicated no statistically significant difference between %s and %s, t(%d) = %s, p = %s (Cohen's d_z = %s). The mean difference of %s is within expected random variation.",
      col_before, col_after, n - 1L, sl_fmt_num(t_pair$statistic, 3L, "en"), sl_fmt_p(p_t, "en"), sl_fmt_num(cohen_dz, 2L, "en"),
      sl_fmt_num(md, 2L, "en")
    )
  }

  meta <- list(
    analysis_type = "paired_t",
    analysis_label = list(id = "Uji-t Dua Sampel Berpasangan", en = "Paired Samples t-Test"),
    response_label = sprintf("%s vs %s", col_before, col_after),
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(n),
    n_valid = as.integer(n),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(col_before, col_after)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(
      normality_violated = isTRUE(diff_sw$valid && diff_sw$p_value < alpha),
      homogeneity_violated = FALSE,
      difference_normality = list(shapiro = diff_sw, lilliefors = diff_lillie)
    ),
    outliers = list(flagged = data.frame()),
    main = list(
      statistic = t_pair$statistic,
      df = n - 1L,
      p_value = t_pair$p.value,
      mean_diff = md,
      cohen_dz = cohen_dz
    ),
    posthoc = NULL,
    effect_size = list(cohen_dz = cohen_dz),
    alternatives = list(wilcoxon = wilc_pair),
    tables_apa = list(tbl_primary),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Uji-t Berpasangan", en = "Paired t-Test"),
        latex = "t = \\frac{\\bar{d} - 0}{s_d / \\sqrt{n}}, \\quad d_i = Y_{2i} - Y_{1i}"
      )
    ),
    plots = list(),
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
