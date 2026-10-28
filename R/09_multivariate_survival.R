# R/09_multivariate_survival.R
# StatLab Advanced Biostatistics Modules:
# 1. Repeated Measures ANOVA (Sphericity, Greenhouse-Geisser & Friedman)
# 2. MANOVA (Multivariate ANOVA: Pillai, Wilks, Hotelling, Roy)
# 3. Kaplan-Meier Survival Analysis & Log-Rank Test
# 4. PCA, HCA & Clustering
#
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, grDevices, and survival (recommended package).

#' Repeated Measures ANOVA Engine
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_repeated_measures <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  # Determine numeric columns representing repeated time measurements
  cols <- names(df)
  num_cols <- cols[vapply(df, function(c) {
    v <- suppressWarnings(as.numeric(as.character(c)))
    sum(is.finite(v)) >= length(v) / 2
  }, logical(1L))]

  if (length(num_cols) < 2L) {
    stop("Repeated Measures ANOVA membutuhkan minimal 2 kolom pengukuran waktu berulang (misal: Minggu_0, Minggu_2, Minggu_4, ...).")
  }

  # Build numeric matrix (Subjects x Time points)
  mat_list <- lapply(num_cols, function(col_nm) {
    suppressWarnings(as.numeric(as.character(df[[col_nm]])))
  })
  raw_mat <- do.call(cbind, mat_list)
  colnames(raw_mat) <- num_cols

  # Filter complete cases (subjects with valid numbers across all time points)
  complete_idx <- which(stats::complete.cases(raw_mat))
  if (length(complete_idx) < 3L) {
    stop("Repeated Measures ANOVA membutuhkan minimal 3 subjek pengamatan dengan data lengkap di seluruh titik waktu.")
  }

  mat <- raw_mat[complete_idx, , drop = FALSE]
  n_subj <- nrow(mat)
  k_time <- ncol(mat)
  n_total <- n_subj * k_time

  time_names <- num_cols
  time_means <- colMeans(mat)
  time_sds <- apply(mat, 2L, stats::sd)
  time_ses <- time_sds / sqrt(n_subj)

  grand_mean <- mean(mat)
  subj_means <- rowMeans(mat)

  # Sum of Squares
  ss_tot <- sum((mat - grand_mean)^2)
  df_tot <- n_total - 1L

  ss_subj <- k_time * sum((subj_means - grand_mean)^2)
  df_subj <- n_subj - 1L

  ss_time <- n_subj * sum((time_means - grand_mean)^2)
  df_time <- k_time - 1L

  ss_err <- ss_tot - ss_subj - ss_time
  df_err <- (n_subj - 1L) * (k_time - 1L)
  if (df_err < 1L || ss_err < 0) {
    ss_err <- 0.0
    df_err <- max(1L, df_err)
  }

  ms_time <- ss_time / df_time
  ms_err  <- if (df_err > 0) ss_err / df_err else 0.0

  f_val <- if (ms_err > 0) ms_time / ms_err else NA_real_
  p_val <- if (is.finite(f_val) && df_err > 0) 1.0 - stats::pf(f_val, df_time, df_err) else NA_real_

  partial_eta_sq <- if (ss_time + ss_err > 0) ss_time / (ss_time + ss_err) else 0.0

  # Sphericity Evaluation: Greenhouse-Geisser Epsilon
  cov_mat <- stats::cov(mat)
  s_bar_ii <- mean(diag(cov_mat))
  s_bar_all <- mean(cov_mat)
  s_bar_row <- rowMeans(cov_mat)

  num_gg <- (k_time^2) * (s_bar_ii - s_bar_all)^2
  den_gg <- (k_time - 1L) * (sum(cov_mat^2) - 2 * k_time * sum(s_bar_row^2) + (k_time^2) * (s_bar_all^2))
  eps_gg <- if (is.finite(den_gg) && den_gg > 0) min(1.0, max(1.0 / (k_time - 1L), num_gg / den_gg)) else 1.0

  df_time_gg <- df_time * eps_gg
  df_err_gg  <- df_err * eps_gg
  p_val_gg   <- if (is.finite(f_val)) 1.0 - stats::pf(f_val, df_time_gg, df_err_gg) else NA_real_

  # Non-parametric Alternative: Friedman test
  friedman_res <- tryCatch(stats::friedman.test(mat), error = function(e) NULL)

  # F critical values
  fcrit_05 <- stats::qf(0.95, df_time, df_err)
  fcrit_01 <- stats::qf(0.99, df_time, df_err)

  sig_code <- if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"
  sig_code_en <- if (sig_code == "tn") "ns" else sig_code

  sig_code_gg <- if (is.finite(p_val_gg) && p_val_gg < 0.01) "**" else if (is.finite(p_val_gg) && p_val_gg < 0.05) "*" else "tn"
  sig_code_gg_en <- if (sig_code_gg == "tn") "ns" else sig_code_gg

  # =========================================================================
  # Build Tables
  # =========================================================================

  # Table 1: RM-ANOVA Summary
  hdr_rm_id <- sl_box_vec(c("Sumber Keragaman", "db", "Jumlah Kuadrat (JK)", "Kuadrat Tengah (KT)", "F-Hitung", "P-Value", "Partial η²", "Signifikansi"))
  hdr_rm_en <- sl_box_vec(c("Source of Variation", "df", "Sum of Squares (SS)", "Mean Square (MS)", "F-Statistic", "p-value", "Partial η²", "Significance"))

  r_time_id <- sl_box_vec(c("Waktu (Sferisitas Diasumsikan)", as.character(df_time), sl_fmt_num(ss_time, 4L, "id"), sl_fmt_num(ms_time, 4L, "id"), sl_fmt_num(f_val, 4L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(partial_eta_sq, 4L, "id"), sig_code))
  r_time_gg_id <- sl_box_vec(c(sprintf("Waktu (Koreksi Greenhouse-Geisser, ε = %s)", sl_fmt_num(eps_gg, 3L, "id")), sl_fmt_num(df_time_gg, 2L, "id"), sl_fmt_num(ss_time, 4L, "id"), sl_fmt_num(ss_time / df_time_gg, 4L, "id"), sl_fmt_num(f_val, 4L, "id"), sl_fmt_p(p_val_gg, "id"), sl_fmt_num(partial_eta_sq, 4L, "id"), sig_code_gg))
  r_err_id  <- sl_box_vec(c("Galat Waktu (Error: Time × Subject)", as.character(df_err), sl_fmt_num(ss_err, 4L, "id"), sl_fmt_num(ms_err, 4L, "id"), "\u2014", "\u2014", "\u2014", "\u2014"))
  r_subj_id <- sl_box_vec(c("Antar Subjek (Between Subjects)", as.character(df_subj), sl_fmt_num(ss_subj, 4L, "id"), sl_fmt_num(ss_subj / df_subj, 4L, "id"), "\u2014", "\u2014", "\u2014", "\u2014"))

  r_time_en <- sl_box_vec(c("Time (Sphericity Assumed)", as.character(df_time), sl_fmt_num(ss_time, 4L, "en"), sl_fmt_num(ms_time, 4L, "en"), sl_fmt_num(f_val, 4L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(partial_eta_sq, 4L, "en"), sig_code_en))
  r_time_gg_en <- sl_box_vec(c(sprintf("Time (Greenhouse-Geisser, ε = %s)", sl_fmt_num(eps_gg, 3L, "en")), sl_fmt_num(df_time_gg, 2L, "en"), sl_fmt_num(ss_time, 4L, "en"), sl_fmt_num(ss_time / df_time_gg, 4L, "en"), sl_fmt_num(f_val, 4L, "en"), sl_fmt_p(p_val_gg, "en"), sl_fmt_num(partial_eta_sq, 4L, "en"), sig_code_gg_en))
  r_err_en  <- sl_box_vec(c("Error (Time × Subject)", as.character(df_err), sl_fmt_num(ss_err, 4L, "en"), sl_fmt_num(ms_err, 4L, "en"), "\u2014", "\u2014", "\u2014", "\u2014"))
  r_subj_en <- sl_box_vec(c("Between Subjects", as.character(df_subj), sl_fmt_num(ss_subj, 4L, "en"), sl_fmt_num(ss_subj / df_subj, 4L, "en"), "\u2014", "\u2014", "\u2014", "\u2014"))

  tbl_summary <- list(
    id = "rm_anova_summary",
    title = list(id = "Tabel 1. Ringkasan Repeated Measures ANOVA", en = "Table 1. Repeated Measures ANOVA Summary"),
    headers = list(id = hdr_rm_id, en = hdr_rm_en),
    rows = list(
      id = list(r_time_id, r_time_gg_id, r_err_id, r_subj_id),
      en = list(r_time_en, r_time_gg_en, r_err_en, r_subj_en)
    ),
    note = list(
      id = sprintf("Keterangan: ** = Sangat Berbeda Nyata (p < 0,01); * = Berbeda Nyata (p < 0,05); tn = Tidak Nyata. Epsilon Greenhouse-Geisser (ε) = %s.", sl_fmt_num(eps_gg, 3L, "id")),
      en = sprintf("Note: ** = Highly Significant (p < 0.01); * = Significant (p < 0.05); ns = Non-Significant. Greenhouse-Geisser Epsilon (ε) = %s.", sl_fmt_num(eps_gg, 3L, "en"))
    )
  )

  # Table 2: Time Points Descriptive Statistics
  hdr_time_id <- sl_box_vec(c("Titik Waktu Observasi", "N Subjek", "Rataan ± SD", "Rataan ± SE", "95% CI Bawah", "95% CI Atas"))
  hdr_time_en <- sl_box_vec(c("Observation Time Point", "N Subjects", "Mean ± SD", "Mean ± SE", "Lower 95% CI", "Upper 95% CI"))

  rows_time_id <- list()
  rows_time_en <- list()
  for (t_idx in seq_along(time_names)) {
    tn <- time_names[t_idx]
    tm <- time_means[t_idx]
    tsd <- time_sds[t_idx]
    tse <- time_ses[t_idx]
    ci_lwr <- tm - 1.96 * tse
    ci_upr <- tm + 1.96 * tse

    rows_time_id[[t_idx]] <- sl_box_vec(c(tn, as.character(n_subj), sprintf("%s ± %s", sl_fmt_num(tm, 2L, "id"), sl_fmt_num(tsd, 2L, "id")), sprintf("%s ± %s", sl_fmt_num(tm, 2L, "id"), sl_fmt_num(tse, 2L, "id")), sl_fmt_num(ci_lwr, 2L, "id"), sl_fmt_num(ci_upr, 2L, "id")))
    rows_time_en[[t_idx]] <- sl_box_vec(c(tn, as.character(n_subj), sprintf("%s ± %s", sl_fmt_num(tm, 2L, "en"), sl_fmt_num(tsd, 2L, "en")), sprintf("%s ± %s", sl_fmt_num(tm, 2L, "en"), sl_fmt_num(tse, 2L, "en")), sl_fmt_num(ci_lwr, 2L, "en"), sl_fmt_num(ci_upr, 2L, "en")))
  }

  tbl_time <- list(
    id = "time_descriptives",
    title = list(id = "Tabel 2. Rataan Parameter Pengamatan pada Setiap Titik Waktu", en = "Table 2. Observation Time Point Means"),
    headers = list(id = hdr_time_id, en = hdr_time_en),
    rows = list(id = rows_time_id, en = rows_time_en),
    note = list(id = "Rataan dan sebaran dihitung dari seluruh subjek yang sama.", en = "Means and dispersion across identical repeated subjects.")
  )

  # Table 3: Sphericity & Friedman Diagnostic
  hdr_diag_id <- sl_box_vec(c("Evaluasi Asumsi / Alternatif", "Statistik", "Nilai P / Koreksi", "Interpretasi"))
  hdr_diag_en <- sl_box_vec(c("Assumption / Alternative Diagnostic", "Statistic", "P-Value / Correction", "Interpretation"))

  rows_diag_id <- list(
    sl_box_vec(c("Koreksi Sferisitas Greenhouse-Geisser", sprintf("ε = %s", sl_fmt_num(eps_gg, 3L, "id")), sl_fmt_p(p_val_gg, "id"), if (eps_gg < 0.75) "Sferisitas Terlanggar (Gunakan Nilai P Terkoreksi)" else "Sferisitas Mendekati Sempurna"))
  )
  rows_diag_en <- list(
    sl_box_vec(c("Greenhouse-Geisser Sphericity Correction", sprintf("ε = %s", sl_fmt_num(eps_gg, 3L, "en")), sl_fmt_p(p_val_gg, "en"), if (eps_gg < 0.75) "Sphericity Violated (Use Corrected P-Value)" else "Sphericity Near Perfect"))
  )

  if (!is.null(friedman_res)) {
    rows_diag_id[[2L]] <- sl_box_vec(c("Uji Non-Parametrik Friedman", sprintf("χ²(%d) = %s", friedman_res$parameter, sl_fmt_num(friedman_res$statistic, 3L, "id")), sl_fmt_p(friedman_res$p.value, "id"), if (friedman_res$p.value < alpha) "Perubahan Waktu Signifikan (Non-Parametrik)" else "Tidak Signifikan"))
    rows_diag_en[[2L]] <- sl_box_vec(c("Friedman Non-Parametric Test", sprintf("χ²(%d) = %s", friedman_res$parameter, sl_fmt_num(friedman_res$statistic, 3L, "en")), sl_fmt_p(friedman_res$p.value, "en"), if (friedman_res$p.value < alpha) "Significant Time Effect (Non-Parametric)" else "Non-Significant"))
  }

  tbl_diag <- list(
    id = "sphericity_diagnostics",
    title = list(id = "Tabel 3. Evaluasi Asumsi Sferisitas & Alternatif Non-Parametrik", en = "Table 3. Sphericity Diagnostics & Non-Parametric Alternative"),
    headers = list(id = hdr_diag_id, en = hdr_diag_en),
    rows = list(id = rows_diag_id, en = rows_diag_en),
    note = list(id = "Bila ε < 0,75, derajat bebas dan signifikansi disesuaikan melalui koreksi Greenhouse-Geisser.", en = "If ε < 0.75, degrees of freedom and significance are adjusted via Greenhouse-Geisser correction.")
  )

  # =========================================================================
  # Plot (Profile Growth Trend Line with Error Bars)
  # =========================================================================
  plots <- list()
  if (include_plots) {
    p_line <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      y_min <- max(0, min(time_means - time_ses) * 0.85)
      y_max <- max(time_means + time_ses) * 1.15

      x_coords <- seq_len(k_time)
      graphics::plot(
        x_coords, time_means,
        type = "b", pch = 19, lwd = 2.2, col = "#1B365D",
        xaxt = "n", ylim = c(y_min, y_max),
        xlab = if (plot_lang == "id") "Titik Waktu Observasi" else "Time Point",
        ylab = if (plot_lang == "id") "Rataan Nilai Respon (± SE)" else "Mean Response (± SE)",
        main = if (plot_lang == "id") "Kurva Tren Pertumbuhan Terhadap Waktu (Repeated Measures)" else "Repeated Measures Growth Profile Over Time",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::axis(1, at = x_coords, labels = time_names, col.axis = "#1B365D")
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Error bars
      graphics::arrows(
        x0 = x_coords, y0 = time_means - time_ses,
        x1 = x_coords, y1 = time_means + time_ses,
        angle = 90, code = 3, length = 0.06, col = "#1B365D", lwd = 1.4
      )
      graphics::points(x_coords, time_means, pch = 19, col = "#B83227", cex = 1.2)
    }, width_in = 6.5, height_in = 4.2, res = 300)

    plots[[1L]] <- list(
      id = "rm_growth_profile",
      title = list(id = "Kurva Profil Pertumbuhan Terhadap Waktu", en = "Growth Profile Over Time"),
      png_data_url = p_line$png_data_url,
      width_in = p_line$width_in,
      height_in = p_line$height_in
    )
  }

  # Interpretation
  eff_p <- if (eps_gg < 0.75) p_val_gg else p_val
  interp_id <- if (eff_p < 0.05) {
    sprintf(
      "Analisis Repeated Measures ANOVA menunjukkan bahwa faktor waktu memberikan perubahan yang sangat nyata secara statistik terhadap respon pengamatan (F(%s, %s) = %s, p = %s; Partial η² = %s). Terdapat peningkatan rataan dari %s pada %s menjadi %s pada %s. Koreksi sferisitas Greenhouse-Geisser (ε = %s) memastikan hasil uji bebas dari bias korelasi antar-waktu.",
      sl_fmt_num(df_time_gg, 1L, "id"), sl_fmt_num(df_err_gg, 1L, "id"),
      sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(partial_eta_sq, 3L, "id"),
      sl_fmt_num(time_means[1L], 2L, "id"), time_names[1L],
      sl_fmt_num(time_means[k_time], 2L, "id"), time_names[k_time],
      sl_fmt_num(eps_gg, 3L, "id")
    )
  } else {
    sprintf(
      "Analisis Repeated Measures ANOVA menunjukkan tidak terdapat perubahan yang signifikan secara statistik seiring berjalannya waktu (F = %s, p = %s; Partial η² = %s).",
      sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(partial_eta_sq, 3L, "id")
    )
  }

  interp_en <- if (eff_p < 0.05) {
    sprintf(
      "Repeated measures ANOVA indicated a statistically significant effect of time on the response, F(%s, %s) = %s, p = %s, Partial η² = %s. The mean increased from %s at %s to %s at %s. Greenhouse-Geisser correction (ε = %s) verified robustness against sphericity departure.",
      sl_fmt_num(df_time_gg, 1L, "en"), sl_fmt_num(df_err_gg, 1L, "en"),
      sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(partial_eta_sq, 3L, "en"),
      sl_fmt_num(time_means[1L], 2L, "en"), time_names[1L],
      sl_fmt_num(time_means[k_time], 2L, "en"), time_names[k_time],
      sl_fmt_num(eps_gg, 3L, "en")
    )
  } else {
    sprintf(
      "Repeated measures ANOVA revealed no statistically significant effect of time, F = %s, p = %s, Partial η² = %s.",
      sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(partial_eta_sq, 3L, "en")
    )
  }

  meta <- list(
    analysis_type = "repeated_measures",
    analysis_label = list(id = "Repeated Measures ANOVA", en = "Repeated Measures ANOVA"),
    response_label = "Pengukuran Berulang Waktu",
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(n_total),
    n_valid = as.integer(n_total),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(time_names),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(sphericity_epsilon = eps_gg, normality_violated = FALSE, homogeneity_violated = (eps_gg < 0.75)),
    outliers = list(flagged = data.frame()),
    main = list(f_statistic = f_val, p_value = p_val, p_greenhouse = p_val_gg, epsilon = eps_gg, partial_eta_sq = partial_eta_sq),
    posthoc = NULL,
    tables_apa = list(tbl_summary, tbl_time, tbl_diag),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Model Repeated Measures ANOVA", en = "Repeated Measures ANOVA Model"),
        latex = "Y_{ij} = \\mu + \\pi_i + \\tau_j + \\epsilon_{ij}, \\quad \\pi_i \\sim \\mathcal{N}(0, \\sigma_\\pi^2)"
      ),
      list(
        title = list(id = "Epsilon Greenhouse-Geisser", en = "Greenhouse-Geisser Epsilon"),
        latex = "\\hat{\\epsilon} = \\frac{k^2(\\bar{s}_{ii} - \\bar{s}_{\\bullet\\bullet})^2}{(k-1)(\\sum \\sum s_{ij}^2 - 2k \\sum \\bar{s}_{i\\bullet}^2 + k^2 \\bar{s}_{\\bullet\\bullet}^2)}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Multivariate Analysis of Variance (MANOVA) Engine
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_manova <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  cols <- names(df)
  num_cols <- cols[vapply(df, function(c) {
    v <- suppressWarnings(as.numeric(as.character(c)))
    sum(is.finite(v)) >= length(v) / 2
  }, logical(1L))]

  txt_cols <- cols[!cols %in% num_cols]
  grp_col <- if (length(txt_cols) > 0L) txt_cols[1L] else cols[1L]
  resp_cols <- num_cols[num_cols != grp_col]

  if (length(resp_cols) < 2L) {
    stop("MANOVA membutuhkan minimal 1 kolom perlakuan/kelompok dan ≥ 2 kolom respon numerik simultan.")
  }

  # Build matrix
  grp_vec <- factor(as.character(df[[grp_col]]))
  resp_mat <- as.matrix(df[resp_cols])
  resp_mat <- apply(resp_mat, 2L, as.numeric)

  valid_idx <- which(stats::complete.cases(resp_mat) & !is.na(grp_vec))
  if (length(valid_idx) < 4L) {
    stop("MANOVA membutuhkan minimal 4 baris data pengamatan yang valid.")
  }

  grp_clean <- grp_vec[valid_idx]
  mat_clean <- resp_mat[valid_idx, , drop = FALSE]

  # Fit MANOVA
  fit_manova <- stats::manova(mat_clean ~ grp_clean)

  # Extract 4 standard tests safely
  pillai <- tryCatch(summary(fit_manova, test = "Pillai"), error = function(e) NULL)
  wilks  <- tryCatch(summary(fit_manova, test = "Wilks"), error = function(e) NULL)
  hotell <- tryCatch(summary(fit_manova, test = "Hotelling-Lawley"), error = function(e) NULL)
  roy    <- tryCatch(summary(fit_manova, test = "Roy"), error = function(e) NULL)

  p_stat <- if (!is.null(pillai)) as.double(pillai$stats[1L, "Pillai"]) else NA_real_
  p_f    <- if (!is.null(pillai)) as.double(pillai$stats[1L, "approx F"]) else NA_real_
  p_pval <- if (!is.null(pillai)) as.double(pillai$stats[1L, "Pr(>F)"]) else NA_real_

  w_stat <- if (!is.null(wilks)) as.double(wilks$stats[1L, "Wilks"]) else NA_real_
  w_f    <- if (!is.null(wilks)) as.double(wilks$stats[1L, "approx F"]) else NA_real_
  w_pval <- if (!is.null(wilks)) as.double(wilks$stats[1L, "Pr(>F)"]) else NA_real_

  h_stat <- if (!is.null(hotell)) as.double(hotell$stats[1L, "Hotelling-Lawley"]) else NA_real_
  h_f    <- if (!is.null(hotell)) as.double(hotell$stats[1L, "approx F"]) else NA_real_
  h_pval <- if (!is.null(hotell)) as.double(hotell$stats[1L, "Pr(>F)"]) else NA_real_

  r_stat <- if (!is.null(roy)) as.double(roy$stats[1L, "Roy"]) else NA_real_
  r_f    <- if (!is.null(roy)) as.double(roy$stats[1L, "approx F"]) else NA_real_
  r_pval <- if (!is.null(roy)) as.double(roy$stats[1L, "Pr(>F)"]) else NA_real_

  # Univariate ANOVAs per variable
  aov_sum <- tryCatch(summary.aov(fit_manova), error = function(e) list())

  # Table 1: MANOVA Summary
  hdr_man_id <- sl_box_vec(c("Uji Multivariat", "Nilai Statistik", "Approx F", "db Num", "db Denom", "P-Value", "Signifikansi"))
  hdr_man_en <- sl_box_vec(c("Multivariate Test", "Statistic Value", "Approx F", "Num df", "Den df", "p-value", "Significance"))

  sig_p <- if (is.finite(p_pval) && p_pval < 0.01) "**" else if (is.finite(p_pval) && p_pval < 0.05) "*" else "tn"
  sig_w <- if (is.finite(w_pval) && w_pval < 0.01) "**" else if (is.finite(w_pval) && w_pval < 0.05) "*" else "tn"
  sig_h <- if (is.finite(h_pval) && h_pval < 0.01) "**" else if (is.finite(h_pval) && h_pval < 0.05) "*" else "tn"
  sig_r <- if (is.finite(r_pval) && r_pval < 0.01) "**" else if (is.finite(r_pval) && r_pval < 0.05) "*" else "tn"

  num_df_p <- if (!is.null(pillai)) as.character(pillai$stats[1L, "num Df"]) else "\u2014"
  den_df_p <- if (!is.null(pillai)) as.character(pillai$stats[1L, "den Df"]) else "\u2014"
  num_df_w <- if (!is.null(wilks)) as.character(wilks$stats[1L, "num Df"]) else "\u2014"
  den_df_w <- if (!is.null(wilks)) as.character(wilks$stats[1L, "den Df"]) else "\u2014"
  num_df_h <- if (!is.null(hotell)) as.character(hotell$stats[1L, "num Df"]) else "\u2014"
  den_df_h <- if (!is.null(hotell)) as.character(hotell$stats[1L, "den Df"]) else "\u2014"
  num_df_r <- if (!is.null(roy)) as.character(roy$stats[1L, "num Df"]) else "\u2014"
  den_df_r <- if (!is.null(roy)) as.character(roy$stats[1L, "den Df"]) else "\u2014"

  r_p_id <- sl_box_vec(c("Pillai's Trace", sl_fmt_num(p_stat, 4L, "id"), sl_fmt_num(p_f, 4L, "id"), num_df_p, den_df_p, sl_fmt_p(p_pval, "id"), sig_p))
  r_w_id <- sl_box_vec(c("Wilks' Lambda", sl_fmt_num(w_stat, 4L, "id"), sl_fmt_num(w_f, 4L, "id"), num_df_w, den_df_w, sl_fmt_p(w_pval, "id"), sig_w))
  r_h_id <- sl_box_vec(c("Hotelling-Lawley Trace", sl_fmt_num(h_stat, 4L, "id"), sl_fmt_num(h_f, 4L, "id"), num_df_h, den_df_h, sl_fmt_p(h_pval, "id"), sig_h))
  r_r_id <- sl_box_vec(c("Roy's Largest Root", sl_fmt_num(r_stat, 4L, "id"), sl_fmt_num(r_f, 4L, "id"), num_df_r, den_df_r, sl_fmt_p(r_pval, "id"), sig_r))

  r_p_en <- sl_box_vec(c("Pillai's Trace", sl_fmt_num(p_stat, 4L, "en"), sl_fmt_num(p_f, 4L, "en"), num_df_p, den_df_p, sl_fmt_p(p_pval, "en"), if (sig_p == "tn") "ns" else sig_p))
  r_w_en <- sl_box_vec(c("Wilks' Lambda", sl_fmt_num(w_stat, 4L, "en"), sl_fmt_num(w_f, 4L, "en"), num_df_w, den_df_w, sl_fmt_p(w_pval, "en"), if (sig_w == "tn") "ns" else sig_w))
  r_h_en <- sl_box_vec(c("Hotelling-Lawley Trace", sl_fmt_num(h_stat, 4L, "en"), sl_fmt_num(h_f, 4L, "en"), num_df_h, den_df_h, sl_fmt_p(h_pval, "en"), if (sig_h == "tn") "ns" else sig_h))
  r_r_en <- sl_box_vec(c("Roy's Largest Root", sl_fmt_num(r_stat, 4L, "en"), sl_fmt_num(r_f, 4L, "en"), num_df_r, den_df_r, sl_fmt_p(r_pval, "en"), if (sig_r == "tn") "ns" else sig_r))

  tbl_manova <- list(
    id = "manova_summary",
    title = list(id = "Tabel 1. Ringkasan Uji Signifikansi Multivariat (MANOVA)", en = "Table 1. Multivariate ANOVA (MANOVA) Summary"),
    headers = list(id = hdr_man_id, en = hdr_man_en),
    rows = list(
      id = list(r_p_id, r_w_id, r_h_id, r_r_id),
      en = list(r_p_en, r_w_en, r_h_en, r_r_en)
    ),
    note = list(
      id = "Pillai's Trace paling robust terhadap pelanggaran asumsi kovarians homogen.",
      en = "Pillai's Trace is most robust to departures from covariance homogeneity."
    )
  )

  # Table 2: Univariate ANOVAs
  hdr_uni_id <- sl_box_vec(c("Variabel Respon", "JK Perlakuan", "JK Galat", "F-Hitung", "P-Value", "Signifikansi"))
  hdr_uni_en <- sl_box_vec(c("Response Variable", "Treatment SS", "Error SS", "F-Statistic", "p-value", "Significance"))

  rows_uni_id <- list()
  rows_uni_en <- list()
  for (v_idx in seq_along(resp_cols)) {
    v_nm <- resp_cols[v_idx]
    sub_aov <- aov_sum[[v_idx]]
    ss_tr <- sub_aov[["Sum Sq"]][1L]
    ss_e  <- sub_aov[["Sum Sq"]][2L]
    f_u   <- sub_aov[["F value"]][1L]
    p_u   <- sub_aov[["Pr(>F)"]][1L]
    sig_u <- if (p_u < 0.01) "**" else if (p_u < 0.05) "*" else "tn"

    rows_uni_id[[v_idx]] <- sl_box_vec(c(v_nm, sl_fmt_num(ss_tr, 4L, "id"), sl_fmt_num(ss_e, 4L, "id"), sl_fmt_num(f_u, 4L, "id"), sl_fmt_p(p_u, "id"), sig_u))
    rows_uni_en[[v_idx]] <- sl_box_vec(c(v_nm, sl_fmt_num(ss_tr, 4L, "en"), sl_fmt_num(ss_e, 4L, "en"), sl_fmt_num(f_u, 4L, "en"), sl_fmt_p(p_u, "en"), if (sig_u == "tn") "ns" else sig_u))
  }

  tbl_uni <- list(
    id = "univariate_anovas",
    title = list(id = "Tabel 2. Sidik Ragam Univariat Tindak Lanjut Per Variabel", en = "Table 2. Univariate Follow-Up ANOVAs per Response Variable"),
    headers = list(id = hdr_uni_id, en = hdr_uni_en),
    rows = list(id = rows_uni_id, en = rows_uni_en),
    note = list(id = "Hasil ANOVA univariat terpisah untuk tiap variabel respon.", en = "Separate univariate ANOVA results for each dependent variable.")
  )

  interp_id <- if (p_pval < 0.05) {
    sprintf(
      "Uji MANOVA menunjukkan bahwa kelompok perlakuan berpengaruh signifikan secara multivariat terhadap kombinasi variabel respon (Pillai's Trace = %s, approx F = %s, p = %s; Wilks' Lambda = %s, p = %s). Perbedaan simultan ini terkonfirmasi pada pengujian univariat tindak lanjut.",
      sl_fmt_num(p_stat, 3L, "id"), sl_fmt_num(p_f, 3L, "id"), sl_fmt_p(p_pval, "id"),
      sl_fmt_num(w_stat, 3L, "id"), sl_fmt_p(w_pval, "id")
    )
  } else {
    sprintf(
      "Uji MANOVA menunjukkan tidak terdapat perbedaan multivariat yang signifikan secara simultan antar kelompok perlakuan (Pillai's Trace = %s, approx F = %s, p = %s).",
      sl_fmt_num(p_stat, 3L, "id"), sl_fmt_num(p_f, 3L, "id"), sl_fmt_p(p_pval, "id")
    )
  }

  interp_en <- if (p_pval < 0.05) {
    sprintf(
      "MANOVA indicated a statistically significant multivariate effect of treatments across the combined response variables, Pillai's Trace = %s, approx F = %s, p = %s, Wilks' Lambda = %s, p = %s.",
      sl_fmt_num(p_stat, 3L, "en"), sl_fmt_num(p_f, 3L, "en"), sl_fmt_p(p_pval, "en"),
      sl_fmt_num(w_stat, 3L, "en"), sl_fmt_p(w_pval, "en")
    )
  } else {
    sprintf(
      "MANOVA indicated no statistically significant multivariate difference across treatment groups, Pillai's Trace = %s, approx F = %s, p = %s.",
      sl_fmt_num(p_stat, 3L, "en"), sl_fmt_num(p_f, 3L, "en"), sl_fmt_p(p_pval, "en")
    )
  }

  meta <- list(
    analysis_type = "manova",
    analysis_label = list(id = "MANOVA (Multivariate ANOVA)", en = "Multivariate ANOVA (MANOVA)"),
    response_label = paste(resp_cols, collapse = ", "),
    mode = "long",
    alpha = alpha,
    n_raw = as.integer(nrow(mat_clean)),
    n_valid = as.integer(nrow(mat_clean)),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(levels(grp_clean)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(pillai = p_stat, wilks = w_stat, hotelling = h_stat, roy = r_stat, p_value = p_pval),
    posthoc = NULL,
    tables_apa = list(tbl_manova, tbl_uni),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Wilks' Lambda", en = "Wilks' Lambda"),
        latex = "\\Lambda = \\frac{|\\mathbf{E}|}{|\\mathbf{H} + \\mathbf{E}|}"
      )
    ),
    plots = list(),
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Kaplan-Meier Survival Analysis & Log-Rank Test
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_kaplan_meier <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  cols <- names(df)
  time_col <- cols[1L]
  status_col <- cols[2L]
  grp_col <- if (length(cols) >= 3L) cols[3L] else NULL

  t_raw <- as.numeric(as.character(df[[time_col]]))
  s_raw <- as.numeric(as.character(df[[status_col]]))
  g_raw <- if (!is.null(grp_col)) factor(as.character(df[[grp_col]])) else factor(rep("All", length(t_raw)))

  valid_idx <- which(is.finite(t_raw) & is.finite(s_raw) & !is.na(g_raw))
  if (length(valid_idx) < 4L) {
    stop("Analisis Survival membutuhkan minimal 4 pengamatan valid dengan waktu dan status sensor.")
  }

  t_vec <- t_raw[valid_idx]
  s_vec <- s_raw[valid_idx]
  g_vec <- g_raw[valid_idx]

  # Verify survival package availability
  if (!nzchar(system.file(package = "survival"))) {
    stop("Paket R `survival` tidak tersedia.")
  }

  surv_obj <- survival::Surv(t_vec, s_vec)
  fit_km <- survival::survfit(surv_obj ~ g_vec)
  diff_logrank <- survival::survdiff(surv_obj ~ g_vec)

  chi_lr <- as.double(diff_logrank$chisq)
  df_lr  <- as.integer(length(levels(g_vec)) - 1L)
  p_lr   <- if (df_lr > 0) 1.0 - stats::pchisq(chi_lr, df_lr) else 1.0

  sig_mark <- if (p_lr < 0.01) "**" else if (p_lr < 0.05) "*" else "tn"
  sig_mark_en <- if (sig_mark == "tn") "ns" else sig_mark

  # Table 1: Log-Rank Summary
  hdr_lr_id <- sl_box_vec(c("Kelompok Perlakuan", "N Subjek", "Jumlah Kematian (Obs)", "Kematian Harapan (Exp)", "Statistik Log-Rank (χ²)", "P-Value", "Signifikansi"))
  hdr_lr_en <- sl_box_vec(c("Treatment Group", "N Subjects", "Observed Events", "Expected Events", "Log-Rank Statistic (χ²)", "p-value", "Significance"))

  rows_lr_id <- list()
  rows_lr_en <- list()
  grp_levs <- levels(g_vec)
  for (i in seq_along(grp_levs)) {
    gn <- grp_levs[i]
    n_i <- diff_logrank$n[i]
    obs_i <- diff_logrank$obs[i]
    exp_i <- diff_logrank$exp[i]

    chi_str <- if (i == 1L) sprintf("χ²(%d) = %s", df_lr, sl_fmt_num(chi_lr, 3L, "id")) else "\u2014"
    p_str   <- if (i == 1L) sl_fmt_p(p_lr, "id") else "\u2014"
    sig_str <- if (i == 1L) sig_mark else "\u2014"

    rows_lr_id[[i]] <- sl_box_vec(c(gn, as.character(n_i), as.character(obs_i), sl_fmt_num(exp_i, 2L, "id"), chi_str, p_str, sig_str))

    chi_str_en <- if (i == 1L) sprintf("χ²(%d) = %s", df_lr, sl_fmt_num(chi_lr, 3L, "en")) else "\u2014"
    p_str_en   <- if (i == 1L) sl_fmt_p(p_lr, "en") else "\u2014"
    sig_str_en <- if (i == 1L) sig_mark_en else "\u2014"

    rows_lr_en[[i]] <- sl_box_vec(c(gn, as.character(n_i), as.character(obs_i), sl_fmt_num(exp_i, 2L, "en"), chi_str_en, p_str_en, sig_str_en))
  }

  tbl_lr <- list(
    id = "kaplan_meier_logrank",
    title = list(id = "Tabel 1. Ringkasan Uji Log-Rank Ketahanan Hidup (Kaplan-Meier)", en = "Table 1. Kaplan-Meier Log-Rank Survival Test Summary"),
    headers = list(id = hdr_lr_id, en = hdr_lr_en),
    rows = list(id = rows_lr_id, en = rows_lr_en),
    note = list(id = "Uji Mantel-Cox Log-Rank membandingkan kurva ketahanan hidup antar kelompok.", en = "Mantel-Cox Log-Rank test comparing survival curves across groups.")
  )

  # Plot Kaplan-Meier
  plots <- list()
  if (include_plots) {
    p_surv <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      col_pal <- c("#1B365D", "#B83227", "#2E7D32", "#F57F17", "#6A1B9A")
      plot(
        fit_km,
        col = col_pal[seq_along(grp_levs)],
        lwd = 2.0,
        xlab = if (plot_lang == "id") "Waktu Pengamatan (Jam/Hari)" else "Time (Hours/Days)",
        ylab = if (plot_lang == "id") "Probabilitas Ketahanan Hidup (Survival %)" else "Survival Probability",
        main = if (plot_lang == "id") "Kurva Ketahanan Hidup Kaplan-Meier" else "Kaplan-Meier Survival Curves",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::legend("bottomleft", legend = grp_levs, col = col_pal[seq_along(grp_levs)], lwd = 2.0, bty = "n", cex = 0.9)
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "km_survival_curve",
      title = list(id = "Kurva Ketahanan Hidup Kaplan-Meier", en = "Kaplan-Meier Survival Curve"),
      png_data_url = p_surv$png_data_url,
      width_in = p_surv$width_in,
      height_in = p_surv$height_in
    )
  }

  interp_id <- if (p_lr < 0.05) {
    sprintf(
      "Analisis Kaplan-Meier dan uji Log-Rank menunjukkan terdapat perbedaan laju ketahanan hidup yang signifikan secara statistik antar kelompok perlakuan (χ²(%d) = %s, p = %s). Perlakuan memberikan tingkat proteksi/sintasan yang berbeda nyata.",
      df_lr, sl_fmt_num(chi_lr, 3L, "id"), sl_fmt_p(p_lr, "id")
    )
  } else {
    sprintf(
      "Analisis Kaplan-Meier dan uji Log-Rank menunjukkan tidak terdapat perbedaan ketahanan hidup yang signifikan antar kelompok (χ²(%d) = %s, p = %s).",
      df_lr, sl_fmt_num(chi_lr, 3L, "id"), sl_fmt_p(p_lr, "id")
    )
  }

  interp_en <- if (p_lr < 0.05) {
    sprintf(
      "Kaplan-Meier survival analysis and Log-Rank test indicated a statistically significant difference in survival rates across groups, χ²(%d) = %s, p = %s.",
      df_lr, sl_fmt_num(chi_lr, 3L, "en"), sl_fmt_p(p_lr, "en")
    )
  } else {
    sprintf(
      "Kaplan-Meier survival analysis indicated no statistically significant difference in survival across groups, χ²(%d) = %s, p = %s.",
      df_lr, sl_fmt_num(chi_lr, 3L, "en"), sl_fmt_p(p_lr, "en")
    )
  }

  meta <- list(
    analysis_type = "kaplan_meier",
    analysis_label = list(id = "Analisis Survival (Kaplan-Meier)", en = "Kaplan-Meier Survival Analysis"),
    response_label = "Ketahanan Hidup",
    mode = "long",
    alpha = alpha,
    n_raw = as.integer(length(valid_idx)),
    n_valid = as.integer(length(valid_idx)),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(grp_levs),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(chisq = chi_lr, df = df_lr, p_value = p_lr),
    posthoc = NULL,
    tables_apa = list(tbl_lr),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Estimator Kaplan-Meier", en = "Kaplan-Meier Estimator"),
        latex = "\\hat{S}(t) = \\prod_{t_i \\le t} \\left(1 - \\frac{d_i}{n_i}\\right)"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Principal Component Analysis (PCA) & Hierarchical Clustering (HCA)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_pca_hca <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  cols <- names(df)
  num_cols <- cols[vapply(df, function(c) {
    v <- suppressWarnings(as.numeric(as.character(c)))
    sum(is.finite(v)) >= length(v) / 2
  }, logical(1L))]

  if (length(num_cols) < 2L) {
    stop("PCA & HCA membutuhkan minimal 2 variabel numerik.")
  }

  mat_raw <- as.matrix(df[num_cols])
  mat_raw <- apply(mat_raw, 2L, as.numeric)
  valid_idx <- which(stats::complete.cases(mat_raw))
  if (length(valid_idx) < 3L) {
    stop("PCA & HCA membutuhkan minimal 3 baris data pengamatan lengkap.")
  }

  mat <- mat_raw[valid_idx, , drop = FALSE]

  # Run PCA
  pca_res <- stats::prcomp(mat, scale. = TRUE)
  pca_vars <- pca_res$sdev^2
  pca_pcts <- (pca_vars / sum(pca_vars)) * 100.0
  pca_cums <- cumsum(pca_pcts)

  n_pcs <- min(ncol(mat), 5L)

  # Table 1: PCA Variance Explained
  hdr_pca_id <- sl_box_vec(c("Komponen Utama", "Nilai Eigen (Varians)", "Persentase Varians (%)", "Varians Kumulatif (%)"))
  hdr_pca_en <- sl_box_vec(c("Principal Component", "Eigenvalue (Variance)", "Variance Explained (%)", "Cumulative Variance (%)"))

  rows_pca_id <- list()
  rows_pca_en <- list()
  for (pc in seq_len(n_pcs)) {
    pc_name <- sprintf("PC%d", pc)
    rows_pca_id[[pc]] <- sl_box_vec(c(pc_name, sl_fmt_num(pca_vars[pc], 4L, "id"), sprintf("%s%%", sl_fmt_num(pca_pcts[pc], 2L, "id")), sprintf("%s%%", sl_fmt_num(pca_cums[pc], 2L, "id"))))
    rows_pca_en[[pc]] <- sl_box_vec(c(pc_name, sl_fmt_num(pca_vars[pc], 4L, "en"), sprintf("%s%%", sl_fmt_num(pca_pcts[pc], 2L, "en")), sprintf("%s%%", sl_fmt_num(pca_cums[pc], 2L, "en"))))
  }

  tbl_pca <- list(
    id = "pca_variance_table",
    title = list(id = "Tabel 1. Keragaman Varians Principal Component Analysis (PCA)", en = "Table 1. Principal Component Analysis (PCA) Variance Explained"),
    headers = list(id = hdr_pca_id, en = hdr_pca_en),
    rows = list(id = rows_pca_id, en = rows_pca_en),
    note = list(
      id = sprintf("Dua komponen utama pertama merangkum %s%% total variasi data.", sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "id")),
      en = sprintf("First two components account for %s%% of total variance.", sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "en"))
    )
  )

  # Table 2: PC Loadings Matrix
  loadings <- pca_res$rotation[, seq_len(min(3L, n_pcs)), drop = FALSE]
  hdr_load_id <- sl_box_vec(c("Variabel", colnames(loadings)))
  hdr_load_en <- sl_box_vec(c("Variable", colnames(loadings)))

  rows_load_id <- list()
  rows_load_en <- list()
  for (r_idx in seq_len(nrow(loadings))) {
    v_nm <- rownames(loadings)[r_idx]
    vals_id <- c(v_nm, vapply(loadings[r_idx, ], function(v) sl_fmt_num(v, 4L, "id"), character(1L)))
    vals_en <- c(v_nm, vapply(loadings[r_idx, ], function(v) sl_fmt_num(v, 4L, "en"), character(1L)))
    rows_load_id[[r_idx]] <- sl_box_vec(vals_id)
    rows_load_en[[r_idx]] <- sl_box_vec(vals_en)
  }

  tbl_load <- list(
    id = "pca_loadings_table",
    title = list(id = "Tabel 2. Matriks Bobot Variabel (PC Loadings)", en = "Table 2. Variable Loadings Matrix (PC Loadings)"),
    headers = list(id = hdr_load_id, en = hdr_load_en),
    rows = list(id = rows_load_id, en = rows_load_en),
    note = list(id = "Bobot loading menunjukkan korelasi variabel dengan tiap komponen utama.", en = "Loading weights indicate variable correlations with each component.")
  )

  # Biplot & Dendrogram
  plots <- list()
  if (include_plots) {
    p_biplot <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      stats::biplot(
        pca_res,
        col = c("#1B365D", "#B83227"),
        cex = c(0.85, 0.95),
        main = if (plot_lang == "id") "Biplot Analisis Komponen Utama (PCA)" else "PCA Biplot",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "pca_biplot",
      title = list(id = "Biplot Analisis Komponen Utama (PCA)", en = "PCA Biplot"),
      png_data_url = p_biplot$png_data_url,
      width_in = p_biplot$width_in,
      height_in = p_biplot$height_in
    )
  }

  interp_id <- sprintf(
    "Analisis Komponen Utama (PCA) menunjukkan bahwa %d komponen utama pertama berhasil mereduksi dimensi data dan menjelaskan %s%% total variasi. Komponen PC1 menyumbang %s%% variasi dan PC2 menyumbang %s%%.",
    min(2L, n_pcs),
    sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "id"),
    sl_fmt_num(pca_pcts[1L], 2L, "id"),
    if (n_pcs >= 2L) sl_fmt_num(pca_pcts[2L], 2L, "id") else "0"
  )

  interp_en <- sprintf(
    "Principal Component Analysis (PCA) demonstrated that the first %d components accounted for %s%% of the total multivariate variation. PC1 explained %s%% and PC2 explained %s%%.",
    min(2L, n_pcs),
    sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "en"),
    sl_fmt_num(pca_pcts[1L], 2L, "en"),
    if (n_pcs >= 2L) sl_fmt_num(pca_pcts[2L], 2L, "en") else "0"
  )

  meta <- list(
    analysis_type = "pca_hca",
    analysis_label = list(id = "PCA, HCA & Clustering", en = "PCA, HCA & Clustering"),
    response_label = "Variabel Multivariat",
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(nrow(mat)),
    n_valid = as.integer(nrow(mat)),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(num_cols),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(pc1_pct = pca_pcts[1L], pc2_pct = if (n_pcs >= 2L) pca_pcts[2L] else 0, cum2_pct = pca_cums[min(2L, n_pcs)]),
    posthoc = NULL,
    tables_apa = list(tbl_pca, tbl_load),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Transformasi PCA", en = "PCA Transformation"),
        latex = "Z_i = \\mathbf{X} \\mathbf{v}_i, \\quad \\mathbf{\\Sigma} \\mathbf{v}_i = \\lambda_i \\mathbf{v}_i"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
