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

  # Warnings & Interpretation (Key Points)
  warnings_id <- character(0)
  warnings_en <- character(0)
  if (eps_gg < 0.75) {
    warnings_id <- c(warnings_id, sprintf("Asumsi sferisitas dilanggar (Greenhouse-Geisser epsilon = %s < 0.75). Derajat bebas dan p-value telah dikoreksi menggunakan penyesuaian Greenhouse-Geisser.", sl_fmt_num(eps_gg, 3L, "id")))
    warnings_en <- c(warnings_en, sprintf("Sphericity assumption violated (Greenhouse-Geisser epsilon = %s < 0.75). Degrees of freedom and p-value were corrected using Greenhouse-Geisser adjustment.", sl_fmt_num(eps_gg, 3L, "en")))
  }

  eff_p <- if (eps_gg < 0.75) p_val_gg else p_val
  pts_id <- c(
    if (eff_p < 0.05) {
      sprintf("• **Efek Waktu**: Faktor waktu memberikan pengaruh yang sangat nyata secara statistik terhadap respon pengamatan (F(%s, %s) = %s, p = %s; Partial η² = %s).",
        sl_fmt_num(df_time_gg, 1L, "id"), sl_fmt_num(df_err_gg, 1L, "id"),
        sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(partial_eta_sq, 3L, "id"))
    } else {
      sprintf("• **Efek Waktu**: Tidak terdapat perubahan yang signifikan secara statistik seiring berjalannya waktu (F = %s, p = %s; Partial η² = %s).",
        sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(eff_p, "id"), sl_fmt_num(partial_eta_sq, 3L, "id"))
    },
    sprintf("• **Tren Rataan**: Rataan respon berkembang dari %s (%s) menjadi %s (%s).",
      sl_fmt_num(time_means[1L], 2L, "id"), time_names[1L],
      sl_fmt_num(time_means[k_time], 2L, "id"), time_names[k_time]),
    sprintf("• **Koreksi Sferisitas**: Nilai estimasi Greenhouse-Geisser ε = %s digunakan untuk memastikan keabsahan inferensi terhadap korelasi antar-waktu.",
      sl_fmt_num(eps_gg, 3L, "id"))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (eff_p < 0.05) {
      sprintf("• **Time Effect**: Time exerted a statistically significant effect on the response (F(%s, %s) = %s, p = %s, Partial η² = %s).",
        sl_fmt_num(df_time_gg, 1L, "en"), sl_fmt_num(df_err_gg, 1L, "en"),
        sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(partial_eta_sq, 3L, "en"))
    } else {
      sprintf("• **Time Effect**: No statistically significant time effect was observed (F = %s, p = %s, Partial η² = %s).",
        sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(eff_p, "en"), sl_fmt_num(partial_eta_sq, 3L, "en"))
    },
    sprintf("• **Trajectory**: Mean response shifted from %s (%s) to %s (%s).",
      sl_fmt_num(time_means[1L], 2L, "en"), time_names[1L],
      sl_fmt_num(time_means[k_time], 2L, "en"), time_names[k_time]),
    sprintf("• **Sphericity Correction**: Greenhouse-Geisser ε = %s ensures statistical inference remains robust against temporal correlation.",
      sl_fmt_num(eps_gg, 3L, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

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
    warnings = list(id = sl_box_vec(warnings_id), en = sl_box_vec(warnings_en))
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

  # Check rank of residuals to prevent crash on collinear variables
  warnings_id <- character(0)
  warnings_en <- character(0)
  qr_resid <- qr(residuals(fit_manova))
  r_rank <- qr_resid$rank
  p_cols <- ncol(mat_clean)
  manova_mat <- mat_clean

  if (r_rank < p_cols) {
    if (r_rank >= 1L) {
      kept_idx <- qr_resid$pivot[seq_len(r_rank)]
      dropped_cols <- colnames(mat_clean)[-kept_idx]
      warnings_id <- c(warnings_id, sprintf("Variabel respon [%s] terdeteksi memiliki multikolinearitas sempurna (redundansi linier) dan diakomodasi melalui reduksi dimensi rank.", paste(dropped_cols, collapse = ", ")))
      warnings_en <- c(warnings_en, sprintf("Response variables [%s] exhibit perfect collinearity and were handled via rank reduction.", paste(dropped_cols, collapse = ", ")))
      manova_mat <- mat_clean[, kept_idx, drop = FALSE]
      fit_manova <- stats::manova(manova_mat ~ grp_clean)
    }
  }

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
    sub_aov <- tryCatch(summary(stats::aov(mat_clean[, v_idx] ~ grp_clean))[[1L]], error = function(e) NULL)
    if (!is.null(sub_aov) && nrow(sub_aov) >= 2L) {
      ss_tr <- sub_aov[["Sum Sq"]][1L]
      ss_e  <- sub_aov[["Sum Sq"]][2L]
      f_u   <- sub_aov[["F value"]][1L]
      p_u   <- sub_aov[["Pr(>F)"]][1L]
      sig_u <- if (is.finite(p_u) && p_u < 0.01) "**" else if (is.finite(p_u) && p_u < 0.05) "*" else "tn"
    } else {
      ss_tr <- NA_real_
      ss_e  <- NA_real_
      f_u   <- NA_real_
      p_u   <- NA_real_
      sig_u <- "tn"
    }

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

  # Format Key Points (Poin Kunci / Key Points)
  pts_id <- c(
    if (is.finite(p_pval) && p_pval < 0.05) {
      sprintf("• **Signifikansi Multivariat**: Perlakuan berpengaruh sangat nyata secara multivariat terhadap kombinasi variabel respon (Pillai's Trace = %s, approx F = %s, p = %s; Wilks' Lambda = %s, p = %s).",
        sl_fmt_num(p_stat, 3L, "id"), sl_fmt_num(p_f, 3L, "id"), sl_fmt_p(p_pval, "id"),
        sl_fmt_num(w_stat, 3L, "id"), sl_fmt_p(w_pval, "id"))
    } else if (is.finite(p_pval)) {
      sprintf("• **Signifikansi Multivariat**: Tidak terdapat perbedaan multivariat yang signifikan secara simultan antar kelompok perlakuan (Pillai's Trace = %s, approx F = %s, p = %s).",
        sl_fmt_num(p_stat, 3L, "id"), sl_fmt_num(p_f, 3L, "id"), sl_fmt_p(p_pval, "id"))
    } else {
      "• **Signifikansi Multivariat**: Uji multivariat simultan dievaluasi melalui reduksi rank akibat multikolinearitas variabel. Amati hasil ANOVA univariat terpisah di bawah."
    },
    sprintf("• **Uji Lanjut Univariat**: ANOVA univariat dievaluasi secara terpisah untuk %d variabel respon (%s) guna mendeteksi parameter spesifik yang paling responsif.",
      length(resp_cols), paste(resp_cols, collapse = ", "))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_pval) && p_pval < 0.05) {
      sprintf("• **Multivariate Significance**: Treatments exerted a statistically significant multivariate effect on combined responses (Pillai's Trace = %s, approx F = %s, p = %s; Wilks' Lambda = %s, p = %s).",
        sl_fmt_num(p_stat, 3L, "en"), sl_fmt_num(p_f, 3L, "en"), sl_fmt_p(p_pval, "en"),
        sl_fmt_num(w_stat, 3L, "en"), sl_fmt_p(w_pval, "en"))
    } else if (is.finite(p_pval)) {
      sprintf("• **Multivariate Significance**: No statistically significant multivariate difference was found across groups (Pillai's Trace = %s, approx F = %s, p = %s).",
        sl_fmt_num(p_stat, 3L, "en"), sl_fmt_num(p_f, 3L, "en"), sl_fmt_p(p_pval, "en"))
    } else {
      "• **Multivariate Significance**: Evaluated via rank reduction due to collinearity. Refer to separate follow-up univariate ANOVAs below."
    },
    sprintf("• **Univariate Follow-Up**: Separate univariate ANOVAs evaluated across %d dependent variables (%s).",
      length(resp_cols), paste(resp_cols, collapse = ", "))
  )
  interp_en <- paste(pts_en, collapse = "\n")

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
    warnings = list(id = sl_box_vec(warnings_id), en = sl_box_vec(warnings_en))
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

  warnings_id <- character(0)
  warnings_en <- character(0)

  pts_id <- c(
    if (p_lr < 0.05) {
      sprintf("• **Perbedaan Kurva Survival**: Uji Log-Rank membuktikan adanya perbedaan laju ketahanan hidup yang signifikan secara statistik antar kelompok perlakuan (χ²(%d) = %s, p = %s).",
        df_lr, sl_fmt_num(chi_lr, 3L, "id"), sl_fmt_p(p_lr, "id"))
    } else {
      sprintf("• **Perbedaan Kurva Survival**: Uji Log-Rank menunjukkan tidak terdapat perbedaan ketahanan hidup yang signifikan secara statistik antar kelompok perlakuan (χ²(%d) = %s, p = %s).",
        df_lr, sl_fmt_num(chi_lr, 3L, "id"), sl_fmt_p(p_lr, "id"))
    },
    sprintf("• **Kohort Pengamatan**: Melibatkan %d kelompok perlakuan (%s) dengan total %d spesimen/individu terobservasi.",
      length(grp_levs), paste(grp_levs, collapse = ", "), length(valid_idx))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (p_lr < 0.05) {
      sprintf("• **Survival Curve Comparison**: Log-Rank test indicated a statistically significant difference in survival distributions across groups (χ²(%d) = %s, p = %s).",
        df_lr, sl_fmt_num(chi_lr, 3L, "en"), sl_fmt_p(p_lr, "en"))
    } else {
      sprintf("• **Survival Curve Comparison**: Log-Rank test revealed no statistically significant difference in survival across groups (χ²(%d) = %s, p = %s).",
        df_lr, sl_fmt_num(chi_lr, 3L, "en"), sl_fmt_p(p_lr, "en"))
    },
    sprintf("• **Cohort Summary**: %d treatment groups (%s) evaluated across %d total subjects.",
      length(grp_levs), paste(grp_levs, collapse = ", "), length(valid_idx))
  )
  interp_en <- paste(pts_en, collapse = "\n")

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
    warnings = list(id = sl_box_vec(warnings_id), en = sl_box_vec(warnings_en))
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

  # Check roles
  resp_roles <- if (!is.null(opts$roles$response_cols)) as.character(opts$roles$response_cols) else character(0)
  if (length(resp_roles) >= 2L && all(resp_roles %in% cols)) {
    num_cols <- resp_roles
  } else {
    num_cols <- cols[vapply(df, function(c) {
      v <- suppressWarnings(as.numeric(as.character(c)))
      sum(is.finite(v)) >= length(v) / 2
    }, logical(1L))]
  }

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
  n_obs <- nrow(mat)
  p_vars <- ncol(mat)

  # Extract sample labels
  sample_col_role <- if (!is.null(opts$roles$sample_col)) as.character(opts$roles$sample_col) else ""
  char_cols <- cols[!cols %in% num_cols]
  sample_labels <- if (nzchar(sample_col_role) && sample_col_role %in% cols) {
    as.character(df[[sample_col_role]])[valid_idx]
  } else if (length(char_cols) >= 1L) {
    as.character(df[[char_cols[1L]]])[valid_idx]
  } else if (!is.null(rownames(df)) && !all(rownames(df) == as.character(seq_len(nrow(df))))) {
    rownames(df)[valid_idx]
  } else {
    sprintf("S%02d", seq_len(n_obs))
  }

  # Target clusters k (user specified or auto recommended)
  k_req <- if (!is.null(opts$k_clusters)) suppressWarnings(as.integer(opts$k_clusters[[1L]])) else NA_integer_
  if (is.na(k_req) || k_req < 2L || k_req > min(6L, n_obs - 1L)) {
    k_clusters <- if (n_obs >= 9L) 3L else 2L
  } else {
    k_clusters <- k_req
  }

  # 1. PCA computation
  pca_res <- stats::prcomp(mat, scale. = TRUE)
  pca_vars <- pca_res$sdev^2
  pca_pcts <- (pca_vars / sum(pca_vars)) * 100.0
  pca_cums <- cumsum(pca_pcts)
  n_pcs <- min(p_vars, 5L)

  # Bartlett's test of sphericity for correlation matrix
  cor_mat <- stats::cor(mat)
  det_cor <- base::det(cor_mat)
  if (is.finite(det_cor) && det_cor > 1e-12 && n_obs > p_vars) {
    bartlett_chi <- -(n_obs - 1 - (2 * p_vars + 5) / 6) * log(det_cor)
    bartlett_df <- as.integer((p_vars * (p_vars - 1)) / 2)
    bartlett_pval <- stats::pchisq(bartlett_chi, df = bartlett_df, lower.tail = FALSE)
  } else {
    bartlett_chi <- NA_real_
    bartlett_df <- as.integer((p_vars * (p_vars - 1)) / 2)
    bartlett_pval <- NA_real_
  }

  # 2. HCA (Hierarchical Cluster Analysis) - Ward.D2
  scaled_mat <- scale(mat)
  dist_mat <- stats::dist(scaled_mat, method = "euclidean")
  hc_fit <- stats::hclust(dist_mat, method = "ward.D2")
  cl_hca <- stats::cutree(hc_fit, k = k_clusters)

  # 3. K-Means Clustering Comparison
  km_fit <- tryCatch(
    stats::kmeans(scaled_mat, centers = k_clusters, nstart = 25),
    error = function(e) stats::kmeans(scaled_mat, centers = k_clusters, nstart = 5)
  )
  cl_km <- km_fit$cluster
  bss_pct <- (km_fit$betweenss / km_fit$totss) * 100.0

  # Distinct publication palette for clusters
  cluster_palette <- c("#1B365D", "#B83227", "#2E7D32", "#D97706", "#7C3AED", "#0891B2")

  # Table 1: PCA Variance Explained & Kaiser Criterion
  hdr_pca_id <- sl_box_vec(c("Komponen Utama", "Nilai Eigen (λ)", "Proporsi Varians (%)", "Varians Kumulatif (%)", "Kriteria Kaiser (λ ≥ 1)"))
  hdr_pca_en <- sl_box_vec(c("Principal Component", "Eigenvalue (λ)", "Variance Explained (%)", "Cumulative Variance (%)", "Kaiser Criterion (λ ≥ 1)"))

  rows_pca_id <- list()
  rows_pca_en <- list()
  kaiser_count <- 0L
  for (pc in seq_len(n_pcs)) {
    pc_name <- sprintf("PC%d", pc)
    is_kaiser <- pca_vars[pc] >= 1.0
    if (is_kaiser) kaiser_count <- kaiser_count + 1L
    kaiser_txt_id <- if (is_kaiser) "Ya (λ ≥ 1, Signifikan)" else "Tidak (Meredup)"
    kaiser_txt_en <- if (is_kaiser) "Yes (λ ≥ 1, Retain)" else "No (λ < 1, Drop)"
    rows_pca_id[[pc]] <- sl_box_vec(c(pc_name, sl_fmt_num(pca_vars[pc], 4L, "id"), sprintf("%s%%", sl_fmt_num(pca_pcts[pc], 2L, "id")), sprintf("%s%%", sl_fmt_num(pca_cums[pc], 2L, "id")), kaiser_txt_id))
    rows_pca_en[[pc]] <- sl_box_vec(c(pc_name, sl_fmt_num(pca_vars[pc], 4L, "en"), sprintf("%s%%", sl_fmt_num(pca_pcts[pc], 2L, "en")), sprintf("%s%%", sl_fmt_num(pca_cums[pc], 2L, "en")), kaiser_txt_en))
  }

  sphericity_note_id <- if (is.finite(bartlett_pval)) {
    sprintf("Uji Sferisitas Bartlett: χ²(%d) = %s, p = %s (%s). %s",
      bartlett_df, sl_fmt_num(bartlett_chi, 3L, "id"), sl_fmt_p(bartlett_pval, "id"),
      if (bartlett_pval < 0.05) "matriks korelasi saling berasosiasi nyata" else "korelasi antar variabel lemah",
      sprintf("Dua komponen utama pertama merangkum %s%% total variasi data.", sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "id")))
  } else {
    sprintf("Dua komponen utama pertama merangkum %s%% total variasi data.", sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "id"))
  }

  sphericity_note_en <- if (is.finite(bartlett_pval)) {
    sprintf("Bartlett Sphericity Test: χ²(%d) = %s, p = %s. First two components account for %s%% of total variance.",
      bartlett_df, sl_fmt_num(bartlett_chi, 3L, "en"), sl_fmt_p(bartlett_pval, "en"), sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "en"))
  } else {
    sprintf("First two components account for %s%% of total variance.", sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "en"))
  }

  tbl_pca <- list(
    id = "pca_variance_table",
    title = list(id = "Tabel 1. Keragaman Varians & Nilai Eigen PCA (Kriteria Kaiser)", en = "Table 1. PCA Variance Explained & Eigenvalues (Kaiser Criterion)"),
    headers = list(id = hdr_pca_id, en = hdr_pca_en),
    rows = list(id = rows_pca_id, en = rows_pca_en),
    note = list(id = sphericity_note_id, en = sphericity_note_en)
  )

  # Table 2: PC Loadings & Variable Contribution Matrix
  loadings <- pca_res$rotation[, seq_len(min(3L, n_pcs)), drop = FALSE]
  contrib_pc1 <- (loadings[, 1L]^2 / sum(loadings[, 1L]^2)) * 100.0
  contrib_pc2 <- if (ncol(loadings) >= 2L) (loadings[, 2L]^2 / sum(loadings[, 2L]^2)) * 100.0 else rep(0, nrow(loadings))

  hdr_load_id <- sl_box_vec(c("Variabel Respon", "Loading PC1", "Kontribusi PC1 (%)", "Loading PC2", "Kontribusi PC2 (%)", "Peran Dominan"))
  hdr_load_en <- sl_box_vec(c("Response Variable", "PC1 Loading", "PC1 Contrib (%)", "PC2 Loading", "PC2 Contrib (%)", "Dominant Role"))

  rows_load_id <- list()
  rows_load_en <- list()
  for (r_idx in seq_len(nrow(loadings))) {
    v_nm <- rownames(loadings)[r_idx]
    l1 <- loadings[r_idx, 1L]
    c1 <- contrib_pc1[r_idx]
    l2 <- if (ncol(loadings) >= 2L) loadings[r_idx, 2L] else 0
    c2 <- contrib_pc2[r_idx]
    dom_id <- if (abs(l1) >= abs(l2)) "Penentu PC1" else "Penentu PC2"
    dom_en <- if (abs(l1) >= abs(l2)) "Drives PC1" else "Drives PC2"

    rows_load_id[[r_idx]] <- sl_box_vec(c(v_nm, sl_fmt_num(l1, 4L, "id"), sprintf("%s%%", sl_fmt_num(c1, 2L, "id")), sl_fmt_num(l2, 4L, "id"), sprintf("%s%%", sl_fmt_num(c2, 2L, "id")), dom_id))
    rows_load_en[[r_idx]] <- sl_box_vec(c(v_nm, sl_fmt_num(l1, 4L, "en"), sprintf("%s%%", sl_fmt_num(c1, 2L, "en")), sl_fmt_num(l2, 4L, "en"), sprintf("%s%%", sl_fmt_num(c2, 2L, "en")), dom_en))
  }

  tbl_load <- list(
    id = "pca_loadings_table",
    title = list(id = "Tabel 2. Matriks Bobot Variabel (PC Loadings) & Kontribusi Relatif", en = "Table 2. Variable Loadings & Relative Variance Contributions"),
    headers = list(id = hdr_load_id, en = hdr_load_en),
    rows = list(id = rows_load_id, en = rows_load_en),
    note = list(id = "Loading mendekati +1/-1 menunjukkan korelasi sangat kuat; kontribusi (%) mengukur persentase pengaruh variabel terhadap orientasi sumbu komponen.", en = "Loadings near ±1 indicate strong correlation; contribution (%) reflects variable weight in defining the principal axis.")
  )

  # Table 3: Analisa Lanjutan — Profil Rataan & Uji Beda Nyata Antar-Klaster (One-Way ANOVA)
  cl_counts <- table(factor(cl_hca, levels = seq_len(k_clusters)))
  hdr_prof_id_vec <- c("Variabel Respon", "Rataan Total (SD)", sprintf("Klaster %d (n=%d)", seq_len(k_clusters), as.vector(cl_counts)), "F-Hitung", "P-Value", "Signifikansi")
  hdr_prof_en_vec <- c("Response Variable", "Overall Mean (SD)", sprintf("Cluster %d (n=%d)", seq_len(k_clusters), as.vector(cl_counts)), "F-Statistic", "p-value", "Significance")
  hdr_prof_id <- sl_box_vec(hdr_prof_id_vec)
  hdr_prof_en <- sl_box_vec(hdr_prof_en_vec)

  rows_prof_id <- list()
  rows_prof_en <- list()
  sig_cluster_vars <- character(0)

  for (v_idx in seq_along(num_cols)) {
    v_nm <- num_cols[v_idx]
    v_vec <- mat[, v_idx]
    tot_mean <- mean(v_vec)
    tot_sd <- sd(v_vec)
    tot_str_id <- sprintf("%s ± %s", sl_fmt_num(tot_mean, 2L, "id"), sl_fmt_num(tot_sd, 2L, "id"))
    tot_str_en <- sprintf("%s ± %s", sl_fmt_num(tot_mean, 2L, "en"), sl_fmt_num(tot_sd, 2L, "en"))

    cl_strs_id <- character(k_clusters)
    cl_strs_en <- character(k_clusters)
    for (k_idx in seq_len(k_clusters)) {
      sub_v <- v_vec[cl_hca == k_idx]
      m_k <- mean(sub_v)
      sd_k <- if (length(sub_v) > 1L) sd(sub_v) else 0
      cl_strs_id[k_idx] <- sprintf("%s ± %s", sl_fmt_num(m_k, 2L, "id"), sl_fmt_num(sd_k, 2L, "id"))
      cl_strs_en[k_idx] <- sprintf("%s ± %s", sl_fmt_num(m_k, 2L, "en"), sl_fmt_num(sd_k, 2L, "en"))
    }

    # One-Way ANOVA across clusters
    aov_res <- tryCatch(summary(stats::aov(v_vec ~ factor(cl_hca)))[[1L]], error = function(e) NULL)
    if (!is.null(aov_res) && nrow(aov_res) >= 2L) {
      f_stat <- aov_res[["F value"]][1L]
      p_val  <- aov_res[["Pr(>F)"]][1L]
      sig_mark <- if (is.finite(p_val) && p_val < 0.001) "***" else if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"
      if (is.finite(p_val) && p_val < 0.05) sig_cluster_vars <- c(sig_cluster_vars, v_nm)
    } else {
      f_stat <- NA_real_
      p_val  <- NA_real_
      sig_mark <- "tn"
    }

    rows_prof_id[[v_idx]] <- sl_box_vec(c(v_nm, tot_str_id, cl_strs_id, sl_fmt_num(f_stat, 3L, "id"), sl_fmt_p(p_val, "id"), sig_mark))
    rows_prof_en[[v_idx]] <- sl_box_vec(c(v_nm, tot_str_en, cl_strs_en, sl_fmt_num(f_stat, 3L, "en"), sl_fmt_p(p_val, "en"), if (sig_mark == "tn") "ns" else sig_mark))
  }

  tbl_prof <- list(
    id = "cluster_profiles_table",
    title = list(id = "Tabel 3. Analisa Lanjutan: Profil Karakteristik & Uji Beda Nyata Antar-Klaster (One-Way ANOVA)", en = "Table 3. Follow-Up Analysis: Cluster Profiles & Inter-Cluster ANOVA"),
    headers = list(id = hdr_prof_id, en = hdr_prof_en),
    rows = list(id = rows_prof_id, en = rows_prof_en),
    note = list(
      id = sprintf("Uji ANOVA univariat membuktikan variabel mana yang membedakan profil klaster secara signifikan (* p < 0,05; ** p < 0,01; *** p < 0,001). Efisiensi separasi K-Means = %s%% (BSS/TSS).", sl_fmt_num(bss_pct, 2L, "id")),
      en = sprintf("Univariate ANOVA proves which variables distinguish the clusters significantly (* p < .05; ** p < .01; *** p < .001). K-Means separation ratio = %s%% (BSS/TSS).", sl_fmt_num(bss_pct, 2L, "en"))
    )
  )

  # Table 4: Keanggotaan Klaster & Koordinat Sampel
  hdr_mem_id <- sl_box_vec(c("ID Sampel", "Skor PC1", "Skor PC2", "Klaster HCA (Ward.D2)", "Klaster K-Means", "Kesesuaian"))
  hdr_mem_en <- sl_box_vec(c("Sample ID", "PC1 Score", "PC2 Score", "HCA Cluster (Ward.D2)", "K-Means Cluster", "Consensus"))

  rows_mem_id <- list()
  rows_mem_en <- list()
  for (s_idx in seq_len(n_obs)) {
    s_id <- sample_labels[s_idx]
    pc1_s <- pca_res$x[s_idx, 1L]
    pc2_s <- if (ncol(pca_res$x) >= 2L) pca_res$x[s_idx, 2L] else 0
    cl_h <- sprintf("Klaster %d", cl_hca[s_idx])
    cl_k <- sprintf("Klaster %d", cl_km[s_idx])
    match_txt_id <- if (cl_hca[s_idx] == cl_km[s_idx]) "Konsisten" else "Variasi Model"
    match_txt_en <- if (cl_hca[s_idx] == cl_km[s_idx]) "Consistent" else "Model Variation"

    rows_mem_id[[s_idx]] <- sl_box_vec(c(s_id, sl_fmt_num(pc1_s, 3L, "id"), sl_fmt_num(pc2_s, 3L, "id"), cl_h, cl_k, match_txt_id))
    rows_mem_en[[s_idx]] <- sl_box_vec(c(s_id, sl_fmt_num(pc1_s, 3L, "en"), sl_fmt_num(pc2_s, 3L, "en"), sprintf("Cluster %d", cl_hca[s_idx]), sprintf("Cluster %d", cl_km[s_idx]), match_txt_en))
  }

  tbl_mem <- list(
    id = "sample_membership_table",
    title = list(id = "Tabel 4. Pemetaan Koordinat Komponen Utama & Keanggotaan Klaster Sampel", en = "Table 4. Sample PC Coordinates & Cluster Membership Assignment"),
    headers = list(id = hdr_mem_id, en = hdr_mem_en),
    rows = list(id = rows_mem_id, en = rows_mem_en),
    note = list(id = "Menunjukkan posisi tiap unit pengamatan pada ruang komponen utama serta penugasan klaster berdasarkan algoritma hirarki dan non-hirarki.", en = "Displays the coordinates of each sample in PC space and its assigned cluster across hierarchical and partitioning methods.")
  )

  # Biplot & Dendrogram & Scree & Profile Plots
  plots <- list()
  if (include_plots) {
    # Plot 1: Cluster-Colored PCA Biplot
    p_biplot <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      x_scores <- pca_res$x[, 1L]
      y_scores <- if (ncol(pca_res$x) >= 2L) pca_res$x[, 2L] else rep(0, n_obs)
      
      max_x <- max(abs(x_scores)) * 1.3
      max_y <- max(abs(y_scores)) * 1.3
      if (max_x == 0) max_x <- 1
      if (max_y == 0) max_y <- 1

      plot(
        x_scores, y_scores,
        type = "n",
        xlim = c(-max_x, max_x), ylim = c(-max_y, max_y),
        xlab = sprintf(if (plot_lang == "id") "PC1 (%s%% Varians)" else "PC1 (%s%% Variance)", sl_fmt_num(pca_pcts[1L], 1L, plot_lang)),
        ylab = sprintf(if (plot_lang == "id") "PC2 (%s%% Varians)" else "PC2 (%s%% Variance)", sl_fmt_num(if (n_pcs >= 2L) pca_pcts[2L] else 0, 1L, plot_lang)),
        main = if (plot_lang == "id") "Biplot PCA Berwarna Berdasarkan Klaster (Ward.D2)" else "PCA Biplot Colored by HCA Clusters",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::abline(h = 0, v = 0, col = "#8C9BAE", lty = 2, lwd = 0.9)

      # Loading arrows scaled to plot
      max_load <- max(abs(loadings[, seq_len(min(2L, ncol(loadings)))]))
      if (max_load == 0) max_load <- 1
      scale_arrow <- (max_x * 0.75) / max_load

      for (j in seq_len(nrow(loadings))) {
        lx <- loadings[j, 1L] * scale_arrow
        ly <- if (ncol(loadings) >= 2L) loadings[j, 2L] * scale_arrow else 0
        graphics::arrows(0, 0, lx, ly, length = 0.08, col = "#B83227", lwd = 1.8)
        graphics::text(lx * 1.15, ly * 1.15, labels = rownames(loadings)[j], col = "#B83227", font = 2, cex = 0.82)
      }

      # Sample points colored by cluster
      graphics::points(x_scores, y_scores, pch = 21, bg = cluster_palette[cl_hca], col = "#1B365D", cex = 1.4, lwd = 1.2)
      graphics::text(x_scores, y_scores, labels = sample_labels, pos = 3, cex = 0.8, col = "#1B365D", font = 2)

      # Cluster legend
      leg_labels <- sprintf(if (plot_lang == "id") "Klaster %d (n=%d)" else "Cluster %d (n=%d)", seq_len(k_clusters), as.vector(cl_counts))
      graphics::legend("topright", legend = leg_labels, pt.bg = cluster_palette[seq_len(k_clusters)], pch = 21, col = "#1B365D", pt.cex = 1.2, bty = "n", cex = 0.82)
    }, width_in = 6.5, height_in = 4.6, res = 300)

    # Plot 2: HCA Dendrogram with Cluster Rectangles
    p_dend <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      plot(
        hc_fit,
        labels = sample_labels,
        hang = -1,
        main = if (plot_lang == "id") sprintf("Dendrogram Klasterisasi Hirarki (%d Klaster Metode Ward.D2)", k_clusters) else sprintf("Hierarchical Clustering Dendrogram (%d Clusters Ward.D2)", k_clusters),
        xlab = if (plot_lang == "id") "Sampel Unit Pengamatan" else "Sample Units",
        ylab = if (plot_lang == "id") "Jarak Disimilaritas Euclidean Terstandarisasi" else "Standardized Euclidean Dissimilarity",
        sub = "",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      stats::rect.hclust(hc_fit, k = k_clusters, border = cluster_palette[seq_len(k_clusters)])
    }, width_in = 6.5, height_in = 4.4, res = 300)

    # Plot 3: Scree Plot & Kaiser Criterion Line
    p_scree <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      bp_vals <- pca_vars[seq_len(n_pcs)]
      names(bp_vals) <- sprintf("PC%d", seq_len(n_pcs))
      max_bp <- max(max(bp_vals) * 1.2, 1.5)
      
      bp <- graphics::barplot(
        bp_vals,
        ylim = c(0, max_bp),
        col = ifelse(bp_vals >= 1.0, "#1B365D", "#8C9BAE"),
        main = if (plot_lang == "id") "Scree Plot Nilai Eigen & Kriteria Kaiser (λ ≥ 1.0)" else "Scree Plot of Eigenvalues & Kaiser Criterion",
        ylab = if (plot_lang == "id") "Nilai Eigen (Varians)" else "Eigenvalue (Variance)",
        xlab = if (plot_lang == "id") "Komponen Utama" else "Principal Component",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D", border = NA
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::abline(h = 1.0, col = "#B83227", lty = 2, lwd = 1.8)
      graphics::text(mean(bp), 1.0 + max_bp * 0.04, labels = if (plot_lang == "id") "Ambang Kriteria Kaiser (λ = 1.0)" else "Kaiser Criterion Cutoff (λ = 1.0)", col = "#B83227", font = 2, cex = 0.8)
      
      for (i in seq_along(bp)) {
        graphics::text(bp[i], bp_vals[i] + max_bp * 0.03, labels = sprintf("%s%%", sl_fmt_num(pca_pcts[i], 1L, plot_lang)), font = 2, cex = 0.8, col = "#1B365D")
      }
    }, width_in = 6.5, height_in = 4.2, res = 300)

    # Plot 4: Cluster Profile Comparison (Z-score Barplot)
    p_prof <- sl_capture_plot(function() {
      graphics::par(mar = c(6.0, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      z_mat <- scale(mat)
      z_means <- matrix(0, nrow = p_vars, ncol = k_clusters)
      rownames(z_means) <- num_cols
      colnames(z_means) <- sprintf(if (plot_lang == "id") "Klaster %d" else "Cluster %d", seq_len(k_clusters))
      for (j in seq_len(p_vars)) {
        z_means[j, ] <- tapply(z_mat[, j], factor(cl_hca, levels = seq_len(k_clusters)), mean, default = 0)
      }
      
      bp_prof <- graphics::barplot(
        t(z_means), beside = TRUE, col = cluster_palette[seq_len(k_clusters)],
        main = if (plot_lang == "id") "Profil Komparasi Karakteristik Antar-Klaster (Skor Standar Z-Score)" else "Cluster Profile Comparison (Standardized Z-Score Means)",
        ylab = if (plot_lang == "id") "Deviasi Relatif (Z-Score Rataan)" else "Relative Deviation (Mean Z-Score)",
        las = 2, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D", border = NA
      )
      graphics::abline(h = 0, col = "#1B365D", lwd = 1.2)
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::legend("topright", legend = sprintf(if (plot_lang == "id") "Klaster %d" else "Cluster %d", seq_len(k_clusters)), fill = cluster_palette[seq_len(k_clusters)], bty = "n", cex = 0.8)
    }, width_in = 6.5, height_in = 4.5, res = 300)

    plots <- list(
      list(
        id = "pca_biplot",
        title = list(id = "Biplot PCA & Sebaran Klaster Sampel", en = "PCA Biplot & Sample Cluster Distribution"),
        png_data_url = p_biplot$png_data_url,
        width_in = p_biplot$width_in,
        height_in = p_biplot$height_in
      ),
      list(
        id = "hca_dendrogram",
        title = list(id = "Dendrogram Klasterisasi Hirarki (Metode Ward.D2)", en = "Hierarchical Clustering Dendrogram (Ward.D2 Method)"),
        png_data_url = p_dend$png_data_url,
        width_in = p_dend$width_in,
        height_in = p_dend$height_in
      ),
      list(
        id = "pca_scree",
        title = list(id = "Scree Plot Varians & Kriteria Kaiser (λ ≥ 1.0)", en = "Scree Plot & Kaiser Criterion Cutoff (λ ≥ 1.0)"),
        png_data_url = p_scree$png_data_url,
        width_in = p_scree$width_in,
        height_in = p_scree$height_in
      ),
      list(
        id = "cluster_profile",
        title = list(id = "Profil Karakteristik Klaster (Skor Standar Z-Score)", en = "Cluster Profiles (Standardized Z-Score Means)"),
        png_data_url = p_prof$png_data_url,
        width_in = p_prof$width_in,
        height_in = p_prof$height_in
      )
    )
  }

  warnings_id <- character(0)
  warnings_en <- character(0)

  # Poin Kunci (Key Points)
  pts_id <- c(
    if (is.finite(bartlett_pval) && bartlett_pval < 0.05) {
      sprintf("• **Kelayakan Reduksi Dimensi**: Uji Sferisitas Bartlett membuktikan terdapat korelasi multivariat yang sangat signifikan antar variabel (χ²(%d) = %s, p = %s), mengonfirmasi bahwa reduksi dimensi PCA sangat valid dan tepat digunakan.",
        bartlett_df, sl_fmt_num(bartlett_chi, 3L, "id"), sl_fmt_p(bartlett_pval, "id"))
    } else if (is.finite(bartlett_pval)) {
      sprintf("• **Kelayakan Reduksi Dimensi**: Uji Sferisitas Bartlett menunjukkan korelasi antar variabel tergolong moderat (χ²(%d) = %s, p = %s).",
        bartlett_df, sl_fmt_num(bartlett_chi, 3L, "id"), sl_fmt_p(bartlett_pval, "id"))
    } else {
      "• **Kelayakan Reduksi Dimensi**: Matriks kovarian/korelasi berhasil distandarisasi untuk analisis multivariat komprehensif."
    },
    sprintf("• **Daya Jelas PCA & Kriteria Kaiser**: Dua komponen utama pertama menjelaskan %s%% dari total variansi multivariat (PC1 = %s%%, PC2 = %s%%). Terdapat %d komponen yang memenuhi Kriteria Kaiser (nilai eigen λ ≥ 1,0) sebagai dimensi inti yang representatif.",
      sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "id"),
      sl_fmt_num(pca_pcts[1L], 2L, "id"),
      if (n_pcs >= 2L) sl_fmt_num(pca_pcts[2L], 2L, "id") else "0",
      kaiser_count),
    sprintf("• **Pemisahan Klaster (HCA Ward.D2 & K-Means)**: Analisis klaster hirarki aglomeratif berhasil mengelompokkan %d sampel pengamatan ke dalam %d klaster terpisah dengan efisiensi pemisahan sebesar %s%% (BSS/TSS).",
      n_obs, k_clusters, sl_fmt_num(bss_pct, 2L, "id")),
    if (length(sig_cluster_vars) > 0L) {
      sprintf("• **Analisa Lanjutan — Pembeda Signifikan Antar-Klaster**: Uji ANOVA univariat membuktikan bahwa klaster-klaster tersebut terpisah secara nyata (p < 0,05) terutama dipicu oleh parameter [%s]. Karakteristik unik masing-masing klaster terangkum pada Tabel 3 dan Grafik Profil Klaster.",
        paste(sig_cluster_vars, collapse = ", "))
    } else {
      "• **Analisa Lanjutan — Karakteristik Klaster**: Profil masing-masing klaster menunjukkan diferensiasi karakteristik yang terangkum pada Tabel 3."
    },
    sprintf("• **Distribusi Unit Sampel**: Sampel terdistribusi ke dalam kelompok: %s. Pengelompokan ini dapat langsung dijadikan dasar segmentasi penanganan dan strategi intervensi spesifik di lapangan.",
      paste(sprintf("Klaster %d (%d sampel)", seq_len(k_clusters), as.vector(cl_counts)), collapse = ", "))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(bartlett_pval) && bartlett_pval < 0.05) {
      sprintf("• **Dimensionality Feasibility**: Bartlett's test of sphericity confirmed significant multivariate correlation among variables (χ²(%d) = %s, p = %s), demonstrating that PCA dimensionality reduction is fully warranted.",
        bartlett_df, sl_fmt_num(bartlett_chi, 3L, "en"), sl_fmt_p(bartlett_pval, "en"))
    } else if (is.finite(bartlett_pval)) {
      sprintf("• **Dimensionality Feasibility**: Bartlett's test showed moderate correlation among variables (χ²(%d) = %s, p = %s).",
        bartlett_df, sl_fmt_num(bartlett_chi, 3L, "en"), sl_fmt_p(bartlett_pval, "en"))
    } else {
      "• **Dimensionality Feasibility**: Covariance/correlation matrix successfully standardized for multivariate analysis."
    },
    sprintf("• **PCA Explanatory Power & Kaiser Rule**: The first two principal components explain %s%% of the total multivariate variation (PC1 = %s%%, PC2 = %s%%). %d component(s) satisfy Kaiser's criterion (eigenvalue λ ≥ 1.0) as core representative dimensions.",
      sl_fmt_num(pca_cums[min(2L, n_pcs)], 2L, "en"),
      sl_fmt_num(pca_pcts[1L], 2L, "en"),
      if (n_pcs >= 2L) sl_fmt_num(pca_pcts[2L], 2L, "en") else "0",
      kaiser_count),
    sprintf("• **Cluster Partitioning (HCA Ward.D2 & K-Means)**: Agglomerative hierarchical clustering segmented %d sample units into %d distinct clusters with a between-cluster separation efficiency of %s%% (BSS/TSS).",
      n_obs, k_clusters, sl_fmt_num(bss_pct, 2L, "en")),
    if (length(sig_cluster_vars) > 0L) {
      sprintf("• **Follow-Up Analysis — Key Cluster Drivers**: Univariate ANOVA confirmed that cluster profiles differ significantly (p < .05) driven primarily by parameter(s) [%s]. Detailed profiles are reported in Table 3 and the Cluster Profile plot.",
        paste(sig_cluster_vars, collapse = ", "))
    } else {
      "• **Follow-Up Analysis — Cluster Characteristics**: Distinct biological and environmental profiles across clusters are presented in Table 3."
    },
    sprintf("• **Sample Distribution**: Observation units partitioned as follows: %s. This provides a direct basis for targeted intervention and resource allocation.",
      paste(sprintf("Cluster %d (%d samples)", seq_len(k_clusters), as.vector(cl_counts)), collapse = ", "))
  )
  interp_en <- paste(pts_en, collapse = "\n")

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
    main = list(
      pc1_pct = pca_pcts[1L],
      pc2_pct = if (n_pcs >= 2L) pca_pcts[2L] else 0,
      cum2_pct = pca_cums[min(2L, n_pcs)],
      kaiser_count = kaiser_count,
      k_clusters = k_clusters,
      bss_pct = bss_pct
    ),
    posthoc = NULL,
    tables_apa = list(tbl_pca, tbl_load, tbl_prof, tbl_mem),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Model Komponen Utama (PCA)", en = "Principal Component Model"),
        latex = "Z_m = \\sum_{j=1}^p w_{mj} X_j, \\quad \\mathbf{\\Sigma} \\mathbf{w}_m = \\lambda_m \\mathbf{w}_m"
      ),
      list(
        title = list(id = "Jarak Disimilaritas Euclidean Terstandarisasi", en = "Standardized Euclidean Distance"),
        latex = "d(\\mathbf{x}_i, \\mathbf{x}_j) = \\sqrt{\\sum_{k=1}^p \\left(\\frac{x_{ik} - x_{jk}}{s_k}\\right)^2}"
      ),
      list(
        title = list(id = "Kriteria Aglomerasi Varians Minimum Ward", en = "Ward's Minimum Variance Agglomeration"),
        latex = "\\Delta ESS = \\frac{n_A n_B}{n_A + n_B} \\|\\bar{\\mathbf{x}}_A - \\bar{\\mathbf{x}}_B\\|^2"
      ),
      list(
        title = list(id = "Uji Sferisitas Bartlett untuk PCA", en = "Bartlett's Test of Sphericity"),
        latex = "\\chi^2 = -\\left(n - 1 - \\frac{2p+5}{6}\\right) \\ln |\\mathbf{R}|, \\quad \\text{db} = \\frac{p(p-1)}{2}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(warnings_id), en = sl_box_vec(warnings_en))
  )
}
