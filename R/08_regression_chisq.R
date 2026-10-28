# R/08_regression_chisq.R
# StatLab Linear Regression, Correlation, and Chi-Square Engine (pure Base R)
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

#' Linear Regression and Correlation Analysis
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_regression <- function(df, opts = list()) {
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
    stop("Regresi Linear membutuhkan minimal 2 kolom numerik (Variabel X Bebas dan Variabel Y Terikat).")
  }

  col_x <- cols[1L]
  col_y <- cols[2L]

  raw_x <- as.numeric(df[[col_x]])
  raw_y <- as.numeric(df[[col_y]])

  valid_idx <- which(is.finite(raw_x) & is.finite(raw_y))
  if (length(valid_idx) < 4L) {
    stop("Regresi Linear membutuhkan minimal 4 titik data pengamatan yang valid.")
  }

  x_val <- raw_x[valid_idx]
  y_val <- raw_y[valid_idx]
  n_pts <- length(x_val)

  # Fit Linear Model
  fit_lm <- stats::lm(y_val ~ x_val)
  sum_lm <- summary(fit_lm)
  anova_lm <- stats::anova(fit_lm)

  resids <- stats::residuals(fit_lm)
  fitted_vals <- stats::fitted(fit_lm)

  # Model Statistics
  r_squared <- as.double(sum_lm$r.squared)
  adj_r_squared <- as.double(sum_lm$adj.r.squared)
  pearson_cor <- stats::cor(x_val, y_val, method = "pearson")
  spearman_cor <- stats::cor(x_val, y_val, method = "spearman")

  # ANOVA Table components
  ss_reg <- as.double(anova_lm[["Sum Sq"]][1L])
  ss_res <- as.double(anova_lm[["Sum Sq"]][2L])
  ss_tot <- ss_reg + ss_res

  df_reg <- as.integer(anova_lm[["Df"]][1L])
  df_res <- as.integer(anova_lm[["Df"]][2L])
  df_tot <- df_reg + df_res

  ms_reg <- as.double(anova_lm[["Mean Sq"]][1L])
  ms_res <- as.double(anova_lm[["Mean Sq"]][2L])

  f_val <- as.double(anova_lm[["F value"]][1L])
  p_val <- as.double(anova_lm[["Pr(>F)"]][1L])

  fcrit_05 <- stats::qf(0.95, df_reg, df_res)
  fcrit_01 <- stats::qf(0.99, df_reg, df_res)

  # Coefficients
  coef_mat <- sum_lm$coefficients
  b0 <- as.double(coef_mat[1L, "Estimate"])
  se_b0 <- as.double(coef_mat[1L, "Std. Error"])
  t_b0 <- as.double(coef_mat[1L, "t value"])
  p_b0 <- as.double(coef_mat[1L, "Pr(>|t|)"])

  b1 <- as.double(coef_mat[2L, "Estimate"])
  se_b1 <- as.double(coef_mat[2L, "Std. Error"])
  t_b1 <- as.double(coef_mat[2L, "t value"])
  p_b1 <- as.double(coef_mat[2L, "Pr(>|t|)"])

  ci_b <- stats::confint(fit_lm, level = 1.0 - alpha)

  # Assumption Testing
  sw_res <- sl_shapiro_test(resids)
  lillie_res <- sl_lilliefors_test(resids)

  # Durbin-Watson statistic for autocorrelation
  diff_e <- diff(resids)
  dw_stat <- if (sum(resids^2) > 0) sum(diff_e^2) / sum(resids^2) else 2.0

  sig_code <- if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "tn"
  sig_code_en <- if (sig_code == "tn") "ns" else sig_code

  # =========================================================================
  # Build Tables
  # =========================================================================

  # Table 1: Model Summary & ANOVA Regresi
  hdr_anova_id <- sl_box_vec(c("Sumber Keragaman", "db", "Jumlah Kuadrat (JK)", "Kuadrat Tengah (KT)", "F-Hitung", "P-Value", "F-Tabel 5%", "Signifikansi"))
  hdr_anova_en <- sl_box_vec(c("Source of Variation", "df", "Sum of Squares (SS)", "Mean Square (MS)", "F-Statistic", "p-value", "F-Critical (0.05)", "Significance"))

  row_reg_id <- sl_box_vec(c("Regresi Linier", as.character(df_reg), sl_fmt_num(ss_reg, 4L, "id"), sl_fmt_num(ms_reg, 4L, "id"), sl_fmt_num(f_val, 4L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(fcrit_05, 4L, "id"), sig_code))
  row_res_id <- sl_box_vec(c("Sisaan (Galat)", as.character(df_res), sl_fmt_num(ss_res, 4L, "id"), sl_fmt_num(ms_res, 4L, "id"), "\u2014", "\u2014", "\u2014", "\u2014"))
  row_tot_id <- sl_box_vec(c("Total", as.character(df_tot), sl_fmt_num(ss_tot, 4L, "id"), "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"))

  row_reg_en <- sl_box_vec(c("Regression", as.character(df_reg), sl_fmt_num(ss_reg, 4L, "en"), sl_fmt_num(ms_reg, 4L, "en"), sl_fmt_num(f_val, 4L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(fcrit_05, 4L, "en"), sig_code_en))
  row_res_en <- sl_box_vec(c("Residual", as.character(df_res), sl_fmt_num(ss_res, 4L, "en"), sl_fmt_num(ms_res, 4L, "en"), "\u2014", "\u2014", "\u2014", "\u2014"))
  row_tot_en <- sl_box_vec(c("Total", as.character(df_tot), sl_fmt_num(ss_tot, 4L, "en"), "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"))

  tbl_anova <- list(
    id = "regression_anova",
    title = list(
      id = sprintf("Tabel 1. Sidik Ragam Model Regresi Linier (%s terhadap %s)", col_y, col_x),
      en = sprintf("Table 1. Linear Regression ANOVA Summary (%s on %s)", col_y, col_x)
    ),
    headers = list(id = hdr_anova_id, en = hdr_anova_en),
    rows = list(
      id = list(row_reg_id, row_res_id, row_tot_id),
      en = list(row_reg_en, row_res_en, row_tot_en)
    ),
    note = list(
      id = sprintf("R² = %s; Adj-R² = %s; Korelasi Pearson (r) = %s; Durbin-Watson (DW) = %s.",
        sl_fmt_num(r_squared, 4L, "id"), sl_fmt_num(adj_r_squared, 4L, "id"),
        sl_fmt_num(pearson_cor, 4L, "id"), sl_fmt_num(dw_stat, 3L, "id")),
      en = sprintf("R² = %s; Adj-R² = %s; Pearson Correlation (r) = %s; Durbin-Watson (DW) = %s.",
        sl_fmt_num(r_squared, 4L, "en"), sl_fmt_num(adj_r_squared, 4L, "en"),
        sl_fmt_num(pearson_cor, 4L, "en"), sl_fmt_num(dw_stat, 3L, "en"))
    )
  )

  # Table 2: Coefficients Table
  hdr_coef_id <- sl_box_vec(c("Parameter", "Koefisien (B)", "Standar Error (SE)", "t-Hitung", "P-Value", "95% CI Bawah", "95% CI Atas", "Signifikansi"))
  hdr_coef_en <- sl_box_vec(c("Parameter", "Coefficient (B)", "Std Error (SE)", "t-Statistic", "p-value", "Lower 95% CI", "Upper 95% CI", "Significance"))

  sig_b0 <- if (p_b0 < 0.01) "**" else if (p_b0 < 0.05) "*" else "tn"
  sig_b1 <- if (p_b1 < 0.01) "**" else if (p_b1 < 0.05) "*" else "tn"

  r_b0_id <- sl_box_vec(c("Konstanta (Intercept)", sl_fmt_num(b0, 4L, "id"), sl_fmt_num(se_b0, 4L, "id"), sl_fmt_num(t_b0, 4L, "id"), sl_fmt_p(p_b0, "id"), sl_fmt_num(ci_b[1L, 1L], 4L, "id"), sl_fmt_num(ci_b[1L, 2L], 4L, "id"), sig_b0))
  r_b1_id <- sl_box_vec(c(sprintf("Slope (%s)", col_x), sl_fmt_num(b1, 4L, "id"), sl_fmt_num(se_b1, 4L, "id"), sl_fmt_num(t_b1, 4L, "id"), sl_fmt_p(p_b1, "id"), sl_fmt_num(ci_b[2L, 1L], 4L, "id"), sl_fmt_num(ci_b[2L, 2L], 4L, "id"), sig_b1))

  r_b0_en <- sl_box_vec(c("Intercept", sl_fmt_num(b0, 4L, "en"), sl_fmt_num(se_b0, 4L, "en"), sl_fmt_num(t_b0, 4L, "en"), sl_fmt_p(p_b0, "en"), sl_fmt_num(ci_b[1L, 1L], 4L, "en"), sl_fmt_num(ci_b[1L, 2L], 4L, "en"), if (sig_b0 == "tn") "ns" else sig_b0))
  r_b1_en <- sl_box_vec(c(sprintf("Slope (%s)", col_x), sl_fmt_num(b1, 4L, "en"), sl_fmt_num(se_b1, 4L, "en"), sl_fmt_num(t_b1, 4L, "en"), sl_fmt_p(p_b1, "en"), sl_fmt_num(ci_b[2L, 1L], 4L, "en"), sl_fmt_num(ci_b[2L, 2L], 4L, "en"), if (sig_b1 == "tn") "ns" else sig_b1))

  tbl_coef <- list(
    id = "regression_coefficients",
    title = list(
      id = "Tabel 2. Estimasi Parameter Koefisien Persamaan Regresi",
      en = "Table 2. Regression Equation Parameter Estimates"
    ),
    headers = list(id = hdr_coef_id, en = hdr_coef_en),
    rows = list(
      id = list(r_b0_id, r_b1_id),
      en = list(r_b0_en, r_b1_en)
    ),
    note = list(
      id = sprintf("Persamaan Regresi: %s = %s + %s(%s)", col_y, sl_fmt_num(b0, 3L, "id"), sl_fmt_num(b1, 3L, "id"), col_x),
      en = sprintf("Regression Equation: %s = %s + %s(%s)", col_y, sl_fmt_num(b0, 3L, "en"), sl_fmt_num(b1, 3L, "en"), col_x)
    )
  )

  # =========================================================================
  # Plots (Scatter plot with trendline)
  # =========================================================================
  plots <- list()
  if (include_plots) {
    p_scat <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      graphics::plot(
        x_val, y_val,
        xlab = col_x, ylab = col_y,
        main = sprintf("Diagram Pencar & Garis Regresi: %s vs %s", col_y, col_x),
        col = "#1B365D", pch = 19, cex = 1.1,
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::points(x_val, y_val, col = "#1B365D", pch = 19, cex = 1.1)

      # Regression line
      graphics::abline(fit_lm, col = "#B83227", lwd = 2.0)

      # Equation legend
      eq_text <- sprintf("Y = %s + %s X\nR² = %s (p = %s)",
        sl_fmt_num(b0, 2L, plot_lang), sl_fmt_num(b1, 2L, plot_lang),
        sl_fmt_num(r_squared, 3L, plot_lang), sl_fmt_p(p_val, plot_lang))
      graphics::legend("topleft", legend = eq_text, bty = "n", text.col = "#B83227", text.font = 2, cex = 0.9)
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "regression_scatter",
      title = list(id = "Diagram Pencar Data & Garis Tren Regresi", en = "Data Scatter Plot & Regression Trendline"),
      png_data_url = p_scat$png_data_url,
      width_in = p_scat$width_in,
      height_in = p_scat$height_in
    )
  }

  # Interpretation
  interp_id <- if (p_val < 0.05) {
    sprintf(
      "Analisis regresi linier menunjukkan bahwa variabel %s memiliki pengaruh yang signifikan secara statistik terhadap %s (F(%d, %d) = %s, p = %s; R² = %s). Persamaan regresi yang terbentuk adalah Y = %s + %s X. Koefisien korelasi Pearson sebesar r = %s menunjukkan hubungan linier yang kuat.",
      col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(p_val, "id"),
      sl_fmt_num(r_squared, 3L, "id"), sl_fmt_num(b0, 3L, "id"), sl_fmt_num(b1, 3L, "id"), sl_fmt_num(pearson_cor, 3L, "id")
    )
  } else {
    sprintf(
      "Analisis regresi linier menunjukkan bahwa variabel %s tidak berpengaruh signifikan terhadap %s (F(%d, %d) = %s, p = %s; R² = %s).",
      col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(r_squared, 3L, "id")
    )
  }

  interp_en <- if (p_val < 0.05) {
    sprintf(
      "Linear regression analysis revealed a statistically significant relationship where %s significantly predicted %s, F(%d, %d) = %s, p = %s, R² = %s. The fitted model equation is Y = %s + %s X. The Pearson correlation coefficient r = %s indicates strong linear association.",
      col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(p_val, "en"),
      sl_fmt_num(r_squared, 3L, "en"), sl_fmt_num(b0, 3L, "en"), sl_fmt_num(b1, 3L, "en"), sl_fmt_num(pearson_cor, 3L, "en")
    )
  } else {
    sprintf(
      "Linear regression analysis indicated that %s did not have a statistically significant predictive effect on %s, F(%d, %d) = %s, p = %s, R² = %s.",
      col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(r_squared, 3L, "en")
    )
  }

  meta <- list(
    analysis_type = "regression",
    analysis_label = list(id = "Regresi Linier & Korelasi", en = "Linear Regression & Correlation"),
    response_label = sprintf("%s vs %s", col_y, col_x),
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(n_pts),
    n_valid = as.integer(n_pts),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(col_x, col_y)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(
      normality_violated = isTRUE(sw_res$valid && sw_res$p_value < alpha),
      homogeneity_violated = FALSE,
      durbin_watson = dw_stat
    ),
    outliers = list(flagged = data.frame()),
    main = list(
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      pearson = pearson_cor,
      spearman = spearman_cor,
      intercept = b0,
      slope = b1,
      f_statistic = f_val,
      p_value = p_val
    ),
    posthoc = NULL,
    tables_apa = list(tbl_anova, tbl_coef),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Model Regresi Linier", en = "Linear Regression Model"),
        latex = "Y = \\beta_0 + \\beta_1 X + \\epsilon, \\quad \\epsilon \\sim \\mathcal{N}(0, \\sigma^2)"
      ),
      list(
        title = list(id = "Koefisien Korelasi Pearson", en = "Pearson Correlation"),
        latex = "r = \\frac{\\sum (X_i - \\bar{X})(Y_i - \\bar{Y})}{\\sqrt{\\sum(X_i - \\bar{X})^2 \\sum(Y_i - \\bar{Y})^2}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Chi-Square Test of Independence
#'
#' @param df Rectangular data.frame or contingency matrix
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_chisq <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  cols <- names(df)
  # Check if df is already a contingency table (numeric columns with row headers)
  num_mask <- vapply(df, is.numeric, logical(1L))

  tbl <- NULL
  if (sum(num_mask) >= 2L && all(num_mask)) {
    tbl <- as.matrix(df)
  } else if (length(cols) >= 2L) {
    # 2 categorical columns
    f1 <- factor(as.character(df[[cols[1L]]]))
    f2 <- factor(as.character(df[[cols[2L]]]))
    tbl <- table(f1, f2)
  } else {
    stop("Uji Chi-Square membutuhkan minimal 2 variabel kategorik atau tabel kontingensi numerik.")
  }

  ct <- stats::chisq.test(tbl, correct = FALSE)
  stat_chi <- as.double(ct$statistic)
  df_chi <- as.integer(ct$parameter)
  p_chi <- as.double(ct$p.value)

  # Cramér's V
  n_total <- sum(tbl)
  min_dim <- min(nrow(tbl) - 1L, ncol(tbl) - 1L)
  cramer_v <- if (n_total > 0 && min_dim > 0) sqrt(stat_chi / (n_total * min_dim)) else 0.0

  sig_code <- if (p_chi < 0.01) "**" else if (p_chi < 0.05) "*" else "tn"
  sig_code_en <- if (sig_code == "tn") "ns" else sig_code

  # Table 1: Summary Table
  hdr_id <- sl_box_vec(c("Uji Asosiasi", "Statistik (χ²)", "db", "P-Value", "Effect Size (Cramér's V)", "Signifikansi"))
  hdr_en <- sl_box_vec(c("Association Test", "Statistic (χ²)", "df", "p-value", "Effect Size (Cramér's V)", "Significance"))

  r_id <- sl_box_vec(c("Pearson Chi-Square", sl_fmt_num(stat_chi, 4L, "id"), as.character(df_chi), sl_fmt_p(p_chi, "id"), sl_fmt_num(cramer_v, 4L, "id"), sig_code))
  r_en <- sl_box_vec(c("Pearson Chi-Square", sl_fmt_num(stat_chi, 4L, "en"), as.character(df_chi), sl_fmt_p(p_chi, "en"), sl_fmt_num(cramer_v, 4L, "en"), sig_code_en))

  tbl_summary <- list(
    id = "chisq_summary",
    title = list(id = "Tabel 1. Ringkasan Uji Chi-Square Independensi", en = "Table 1. Chi-Square Test of Independence Summary"),
    headers = list(id = hdr_id, en = hdr_en),
    rows = list(id = list(r_id), en = list(r_en)),
    note = list(
      id = sprintf("Total sampel N = %d. Derajat asosiasi Cramér's V = %s.", n_total, sl_fmt_num(cramer_v, 3L, "id")),
      en = sprintf("Total sample N = %d. Cramér's V = %s.", n_total, sl_fmt_num(cramer_v, 3L, "en"))
    )
  )

  interp_id <- if (p_chi < 0.05) {
    sprintf("Uji Chi-Square independensi menunjukkan terdapat hubungan yang signifikan secara statistik antara kedua variabel (χ²(%d) = %s, p = %s; Cramér's V = %s).",
      df_chi, sl_fmt_num(stat_chi, 3L, "id"), sl_fmt_p(p_chi, "id"), sl_fmt_num(cramer_v, 3L, "id"))
  } else {
    sprintf("Uji Chi-Square independensi menunjukkan tidak terdapat hubungan yang signifikan antara kedua variabel (χ²(%d) = %s, p = %s). Kedua variabel bersifat independen.",
      df_chi, sl_fmt_num(stat_chi, 3L, "id"), sl_fmt_p(p_chi, "id"))
  }

  interp_en <- if (p_chi < 0.05) {
    sprintf("Chi-Square test of independence indicated a statistically significant association between the two variables, χ²(%d) = %s, p = %s, Cramér's V = %s.",
      df_chi, sl_fmt_num(stat_chi, 3L, "en"), sl_fmt_p(p_chi, "en"), sl_fmt_num(cramer_v, 3L, "en"))
  } else {
    sprintf("Chi-Square test of independence indicated no statistically significant association between the variables, χ²(%d) = %s, p = %s. The variables are independent.",
      df_chi, sl_fmt_num(stat_chi, 3L, "en"), sl_fmt_p(p_chi, "en"))
  }

  meta <- list(
    analysis_type = "chisquare_indep",
    analysis_label = list(id = "Uji Chi-Square Independensi", en = "Chi-Square Test of Independence"),
    response_label = "Frekuensi Kontingensi",
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(n_total),
    n_valid = as.integer(n_total),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(colnames(tbl)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(chi_statistic = stat_chi, df = df_chi, p_value = p_chi, cramer_v = cramer_v),
    posthoc = NULL,
    tables_apa = list(tbl_summary),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Rumus Chi-Square", en = "Chi-Square Formula"),
        latex = "\\chi^2 = \\sum \\frac{(O_{ij} - E_{ij})^2}{E_{ij}}, \\quad E_{ij} = \\frac{R_i C_j}{N}"
      )
    ),
    plots = list(),
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
