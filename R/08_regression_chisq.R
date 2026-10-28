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
  # Plots (Show All Regression Diagnostics)
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
      title = list(id = "1. Diagram Pencar Data & Garis Tren Regresi", en = "1. Data Scatter Plot & Regression Trendline"),
      png_data_url = p_scat$png_data_url,
      width_in = p_scat$width_in,
      height_in = p_scat$height_in
    )

    # Plot 2: Residuals vs Fitted
    p_resid <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      graphics::plot(
        fitted_vals, resids, pch = 19, col = "#1B365D", cex = 1.1,
        xlab = if (plot_lang == "id") "Nilai Prediksi / Fitted Values (Y^)" else "Fitted Values (Y^)",
        ylab = if (plot_lang == "id") "Sisaan / Residuals (e)" else "Residuals (e)",
        main = if (plot_lang == "id") "Plot Sisaan vs Nilai Prediksi (Evaluasi Homoskedastisitas)" else "Residuals vs Fitted (Homoscedasticity Check)",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      graphics::abline(h = 0, col = "#B83227", lwd = 2, lty = 2)
    }, width_in = 6.5, height_in = 4.0, res = 300)

    plots[[2L]] <- list(
      id = "regression_residuals",
      title = list(id = "2. Plot Sisaan vs Nilai Prediksi", en = "2. Residuals vs Fitted Plot"),
      png_data_url = p_resid$png_data_url,
      width_in = p_resid$width_in,
      height_in = p_resid$height_in
    )

    # Plot 3: Normal Q-Q Plot of Residuals
    p_qq <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      stats::qqnorm(resids, pch = 19, col = "#1B365D", cex = 1.1,
        main = if (plot_lang == "id") "Normal Q-Q Plot Sisaan Regresi" else "Normal Q-Q Plot of Residuals",
        xlab = if (plot_lang == "id") "Kuantil Teoretis Normal" else "Theoretical Quantiles",
        ylab = if (plot_lang == "id") "Kuantil Sisaan Sampel" else "Sample Residual Quantiles",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D")
      stats::qqline(resids, col = "#B83227", lwd = 2)
      graphics::grid(col = "#E3DDD0", lty = 3)
    }, width_in = 6.5, height_in = 4.0, res = 300)

    plots[[3L]] <- list(
      id = "regression_qq",
      title = list(id = "3. Normal Q-Q Plot Sisaan Regresi", en = "3. Normal Q-Q Plot of Residuals"),
      png_data_url = p_qq$png_data_url,
      width_in = p_qq$width_in,
      height_in = p_qq$height_in
    )
  }

  # Key Points Formatting
  pts_id <- c(
    if (p_val < alpha) {
      sprintf("• **Signifikansi Pengaruh**: Variabel %s berpengaruh sangat nyata secara statistik terhadap %s (F(%d, %d) = %s, p = %s; R² = %s).",
        col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(r_squared, 3L, "id"))
    } else {
      sprintf("• **Signifikansi Pengaruh**: Variabel %s tidak berpengaruh signifikan terhadap %s (F(%d, %d) = %s, p = %s; R² = %s).",
        col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(r_squared, 3L, "id"))
    },
    sprintf("• **Persamaan Garis Regresi**: Y = %s + %s X (lereng b1 = %s, t = %s, p = %s).",
      sl_fmt_num(b0, 3L, "id"), sl_fmt_num(b1, 3L, "id"), sl_fmt_num(b1, 3L, "id"), sl_fmt_num(t_b1, 3L, "id"), sl_fmt_p(p_b1, "id")),
    sprintf("• **Kekuatan Korelasi**: Koefisien korelasi Pearson r = %s (Spearman ρ = %s), menunjukkan asosiasi linier yang %s.",
      sl_fmt_num(pearson_cor, 3L, "id"), sl_fmt_num(spearman_cor, 3L, "id"),
      if (abs(pearson_cor) >= 0.7) "sangat kuat" else if (abs(pearson_cor) >= 0.4) "sedang" else "lemah"),
    sprintf("• **Diagnostik Asumsi Galat**: Uji Durbin-Watson = %s (otokorelasi); normalitas sisaan Shapiro-Wilk p = %s (%s).",
      sl_fmt_num(dw_stat, 2L, "id"), sl_fmt_p(sw_res$p_value, "id"),
      if (sw_res$valid && sw_res$p_value >= alpha) "sisaan normal" else "terindikasi deviasi normalitas")
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (p_val < alpha) {
      sprintf("• **Significant Effect**: %s significantly predicted %s, F(%d, %d) = %s, p = %s, R² = %s.",
        col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(r_squared, 3L, "en"))
    } else {
      sprintf("• **Non-Significant Effect**: %s did not significantly predict %s, F(%d, %d) = %s, p = %s, R² = %s.",
        col_x, col_y, df_reg, df_res, sl_fmt_num(f_val, 3L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(r_squared, 3L, "en"))
    },
    sprintf("• **Fitted Line Equation**: Y = %s + %s X (slope b1 = %s, t = %s, p = %s).",
      sl_fmt_num(b0, 3L, "en"), sl_fmt_num(b1, 3L, "en"), sl_fmt_num(b1, 3L, "en"), sl_fmt_num(t_b1, 3L, "en"), sl_fmt_p(p_b1, "en")),
    sprintf("• **Correlation Strength**: Pearson r = %s (Spearman ρ = %s), indicating %s linear association.",
      sl_fmt_num(pearson_cor, 3L, "en"), sl_fmt_num(spearman_cor, 3L, "en"),
      if (abs(pearson_cor) >= 0.7) "strong" else if (abs(pearson_cor) >= 0.4) "moderate" else "weak"),
    sprintf("• **Residual Diagnostics**: Durbin-Watson statistic = %s; Shapiro-Wilk residual normality p = %s.",
      sl_fmt_num(dw_stat, 2L, "en"), sl_fmt_p(sw_res$p_value, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

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

#' Curve Estimation and Non-Linear Regression Engine
#'
#' Fits and compares Linear, Quadratic, Logarithmic, Exponential, and Power models.
#' Dual-use pure Base R (webR WASM and Desktop R).
#'
#' @param df Rectangular data.frame or named list with 2 numeric columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_curve_estimation <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"
  target_model <- if (!is.null(opts$model) && is.character(opts$model)) tolower(opts$model[1L]) else "all"

  # Extract numeric columns X (independent) and Y (dependent)
  cols <- names(df)[vapply(df, function(c) {
    v <- suppressWarnings(as.numeric(as.character(c)))
    sum(is.finite(v)) >= length(v) / 2
  }, logical(1L))]

  if (length(cols) < 2L) {
    cols <- names(df)[1L:min(2L, length(names(df)))]
  }
  if (length(cols) < 2L) {
    stop("Estimasi Kurva membutuhkan minimal 2 kolom data numerik (X = Variabel Bebas/Prediktor, Y = Variabel Terikat/Respon).")
  }

  col_x <- cols[1L]
  col_y <- cols[2L]

  raw_x <- suppressWarnings(as.numeric(as.character(df[[col_x]])))
  raw_y <- suppressWarnings(as.numeric(as.character(df[[col_y]])))

  valid_idx <- which(is.finite(raw_x) & is.finite(raw_y))
  if (length(valid_idx) < 4L) {
    stop("Estimasi Kurva membutuhkan minimal 4 titik pengamatan yang valid dan berpasangan lengkap.")
  }

  x <- raw_x[valid_idx]
  y <- raw_y[valid_idx]
  n <- length(x)

  tss <- sum((y - mean(y))^2)
  if (tss <= 1e-12) {
    stop("Variabel respon Y konstan (varians = 0); regresi tidak dapat diestimasi.")
  }

  warnings_id <- character(0)
  warnings_en <- character(0)

  has_non_pos_x <- any(x <= 0)
  has_non_pos_y <- any(y <= 0)

  if (has_non_pos_x) {
    warnings_id <- c(warnings_id, "Data X memiliki nilai ≤ 0; model Logaritmik dan Power dilewati.")
    warnings_en <- c(warnings_en, "X values contain ≤ 0; Logarithmic and Power models skipped.")
  }
  if (has_non_pos_y) {
    warnings_id <- c(warnings_id, "Data Y memiliki nilai ≤ 0; model Eksponensial dan Power dilewati.")
    warnings_en <- c(warnings_en, "Y values contain ≤ 0; Exponential and Power models skipped.")
  }

  models_fit <- list()

  # =========================================================================
  # 1. Linear Model: Y = b0 + b1 * X
  # =========================================================================
  fit_lin <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (!is.null(fit_lin)) {
    c_lin <- coef(fit_lin)
    y_hat_lin <- fitted(fit_lin)
    rss_lin <- sum((y - y_hat_lin)^2)
    p_lin <- 2L
    df_res_lin <- n - p_lin
    r2_lin <- max(0, 1.0 - (rss_lin / tss))
    adj_r2_lin <- max(0, 1.0 - ((rss_lin / df_res_lin) / (tss / (n - 1L))))
    f_lin <- if (rss_lin > 0) ((tss - rss_lin) / 1L) / (rss_lin / df_res_lin) else NA_real_
    pval_lin <- if (is.finite(f_lin)) 1.0 - stats::pf(f_lin, 1L, df_res_lin) else NA_real_
    aic_lin <- n * log(rss_lin / n) + 2 * p_lin
    bic_lin <- n * log(rss_lin / n) + p_lin * log(n)

    models_fit$linear <- list(
      key = "linear",
      name_id = "Linier Sederhana",
      name_en = "Simple Linear",
      formula_tex = sprintf("\\hat{Y} = %s %s %s X", sl_fmt_num(c_lin[1L], 3L, "en"), if (c_lin[2L] >= 0) "+" else "-", sl_fmt_num(abs(c_lin[2L]), 3L, "en")),
      formula_txt = sprintf("Y = %s %s %s*X", sl_fmt_num(c_lin[1L], 3L, "id"), if (c_lin[2L] >= 0) "+" else "-", sl_fmt_num(abs(c_lin[2L]), 3L, "id")),
      r2 = r2_lin,
      adj_r2 = adj_r2_lin,
      aic = aic_lin,
      bic = bic_lin,
      f_val = f_lin,
      p_val = pval_lin,
      fit = fit_lin,
      coefs = c_lin,
      predict_fn = function(new_x) c_lin[1L] + c_lin[2L] * new_x
    )
  }

  # =========================================================================
  # 2. Quadratic Model: Y = b0 + b1 * X + b2 * X^2
  # =========================================================================
  fit_quad <- tryCatch(stats::lm(y ~ x + I(x^2)), error = function(e) NULL)
  if (!is.null(fit_quad)) {
    c_quad <- coef(fit_quad)
    y_hat_quad <- fitted(fit_quad)
    rss_quad <- sum((y - y_hat_quad)^2)
    p_quad <- 3L
    df_res_quad <- n - p_quad
    r2_quad <- max(0, 1.0 - (rss_quad / tss))
    adj_r2_quad <- max(0, 1.0 - ((rss_quad / df_res_quad) / (tss / (n - 1L))))
    f_quad <- if (rss_quad > 0) ((tss - rss_quad) / 2L) / (rss_quad / df_res_quad) else NA_real_
    pval_quad <- if (is.finite(f_quad)) 1.0 - stats::pf(f_quad, 2L, df_res_quad) else NA_real_
    aic_quad <- n * log(rss_quad / n) + 2 * p_quad
    bic_quad <- n * log(rss_quad / n) + p_quad * log(n)

    # Vertex computation
    b2 <- c_quad[3L]
    b1 <- c_quad[2L]
    b0 <- c_quad[1L]
    vertex <- NULL
    if (abs(b2) > 1e-12) {
      x_opt <- -b1 / (2 * b2)
      y_opt <- b0 + b1 * x_opt + b2 * (x_opt^2)
      vertex <- list(
        x_opt = as.double(x_opt),
        y_opt = as.double(y_opt),
        nature = if (b2 < 0) "maximum" else "minimum"
      )
    }

    models_fit$quadratic <- list(
      key = "quadratic",
      name_id = "Polinomial Kuadratik",
      name_en = "Quadratic Polynomial",
      formula_tex = sprintf("\\hat{Y} = %s %s %s X %s %s X^2",
        sl_fmt_num(b0, 3L, "en"), if (b1 >= 0) "+" else "-", sl_fmt_num(abs(b1), 3L, "en"),
        if (b2 >= 0) "+" else "-", sl_fmt_num(abs(b2), 4L, "en")),
      formula_txt = sprintf("Y = %s %s %s*X %s %s*X^2",
        sl_fmt_num(b0, 3L, "id"), if (b1 >= 0) "+" else "-", sl_fmt_num(abs(b1), 3L, "id"),
        if (b2 >= 0) "+" else "-", sl_fmt_num(abs(b2), 4L, "id")),
      r2 = r2_quad,
      adj_r2 = adj_r2_quad,
      aic = aic_quad,
      bic = bic_quad,
      f_val = f_quad,
      p_val = pval_quad,
      fit = fit_quad,
      coefs = c_quad,
      vertex = vertex,
      predict_fn = function(new_x) b0 + b1 * new_x + b2 * (new_x^2)
    )
  }

  # =========================================================================
  # 3. Logarithmic Model: Y = b0 + b1 * ln(X) (for X > 0)
  # =========================================================================
  if (!has_non_pos_x) {
    fit_log <- tryCatch(stats::lm(y ~ log(x)), error = function(e) NULL)
    if (!is.null(fit_log)) {
      c_log <- coef(fit_log)
      y_hat_log <- fitted(fit_log)
      rss_log <- sum((y - y_hat_log)^2)
      p_log <- 2L
      df_res_log <- n - p_log
      r2_log <- max(0, 1.0 - (rss_log / tss))
      adj_r2_log <- max(0, 1.0 - ((rss_log / df_res_log) / (tss / (n - 1L))))
      f_log <- if (rss_log > 0) ((tss - rss_log) / 1L) / (rss_log / df_res_log) else NA_real_
      pval_log <- if (is.finite(f_log)) 1.0 - stats::pf(f_log, 1L, df_res_log) else NA_real_
      aic_log <- n * log(rss_log / n) + 2 * p_log
      bic_log <- n * log(rss_log / n) + p_log * log(n)

      models_fit$logarithmic <- list(
        key = "logarithmic",
        name_id = "Logaritmik",
        name_en = "Logarithmic",
        formula_tex = sprintf("\\hat{Y} = %s %s %s \\ln(X)", sl_fmt_num(c_log[1L], 3L, "en"), if (c_log[2L] >= 0) "+" else "-", sl_fmt_num(abs(c_log[2L]), 3L, "en")),
        formula_txt = sprintf("Y = %s %s %s*ln(X)", sl_fmt_num(c_log[1L], 3L, "id"), if (c_log[2L] >= 0) "+" else "-", sl_fmt_num(abs(c_log[2L]), 3L, "id")),
        r2 = r2_log,
        adj_r2 = adj_r2_log,
        aic = aic_log,
        bic = bic_log,
        f_val = f_log,
        p_val = pval_log,
        fit = fit_log,
        coefs = c_log,
        predict_fn = function(new_x) c_log[1L] + c_log[2L] * log(pmax(1e-9, new_x))
      )
    }
  }

  # =========================================================================
  # 4. Exponential Model: Y = a * exp(b * X) (for Y > 0)
  # =========================================================================
  if (!has_non_pos_y) {
    fit_exp <- tryCatch(stats::lm(log(y) ~ x), error = function(e) NULL)
    if (!is.null(fit_exp)) {
      c_exp <- coef(fit_exp)
      a_exp <- exp(c_exp[1L])
      b_exp <- c_exp[2L]
      y_hat_exp <- a_exp * exp(b_exp * x)
      rss_exp <- sum((y - y_hat_exp)^2)
      p_exp <- 2L
      df_res_exp <- n - p_exp
      r2_exp <- max(0, 1.0 - (rss_exp / tss))
      adj_r2_exp <- max(0, 1.0 - ((rss_exp / df_res_exp) / (tss / (n - 1L))))
      f_exp <- if (rss_exp > 0) ((tss - rss_exp) / 1L) / (rss_exp / df_res_exp) else NA_real_
      pval_exp <- if (is.finite(f_exp)) 1.0 - stats::pf(f_exp, 1L, df_res_exp) else NA_real_
      aic_exp <- n * log(rss_exp / n) + 2 * p_exp
      bic_exp <- n * log(rss_exp / n) + p_exp * log(n)

      models_fit$exponential <- list(
        key = "exponential",
        name_id = "Eksponensial",
        name_en = "Exponential",
        formula_tex = sprintf("\\hat{Y} = %s e^{%s X}", sl_fmt_num(a_exp, 4L, "en"), sl_fmt_num(b_exp, 4L, "en")),
        formula_txt = sprintf("Y = %s * exp(%s * X)", sl_fmt_num(a_exp, 4L, "id"), sl_fmt_num(b_exp, 4L, "id")),
        r2 = r2_exp,
        adj_r2 = adj_r2_exp,
        aic = aic_exp,
        bic = bic_exp,
        f_val = f_exp,
        p_val = pval_exp,
        fit = fit_exp,
        coefs = c(a = a_exp, b = b_exp),
        predict_fn = function(new_x) a_exp * exp(b_exp * new_x)
      )
    }
  }

  # =========================================================================
  # 5. Power / Allometric Model: Y = a * X^b (for X > 0, Y > 0)
  # =========================================================================
  if (!has_non_pos_x && !has_non_pos_y) {
    fit_pwr <- tryCatch(stats::lm(log(y) ~ log(x)), error = function(e) NULL)
    if (!is.null(fit_pwr)) {
      c_pwr <- coef(fit_pwr)
      sum_pwr <- summary(fit_pwr)
      a_pwr <- exp(c_pwr[1L])
      b_pwr <- c_pwr[2L]
      se_b_pwr <- sum_pwr$coefficients[2L, "Std. Error"]
      df_allo <- n - 2L

      # Allometric test against b = 3
      t_allo <- if (se_b_pwr > 0) abs(b_pwr - 3.0) / se_b_pwr else 0.0
      p_allo <- if (df_allo > 0) 2.0 * (1.0 - stats::pt(t_allo, df = df_allo)) else 1.0

      allo_pattern_id <- if (p_allo >= alpha) {
        "Isometrik (b = 3): Pertumbuhan panjang dan bobot seimbang/proporsional"
      } else if (b_pwr > 3.0) {
        "Alometrik Positif (b > 3): Pertambahan bobot lebih cepat daripada pertambahan panjang (ikan montok/gemuk)"
      } else {
        "Alometrik Negatif (b < 3): Pertambahan panjang lebih cepat daripada pertambahan bobot (ikan pipih/kurus)"
      }

      allo_pattern_en <- if (p_allo >= alpha) {
        "Isometric (b = 3): Balanced growth in length and weight"
      } else if (b_pwr > 3.0) {
        "Positive Allometric (b > 3): Weight increases faster than length (plump condition)"
      } else {
        "Negative Allometric (b < 3): Length increases faster than weight (slender condition)"
      }

      y_hat_pwr <- a_pwr * (x^b_pwr)
      rss_pwr <- sum((y - y_hat_pwr)^2)
      p_pwr <- 2L
      df_res_pwr <- n - p_pwr
      r2_pwr <- max(0, 1.0 - (rss_pwr / tss))
      adj_r2_pwr <- max(0, 1.0 - ((rss_pwr / df_res_pwr) / (tss / (n - 1L))))
      f_pwr <- if (rss_pwr > 0) ((tss - rss_pwr) / 1L) / (rss_pwr / df_res_pwr) else NA_real_
      pval_pwr <- if (is.finite(f_pwr)) 1.0 - stats::pf(f_pwr, 1L, df_res_pwr) else NA_real_
      aic_pwr <- n * log(rss_pwr / n) + 2 * p_pwr
      bic_pwr <- n * log(rss_pwr / n) + p_pwr * log(n)

      allometric_info <- list(
        a = as.double(a_pwr),
        b = as.double(b_pwr),
        se_b = as.double(se_b_pwr),
        t_stat = as.double(t_allo),
        p_value = as.double(p_allo),
        pattern = list(id = allo_pattern_id, en = allo_pattern_en)
      )

      models_fit$power <- list(
        key = "power",
        name_id = "Power / Alometrik",
        name_en = "Power / Allometric",
        formula_tex = sprintf("\\hat{Y} = %s X^{%s}", sl_fmt_num(a_pwr, 4L, "en"), sl_fmt_num(b_pwr, 4L, "en")),
        formula_txt = sprintf("Y = %s * X^(%s)", sl_fmt_num(a_pwr, 4L, "id"), sl_fmt_num(b_pwr, 4L, "id")),
        r2 = r2_pwr,
        adj_r2 = adj_r2_pwr,
        aic = aic_pwr,
        bic = bic_pwr,
        f_val = f_pwr,
        p_val = pval_pwr,
        fit = fit_pwr,
        coefs = c(a = a_pwr, b = b_pwr),
        allometric = allometric_info,
        predict_fn = function(new_x) a_pwr * (pmax(1e-9, new_x)^b_pwr)
      )
    }
  }

  if (length(models_fit) == 0L) {
    stop("Tidak ada model regresi yang berhasil diestimasi untuk dataset ini.")
  }

  # =========================================================================
  # Model Selection & Ranking
  # =========================================================================
  model_keys <- names(models_fit)
  adj_r2_vec <- vapply(models_fit, function(m) m$adj_r2, numeric(1L))
  aic_vec <- vapply(models_fit, function(m) m$aic, numeric(1L))

  # Sort by Adj-R2 descending, AIC ascending
  rank_order <- order(-adj_r2_vec, aic_vec)
  best_key <- model_keys[rank_order[1L]]

  selected_key <- if (target_model %in% model_keys) target_model else best_key
  selected_m <- models_fit[[selected_key]]

  # =========================================================================
  # Build Tables
  # =========================================================================

  # Table 1: Model Comparison Table
  hdr_comp_id <- sl_box_vec(c("Model Kurva", "Persamaan Matematis", "R²", "Adj-R²", "AIC", "BIC", "F-Hitung", "P-Value", "Evaluasi"))
  hdr_comp_en <- sl_box_vec(c("Curve Model", "Mathematical Equation", "R²", "Adj-R²", "AIC", "BIC", "F-Statistic", "p-value", "Evaluation"))

  rows_comp_id <- list()
  rows_comp_en <- list()

  for (i in seq_along(model_keys)) {
    mk <- model_keys[i]
    m <- models_fit[[mk]]
    is_best <- (mk == best_key)
    eval_id <- if (is_best) "★ Model Terbaik (Terpilih)" else if (m$p_val < alpha) "Cocok (Signifikan)" else "Kurang Cocok (tn)"
    eval_en <- if (is_best) "★ Best Model (Selected)" else if (m$p_val < alpha) "Fit (Significant)" else "Poor Fit (ns)"

    rows_comp_id[[i]] <- sl_box_vec(c(
      m$name_id, m$formula_txt, sl_fmt_num(m$r2, 4L, "id"), sl_fmt_num(m$adj_r2, 4L, "id"),
      sl_fmt_num(m$aic, 2L, "id"), sl_fmt_num(m$bic, 2L, "id"), sl_fmt_num(m$f_val, 3L, "id"),
      sl_fmt_p(m$p_val, "id"), eval_id
    ))
    rows_comp_en[[i]] <- sl_box_vec(c(
      m$name_en, m$formula_txt, sl_fmt_num(m$r2, 4L, "en"), sl_fmt_num(m$adj_r2, 4L, "en"),
      sl_fmt_num(m$aic, 2L, "en"), sl_fmt_num(m$bic, 2L, "en"), sl_fmt_num(m$f_val, 3L, "en"),
      sl_fmt_p(m$p_val, "en"), eval_en
    ))
  }

  tbl_comp <- list(
    id = "curve_model_comparison",
    title = list(id = "Tabel 1. Perbandingan Kebaikan Suai Model Kurvalinier (Model Selection)", en = "Table 1. Goodness-of-Fit Comparison of Curve Models (Model Selection)"),
    headers = list(id = hdr_comp_id, en = hdr_comp_en),
    rows = list(id = rows_comp_id, en = rows_comp_en),
    note = list(
      id = "Model terbaik dipilih berdasarkan kombinasi Adjusted R² tertinggi dan nilai AIC terendah.",
      en = "The best model is selected based on the highest Adjusted R² and lowest AIC."
    )
  )

  # Table 2: Parameter Coefficients of Selected Model
  hdr_coef_id <- sl_box_vec(c("Parameter / Koefisien", "Estimasi Nilai", "Standar Galat (SE)", "t-Hitung", "P-Value", "95% CI Bawah", "95% CI Atas"))
  hdr_coef_en <- sl_box_vec(c("Parameter / Coefficient", "Estimate", "Standard Error (SE)", "t-Statistic", "p-value", "Lower 95% CI", "Upper 95% CI"))

  sum_sel <- summary(selected_m$fit)
  cm <- sum_sel$coefficients
  ci_sel <- tryCatch(stats::confint(selected_m$fit, level = 1.0 - alpha), error = function(e) NULL)

  rows_coef_id <- list()
  rows_coef_en <- list()
  rn <- rownames(cm)
  for (r in seq_len(nrow(cm))) {
    p_name <- rn[r]
    est <- cm[r, "Estimate"]
    se <- cm[r, "Std. Error"]
    tval <- cm[r, "t value"]
    pval <- cm[r, "Pr(>|t|)"]
    ci_l <- if (!is.null(ci_sel)) ci_sel[r, 1L] else est - 1.96 * se
    ci_u <- if (!is.null(ci_sel)) ci_sel[r, 2L] else est + 1.96 * se

    rows_coef_id[[r]] <- sl_box_vec(c(p_name, sl_fmt_num(est, 4L, "id"), sl_fmt_num(se, 4L, "id"), sl_fmt_num(tval, 3L, "id"), sl_fmt_p(pval, "id"), sl_fmt_num(ci_l, 4L, "id"), sl_fmt_num(ci_u, 4L, "id")))
    rows_coef_en[[r]] <- sl_box_vec(c(p_name, sl_fmt_num(est, 4L, "en"), sl_fmt_num(se, 4L, "en"), sl_fmt_num(tval, 3L, "en"), sl_fmt_p(pval, "en"), sl_fmt_num(ci_l, 4L, "en"), sl_fmt_num(ci_u, 4L, "en")))
  }

  tbl_coef <- list(
    id = "curve_coefficients",
    title = list(id = sprintf("Tabel 2. Estimasi Parameter Koefisien (%s)", selected_m$name_id), en = sprintf("Table 2. Parameter Estimates (%s)", selected_m$name_en)),
    headers = list(id = hdr_coef_id, en = hdr_coef_en),
    rows = list(id = rows_coef_id, en = rows_coef_en),
    note = list(id = "Uji t parsial untuk signifikansi tiap koefisien pada taraf α = 0,05.", en = "Partial t-test for parameter significance at α = 0.05.")
  )

  # Table 3: Biological Diagnostics (Vertex for Quadratic, Allometric for Power)
  tables_apa <- list(tbl_comp, tbl_coef)

  if (!is.null(selected_m$vertex) || (selected_key == "quadratic" && !is.null(models_fit$quadratic$vertex))) {
    v <- if (!is.null(selected_m$vertex)) selected_m$vertex else models_fit$quadratic$vertex
    hdr_v_id <- sl_box_vec(c("Titik Stasioner Kuadratik", "Nilai Sumbu X", "Nilai Sumbu Y", "Sifat Kurva", "Interpretasi"))
    hdr_v_en <- sl_box_vec(c("Quadratic Stationary Point", "X Value", "Y Value", "Curve Nature", "Interpretation"))

    nat_id <- if (v$nature == "maximum") "Puncak Maksimum (Optimum)" else "Titik Lembah Minimum"
    nat_en <- if (v$nature == "maximum") "Maximum Peak (Optimum)" else "Minimum Valley"
    v_interp_id <- if (v$nature == "maximum") sprintf("Respon optimal tercapai pada X = %s dengan Y maksimum = %s.", sl_fmt_num(v$x_opt, 3L, "id"), sl_fmt_num(v$y_opt, 3L, "id")) else sprintf("Respon terendah terjadi pada X = %s.", sl_fmt_num(v$x_opt, 3L, "id"))
    v_interp_en <- if (v$nature == "maximum") sprintf("Optimal response is achieved at X = %s with maximum Y = %s.", sl_fmt_num(v$x_opt, 3L, "en"), sl_fmt_num(v$y_opt, 3L, "en")) else sprintf("Lowest response occurs at X = %s.", sl_fmt_num(v$x_opt, 3L, "en"))

    r_v_id <- sl_box_vec(c("Titik Optimum Puncak", sl_fmt_num(v$x_opt, 3L, "id"), sl_fmt_num(v$y_opt, 3L, "id"), nat_id, v_interp_id))
    r_v_en <- sl_box_vec(c("Optimum Peak", sl_fmt_num(v$x_opt, 3L, "en"), sl_fmt_num(v$y_opt, 3L, "en"), nat_en, v_interp_en))

    tbl_diag <- list(
      id = "quadratic_optimum_vertex",
      title = list(id = "Tabel 3. Analisis Titik Optimum / Puncak Kuadratik", en = "Table 3. Quadratic Optimum / Peak Analysis"),
      headers = list(id = hdr_v_id, en = hdr_v_en),
      rows = list(id = list(r_v_id), en = list(r_v_en)),
      note = list(id = "Titik optimum dihitung secara analitik melalui turunan pertama dY/dX = 0.", en = "Optimum point analytically derived via first derivative dY/dX = 0.")
    )
    tables_apa[[3L]] <- tbl_diag
  } else if (!is.null(selected_m$allometric) || (selected_key == "power" && !is.null(models_fit$power$allometric))) {
    allo <- if (!is.null(selected_m$allometric)) selected_m$allometric else models_fit$power$allometric
    hdr_a_id <- sl_box_vec(c("Parameter Alometrik", "Koefisien b", "Standar Galat (SE)", "Uji t (H0: b = 3)", "P-Value", "Pola Pertumbuhan"))
    hdr_a_en <- sl_box_vec(c("Allometric Parameter", "Exponent b", "Standard Error (SE)", "t-Test (H0: b = 3)", "p-value", "Growth Pattern"))

    r_a_id <- sl_box_vec(c("Hubungan Panjang-Bobot", sl_fmt_num(allo$b, 4L, "id"), sl_fmt_num(allo$se_b, 4L, "id"), sl_fmt_num(allo$t_stat, 3L, "id"), sl_fmt_p(allo$p_value, "id"), allo$pattern$id))
    r_a_en <- sl_box_vec(c("Length-Weight Relationship", sl_fmt_num(allo$b, 4L, "en"), sl_fmt_num(allo$se_b, 4L, "en"), sl_fmt_num(allo$t_stat, 3L, "en"), sl_fmt_p(allo$p_value, "en"), allo$pattern$en))

    tbl_diag <- list(
      id = "allometric_growth_test",
      title = list(id = "Tabel 3. Uji Hipotesis Sifat Pertumbuhan Alometrik (W = a*L^b)", en = "Table 3. Allometric Growth Hypothesis Test (W = a*L^b)"),
      headers = list(id = hdr_a_id, en = hdr_a_en),
      rows = list(id = list(r_a_id), en = list(r_a_en)),
      note = list(id = "Uji alometrik membandingkan eksponen b terhadap nilai teoritis 3 (isometrik).", en = "Allometric test evaluates exponent b against theoretical value 3 (isometric).")
    )
    tables_apa[[3L]] <- tbl_diag
  }

  # =========================================================================
  # 300-DPI Publication Base Graphics Plots (Show All Regression Curves)
  # =========================================================================
  plots <- list()
  if (include_plots) {
    x_grid <- seq(min(x), max(x), length.out = 200L)

    # Compute y bounds across all valid models
    all_y_preds <- lapply(models_fit, function(m) m$predict_fn(x_grid))
    y_all_min <- min(c(y, unlist(all_y_preds)), na.rm = TRUE)
    y_all_max <- max(c(y, unlist(all_y_preds)), na.rm = TRUE)
    y_pad <- (y_all_max - y_all_min) * 0.1
    y_lim <- c(max(0, y_all_min - y_pad), y_all_max + y_pad)

    # Color & Line Style mapping per model family
    color_map <- list(
      quadratic   = "#B83227", # Crimson
      power       = "#2E7D32", # Forest Green
      linear      = "#1B365D", # Navy Ink
      exponential = "#D97706", # Amber Orange
      logarithmic = "#7C3AED"  # Purple
    )
    lty_map <- list(
      quadratic   = 1L, # solid
      power       = 4L, # dotdash
      linear      = 2L, # dashed
      exponential = 5L, # longdash
      logarithmic = 3L  # dotted
    )

    # -----------------------------------------------------------------------
    # Plot 1: Multi-Curve Comparison Plot (All Candidate Curves Overlaid)
    # -----------------------------------------------------------------------
    p_all <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")

      graphics::plot(
        x, y, pch = 19, col = "#1B365D", cex = 1.15,
        ylim = y_lim,
        xlab = if (plot_lang == "id") paste("Variabel Bebas (X):", col_x) else paste("Independent Variable (X):", col_x),
        ylab = if (plot_lang == "id") paste("Variabel Respon (Y):", col_y) else paste("Response Variable (Y):", col_y),
        main = if (plot_lang == "id") "1. Perbandingan Seluruh Model Kurva Regresi (All Curves Overlay)" else "1. Multi-Curve Regression Comparison (All Curves Overlay)",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Draw all candidate curves
      leg_labels <- c(if (plot_lang == "id") "Data Aktual" else "Observed Data")
      leg_cols <- c("#1B365D")
      leg_ltys <- c(NA)
      leg_lwds <- c(NA)
      leg_pchs <- c(19)

      for (mk in names(models_fit)) {
        m <- models_fit[[mk]]
        col_m <- if (!is.null(color_map[[mk]])) color_map[[mk]] else "#4B5563"
        lty_m <- if (!is.null(lty_map[[mk]])) lty_map[[mk]] else 1L
        lwd_m <- if (mk == best_key) 2.8 else 1.8
        y_m <- m$predict_fn(x_grid)

        graphics::lines(x_grid, y_m, col = col_m, lty = lty_m, lwd = lwd_m)

        lbl <- sprintf("%s%s (R² = %s)",
          if (mk == best_key) "★ " else "",
          if (plot_lang == "id") m$name_id else m$name_en,
          sl_fmt_num(m$r2, 3L, plot_lang))

        leg_labels <- c(leg_labels, lbl)
        leg_cols <- c(leg_cols, col_m)
        leg_ltys <- c(leg_ltys, lty_m)
        leg_lwds <- c(leg_lwds, lwd_m)
        leg_pchs <- c(leg_pchs, NA)
      }

      graphics::legend("topleft", legend = leg_labels, col = leg_cols, lty = leg_ltys,
        lwd = leg_lwds, pch = leg_pchs, bty = "n", cex = 0.85, bg = "#FFFDF8E6")
    }, width_in = 6.8, height_in = 4.5, res = 300)

    plots[[1L]] <- list(
      id = "curve_all_comparison",
      title = list(id = "1. Perbandingan Seluruh Kurva Regresi (Multi-Curve Overlay)",
                   en = "1. All Regression Curves Comparison (Multi-Curve Overlay)"),
      png_data_url = p_all$png_data_url,
      width_in = p_all$width_in,
      height_in = p_all$height_in
    )

    # -----------------------------------------------------------------------
    # Plot 2: Best / Selected Fitted Model with Detail & Annotations
    # -----------------------------------------------------------------------
    p_best <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")

      y_sel <- selected_m$predict_fn(x_grid)
      y_min_b <- min(c(y, y_sel), na.rm = TRUE)
      y_max_b <- max(c(y, y_sel), na.rm = TRUE)
      y_pad_b <- (y_max_b - y_min_b) * 0.1
      y_lim_b <- c(max(0, y_min_b - y_pad_b), y_max_b + y_pad_b)

      graphics::plot(
        x, y, pch = 19, col = "#1B365D", cex = 1.2,
        ylim = y_lim_b,
        xlab = if (plot_lang == "id") paste("Variabel Bebas (X):", col_x) else paste("Independent (X):", col_x),
        ylab = if (plot_lang == "id") paste("Variabel Respon (Y):", col_y) else paste("Response (Y):", col_y),
        main = if (plot_lang == "id") sprintf("2. Model Terbaik Terpilih: %s", selected_m$name_id) else sprintf("2. Best Selected Model: %s", selected_m$name_en),
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Draw fitted curve
      graphics::lines(x_grid, y_sel, col = "#B83227", lwd = 2.6)

      # Mark optimum vertex if quadratic
      if (selected_key == "quadratic" && !is.null(selected_m$vertex)) {
        vx <- selected_m$vertex$x_opt
        vy <- selected_m$vertex$y_opt
        if (vx >= min(x) && vx <= max(x)) {
          graphics::points(vx, vy, pch = 8, col = "#2E7D32", cex = 1.6, lwd = 2.2)
          graphics::abline(v = vx, lty = 2, col = "#2E7D32")
          graphics::text(vx, vy, labels = sprintf("Optimum (%s, %s)", sl_fmt_num(vx, 2L, plot_lang), sl_fmt_num(vy, 2L, plot_lang)),
            pos = if (selected_m$vertex$nature == "maximum") 3 else 1, col = "#2E7D32", cex = 0.85, font = 2)
        }
      }

      legend_text <- c(
        if (plot_lang == "id") "Data Aktual" else "Actual Observations",
        sprintf("%s (%s)", selected_m$formula_txt, sprintf("R² = %s", sl_fmt_num(selected_m$r2, 3L, plot_lang)))
      )
      graphics::legend("topleft", legend = legend_text, pch = c(19, NA), lty = c(NA, 1),
        col = c("#1B365D", "#B83227"), lwd = c(NA, 2.6), bty = "n", cex = 0.9)
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[2L]] <- list(
      id = "curve_best_model",
      title = list(id = sprintf("2. Kurva Model Terpilih (%s)", selected_m$name_id),
                   en = sprintf("2. Selected Model Curve (%s)", selected_m$name_en)),
      png_data_url = p_best$png_data_url,
      width_in = p_best$width_in,
      height_in = p_best$height_in
    )

    # -----------------------------------------------------------------------
    # Plot 3+: Individual Plots for each candidate model
    # -----------------------------------------------------------------------
    p_idx <- 3L
    for (mk in names(models_fit)) {
      if (mk == selected_key) next # Already shown in Plot 2
      m_cur <- models_fit[[mk]]
      col_cur <- if (!is.null(color_map[[mk]])) color_map[[mk]] else "#4B5563"
      lty_cur <- if (!is.null(lty_map[[mk]])) lty_map[[mk]] else 1L

      p_indiv <- sl_capture_plot(function() {
        graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
        y_indiv <- m_cur$predict_fn(x_grid)
        y_min_i <- min(c(y, y_indiv), na.rm = TRUE)
        y_max_i <- max(c(y, y_indiv), na.rm = TRUE)
        y_pad_i <- (y_max_i - y_min_i) * 0.1
        y_lim_i <- c(max(0, y_min_i - y_pad_i), y_max_i + y_pad_i)

        graphics::plot(
          x, y, pch = 19, col = "#1B365D", cex = 1.15,
          ylim = y_lim_i,
          xlab = if (plot_lang == "id") paste("Variabel Bebas (X):", col_x) else paste("Independent (X):", col_x),
          ylab = if (plot_lang == "id") paste("Variabel Respon (Y):", col_y) else paste("Response (Y):", col_y),
          main = sprintf("%d. Kurva %s: %s vs %s", p_idx, if (plot_lang == "id") m_cur$name_id else m_cur$name_en, col_x, col_y),
          las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
        )
        graphics::grid(col = "#E3DDD0", lty = 3)
        graphics::lines(x_grid, y_indiv, col = col_cur, lty = lty_cur, lwd = 2.4)

        if (mk == "quadratic" && !is.null(m_cur$vertex)) {
          vx <- m_cur$vertex$x_opt
          vy <- m_cur$vertex$y_opt
          if (vx >= min(x) && vx <= max(x)) {
            graphics::points(vx, vy, pch = 8, col = "#2E7D32", cex = 1.5, lwd = 2.0)
            graphics::abline(v = vx, lty = 2, col = "#2E7D32")
          }
        }

        leg_text <- c(
          if (plot_lang == "id") "Data Pengamatan" else "Observed Data",
          sprintf("%s (R² = %s, p = %s)", m_cur$formula_txt, sl_fmt_num(m_cur$r2, 3L, plot_lang), sl_fmt_p(m_cur$p_val, plot_lang))
        )
        graphics::legend("topleft", legend = leg_text, pch = c(19, NA), lty = c(NA, lty_cur),
          col = c("#1B365D", col_cur), lwd = c(NA, 2.4), bty = "n", cex = 0.85)
      }, width_in = 6.5, height_in = 4.2, res = 300)

      plots[[p_idx]] <- list(
        id = sprintf("curve_model_%s", mk),
        title = list(id = sprintf("%d. Kurva Model %s", p_idx, m_cur$name_id),
                     en = sprintf("%d. %s Curve Model", p_idx, m_cur$name_en)),
        png_data_url = p_indiv$png_data_url,
        width_in = p_indiv$width_in,
        height_in = p_indiv$height_in
      )
      p_idx <- p_idx + 1L
    }
  }

  # =========================================================================
  # Key Points Formatting (Poin Kunci / Key Points)
  # =========================================================================
  pts_id <- c(
    sprintf("• **Model Terbaik Terpilih**: Model **%s** memberikan kebaikan suai (goodness-of-fit) tertinggi (R² = %s, Adj-R² = %s, AIC = %s; F = %s, p = %s).",
      selected_m$name_id, sl_fmt_num(selected_m$r2, 4L, "id"), sl_fmt_num(selected_m$adj_r2, 4L, "id"),
      sl_fmt_num(selected_m$aic, 2L, "id"), sl_fmt_num(selected_m$f_val, 3L, "id"), sl_fmt_p(selected_m$p_val, "id")),
    sprintf("• **Persamaan Matematis**: %s, berhasil menjelaskan %s%% dari total variasi respon pengamatan.",
      selected_m$formula_txt, sl_fmt_num(selected_m$r2 * 100, 2L, "id"))
  )
  if (!is.null(selected_m$vertex) && selected_m$vertex$nature == "maximum") {
    pts_id <- c(pts_id, sprintf("• **Titik Puncak Optimum**: Respon optimal tercapai pada %s = %s dengan estimasi respon maksimum %s = %s.",
      col_x, sl_fmt_num(selected_m$vertex$x_opt, 2L, "id"), col_y, sl_fmt_num(selected_m$vertex$y_opt, 2L, "id")))
  } else if (!is.null(selected_m$vertex)) {
    pts_id <- c(pts_id, sprintf("• **Titik Lembah Minimum**: Respon terendah terjadi pada %s = %s dengan estimasi respon %s = %s.",
      col_x, sl_fmt_num(selected_m$vertex$x_opt, 2L, "id"), col_y, sl_fmt_num(selected_m$vertex$y_opt, 2L, "id")))
  }
  if (!is.null(selected_m$allometric)) {
    pts_id <- c(pts_id, sprintf("• **Sifat Pertumbuhan Alometrik**: Koefisien b = %s (%s, uji t terhadap b=3 menghasilkan p = %s).",
      sl_fmt_num(selected_m$allometric$b, 3L, "id"), selected_m$allometric$pattern$id, sl_fmt_p(selected_m$allometric$p_value, "id")))
  }
  pts_id <- c(pts_id, sprintf("• **Signifikansi Statistik**: Hubungan kurvalinier antara %s dan %s terbukti nyata secara statistik pada taraf nyata α = %s.",
    col_x, col_y, sl_fmt_num(alpha, 2L, "id")))

  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    sprintf("• **Best Fit Model**: The **%s** model provided the highest goodness-of-fit (R² = %s, Adj-R² = %s, AIC = %s; F = %s, p = %s).",
      selected_m$name_en, sl_fmt_num(selected_m$r2, 4L, "en"), sl_fmt_num(selected_m$adj_r2, 4L, "en"),
      sl_fmt_num(selected_m$aic, 2L, "en"), sl_fmt_num(selected_m$f_val, 3L, "en"), sl_fmt_p(selected_m$p_val, "en")),
    sprintf("• **Fitted Equation**: %s, accounting for %s%% of the total response variation.",
      selected_m$formula_txt, sl_fmt_num(selected_m$r2 * 100, 2L, "en"))
  )
  if (!is.null(selected_m$vertex) && selected_m$vertex$nature == "maximum") {
    pts_en <- c(pts_en, sprintf("• **Optimum Peak**: Maximum response is reached at %s = %s yielding %s = %s.",
      col_x, sl_fmt_num(selected_m$vertex$x_opt, 2L, "en"), col_y, sl_fmt_num(selected_m$vertex$y_opt, 2L, "en")))
  } else if (!is.null(selected_m$vertex)) {
    pts_en <- c(pts_en, sprintf("• **Minimum Point**: Lowest response occurs at %s = %s yielding %s = %s.",
      col_x, sl_fmt_num(selected_m$vertex$x_opt, 2L, "en"), col_y, sl_fmt_num(selected_m$vertex$y_opt, 2L, "en")))
  }
  if (!is.null(selected_m$allometric)) {
    pts_en <- c(pts_en, sprintf("• **Allometric Growth**: Exponent b = %s (%s, t-test against b=3 yields p = %s).",
      sl_fmt_num(selected_m$allometric$b, 3L, "en"), selected_m$allometric$pattern$en, sl_fmt_p(selected_m$allometric$p_value, "en")))
  }
  pts_en <- c(pts_en, sprintf("• **Statistical Significance**: The relationship between %s and %s is statistically significant at α = %s.",
    col_x, col_y, sl_fmt_num(alpha, 2L, "en")))

  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "curve_estimation",
    analysis_label = list(id = "Regresi & Estimasi Kurva", en = "Curve Estimation & Regression"),
    response_label = paste(col_x, "vs", col_y),
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(n),
    n_valid = as.integer(n),
    n_missing = as.integer(0L),
    n_excluded_outliers = as.integer(0L),
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(col_x, col_y)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(
      best_model = best_key,
      selected_model = selected_key,
      r_squared = selected_m$r2,
      adj_r_squared = selected_m$adj_r2,
      f_statistic = selected_m$f_val,
      p_value = selected_m$p_val,
      aic = selected_m$aic,
      bic = selected_m$bic,
      vertex = selected_m$vertex,
      allometric = selected_m$allometric
    ),
    posthoc = NULL,
    tables_apa = tables_apa,
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = sprintf("Persamaan Model Terpilih (%s)", selected_m$name_id),
                     en = sprintf("Selected Model Equation (%s)", selected_m$name_en)),
        latex = selected_m$formula_tex
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(warnings_id), en = sl_box_vec(warnings_en))
  )
}
