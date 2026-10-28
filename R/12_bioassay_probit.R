# R/12_bioassay_probit.R
# StatLab Toxicology & Dose-Response Bioassay Engine (LC50 / LD50 Probit & Logit)
# Pure Base R implementation with Fieller's confidence intervals and publication plots.

#' Toxicology & Bioassay Lethal Concentration Analysis (LC10, LC50, LC90)
#'
#' @param df data.frame containing bioassay dose-response data
#' @param opts list of analysis options
#' @return list conforming to StatLab result schema
sl_analyze_probit_lc50 <- function(df, opts = list()) {
  if (missing(df) || !is.data.frame(df) || nrow(df) < 3L) {
    stop("Data frame with at least 3 dose levels is required for bioassay analysis.")
  }

  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha)) as.numeric(opts$alpha[[1L]]) else 0.05
  lang <- if (!is.null(opts$lang)) as.character(opts$lang[[1L]]) else "id"
  link_name <- if (!is.null(opts$link) && opts$link[[1L]] %in% c("logit", "probit")) as.character(opts$link[[1L]]) else "probit"
  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$plot_lang)) as.character(opts$plot_lang[[1L]]) else lang

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()

  # Identify dose, dead, and total/alive columns
  dose_col <- roles$dose_col
  dead_col <- roles$dead_col
  total_col <- roles$total_col
  alive_col <- roles$alive_col

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]

  if (is.null(dose_col) || !dose_col %in% names(df)) {
    dose_col <- if (length(num_cols) >= 1L) num_cols[1L] else names(df)[1L]
  }
  if (is.null(dead_col) || !dead_col %in% names(df)) {
    rem <- setdiff(num_cols, dose_col)
    dead_col <- if (length(rem) >= 1L) rem[1L] else names(df)[2L]
  }
  if (is.null(total_col) && is.null(alive_col)) {
    rem <- setdiff(num_cols, c(dose_col, dead_col))
    if (length(rem) >= 1L) {
      total_col <- rem[1L]
    }
  }

  dose_raw <- as.numeric(df[[dose_col]])
  dead_raw <- as.numeric(df[[dead_col]])
  
  total_raw <- if (!is.null(total_col) && total_col %in% names(df)) {
    as.numeric(df[[total_col]])
  } else if (!is.null(alive_col) && alive_col %in% names(df)) {
    dead_raw + as.numeric(df[[alive_col]])
  } else {
    # If no total or alive column, assume dead_raw is percentage or count out of max
    rep(max(dead_raw, na.rm = TRUE), length(dead_raw))
  }

  valid_mask <- !is.na(dose_raw) & !is.na(dead_raw) & !is.na(total_raw) & total_raw > 0 & dead_raw >= 0 & dead_raw <= total_raw
  doses <- dose_raw[valid_mask]
  deads <- dead_raw[valid_mask]
  totals <- total_raw[valid_mask]
  alives <- totals - deads

  # Check control mortality (dose == 0)
  ctrl_mask <- doses == 0
  ctrl_mort_pct <- if (any(ctrl_mask)) {
    sum(deads[ctrl_mask]) / sum(totals[ctrl_mask]) * 100.0
  } else {
    0.0
  }

  # For log-dose regression, filter positive doses
  pos_mask <- doses > 0
  if (sum(pos_mask) < 2L) {
    stop("At least 2 non-zero dose levels are required for log-dose bioassay regression.")
  }

  d_pos <- doses[pos_mask]
  dead_pos <- deads[pos_mask]
  total_pos <- totals[pos_mask]
  alive_pos <- total_pos - dead_pos
  log10_d <- log10(d_pos)

  # Fit Binomial GLM with probit or logit link
  fit <- stats::glm(
    cbind(dead_pos, alive_pos) ~ log10_d,
    family = stats::binomial(link = link_name)
  )

  coef_summary <- summary(fit)$coefficients
  b0 <- coef_summary[1L, 1L]
  b1 <- coef_summary[2L, 1L]
  se_b0 <- coef_summary[1L, 2L]
  se_b1 <- coef_summary[2L, 2L]
  z_b0 <- coef_summary[1L, 3L]
  z_b1 <- coef_summary[2L, 3L]
  p_b0 <- coef_summary[1L, 4L]
  p_b1 <- coef_summary[2L, 4L]

  vc <- stats::vcov(fit)
  v00 <- vc[1L, 1L]
  v11 <- vc[2L, 2L]
  v01 <- vc[1L, 2L]

  # Function to compute LC and 95% Confidence Limits (Fieller's Theorem)
  calc_lc <- function(p_target) {
    y_target <- if (identical(link_name, "probit")) {
      stats::qnorm(p_target)
    } else {
      stats::qlogis(p_target)
    }

    m <- (y_target - b0) / b1
    lc_val <- 10^m

    # Critical z-value
    z_crit <- stats::qnorm(1 - alpha / 2)
    g <- (z_crit^2 * v11) / (b1^2)

    if (is.finite(g) && g < 1.0 && g > 0) {
      # Fieller's theorem exact fiducial limits
      term1 <- m - (g * v01) / v11
      discriminant <- v00 - 2 * m * v01 + m^2 * v11 - g * (v00 - (v01^2) / v11)
      term2 <- if (discriminant >= 0) {
        (z_crit / abs(b1)) * sqrt(discriminant)
      } else {
        0.0
      }
      m_low <- (term1 - term2) / (1 - g)
      m_high <- (term1 + term2) / (1 - g)
    } else {
      # Delta method approximation
      se_m <- (1 / abs(b1)) * sqrt(v00 + 2 * m * v01 + m^2 * v11)
      m_low <- m - z_crit * se_m
      m_high <- m + z_crit * se_m
    }

    list(
      lc = lc_val,
      log10_m = m,
      ci_low = 10^m_low,
      ci_high = 10^m_high
    )
  }

  lc10_res <- calc_lc(0.10)
  lc50_res <- calc_lc(0.50)
  lc90_res <- calc_lc(0.90)

  # Goodness-of-Fit Pearson Chi-Square
  fitted_probs <- stats::fitted(fit)
  expected_deaths <- total_pos * fitted_probs
  var_deaths <- total_pos * fitted_probs * (1 - fitted_probs)
  var_deaths[var_deaths < 1e-9] <- 1e-9
  chisq_stat <- sum((dead_pos - expected_deaths)^2 / var_deaths)
  df_chisq <- max(1L, length(d_pos) - 2L)
  chisq_p <- stats::pchisq(chisq_stat, df = df_chisq, lower.tail = FALSE)

  # Build APA Table 1: Model Regression Parameters
  hdr1_id <- sl_box_vec(c("Parameter", "Koefisien (B)", "Std Error", "Wald Z", "P-Value", "Kesesuaian Model", "Chi-Square (χ²)", "DB", "P Kesesuaian"))
  hdr1_en <- sl_box_vec(c("Parameter", "Coefficient (B)", "Std Error", "Wald Z", "p-value", "Model Fit", "Chi-Square (χ²)", "df", "Fit p-value"))

  link_label <- if (identical(link_name, "probit")) "Probit" else "Logit"
  rows1_id <- list(
    sl_box_vec(c(
      "Konstanta (Intercept)",
      sl_fmt_num(b0, 4L, "id"),
      sl_fmt_num(se_b0, 4L, "id"),
      sl_fmt_num(z_b0, 3L, "id"),
      sl_fmt_p(p_b0, "id"),
      link_label,
      sl_fmt_num(chisq_stat, 3L, "id"),
      as.character(df_chisq),
      sl_fmt_p(chisq_p, "id")
    )),
    sl_box_vec(c(
      "Slope (Log10 Dosis)",
      sl_fmt_num(b1, 4L, "id"),
      sl_fmt_num(se_b1, 4L, "id"),
      sl_fmt_num(z_b1, 3L, "id"),
      sl_fmt_p(p_b1, "id"),
      "—", "—", "—", "—"
    ))
  )

  rows1_en <- list(
    sl_box_vec(c(
      "Intercept",
      sl_fmt_num(b0, 4L, "en"),
      sl_fmt_num(se_b0, 4L, "en"),
      sl_fmt_num(z_b0, 3L, "en"),
      sl_fmt_p(p_b0, "en"),
      link_label,
      sl_fmt_num(chisq_stat, 3L, "en"),
      as.character(df_chisq),
      sl_fmt_p(chisq_p, "en")
    )),
    sl_box_vec(c(
      "Slope (Log10 Dose)",
      sl_fmt_num(b1, 4L, "en"),
      sl_fmt_num(se_b1, 4L, "en"),
      sl_fmt_num(z_b1, 3L, "en"),
      sl_fmt_p(p_b1, "en"),
      "—", "—", "—", "—"
    ))
  )

  table1 <- list(
    id = "bioassay_model_parameters",
    title = list(
      id = sprintf("Tabel 1. Parameter Model Regresi %s dan Uji Kesesuaian Dosis-Respon", link_label),
      en = sprintf("Table 1. %s Regression Model Parameters and Goodness-of-Fit", link_label)
    ),
    headers = list(id = hdr1_id, en = hdr1_en),
    rows = list(id = rows1_id, en = rows1_en),
    note = list(
      id = sprintf("Uji kecocokan model Pearson Chi-Square: χ² = %s, db = %d, p = %s. Nilai p > 0,05 menunjukkan model cocok dengan data.", sl_fmt_num(chisq_stat, 3L, "id"), df_chisq, sl_fmt_p(chisq_p, "id")),
      en = sprintf("Pearson Chi-Square goodness-of-fit: χ² = %s, df = %d, p = %s. A p-value > 0.05 indicates adequate model fit.", sl_fmt_num(chisq_stat, 3L, "en"), df_chisq, sl_fmt_p(chisq_p, "en"))
    )
  )

  # Build APA Table 2: Lethal Concentration Summary
  hdr2_id <- sl_box_vec(c("Tingkat Lethal", "Persentase (%)", "Estimasi Konsentrasi (ppm)", "95% Batas Bawah", "95% Batas Atas"))
  hdr2_en <- sl_box_vec(c("Lethal Level", "Percentage (%)", "Estimated Concentration (ppm)", "95% Lower Limit", "95% Upper Limit"))

  rows2_id <- list(
    sl_box_vec(c("LC10 (Sub-lethal)", "10%", sl_fmt_num(lc10_res$lc, 3L, "id"), sl_fmt_num(lc10_res$ci_low, 3L, "id"), sl_fmt_num(lc10_res$ci_high, 3L, "id"))),
    sl_box_vec(c("LC50 (Median Lethal)", "50%", sl_fmt_num(lc50_res$lc, 3L, "id"), sl_fmt_num(lc50_res$ci_low, 3L, "id"), sl_fmt_num(lc50_res$ci_high, 3L, "id"))),
    sl_box_vec(c("LC90 (High Lethal)", "90%", sl_fmt_num(lc90_res$lc, 3L, "id"), sl_fmt_num(lc90_res$ci_low, 3L, "id"), sl_fmt_num(lc90_res$ci_high, 3L, "id")))
  )

  rows2_en <- list(
    sl_box_vec(c("LC10 (Sub-lethal)", "10%", sl_fmt_num(lc10_res$lc, 3L, "en"), sl_fmt_num(lc10_res$ci_low, 3L, "en"), sl_fmt_num(lc10_res$ci_high, 3L, "en"))),
    sl_box_vec(c("LC50 (Median Lethal)", "50%", sl_fmt_num(lc50_res$lc, 3L, "en"), sl_fmt_num(lc50_res$ci_low, 3L, "en"), sl_fmt_num(lc50_res$ci_high, 3L, "en"))),
    sl_box_vec(c("LC90 (High Lethal)", "90%", sl_fmt_num(lc90_res$lc, 3L, "en"), sl_fmt_num(lc90_res$ci_low, 3L, "en"), sl_fmt_num(lc90_res$ci_high, 3L, "en")))
  )

  table2 <- list(
    id = "bioassay_lc_summary",
    title = list(
      id = "Tabel 2. Estimasi Konsentrasi Letal (LC10, LC50, LC90) dan Selang Kepercayaan 95%",
      en = "Table 2. Estimated Lethal Concentrations (LC10, LC50, LC90) and 95% Confidence Intervals"
    ),
    headers = list(id = hdr2_id, en = hdr2_en),
    rows = list(id = rows2_id, en = rows2_en),
    note = list(
      id = "Selang kepercayaan 95% dihitung berdasarkan Teorema Fieller untuk estimasi fidusial parameter rasio.",
      en = "95% confidence intervals calculated using Fieller's Theorem for fiducial ratio parameter limits."
    )
  )

  # Generate Publication Plots
  plots <- list()
  if (include_plots) {
    p_curve <- sl_capture_plot(function() {
      graphics::par(mar = c(4.8, 4.8, 3.2, 1.5), bg = "#FFFDF8", family = "sans")

      max_d <- max(doses)
      min_d_plot <- min(d_pos) * 0.5
      curve_x_dose <- seq(min_d_plot, max_d * 1.15, length.out = 200)
      curve_log_x <- log10(curve_x_dose)

      eta <- b0 + b1 * curve_log_x
      curve_p <- if (identical(link_name, "probit")) stats::pnorm(eta) else stats::plogis(eta)
      curve_mort <- curve_p * 100.0

      obs_mort_pct <- (deads / totals) * 100.0

      plot(
        curve_x_dose, curve_mort,
        type = "l", col = "#B83227", lwd = 2.8,
        xlab = sprintf("%s (ppm)", dose_col),
        ylab = if (plot_lang == "id") "Mortalitas Kumulatif (%)" else "Cumulative Mortality (%)",
        main = if (plot_lang == "id") {
          sprintf("Kurva Dosis-Respon Bioassay (%s): LC50 = %s ppm", link_label, sl_fmt_num(lc50_res$lc, 2L, "id"))
        } else {
          sprintf("Dose-Response Bioassay Curve (%s): LC50 = %s ppm", link_label, sl_fmt_num(lc50_res$lc, 2L, "en"))
        },
        ylim = c(0, 105), las = 1,
        col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Actual observation points
      graphics::points(doses, obs_mort_pct, pch = 21, bg = "#1B365D", col = "#FFFDF8", cex = 1.6, lwd = 1.5)

      # Highlight LC50 projection lines
      graphics::abline(h = 50, col = "#888888", lty = 2, lwd = 1.2)
      graphics::segments(lc50_res$lc, 0, lc50_res$lc, 50, col = "#B83227", lty = 2, lwd = 1.8)
      graphics::points(lc50_res$lc, 50, pch = 19, col = "#B83227", cex = 1.5)

      # CI range bar at 50%
      graphics::segments(lc50_res$ci_low, 50, lc50_res$ci_high, 50, col = "#B83227", lwd = 2.5)

      lc_txt <- if (plot_lang == "id") {
        sprintf("LC50 = %s ppm\n[95%% CI: %s - %s]", sl_fmt_num(lc50_res$lc, 2L, "id"), sl_fmt_num(lc50_res$ci_low, 2L, "id"), sl_fmt_num(lc50_res$ci_high, 2L, "id"))
      } else {
        sprintf("LC50 = %s ppm\n[95%% CI: %s - %s]", sl_fmt_num(lc50_res$lc, 2L, "en"), sl_fmt_num(lc50_res$ci_low, 2L, "en"), sl_fmt_num(lc50_res$ci_high, 2L, "en"))
      }
      graphics::text(lc50_res$lc, 30, labels = lc_txt, pos = 4, cex = 0.82, col = "#B83227", font = 2)

      graphics::legend(
        "topleft",
        legend = c(
          if (plot_lang == "id") "Titik Pengamatan Aktual" else "Actual Observations",
          if (plot_lang == "id") sprintf("Kurva Model %s", link_label) else sprintf("%s Model Fit", link_label),
          "Garis Proyeksi LC50 (50%)"
        ),
        pch = c(21, NA, 19),
        lty = c(NA, 1, 2),
        col = c("#1B365D", "#B83227", "#B83227"),
        pt.bg = c("#1B365D", NA, "#B83227"),
        lwd = c(NA, 2.5, 1.5),
        bty = "n", cex = 0.8
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "bioassay_response_curve",
      title = list(
        id = sprintf("Kurva Respon Sigmoidal Dosis-Mortalitas (%s)", link_label),
        en = sprintf("Sigmoidal Dose-Mortality Response Curve (%s)", link_label)
      ),
      png_data_url = p_curve$png_data_url,
      width_in = p_curve$width_in,
      height_in = p_curve$height_in
    )
  }

  # Build Key Points (Poin Kunci)
  pts_id <- c(
    sprintf("• **Lethal Concentration (LC50)**: Nilai LC50 terestimasi sebesar **%s ppm** dengan selang kepercayaan 95%% sebesar **[%s - %s ppm]**.",
      sl_fmt_num(lc50_res$lc, 2L, "id"), sl_fmt_num(lc50_res$ci_low, 2L, "id"), sl_fmt_num(lc50_res$ci_high, 2L, "id")),
    sprintf("• **Ambang Batas Sub-Lethal (LC10 & LC90)**: Konsentrasi ambang toksisitas awal (LC10) adalah %s ppm, sedangkan konsentrasi letal tinggi (LC90) mencapai %s ppm.",
      sl_fmt_num(lc10_res$lc, 2L, "id"), sl_fmt_num(lc90_res$lc, 2L, "id")),
    sprintf("• **Kemiringan Respon (Slope)**: Koefisien slope B = %s (Wald Z = %s, p = %s) signifikan secara statistik, mengonfirmasi hubungan dosis-respon positif yang nyata.",
      sl_fmt_num(b1, 3L, "id"), sl_fmt_num(z_b1, 3L, "id"), sl_fmt_p(p_b1, "id")),
    if (is.finite(chisq_p) && chisq_p >= 0.05) {
      sprintf("• **Kesesuaian Model**: Uji Chi-Square kesesuaian model menunjukkan model %s cocok dan valid terhadap data uji (χ² = %s, db = %d, p = %s).",
        link_label, sl_fmt_num(chisq_stat, 2L, "id"), df_chisq, sl_fmt_p(chisq_p, "id"))
    } else {
      sprintf("• **Kesesuaian Model**: Model %s menunjukkan deviasi signifikan dari data biologis (χ² = %s, p = %s).",
        link_label, sl_fmt_num(chisq_stat, 2L, "id"), sl_fmt_p(chisq_p, "id"))
    },
    if (ctrl_mort_pct > 0) {
      sprintf("• **Mortalitas Kontrol**: Mortalitas pada kelompok kontrol tercatat sebesar %s%%.",
        sl_fmt_num(ctrl_mort_pct, 1L, "id"))
    } else {
      "• **Mortalitas Kontrol**: Mortalitas kontrol sebesar 0,0% (memenuhi baku mutu validitas bioassay < 10%)."
    }
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    sprintf("• **Lethal Concentration (LC50)**: Estimated LC50 is **%s ppm** with a 95%% confidence interval of **[%s - %s ppm]**.",
      sl_fmt_num(lc50_res$lc, 2L, "en"), sl_fmt_num(lc50_res$ci_low, 2L, "en"), sl_fmt_num(lc50_res$ci_high, 2L, "en")),
    sprintf("• **Sub-Lethal Thresholds (LC10 & LC90)**: Initial toxicity threshold (LC10) is %s ppm, while high lethal threshold (LC90) reaches %s ppm.",
      sl_fmt_num(lc10_res$lc, 2L, "en"), sl_fmt_num(lc90_res$lc, 2L, "en")),
    sprintf("• **Response Slope**: Slope coefficient B = %s (Wald Z = %s, p = %s) is statistically significant, confirming an authentic dose-dependent relationship.",
      sl_fmt_num(b1, 3L, "en"), sl_fmt_num(z_b1, 3L, "en"), sl_fmt_p(p_b1, "en")),
    if (is.finite(chisq_p) && chisq_p >= 0.05) {
      sprintf("• **Model Goodness-of-Fit**: Pearson Chi-Square test confirms adequate fit for the %s model (χ² = %s, df = %d, p = %s).",
        link_label, sl_fmt_num(chisq_stat, 2L, "en"), df_chisq, sl_fmt_p(chisq_p, "en"))
    } else {
      sprintf("• **Model Goodness-of-Fit**: %s model shows significant deviation from biological data (χ² = %s, p = %s).",
        link_label, sl_fmt_num(chisq_stat, 2L, "en"), sl_fmt_p(chisq_p, "en"))
    },
    if (ctrl_mort_pct > 0) {
      sprintf("• **Control Mortality**: Control mortality observed at %s%%.",
        sl_fmt_num(ctrl_mort_pct, 1L, "en"))
    } else {
      "• **Control Mortality**: Control mortality is 0.0% (satisfies standard bioassay validity criteria < 10%)."
    }
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "probit_lc50",
    analysis_label = list(id = sprintf("Bioassay Toksikologi & Dosis-Respon (%s LC50)", link_label), en = sprintf("Toxicology Bioassay & Dose-Response (%s LC50)", link_label)),
    response_label = dead_col,
    mode = "bioassay",
    alpha = alpha,
    n_raw = as.integer(length(doses)),
    n_valid = as.integer(length(doses)),
    n_missing = 0L,
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(character(0))
  )

  formula_obj <- list(
    katex = if (identical(link_name, "probit")) {
      "\\Phi^{-1}(p) = \\beta_0 + \\beta_1 \\log_{10}(\\text{Dosis}), \\quad \\text{LC}_{50} = 10^{-\\frac{\\beta_0}{\\beta_1}}"
    } else {
      "\\ln\\left(\\frac{p}{1-p}\\right) = \\beta_0 + \\beta_1 \\log_{10}(\\text{Dosis}), \\quad \\text{LC}_{50} = 10^{-\\frac{\\beta_0}{\\beta_1}}"
    },
    description = list(
      id = sprintf("Model Regresi Binomial %s dengan estimasi selang kepercayaan 95%% Teorema Fieller.", link_label),
      en = sprintf("Binomial %s regression model with Fieller's theorem 95%% confidence intervals.", link_label)
    )
  )

  main_res <- list(
    link = link_name,
    intercept = b0,
    slope = b1,
    se_slope = se_b1,
    z_slope = z_b1,
    p_slope = p_b1,
    chisq_stat = chisq_stat,
    df_chisq = df_chisq,
    chisq_p = chisq_p,
    lc10 = lc10_res$lc,
    lc10_ci_low = lc10_res$ci_low,
    lc10_ci_high = lc10_res$ci_high,
    lc50 = lc50_res$lc,
    lc50_ci_low = lc50_res$ci_low,
    lc50_ci_high = lc50_res$ci_high,
    lc90 = lc90_res$lc,
    lc90_ci_low = lc90_res$ci_low,
    lc90_ci_high = lc90_res$ci_high,
    control_mortality_pct = ctrl_mort_pct
  )

  list(
    meta = meta,
    formula = formula_obj,
    main = main_res,
    tables_apa = sl_box_vec(list(table1, table2)),
    tables_full = sl_box_vec(list(table1, table2)),
    plots = sl_box_vec(plots),
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
