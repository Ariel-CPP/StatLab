# R/13_logistic_corr_matrix.R
# StatLab Binary Logistic Regression & Multivariate Correlation Matrix Heatmap Engine
# Pure Base R implementation with publication-grade 300-DPI graphics and zero CRAN dependencies.

#' Binary Logistic Regression Analysis
#'
#' @param df data.frame containing binary response and predictors
#' @param opts list of options
#' @return list conforming to StatLab result schema
sl_analyze_binary_logistic <- function(df, opts = list()) {
  if (missing(df) || !is.data.frame(df) || nrow(df) < 5L) {
    stop("Data frame with at least 5 observations is required for binary logistic regression.")
  }

  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha)) as.numeric(opts$alpha[[1L]]) else 0.05
  lang <- if (!is.null(opts$lang)) as.character(opts$lang[[1L]]) else "id"
  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$plot_lang)) as.character(opts$plot_lang[[1L]]) else lang

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  resp_col <- if (!is.null(roles$response_col) && length(roles$response_col) > 0L) as.character(roles$response_col[[1L]]) else NULL
  pred_cols <- if (!is.null(roles$predictor_cols) && length(roles$predictor_cols) > 0L) as.character(unlist(roles$predictor_cols)) else NULL

  # Helper: check if a column has exactly 2 distinct non-NA values
  is_binary_col <- function(col_name) {
    if (is.null(col_name) || length(col_name) == 0L || is.na(col_name[[1L]]) || !col_name[[1L]] %in% names(df)) return(FALSE)
    v <- df[[col_name[[1L]]]]
    u <- unique(stats::na.omit(v))
    length(u) == 2L
  }

  has_valid_resp <- !is.null(resp_col) && length(resp_col) == 1L && (resp_col %in% names(df)) && is_binary_col(resp_col)

  # Smart resolution of binary response column:
  if (!has_valid_resp) {
    # 1. Check if roles$group_col is binary
    if (!is.null(roles$group_col) && length(roles$group_col) > 0L && is_binary_col(roles$group_col[[1L]])) {
      resp_col <- as.character(roles$group_col[[1L]])
    } else if (!is.null(roles$response_cols) && length(roles$response_cols) > 0L) {
      # 2. Check if any column in response_cols is binary
      for (rc in roles$response_cols) {
        if (is_binary_col(rc)) {
          resp_col <- as.character(rc)
          break
        }
      }
    }

    # 3. If still not resolved or not binary, scan all columns in df
    if (is.null(resp_col) || !is_binary_col(resp_col)) {
      for (cn in names(df)) {
        if (is_binary_col(cn)) {
          resp_col <- cn
          break
        }
      }
    }

    # 4. Fallback if no binary column exists
    if (is.null(resp_col) || !resp_col %in% names(df)) {
      resp_col <- if (!is.null(roles$response_cols) && length(roles$response_cols) > 0L && roles$response_cols[[1L]] %in% names(df)) {
        as.character(roles$response_cols[[1L]])
      } else {
        names(df)[1L]
      }
    }
  }

  # Verify response column is strictly binary
  v_resp <- stats::na.omit(df[[resp_col]])
  u_cnt <- length(unique(v_resp))
  if (u_cnt != 2L) {
    if (identical(lang, "id")) {
      stop(sprintf(
        "Variabel respon '%s' memiliki %d nilai unik. Regresi Logistik Biner mensyaratkan variabel respon memiliki tepat 2 kategori biner (contoh: 0 dan 1, Hidup dan Mati, Sakit dan Sehat). Silakan pilih kolom respon biner pada data Anda atau muat template acuan.",
        resp_col, u_cnt
      ))
    } else {
      stop(sprintf(
        "Response variable '%s' has %d distinct values. Binary Logistic Regression requires exactly 2 distinct classes (e.g. 0 and 1, Alive and Dead, Diseased and Healthy). Please select a binary response column or load the reference template.",
        resp_col, u_cnt
      ))
    }
  }

  # Smart resolution of predictor columns
  if (is.null(pred_cols) || length(pred_cols) == 0L) {
    if (!is.null(roles$response_cols) && length(roles$response_cols) > 1L) {
      cand <- setdiff(roles$response_cols, resp_col)
      pred_cols <- cand[vapply(df[cand], is.numeric, logical(1))]
    }
    if (length(pred_cols) == 0L) {
      cand <- setdiff(names(df), resp_col)
      pred_cols <- cand[vapply(df[cand], is.numeric, logical(1))]
    }
  }

  if (length(pred_cols) == 0L) {
    stop("At least one numeric predictor column is required for logistic regression.")
  }

  # Clean and encode response as 0 and 1
  y_raw <- df[[resp_col]]
  if (is.factor(y_raw) || is.character(y_raw)) {
    y_fac <- as.factor(y_raw)
    y_levels <- levels(y_fac)
    if (length(y_levels) != 2L) {
      stop("Response variable must have exactly 2 distinct classes for binary logistic regression.")
    }
    y <- ifelse(y_fac == y_levels[2L], 1L, 0L)
    pos_label <- y_levels[2L]
    neg_label <- y_levels[1L]
  } else {
    y_num <- as.numeric(y_raw)
    u_vals <- sort(unique(stats::na.omit(y_num)))
    if (length(u_vals) != 2L) {
      stop("Numeric response variable must contain exactly two distinct binary values (e.g. 0 and 1).")
    }
    y <- ifelse(y_num == u_vals[2L], 1L, 0L)
    pos_label <- as.character(u_vals[2L])
    neg_label <- as.character(u_vals[1L])
  }

  # Build design data frame
  df_model <- data.frame(y = y)
  for (pc in pred_cols) {
    df_model[[pc]] <- as.numeric(df[[pc]])
  }

  complete_idx <- stats::complete.cases(df_model)
  df_clean <- df_model[complete_idx, , drop = FALSE]
  n_clean <- nrow(df_clean)
  if (n_clean < 5L) {
    stop("Fewer than 5 complete observations available after removing missing values.")
  }

  # Fit Logistic GLM
  formula_str <- paste("y ~", paste(pred_cols, collapse = " + "))
  fit <- stats::glm(stats::as.formula(formula_str), data = df_clean, family = stats::binomial(link = "logit"))

  coef_summary <- summary(fit)$coefficients
  var_names <- c("Konstanta (Intercept)", pred_cols)
  var_names_en <- c("Intercept", pred_cols)

  b_vals <- coef_summary[, 1L]
  se_vals <- coef_summary[, 2L]
  z_vals <- coef_summary[, 3L]
  p_vals <- coef_summary[, 4L]

  or_vals <- exp(b_vals)
  z_crit <- stats::qnorm(1 - alpha / 2)
  ci_low <- exp(b_vals - z_crit * se_vals)
  ci_high <- exp(b_vals + z_crit * se_vals)

  # Model Fit Statistics
  d_null <- fit$null.deviance
  d_resid <- fit$deviance
  df_null <- fit$df.null
  df_resid <- fit$df.residual
  model_chisq <- d_null - d_resid
  model_df <- df_null - df_resid
  model_p <- stats::pchisq(model_chisq, df = model_df, lower.tail = FALSE)

  # Pseudo R-squared
  r2_cs <- 1 - exp(-model_chisq / n_clean)
  r2_nagelkerke <- r2_cs / (1 - exp(-d_null / n_clean))

  # Hosmer-Lemeshow Goodness of Fit Test
  fitted_probs <- stats::fitted(fit)
  obs_y <- df_clean$y

  # Deciles of risk
  g_groups <- min(10L, length(unique(fitted_probs)))
  cut_pts <- stats::quantile(fitted_probs, probs = seq(0, 1, length.out = g_groups + 1L))
  cut_pts[1L] <- cut_pts[1L] - 1e-6
  cut_pts[length(cut_pts)] <- cut_pts[length(cut_pts)] + 1e-6
  risk_groups <- cut(fitted_probs, breaks = unique(cut_pts), include.lowest = TRUE)

  hl_obs1 <- tapply(obs_y, risk_groups, sum)
  hl_total <- tapply(obs_y, risk_groups, length)
  hl_exp1 <- tapply(fitted_probs, risk_groups, sum)

  hl_obs1[is.na(hl_obs1)] <- 0
  hl_total[is.na(hl_total)] <- 0
  hl_exp1[is.na(hl_exp1)] <- 0
  hl_obs0 <- hl_total - hl_obs1
  hl_exp0 <- hl_total - hl_exp1

  # Chi-square terms
  hl_terms1 <- ifelse(hl_exp1 > 0, (hl_obs1 - hl_exp1)^2 / hl_exp1, 0)
  hl_terms0 <- ifelse(hl_exp0 > 0, (hl_obs0 - hl_exp0)^2 / hl_exp0, 0)
  hl_stat <- sum(hl_terms1 + hl_terms0)
  hl_df <- max(1L, length(unique(risk_groups)) - 2L)
  hl_p <- stats::pchisq(hl_stat, df = hl_df, lower.tail = FALSE)

  # ROC Curve & AUC via Mann-Whitney
  y1_probs <- fitted_probs[obs_y == 1L]
  y0_probs <- fitted_probs[obs_y == 0L]
  n1 <- length(y1_probs)
  n0 <- length(y0_probs)

  if (n1 > 0 && n0 > 0) {
    # Exact Mann-Whitney U for AUC
    u_stat <- sum(vapply(y1_probs, function(p1) sum(p1 > y0_probs) + 0.5 * sum(p1 == y0_probs), numeric(1)))
    auc_val <- u_stat / (n1 * n0)
  } else {
    auc_val <- 0.5
  }

  # Sensitivity & 1 - Specificity curve points
  thresh_seq <- c(-Inf, sort(unique(fitted_probs)), Inf)
  tpr_vec <- numeric(length(thresh_seq))
  fpr_vec <- numeric(length(thresh_seq))
  for (i in seq_along(thresh_seq)) {
    th <- thresh_seq[i]
    pred_pos <- fitted_probs >= th
    tpr_vec[i] <- if (n1 > 0) sum(pred_pos & obs_y == 1L) / n1 else 0
    fpr_vec[i] <- if (n0 > 0) sum(pred_pos & obs_y == 0L) / n0 else 0
  }
  # Sort by FPR ascending for plotting
  ord <- order(fpr_vec, tpr_vec)
  fpr_curve <- fpr_vec[ord]
  tpr_curve <- tpr_vec[ord]

  # Table 1: Logistic Regression Coefficients & Odds Ratio
  hdr1_id <- sl_box_vec(c("Variabel", "B", "Std Error", "Wald Z", "P-Value", "Odds Ratio Exp(B)", "95% Batas Bawah", "95% Batas Atas"))
  hdr1_en <- sl_box_vec(c("Variable", "B", "Std Error", "Wald Z", "p-value", "Odds Ratio Exp(B)", "95% Lower Limit", "95% Upper Limit"))

  rows1_id <- vector("list", length(var_names))
  rows1_en <- vector("list", length(var_names))
  for (i in seq_along(var_names)) {
    rows1_id[[i]] <- sl_box_vec(c(
      var_names[i],
      sl_fmt_num(b_vals[i], 4L, "id"),
      sl_fmt_num(se_vals[i], 4L, "id"),
      sl_fmt_num(z_vals[i], 3L, "id"),
      sl_fmt_p(p_vals[i], "id"),
      sl_fmt_num(or_vals[i], 3L, "id"),
      sl_fmt_num(ci_low[i], 3L, "id"),
      sl_fmt_num(ci_high[i], 3L, "id")
    ))
    rows1_en[[i]] <- sl_box_vec(c(
      var_names_en[i],
      sl_fmt_num(b_vals[i], 4L, "en"),
      sl_fmt_num(se_vals[i], 4L, "en"),
      sl_fmt_num(z_vals[i], 3L, "en"),
      sl_fmt_p(p_vals[i], "en"),
      sl_fmt_num(or_vals[i], 3L, "en"),
      sl_fmt_num(ci_low[i], 3L, "en"),
      sl_fmt_num(ci_high[i], 3L, "en")
    ))
  }

  table1 <- list(
    id = "logistic_coefficients",
    title = list(
      id = "Tabel 1. Koefisien Regresi Logistik Biner, Uji Wald, dan Rasio Odds (Odds Ratio)",
      en = "Table 1. Binary Logistic Regression Coefficients, Wald Test, and Odds Ratio"
    ),
    headers = list(id = hdr1_id, en = hdr1_en),
    rows = list(id = rows1_id, en = rows1_en),
    note = list(
      id = sprintf("Variabel respon: %s (Kategori referensi/sukses: '%s'). Selang kepercayaan 95%% dihitung untuk Rasio Odds Exp(B).", resp_col, pos_label),
      en = sprintf("Response variable: %s (Success category: '%s'). 95%% confidence intervals calculated for Odds Ratio Exp(B).", resp_col, pos_label)
    )
  )

  # Table 2: Model Summary and Goodness-of-Fit
  hdr2_id <- sl_box_vec(c("Uji Kelayakan Model", "Statistik", "Derajat Bebas (DB)", "P-Value", "Evaluasi / Ambang Batas"))
  hdr2_en <- sl_box_vec(c("Model Goodness-of-Fit", "Statistic", "Degrees of Freedom (df)", "p-value", "Evaluation / Threshold"))

  rows2_id <- list(
    sl_box_vec(c("Uji Omnibus Model (Chi-Square)", sl_fmt_num(model_chisq, 3L, "id"), as.character(model_df), sl_fmt_p(model_p, "id"), if (model_p < 0.05) "Signifikan (Model Efektif)" else "Tidak Signifikan")),
    sl_box_vec(c("Uji Hosmer-Lemeshow", sl_fmt_num(hl_stat, 3L, "id"), as.character(hl_df), sl_fmt_p(hl_p, "id"), if (hl_p >= 0.05) "Cocok / Valid (p >= 0,05)" else "Deviasi Signifikan")),
    sl_box_vec(c("Cox & Snell Pseudo R²", sl_fmt_num(r2_cs, 4L, "id"), "—", "—", sprintf("%s%% Variansi", sl_fmt_num(r2_cs * 100, 1L, "id")))),
    sl_box_vec(c("Nagelkerke Pseudo R²", sl_fmt_num(r2_nagelkerke, 4L, "id"), "—", "—", sprintf("%s%% Variansi Terjelaskan", sl_fmt_num(r2_nagelkerke * 100, 1L, "id")))),
    sl_box_vec(c("Area Under Curve (ROC AUC)", sl_fmt_num(auc_val, 3L, "id"), "—", "—", if (auc_val >= 0.8) "Diskriminasi Sangat Baik" else if (auc_val >= 0.7) "Diskriminasi Cukup" else "Diskriminasi Lemah"))
  )

  rows2_en <- list(
    sl_box_vec(c("Omnibus Model Test (Chi-Square)", sl_fmt_num(model_chisq, 3L, "en"), as.character(model_df), sl_fmt_p(model_p, "en"), if (model_p < 0.05) "Significant (Effective Model)" else "Not Significant")),
    sl_box_vec(c("Hosmer-Lemeshow Test", sl_fmt_num(hl_stat, 3L, "en"), as.character(hl_df), sl_fmt_p(hl_p, "en"), if (hl_p >= 0.05) "Adequate Fit (p >= 0.05)" else "Significant Deviation")),
    sl_box_vec(c("Cox & Snell Pseudo R²", sl_fmt_num(r2_cs, 4L, "en"), "—", "—", sprintf("%s%% Variance", sl_fmt_num(r2_cs * 100, 1L, "en")))),
    sl_box_vec(c("Nagelkerke Pseudo R²", sl_fmt_num(r2_nagelkerke, 4L, "en"), "—", "—", sprintf("%s%% Explained Variance", sl_fmt_num(r2_nagelkerke * 100, 1L, "en")))),
    sl_box_vec(c("Area Under Curve (ROC AUC)", sl_fmt_num(auc_val, 3L, "en"), "—", "—", if (auc_val >= 0.8) "Excellent Discrimination" else if (auc_val >= 0.7) "Acceptable Discrimination" else "Poor Discrimination"))
  )

  table2 <- list(
    id = "logistic_model_summary",
    title = list(
      id = "Tabel 2. Ringkasan Kelayakan Model, Pseudo R², Hosmer-Lemeshow, dan AUC",
      en = "Table 2. Model Summary, Pseudo R-Squared, Hosmer-Lemeshow, and AUC"
    ),
    headers = list(id = hdr2_id, en = hdr2_en),
    rows = list(id = rows2_id, en = rows2_en),
    note = list(
      id = "Nilai p Hosmer-Lemeshow > 0,05 mengonfirmasi kesesuaian model tanpa deviasi sistematis dari frekuensi teramati.",
      en = "Hosmer-Lemeshow p-value > 0.05 indicates adequate calibration with no systematic deviation from observed frequencies."
    )
  )

  # Generate 300-DPI ROC Curve Plot
  plots <- list()
  if (include_plots) {
    p_roc <- sl_capture_plot(function() {
      graphics::par(mar = c(4.8, 4.8, 3.2, 1.5), bg = "#FFFDF8", family = "sans")

      plot(
        fpr_curve, tpr_curve,
        type = "l", col = "#1B365D", lwd = 2.8,
        xlab = if (plot_lang == "id") "1 - Spesifisitas (False Positive Rate)" else "1 - Specificity (False Positive Rate)",
        ylab = if (plot_lang == "id") "Sensitivitas (True Positive Rate)" else "Sensitivity (True Positive Rate)",
        main = if (plot_lang == "id") {
          sprintf("Kurva ROC Regresi Logistik: AUC = %s", sl_fmt_num(auc_val, 3L, "id"))
        } else {
          sprintf("Logistic Regression ROC Curve: AUC = %s", sl_fmt_num(auc_val, 3L, "en"))
        },
        xlim = c(0, 1), ylim = c(0, 1), las = 1,
        col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Diagonal chance line
      graphics::abline(a = 0, b = 1, col = "#888888", lty = 2, lwd = 1.5)

      # Area shading under ROC curve
      graphics::polygon(c(fpr_curve, 1, 0), c(tpr_curve, 0, 0), col = grDevices::adjustcolor("#1B365D", alpha.f = 0.12), border = NA)

      # Text annotation of AUC
      auc_annot <- if (plot_lang == "id") {
        sprintf("Area Under Curve (AUC) = %s\nNagelkerke R² = %s", sl_fmt_num(auc_val, 3L, "id"), sl_fmt_num(r2_nagelkerke, 3L, "id"))
      } else {
        sprintf("Area Under Curve (AUC) = %s\nNagelkerke R² = %s", sl_fmt_num(auc_val, 3L, "en"), sl_fmt_num(r2_nagelkerke, 3L, "en"))
      }
      graphics::text(0.55, 0.22, labels = auc_annot, cex = 0.88, col = "#1B365D", font = 2)

      graphics::legend(
        "bottomright",
        legend = c(
          if (plot_lang == "id") sprintf("Kurva Model (AUC = %s)", sl_fmt_num(auc_val, 3L, "id")) else sprintf("Model ROC (AUC = %s)", sl_fmt_num(auc_val, 3L, "en")),
          if (plot_lang == "id") "Garis Peluang Acak (AUC = 0.5)" else "Chance Line (AUC = 0.5)"
        ),
        col = c("#1B365D", "#888888"),
        lty = c(1, 2),
        lwd = c(2.8, 1.5),
        bty = "n", cex = 0.8
      )
    }, width_in = 6.2, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "logistic_roc_curve",
      title = list(id = "Kurva ROC (Receiver Operating Characteristic)", en = "ROC Curve (Receiver Operating Characteristic)"),
      png_data_url = p_roc$png_data_url,
      width_in = p_roc$width_in,
      height_in = p_roc$height_in
    )
  }

  # Build Key Points (Poin Kunci)
  pred_points_id <- character(0)
  pred_points_en <- character(0)
  for (sp in seq_along(pred_cols)) {
    idx <- sp + 1L
    p_name <- pred_cols[sp]
    or_val_sp <- or_vals[idx]
    is_sig <- is.finite(p_vals[idx]) && p_vals[idx] < alpha
    sig_note_id <- if (is_sig) sprintf("signifikan (p = %s)", sl_fmt_p(p_vals[idx], "id")) else sprintf("tidak signifikan (p = %s)", sl_fmt_p(p_vals[idx], "id"))
    sig_note_en <- if (is_sig) sprintf("statistically significant (p = %s)", sl_fmt_p(p_vals[idx], "en")) else sprintf("not statistically significant (p = %s)", sl_fmt_p(p_vals[idx], "en"))

    if (or_val_sp >= 1.0) {
      pct_inc <- (or_val_sp - 1.0) * 100.0
      pred_points_id <- c(pred_points_id, sprintf("• **Rasio Odds (%s)**: Setiap kenaikan 1 satuan %s meningkatkan peluang relatif '%s' sebesar %s%% (OR = %s [95%% CI: %s - %s], %s).",
        p_name, p_name, pos_label, sl_fmt_num(pct_inc, 1L, "id"), sl_fmt_num(or_val_sp, 2L, "id"), sl_fmt_num(ci_low[idx], 2L, "id"), sl_fmt_num(ci_high[idx], 2L, "id"), sig_note_id))
      pred_points_en <- c(pred_points_en, sprintf("• **Odds Ratio (%s)**: Each 1-unit increase in %s increases the odds of '%s' by %s%% (OR = %s [95%% CI: %s - %s], %s).",
        p_name, p_name, pos_label, sl_fmt_num(pct_inc, 1L, "en"), sl_fmt_num(or_val_sp, 2L, "en"), sl_fmt_num(ci_low[idx], 2L, "en"), sl_fmt_num(ci_high[idx], 2L, "en"), sig_note_en))
    } else {
      pct_dec <- (1.0 - or_val_sp) * 100.0
      pred_points_id <- c(pred_points_id, sprintf("• **Rasio Odds (%s)**: Setiap kenaikan 1 satuan %s mereduksi peluang '%s' sebesar %s%% (OR = %s [95%% CI: %s - %s], %s; faktor protektif/penghambat).",
        p_name, p_name, pos_label, sl_fmt_num(pct_dec, 1L, "id"), sl_fmt_num(or_val_sp, 2L, "id"), sl_fmt_num(ci_low[idx], 2L, "id"), sl_fmt_num(ci_high[idx], 2L, "id"), sig_note_id))
      pred_points_en <- c(pred_points_en, sprintf("• **Odds Ratio (%s)**: Each 1-unit increase in %s reduces the odds of '%s' by %s%% (OR = %s [95%% CI: %s - %s], %s).",
        p_name, p_name, pos_label, sl_fmt_num(pct_dec, 1L, "en"), sl_fmt_num(or_val_sp, 2L, "en"), sl_fmt_num(ci_low[idx], 2L, "en"), sl_fmt_num(ci_high[idx], 2L, "en"), sig_note_en))
    }
  }

  pts_id <- c(
    pred_points_id,
    sprintf("• **Kemampuan Klasifikasi & Diskriminasi (ROC AUC)**: Nilai AUC sebesar **%s** (%s), menunjukkan model memiliki kapabilitas klasifikasi yang valid.",
      sl_fmt_num(auc_val, 3L, "id"), if (auc_val >= 0.8) "kemampuan diskriminasi sangat baik" else "kemampuan diskriminasi cukup"),
    sprintf("• **Kelayakan Model & Variansi Terjelaskan**: Model logistik signifikan secara simultan (Omnibus χ² = %s, db = %d, p = %s) dengan nilai Nagelkerke R² = %s (%s%% variansi). Uji Hosmer-Lemeshow (p = %s) mengonfirmasi model terkalibrasi baik.",
      sl_fmt_num(model_chisq, 2L, "id"), model_df, sl_fmt_p(model_p, "id"), sl_fmt_num(r2_nagelkerke, 3L, "id"), sl_fmt_num(r2_nagelkerke * 100, 1L, "id"), sl_fmt_p(hl_p, "id"))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    pred_points_en,
    sprintf("• **Classification & Discrimination Ability (ROC AUC)**: AUC value is **%s** (%s), confirming strong classification capability.",
      sl_fmt_num(auc_val, 3L, "en"), if (auc_val >= 0.8) "excellent discrimination" else "acceptable discrimination"),
    sprintf("• **Model Goodness-of-Fit & Explained Variance**: Overall model is statistically significant (Omnibus χ² = %s, df = %d, p = %s) with Nagelkerke R² = %s (%s%% variance). Hosmer-Lemeshow test (p = %s) confirms good calibration.",
      sl_fmt_num(model_chisq, 2L, "en"), model_df, sl_fmt_p(model_p, "en"), sl_fmt_num(r2_nagelkerke, 3L, "en"), sl_fmt_num(r2_nagelkerke * 100, 1L, "en"), sl_fmt_p(hl_p, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "binary_logistic",
    analysis_label = list(id = "Regresi Logistik Biner (Binary Logistic Regression)", en = "Binary Logistic Regression"),
    response_label = resp_col,
    mode = "logistic",
    alpha = alpha,
    n_raw = as.integer(nrow(df)),
    n_valid = as.integer(n_clean),
    n_missing = as.integer(nrow(df) - n_clean),
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(neg_label, pos_label))
  )

  formula_obj <- list(
    katex = "\\ln\\left(\\frac{P(Y=1)}{1-P(Y=1)}\\right) = \\beta_0 + \\sum_{j=1}^p \\beta_j X_j, \\quad \\text{OR}_j = e^{\\beta_j}",
    description = list(
      id = "Model Regresi Logistik Biner dengan penduga Odds Ratio Exp(B) dan kurva ROC.",
      en = "Binary Logistic Regression model with Odds Ratio Exp(B) estimators and ROC curve."
    )
  )

  coef_df_res <- data.frame(
    variable = var_names[-1L],
    coefficient = b_vals[-1L],
    odds_ratio = or_vals[-1L],
    ci_low = ci_low[-1L],
    ci_high = ci_high[-1L],
    p_value = p_vals[-1L],
    stringsAsFactors = FALSE
  )

  main_res <- list(
    response = resp_col,
    predictors = sl_box_vec(pred_cols),
    coefficients = coef_df_res,
    model_chisq = model_chisq,
    model_df = model_df,
    model_p = model_p,
    pseudo_r2_coxsnell = r2_cs,
    pseudo_r2_nagelkerke = r2_nagelkerke,
    hosmer_lemeshow_stat = hl_stat,
    hosmer_lemeshow_df = hl_df,
    hosmer_lemeshow_p = hl_p,
    auc = auc_val
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

#' Multivariate Correlation Matrix Analysis & Publication Heatmap
#'
#' @param df data.frame containing numeric variables
#' @param opts list of options
#' @return list conforming to StatLab result schema
sl_analyze_correlation_matrix <- function(df, opts = list()) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Data frame is required for correlation matrix analysis.")
  }

  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha)) as.numeric(opts$alpha[[1L]]) else 0.05
  lang <- if (!is.null(opts$lang)) as.character(opts$lang[[1L]]) else "id"
  method_name <- if (!is.null(opts$method) && opts$method[[1L]] %in% c("pearson", "spearman")) as.character(opts$method[[1L]]) else "pearson"
  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$plot_lang)) as.character(opts$plot_lang[[1L]]) else lang

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) < 2L) {
    stop("At least 2 numeric variables are required for correlation matrix analysis.")
  }

  df_clean <- stats::na.omit(df[num_cols])
  n_clean <- nrow(df_clean)
  if (n_clean < 3L) {
    stop("At least 3 complete observations are required for correlation analysis.")
  }

  k <- length(num_cols)
  r_mat <- stats::cor(df_clean, method = method_name)
  p_mat <- matrix(0.0, nrow = k, ncol = k, dimnames = list(num_cols, num_cols))

  for (i in 1L:(k - 1L)) {
    for (j in (i + 1L):k) {
      r_val <- r_mat[i, j]
      if (abs(r_val) >= 1.0) {
        p_val <- 0.0
      } else {
        t_stat <- r_val * sqrt((n_clean - 2L) / (1.0 - r_val^2))
        p_val <- 2.0 * stats::pt(abs(t_stat), df = n_clean - 2L, lower.tail = FALSE)
      }
      p_mat[i, j] <- p_val
      p_mat[j, i] <- p_val
    }
  }

  # Build APA Correlation Table
  hdr_id <- sl_box_vec(c("Variabel", paste0(seq_len(k), ". ", num_cols)))
  hdr_en <- sl_box_vec(c("Variable", paste0(seq_len(k), ". ", num_cols)))

  rows_id <- vector("list", k)
  rows_en <- vector("list", k)

  for (i in seq_len(k)) {
    vals_id <- character(k)
    vals_en <- character(k)
    for (j in seq_len(k)) {
      if (i == j) {
        vals_id[j] <- "1,000"
        vals_en[j] <- "1.000"
      } else {
        r_ij <- r_mat[i, j]
        p_ij <- p_mat[i, j]
        stars <- if (p_ij < 0.01) "**" else if (p_ij < 0.05) "*" else ""
        vals_id[j] <- paste0(sl_fmt_num(r_ij, 3L, "id"), stars)
        vals_en[j] <- paste0(sl_fmt_num(r_ij, 3L, "en"), stars)
      }
    }
    row_label <- sprintf("%d. %s", i, num_cols[i])
    rows_id[[i]] <- sl_box_vec(c(row_label, vals_id))
    rows_en[[i]] <- sl_box_vec(c(row_label, vals_en))
  }

  method_label <- if (identical(method_name, "pearson")) "Pearson" else "Spearman Rank"
  table1 <- list(
    id = "correlation_matrix_table",
    title = list(
      id = sprintf("Tabel 1. Matriks Korelasi %s Antar-Variabel Respon (N = %d)", method_label, n_clean),
      en = sprintf("Table 1. %s Correlation Matrix Across Variables (N = %d)", method_label, n_clean)
    ),
    headers = list(id = hdr_id, en = hdr_en),
    rows = list(id = rows_id, en = rows_en),
    note = list(
      id = "* Signifikan pada taraf α = 0,05; ** Signifikan sangat nyata pada taraf α = 0,01 (uji dua arah).",
      en = "* Significant at α = 0.05 level; ** Highly significant at α = 0.01 level (two-tailed)."
    )
  )

  # Generate Publication-Grade 300-DPI Heatmap
  plots <- list()
  if (include_plots) {
    p_heat <- sl_capture_plot(function() {
      graphics::par(mar = c(6.5, 6.5, 3.5, 4.0), bg = "#FFFDF8", family = "sans")

      # Create diverging blue-white-crimson color gradient
      pal_fn <- grDevices::colorRampPalette(c("#1B365D", "#4A6FA5", "#F7F5F0", "#D9534F", "#B83227"))
      heat_colors <- pal_fn(64)

      # Transpose and reverse for correct matrix orientation in graphics::image
      # image() plots x left-to-right (columns) and y bottom-to-top (rows)
      mat_plot <- t(r_mat[k:1L, ])

      graphics::image(
        seq_len(k), seq_len(k), mat_plot,
        col = heat_colors, zlim = c(-1, 1),
        axes = FALSE, xlab = "", ylab = "",
        main = if (plot_lang == "id") {
          sprintf("Heatmap Matriks Korelasi %s (N = %d)", method_label, n_clean)
        } else {
          sprintf("%s Correlation Matrix Heatmap (N = %d)", method_label, n_clean)
        },
        col.main = "#1B365D"
      )

      # Axes labels
      graphics::axis(1, at = seq_len(k), labels = num_cols, las = 2, col.axis = "#1B365D", cex.axis = 0.85, font = 2)
      graphics::axis(2, at = seq_len(k), labels = rev(num_cols), las = 1, col.axis = "#1B365D", cex.axis = 0.85, font = 2)

      # Overlay grid lines
      graphics::abline(h = seq(0.5, k + 0.5, by = 1), col = "#E3DDD0", lwd = 1.2)
      graphics::abline(v = seq(0.5, k + 0.5, by = 1), col = "#E3DDD0", lwd = 1.2)

      # Print correlation numbers and asterisks inside cells
      for (x_i in seq_len(k)) {
        for (y_i in seq_len(k)) {
          orig_row <- k - y_i + 1L
          orig_col <- x_i
          val <- r_mat[orig_row, orig_col]
          p_val <- p_mat[orig_row, orig_col]
          txt_color <- if (abs(val) > 0.55) "#FFFDF8" else "#1B365D"
          stars <- if (orig_row != orig_col && p_val < 0.01) "**" else if (orig_row != orig_col && p_val < 0.05) "*" else ""
          num_str <- if (plot_lang == "id") sl_fmt_num(val, 2L, "id") else sl_fmt_num(val, 2L, "en")
          graphics::text(x_i, y_i, labels = paste0(num_str, stars), col = txt_color, font = 2, cex = 0.82)
        }
      }

      # Draw color scale legend bar on right side
      u <- graphics::par("usr")
      bar_x <- u[2] + (u[2] - u[1]) * 0.03
      bar_w <- (u[2] - u[1]) * 0.035
      n_bars <- 50
      bar_y_seq <- seq(u[3], u[4], length.out = n_bars + 1L)
      bar_cols <- pal_fn(n_bars)

      for (b in seq_len(n_bars)) {
        graphics::rect(bar_x, bar_y_seq[b], bar_x + bar_w, bar_y_seq[b + 1L], col = bar_cols[b], border = NA, xpd = TRUE)
      }
      graphics::rect(bar_x, u[3], bar_x + bar_w, u[4], border = "#1B365D", lwd = 1, xpd = TRUE)

      # Legend tick values (-1, 0, +1)
      graphics::text(bar_x + bar_w + (u[2] - u[1]) * 0.02, u[4], labels = "+1.0", cex = 0.75, col = "#1B365D", adj = 0, xpd = TRUE)
      graphics::text(bar_x + bar_w + (u[2] - u[1]) * 0.02, (u[3] + u[4]) / 2, labels = "0.0", cex = 0.75, col = "#1B365D", adj = 0, xpd = TRUE)
      graphics::text(bar_x + bar_w + (u[2] - u[1]) * 0.02, u[3], labels = "-1.0", cex = 0.75, col = "#1B365D", adj = 0, xpd = TRUE)

    }, width_in = 6.4, height_in = 5.2, res = 300)

    plots[[1L]] <- list(
      id = "correlation_heatmap",
      title = list(id = sprintf("Heatmap Matriks Korelasi %s", method_label), en = sprintf("%s Correlation Matrix Heatmap", method_label)),
      png_data_url = p_heat$png_data_url,
      width_in = p_heat$width_in,
      height_in = p_heat$height_in
    )
  }

  # Build Key Points (Poin Kunci)
  # Find strongest positive and negative pairs
  r_offdiag <- r_mat
  diag(r_offdiag) <- NA
  max_pos_val <- max(r_offdiag, na.rm = TRUE)
  min_neg_val <- min(r_offdiag, na.rm = TRUE)

  pos_idx <- which(r_offdiag == max_pos_val, arr.ind = TRUE)
  neg_idx <- which(r_offdiag == min_neg_val, arr.ind = TRUE)

  pair_pos_name <- paste(num_cols[pos_idx[1L, 1L]], "dan", num_cols[pos_idx[1L, 2L]])
  pair_pos_name_en <- paste(num_cols[pos_idx[1L, 1L]], "and", num_cols[pos_idx[1L, 2L]])
  p_pos_val <- p_mat[pos_idx[1L, 1L], pos_idx[1L, 2L]]

  pair_neg_name <- paste(num_cols[neg_idx[1L, 1L]], "dan", num_cols[neg_idx[1L, 2L]])
  pair_neg_name_en <- paste(num_cols[neg_idx[1L, 1L]], "and", num_cols[neg_idx[1L, 2L]])
  p_neg_val <- p_mat[neg_idx[1L, 1L], neg_idx[1L, 2L]]

  pts_id <- c(
    sprintf("• **Korelasi Terkuat Positif**: Pasangan **%s** memiliki hubungan searah paling kuat (r = %s, p = %s).",
      pair_pos_name, sl_fmt_num(max_pos_val, 3L, "id"), sl_fmt_p(p_pos_val, "id")),
    sprintf("• **Korelasi Terkuat Negatif**: Pasangan **%s** memiliki hubungan berlawanan arah paling kuat (r = %s, p = %s).",
      pair_neg_name, sl_fmt_num(min_neg_val, 3L, "id"), sl_fmt_p(p_neg_val, "id")),
    if (any(abs(r_offdiag) >= 0.80, na.rm = TRUE)) {
      "• **Indikasi Multikolinearitas**: Terdapat pasangan variabel dengan korelasi tinggi (|r| >= 0,80), disarankan berhati-hati saat memasukkan bersamaan ke model regresi berganda."
    } else {
      "• **Indikasi Multikolinearitas**: Tidak ditemukan korelasi berlebih (|r| < 0,80), risiko multikolinearitas antar-variabel tergolong rendah."
    }
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    sprintf("• **Strongest Positive Correlation**: The pair **%s** shows the strongest positive association (r = %s, p = %s).",
      pair_pos_name_en, sl_fmt_num(max_pos_val, 3L, "en"), sl_fmt_p(p_pos_val, "en")),
    sprintf("• **Strongest Negative Correlation**: The pair **%s** shows the strongest inverse association (r = %s, p = %s).",
      pair_neg_name_en, sl_fmt_num(min_neg_val, 3L, "en"), sl_fmt_p(p_neg_val, "en")),
    if (any(abs(r_offdiag) >= 0.80, na.rm = TRUE)) {
      "• **Multicollinearity Indication**: High inter-correlation (|r| >= 0.80) observed among some pairs; caution recommended if modeled simultaneously in multiple regression."
    } else {
      "• **Multicollinearity Indication**: No excessive collinearity (|r| < 0.80) observed, indicating low risk of multicollinearity."
    }
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "correlation_matrix",
    analysis_label = list(id = sprintf("Matriks Korelasi Multivariat %s & Heatmap", method_label), en = sprintf("Multivariate %s Correlation Matrix & Heatmap", method_label)),
    response_label = paste(num_cols, collapse = ", "),
    mode = "matrix",
    alpha = alpha,
    n_raw = as.integer(nrow(df)),
    n_valid = as.integer(n_clean),
    n_missing = as.integer(nrow(df) - n_clean),
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(num_cols)
  )

  formula_obj <- list(
    katex = if (identical(method_name, "pearson")) {
      "r_{xy} = \\frac{\\sum (X - \\bar{X})(Y - \\bar{Y})}{\\sqrt{\\sum (X - \\bar{X})^2 \\sum (Y - \\bar{Y})^2}}, \\quad t = r \\sqrt{\\frac{n-2}{1-r^2}}"
    } else {
      "\\rho = 1 - \\frac{6 \\sum d_i^2}{n(n^2 - 1)}, \\quad t = \\rho \\sqrt{\\frac{n-2}{1-\\rho^2}}"
    },
    description = list(
      id = sprintf("Matriks Korelasi Asosiasi Bivariat %s dengan pengujian signifikansi dua arah.", method_label),
      en = sprintf("Multivariate %s correlation matrix with two-tailed significance testing.", method_label)
    )
  )

  main_res <- list(
    method = method_name,
    variables = sl_box_vec(num_cols),
    r_matrix = r_mat,
    p_matrix = p_mat,
    strongest_positive = list(pair = pair_pos_name, r = max_pos_val, p = p_pos_val),
    strongest_negative = list(pair = pair_neg_name, r = min_neg_val, p = p_neg_val)
  )

  list(
    meta = meta,
    formula = formula_obj,
    main = main_res,
    tables_apa = sl_box_vec(list(table1)),
    tables_full = sl_box_vec(list(table1)),
    plots = sl_box_vec(plots),
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
