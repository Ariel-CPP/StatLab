# R/11_experimental_design.R
# StatLab Experimental Design Engine (pure Base R)
# Randomized Complete Block Design (RAKL / RCBD) & Split-Plot Design
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

#' Randomized Complete Block Design (RAKL / RCBD) Analysis
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_rcbd <- function(df, opts = list()) {
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
  block_col <- if (!is.null(roles$block_col) && nzchar(as.character(roles$block_col[[1L]]))) as.character(roles$block_col[[1L]]) else NULL

  # Reshape wide to long if in wide mode
  if (mode == "wide") {
    cols <- names(df)
    # Check if first column is non-numeric (block names)
    first_col_num <- suppressWarnings(all(is.finite(as.numeric(as.character(df[[1L]])))))
    if (!first_col_num && length(cols) >= 3L) {
      block_names <- as.character(df[[1L]])
      trt_cols <- if (length(response_cols) >= 2L) intersect(response_cols, cols[-1L]) else cols[-1L]
    } else {
      block_names <- sprintf("Blok_%d", seq_len(nrow(df)))
      trt_cols <- if (length(response_cols) >= 2L) intersect(response_cols, cols) else cols
    }

    long_vals <- double(0L)
    long_trts <- character(0L)
    long_blks <- character(0L)

    for (t_idx in seq_along(trt_cols)) {
      t_nm <- trt_cols[t_idx]
      v_col <- suppressWarnings(as.numeric(as.character(df[[t_nm]])))
      for (r_idx in seq_along(v_col)) {
        if (is.finite(v_col[r_idx])) {
          long_vals <- c(long_vals, v_col[r_idx])
          long_trts <- c(long_trts, t_nm)
          long_blks <- c(long_blks, block_names[r_idx])
        }
      }
    }

    df_clean <- data.frame(
      Respon = long_vals,
      Perlakuan = factor(long_trts, levels = unique(long_trts)),
      Kelompok = factor(long_blks, levels = unique(long_blks))
    )
    resp_label <- "Respon Pengamatan"
  } else {
    # Long mode
    cols <- names(df)
    resp_col_nm <- if (length(response_cols) >= 1L && response_cols[1L] %in% cols) response_cols[1L] else cols[vapply(df, is.numeric, logical(1L))][1L]
    trt_col_nm <- if (!is.null(group_col) && group_col %in% cols) group_col else cols[cols != resp_col_nm][1L]
    blk_col_nm <- if (!is.null(block_col) && block_col %in% cols) block_col else cols[!cols %in% c(resp_col_nm, trt_col_nm)][1L]

    raw_y <- suppressWarnings(as.numeric(as.character(df[[resp_col_nm]])))
    raw_t <- as.character(df[[trt_col_nm]])
    raw_b <- as.character(df[[blk_col_nm]])

    valid_idx <- which(is.finite(raw_y) & nzchar(raw_t) & nzchar(raw_b))
    if (length(valid_idx) < 4L) {
      stop("RAKL / RCBD membutuhkan minimal 4 data observasi lengkap.")
    }

    df_clean <- data.frame(
      Respon = raw_y[valid_idx],
      Perlakuan = factor(raw_t[valid_idx], levels = unique(raw_t[valid_idx])),
      Kelompok = factor(raw_b[valid_idx], levels = unique(raw_b[valid_idx]))
    )
    resp_label <- resp_col_nm
  }

  trts <- levels(df_clean$Perlakuan)
  blks <- levels(df_clean$Kelompok)
  t_len <- length(trts)
  r_len <- length(blks)
  n_total <- nrow(df_clean)

  if (t_len < 2L || r_len < 2L) {
    stop("RAKL / RCBD membutuhkan minimal 2 perlakuan dan 2 kelompok/ulangan.")
  }

  # Fit linear model for RCBD: Respon ~ Kelompok + Perlakuan
  fit_rcbd <- stats::aov(Respon ~ Kelompok + Perlakuan, data = df_clean)
  aov_tab <- summary(fit_rcbd)[[1L]]

  # ANOVA components
  db_b <- aov_tab["Kelompok", "Df"]
  jk_b <- aov_tab["Kelompok", "Sum Sq"]
  kt_b <- aov_tab["Kelompok", "Mean Sq"]
  f_b  <- aov_tab["Kelompok", "F value"]
  p_b  <- aov_tab["Kelompok", "Pr(>F)"]

  db_t <- aov_tab["Perlakuan", "Df"]
  jk_t <- aov_tab["Perlakuan", "Sum Sq"]
  kt_t <- aov_tab["Perlakuan", "Mean Sq"]
  f_t  <- aov_tab["Perlakuan", "F value"]
  p_t  <- aov_tab["Perlakuan", "Pr(>F)"]

  db_e <- aov_tab["Residuals", "Df"]
  jk_e <- aov_tab["Residuals", "Sum Sq"]
  kt_e <- aov_tab["Residuals", "Mean Sq"]

  db_tot <- db_b + db_t + db_e
  jk_tot <- jk_b + jk_t + jk_e

  # F-table critical values
  f_tab_05 <- stats::qf(1.0 - alpha, db_t, db_e)
  f_tab_01 <- stats::qf(0.99, db_t, db_e)

  # Koefisien Keragaman (KK / CV Galat)
  grand_mean <- mean(df_clean$Respon)
  cv_error <- (sqrt(kt_e) / grand_mean) * 100.0

  # Quality classification of KK
  cv_eval_id <- if (cv_error < 5.0) {
    "Sangat Tinggi (KK < 5%, keragaman galat sangat homogen)"
  } else if (cv_error <= 15.0) {
    "Baik / Sedang (5% ≤ KK ≤ 15%, tingkat ketelitian standar percobaan lapangan)"
  } else {
    "Heterogen (KK > 15%, variasi galat lapangan cukup tinggi)"
  }

  cv_eval_en <- if (cv_error < 5.0) {
    "Very High Precision (CV < 5%)"
  } else if (cv_error <= 15.0) {
    "Good / Moderate Precision (5% ≤ CV ≤ 15%)"
  } else {
    "High Variation (CV > 15%)"
  }

  # Treatment descriptive metrics
  trt_means <- tapply(df_clean$Respon, df_clean$Perlakuan, mean)
  trt_sds   <- tapply(df_clean$Respon, df_clean$Perlakuan, stats::sd)
  trt_ns    <- tapply(df_clean$Respon, df_clean$Perlakuan, length)
  trt_ses   <- sqrt(kt_e / r_len) # Standard error of treatment mean in RCBD is sqrt(KTG / r)

  # Uji BNT (LSD - Least Significant Difference)
  t_crit_lsd <- stats::qt(1.0 - alpha / 2.0, df = db_e)
  lsd_val <- t_crit_lsd * sqrt((2.0 * kt_e) / r_len)

  # Uji Tukey HSD
  q_crit_tukey <- stats::qtukey(1.0 - alpha, nmeans = t_len, df = db_e)
  tukey_val <- q_crit_tukey * sqrt(kt_e / r_len)

  # Pairwise LSD p-values matrix for CLD letters
  pval_mat_lsd <- matrix(1.0, nrow = t_len, ncol = t_len)
  rownames(pval_mat_lsd) <- trts
  colnames(pval_mat_lsd) <- trts
  se_diff <- sqrt((2.0 * kt_e) / r_len)

  for (i in 1L:(t_len - 1L)) {
    for (j in (i + 1L):t_len) {
      d_ij <- abs(trt_means[i] - trt_means[j])
      t_calc <- d_ij / se_diff
      p_ij <- 2.0 * (1.0 - stats::pt(t_calc, df = db_e))
      pval_mat_lsd[i, j] <- p_ij
      pval_mat_lsd[j, i] <- p_ij
    }
  }

  # Assign letters using CLD
  lsd_letters <- sl_cld_letters(trts, trt_means, alpha, pval_mat_lsd)

  # Table 1: RAKL / RCBD ANOVA Table
  hdr_aov_id <- sl_box_vec(c("Sumber Keragaman (SK)", "Derajat Bebas (DB)", "Jumlah Kuadrat (JK)", "Kuadrat Tengah (KT)", "F-Hitung", "F-Tabel 5%", "F-Tabel 1%", "P-Value", "Signifikansi"))
  hdr_aov_en <- sl_box_vec(c("Source of Variation", "Degrees of Freedom (df)", "Sum of Squares (SS)", "Mean Square (MS)", "F-Statistic", "F-Crit 5%", "F-Crit 1%", "p-value", "Significance"))

  sig_b <- if (is.finite(p_b) && p_b < 0.01) "**" else if (is.finite(p_b) && p_b < 0.05) "*" else "tn"
  sig_t <- if (is.finite(p_t) && p_t < 0.01) "**" else if (is.finite(p_t) && p_t < 0.05) "*" else "tn"

  rows_aov_id <- list(
    sl_box_vec(c("Kelompok (Blok)", as.character(db_b), sl_fmt_num(jk_b, 4L, "id"), sl_fmt_num(kt_b, 4L, "id"), sl_fmt_num(f_b, 3L, "id"), sl_fmt_num(stats::qf(0.95, db_b, db_e), 3L, "id"), sl_fmt_num(stats::qf(0.99, db_b, db_e), 3L, "id"), sl_fmt_p(p_b, "id"), sig_b)),
    sl_box_vec(c("Perlakuan", as.character(db_t), sl_fmt_num(jk_t, 4L, "id"), sl_fmt_num(kt_t, 4L, "id"), sl_fmt_num(f_t, 3L, "id"), sl_fmt_num(f_tab_05, 3L, "id"), sl_fmt_num(f_tab_01, 3L, "id"), sl_fmt_p(p_t, "id"), sig_t)),
    sl_box_vec(c("Galat (Error)", as.character(db_e), sl_fmt_num(jk_e, 4L, "id"), sl_fmt_num(kt_e, 4L, "id"), "—", "—", "—", "—", "—")),
    sl_box_vec(c("Total", as.character(db_tot), sl_fmt_num(jk_tot, 4L, "id"), "—", "—", "—", "—", "—", "—"))
  )

  rows_aov_en <- list(
    sl_box_vec(c("Blocks", as.character(db_b), sl_fmt_num(jk_b, 4L, "en"), sl_fmt_num(kt_b, 4L, "en"), sl_fmt_num(f_b, 3L, "en"), sl_fmt_num(stats::qf(0.95, db_b, db_e), 3L, "en"), sl_fmt_num(stats::qf(0.99, db_b, db_e), 3L, "en"), sl_fmt_p(p_b, "en"), if (sig_b == "tn") "ns" else sig_b)),
    sl_box_vec(c("Treatments", as.character(db_t), sl_fmt_num(jk_t, 4L, "en"), sl_fmt_num(kt_t, 4L, "en"), sl_fmt_num(f_t, 3L, "en"), sl_fmt_num(f_tab_05, 3L, "en"), sl_fmt_num(f_tab_01, 3L, "en"), sl_fmt_p(p_t, "en"), if (sig_t == "tn") "ns" else sig_t)),
    sl_box_vec(c("Error", as.character(db_e), sl_fmt_num(jk_e, 4L, "en"), sl_fmt_num(kt_e, 4L, "en"), "—", "—", "—", "—", "—")),
    sl_box_vec(c("Total", as.character(db_tot), sl_fmt_num(jk_tot, 4L, "en"), "—", "—", "—", "—", "—", "—"))
  )

  tbl_aov <- list(
    id = "rcbd_anova_table",
    title = list(id = sprintf("Tabel 1. Daftar Sidik Ragam Rancangan Acak Kelompok Lengkap (RAKL) - %s", resp_label), en = sprintf("Table 1. ANOVA Table for Randomized Complete Block Design (RCBD) - %s", resp_label)),
    headers = list(id = hdr_aov_id, en = hdr_aov_en),
    rows = list(id = rows_aov_id, en = rows_aov_en),
    note = list(
      id = sprintf("Koefisien Keragaman (KK) = %s%% [%s]. Nilai Kritis BNT 5%% = %s; BNJ 5%% = %s.", sl_fmt_num(cv_error, 2L, "id"), cv_eval_id, sl_fmt_num(lsd_val, 3L, "id"), sl_fmt_num(tukey_val, 3L, "id")),
      en = sprintf("Coefficient of Variation (CV) = %s%% [%s]. Critical LSD 5%% = %s; Tukey HSD 5%% = %s.", sl_fmt_num(cv_error, 2L, "en"), cv_eval_en, sl_fmt_num(lsd_val, 3L, "en"), sl_fmt_num(tukey_val, 3L, "en"))
    )
  )

  # Table 2: Treatment Means & Letters Table
  hdr_means_id <- sl_box_vec(c("Perlakuan", "Ulangan (r)", "Rataan", "Simpangan Baku (SD)", "Galat Baku (SE)", "Notasi Uji BNT 5%"))
  hdr_means_en <- sl_box_vec(c("Treatment", "Replicates (r)", "Mean", "Standard Deviation (SD)", "Standard Error (SE)", "LSD 5% Notation"))

  ord_m <- order(trt_means, decreasing = TRUE)
  rows_means_id <- list()
  rows_means_en <- list()

  for (idx in seq_along(ord_m)) {
    orig_idx <- ord_m[idx]
    t_nm <- trts[orig_idx]
    let_str <- if (!is.null(lsd_letters[[t_nm]])) lsd_letters[[t_nm]] else "a"

    rows_means_id[[idx]] <- sl_box_vec(c(
      t_nm, as.character(trt_ns[orig_idx]), sl_fmt_num(trt_means[orig_idx], 2L, "id"),
      sl_fmt_num(trt_sds[orig_idx], 2L, "id"), sl_fmt_num(trt_ses, 3L, "id"), let_str
    ))
    rows_means_en[[idx]] <- sl_box_vec(c(
      t_nm, as.character(trt_ns[orig_idx]), sl_fmt_num(trt_means[orig_idx], 2L, "en"),
      sl_fmt_num(trt_sds[orig_idx], 2L, "en"), sl_fmt_num(trt_ses, 3L, "en"), let_str
    ))
  }

  tbl_means <- list(
    id = "rcbd_means_table",
    title = list(id = "Tabel 2. Rataan Perlakuan & Notasi Uji Lanjut Beda Nyata Terkecil (BNT)", en = "Table 2. Treatment Means & LSD Post-Hoc Letter Display"),
    headers = list(id = hdr_means_id, en = hdr_means_en),
    rows = list(id = rows_means_id, en = rows_means_en),
    note = list(
      id = "Huruf yang sama pada kolom notasi menunjukkan tidak berbeda nyata berdasarkan uji BNT pada taraf signifikansi 5%.",
      en = "Means followed by the same letter are not significantly different at alpha = 0.05 based on LSD test."
    )
  )

  # Graphics: Barplot with Letters & Block Profile Plot
  plots <- list()
  if (include_plots) {
    # Plot 1: Publication Barplot with SE error bars & Letters
    p_bar <- sl_capture_plot(function() {
      let_vec <- vapply(trts, function(t_nm) if (!is.null(lsd_letters[[t_nm]])) lsd_letters[[t_nm]] else "", character(1L))
      sl_plot_bar_letters(
        means = trt_means,
        ses = rep(trt_ses, t_len),
        labels = trts,
        letters = let_vec,
        ylab = resp_label,
        title = if (plot_lang == "id") sprintf("Diagram Batang Rataan RAKL dengan Error Bar (SE) & Notasi BNT (%s)", resp_label) else sprintf("RCBD Treatment Means with SE & LSD Letters (%s)", resp_label),
        lang = plot_lang
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    # Plot 2: Block Profile Plot (Interaksi Perlakuan x Kelompok)
    p_prof <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      col_pal <- c("#1B365D", "#B83227", "#2E7D32", "#D97706", "#7C3AED", "#0891B2")
      y_min <- min(df_clean$Respon) * 0.95
      y_max <- max(df_clean$Respon) * 1.05

      plot(
        c(1, r_len), c(y_min, y_max),
        type = "n", xaxt = "n",
        xlab = if (plot_lang == "id") "Kelompok / Blok Ulangan" else "Block / Replication",
        ylab = resp_label,
        main = if (plot_lang == "id") "Profil Respon Perlakuan Antar-Kelompok (Uji Efektivitas Blok)" else "Treatment Response Profile Across Blocks",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::axis(1, at = seq_len(r_len), labels = blks, col.axis = "#1B365D", font = 2)
      graphics::grid(col = "#E3DDD0", lty = 3)

      for (i in seq_len(t_len)) {
        t_nm <- trts[i]
        sub_d <- df_clean[df_clean$Perlakuan == t_nm, ]
        # order by block levels
        sub_d <- sub_d[order(match(sub_d$Kelompok, blks)), ]
        graphics::lines(seq_along(sub_d$Respon), sub_d$Respon, col = col_pal[(i - 1L) %% length(col_pal) + 1L], lwd = 2.0, type = "b", pch = 19, cex = 1.2)
      }

      graphics::legend(
        "topleft", legend = trts, col = col_pal[seq_len(t_len)],
        lwd = 2.0, pch = 19, bty = "n", cex = 0.82
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots <- list(
      list(
        id = "rcbd_barplot_letters",
        title = list(id = "Diagram Batang Rataan & Notasi Huruf Uji BNT", en = "Treatment Means Barplot with LSD Letters"),
        png_data_url = p_bar$png_data_url,
        width_in = p_bar$width_in,
        height_in = p_bar$height_in
      ),
      list(
        id = "rcbd_block_profile",
        title = list(id = "Plot Profil Respon Antar-Kelompok (Blok)", en = "Treatment Response Profile Across Blocks"),
        png_data_url = p_prof$png_data_url,
        width_in = p_prof$width_in,
        height_in = p_prof$height_in
      )
    )
  }

  best_trt <- names(which.max(trt_means))

  pts_id <- c(
    if (is.finite(p_t) && p_t < 0.05) {
      sprintf("• **Signifikansi Perlakuan**: Perlakuan berpengaruh sangat nyata secara statistik terhadap respon %s (F(%d, %d) = %s, p = %s). Perbedaan antar perlakuan melampaui nilai kritis F-Tabel 5%% (%s).",
        resp_label, db_t, db_e, sl_fmt_num(f_t, 3L, "id"), sl_fmt_p(p_t, "id"), sl_fmt_num(f_tab_05, 3L, "id"))
    } else {
      sprintf("• **Signifikansi Perlakuan**: Tidak terdapat pengaruh yang signifikan dari faktor perlakuan terhadap respon %s (F(%d, %d) = %s, p = %s).",
        resp_label, db_t, db_e, sl_fmt_num(f_t, 3L, "id"), sl_fmt_p(p_t, "id"))
    },
    if (is.finite(p_b) && p_b < 0.05) {
      sprintf("• **Efektivitas Pengelompokan (Blok)**: Pengelompokan efektif mereduksi keragaman galat (F(%d, %d) = %s, p = %s), mengonfirmasi bahwa penerapan desain RAKL lebih efisien dibandingkan RAL.",
        db_b, db_e, sl_fmt_num(f_b, 3L, "id"), sl_fmt_p(p_b, "id"))
    } else {
      sprintf("• **Efektivitas Pengelompokan (Blok)**: Keragaman antar-kelompok homogen (F(%d, %d) = %s, p = %s).",
        db_b, db_e, sl_fmt_num(f_b, 3L, "id"), sl_fmt_p(p_b, "id"))
    },
    sprintf("• **Koefisien Keragaman (KK)**: Nilai KK galat sebesar %s%%, tergolong dalam kategori %s.",
      sl_fmt_num(cv_error, 2L, "id"), cv_eval_id),
    sprintf("• **Perlakuan Terbaik & Uji Lanjut BNT**: Perlakuan dengan respon tertinggi dicapai oleh **%s** (rataan = %s, notasi '%s'). Nilai kritis pembeda BNT 5%% adalah %s.",
      best_trt, sl_fmt_num(trt_means[best_trt], 2L, "id"), lsd_letters[[best_trt]], sl_fmt_num(lsd_val, 3L, "id"))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_t) && p_t < 0.05) {
      sprintf("• **Treatment Significance**: Treatment factor exerted a statistically significant effect on %s (F(%d, %d) = %s, p = %s), exceeding critical F-Table 5%% (%s).",
        resp_label, db_t, db_e, sl_fmt_num(f_t, 3L, "en"), sl_fmt_p(p_t, "en"), sl_fmt_num(f_tab_05, 3L, "en"))
    } else {
      sprintf("• **Treatment Significance**: No statistically significant treatment effect on %s (F(%d, %d) = %s, p = %s).",
        resp_label, db_t, db_e, sl_fmt_num(f_t, 3L, "en"), sl_fmt_p(p_t, "en"))
    },
    if (is.finite(p_b) && p_b < 0.05) {
      sprintf("• **Blocking Efficiency**: Blocking successfully controlled background noise (F(%d, %d) = %s, p = %s), justifying RCBD over CRD.",
        db_b, db_e, sl_fmt_num(f_b, 3L, "en"), sl_fmt_p(p_b, "en"))
    } else {
      sprintf("• **Blocking Efficiency**: Variance among blocks was homogeneous (F(%d, %d) = %s, p = %s).",
        db_b, db_e, sl_fmt_num(f_b, 3L, "en"), sl_fmt_p(p_b, "en"))
    },
    sprintf("• **Coefficient of Variation (CV)**: Experimental error CV is %s%% (%s).",
      sl_fmt_num(cv_error, 2L, "en"), cv_eval_en),
    sprintf("• **Optimal Treatment & LSD**: Highest response achieved by **%s** (mean = %s, notation '%s'). Critical LSD 5%% threshold = %s.",
      best_trt, sl_fmt_num(trt_means[best_trt], 2L, "en"), lsd_letters[[best_trt]], sl_fmt_num(lsd_val, 3L, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "rcbd",
    analysis_label = list(id = "Rancangan Acak Kelompok Lengkap (RAKL / RCBD)", en = "Randomized Complete Block Design (RCBD)"),
    response_label = resp_label,
    mode = mode,
    alpha = alpha,
    n_raw = as.integer(n_total),
    n_valid = as.integer(n_total),
    n_missing = 0L,
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(trts),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(
      f_treatment = f_t,
      p_treatment = p_t,
      f_block = f_b,
      p_block = p_b,
      cv_error = cv_error,
      lsd_critical = lsd_val,
      tukey_critical = tukey_val
    ),
    posthoc = list(letters = as.list(lsd_letters)),
    tables_apa = list(tbl_aov, tbl_means),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Model Linier RAKL / RCBD", en = "RCBD Linear Model"),
        latex = "Y_{ij} = \\mu + \\tau_i + \\beta_j + \\epsilon_{ij}, \\quad \\epsilon_{ij} \\sim \\mathcal{N}(0, \\sigma^2)"
      ),
      list(
        title = list(id = "Koefisien Keragaman (KK) Galat", en = "Error Coefficient of Variation (CV)"),
        latex = "KK = \\frac{\\sqrt{KT_{\\text{Galat}}}}{\\bar{Y}_{\\bullet\\bullet}} \\times 100\\%"
      ),
      list(
        title = list(id = "Uji Beda Nyata Terkecil (BNT / LSD)", en = "Least Significant Difference (LSD)"),
        latex = "BNT_\\alpha = t_{\\alpha/2, db_g} \\sqrt{\\frac{2 KT_{\\text{Galat}}}{r}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Alias for sl_analyze_rcbd (Indonesian nomenclature)
sl_analyze_rakl <- sl_analyze_rcbd

#' Split-Plot Design (Rancangan Petak Terbagi) Analysis
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_split_plot <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  cols <- names(df)

  resp_nm <- if (!is.null(roles$response_cols) && length(roles$response_cols) >= 1L && roles$response_cols[1L] %in% cols) {
    roles$response_cols[1L]
  } else {
    cols[vapply(df, is.numeric, logical(1L))][1L]
  }

  main_nm <- if (!is.null(roles$main_plot_col) && roles$main_plot_col %in% cols) {
    roles$main_plot_col
  } else {
    cols[!cols %in% resp_nm][1L]
  }

  sub_nm <- if (!is.null(roles$sub_plot_col) && roles$sub_plot_col %in% cols) {
    roles$sub_plot_col
  } else {
    cols[!cols %in% c(resp_nm, main_nm)][1L]
  }

  blk_nm <- if (!is.null(roles$block_col) && roles$block_col %in% cols) {
    roles$block_col
  } else {
    cols[!cols %in% c(resp_nm, main_nm, sub_nm)][1L]
  }

  raw_y <- suppressWarnings(as.numeric(as.character(df[[resp_nm]])))
  raw_a <- as.character(df[[main_nm]])
  raw_b <- as.character(df[[sub_nm]])
  raw_r <- as.character(df[[blk_nm]])

  valid_idx <- which(is.finite(raw_y) & nzchar(raw_a) & nzchar(raw_b) & nzchar(raw_r))
  if (length(valid_idx) < 8L) {
    stop("Rancangan Petak Terbagi membutuhkan minimal 8 data observasi lengkap.")
  }

  df_sp <- data.frame(
    Y = raw_y[valid_idx],
    A = factor(raw_a[valid_idx], levels = unique(raw_a[valid_idx])),
    B = factor(raw_b[valid_idx], levels = unique(raw_b[valid_idx])),
    Blok = factor(raw_r[valid_idx], levels = unique(raw_r[valid_idx]))
  )

  a_levs <- levels(df_sp$A)
  b_levs <- levels(df_sp$B)
  r_levs <- levels(df_sp$Blok)
  a_cnt <- length(a_levs)
  b_cnt <- length(b_levs)
  r_cnt <- length(r_levs)
  n_total <- nrow(df_sp)

  # Explicit SS / MS calculation for Split-Plot
  y_all <- df_sp$Y
  grand_mean <- mean(y_all)
  fk <- sum(y_all)^2 / n_total

  ss_tot <- sum(y_all^2) - fk
  df_tot <- n_total - 1L

  # Block SS
  blk_sums <- tapply(y_all, df_sp$Blok, sum)
  ss_b <- sum(blk_sums^2) / (a_cnt * b_cnt) - fk
  df_b <- r_cnt - 1L

  # Main Plot A SS
  a_sums <- tapply(y_all, df_sp$A, sum)
  ss_a <- sum(a_sums^2) / (b_cnt * r_cnt) - fk
  df_a <- a_cnt - 1L

  # Subtotal Main Plot: A x Blok
  ablk_sums <- tapply(y_all, list(df_sp$A, df_sp$Blok), sum)
  ss_ablk <- sum(ablk_sums^2) / b_cnt - fk
  ss_ea <- max(0, ss_ablk - ss_b - ss_a)
  df_ea <- df_b * df_a

  # Sub Plot B SS
  b_sums <- tapply(y_all, df_sp$B, sum)
  ss_b_factor <- sum(b_sums^2) / (a_cnt * r_cnt) - fk
  df_b_factor <- b_cnt - 1L

  # Interaction A x B SS
  ab_sums <- tapply(y_all, list(df_sp$A, df_sp$B), sum)
  ss_ab_subtot <- sum(ab_sums^2) / r_cnt - fk
  ss_ab <- max(0, ss_ab_subtot - ss_a - ss_b_factor)
  df_ab <- df_a * df_b_factor

  # Sub Plot Error (Galat b) SS
  ss_eb <- max(0, ss_tot - ss_ablk - ss_b_factor - ss_ab)
  df_eb <- a_cnt * df_b * df_b_factor

  # Mean Squares
  ms_b  <- if (df_b > 0) ss_b / df_b else 0
  ms_a  <- if (df_a > 0) ss_a / df_a else 0
  ms_ea <- if (df_ea > 0) ss_ea / df_ea else 1e-12
  ms_b_factor <- if (df_b_factor > 0) ss_b_factor / df_b_factor else 0
  ms_ab <- if (df_ab > 0) ss_ab / df_ab else 0
  ms_eb <- if (df_eb > 0) ss_eb / df_eb else 1e-12

  # F-values & p-values
  # Main plot A is tested against Galat (a)
  f_a <- if (ms_ea > 0) ms_a / ms_ea else 0.0
  p_a <- if (df_a > 0 && df_ea > 0) stats::pf(f_a, df_a, df_ea, lower.tail = FALSE) else NA_real_

  # Sub plot B & Interaction A:B are tested against Galat (b)
  f_b_factor <- if (ms_eb > 0) ms_b_factor / ms_eb else 0.0
  p_b_factor <- if (df_b_factor > 0 && df_eb > 0) stats::pf(f_b_factor, df_b_factor, df_eb, lower.tail = FALSE) else NA_real_

  f_ab <- if (ms_eb > 0) ms_ab / ms_eb else 0.0
  p_ab <- if (df_ab > 0 && df_eb > 0) stats::pf(f_ab, df_ab, df_eb, lower.tail = FALSE) else NA_real_

  # KK(a) and KK(b)
  cv_a <- (sqrt(ms_ea) / grand_mean) * 100.0
  cv_b <- (sqrt(ms_eb) / grand_mean) * 100.0

  sig_a  <- if (is.finite(p_a) && p_a < 0.01) "**" else if (is.finite(p_a) && p_a < 0.05) "*" else "tn"
  sig_bf <- if (is.finite(p_b_factor) && p_b_factor < 0.01) "**" else if (is.finite(p_b_factor) && p_b_factor < 0.05) "*" else "tn"
  sig_ab <- if (is.finite(p_ab) && p_ab < 0.01) "**" else if (is.finite(p_ab) && p_ab < 0.05) "*" else "tn"

  # Table 1: Split-Plot ANOVA Table
  hdr_sp_id <- sl_box_vec(c("Sumber Keragaman (SK)", "DB", "Jumlah Kuadrat (JK)", "Kuadrat Tengah (KT)", "F-Hitung", "F-Tabel 5%", "P-Value", "Signifikansi"))
  hdr_sp_en <- sl_box_vec(c("Source of Variation", "df", "Sum of Squares (SS)", "Mean Square (MS)", "F-Statistic", "F-Crit 5%", "p-value", "Significance"))

  rows_sp_id <- list(
    sl_box_vec(c("Kelompok (Blok)", as.character(df_b), sl_fmt_num(ss_b, 4L, "id"), sl_fmt_num(ms_b, 4L, "id"), "—", "—", "—", "—")),
    sl_box_vec(c(sprintf("Petak Utama (%s)", main_nm), as.character(df_a), sl_fmt_num(ss_a, 4L, "id"), sl_fmt_num(ms_a, 4L, "id"), sl_fmt_num(f_a, 3L, "id"), sl_fmt_num(stats::qf(0.95, df_a, df_ea), 3L, "id"), sl_fmt_p(p_a, "id"), sig_a)),
    sl_box_vec(c("Galat (a)", as.character(df_ea), sl_fmt_num(ss_ea, 4L, "id"), sl_fmt_num(ms_ea, 4L, "id"), "—", "—", "—", "—")),
    sl_box_vec(c(sprintf("Anak Petak (%s)", sub_nm), as.character(df_b_factor), sl_fmt_num(ss_b_factor, 4L, "id"), sl_fmt_num(ms_b_factor, 4L, "id"), sl_fmt_num(f_b_factor, 3L, "id"), sl_fmt_num(stats::qf(0.95, df_b_factor, df_eb), 3L, "id"), sl_fmt_p(p_b_factor, "id"), sig_bf)),
    sl_box_vec(c(sprintf("Interaksi %s x %s", main_nm, sub_nm), as.character(df_ab), sl_fmt_num(ss_ab, 4L, "id"), sl_fmt_num(ms_ab, 4L, "id"), sl_fmt_num(f_ab, 3L, "id"), sl_fmt_num(stats::qf(0.95, df_ab, df_eb), 3L, "id"), sl_fmt_p(p_ab, "id"), sig_ab)),
    sl_box_vec(c("Galat (b)", as.character(df_eb), sl_fmt_num(ss_eb, 4L, "id"), sl_fmt_num(ms_eb, 4L, "id"), "—", "—", "—", "—")),
    sl_box_vec(c("Total", as.character(df_tot), sl_fmt_num(ss_tot, 4L, "id"), "—", "—", "—", "—", "—"))
  )

  rows_sp_en <- list(
    sl_box_vec(c("Blocks", as.character(df_b), sl_fmt_num(ss_b, 4L, "en"), sl_fmt_num(ms_b, 4L, "en"), "—", "—", "—", "—")),
    sl_box_vec(c(sprintf("Main Plot (%s)", main_nm), as.character(df_a), sl_fmt_num(ss_a, 4L, "en"), sl_fmt_num(ms_a, 4L, "en"), sl_fmt_num(f_a, 3L, "en"), sl_fmt_num(stats::qf(0.95, df_a, df_ea), 3L, "en"), sl_fmt_p(p_a, "en"), if (sig_a == "tn") "ns" else sig_a)),
    sl_box_vec(c("Error (a)", as.character(df_ea), sl_fmt_num(ss_ea, 4L, "en"), sl_fmt_num(ms_ea, 4L, "en"), "—", "—", "—", "—")),
    sl_box_vec(c(sprintf("Sub Plot (%s)", sub_nm), as.character(df_b_factor), sl_fmt_num(ss_b_factor, 4L, "en"), sl_fmt_num(ms_b_factor, 4L, "en"), sl_fmt_num(f_b_factor, 3L, "en"), sl_fmt_num(stats::qf(0.95, df_b_factor, df_eb), 3L, "en"), sl_fmt_p(p_b_factor, "en"), if (sig_bf == "tn") "ns" else sig_bf)),
    sl_box_vec(c(sprintf("Interaction %s x %s", main_nm, sub_nm), as.character(df_ab), sl_fmt_num(ss_ab, 4L, "en"), sl_fmt_num(ms_ab, 4L, "en"), sl_fmt_num(f_ab, 3L, "en"), sl_fmt_num(stats::qf(0.95, df_ab, df_eb), 3L, "en"), sl_fmt_p(p_ab, "en"), if (sig_ab == "tn") "ns" else sig_ab)),
    sl_box_vec(c("Error (b)", as.character(df_eb), sl_fmt_num(ss_eb, 4L, "en"), sl_fmt_num(ms_eb, 4L, "en"), "—", "—", "—", "—")),
    sl_box_vec(c("Total", as.character(df_tot), sl_fmt_num(ss_tot, 4L, "en"), "—", "—", "—", "—", "—"))
  )

  tbl_sp <- list(
    id = "split_plot_anova_table",
    title = list(id = sprintf("Tabel 1. Daftar Sidik Ragam Petak Terbagi (Split-Plot) - Respon %s", resp_nm), en = sprintf("Table 1. Split-Plot ANOVA Table - Response %s", resp_nm)),
    headers = list(id = hdr_sp_id, en = hdr_sp_en),
    rows = list(id = rows_sp_id, en = rows_sp_en),
    note = list(
      id = sprintf("Koefisien Keragaman Petak Utama KK(a) = %s%%; KK Anak Petak KK(b) = %s%%. Petak Utama diuji terhadap Galat (a), Anak Petak & Interaksi diuji terhadap Galat (b).", sl_fmt_num(cv_a, 2L, "id"), sl_fmt_num(cv_b, 2L, "id")),
      en = sprintf("Main plot CV(a) = %s%%; Sub plot CV(b) = %s%%. Main Plot tested against Error (a); Sub Plot & Interaction tested against Error (b).", sl_fmt_num(cv_a, 2L, "en"), sl_fmt_num(cv_b, 2L, "en"))
    )
  )

  # Table 2: Interaction Means Matrix Table
  hdr_mat_id <- sl_box_vec(c(sprintf("Petak Utama (%s)", main_nm), sprintf("%s (%s)", b_levs, sub_nm), "Rataan Petak Utama"))
  hdr_mat_en <- sl_box_vec(c(sprintf("Main Plot (%s)", main_nm), sprintf("%s (%s)", b_levs, sub_nm), "Main Plot Mean"))

  means_ab <- tapply(y_all, list(df_sp$A, df_sp$B), mean)
  means_a  <- tapply(y_all, df_sp$A, mean)
  means_b  <- tapply(y_all, df_sp$B, mean)

  rows_mat_id <- list()
  rows_mat_en <- list()
  for (i in seq_len(a_cnt)) {
    a_str <- a_levs[i]
    b_vals_id <- vapply(seq_len(b_cnt), function(j) sl_fmt_num(means_ab[i, j], 2L, "id"), character(1L))
    b_vals_en <- vapply(seq_len(b_cnt), function(j) sl_fmt_num(means_ab[i, j], 2L, "en"), character(1L))
    m_a_id <- sl_fmt_num(means_a[i], 2L, "id")
    m_a_en <- sl_fmt_num(means_a[i], 2L, "en")

    rows_mat_id[[i]] <- sl_box_vec(c(a_str, b_vals_id, m_a_id))
    rows_mat_en[[i]] <- sl_box_vec(c(a_str, b_vals_en, m_a_en))
  }
  # Add bottom marginal row
  b_marg_id <- vapply(seq_len(b_cnt), function(j) sl_fmt_num(means_b[j], 2L, "id"), character(1L))
  b_marg_en <- vapply(seq_len(b_cnt), function(j) sl_fmt_num(means_b[j], 2L, "en"), character(1L))
  rows_mat_id[[a_cnt + 1L]] <- sl_box_vec(c("Rataan Anak Petak", b_marg_id, sl_fmt_num(grand_mean, 2L, "id")))
  rows_mat_en[[a_cnt + 1L]] <- sl_box_vec(c("Sub Plot Mean", b_marg_en, sl_fmt_num(grand_mean, 2L, "en")))

  tbl_mat <- list(
    id = "split_plot_interaction_matrix",
    title = list(id = "Tabel 2. Matriks Rataan Kombinasi Interaksi Petak Utama x Anak Petak", en = "Table 2. Interaction Means Matrix (Main Plot x Sub Plot)"),
    headers = list(id = hdr_mat_id, en = hdr_mat_en),
    rows = list(id = rows_mat_id, en = rows_mat_en),
    note = list(id = "Menampilkan rerata kombinasi perlakuan beserta rataan marjinal petak utama dan anak petak.", en = "Displays combination means alongside marginal means for main and sub plots.")
  )

  # Graphics: Interaction Plot
  plots <- list()
  if (include_plots) {
    p_int <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      col_pal <- c("#1B365D", "#B83227", "#2E7D32", "#D97706", "#7C3AED", "#0891B2")
      y_min <- min(means_ab) * 0.92
      y_max <- max(means_ab) * 1.08

      plot(
        c(1, b_cnt), c(y_min, y_max),
        type = "n", xaxt = "n",
        xlab = sprintf("%s (Anak Petak)", sub_nm),
        ylab = sprintf("Rataan %s", resp_nm),
        main = if (plot_lang == "id") sprintf("Grafik Interaksi Petak Utama (%s) x Anak Petak (%s)", main_nm, sub_nm) else sprintf("Interaction Plot (%s x %s)", main_nm, sub_nm),
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::axis(1, at = seq_len(b_cnt), labels = b_levs, col.axis = "#1B365D", font = 2)
      graphics::grid(col = "#E3DDD0", lty = 3)

      for (i in seq_len(a_cnt)) {
        graphics::lines(seq_len(b_cnt), means_ab[i, ], col = col_pal[(i - 1L) %% length(col_pal) + 1L], lwd = 2.2, type = "b", pch = 19, cex = 1.3)
      }

      graphics::legend(
        "topleft", legend = paste0(main_nm, ": ", a_levs), col = col_pal[seq_len(a_cnt)],
        lwd = 2.2, pch = 19, bty = "n", cex = 0.82
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "split_plot_interaction",
      title = list(id = "Grafik Interaksi Petak Utama x Anak Petak", en = "Interaction Plot (Main Plot x Sub Plot)"),
      png_data_url = p_int$png_data_url,
      width_in = p_int$width_in,
      height_in = p_int$height_in
    )
  }

  pts_id <- c(
    if (is.finite(p_a) && p_a < 0.05) {
      sprintf("• **Petak Utama (%s)**: Berpengaruh sangat nyata secara statistik terhadap %s (F(%d, %d) = %s, p = %s; diuji terhadap Galat a).",
        main_nm, resp_nm, df_a, df_ea, sl_fmt_num(f_a, 3L, "id"), sl_fmt_p(p_a, "id"))
    } else {
      sprintf("• **Petak Utama (%s)**: Tidak berpengaruh signifikan terhadap %s (F(%d, %d) = %s, p = %s).",
        main_nm, resp_nm, df_a, df_ea, sl_fmt_num(f_a, 3L, "id"), sl_fmt_p(p_a, "id"))
    },
    if (is.finite(p_b_factor) && p_b_factor < 0.05) {
      sprintf("• **Anak Petak (%s)**: Berpengaruh sangat nyata secara statistik terhadap %s (F(%d, %d) = %s, p = %s; diuji terhadap Galat b).",
        sub_nm, resp_nm, df_b_factor, df_eb, sl_fmt_num(f_b_factor, 3L, "id"), sl_fmt_p(p_b_factor, "id"))
    } else {
      sprintf("• **Anak Petak (%s)**: Tidak berpengaruh signifikan terhadap %s (F(%d, %d) = %s, p = %s).",
        sub_nm, resp_nm, df_b_factor, df_eb, sl_fmt_num(f_b_factor, 3L, "id"), sl_fmt_p(p_b_factor, "id"))
    },
    if (is.finite(p_ab) && p_ab < 0.05) {
      sprintf("• **Interaksi Petak Utama x Anak Petak**: Terdapat interaksi sinergis yang sangat nyata antara %s dan %s (F(%d, %d) = %s, p = %s). Pengaruh faktor anak petak bergantung secara signifikan pada aras petak utama.",
        main_nm, sub_nm, df_ab, df_eb, sl_fmt_num(f_ab, 3L, "id"), sl_fmt_p(p_ab, "id"))
    } else {
      sprintf("• **Interaksi Petak Utama x Anak Petak**: Tidak terdapat interaksi yang signifikan antara %s dan %s (F(%d, %d) = %s, p = %s). Kedua faktor bertindak secara independen.",
        main_nm, sub_nm, df_ab, df_eb, sl_fmt_num(f_ab, 3L, "id"), sl_fmt_p(p_ab, "id"))
    },
    sprintf("• **Ketelitian Percobaan**: Koefisien keragaman petak utama KK(a) = %s%% dan anak petak KK(b) = %s%%.",
      sl_fmt_num(cv_a, 2L, "id"), sl_fmt_num(cv_b, 2L, "id"))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_a) && p_a < 0.05) {
      sprintf("• **Main Plot Effect (%s)**: Statistically significant effect on %s (F(%d, %d) = %s, p = %s; tested against Error a).",
        main_nm, resp_nm, df_a, df_ea, sl_fmt_num(f_a, 3L, "en"), sl_fmt_p(p_a, "en"))
    } else {
      sprintf("• **Main Plot Effect (%s)**: No statistically significant effect on %s (F(%d, %d) = %s, p = %s).",
        main_nm, resp_nm, df_a, df_ea, sl_fmt_num(f_a, 3L, "en"), sl_fmt_p(p_a, "en"))
    },
    if (is.finite(p_b_factor) && p_b_factor < 0.05) {
      sprintf("• **Sub Plot Effect (%s)**: Statistically significant effect on %s (F(%d, %d) = %s, p = %s; tested against Error b).",
        sub_nm, resp_nm, df_b_factor, df_eb, sl_fmt_num(f_b_factor, 3L, "en"), sl_fmt_p(p_b_factor, "en"))
    } else {
      sprintf("• **Sub Plot Effect (%s)**: No statistically significant effect on %s (F(%d, %d) = %s, p = %s).",
        sub_nm, resp_nm, df_b_factor, df_eb, sl_fmt_num(f_b_factor, 3L, "en"), sl_fmt_p(p_b_factor, "en"))
    },
    if (is.finite(p_ab) && p_ab < 0.05) {
      sprintf("• **Main Plot x Sub Plot Interaction**: Significant synergistic interaction detected (F(%d, %d) = %s, p = %s). Sub-plot factor effect depends on main-plot level.",
        df_ab, df_eb, sl_fmt_num(f_ab, 3L, "en"), sl_fmt_p(p_ab, "en"))
    } else {
      sprintf("• **Main Plot x Sub Plot Interaction**: No significant interaction detected (F(%d, %d) = %s, p = %s). Factors act additively.",
        df_ab, df_eb, sl_fmt_num(f_ab, 3L, "en"), sl_fmt_p(p_ab, "en"))
    },
    sprintf("• **Precision**: Main plot CV(a) = %s%% and sub plot CV(b) = %s%%.",
      sl_fmt_num(cv_a, 2L, "en"), sl_fmt_num(cv_b, 2L, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "split_plot",
    analysis_label = list(id = "Rancangan Petak Terbagi (Split-Plot Design)", en = "Split-Plot Design"),
    response_label = resp_nm,
    mode = "long",
    alpha = alpha,
    n_raw = as.integer(n_total),
    n_valid = as.integer(n_total),
    n_missing = 0L,
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(main_nm, sub_nm)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(
      f_main_plot = f_a,
      p_main_plot = p_a,
      f_sub_plot = f_b_factor,
      p_sub_plot = p_b_factor,
      f_interaction = f_ab,
      p_interaction = p_ab,
      cv_a = cv_a,
      cv_b = cv_b
    ),
    posthoc = NULL,
    tables_apa = list(tbl_sp, tbl_mat),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Model Linier Petak Terbagi (Split-Plot)", en = "Split-Plot Linear Model"),
        latex = "Y_{ijk} = \\mu + \\beta_k + \\alpha_i + \\delta_{ik} + \\beta_j + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}"
      ),
      list(
        title = list(id = "Uji F Petak Utama & Anak Petak", en = "F-Tests for Main & Sub Plots"),
        latex = "F_A = \\frac{KT_A}{KT_{\\text{Galat}(a)}}, \\quad F_B = \\frac{KT_B}{KT_{\\text{Galat}(b)}}, \\quad F_{AB} = \\frac{KT_{AB}}{KT_{\\text{Galat}(b)}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
