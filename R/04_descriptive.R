# R/04_descriptive.R
# StatLab Descriptive Biostatistics Engine, APA Layer-1 & SPSS Layer-2 Tables, and KaTeX Formulas
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base and stats functions + StatLab R/00..05 modules (zero external/CRAN packages).

.sl_coerce_col_vec <- function(col) {
  if (is.null(col)) {
    return(logical(0L))
  }
  if (is.factor(col)) {
    return(as.character(col))
  }
  if (is.atomic(col)) {
    return(col)
  }
  if (is.list(col)) {
    n <- length(col)
    if (n == 0L) {
      return(logical(0L))
    }
    has_chr <- FALSE
    has_num <- FALSE
    has_lgl <- FALSE
    for (i in seq_len(n)) {
      el <- col[[i]]
      if (!is.null(el) && length(el) > 0L && !is.na(el[[1L]])) {
        v <- el[[1L]]
        if (is.character(v) || is.factor(v)) {
          has_chr <- TRUE
        } else if (is.numeric(v)) {
          has_num <- TRUE
        } else if (is.logical(v)) {
          has_lgl <- TRUE
        } else {
          has_chr <- TRUE
        }
      }
    }
    if (has_chr) {
      out <- rep(NA_character_, n)
      for (i in seq_len(n)) {
        el <- col[[i]]
        if (!is.null(el) && length(el) > 0L && !is.na(el[[1L]])) {
          out[i] <- as.character(el[[1L]])
        }
      }
      return(out)
    }
    if (has_num) {
      out <- rep(NA_real_, n)
      for (i in seq_len(n)) {
        el <- col[[i]]
        if (!is.null(el) && length(el) > 0L && !is.na(el[[1L]])) {
          out[i] <- as.double(el[[1L]])
        }
      }
      return(out)
    }
    if (has_lgl) {
      out <- rep(NA, n)
      for (i in seq_len(n)) {
        el <- col[[i]]
        if (!is.null(el) && length(el) > 0L && !is.na(el[[1L]])) {
          out[i] <- as.logical(el[[1L]])
        }
      }
      return(out)
    }
    return(rep(NA_real_, n))
  }
  as.character(col)
}

#' Safely convert a data.frame or jagged named list of columns into a rectangular data.frame
#'
#' Pads shorter vectors with trailing `NA`s so `sl_prepare_groups(..., mode = "wide")`
#' treats padded cells as blank trailing cells without inflating `n_missing`.
#'
#' @param data_obj `data.frame` or named `list` of column vectors
#' @return Rectangular `data.frame`
.sl_as_dataframe <- function(data_obj) {
  if (is.data.frame(data_obj)) {
    return(data_obj)
  }
  if (!is.list(data_obj) || is.null(names(data_obj))) {
    stop("`df` must be a data.frame or named list of columns.")
  }
  col_names <- names(data_obj)
  nc <- length(col_names)
  if (nc == 0L) {
    return(data.frame())
  }
  vecs <- lapply(data_obj, .sl_coerce_col_vec)
  lens <- lengths(vecs)
  max_len <- max(lens)

  padded <- vector("list", nc)
  for (j in seq_len(nc)) {
    v <- vecs[[j]]
    lv <- length(v)
    if (lv < max_len) {
      if (lv == 0L) {
        v <- rep(NA_real_, max_len)
      } else {
        length(v) <- max_len
      }
    }
    padded[[j]] <- v
  }
  names(padded) <- col_names
  as.data.frame(padded, stringsAsFactors = FALSE, check.names = FALSE)
}

.sl_count_missing_per_group <- function(df, mode, response_cols, group_col, grp_levels) {
  out <- stats::setNames(rep(0L, length(grp_levels)), grp_levels)
  if (length(grp_levels) == 0L) {
    return(out)
  }

  if (mode == "wide") {
    for (g_nm in grp_levels) {
      if (g_nm %in% names(df)) {
        parsed <- .sl_parse_numeric_vec(df[[g_nm]])
        occ_idx <- which(!parsed$is_blank)
        last_occ <- if (length(occ_idx) > 0L) max(occ_idx) else 0L
        if (last_occ > 0L) {
          out[[g_nm]] <- as.integer(sum(parsed$is_missing[seq_len(last_occ)]))
        }
      }
    }
  } else {
    resp_nm <- response_cols[1L]
    parsed_y <- .sl_parse_numeric_vec(df[[resp_nm]])
    has_grp <- !is.null(group_col) && length(group_col) > 0L &&
      !is.na(group_col[1L]) && nzchar(as.character(group_col[1L]))
    if (has_grp) {
      parsed_g <- .sl_parse_group_vec(df[[as.character(group_col[1L])]])
      for (g_nm in grp_levels) {
        out[[g_nm]] <- as.integer(sum(!parsed_g$is_missing & parsed_g$labels == g_nm & parsed_y$is_missing))
      }
    } else {
      out[[grp_levels[1L]]] <- as.integer(sum(parsed_y$is_missing))
    }
  }
  out
}

.sl_compute_group_stats <- function(x, group_name, n_missing, alpha) {
  vals <- as.double(x)
  vals <- vals[is.finite(vals)]
  n <- as.integer(length(vals))

  if (n == 0L) {
    return(list(
      group = as.character(group_name),
      n = 0L,
      n_missing = as.integer(n_missing),
      mean = NA_real_,
      sd = NA_real_,
      se = NA_real_,
      median = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      iqr = NA_real_,
      min = NA_real_,
      max = NA_real_,
      cv_pct = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_
    ))
  }

  m <- mean(vals)
  if (!is.finite(m)) {
    m <- NA_real_
  }
  med <- stats::median(vals)
  if (!is.finite(med)) {
    med <- NA_real_
  }
  qs <- stats::quantile(vals, probs = c(0.25, 0.75), names = FALSE, type = 7)
  q1 <- if (is.finite(qs[1L])) as.double(qs[1L]) else NA_real_
  q3 <- if (is.finite(qs[2L])) as.double(qs[2L]) else NA_real_
  iqr_raw <- as.double(q3 - q1)
  iqr_val <- if (is.finite(iqr_raw)) iqr_raw else NA_real_
  min_val <- min(vals)
  max_val <- max(vals)

  if (n < 2L) {
    s <- NA_real_
    se <- NA_real_
    cv_pct <- NA_real_
    ci_low <- NA_real_
    ci_high <- NA_real_
  } else {
    s_raw <- if (max_val == min_val) 0.0 else stats::sd(vals)
    s <- if (is.finite(s_raw)) s_raw else NA_real_
    se_raw <- if (is.finite(s)) s / sqrt(as.double(n)) else NA_real_
    se <- if (is.finite(se_raw)) se_raw else NA_real_
    cv_raw <- if (is.finite(m) && abs(m) >= 1e-12 && is.finite(s)) {
      (s / abs(m)) * 100.0
    } else {
      NA_real_
    }
    cv_pct <- if (is.finite(cv_raw)) cv_raw else NA_real_
    if (is.finite(m) && is.finite(s) && is.finite(se) && s > 0) {
      t_crit <- stats::qt(1.0 - alpha / 2.0, df = n - 1L)
      ci_lo_raw <- m - t_crit * se
      ci_hi_raw <- m + t_crit * se
      if (is.finite(ci_lo_raw) && is.finite(ci_hi_raw)) {
        ci_low <- ci_lo_raw
        ci_high <- ci_hi_raw
      } else {
        ci_low <- NA_real_
        ci_high <- NA_real_
      }
    } else if (is.finite(m) && is.finite(s) && s == 0) {
      ci_low <- m
      ci_high <- m
    } else {
      ci_low <- NA_real_
      ci_high <- NA_real_
    }
  }

  if (n >= 3L && is.finite(m) && is.finite(s) && s > 0) {
    nd <- as.double(n)
    z <- (vals - m) / s
    sk_raw <- (nd / ((nd - 1.0) * (nd - 2.0))) * sum(z ^ 3)
    skewness <- if (is.finite(sk_raw)) sk_raw else NA_real_
  } else {
    skewness <- NA_real_
  }

  if (n >= 4L && is.finite(m) && is.finite(s) && s > 0) {
    nd <- as.double(n)
    z <- (vals - m) / s
    ku_raw <- ((nd * (nd + 1.0)) / ((nd - 1.0) * (nd - 2.0) * (nd - 3.0))) * sum(z ^ 4) -
      (3.0 * ((nd - 1.0) ^ 2)) / ((nd - 2.0) * (nd - 3.0))
    kurtosis <- if (is.finite(ku_raw)) ku_raw else NA_real_
  } else {
    kurtosis <- NA_real_
  }

  list(
    group = as.character(group_name),
    n = n,
    n_missing = as.integer(n_missing),
    mean = as.double(m),
    sd = as.double(s),
    se = as.double(se),
    median = as.double(med),
    q1 = q1,
    q3 = q3,
    iqr = iqr_val,
    min = as.double(min_val),
    max = as.double(max_val),
    cv_pct = as.double(cv_pct),
    skewness = as.double(skewness),
    kurtosis = as.double(kurtosis),
    ci_low = as.double(ci_low),
    ci_high = as.double(ci_high)
  )
}

.sl_build_apa_tables <- function(main_groups, assumptions, outliers, resp_label, alpha) {
  dig <- .sl_choose_summary_digits(main_groups)
  ci_pct_id <- .sl_fmt_conf_pct(alpha, "id")
  ci_pct_en <- .sl_fmt_conf_pct(alpha, "en")
  k <- length(main_groups)

  rows_id <- vector("list", k)
  rows_en <- vector("list", k)

  for (i in seq_len(k)) {
    g <- main_groups[[i]]
    pg <- if (i <= length(assumptions$per_group)) assumptions$per_group[[i]] else NULL
    sw <- if (!is.null(pg)) pg$shapiro else list(valid = FALSE, p_value = NA_real_)

    mean_sd_id <- if (g$n >= 2L) {
      sprintf("%s \u00b1 %s", sl_fmt_num(g$mean, dig, "id"), sl_fmt_num(g$sd, dig, "id"))
    } else if (g$n == 1L) {
      sprintf("%s \u00b1 \u2014", sl_fmt_num(g$mean, dig, "id"))
    } else {
      "\u2014"
    }
    mean_sd_en <- if (g$n >= 2L) {
      sprintf("%s \u00b1 %s", sl_fmt_num(g$mean, dig, "en"), sl_fmt_num(g$sd, dig, "en"))
    } else if (g$n == 1L) {
      sprintf("%s \u00b1 \u2014", sl_fmt_num(g$mean, dig, "en"))
    } else {
      "\u2014"
    }

    med_iqr_id <- if (g$n >= 1L) {
      sprintf("%s (%s)", sl_fmt_num(g$median, dig, "id"), sl_fmt_num(g$iqr, dig, "id"))
    } else {
      "\u2014"
    }
    med_iqr_en <- if (g$n >= 1L) {
      sprintf("%s (%s)", sl_fmt_num(g$median, dig, "en"), sl_fmt_num(g$iqr, dig, "en"))
    } else {
      "\u2014"
    }

    minmax_id <- if (g$n >= 1L) {
      sprintf("%s \u2013 %s", sl_fmt_num(g$min, dig, "id"), sl_fmt_num(g$max, dig, "id"))
    } else {
      "\u2014"
    }
    minmax_en <- if (g$n >= 1L) {
      sprintf("%s \u2013 %s", sl_fmt_num(g$min, dig, "en"), sl_fmt_num(g$max, dig, "en"))
    } else {
      "\u2014"
    }

    ci_id <- if (is.finite(g$ci_low) && is.finite(g$ci_high)) {
      sprintf("[%s; %s]", sl_fmt_num(g$ci_low, dig, "id"), sl_fmt_num(g$ci_high, dig, "id"))
    } else {
      "\u2014"
    }
    ci_en <- if (is.finite(g$ci_low) && is.finite(g$ci_high)) {
      sprintf("[%s, %s]", sl_fmt_num(g$ci_low, dig, "en"), sl_fmt_num(g$ci_high, dig, "en"))
    } else {
      "\u2014"
    }

    sw_p_id <- if (isTRUE(sw$valid)) sl_fmt_p(sw$p_value, "id") else "\u2014"
    sw_p_en <- if (isTRUE(sw$valid)) sl_fmt_p(sw$p_value, "en") else "\u2014"

    rows_id[[i]] <- sl_box_vec(c(
      g$group,
      sprintf("%d", g$n),
      mean_sd_id,
      med_iqr_id,
      minmax_id,
      ci_id,
      sl_fmt_num(g$cv_pct, 2L, "id"),
      sw_p_id
    ))
    rows_en[[i]] <- sl_box_vec(c(
      g$group,
      sprintf("%d", g$n),
      mean_sd_en,
      med_iqr_en,
      minmax_en,
      ci_en,
      sl_fmt_num(g$cv_pct, 2L, "en"),
      sw_p_en
    ))
  }

  hom_note_id <- ""
  hom_note_en <- ""
  if (!is.null(assumptions$homogeneity) && isTRUE(assumptions$homogeneity$levene_bf$valid)) {
    lev <- assumptions$homogeneity$levene_bf
    hom_note_id <- sprintf(
      " Homogenitas Levene Brown-Forsythe: F(%d, %d) = %s, p = %s.",
      lev$df1, lev$df2, sl_fmt_num(lev$statistic, 3L, "id"), sl_fmt_p(lev$p_value, "id")
    )
    hom_note_en <- sprintf(
      " Brown-Forsythe Levene homogeneity: F(%d, %d) = %s, p = %s.",
      lev$df1, lev$df2, sl_fmt_num(lev$statistic, 3L, "en"), sl_fmt_p(lev$p_value, "en")
    )
  }

  n_flag <- if (!is.null(outliers$flagged)) nrow(outliers$flagged) else 0L
  note_id <- sprintf(
    "Catatan. Nilai disajikan sebagai Rerata \u00b1 Simpangan Baku (SD) dan Median (Rentang Antarkuartil, IQR). Taraf signifikansi \u03b1 = %s.%s Pencilan terdeteksi: %d.",
    .sl_fmt_alpha(alpha, "id"), hom_note_id, n_flag
  )
  note_en <- sprintf(
    "Note. Values are presented as Mean \u00b1 Standard Deviation (SD) and Median (Interquartile Range, IQR). Significance level \u03b1 = %s.%s Flagged outliers: %d.",
    .sl_fmt_alpha(alpha, "en"), hom_note_en, n_flag
  )

  list(
    list(
      id = "apa_descriptives",
      title = list(
        id = sprintf("Tabel 1. Ringkasan Statistik Deskriptif dan Normalitas (%s)", resp_label),
        en = sprintf("Table 1. Descriptive Statistics and Normality Summary (%s)", resp_label)
      ),
      headers = list(
        id = sl_box_vec(c(
          "Kelompok", "n", "Rerata \u00b1 SD", "Median (IQR)",
          "Min \u2013 Maks", sprintf("IK %s", ci_pct_id), "CV (%)", "Shapiro-Wilk p"
        )),
        en = sl_box_vec(c(
          "Group", "n", "Mean \u00b1 SD", "Median (IQR)",
          "Min \u2013 Max", sprintf("%s CI", ci_pct_en), "CV (%)", "Shapiro-Wilk p"
        ))
      ),
      rows = list(id = rows_id, en = rows_en),
      note = list(id = enc2utf8(note_id), en = enc2utf8(note_en))
    )
  )
}

.sl_build_full_tables <- function(main_groups, assumptions, outliers, excluded_outliers, alpha) {
  k <- length(main_groups)
  ci_pct_id <- .sl_fmt_conf_pct(alpha, "id")
  ci_pct_en <- .sl_fmt_conf_pct(alpha, "en")

  # 1. descriptives_full
  desc_rows_id <- vector("list", k)
  desc_rows_en <- vector("list", k)
  for (i in seq_len(k)) {
    g <- main_groups[[i]]
    desc_rows_id[[i]] <- sl_box_vec(c(
      g$group,
      sprintf("%d", g$n),
      sprintf("%d", g$n_missing),
      sl_fmt_num(g$mean, 4L, "id"),
      sl_fmt_num(g$sd, 4L, "id"),
      sl_fmt_num(g$se, 4L, "id"),
      sl_fmt_num(g$ci_low, 4L, "id"),
      sl_fmt_num(g$ci_high, 4L, "id"),
      sl_fmt_num(g$median, 4L, "id"),
      sl_fmt_num(g$q1, 4L, "id"),
      sl_fmt_num(g$q3, 4L, "id"),
      sl_fmt_num(g$iqr, 4L, "id"),
      sl_fmt_num(g$min, 4L, "id"),
      sl_fmt_num(g$max, 4L, "id"),
      sl_fmt_num(g$cv_pct, 4L, "id"),
      sl_fmt_num(g$skewness, 4L, "id"),
      sl_fmt_num(g$kurtosis, 4L, "id")
    ))
    desc_rows_en[[i]] <- sl_box_vec(c(
      g$group,
      sprintf("%d", g$n),
      sprintf("%d", g$n_missing),
      sl_fmt_num(g$mean, 4L, "en"),
      sl_fmt_num(g$sd, 4L, "en"),
      sl_fmt_num(g$se, 4L, "en"),
      sl_fmt_num(g$ci_low, 4L, "en"),
      sl_fmt_num(g$ci_high, 4L, "en"),
      sl_fmt_num(g$median, 4L, "en"),
      sl_fmt_num(g$q1, 4L, "en"),
      sl_fmt_num(g$q3, 4L, "en"),
      sl_fmt_num(g$iqr, 4L, "en"),
      sl_fmt_num(g$min, 4L, "en"),
      sl_fmt_num(g$max, 4L, "en"),
      sl_fmt_num(g$cv_pct, 4L, "en"),
      sl_fmt_num(g$skewness, 4L, "en"),
      sl_fmt_num(g$kurtosis, 4L, "en")
    ))
  }

  tb_desc <- list(
    id = "descriptives_full",
    title = list(
      id = "Lampiran A. Statistik Deskriptif Lengkap (Gaya SPSS)",
      en = "Appendix A. Complete Descriptive Statistics (SPSS Style)"
    ),
    headers = list(
      id = sl_box_vec(c(
        "Kelompok", "n Valid", "n Hilang", "Rerata", "SD", "SE",
        sprintf("IK %s Bawah", ci_pct_id), sprintf("IK %s Atas", ci_pct_id),
        "Median", "Q1", "Q3", "IQR", "Min", "Maks", "CV (%)",
        "Kemencengan (G1)", "Kurtosis Eks. (G2)"
      )),
      en = sl_box_vec(c(
        "Group", "Valid n", "Missing n", "Mean", "SD", "SE",
        sprintf("%s CI Lower", ci_pct_en), sprintf("%s CI Upper", ci_pct_en),
        "Median", "Q1", "Q3", "IQR", "Min", "Max", "CV (%)",
        "Skewness (G1)", "Excess Kurtosis (G2)"
      ))
    ),
    rows = list(id = desc_rows_id, en = desc_rows_en),
    note = list(
      id = "Kemencengan (G1) dan Kurtosis Ekses (G2) menggunakan koreksi sampel Fisher-Pearson (setara SPSS/Excel).",
      en = "Skewness (G1) and Excess Kurtosis (G2) use the adjusted Fisher-Pearson sample formulas (matching SPSS/Excel)."
    )
  )

  # 2. normality_full
  n_pg <- length(assumptions$per_group)
  norm_rows_id <- vector("list", n_pg)
  norm_rows_en <- vector("list", n_pg)
  for (i in seq_len(n_pg)) {
    pg <- assumptions$per_group[[i]]
    sw <- pg$shapiro
    lf <- pg$lilliefors

    st_id <- if (is.na(pg$is_normal)) {
      "Tidak berlaku"
    } else if (isTRUE(pg$is_normal)) {
      "Normal"
    } else {
      "Tidak Normal"
    }
    st_en <- if (is.na(pg$is_normal)) {
      "Not applicable"
    } else if (isTRUE(pg$is_normal)) {
      "Normal"
    } else {
      "Non-normal"
    }

    note_key <- if (!is.na(sw$note_key)) sw$note_key else lf$note_key
    nt_id <- if (!is.na(note_key) && nzchar(note_key)) .sl_note_text(note_key, "id") else "\u2014"
    nt_en <- if (!is.na(note_key) && nzchar(note_key)) .sl_note_text(note_key, "en") else "\u2014"

    norm_rows_id[[i]] <- sl_box_vec(c(
      pg$group,
      sprintf("%d", pg$n),
      if (isTRUE(sw$valid)) sl_fmt_num(sw$statistic, 4L, "id") else "\u2014",
      if (isTRUE(sw$valid)) sl_fmt_p(sw$p_value, "id") else "\u2014",
      if (isTRUE(lf$valid)) sl_fmt_num(lf$statistic, 4L, "id") else "\u2014",
      if (isTRUE(lf$valid)) sl_fmt_p(lf$p_value, "id") else "\u2014",
      st_id,
      nt_id
    ))
    norm_rows_en[[i]] <- sl_box_vec(c(
      pg$group,
      sprintf("%d", pg$n),
      if (isTRUE(sw$valid)) sl_fmt_num(sw$statistic, 4L, "en") else "\u2014",
      if (isTRUE(sw$valid)) sl_fmt_p(sw$p_value, "en") else "\u2014",
      if (isTRUE(lf$valid)) sl_fmt_num(lf$statistic, 4L, "en") else "\u2014",
      if (isTRUE(lf$valid)) sl_fmt_p(lf$p_value, "en") else "\u2014",
      st_en,
      nt_en
    ))
  }

  tb_norm <- list(
    id = "normality_full",
    title = list(
      id = "Lampiran B. Uji Normalitas (Shapiro-Wilk & Lilliefors)",
      en = "Appendix B. Tests of Normality (Shapiro-Wilk & Lilliefors)"
    ),
    headers = list(
      id = sl_box_vec(c(
        "Kelompok", "n", "Shapiro-Wilk W", "Shapiro-Wilk Sig.",
        "Lilliefors D", "Lilliefors Sig.", "Keputusan", "Catatan"
      )),
      en = sl_box_vec(c(
        "Group", "n", "Shapiro-Wilk W", "Shapiro-Wilk Sig.",
        "Lilliefors D", "Lilliefors Sig.", "Decision", "Note"
      ))
    ),
    rows = list(id = norm_rows_id, en = norm_rows_en),
    note = list(
      id = sprintf("Shapiro-Wilk merupakan uji utama untuk 3 \u2264 n \u2264 5000 (\u03b1 = %s); Kolmogorov-Smirnov koreksi Lilliefors disajikan sebagai pembanding.", .sl_fmt_alpha(alpha, "id")),
      en = sprintf("Shapiro-Wilk is the primary test for 3 \u2264 n \u2264 5000 (\u03b1 = %s); Lilliefors-corrected Kolmogorov-Smirnov is reported as a reference.", .sl_fmt_alpha(alpha, "en"))
    )
  )

  # 3. homogeneity_full
  if (!is.null(assumptions$homogeneity)) {
    lev <- assumptions$homogeneity$levene_bf
    bart <- assumptions$homogeneity$bartlett

    lev_st_id <- if (!isTRUE(lev$valid)) "Tidak berlaku" else if (isTRUE(assumptions$homogeneity$is_homogeneous)) "Homogen" else "Tidak Homogen"
    lev_st_en <- if (!isTRUE(lev$valid)) "Not applicable" else if (isTRUE(assumptions$homogeneity$is_homogeneous)) "Homogeneous" else "Heterogeneous"
    bart_st_id <- if (!isTRUE(bart$valid)) "Tidak berlaku" else if (bart$p_value >= alpha) "Homogen (pendukung)" else "Tidak Homogen (pendukung)"
    bart_st_en <- if (!isTRUE(bart$valid)) "Not applicable" else if (bart$p_value >= alpha) "Homogeneous (supporting)" else "Heterogeneous (supporting)"

    hom_rows_id <- list(
      sl_box_vec(c(
        "Levene (Brown-Forsythe, Median)",
        if (isTRUE(lev$valid)) sl_fmt_num(lev$statistic, 4L, "id") else "\u2014",
        if (!is.na(lev$df1)) sprintf("%d", lev$df1) else "\u2014",
        if (!is.na(lev$df2)) sprintf("%d", lev$df2) else "\u2014",
        if (isTRUE(lev$valid)) sl_fmt_p(lev$p_value, "id") else "\u2014",
        lev_st_id,
        if (!is.na(lev$note_key)) .sl_note_text(lev$note_key, "id") else "Uji penentu utama"
      )),
      sl_box_vec(c(
        "Bartlett (Chi-Square)",
        if (isTRUE(bart$valid)) sl_fmt_num(bart$statistic, 4L, "id") else "\u2014",
        if (!is.na(bart$df)) sprintf("%d", bart$df) else "\u2014",
        "\u2014",
        if (isTRUE(bart$valid)) sl_fmt_p(bart$p_value, "id") else "\u2014",
        bart_st_id,
        if (!is.na(bart$note_key)) .sl_note_text(bart$note_key, "id") else "Sensitif ketidaknormalan"
      ))
    )
    hom_rows_en <- list(
      sl_box_vec(c(
        "Levene (Brown-Forsythe, Median)",
        if (isTRUE(lev$valid)) sl_fmt_num(lev$statistic, 4L, "en") else "\u2014",
        if (!is.na(lev$df1)) sprintf("%d", lev$df1) else "\u2014",
        if (!is.na(lev$df2)) sprintf("%d", lev$df2) else "\u2014",
        if (isTRUE(lev$valid)) sl_fmt_p(lev$p_value, "en") else "\u2014",
        lev_st_en,
        if (!is.na(lev$note_key)) .sl_note_text(lev$note_key, "en") else "Primary decision rule"
      )),
      sl_box_vec(c(
        "Bartlett (Chi-Square)",
        if (isTRUE(bart$valid)) sl_fmt_num(bart$statistic, 4L, "en") else "\u2014",
        if (!is.na(bart$df)) sprintf("%d", bart$df) else "\u2014",
        "\u2014",
        if (isTRUE(bart$valid)) sl_fmt_p(bart$p_value, "en") else "\u2014",
        bart_st_en,
        if (!is.na(bart$note_key)) .sl_note_text(bart$note_key, "en") else "Sensitive to non-normality"
      ))
    )
  } else {
    hom_rows_id <- list(
      sl_box_vec(c("Levene (Brown-Forsythe, Median)", "\u2014", "\u2014", "\u2014", "\u2014", "Tidak berlaku", "Kurang dari 2 kelompok"))
    )
    hom_rows_en <- list(
      sl_box_vec(c("Levene (Brown-Forsythe, Median)", "\u2014", "\u2014", "\u2014", "\u2014", "Not applicable", "Fewer than 2 groups"))
    )
  }

  tb_hom <- list(
    id = "homogeneity_full",
    title = list(
      id = "Lampiran C. Uji Homogenitas Varians",
      en = "Appendix C. Tests of Homogeneity of Variance"
    ),
    headers = list(
      id = sl_box_vec(c("Uji", "Statistik", "df1", "df2", "Sig.", "Keputusan", "Catatan")),
      en = sl_box_vec(c("Test", "Statistic", "df1", "df2", "Sig.", "Decision", "Note"))
    ),
    rows = list(id = hom_rows_id, en = hom_rows_en),
    note = list(
      id = "Keputusan homogenitas mengikuti uji Levene Brown-Forsythe (simpangan absolut terhadap median kelompok).",
      en = "Homogeneity decisions follow the Brown-Forsythe Levene test (absolute deviations from group medians)."
    )
  )

  # 4. outliers_full
  grp_names <- names(outliers$per_group)
  n_og <- length(grp_names)
  out_rows_id <- vector("list", n_og)
  out_rows_en <- vector("list", n_og)
  for (i in seq_len(n_og)) {
    gnm <- grp_names[i]
    og <- outliers$per_group[[gnm]]
    bnds <- as.double(og$iqr_bounds)
    gr <- og$grubbs

    fl_sub <- if (!is.null(outliers$flagged) && nrow(outliers$flagged) > 0L) {
      outliers$flagged[outliers$flagged$group == gnm, , drop = FALSE]
    } else {
      data.frame()
    }

    if (nrow(fl_sub) > 0L) {
      fl_txt_id <- paste(
        vapply(
          seq_len(nrow(fl_sub)),
          function(r) sprintf("#%d (%s)", fl_sub$source_row[r], sl_fmt_num(fl_sub$value[r], 4L, "id")),
          character(1L)
        ),
        collapse = ", "
      )
      fl_txt_en <- paste(
        vapply(
          seq_len(nrow(fl_sub)),
          function(r) sprintf("#%d (%s)", fl_sub$source_row[r], sl_fmt_num(fl_sub$value[r], 4L, "en")),
          character(1L)
        ),
        collapse = ", "
      )
    } else {
      fl_txt_id <- "Tidak ada"
      fl_txt_en <- "None"
    }

    out_rows_id[[i]] <- sl_box_vec(c(
      gnm,
      sl_fmt_num(bnds[1L], 4L, "id"),
      sl_fmt_num(bnds[2L], 4L, "id"),
      if (isTRUE(gr$valid)) sl_fmt_num(gr$G, 4L, "id") else "\u2014",
      if (isTRUE(gr$valid)) sl_fmt_p(gr$p_value, "id") else "\u2014",
      if (isTRUE(gr$valid) && !is.na(gr$suspect_row)) paste0("#", gr$suspect_row) else "\u2014",
      if (isTRUE(gr$valid)) sl_fmt_num(gr$suspect_value, 4L, "id") else "\u2014",
      fl_txt_id
    ))
    out_rows_en[[i]] <- sl_box_vec(c(
      gnm,
      sl_fmt_num(bnds[1L], 4L, "en"),
      sl_fmt_num(bnds[2L], 4L, "en"),
      if (isTRUE(gr$valid)) sl_fmt_num(gr$G, 4L, "en") else "\u2014",
      if (isTRUE(gr$valid)) sl_fmt_p(gr$p_value, "en") else "\u2014",
      if (isTRUE(gr$valid) && !is.na(gr$suspect_row)) paste0("#", gr$suspect_row) else "\u2014",
      if (isTRUE(gr$valid)) sl_fmt_num(gr$suspect_value, 4L, "en") else "\u2014",
      fl_txt_en
    ))
  }

  n_excl <- if (!is.null(excluded_outliers) && is.data.frame(excluded_outliers)) nrow(excluded_outliers) else 0L
  excl_note_id <- if (n_excl > 0L) {
    sprintf(" Baris yang dikeluarkan secara eksplisit oleh pengguna: %s.", paste(excluded_outliers$source_row, collapse = ", "))
  } else {
    ""
  }
  excl_note_en <- if (n_excl > 0L) {
    sprintf(" Rows explicitly excluded by user: %s.", paste(excluded_outliers$source_row, collapse = ", "))
  } else {
    ""
  }

  tb_out <- list(
    id = "outliers_full",
    title = list(
      id = "Lampiran D. Diagnostik Pencilan (Pagar 1,5\u00d7IQR & Uji Grubbs)",
      en = "Appendix D. Outlier Diagnostics (1.5\u00d7IQR Fences & Grubbs Test)"
    ),
    headers = list(
      id = sl_box_vec(c(
        "Kelompok", "Batas Bawah IQR", "Batas Atas IQR",
        "Grubbs G", "Grubbs Sig.", "Baris Tersangka", "Nilai Tersangka", "Pencilan Ditandai"
      )),
      en = sl_box_vec(c(
        "Group", "IQR Lower Fence", "IQR Upper Fence",
        "Grubbs G", "Grubbs Sig.", "Suspect Row", "Suspect Value", "Flagged Outliers"
      ))
    ),
    rows = list(id = out_rows_id, en = out_rows_en),
    note = list(
      id = paste0("Pencilan ditandai secara non-destruktif dan tidak pernah dihapus tanpa persetujuan eksplisit pengguna.", excl_note_id),
      en = paste0("Outliers are flagged non-destructively and never removed without explicit user selection.", excl_note_en)
    )
  )

  list(tb_desc, tb_norm, tb_hom, tb_out)
}

.sl_build_descriptive_formulas <- function() {
  list(
    list(
      label = list(
        id = "Rerata Sampel, Simpangan Baku & Interval Kepercayaan",
        en = "Sample Mean, Standard Deviation & Confidence Interval"
      ),
      latex = "\\bar{x} = \\frac{1}{n}\\sum_{i=1}^{n} x_i, \\quad s = \\sqrt{\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i - \\bar{x})^2}, \\quad \\text{CI}_{1-\\alpha} = \\bar{x} \\pm t_{1-\\alpha/2,\\,n-1}\\frac{s}{\\sqrt{n}}"
    ),
    list(
      label = list(
        id = "Koefisien Keragaman (CV), Kemencengan (G1) & Kurtosis Ekses (G2)",
        en = "Coefficient of Variation (CV), Skewness (G1) & Excess Kurtosis (G2)"
      ),
      latex = "\\text{CV}(\\%) = \\frac{s}{|\\bar{x}|}\\times 100, \\quad G_1 = \\frac{n}{(n-1)(n-2)}\\sum_{i=1}^{n}\\left(\\frac{x_i-\\bar{x}}{s}\\right)^3, \\quad G_2 = \\frac{n(n+1)}{(n-1)(n-2)(n-3)}\\sum_{i=1}^{n}\\left(\\frac{x_i-\\bar{x}}{s}\\right)^4 - \\frac{3(n-1)^2}{(n-2)(n-3)}"
    ),
    list(
      label = list(
        id = "Statistik Uji Normalitas Shapiro-Wilk",
        en = "Shapiro-Wilk Normality Statistic"
      ),
      latex = "W = \\frac{\\left(\\sum_{i=1}^{n} a_i x_{(i)}\\right)^2}{\\sum_{i=1}^{n}(x_i - \\bar{x})^2}"
    ),
    list(
      label = list(
        id = "Uji Homogenitas Varians Levene (Brown-Forsythe Berbasis Median)",
        en = "Brown-Forsythe (Median-Centered Levene) Homogeneity Statistic"
      ),
      latex = "F_{\\text{BF}} = \\frac{(N-k)\\sum_{i=1}^{k} n_i(\\bar{z}_{i\\cdot} - \\bar{z}_{\\cdot\\cdot})^2}{(k-1)\\sum_{i=1}^{k}\\sum_{j=1}^{n_i}(z_{ij} - \\bar{z}_{i\\cdot})^2}, \\quad z_{ij} = |y_{ij} - \\tilde{y}_i|"
    )
  )
}

.sl_flagged_outlier_mask <- function(prep, outliers) {
  n_obs <- length(prep$values)
  if (n_obs == 0L) {
    return(logical(0L))
  }
  fl <- if (!is.null(outliers) && is.data.frame(outliers$flagged)) {
    outliers$flagged
  } else {
    NULL
  }
  if (is.null(fl) || nrow(fl) == 0L) {
    return(rep(FALSE, n_obs))
  }
  sl_match_outlier_mask(
    groups = prep$groups,
    source_rows = prep$source_rows,
    outlier_rows = fl$source_row,
    outlier_groups = fl$group
  )
}

.sl_build_descriptive_plots <- function(prep, outliers, grp_levels, resp_label, lang = "id") {
  k <- length(grp_levels)
  flagged_mask <- .sl_flagged_outlier_mask(prep, outliers)
  flagged_rows <- if (!is.null(outliers$flagged)) outliers$flagged$source_row else integer(0L)
  flagged_groups <- if (!is.null(outliers$flagged)) outliers$flagged$group else character(0L)
  g_chr <- as.character(prep$groups)

  if (k <= 1L) {
    mf_grid <- c(1L, 1L)
    panel_w <- 6.5
    panel_h <- 4.2
  } else if (k == 2L) {
    mf_grid <- c(1L, 2L)
    panel_w <- 7.0
    panel_h <- 3.9
  } else if (k == 3L) {
    mf_grid <- c(1L, 3L)
    panel_w <- 7.5
    panel_h <- 3.6
  } else if (k == 4L) {
    mf_grid <- c(2L, 2L)
    panel_w <- 6.8
    panel_h <- 5.4
  } else {
    mf_grid <- c(2L, 3L)
    panel_w <- 7.5
    panel_h <- 5.2
  }

  is_en <- .sl_is_en(lang)

  # 1. Q-Q Plot
  qq_cap <- sl_capture_plot(function() {
    if (k <= 1L) {
      out_idx <- which(flagged_mask)
      t_str <- if (is_en) {
        sprintf("Normal Q-Q Plot (%s)", resp_label)
      } else {
        sprintf("Plot Q-Q Normalitas (%s)", resp_label)
      }
      sl_plot_qq(prep$values, title = t_str, lang = lang, outlier_idx = out_idx)
    } else if (k <= 6L) {
      graphics::par(mfrow = mf_grid)
      for (g_nm in grp_levels) {
        idx_g <- which(g_chr == g_nm)
        out_idx_g <- which(flagged_mask[idx_g])
        sl_plot_qq(
          prep$values[idx_g],
          title = paste0("Q-Q: ", g_nm),
          lang = lang,
          outlier_idx = out_idx_g
        )
      }
    } else {
      # Standardized within-group residuals for > 6 groups
      z_pooled <- numeric(length(prep$values))
      for (g_nm in grp_levels) {
        idx_g <- which(g_chr == g_nm)
        xg <- prep$values[idx_g]
        sg <- if (length(xg) >= 2L && max(xg) > min(xg)) stats::sd(xg) else 1.0
        z_pooled[idx_g] <- (xg - mean(xg)) / sg
      }
      out_idx <- which(flagged_mask)
      t_str <- if (is_en) "Pooled Standardized Q-Q Plot" else "Plot Q-Q Terstandardisasi Gabungan"
      sl_plot_qq(z_pooled, title = t_str, lang = lang, outlier_idx = out_idx)
    }
  }, width_in = if (k >= 2L && k <= 6L) panel_w else 6.5,
     height_in = if (k >= 2L && k <= 6L) panel_h else 4.2,
     res = 300)

  plt_qq <- list(
    id = "qq_plot",
    title = list(
      id = sprintf("Plot Kuantil-Kuantil (Q-Q) Normalitas \u2014 %s", resp_label),
      en = sprintf("Normal Quantile-Quantile (Q-Q) Plot \u2014 %s", resp_label)
    ),
    png_data_url = qq_cap$png_data_url,
    width_in = qq_cap$width_in,
    height_in = qq_cap$height_in
  )

  # 2. Boxplot + Jittered Observations
  box_cap <- sl_capture_plot(function() {
    t_str <- if (is_en) {
      sprintf("Distribution & Outliers (%s)", resp_label)
    } else {
      sprintf("Sebaran Data & Pencilan (%s)", resp_label)
    }
    sl_plot_box_jitter(
      values = prep$values,
      groups = prep$groups,
      title = t_str,
      ylab = resp_label,
      lang = lang,
      outlier_rows = flagged_rows,
      source_rows = prep$source_rows,
      outlier_groups = flagged_groups,
      outlier_mask = flagged_mask
    )
  }, width_in = 6.5, height_in = 4.2, res = 300)

  plt_box <- list(
    id = "box_jitter",
    title = list(
      id = sprintf("Boxplot & Sebaran Observasi \u2014 %s", resp_label),
      en = sprintf("Boxplot & Jittered Observations \u2014 %s", resp_label)
    ),
    png_data_url = box_cap$png_data_url,
    width_in = box_cap$width_in,
    height_in = box_cap$height_in
  )

  # 3. Histogram + Normal Density Curve
  hist_cap <- sl_capture_plot(function() {
    if (k <= 1L) {
      t_str <- if (is_en) {
        sprintf("Histogram & Normal Curve (%s)", resp_label)
      } else {
        sprintf("Histogram & Kurva Normal (%s)", resp_label)
      }
      sl_plot_hist_density(prep$values, title = t_str, xlab = resp_label, lang = lang)
    } else if (k <= 6L) {
      graphics::par(mfrow = mf_grid)
      for (g_nm in grp_levels) {
        idx_g <- which(g_chr == g_nm)
        sl_plot_hist_density(
          prep$values[idx_g],
          title = g_nm,
          xlab = resp_label,
          lang = lang
        )
      }
    } else {
      t_str <- if (is_en) {
        sprintf("Pooled Histogram (%s)", resp_label)
      } else {
        sprintf("Histogram Gabungan (%s)", resp_label)
      }
      sl_plot_hist_density(prep$values, title = t_str, xlab = resp_label, lang = lang)
    }
  }, width_in = if (k >= 2L && k <= 6L) panel_w else 6.5,
     height_in = if (k >= 2L && k <= 6L) panel_h else 4.2,
     res = 300)

  plt_hist <- list(
    id = "hist_density",
    title = list(
      id = sprintf("Histogram & Kurva Normal \u2014 %s", resp_label),
      en = sprintf("Histogram & Normal Density Curve \u2014 %s", resp_label)
    ),
    png_data_url = hist_cap$png_data_url,
    width_in = hist_cap$width_in,
    height_in = hist_cap$height_in
  )

  list(plt_qq, plt_box, plt_hist)
}

.sl_build_descriptive_warnings <- function(prep, assumptions, outliers, alpha) {
  warn_id <- character(0L)
  warn_en <- character(0L)

  # 1. Review Focus #3: Coerced non-numeric text cells MUST emit warning naming row indices
  if (!is.null(prep$coerced_non_numeric) && prep$coerced_non_numeric$count > 0L) {
    c_rows <- paste(unique(prep$coerced_non_numeric$rows), collapse = ", ")
    c_cols <- paste(unique(prep$coerced_non_numeric$cols), collapse = ", ")
    warn_id <- c(
      warn_id,
      sprintf(
        "Ditemukan %d sel teks non-numerik pada kolom '%s' (baris: %s) yang dikonversi menjadi NA.",
        prep$coerced_non_numeric$count, c_cols, c_rows
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Found %d unexpected non-numeric text cell(s) in column '%s' (row(s): %s); coerced to NA.",
        prep$coerced_non_numeric$count, c_cols, c_rows
      )
    )
  }

  # 2. Normality violation
  if (isTRUE(assumptions$normality_violated)) {
    viol_grps <- vapply(
      Filter(function(pg) identical(pg$is_normal, FALSE), assumptions$per_group),
      function(pg) pg$group,
      character(1L)
    )
    warn_id <- c(
      warn_id,
      sprintf(
        "Asumsi normalitas tidak terpenuhi pada \u03b1 = %s untuk kelompok: %s. Gunakan Median (IQR) dan uji non-parametrik.",
        .sl_fmt_alpha(alpha, "id"), paste(viol_grps, collapse = ", ")
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Normality assumption violated at \u03b1 = %s for group(s): %s. Prefer Median (IQR) and non-parametric tests.",
        .sl_fmt_alpha(alpha, "en"), paste(viol_grps, collapse = ", ")
      )
    )
  }

  # 3. Homogeneity violation
  if (isTRUE(assumptions$homogeneity_violated) && !is.null(assumptions$homogeneity)) {
    lev_p <- assumptions$homogeneity$levene_bf$p_value
    warn_id <- c(
      warn_id,
      sprintf(
        "Asumsi homogenitas varians tidak terpenuhi menurut uji Levene Brown-Forsythe (p = %s < \u03b1 = %s).",
        sl_fmt_p(lev_p, "id"), .sl_fmt_alpha(alpha, "id")
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Homogeneity of variance violated by Brown-Forsythe Levene test (p = %s < \u03b1 = %s).",
        sl_fmt_p(lev_p, "en"), .sl_fmt_alpha(alpha, "en")
      )
    )
  }

  # 4. Constant / zero-variance or tiny sample groups
  const_grps <- vapply(
    Filter(function(pg) identical(pg$shapiro$note_key, "zero_variance"), assumptions$per_group),
    function(pg) pg$group,
    character(1L)
  )
  if (length(const_grps) > 0L) {
    warn_id <- c(
      warn_id,
      sprintf(
        "Kelompok dengan varians nol (data konstan, SD = 0): %s; uji normalitas dan Grubbs tidak berlaku.",
        paste(const_grps, collapse = ", ")
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Zero-variance (constant data, SD = 0) in group(s): %s; normality and Grubbs tests are not applicable.",
        paste(const_grps, collapse = ", ")
      )
    )
  }

  small_grps <- vapply(
    Filter(function(pg) pg$n > 0L && pg$n < 3L, assumptions$per_group),
    function(pg) pg$group,
    character(1L)
  )
  if (length(small_grps) > 0L) {
    warn_id <- c(
      warn_id,
      sprintf(
        "Ukuran sampel kecil (n < 3) pada kelompok: %s; uji Shapiro-Wilk dan kemencengan tidak dapat dihitung.",
        paste(small_grps, collapse = ", ")
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Small sample size (n < 3) in group(s): %s; Shapiro-Wilk and skewness cannot be computed.",
        paste(small_grps, collapse = ", ")
      )
    )
  }

  # 5. Flagged or user-excluded outliers
  if (!is.null(outliers$flagged) && nrow(outliers$flagged) > 0L) {
    f_rows <- paste(outliers$flagged$source_row, collapse = ", ")
    warn_id <- c(
      warn_id,
      sprintf(
        "Terdeteksi %d kandidat pencilan (baris: %s); data tetap dipertahankan secara non-destruktif.",
        nrow(outliers$flagged), f_rows
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "Detected %d candidate outlier(s) (row(s): %s); observations retained non-destructively.",
        nrow(outliers$flagged), f_rows
      )
    )
  }

  if (!is.null(prep$excluded_outliers) && nrow(prep$excluded_outliers) > 0L) {
    e_rows <- paste(prep$excluded_outliers$source_row, collapse = ", ")
    warn_id <- c(
      warn_id,
      sprintf(
        "%d observasi dikeluarkan secara eksplisit oleh pengguna (baris: %s).",
        nrow(prep$excluded_outliers), e_rows
      )
    )
    warn_en <- c(
      warn_en,
      sprintf(
        "%d observation(s) explicitly excluded by user (row(s): %s).",
        nrow(prep$excluded_outliers), e_rows
      )
    )
  }

  list(
    id = sl_box_vec(enc2utf8(warn_id)),
    en = sl_box_vec(enc2utf8(warn_en))
  )
}

#' Run Descriptive Biostatistics & Mandatory Assumption Analysis
#'
#' @param df `data.frame` or named `list` of column vectors
#' @param opts Named list containing `alpha`, `exclude_rows`, `include_plots`, `lang`,
#'   and `roles` (`mode`, `response_cols`, `group_col`) either nested in `opts$roles` or at top-level
#' @return Full StatLab structured result contract list
sl_analyze_descriptive <- function(df, opts = list()) {
  df <- .sl_as_dataframe(df)
  if (is.null(opts)) {
    opts <- list()
  }

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else opts
  mode <- if (!is.null(roles$mode) && length(roles$mode) > 0L) {
    as.character(roles$mode[[1L]])
  } else {
    "wide"
  }

  response_cols <- if (!is.null(roles$response_cols) && length(roles$response_cols) > 0L) {
    as.character(unlist(roles$response_cols, use.names = FALSE))
  } else {
    names(df)
  }

  group_col <- if (!is.null(roles$group_col) && length(roles$group_col) > 0L &&
                   !is.na(roles$group_col[[1L]]) && nzchar(as.character(roles$group_col[[1L]]))) {
    as.character(roles$group_col[[1L]])
  } else {
    NULL
  }

  alpha <- if (!is.null(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1.")
  }

  exclude_rows <- if (!is.null(opts$exclude_rows) && length(opts$exclude_rows) > 0L) {
    as.integer(unlist(opts$exclude_rows, use.names = FALSE))
  } else {
    integer(0L)
  }

  include_plots <- if (!is.null(opts$include_plots) && length(opts$include_plots) > 0L) {
    !identical(as.logical(opts$include_plots[[1L]]), FALSE)
  } else {
    TRUE
  }

  plot_lang <- if (!is.null(opts$lang) && length(opts$lang) > 0L) {
    as.character(opts$lang[[1L]])
  } else {
    "id"
  }

  prep <- sl_prepare_groups(
    df = df,
    mode = mode,
    response_cols = response_cols,
    group_col = group_col,
    exclude_rows = exclude_rows
  )

  if (prep$n_valid == 0L) {
    stop("No valid numeric observations found in the selected response column(s).")
  }

  grp_levels <- levels(prep$groups)
  resp_label <- if (mode == "long") {
    response_cols[1L]
  } else if (length(response_cols) == 1L) {
    response_cols[1L]
  } else {
    paste(response_cols, collapse = ", ")
  }

  miss_by_grp <- .sl_count_missing_per_group(df, mode, response_cols, group_col, grp_levels)

  assumptions <- sl_check_assumptions(
    values = prep$values,
    groups = prep$groups,
    residuals = NULL,
    alpha = alpha,
    require_homogeneity = (length(grp_levels) >= 2L)
  )

  outliers <- sl_detect_outliers(
    values = prep$values,
    groups = prep$groups,
    source_rows = prep$source_rows,
    alpha = alpha
  )
  outliers$excluded <- prep$excluded_outliers

  g_chr <- as.character(prep$groups)
  main_groups <- vector("list", length(grp_levels))
  for (i in seq_along(grp_levels)) {
    g_nm <- grp_levels[i]
    idx_g <- which(g_chr == g_nm)
    main_groups[[i]] <- .sl_compute_group_stats(
      x = prep$values[idx_g],
      group_name = g_nm,
      n_missing = miss_by_grp[[g_nm]],
      alpha = alpha
    )
  }

  meta <- list(
    analysis_type = "descriptive",
    analysis_label = list(
      id = "Statistik Deskriptif & Uji Asumsi",
      en = "Descriptive Statistics & Assumption Tests"
    ),
    response_label = resp_label,
    mode = mode,
    alpha = alpha,
    n_raw = as.integer(prep$n_raw_obs),
    n_valid = as.integer(prep$n_valid),
    n_missing = as.integer(prep$n_missing),
    n_excluded_outliers = as.integer(nrow(prep$excluded_outliers)),
    excluded_rows = sl_box_vec(as.integer(prep$excluded_outliers$source_row)),
    group_names = sl_box_vec(as.character(grp_levels)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  tables_apa <- .sl_build_apa_tables(main_groups, assumptions, outliers, resp_label, alpha)
  tables_full <- .sl_build_full_tables(main_groups, assumptions, outliers, prep$excluded_outliers, alpha)
  formulas <- .sl_build_descriptive_formulas()

  plots <- if (include_plots) {
    .sl_build_descriptive_plots(prep, outliers, grp_levels, resp_label, lang = plot_lang)
  } else {
    sl_box_vec(list())
  }

  interpretation <- sl_build_descriptive_conclusion(
    meta = meta,
    main_groups = main_groups,
    assumptions = assumptions,
    outliers = outliers,
    excluded_outliers = prep$excluded_outliers
  )

  warnings <- .sl_build_descriptive_warnings(prep, assumptions, outliers, alpha)

  list(
    meta = meta,
    assumptions = assumptions,
    outliers = outliers,
    main = list(
      conf_level = 1.0 - alpha,
      groups = main_groups
    ),
    posthoc = NULL,
    effect_size = NULL,
    alternatives = sl_box_vec(list()),
    tables_apa = tables_apa,
    tables_full = tables_full,
    formulas = formulas,
    plots = plots,
    interpretation = interpretation,
    warnings = warnings
  )
}
