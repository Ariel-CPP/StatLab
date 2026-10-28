# R/05_conclusions.R
# StatLab Rule-Based Bilingual (ID/EN) Conclusions & Cumulative General Conclusion Compiler
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base R functions + sl_fmt_num / sl_fmt_p from R/00_json.R (zero CRAN packages).

.sl_fmt_alpha <- function(alpha, lang = "id") {
  dig <- if (abs(alpha * 100 - round(alpha * 100)) < 1e-9) 2L else 3L
  sl_fmt_num(alpha, digits = dig, lang = lang)
}

.sl_fmt_conf_pct <- function(alpha, lang = "id") {
  pct <- (1.0 - alpha) * 100.0
  if (abs(pct - round(pct)) < 1e-9) {
    paste0(as.integer(round(pct)), "%")
  } else {
    paste0(sl_fmt_num(pct, digits = 1L, lang = lang), "%")
  }
}

.sl_choose_summary_digits <- function(main_groups) {
  means <- vapply(
    main_groups,
    function(g) if (!is.null(g$mean) && is.finite(g$mean)) abs(g$mean) else NA_real_,
    numeric(1L)
  )
  sds <- vapply(
    main_groups,
    function(g) if (!is.null(g$sd) && is.finite(g$sd)) abs(g$sd) else NA_real_,
    numeric(1L)
  )
  ref_vals <- c(
    means[is.finite(means) & means > 0],
    sds[is.finite(sds) & sds > 0]
  )
  if (length(ref_vals) == 0L) {
    return(2L)
  }
  scale_min <- min(ref_vals)
  if (scale_min < 0.05) {
    4L
  } else if (scale_min < 0.5) {
    3L
  } else {
    2L
  }
}

.sl_note_text <- function(note_key, lang = "id") {
  is_en <- identical(tolower(as.character(lang[1L])), "en")
  if (is.null(note_key) || length(note_key) == 0L || is.na(note_key[1L]) || !nzchar(note_key[1L])) {
    return("")
  }
  key <- as.character(note_key[1L])
  switch(
    key,
    empty_group = if (is_en) "empty group (n = 0)" else "kelompok kosong (n = 0)",
    n_lt_3 = if (is_en) "sample size n < 3" else "ukuran sampel n < 3",
    n_lt_5 = if (is_en) "sample size n < 5" else "ukuran sampel n < 5",
    n_gt_5000 = if (is_en) "n > 5000 (evaluated via Lilliefors)" else "n > 5000 (dievaluasi dengan Lilliefors)",
    zero_variance = if (is_en) "zero variance (constant data)" else "varians nol (data konstan)",
    lt_2_groups = if (is_en) "fewer than 2 groups" else "kurang dari 2 kelompok",
    group_n_lt_2 = if (is_en) "at least one group has n < 2" else "terdapat kelompok dengan n < 2",
    insufficient_df = if (is_en) "insufficient residual df" else "derajat bebas galat tidak mencukupi",
    lower_tail_no_outlier = if (is_en) "inlier distribution (no extreme tail)" else "sebaran seragam (tanpa ekor ekstrem)",
    computation_failed = if (is_en) "numerical computation not applicable" else "komputasi numerik tidak berlaku",
    key
  )
}

#' Build rule-based bilingual interpretation paragraph for Descriptive Analysis
#'
#' @param meta Result `meta` list
#' @param main_groups Unnamed list of per-group descriptive objects
#' @param assumptions Output of `sl_check_assumptions`
#' @param outliers Output of `sl_detect_outliers`
#' @param excluded_outliers Optional `data.frame` of user-excluded rows
#' @return Named list `list(id = "...", en = "...")`
sl_build_descriptive_conclusion <- function(
  meta,
  main_groups,
  assumptions,
  outliers,
  excluded_outliers = NULL
) {
  alpha <- as.double(meta$alpha)
  alpha_id <- .sl_fmt_alpha(alpha, "id")
  alpha_en <- .sl_fmt_alpha(alpha, "en")
  ci_pct_id <- .sl_fmt_conf_pct(alpha, "id")
  ci_pct_en <- .sl_fmt_conf_pct(alpha, "en")
  dig <- .sl_choose_summary_digits(main_groups)

  k <- length(main_groups)
  grp_names <- vapply(main_groups, function(g) as.character(g$group), character(1L))
  n_excl <- if (!is.null(excluded_outliers) && is.data.frame(excluded_outliers)) {
    nrow(excluded_outliers)
  } else {
    as.integer(meta$n_excluded_outliers)
  }

  # 1. Sample & listwise audit sentence
  audit_extra_id <- character(0L)
  audit_extra_en <- character(0L)
  if (meta$n_missing > 0L) {
    audit_extra_id <- c(
      audit_extra_id,
      sprintf("%d data kosong/hilang dikeluarkan secara listwise", meta$n_missing)
    )
    audit_extra_en <- c(
      audit_extra_en,
      sprintf("%d missing value(s) omitted listwise", meta$n_missing)
    )
  }
  if (n_excl > 0L) {
    excl_rows_txt <- if (!is.null(excluded_outliers) && nrow(excluded_outliers) > 0L) {
      paste(excluded_outliers$source_row, collapse = ", ")
    } else {
      ""
    }
    audit_extra_id <- c(
      audit_extra_id,
      if (nzchar(excl_rows_txt)) {
        sprintf("%d observasi dikeluarkan secara eksplisit oleh pengguna (baris: %s)", n_excl, excl_rows_txt)
      } else {
        sprintf("%d observasi dikeluarkan secara eksplisit oleh pengguna", n_excl)
      }
    )
    audit_extra_en <- c(
      audit_extra_en,
      if (nzchar(excl_rows_txt)) {
        sprintf("%d observation(s) explicitly excluded by user (row(s): %s)", n_excl, excl_rows_txt)
      } else {
        sprintf("%d observation(s) explicitly excluded by user", n_excl)
      }
    )
  }

  suffix_id <- if (length(audit_extra_id) > 0L) {
    paste0(" (", paste(audit_extra_id, collapse = "; "), ")")
  } else {
    ""
  }
  suffix_en <- if (length(audit_extra_en) > 0L) {
    paste0(" (", paste(audit_extra_en, collapse = "; "), ")")
  } else {
    ""
  }

  s1_id <- sprintf(
    "Analisis statistik deskriptif dilakukan terhadap %d observasi valid pada %d kelompok (%s)%s.",
    meta$n_valid, k, paste(grp_names, collapse = ", "), suffix_id
  )
  s1_en <- sprintf(
    "Descriptive statistical analysis was performed on %d valid observation(s) across %d group(s) (%s)%s.",
    meta$n_valid, k, paste(grp_names, collapse = ", "), suffix_en
  )

  # 2. Normality per group sentence
  norm_parts_id <- character(length(assumptions$per_group))
  norm_parts_en <- character(length(assumptions$per_group))
  for (i in seq_along(assumptions$per_group)) {
    pg <- assumptions$per_group[[i]]
    sw <- pg$shapiro
    lf <- pg$lilliefors
    if (isTRUE(sw$valid)) {
      st_id <- if (isTRUE(pg$is_normal)) "berdistribusi normal" else "tidak berdistribusi normal"
      st_en <- if (isTRUE(pg$is_normal)) "normally distributed" else "non-normal"
      norm_parts_id[i] <- sprintf(
        "%s (W = %s, p = %s; %s)",
        pg$group, sl_fmt_num(sw$statistic, 4L, "id"), sl_fmt_p(sw$p_value, "id"), st_id
      )
      norm_parts_en[i] <- sprintf(
        "%s (W = %s, p = %s; %s)",
        pg$group, sl_fmt_num(sw$statistic, 4L, "en"), sl_fmt_p(sw$p_value, "en"), st_en
      )
    } else if (identical(sw$note_key, "n_gt_5000") && isTRUE(lf$valid)) {
      st_id <- if (isTRUE(pg$is_normal)) "berdistribusi normal" else "tidak berdistribusi normal"
      st_en <- if (isTRUE(pg$is_normal)) "normally distributed" else "non-normal"
      norm_parts_id[i] <- sprintf(
        "%s (Lilliefors D = %s, p = %s; %s)",
        pg$group, sl_fmt_num(lf$statistic, 4L, "id"), sl_fmt_p(lf$p_value, "id"), st_id
      )
      norm_parts_en[i] <- sprintf(
        "%s (Lilliefors D = %s, p = %s; %s)",
        pg$group, sl_fmt_num(lf$statistic, 4L, "en"), sl_fmt_p(lf$p_value, "en"), st_en
      )
    } else {
      nt_id <- .sl_note_text(sw$note_key, "id")
      nt_en <- .sl_note_text(sw$note_key, "en")
      norm_parts_id[i] <- sprintf("%s (uji normalitas tidak berlaku: %s)", pg$group, nt_id)
      norm_parts_en[i] <- sprintf("%s (normality test not applicable: %s)", pg$group, nt_en)
    }
  }
  s2_id <- sprintf(
    "Hasil uji normalitas Shapiro-Wilk pada taraf signifikansi \u03b1 = %s menunjukkan: %s.",
    alpha_id, paste(norm_parts_id, collapse = "; ")
  )
  s2_en <- sprintf(
    "Shapiro-Wilk normality testing at \u03b1 = %s indicated: %s.",
    alpha_en, paste(norm_parts_en, collapse = "; ")
  )

  # 3. Homogeneity sentence (when k >= 2)
  s3_id <- ""
  s3_en <- ""
  if (k >= 2L && !is.null(assumptions$homogeneity)) {
    lev <- assumptions$homogeneity$levene_bf
    if (isTRUE(lev$valid)) {
      hom_id <- if (isTRUE(assumptions$homogeneity$is_homogeneous)) {
        "homogen"
      } else {
        "tidak homogen (heteroskedastis)"
      }
      hom_en <- if (isTRUE(assumptions$homogeneity$is_homogeneous)) {
        "homogeneous"
      } else {
        "heterogeneous"
      }
      s3_id <- sprintf(
        "Uji homogenitas varians Levene (Brown-Forsythe berbasis median) menghasilkan F(%d, %d) = %s, p = %s, sehingga varians antarkelompok dinyatakan %s.",
        lev$df1, lev$df2, sl_fmt_num(lev$statistic, 4L, "id"), sl_fmt_p(lev$p_value, "id"), hom_id
      )
      s3_en <- sprintf(
        "The Brown-Forsythe (median-centered Levene) homogeneity of variance test yielded F(%d, %d) = %s, p = %s, indicating %s variances across groups.",
        lev$df1, lev$df2, sl_fmt_num(lev$statistic, 4L, "en"), sl_fmt_p(lev$p_value, "en"), hom_en
      )
    } else {
      nt_id <- .sl_note_text(lev$note_key, "id")
      nt_en <- .sl_note_text(lev$note_key, "en")
      s3_id <- sprintf("Uji homogenitas varians Levene tidak dapat dihitung (%s).", nt_id)
      s3_en <- sprintf("The Levene homogeneity of variance test was not applicable (%s).", nt_en)
    }
  }

  # 4. Primary descriptive summary per group (Parametric Mean +- SD vs Non-parametric Median [IQR])
  desc_parts_id <- character(k)
  desc_parts_en <- character(k)
  for (i in seq_len(k)) {
    g <- main_groups[[i]]
    pg <- if (i <= length(assumptions$per_group)) assumptions$per_group[[i]] else list(is_normal = NA)

    if (g$n == 0L) {
      desc_parts_id[i] <- sprintf("Kelompok %s tidak memiliki data valid", g$group)
      desc_parts_en[i] <- sprintf("Group %s had no valid observations", g$group)
    } else if (g$n == 1L) {
      desc_parts_id[i] <- sprintf(
        "Kelompok %s hanya memiliki 1 observasi (%s)",
        g$group, sl_fmt_num(g$mean, dig, "id")
      )
      desc_parts_en[i] <- sprintf(
        "Group %s contained a single observation (%s)",
        g$group, sl_fmt_num(g$mean, dig, "en")
      )
    } else if (is.finite(g$sd) && g$sd == 0) {
      desc_parts_id[i] <- sprintf(
        "Kelompok %s (n = %d) bernilai konstan pada %s (SD = %s)",
        g$group, g$n, sl_fmt_num(g$mean, dig, "id"), sl_fmt_num(0, dig, "id")
      )
      desc_parts_en[i] <- sprintf(
        "Group %s (n = %d) was constant at %s (SD = %s)",
        g$group, g$n, sl_fmt_num(g$mean, dig, "en"), sl_fmt_num(0, dig, "en")
      )
    } else if (identical(pg$is_normal, FALSE)) {
      desc_parts_id[i] <- sprintf(
        "Kelompok %s (n = %d) menggunakan ukuran non-parametrik utama Median [IQR] = %s [%s] karena tidak normal (rentang %s\u2013%s; Rerata \u00b1 SD = %s \u00b1 %s)",
        g$group, g$n,
        sl_fmt_num(g$median, dig, "id"), sl_fmt_num(g$iqr, dig, "id"),
        sl_fmt_num(g$min, dig, "id"), sl_fmt_num(g$max, dig, "id"),
        sl_fmt_num(g$mean, dig, "id"), sl_fmt_num(g$sd, dig, "id")
      )
      desc_parts_en[i] <- sprintf(
        "Group %s (n = %d) uses non-parametric Median [IQR] = %s [%s] as primary summary due to non-normality (range %s\u2013%s; Mean \u00b1 SD = %s \u00b1 %s)",
        g$group, g$n,
        sl_fmt_num(g$median, dig, "en"), sl_fmt_num(g$iqr, dig, "en"),
        sl_fmt_num(g$min, dig, "en"), sl_fmt_num(g$max, dig, "en"),
        sl_fmt_num(g$mean, dig, "en"), sl_fmt_num(g$sd, dig, "en")
      )
    } else {
      desc_parts_id[i] <- sprintf(
        "Kelompok %s (n = %d) memiliki ukuran parametrik utama Rerata \u00b1 SD = %s \u00b1 %s (IK %s: [%s; %s]; Median [IQR] = %s [%s])",
        g$group, g$n,
        sl_fmt_num(g$mean, dig, "id"), sl_fmt_num(g$sd, dig, "id"),
        ci_pct_id, sl_fmt_num(g$ci_low, dig, "id"), sl_fmt_num(g$ci_high, dig, "id"),
        sl_fmt_num(g$median, dig, "id"), sl_fmt_num(g$iqr, dig, "id")
      )
      desc_parts_en[i] <- sprintf(
        "Group %s (n = %d) had a primary parametric Mean \u00b1 SD = %s \u00b1 %s (%s CI: [%s, %s]; Median [IQR] = %s [%s])",
        g$group, g$n,
        sl_fmt_num(g$mean, dig, "en"), sl_fmt_num(g$sd, dig, "en"),
        ci_pct_en, sl_fmt_num(g$ci_low, dig, "en"), sl_fmt_num(g$ci_high, dig, "en"),
        sl_fmt_num(g$median, dig, "en"), sl_fmt_num(g$iqr, dig, "en")
      )
    }
  }
  s4_id <- paste0(paste(desc_parts_id, collapse = ". "), ".")
  s4_en <- paste0(paste(desc_parts_en, collapse = ". "), ".")

  # 5. Outlier audit sentence (Note: 1,5xIQR in ID vs 1.5xIQR in EN!)
  n_flag <- if (!is.null(outliers$flagged) && is.data.frame(outliers$flagged)) {
    nrow(outliers$flagged)
  } else {
    0L
  }
  if (n_flag > 0L) {
    fl <- outliers$flagged
    items_id <- vapply(
      seq_len(n_flag),
      function(r) sprintf("%s baris #%d (%s)", fl$group[r], fl$source_row[r], sl_fmt_num(fl$value[r], dig, "id")),
      character(1L)
    )
    items_en <- vapply(
      seq_len(n_flag),
      function(r) sprintf("%s row #%d (%s)", fl$group[r], fl$source_row[r], sl_fmt_num(fl$value[r], dig, "en")),
      character(1L)
    )
    s5_id <- sprintf(
      " Pemeriksaan pencilan (pagar 1,5\u00d7IQR dan uji Grubbs dua arah) menandai %d observasi (%s); seluruh data tetap dipertahankan secara non-destruktif.",
      n_flag, paste(items_id, collapse = ", ")
    )
    s5_en <- sprintf(
      " Outlier screening (1.5\u00d7IQR fences and two-sided Grubbs test) flagged %d observation(s) (%s); all observations were retained non-destructively.",
      n_flag, paste(items_en, collapse = ", ")
    )
  } else {
    s5_id <- " Pemeriksaan pencilan (pagar 1,5\u00d7IQR dan uji Grubbs dua arah) tidak mendeteksi pencilan ekstrem."
    s5_en <- " Outlier screening (1.5\u00d7IQR fences and two-sided Grubbs test) detected no statistical outliers."
  }

  parts_id <- c(s1_id, s2_id, if (nzchar(s3_id)) s3_id else NULL, s4_id)
  parts_en <- c(s1_en, s2_en, if (nzchar(s3_en)) s3_en else NULL, s4_en)

  list(
    id = enc2utf8(paste0(paste(parts_id, collapse = " "), s5_id)),
    en = enc2utf8(paste0(paste(parts_en, collapse = " "), s5_en))
  )
}

.sl_extract_str <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(default)
  }
  val <- trimws(as.character(x[[1L]]))
  if (nzchar(val)) val else default
}

#' Compile a cumulative bilingual General Conclusion across all Session Report items
#'
#' Honors Spec §6.2: when an item has `assumption_violated == TRUE` and
#' `effective_test != chosen_test`, explicitly states that inference follows
#' `effective_test` due to assumption violation while noting `chosen_test`
#' was also executed. Always returns both `id` and `en` texts.
#'
#' @param items List of session report item summaries
#' @param lang Character vector of requested language code(s) (`c("id", "en")`)
#' @return Named list `list(id = "...", en = "...")`
sl_compile_general_conclusion <- function(items, lang = c("id", "en")) {
  if (!is.null(lang) && length(lang) > 0L) {
    valid_langs <- tolower(as.character(lang)) %in% c("id", "en")
    if (!all(valid_langs)) {
      stop("`lang` must contain only \"id\" and/or \"en\".")
    }
  }

  if (is.null(items) || length(items) == 0L) {
    return(list(
      id = enc2utf8("Belum ada analisis yang ditambahkan ke dalam Laporan Sesi."),
      en = enc2utf8("No analyses have been added to the Session Report yet.")
    ))
  }

  m <- length(items)
  n_viol <- 0L
  lines_id <- character(m)
  lines_en <- character(m)

  for (i in seq_len(m)) {
    it <- items[[i]]
    title_id <- .sl_extract_str(it$title, sprintf("Analisis %d", i))
    title_en <- .sl_extract_str(it$title, sprintf("Analysis %d", i))
    chosen   <- .sl_extract_str(it$chosen_test, .sl_extract_str(it$analysis_type, "Statistik Deskriptif"))
    effective <- .sl_extract_str(it$effective_test, chosen)
    violated <- isTRUE(as.logical(if (!is.null(it$assumption_violated) && length(it$assumption_violated) > 0L) {
      it$assumption_violated[[1L]]
    } else {
      FALSE
    }))
    if (violated) {
      n_viol <- n_viol + 1L
    }

    sum_id <- .sl_extract_str(
      it$summary_id,
      if (!is.null(it$interpretation)) .sl_extract_str(it$interpretation$id, "") else ""
    )
    sum_en <- .sl_extract_str(
      it$summary_en,
      if (!is.null(it$interpretation)) .sl_extract_str(it$interpretation$en, "") else ""
    )

    if (violated && !identical(effective, chosen)) {
      method_id <- sprintf(
        "[Asumsi dilanggar \u2192 kesimpulan utama mengikuti %s; uji pilihan pengguna %s tetap dijalankan sebagai pembanding]",
        effective, chosen
      )
      method_en <- sprintf(
        "[Assumption violated \u2192 primary conclusion follows %s; user-selected %s was also executed for comparison]",
        effective, chosen
      )
    } else if (violated) {
      method_id <- sprintf("[Catatan asumsi pada %s: sebagian asumsi parametrik tidak terpenuhi]", effective)
      method_en <- sprintf("[Assumption note on %s: one or more parametric assumptions were violated]", effective)
    } else {
      method_id <- sprintf("[Metode: %s \u2014 asumsi terpenuhi]", effective)
      method_en <- sprintf("[Method: %s \u2014 assumptions satisfied]", effective)
    }

    lines_id[i] <- trimws(sprintf("%d) %s %s: %s", i, title_id, method_id, sum_id))
    lines_en[i] <- trimws(sprintf("%d) %s %s: %s", i, title_en, method_en, sum_en))
  }

  intro_id <- sprintf(
    "Kesimpulan Umum merangkum %d analisis dalam Laporan Sesi (%d memenuhi asumsi parametrik, %d memiliki catatan pelanggaran asumsi):",
    m, m - n_viol, n_viol
  )
  intro_en <- sprintf(
    "This General Conclusion synthesizes %d analysis item(s) in the Session Report (%d met parametric assumptions, %d flagged assumption violation(s)):",
    m, m - n_viol, n_viol
  )

  outro_id <- if (n_viol > 0L) {
    "Secara keseluruhan, interpretasi ilmiah telah disesuaikan dengan uji yang sahih menurut pemenuhan asumsi distribusi dan kehomogenan varians pada masing-masing variabel."
  } else {
    "Secara keseluruhan, seluruh analisis dalam sesi ini memenuhi prasyarat statistik sehingga estimasi parametrik dapat digunakan secara langsung."
  }
  outro_en <- if (n_viol > 0L) {
    "Overall, statistical inferences were aligned with assumption-valid methods accounting for distributional normality and variance homogeneity across variables."
  } else {
    "Overall, all analyses in this session satisfied statistical prerequisites, supporting direct parametric interpretation."
  }

  list(
    id = enc2utf8(paste(c(intro_id, lines_id, outro_id), collapse = "\n\n")),
    en = enc2utf8(paste(c(intro_en, lines_en, outro_en), collapse = "\n\n"))
  )
}
