# R/10_nonparametric.R
# StatLab Non-Parametric Engine (pure Base R)
# Kruskal-Wallis, Mann-Whitney U, Wilcoxon Signed-Rank, and Friedman Tests
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

#' Kruskal-Wallis Test (Non-Parametric One-Way ANOVA)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_kruskal_wallis <- function(df, opts = list()) {
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
      response_cols <- if (length(num_cols) >= 2L) num_cols else names(df)
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
  if (length(grp_levels) < 2L) {
    stop("Uji Kruskal-Wallis membutuhkan minimal 2 kelompok perlakuan.")
  }

  resp_label <- if (mode == "long") response_cols[1L] else "Nilai Pengamatan"

  # Assumptions & Outliers audit
  assumptions <- sl_check_assumptions(
    values = prep$values,
    groups = prep$groups,
    alpha = alpha,
    require_homogeneity = TRUE
  )
  outliers <- sl_detect_outliers(prep$values, prep$groups, prep$source_rows, alpha = alpha)

  # Kruskal-Wallis Calculation
  kt <- stats::kruskal.test(prep$values ~ prep$groups)
  h_stat <- as.double(kt$statistic)
  df_val <- as.integer(kt$parameter)
  p_val  <- as.double(kt$p.value)

  # Descriptive ranks per group
  all_ranks <- rank(prep$values)
  k_grps <- length(grp_levels)
  n_total <- length(prep$values)

  grp_stats <- list()
  for (i in seq_len(k_grps)) {
    g_nm <- grp_levels[i]
    g_vals <- prep$values[prep$groups == g_nm]
    g_rks  <- all_ranks[prep$groups == g_nm]
    n_g    <- length(g_vals)
    med_g  <- stats::median(g_vals)
    q1_g   <- stats::quantile(g_vals, 0.25, names = FALSE)
    q3_g   <- stats::quantile(g_vals, 0.75, names = FALSE)
    mr_g   <- mean(g_rks)
    sum_rk <- sum(g_rks)

    grp_stats[[i]] <- list(
      group = g_nm,
      n = n_g,
      median = med_g,
      q1 = q1_g,
      q3 = q3_g,
      mean_rank = mr_g,
      sum_rank = sum_rk
    )
  }

  # Pairwise Post-Hoc Comparisons (Mann-Whitney with Bonferroni correction)
  m_comparisons <- (k_grps * (k_grps - 1L)) / 2L
  pw_list <- list()
  pw_counter <- 1L

  for (i in 1L:(k_grps - 1L)) {
    for (j in (i + 1L):k_grps) {
      g1 <- grp_levels[i]
      g2 <- grp_levels[j]
      v1 <- prep$values[prep$groups == g1]
      v2 <- prep$values[prep$groups == g2]
      n1 <- length(v1)
      n2 <- length(v2)

      wt <- tryCatch(stats::wilcox.test(v1, v2, exact = FALSE), error = function(e) NULL)
      if (!is.null(wt)) {
        w_val <- as.double(wt$statistic)
        p_raw <- as.double(wt$p.value)
        e_w   <- (n1 * (n1 + n2 + 1L)) / 2.0
        sd_w  <- sqrt((n1 * n2 * (n1 + n2 + 1L)) / 12.0)
        z_val <- if (sd_w > 0) (w_val - e_w) / sd_w else 0.0
      } else {
        w_val <- NA_real_
        p_raw <- NA_real_
        z_val <- NA_real_
      }

      p_adj <- if (is.finite(p_raw)) min(1.0, p_raw * m_comparisons) else NA_real_
      sig_txt <- if (is.finite(p_adj) && p_adj < 0.001) "***" else if (is.finite(p_adj) && p_adj < 0.01) "**" else if (is.finite(p_adj) && p_adj < 0.05) "*" else "tn"

      pw_list[[pw_counter]] <- list(
        pair = sprintf("%s vs %s", g1, g2),
        w = w_val,
        z = z_val,
        p_raw = p_raw,
        p_adj = p_adj,
        sig = sig_txt
      )
      pw_counter <- pw_counter + 1L
    }
  }

  # Build Table 1: Kruskal-Wallis Summary
  hdr_t1_id <- sl_box_vec(c("Kelompok Perlakuan", "N", "Median", "Q1", "Q3", "Rataan Peringkat (Mean Rank)", "Kruskal-Wallis H", "db", "P-Value", "Signifikansi"))
  hdr_t1_en <- sl_box_vec(c("Treatment Group", "N", "Median", "Q1", "Q3", "Mean Rank", "Kruskal-Wallis H", "df", "p-value", "Significance"))

  rows_t1_id <- list()
  rows_t1_en <- list()
  sig_kw <- if (is.finite(p_val) && p_val < 0.001) "***" else if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"

  for (i in seq_len(k_grps)) {
    gs <- grp_stats[[i]]
    h_str_id <- if (i == 1L) sl_fmt_num(h_stat, 3L, "id") else ""
    h_str_en <- if (i == 1L) sl_fmt_num(h_stat, 3L, "en") else ""
    df_str   <- if (i == 1L) as.character(df_val) else ""
    p_str_id <- if (i == 1L) sl_fmt_p(p_val, "id") else ""
    p_str_en <- if (i == 1L) sl_fmt_p(p_val, "en") else ""
    sig_str  <- if (i == 1L) sig_kw else ""

    rows_t1_id[[i]] <- sl_box_vec(c(
      gs$group, as.character(gs$n), sl_fmt_num(gs$median, 2L, "id"),
      sl_fmt_num(gs$q1, 2L, "id"), sl_fmt_num(gs$q3, 2L, "id"),
      sl_fmt_num(gs$mean_rank, 2L, "id"), h_str_id, df_str, p_str_id, sig_str
    ))
    rows_t1_en[[i]] <- sl_box_vec(c(
      gs$group, as.character(gs$n), sl_fmt_num(gs$median, 2L, "en"),
      sl_fmt_num(gs$q1, 2L, "en"), sl_fmt_num(gs$q3, 2L, "en"),
      sl_fmt_num(gs$mean_rank, 2L, "en"), h_str_en, df_str, p_str_en, if (sig_str == "tn") "ns" else sig_str
    ))
  }

  tbl_summary <- list(
    id = "kruskal_summary",
    title = list(id = "Tabel 1. Rangkuman Uji Kruskal-Wallis & Peringkat Rataan Kelompok", en = "Table 1. Kruskal-Wallis Test Summary & Group Mean Ranks"),
    headers = list(id = hdr_t1_id, en = hdr_t1_en),
    rows = list(id = rows_t1_id, en = rows_t1_en),
    note = list(
      id = "Uji Kruskal-Wallis merupakan padanan non-parametrik One-Way ANOVA yang mengevaluasi perbedaan median berbasis urutan peringkat.",
      en = "Kruskal-Wallis test is the non-parametric equivalent of One-Way ANOVA, evaluating median shifts based on ranked scores."
    )
  )

  # Build Table 2: Post-Hoc Pairwise Comparisons
  hdr_t2_id <- sl_box_vec(c("Pasangan Perbandingan", "Wilcoxon W", "Skor Z Asimtotik", "P-Value Asli", "P-Value Bonferroni", "Signifikansi"))
  hdr_t2_en <- sl_box_vec(c("Comparison Pair", "Wilcoxon W", "Asymptotic Z", "Raw p-value", "Bonferroni p-value", "Significance"))

  rows_t2_id <- list()
  rows_t2_en <- list()
  sig_pairs <- character(0)

  for (i in seq_along(pw_list)) {
    pw <- pw_list[[i]]
    if (is.finite(pw$p_adj) && pw$p_adj < 0.05) sig_pairs <- c(sig_pairs, pw$pair)

    rows_t2_id[[i]] <- sl_box_vec(c(
      pw$pair, sl_fmt_num(pw$w, 1L, "id"), sl_fmt_num(pw$z, 3L, "id"),
      sl_fmt_p(pw$p_raw, "id"), sl_fmt_p(pw$p_adj, "id"), pw$sig
    ))
    rows_t2_en[[i]] <- sl_box_vec(c(
      pw$pair, sl_fmt_num(pw$w, 1L, "en"), sl_fmt_num(pw$z, 3L, "en"),
      sl_fmt_p(pw$p_raw, "en"), sl_fmt_p(pw$p_adj, "en"), if (pw$sig == "tn") "ns" else pw$sig
    ))
  }

  tbl_posthoc <- list(
    id = "kruskal_posthoc",
    title = list(id = "Tabel 2. Uji Lanjut Perbandingan Pasangan (Uji Mann-Whitney Berpasangan dengan Koreksi Bonferroni)", en = "Table 2. Post-Hoc Pairwise Comparisons (Pairwise Mann-Whitney with Bonferroni Adjustment)"),
    headers = list(id = hdr_t2_id, en = hdr_t2_en),
    rows = list(id = rows_t2_id, en = rows_t2_en),
    note = list(
      id = sprintf("Tingkat signifikansi disesuaikan dengan pengali Bonferroni m = %d perbandingan.", m_comparisons),
      en = sprintf("Significance levels adjusted by Bonferroni multiplier m = %d comparisons.", m_comparisons)
    )
  )

  # Graphics: Boxplot + Mean Rank Barplot
  plots <- list()
  if (include_plots) {
    # Plot 1: Boxplot with Jitter
    p_box <- sl_capture_plot(function() {
      sl_plot_box_jitter(
        values = prep$values,
        groups = prep$groups,
        title = if (plot_lang == "id") sprintf("Distribusi Nilai Berdasarkan Kelompok (%s)", resp_label) else sprintf("Value Distribution by Group (%s)", resp_label),
        ylab = resp_label,
        lang = plot_lang,
        outlier_rows = prep$source_rows[outliers$flagged$source_row],
        source_rows = prep$source_rows
      )
    }, width_in = 6.5, height_in = 4.2, res = 300)

    # Plot 2: Mean Ranks Barplot
    p_rank <- sl_capture_plot(function() {
      graphics::par(mar = c(5.0, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      mrs <- vapply(grp_stats, function(s) s$mean_rank, double(1L))
      names(mrs) <- grp_levels
      max_mr <- max(mrs) * 1.25

      bp <- graphics::barplot(
        mrs,
        ylim = c(0, max_mr),
        col = "#1B365D",
        border = NA,
        main = if (plot_lang == "id") "Rataan Peringkat Kelompok (Kruskal-Wallis Mean Rank)" else "Group Mean Ranks (Kruskal-Wallis)",
        ylab = if (plot_lang == "id") "Rataan Peringkat (Mean Rank)" else "Mean Rank",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::grid(col = "#E3DDD0", lty = 3)
      for (i in seq_along(bp)) {
        graphics::text(bp[i], mrs[i] + max_mr * 0.04, labels = sl_fmt_num(mrs[i], 1L, plot_lang), font = 2, col = "#1B365D", cex = 0.85)
      }
    }, width_in = 6.5, height_in = 4.2, res = 300)

    plots <- list(
      list(
        id = "kw_box_jitter",
        title = list(id = "Diagram Kotak & Sebaran Data Observasi", en = "Boxplot & Observation Data Distribution"),
        png_data_url = p_box$png_data_url,
        width_in = p_box$width_in,
        height_in = p_box$height_in
      ),
      list(
        id = "kw_mean_rank",
        title = list(id = "Diagram Batang Rataan Peringkat (Mean Rank)", en = "Mean Rank Barplot"),
        png_data_url = p_rank$png_data_url,
        width_in = p_rank$width_in,
        height_in = p_rank$height_in
      )
    )
  }

  # Order groups by mean rank descending
  ord_idx <- order(vapply(grp_stats, function(s) s$mean_rank, double(1L)), decreasing = TRUE)
  ranked_names <- grp_levels[ord_idx]

  # Key Points
  pts_id <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Signifikansi Kruskal-Wallis**: Terdapat perbedaan yang sangat nyata secara statistik antar kelompok perlakuan terhadap respon %s (H(%d) = %s, p = %s).",
        resp_label, df_val, sl_fmt_num(h_stat, 3L, "id"), sl_fmt_p(p_val, "id"))
    } else {
      sprintf("• **Signifikansi Kruskal-Wallis**: Tidak terdapat perbedaan median yang signifikan secara statistik antar kelompok perlakuan terhadap respon %s (H(%d) = %s, p = %s).",
        resp_label, df_val, sl_fmt_num(h_stat, 3L, "id"), sl_fmt_p(p_val, "id"))
    },
    sprintf("• **Urutan Peringkat Kelompok**: Urutan performa kelompok dari rataan peringkat tertinggi ke terendah adalah: %s.",
      paste(sprintf("%s (Rank = %s)", ranked_names, vapply(grp_stats[ord_idx], function(s) sl_fmt_num(s$mean_rank, 1L, "id"), character(1L))), collapse = " > ")),
    if (length(sig_pairs) > 0L) {
      sprintf("• **Uji Lanjut Pasangan (Post-Hoc)**: Perbedaan nyata terkonfirmasi secara signifikan (Bonferroni p < 0,05) pada pasangan: [%s].",
        paste(sig_pairs, collapse = "; "))
    } else if (p_val < 0.05) {
      "• **Uji Lanjut Pasangan (Post-Hoc)**: Perbedaan simultan terdeteksi pada tingkat global, namun tidak ada pasangan tunggal yang melampaui batas konservatif Bonferroni."
    } else {
      "• **Uji Lanjut Pasangan (Post-Hoc)**: Karena uji omnibus tidak signifikan, tidak ditemukan pasangan kelompok yang berbeda nyata."
    }
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Kruskal-Wallis Significance**: A statistically significant difference was detected among treatments on %s (H(%d) = %s, p = %s).",
        resp_label, df_val, sl_fmt_num(h_stat, 3L, "en"), sl_fmt_p(p_val, "en"))
    } else {
      sprintf("• **Kruskal-Wallis Significance**: No statistically significant median difference was found among treatments on %s (H(%d) = %s, p = %s).",
        resp_label, df_val, sl_fmt_num(h_stat, 3L, "en"), sl_fmt_p(p_val, "en"))
    },
    sprintf("• **Rank Order**: Treatment performance ranked from highest to lowest mean rank: %s.",
      paste(sprintf("%s (Rank = %s)", ranked_names, vapply(grp_stats[ord_idx], function(s) sl_fmt_num(s$mean_rank, 1L, "en"), character(1L))), collapse = " > ")),
    if (length(sig_pairs) > 0L) {
      sprintf("• **Post-Hoc Pairwise**: Statistically significant pairwise differences confirmed (Bonferroni p < .05) for: [%s].",
        paste(sig_pairs, collapse = "; "))
    } else if (p_val < 0.05) {
      "• **Post-Hoc Pairwise**: Global difference detected, but no individual pair survived conservative Bonferroni threshold."
    } else {
      "• **Post-Hoc Pairwise**: Pairwise tests show no significant differences consistent with non-significant omnibus test."
    }
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "kruskal_wallis",
    analysis_label = list(id = "Uji Kruskal-Wallis (Non-Parametrik ANOVA)", en = "Kruskal-Wallis Test (Non-Parametric ANOVA)"),
    response_label = resp_label,
    mode = mode,
    alpha = alpha,
    n_raw = prep$n_raw_obs,
    n_valid = prep$n_valid,
    n_missing = prep$n_missing,
    n_excluded_outliers = nrow(prep$excluded_outliers),
    excluded_rows = sl_box_vec(exclude_rows),
    group_names = sl_box_vec(grp_levels),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = assumptions,
    outliers = outliers,
    main = list(
      h_statistic = h_stat,
      df = df_val,
      p_value = p_val,
      groups = grp_stats
    ),
    posthoc = list(comparisons = pw_list),
    tables_apa = list(tbl_summary, tbl_posthoc),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Statistik Kruskal-Wallis H", en = "Kruskal-Wallis H Statistic"),
        latex = "H = \\frac{12}{N(N+1)} \\sum_{i=1}^k \\frac{R_i^2}{n_i} - 3(N+1)"
      ),
      list(
        title = list(id = "Aproksimasi Skor Z Perbandingan Pasangan", en = "Pairwise Z-Score Approximation"),
        latex = "Z = \\frac{W - E[W]}{\\sigma_W}, \\quad E[W] = \\frac{n_1(n_1+n_2+1)}{2}, \\quad \\sigma_W = \\sqrt{\\frac{n_1 n_2 (n_1+n_2+1)}{12}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Mann-Whitney U Test (Wilcoxon Rank-Sum Test for Two Independent Samples)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_mann_whitney <- function(df, opts = list()) {
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
    stop(sprintf("Uji Mann-Whitney U membutuhkan tepat 2 kelompok (ditemukan %d). Untuk 3+ kelompok gunakan Kruskal-Wallis.", length(grp_levels)))
  }

  g1 <- grp_levels[1L]
  g2 <- grp_levels[2L]
  x1 <- prep$values[prep$groups == g1]
  x2 <- prep$values[prep$groups == g2]
  n1 <- length(x1)
  n2 <- length(x2)
  n_total <- n1 + n2

  if (n1 < 2L || n2 < 2L) {
    stop("Uji Mann-Whitney U membutuhkan minimal 2 observasi pada masing-masing kelompok.")
  }

  resp_label <- if (mode == "long") response_cols[1L] else sprintf("%s vs %s", g1, g2)

  assumptions <- sl_check_assumptions(prep$values, prep$groups, alpha = alpha, require_homogeneity = TRUE)
  outliers <- sl_detect_outliers(prep$values, prep$groups, prep$source_rows, alpha = alpha)

  # Descriptive ranks
  all_rks <- rank(c(x1, x2))
  r1 <- all_rks[seq_len(n1)]
  r2 <- all_rks[(n1 + 1L):n_total]
  mr1 <- mean(r1)
  mr2 <- mean(r2)
  sum_r1 <- sum(r1)
  sum_r2 <- sum(r2)

  med1 <- stats::median(x1)
  med2 <- stats::median(x2)
  iqr1 <- stats::IQR(x1)
  iqr2 <- stats::IQR(x2)

  # Mann-Whitney U computation
  wt <- stats::wilcox.test(x1, x2, exact = FALSE, correct = TRUE)
  w_stat <- as.double(wt$statistic) # in R, wilcox.test statistic is W = sum(r1) - n1*(n1+1)/2 = U1
  u1 <- w_stat
  u2 <- (n1 * n2) - u1
  u_stat <- min(u1, u2)
  p_val <- as.double(wt$p.value)

  # Asymptotic Z & Effect size r
  e_u <- (n1 * n2) / 2.0
  sigma_u <- sqrt((n1 * n2 * (n_total + 1.0)) / 12.0)
  z_score <- if (sigma_u > 0) (u1 - e_u) / sigma_u else 0.0
  r_effect <- abs(z_score) / sqrt(n_total)

  # Table 1: Summary Table
  hdr_id <- sl_box_vec(c("Kelompok", "N", "Median", "IQR", "Rataan Rank", "Nilai U", "Nilai W", "Skor Z", "P-Value", "Effect Size (r)"))
  hdr_en <- sl_box_vec(c("Group", "N", "Median", "IQR", "Mean Rank", "Mann-Whitney U", "Wilcoxon W", "Z-Score", "p-value", "Effect Size (r)"))

  sig_txt <- if (is.finite(p_val) && p_val < 0.001) "***" else if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"

  rows_id <- list(
    sl_box_vec(c(g1, as.character(n1), sl_fmt_num(med1, 2L, "id"), sl_fmt_num(iqr1, 2L, "id"), sl_fmt_num(mr1, 2L, "id"), sl_fmt_num(u_stat, 1L, "id"), sl_fmt_num(sum_r1, 1L, "id"), sl_fmt_num(z_score, 3L, "id"), sl_fmt_p(p_val, "id"), sl_fmt_num(r_effect, 3L, "id"))),
    sl_box_vec(c(g2, as.character(n2), sl_fmt_num(med2, 2L, "id"), sl_fmt_num(iqr2, 2L, "id"), sl_fmt_num(mr2, 2L, "id"), "", sl_fmt_num(sum_r2, 1L, "id"), "", "", ""))
  )
  rows_en <- list(
    sl_box_vec(c(g1, as.character(n1), sl_fmt_num(med1, 2L, "en"), sl_fmt_num(iqr1, 2L, "en"), sl_fmt_num(mr1, 2L, "en"), sl_fmt_num(u_stat, 1L, "en"), sl_fmt_num(sum_r1, 1L, "en"), sl_fmt_num(z_score, 3L, "en"), sl_fmt_p(p_val, "en"), sl_fmt_num(r_effect, 3L, "en"))),
    sl_box_vec(c(g2, as.character(n2), sl_fmt_num(med2, 2L, "en"), sl_fmt_num(iqr2, 2L, "en"), sl_fmt_num(mr2, 2L, "en"), "", sl_fmt_num(sum_r2, 1L, "en"), "", "", ""))
  )

  tbl_mwu <- list(
    id = "mann_whitney_summary",
    title = list(id = "Tabel 1. Rangkuman Hasil Uji Mann-Whitney U (Dua Kelompok Independen)", en = "Table 1. Mann-Whitney U Test Summary (Two Independent Groups)"),
    headers = list(id = hdr_id, en = hdr_en),
    rows = list(id = rows_id, en = rows_en),
    note = list(
      id = sprintf("Ukuran efek r: < 0,3 (kecil), 0,3 - 0,5 (sedang), > 0,5 (besar). Signifikansi: %s.", sig_txt),
      en = sprintf("Effect size r: < .3 (small), .3 - .5 (medium), > .5 (large). Significance: %s.", if (sig_txt == "tn") "ns" else sig_txt)
    )
  )

  # Graphics: Boxplot with Jitter
  plots <- list()
  if (include_plots) {
    p_box <- sl_capture_plot(function() {
      sl_plot_box_jitter(
        values = prep$values,
        groups = prep$groups,
        title = if (plot_lang == "id") sprintf("Perbandingan Dua Kelompok Mann-Whitney U (%s)", resp_label) else sprintf("Mann-Whitney U Two-Group Comparison (%s)", resp_label),
        ylab = resp_label,
        lang = plot_lang,
        outlier_rows = prep$source_rows[outliers$flagged$source_row],
        source_rows = prep$source_rows
      )
    }, width_in = 6.5, height_in = 4.2, res = 300)

    plots[[1L]] <- list(
      id = "mwu_box_jitter",
      title = list(id = "Diagram Kotak & Sebaran Nilai Mann-Whitney U", en = "Mann-Whitney U Boxplot & Jitter"),
      png_data_url = p_box$png_data_url,
      width_in = p_box$width_in,
      height_in = p_box$height_in
    )
  }

  # Effect size descriptor
  eff_desc_id <- if (r_effect >= 0.5) "besar (strong)" else if (r_effect >= 0.3) "sedang (moderate)" else "kecil (weak)"
  eff_desc_en <- if (r_effect >= 0.5) "large" else if (r_effect >= 0.3) "medium" else "small"

  # Key Points
  pts_id <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Signifikansi Mann-Whitney U**: Terdapat perbedaan yang signifikan secara statistik antara kelompok %s dan %s (U = %s, W = %s, Z = %s, p = %s).",
        g1, g2, sl_fmt_num(u_stat, 1L, "id"), sl_fmt_num(sum_r1, 1L, "id"), sl_fmt_num(z_score, 3L, "id"), sl_fmt_p(p_val, "id"))
    } else {
      sprintf("• **Signifikansi Mann-Whitney U**: Tidak terdapat perbedaan yang signifikan secara statistik antara kelompok %s dan %s (U = %s, W = %s, Z = %s, p = %s).",
        g1, g2, sl_fmt_num(u_stat, 1L, "id"), sl_fmt_num(sum_r1, 1L, "id"), sl_fmt_num(z_score, 3L, "id"), sl_fmt_p(p_val, "id"))
    },
    sprintf("• **Perbandingan Median & Peringkat**: Kelompok %s memiliki median %s (Mean Rank = %s), sedangkan kelompok %s memiliki median %s (Mean Rank = %s).",
      g1, sl_fmt_num(med1, 2L, "id"), sl_fmt_num(mr1, 2L, "id"),
      g2, sl_fmt_num(med2, 2L, "id"), sl_fmt_num(mr2, 2L, "id")),
    sprintf("• **Besaran Pengaruh (Effect Size)**: Diperoleh nilai r = %s yang mengindikasikan besaran pengaruh berkategori %s.",
      sl_fmt_num(r_effect, 3L, "id"), eff_desc_id)
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Mann-Whitney Significance**: A statistically significant difference was detected between %s and %s (U = %s, W = %s, Z = %s, p = %s).",
        g1, g2, sl_fmt_num(u_stat, 1L, "en"), sl_fmt_num(sum_r1, 1L, "en"), sl_fmt_num(z_score, 3L, "en"), sl_fmt_p(p_val, "en"))
    } else {
      sprintf("• **Mann-Whitney Significance**: No statistically significant difference was observed between %s and %s (U = %s, W = %s, Z = %s, p = %s).",
        g1, g2, sl_fmt_num(u_stat, 1L, "en"), sl_fmt_num(sum_r1, 1L, "en"), sl_fmt_num(z_score, 3L, "en"), sl_fmt_p(p_val, "en"))
    },
    sprintf("• **Median & Rank Contrast**: %s median = %s (Mean Rank = %s) vs %s median = %s (Mean Rank = %s).",
      g1, sl_fmt_num(med1, 2L, "en"), sl_fmt_num(mr1, 2L, "en"),
      g2, sl_fmt_num(med2, 2L, "en"), sl_fmt_num(mr2, 2L, "en")),
    sprintf("• **Effect Size Magnitude**: Effect size r = %s indicates a %s practical effect.",
      sl_fmt_num(r_effect, 3L, "en"), eff_desc_en)
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "mann_whitney",
    analysis_label = list(id = "Uji Mann-Whitney U (Dua Sampel Independen)", en = "Mann-Whitney U Test (Two Independent Samples)"),
    response_label = resp_label,
    mode = mode,
    alpha = alpha,
    n_raw = prep$n_raw_obs,
    n_valid = prep$n_valid,
    n_missing = prep$n_missing,
    n_excluded_outliers = nrow(prep$excluded_outliers),
    excluded_rows = sl_box_vec(exclude_rows),
    group_names = sl_box_vec(grp_levels),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = assumptions,
    outliers = outliers,
    main = list(
      u_statistic = u_stat,
      w_statistic = sum_r1,
      z_score = z_score,
      p_value = p_val,
      effect_size_r = r_effect
    ),
    posthoc = NULL,
    tables_apa = list(tbl_mwu),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Statistik Mann-Whitney U", en = "Mann-Whitney U Statistic"),
        latex = "U_1 = n_1 n_2 + \\frac{n_1(n_1+1)}{2} - R_1, \\quad U_2 = n_1 n_2 - U_1, \\quad U = \\min(U_1, U_2)"
      ),
      list(
        title = list(id = "Skor Z & Ukuran Efek r", en = "Z-Score & Effect Size r"),
        latex = "Z = \\frac{U_1 - \\frac{n_1 n_2}{2}}{\\sqrt{\\frac{n_1 n_2 (n_1 + n_2 + 1)}{12}}}, \\quad r = \\frac{|Z|}{\\sqrt{N}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Wilcoxon Signed-Rank Test (Paired Samples)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_wilcoxon_signed <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  response_cols <- if (!is.null(roles$response_cols)) as.character(roles$response_cols) else character(0L)

  if (length(response_cols) < 2L) {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    response_cols <- if (length(num_cols) >= 2L) num_cols[1L:2L] else names(df)[1L:min(2L, length(names(df)))]
  }

  col_before <- response_cols[1L]
  col_after  <- response_cols[2L]

  v_before <- suppressWarnings(as.numeric(as.character(df[[col_before]])))
  v_after  <- suppressWarnings(as.numeric(as.character(df[[col_after]])))

  valid_idx <- which(is.finite(v_before) & is.finite(v_after))
  if (length(valid_idx) < 3L) {
    stop("Uji Wilcoxon Bertanda membutuhkan minimal 3 pasang data pengamatan lengkap.")
  }

  x_bef <- v_before[valid_idx]
  x_aft <- v_after[valid_idx]
  n_pairs <- length(x_bef)

  diffs <- x_aft - x_bef
  nonzero_diffs <- diffs[diffs != 0]
  n_nonzero <- length(nonzero_diffs)
  n_ties <- n_pairs - n_nonzero

  n_pos <- sum(diffs > 0)
  n_neg <- sum(diffs < 0)

  # Wilcoxon Test Calculation
  wt <- stats::wilcox.test(x_aft, x_bef, paired = TRUE, exact = FALSE, correct = TRUE)
  v_stat <- as.double(wt$statistic)
  p_val  <- as.double(wt$p.value)

  # Normal approximation for Z
  e_v <- (n_nonzero * (n_nonzero + 1.0)) / 4.0
  sigma_v <- sqrt((n_nonzero * (n_nonzero + 1.0) * (2.0 * n_nonzero + 1.0)) / 24.0)
  z_score <- if (sigma_v > 0) (v_stat - e_v) / sigma_v else 0.0
  r_effect <- abs(z_score) / sqrt(n_pairs)

  med_diff <- stats::median(diffs)
  med_bef <- stats::median(x_bef)
  med_aft <- stats::median(x_aft)

  # Summary Table
  hdr_id <- sl_box_vec(c("Perbandingan", "N Pasang", "Positif", "Negatif", "Ties (Nol)", "Wilcoxon V", "Skor Z", "P-Value", "Median Selisih", "Effect Size (r)"))
  hdr_en <- sl_box_vec(c("Comparison", "N Pairs", "Positive", "Negative", "Ties (Zero)", "Wilcoxon V", "Z-Score", "p-value", "Median Difference", "Effect Size (r)"))

  sig_txt <- if (is.finite(p_val) && p_val < 0.001) "***" else if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"

  rows_id <- list(
    sl_box_vec(c(
      sprintf("%s vs %s", col_after, col_before), as.character(n_pairs),
      as.character(n_pos), as.character(n_neg), as.character(n_ties),
      sl_fmt_num(v_stat, 1L, "id"), sl_fmt_num(z_score, 3L, "id"),
      sl_fmt_p(p_val, "id"), sl_fmt_num(med_diff, 2L, "id"), sl_fmt_num(r_effect, 3L, "id")
    ))
  )
  rows_en <- list(
    sl_box_vec(c(
      sprintf("%s vs %s", col_after, col_before), as.character(n_pairs),
      as.character(n_pos), as.character(n_neg), as.character(n_ties),
      sl_fmt_num(v_stat, 1L, "en"), sl_fmt_num(z_score, 3L, "en"),
      sl_fmt_p(p_val, "en"), sl_fmt_num(med_diff, 2L, "en"), sl_fmt_num(r_effect, 3L, "en")
    ))
  )

  tbl_summary <- list(
    id = "wilcoxon_signed_summary",
    title = list(id = "Tabel 1. Rangkuman Hasil Uji Peringkat Bertanda Wilcoxon (Sampel Berpasangan)", en = "Table 1. Wilcoxon Signed-Rank Test Summary (Paired Samples)"),
    headers = list(id = hdr_id, en = hdr_en),
    rows = list(id = rows_id, en = rows_en),
    note = list(
      id = sprintf("Statistik V adalah jumlah peringkat positif. Ukuran efek r = |Z| / √N (%s).", sig_txt),
      en = sprintf("Statistic V represents the sum of positive ranks. Effect size r = |Z| / √N (%s).", if (sig_txt == "tn") "ns" else sig_txt)
    )
  )

  # Graphics: Slopegraph (Before vs After)
  plots <- list()
  if (include_plots) {
    p_slope <- sl_capture_plot(function() {
      graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      y_min <- min(c(x_bef, x_aft)) * 0.95
      y_max <- max(c(x_bef, x_aft)) * 1.05

      plot(
        c(1, 2), c(med_bef, med_aft),
        type = "n", xlim = c(0.7, 2.3), ylim = c(y_min, y_max),
        xaxt = "n",
        xlab = if (plot_lang == "id") "Kondisi Pengamatan" else "Observation Condition",
        ylab = if (plot_lang == "id") "Nilai Respon" else "Response Value",
        main = if (plot_lang == "id") sprintf("Trajektori Perubahan Sampel Berpasangan (%s vs %s)", col_before, col_after) else sprintf("Paired Sample Trajectories (%s vs %s)", col_before, col_after),
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::axis(1, at = c(1, 2), labels = c(col_before, col_after), col.axis = "#1B365D", font = 2)
      graphics::grid(col = "#E3DDD0", lty = 3)

      # Draw individual trajectories
      for (i in seq_len(n_pairs)) {
        col_line <- if (diffs[i] > 0) "#2E7D32" else if (diffs[i] < 0) "#B83227" else "#8C9BAE"
        graphics::lines(c(1, 2), c(x_bef[i], x_aft[i]), col = col_line, lwd = 1.3)
        graphics::points(c(1, 2), c(x_bef[i], x_aft[i]), pch = 21, bg = col_line, col = "#1B365D", cex = 1.0)
      }

      # Draw median trend
      graphics::lines(c(1, 2), c(med_bef, med_aft), col = "#1B365D", lwd = 3.0)
      graphics::points(c(1, 2), c(med_bef, med_aft), pch = 23, bg = "#FFD700", col = "#1B365D", cex = 1.6)

      graphics::legend(
        "topleft",
        legend = c(
          if (plot_lang == "id") sprintf("Meningkat (n=%d)", n_pos) else sprintf("Increased (n=%d)", n_pos),
          if (plot_lang == "id") sprintf("Menurun (n=%d)", n_neg) else sprintf("Decreased (n=%d)", n_neg),
          if (plot_lang == "id") "Rataan Median" else "Median Trend"
        ),
        col = c("#2E7D32", "#B83227", "#1B365D"),
        lwd = c(2, 2, 3), bty = "n", cex = 0.8
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "wilcoxon_slopegraph",
      title = list(id = "Grafik Trajektori Perubahan Sampel Berpasangan", en = "Paired Sample Change Trajectory Plot"),
      png_data_url = p_slope$png_data_url,
      width_in = p_slope$width_in,
      height_in = p_slope$height_in
    )
  }

  pts_id <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Signifikansi Perubahan Berpasangan**: Terdapat perubahan yang signifikan secara statistik antara kondisi %s dan %s (Wilcoxon V = %s, Z = %s, p = %s).",
        col_before, col_after, sl_fmt_num(v_stat, 1L, "id"), sl_fmt_num(z_score, 3L, "id"), sl_fmt_p(p_val, "id"))
    } else {
      sprintf("• **Signifikansi Perubahan Berpasangan**: Tidak terdapat perubahan yang signifikan secara statistik antara kondisi %s dan %s (Wilcoxon V = %s, Z = %s, p = %s).",
        col_before, col_after, sl_fmt_num(v_stat, 1L, "id"), sl_fmt_num(z_score, 3L, "id"), sl_fmt_p(p_val, "id"))
    },
    sprintf("• **Arah Perubahan Subjek**: Dari %d pasang data, %d subjek mengalami peningkatan, %d subjek mengalami penurunan, dan %d subjek bernilai tetap.",
      n_pairs, n_pos, n_neg, n_ties),
    sprintf("• **Median Selisih & Besaran Pengaruh**: Median selisih perubahan bernilai %s dengan ukuran efek r = %s.",
      sl_fmt_num(med_diff, 2L, "id"), sl_fmt_num(r_effect, 3L, "id"))
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Paired Shift Significance**: A statistically significant shift occurred between %s and %s (Wilcoxon V = %s, Z = %s, p = %s).",
        col_before, col_after, sl_fmt_num(v_stat, 1L, "en"), sl_fmt_num(z_score, 3L, "en"), sl_fmt_p(p_val, "en"))
    } else {
      sprintf("• **Paired Shift Significance**: No statistically significant shift was observed between %s and %s (Wilcoxon V = %s, Z = %s, p = %s).",
        col_before, col_after, sl_fmt_num(v_stat, 1L, "en"), sl_fmt_num(z_score, 3L, "en"), sl_fmt_p(p_val, "en"))
    },
    sprintf("• **Directional Breakdown**: Across %d pairs, %d subjects increased, %d decreased, and %d remained unchanged.",
      n_pairs, n_pos, n_neg, n_ties),
    sprintf("• **Median Difference & Effect Size**: Median change difference is %s with effect size r = %s.",
      sl_fmt_num(med_diff, 2L, "en"), sl_fmt_num(r_effect, 3L, "en"))
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "wilcoxon_signed",
    analysis_label = list(id = "Uji Wilcoxon Bertanda (Sampel Berpasangan)", en = "Wilcoxon Signed-Rank Test (Paired Samples)"),
    response_label = sprintf("%s vs %s", col_after, col_before),
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(length(v_before)),
    n_valid = as.integer(n_pairs),
    n_missing = as.integer(length(v_before) - n_pairs),
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(c(col_before, col_after)),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(
      v_statistic = v_stat,
      z_score = z_score,
      p_value = p_val,
      median_diff = med_diff,
      effect_size_r = r_effect
    ),
    posthoc = NULL,
    tables_apa = list(tbl_summary),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Statistik Wilcoxon Bertanda V", en = "Wilcoxon Signed-Rank V"),
        latex = "V = \\sum_{i: d_i > 0} R_i, \\quad d_i = x_{2,i} - x_{1,i}"
      ),
      list(
        title = list(id = "Aproksimasi Skor Z Normal", en = "Normal Z Approximation"),
        latex = "Z = \\frac{V - \\frac{n(n+1)}{4}}{\\sqrt{\\frac{n(n+1)(2n+1)}{24}}}, \\quad r = \\frac{|Z|}{\\sqrt{N}}"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}

#' Friedman Test (Non-Parametric Repeated Measures / Randomized Complete Block)
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list matching StatLab dispatch contract
sl_analyze_friedman <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  response_cols <- if (!is.null(roles$response_cols)) as.character(roles$response_cols) else character(0L)

  if (length(response_cols) < 3L) {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    response_cols <- if (length(num_cols) >= 3L) num_cols else names(df)
  }

  mat_raw <- as.matrix(df[response_cols])
  mat_raw <- apply(mat_raw, 2L, as.numeric)
  valid_rows <- which(stats::complete.cases(mat_raw))
  if (length(valid_rows) < 3L) {
    stop("Uji Friedman membutuhkan minimal 3 baris pengamatan lengkap melintasi waktu/kondisi.")
  }

  mat <- mat_raw[valid_rows, , drop = FALSE]
  n_subjects <- nrow(mat)
  k_time <- ncol(mat)

  # Friedman test
  ft <- stats::friedman.test(mat)
  chi_val <- as.double(ft$statistic)
  df_val  <- as.integer(ft$parameter)
  p_val   <- as.double(ft$p.value)

  # Mean rank across conditions
  # In Friedman test, ranking is done WITHIN each row (subject)
  rank_matrix <- t(apply(mat, 1L, rank))
  mean_ranks <- colMeans(rank_matrix)
  medians <- apply(mat, 2L, stats::median)
  q1s <- apply(mat, 2L, function(v) stats::quantile(v, 0.25, names = FALSE))
  q3s <- apply(mat, 2L, function(v) stats::quantile(v, 0.75, names = FALSE))

  # Post-Hoc Pairwise Comparisons (Paired Wilcoxon with Bonferroni correction)
  m_comparisons <- (k_time * (k_time - 1L)) / 2L
  pw_list <- list()
  pw_counter <- 1L

  for (i in 1L:(k_time - 1L)) {
    for (j in (i + 1L):k_time) {
      c1_nm <- response_cols[i]
      c2_nm <- response_cols[j]
      v1 <- mat[, i]
      v2 <- mat[, j]

      wt <- tryCatch(stats::wilcox.test(v2, v1, paired = TRUE, exact = FALSE), error = function(e) NULL)
      if (!is.null(wt)) {
        v_stat <- as.double(wt$statistic)
        p_raw  <- as.double(wt$p.value)
      } else {
        v_stat <- NA_real_
        p_raw  <- NA_real_
      }

      p_adj <- if (is.finite(p_raw)) min(1.0, p_raw * m_comparisons) else NA_real_
      sig_mark <- if (is.finite(p_adj) && p_adj < 0.001) "***" else if (is.finite(p_adj) && p_adj < 0.01) "**" else if (is.finite(p_adj) && p_adj < 0.05) "*" else "tn"

      pw_list[[pw_counter]] <- list(
        pair = sprintf("%s vs %s", c2_nm, c1_nm),
        v = v_stat,
        p_raw = p_raw,
        p_adj = p_adj,
        sig = sig_mark
      )
      pw_counter <- pw_counter + 1L
    }
  }

  # Build Table 1: Summary Table
  hdr_t1_id <- sl_box_vec(c("Kondisi / Waktu", "N Subjek", "Median", "Q1", "Q3", "Rataan Peringkat (Mean Rank)", "Friedman Chi-Square (χ²)", "db", "P-Value", "Signifikansi"))
  hdr_t1_en <- sl_box_vec(c("Condition / Time", "N Subjects", "Median", "Q1", "Q3", "Mean Rank", "Friedman Chi-Square (χ²)", "df", "p-value", "Significance"))

  sig_fr <- if (is.finite(p_val) && p_val < 0.001) "***" else if (is.finite(p_val) && p_val < 0.01) "**" else if (is.finite(p_val) && p_val < 0.05) "*" else "tn"

  rows_t1_id <- list()
  rows_t1_en <- list()
  for (i in seq_len(k_time)) {
    c_nm <- response_cols[i]
    chi_str_id <- if (i == 1L) sl_fmt_num(chi_val, 3L, "id") else ""
    chi_str_en <- if (i == 1L) sl_fmt_num(chi_val, 3L, "en") else ""
    df_str     <- if (i == 1L) as.character(df_val) else ""
    p_str_id   <- if (i == 1L) sl_fmt_p(p_val, "id") else ""
    p_str_en   <- if (i == 1L) sl_fmt_p(p_val, "en") else ""
    sig_str    <- if (i == 1L) sig_fr else ""

    rows_t1_id[[i]] <- sl_box_vec(c(
      c_nm, as.character(n_subjects), sl_fmt_num(medians[i], 2L, "id"),
      sl_fmt_num(q1s[i], 2L, "id"), sl_fmt_num(q3s[i], 2L, "id"),
      sl_fmt_num(mean_ranks[i], 2L, "id"), chi_str_id, df_str, p_str_id, sig_str
    ))
    rows_t1_en[[i]] <- sl_box_vec(c(
      c_nm, as.character(n_subjects), sl_fmt_num(medians[i], 2L, "en"),
      sl_fmt_num(q1s[i], 2L, "en"), sl_fmt_num(q3s[i], 2L, "en"),
      sl_fmt_num(mean_ranks[i], 2L, "en"), chi_str_en, df_str, p_str_en, if (sig_str == "tn") "ns" else sig_str
    ))
  }

  tbl_summary <- list(
    id = "friedman_summary",
    title = list(id = "Tabel 1. Rangkuman Uji Friedman & Peringkat Rataan Waktu Pengamatan", en = "Table 1. Friedman Test Summary & Mean Ranks Across Time/Conditions"),
    headers = list(id = hdr_t1_id, en = hdr_t1_en),
    rows = list(id = rows_t1_id, en = rows_t1_en),
    note = list(
      id = "Uji Friedman adalah alternatif non-parametrik untuk Repeated Measures ANOVA pada pengukuran berulang subjek yang sama.",
      en = "Friedman test serves as the non-parametric counterpart to Repeated Measures ANOVA for longitudinal/repeated assessments."
    )
  )

  # Build Table 2: Post-Hoc Pairwise Comparisons
  hdr_t2_id <- sl_box_vec(c("Pasangan Waktu", "Wilcoxon V", "P-Value Asli", "P-Value Bonferroni", "Signifikansi"))
  hdr_t2_en <- sl_box_vec(c("Time Pair", "Wilcoxon V", "Raw p-value", "Bonferroni p-value", "Significance"))

  rows_t2_id <- list()
  rows_t2_en <- list()
  sig_pairs <- character(0)

  for (i in seq_along(pw_list)) {
    pw <- pw_list[[i]]
    if (is.finite(pw$p_adj) && pw$p_adj < 0.05) sig_pairs <- c(sig_pairs, pw$pair)

    rows_t2_id[[i]] <- sl_box_vec(c(pw$pair, sl_fmt_num(pw$v, 1L, "id"), sl_fmt_p(pw$p_raw, "id"), sl_fmt_p(pw$p_adj, "id"), pw$sig))
    rows_t2_en[[i]] <- sl_box_vec(c(pw$pair, sl_fmt_num(pw$v, 1L, "en"), sl_fmt_p(pw$p_raw, "en"), sl_fmt_p(pw$p_adj, "en"), if (pw$sig == "tn") "ns" else pw$sig))
  }

  tbl_posthoc <- list(
    id = "friedman_posthoc",
    title = list(id = "Tabel 2. Uji Lanjut Perbandingan Berpasangan Antar-Waktu (Koreksi Bonferroni)", en = "Table 2. Post-Hoc Pairwise Comparisons Across Time Points (Bonferroni Adjusted)"),
    headers = list(id = hdr_t2_id, en = hdr_t2_en),
    rows = list(id = rows_t2_id, en = rows_t2_en),
    note = list(
      id = sprintf("Tingkat signifikansi disesuaikan dengan m = %d perbandingan berpasangan.", m_comparisons),
      en = sprintf("Significance levels adjusted for m = %d pairwise comparisons.", m_comparisons)
    )
  )

  # Graphics: Mean Rank Trajectory Plot
  plots <- list()
  if (include_plots) {
    p_traj <- sl_capture_plot(function() {
      graphics::par(mar = c(5.0, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")
      y_max <- max(k_time * 1.15, max(mean_ranks) * 1.2)

      plot(
        seq_len(k_time), mean_ranks,
        type = "b", pch = 19, col = "#1B365D", lwd = 2.4, cex = 1.4,
        ylim = c(0.8, y_max), xaxt = "n",
        xlab = if (plot_lang == "id") "Waktu / Titik Pengamatan" else "Time Point / Condition",
        ylab = if (plot_lang == "id") "Rataan Peringkat (Mean Rank)" else "Mean Rank",
        main = if (plot_lang == "id") "Dinamika Trajektori Peringkat Friedman Antar-Waktu" else "Friedman Mean Rank Dynamics Across Time",
        las = 1, col.main = "#1B365D", col.lab = "#1B365D", col.axis = "#1B365D"
      )
      graphics::axis(1, at = seq_len(k_time), labels = response_cols, col.axis = "#1B365D", font = 2)
      graphics::grid(col = "#E3DDD0", lty = 3)

      for (i in seq_len(k_time)) {
        graphics::text(i, mean_ranks[i] + y_max * 0.05, labels = sl_fmt_num(mean_ranks[i], 2L, plot_lang), font = 2, col = "#1B365D", cex = 0.85)
      }
    }, width_in = 6.5, height_in = 4.2, res = 300)

    plots[[1L]] <- list(
      id = "friedman_trajectory",
      title = list(id = "Trajektori Rataan Peringkat Waktu Pengamatan", en = "Friedman Mean Rank Trajectory Across Time"),
      png_data_url = p_traj$png_data_url,
      width_in = p_traj$width_in,
      height_in = p_traj$height_in
    )
  }

  pts_id <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Signifikansi Efek Waktu/Kondisi**: Uji Friedman menunjukkan adanya perubahan yang sangat nyata secara statistik seiring waktu pengamatan (χ²(%d) = %s, p = %s).",
        df_val, sl_fmt_num(chi_val, 3L, "id"), sl_fmt_p(p_val, "id"))
    } else {
      sprintf("• **Signifikansi Efek Waktu/Kondisi**: Uji Friedman menunjukkan tidak terdapat perubahan yang signifikan seiring berjalannya waktu (χ²(%d) = %s, p = %s).",
        df_val, sl_fmt_num(chi_val, 3L, "id"), sl_fmt_p(p_val, "id"))
    },
    sprintf("• **Dinamika Peringkat Waktu**: Rataan peringkat berkembang dari %s (%s) menjadi %s (%s).",
      sl_fmt_num(mean_ranks[1L], 2L, "id"), response_cols[1L],
      sl_fmt_num(mean_ranks[k_time], 2L, "id"), response_cols[k_time]),
    if (length(sig_pairs) > 0L) {
      sprintf("• **Perbedaan Antar-Titik Waktu (Post-Hoc)**: Perubahan signifikan terdeteksi (Bonferroni p < 0,05) pada transisi: [%s].",
        paste(sig_pairs, collapse = "; "))
    } else if (p_val < 0.05) {
      "• **Perbedaan Antar-Titik Waktu (Post-Hoc)**: Perbedaan global signifikan, namun tidak ada pasangan spesifik yang melampaui ambang Bonferroni."
    } else {
      "• **Perbedaan Antar-Titik Waktu (Post-Hoc)**: Tidak ditemukan perbedaan nyata antar-titik pengamatan."
    }
  )
  interp_id <- paste(pts_id, collapse = "\n")

  pts_en <- c(
    if (is.finite(p_val) && p_val < 0.05) {
      sprintf("• **Time/Condition Effect Significance**: Friedman test indicated a statistically significant change across observation times (χ²(%d) = %s, p = %s).",
        df_val, sl_fmt_num(chi_val, 3L, "en"), sl_fmt_p(p_val, "en"))
    } else {
      sprintf("• **Time/Condition Effect Significance**: Friedman test showed no statistically significant change over time (χ²(%d) = %s, p = %s).",
        df_val, sl_fmt_num(chi_val, 3L, "en"), sl_fmt_p(p_val, "en"))
    },
    sprintf("• **Rank Progression**: Mean ranks evolved from %s (%s) to %s (%s).",
      sl_fmt_num(mean_ranks[1L], 2L, "en"), response_cols[1L],
      sl_fmt_num(mean_ranks[k_time], 2L, "en"), response_cols[k_time]),
    if (length(sig_pairs) > 0L) {
      sprintf("• **Pairwise Time Contrasts**: Statistically significant shifts confirmed (Bonferroni p < .05) for: [%s].",
        paste(sig_pairs, collapse = "; "))
    } else if (p_val < 0.05) {
      "• **Pairwise Time Contrasts**: Global difference detected, but individual pairs did not meet Bonferroni cutoff."
    } else {
      "• **Pairwise Time Contrasts**: No significant pairwise shifts observed."
    }
  )
  interp_en <- paste(pts_en, collapse = "\n")

  meta <- list(
    analysis_type = "friedman",
    analysis_label = list(id = "Uji Friedman (Non-Parametrik Repeated Measures)", en = "Friedman Test (Non-Parametric Repeated Measures)"),
    response_label = "Pengukuran Berulang",
    mode = "wide",
    alpha = alpha,
    n_raw = as.integer(nrow(mat_raw)),
    n_valid = as.integer(n_subjects),
    n_missing = as.integer(nrow(mat_raw) - n_subjects),
    n_excluded_outliers = 0L,
    excluded_rows = sl_box_vec(integer(0)),
    group_names = sl_box_vec(response_cols),
    r_version = paste0(R.version$major, ".", R.version$minor),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  list(
    meta = meta,
    assumptions = list(normality_violated = FALSE, homogeneity_violated = FALSE),
    outliers = list(flagged = data.frame()),
    main = list(
      chi_square = chi_val,
      df = df_val,
      p_value = p_val,
      mean_ranks = as.list(mean_ranks)
    ),
    posthoc = list(comparisons = pw_list),
    tables_apa = list(tbl_summary, tbl_posthoc),
    tables_full = list(),
    formulas = list(
      list(
        title = list(id = "Statistik Friedman Chi-Square", en = "Friedman Chi-Square Statistic"),
        latex = "\\chi_F^2 = \\frac{12}{n k (k+1)} \\sum_{j=1}^k R_j^2 - 3n(k+1)"
      )
    ),
    plots = plots,
    interpretation = list(id = interp_id, en = interp_en),
    warnings = list(id = sl_box_vec(character(0)), en = sl_box_vec(character(0)))
  )
}
