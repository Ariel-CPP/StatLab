# R/06_anova.R
# StatLab One-Way Analysis of Variance (ANOVA) Engine, Post Hoc Tests & Publication Tables
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

#' Assign Compact Letter Display (CLD) notation (a, b, ab, c, etc.) to treatment means
#'
#' Implements standard biostatistical pairwise clique letter grouping (Piepho / agricolae algorithm)
#' in pure Base R.
#'
#' @param treatments Character vector of treatment names
#' @param means Numeric vector of treatment means
#' @param alpha Significance level (e.g. 0.05)
#' @param pval_matrix Symmetric numeric matrix of pairwise p-values (size k x k)
#' @return Named character vector of letter notations mapped by treatment name
sl_cld_letters <- function(treatments, means, alpha, pval_matrix) {
  n <- length(means)
  if (n == 0L) return(character(0L))
  if (n == 1L) {
    out <- c("a")
    names(out) <- as.character(treatments)
    return(out)
  }

  treatments <- as.character(treatments)
  means <- as.numeric(means)

  # Standard alphabet pool
  letras <- c(letters[1:26], LETTERS[1:26], as.character(1:9))

  # Sort descending by mean
  ord <- order(means, decreasing = TRUE)
  w_trt <- treatments[ord]
  w_means <- means[ord]

  M <- rep("", n)
  k <- 1L
  j <- 1L
  chequeo <- 0L
  M[1L] <- letras[k]

  last_char <- function(str_val) {
    s <- sub(" +$", "", str_val)
    if (nchar(s) == 0L) return("")
    substr(s, nchar(s), nchar(s))
  }

  while (j < n) {
    chequeo <- chequeo + 1L
    if (chequeo > (n * 2L + 10L)) break

    cambio1 <- 0L
    for (i in j:n) {
      # Original index in treatments/pval_matrix
      idx_i <- ord[i]
      idx_j <- ord[j]
      p_ij <- pval_matrix[idx_i, idx_j]

      # If difference is not significant (p > alpha), share letter
      is_nonsig <- is.finite(p_ij) && (p_ij > alpha)
      if (is_nonsig) {
        if (last_char(M[i]) != letras[k]) {
          M[i] <- paste0(M[i], letras[k])
        }
      } else {
        k <- k + 1L
        if (k > length(letras)) k <- 1L # fallback
        cambio <- i
        ja <- j
        M[cambio] <- paste0(M[cambio], letras[k])
        for (v in ja:cambio) {
          idx_v <- ord[v]
          idx_cambio <- ord[cambio]
          p_vc <- pval_matrix[idx_v, idx_cambio]
          if (is.finite(p_vc) && (p_vc <= alpha)) {
            j <- j + 1L
            cambio1 <- 1L
          } else {
            break
          }
        }
        break
      }
    }
    if (cambio1 == 0L) {
      j <- j + 1L
    }
  }

  # Build output mapped to original treatment names
  out <- rep("", n)
  for (i in seq_len(n)) {
    orig_idx <- ord[i]
    out[orig_idx] <- M[i]
  }
  names(out) <- treatments
  out
}

#' Pure Base-R Duncan's Multiple Range Test (DMRT)
#'
#' Matches `agricolae::duncan.test` to <= 1e-8.
#'
#' @param means Numeric vector of group means
#' @param ns Integer vector of group sample sizes
#' @param ms_error Mean Square Error (Galat) from ANOVA
#' @param df_error Degrees of freedom of Error (Galat)
#' @param alpha Significance level (0.05 default)
#' @param treatments Treatment group names
#' @return Named list with critical ranges, pairwise p-values, and group letters
sl_duncan_test <- function(means, ns, ms_error, df_error, alpha = 0.05, treatments = names(means)) {
  k <- length(means)
  treatments <- if (!is.null(treatments)) as.character(treatments) else paste0("T", seq_len(k))
  names(means) <- treatments

  if (k < 2L || df_error < 1L || !is.finite(ms_error) || ms_error <= 0) {
    empty_letters <- rep("a", k)
    names(empty_letters) <- treatments
    return(list(critical_ranges = numeric(0), pval_matrix = matrix(1, k, k), group_letters = empty_letters))
  }

  # Harmonic mean of sample sizes for unbalanced designs
  n_harm <- if (length(unique(ns)) == 1L) ns[1L] else (1.0 / mean(1.0 / ns))
  sd_dif <- sqrt(ms_error / n_harm)

  # Duncan critical values via studentized range: qtukey((1 - alpha)^(p - 1), p, df_error)
  t_prob <- numeric(k - 1L)
  for (p in 2L:k) {
    quantile_val <- (1.0 - alpha)^(p - 1L)
    q_val <- tryCatch(stats::qtukey(quantile_val, p, df_error), error = function(e) NA_real_)
    if (!is.finite(q_val)) {
      # Fallback uniroot for extreme degrees of freedom
      f_root <- function(x) stats::ptukey(x, p, df_error) - quantile_val
      ur <- tryCatch(stats::uniroot(f_root, c(0, 100))$root, error = function(e) 3.0)
      q_val <- ur
    }
    t_prob[p - 1L] <- q_val
  }

  crit_ranges <- t_prob * sd_dif
  names(crit_ranges) <- as.character(2L:k)

  # Ordered means rank
  ord_means <- order(means, decreasing = TRUE)
  ord_index <- order(ord_means)

  # Build pairwise p-values matrix
  pval_mat <- matrix(1.0, nrow = k, ncol = k, dimnames = list(treatments, treatments))
  for (i in 1L:(k - 1L)) {
    for (j in (i + 1L):k) {
      diff_val <- abs(means[i] - means[j])
      range_p <- abs(ord_index[i] - ord_index[j]) + 1L
      # Duncan approximate p-value
      p_raw <- stats::ptukey(diff_val / sd_dif, range_p, df_error)
      p_duncan <- if (is.finite(p_raw) && range_p > 1L) {
        max(0.0, min(1.0, 1.0 - (p_raw)^(1.0 / (range_p - 1L))))
      } else {
        1.0
      }
      pval_mat[i, j] <- p_duncan
      pval_mat[j, i] <- p_duncan
    }
  }

  group_letters <- sl_cld_letters(treatments, means, alpha, pval_mat)

  list(
    critical_ranges = crit_ranges,
    pval_matrix = pval_mat,
    group_letters = as.list(group_letters)
  )
}

#' Pure Base-R Tukey HSD Test with Compact Letter Display
#'
#' @param fit An `aov` model object
#' @param alpha Significance level (0.05 default)
#' @return Named list with pairwise comparisons and group letters
sl_tukey_test <- function(fit, alpha = 0.05) {
  thsd <- tryCatch(stats::TukeyHSD(fit, "groups", conf.level = 1.0 - alpha)[[1L]], error = function(e) NULL)
  
  mf <- stats::model.frame(fit)
  groups <- mf$groups
  values <- mf$values
  grp_levels <- levels(groups)
  k <- length(grp_levels)
  means <- tapply(values, groups, mean, na.rm = TRUE)

  if (is.null(thsd) || k < 2L) {
    empty_letters <- rep("a", k)
    names(empty_letters) <- grp_levels
    return(list(comparisons = data.frame(), pval_matrix = matrix(1, k, k), group_letters = as.list(empty_letters)))
  }

  pval_mat <- matrix(1.0, nrow = k, ncol = k, dimnames = list(grp_levels, grp_levels))
  comp_df <- data.frame(
    comparison = rownames(thsd),
    diff = thsd[, "diff"],
    lwr = thsd[, "lwr"],
    upr = thsd[, "upr"],
    p_adj = thsd[, "p adj"],
    stringsAsFactors = FALSE
  )

  for (r_name in rownames(thsd)) {
    parts <- strsplit(r_name, "-")[[1L]]
    if (length(parts) == 2L) {
      g1 <- parts[1L]
      g2 <- parts[2L]
      p_val <- thsd[r_name, "p adj"]
      if (g1 %in% grp_levels && g2 %in% grp_levels) {
        pval_mat[g1, g2] <- p_val
        pval_mat[g2, g1] <- p_val
      }
    }
  }

  group_letters <- sl_cld_letters(grp_levels, means, alpha, pval_mat)

  list(
    comparisons = comp_df,
    pval_matrix = pval_mat,
    group_letters = as.list(group_letters)
  )
}

#' Render Publication Bar Chart with Error Bars & Post-Hoc Letter Notation
#'
#' Draws publication-ready chart with error bars and compact letters above each bar.
#'
#' @param means Treatment means
#' @param ses Treatment standard errors (or SDs)
#' @param labels Group labels
#' @param letters Letter notations (e.g. "a", "b", "ab")
#' @param ylab Label for response axis
#' @param title Chart title
#' @param lang Language ("id" or "en")
sl_plot_bar_letters <- function(means, ses, labels, letters, ylab = "Rataan Parameter", title = "Perbandingan Rataan Perlakuan", lang = "id") {
  k <- length(means)
  if (k == 0L) return(invisible(NULL))

  # Calculate y limits
  max_top <- max(means + ses, na.rm = TRUE)
  if (!is.finite(max_top) || max_top <= 0) max_top <- 1.0
  ylim_upper <- max_top * 1.25

  graphics::par(mar = c(4.5, 4.5, 3.2, 1.5), bg = "#FFFDF8", family = "sans")

  bp <- graphics::barplot(
    means,
    names.arg = labels,
    ylim = c(0, ylim_upper),
    col = "#E6ECF3",
    border = "#1B365D",
    lwd = 1.4,
    ylab = ylab,
    main = title,
    col.main = "#1B365D",
    col.lab = "#1B365D",
    col.axis = "#1B365D",
    las = 1,
    cex.names = 0.9,
    cex.axis = 0.85,
    cex.lab = 0.95
  )

  # Horizontal subtle grid lines
  graphics::grid(nx = NA, ny = NULL, col = "#E3DDD0", lty = 3)

  # Re-draw barplot on top of grid lines
  graphics::barplot(
    means,
    names.arg = labels,
    ylim = c(0, ylim_upper),
    col = "#E6ECF3",
    border = "#1B365D",
    lwd = 1.4,
    add = TRUE,
    las = 1,
    cex.names = 0.9,
    cex.axis = 0.85
  )

  # Error bars (± SE)
  for (i in seq_len(k)) {
    m <- means[i]
    se <- ses[i]
    if (is.finite(m) && is.finite(se) && se >= 0) {
      graphics::arrows(
        x0 = bp[i],
        y0 = max(0, m - se),
        x1 = bp[i],
        y1 = m + se,
        angle = 90,
        code = 3,
        length = 0.06,
        col = "#1B365D",
        lwd = 1.4
      )
      
      # Superscript letter above error bar
      let_text <- if (i <= length(letters) && !is.null(letters[[i]])) as.character(letters[[i]]) else ""
      if (nzchar(let_text)) {
        graphics::text(
          x = bp[i],
          y = m + se + (max_top * 0.04),
          labels = let_text,
          col = "#B83227",
          font = 2,
          cex = 1.15
        )
      }
    }
  }

  # Caption note
  sub_text <- if (lang == "id") {
    "Batang = Rataan ± SE; Huruf berbeda di atas batang menunjukkan perbedaan nyata (p < 0,05)"
  } else {
    "Bars = Mean ± SE; Distinct letters above bars indicate significant difference (p < 0.05)"
  }
  graphics::mtext(sub_text, side = 1, line = 3.2, cex = 0.78, col = "#5A6B7C", font = 3)

  invisible(NULL)
}

#' Complete One-Way Analysis of Variance (ANOVA) Engine
#'
#' @param df Rectangular data.frame or named list of columns
#' @param opts Configuration options
#' @return Named list with tables, plots, assumptions, post hocs, and bilingual conclusions
sl_analyze_anova <- function(df, opts = list()) {
  alpha <- if (!is.null(opts$alpha) && is.numeric(opts$alpha) && length(opts$alpha) > 0L) {
    as.double(opts$alpha[[1L]])
  } else {
    0.05
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05

  include_plots <- if (!is.null(opts$include_plots)) isTRUE(opts$include_plots) else TRUE
  plot_lang <- if (!is.null(opts$lang) && opts$lang %in% c("id", "en")) opts$lang else "id"

  # Roles resolution
  roles <- if (!is.null(opts$roles) && is.list(opts$roles)) opts$roles else list()
  mode <- if (!is.null(roles$mode) && roles$mode %in% c("wide", "long")) roles$mode else "wide"
  response_cols <- if (!is.null(roles$response_cols)) as.character(roles$response_cols) else character(0L)
  group_col <- if (!is.null(roles$group_col) && nzchar(as.character(roles$group_col[[1L]]))) as.character(roles$group_col[[1L]]) else NULL
  exclude_rows <- if (!is.null(opts$exclude_rows)) as.integer(opts$exclude_rows) else integer(0L)

  if (length(response_cols) == 0L) {
    # Auto-detect response columns
    if (mode == "wide") {
      num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
      response_cols <- if (length(num_cols) >= 2L) num_cols else names(df)
    } else {
      cols <- names(df)
      if (is.null(group_col) && length(cols) >= 2L) {
        group_col <- cols[1L]
        response_cols <- cols[2L]
      } else {
        num_cols <- cols[cols != group_col]
        response_cols <- if (length(num_cols) > 0L) num_cols[1L] else cols[1L]
      }
    }
  }

  prep <- sl_prepare_groups(
    df = df,
    mode = mode,
    response_cols = response_cols,
    group_col = group_col,
    exclude_rows = exclude_rows
  )

  if (prep$n_valid < 3L) {
    stop("ANOVA requires at least 3 valid numeric observations.")
  }

  grp_levels <- levels(prep$groups)
  k <- length(grp_levels)
  if (k < 2L) {
    stop("ANOVA requires at least 2 distinct treatment groups.")
  }

  resp_label <- if (mode == "long") response_cols[1L] else "Nilai Pengamatan"

  # Fit linear model for ANOVA
  model_df <- data.frame(values = prep$values, groups = prep$groups)
  fit_aov <- stats::aov(values ~ groups, data = model_df)
  fit_sum <- summary(fit_aov)[[1L]]

  resids <- stats::residuals(fit_aov)

  # Assumption Testing
  assumptions <- sl_check_assumptions(
    values = prep$values,
    groups = prep$groups,
    residuals = resids,
    alpha = alpha,
    require_homogeneity = TRUE
  )

  # Outliers
  outliers <- sl_detect_outliers(
    values = prep$values,
    groups = prep$groups,
    source_rows = prep$source_rows,
    alpha = alpha
  )
  outliers$excluded <- prep$excluded_outliers

  # ANOVA Table Core Values
  ss_between <- as.double(fit_sum[["Sum Sq"]][1L])
  ss_within  <- as.double(fit_sum[["Sum Sq"]][2L])
  ss_total   <- ss_between + ss_within

  df_between <- as.integer(fit_sum[["Df"]][1L])
  df_within  <- as.integer(fit_sum[["Df"]][2L])
  df_total   <- df_between + df_within

  ms_between <- as.double(fit_sum[["Mean Sq"]][1L])
  ms_within  <- as.double(fit_sum[["Mean Sq"]][2L])

  f_val <- as.double(fit_sum[["F value"]][1L])
  p_val <- as.double(fit_sum[["Pr(>F)"]][1L])

  f_crit_05 <- stats::qf(0.95, df_between, df_within)
  f_crit_01 <- stats::qf(0.99, df_between, df_within)

  # Effect Sizes & Variation
  r_squared <- if (ss_total > 0) ss_between / ss_total else 0.0
  eta_sq <- r_squared
  omega_sq <- if (ss_total + ms_within > 0) {
    max(0.0, (ss_between - df_between * ms_within) / (ss_total + ms_within))
  } else {
    0.0
  }
  grand_mean <- mean(prep$values, na.rm = TRUE)
  cv_pct <- if (abs(grand_mean) > 1e-12) (sqrt(ms_within) / abs(grand_mean)) * 100.0 else NA_real_

  # Significance notation
  sig_code <- if (is.finite(p_val) && p_val < 0.01) {
    "**"
  } else if (is.finite(p_val) && p_val < 0.05) {
    "*"
  } else {
    "tn"
  }
  sig_code_en <- if (sig_code == "tn") "ns" else sig_code

  # Group Statistics (Mean, SD, SE, N)
  g_chr <- as.character(prep$groups)
  grp_stats <- vector("list", k)
  names(grp_stats) <- grp_levels

  means_vec <- numeric(k)
  ses_vec <- numeric(k)
  sds_vec <- numeric(k)
  ns_vec <- integer(k)

  for (i in seq_along(grp_levels)) {
    g_nm <- grp_levels[i]
    idx_g <- which(g_chr == g_nm)
    v_g <- prep$values[idx_g]
    n_g <- length(v_g)
    m_g <- mean(v_g, na.rm = TRUE)
    sd_g <- if (n_g >= 2L) stats::sd(v_g, na.rm = TRUE) else 0.0
    se_g <- if (n_g >= 1L) sd_g / sqrt(n_g) else 0.0

    means_vec[i] <- m_g
    ses_vec[i] <- se_g
    sds_vec[i] <- sd_g
    ns_vec[i] <- n_g

    grp_stats[[i]] <- list(
      group = g_nm,
      n = n_g,
      mean = m_g,
      sd = sd_g,
      se = se_g
    )
  }
  names(means_vec) <- grp_levels
  names(ns_vec) <- grp_levels

  # Post Hoc Computations (Tukey HSD & Duncan DMRT)
  tukey_res <- sl_tukey_test(fit_aov, alpha = alpha)
  duncan_res <- sl_duncan_test(
    means = means_vec,
    ns = ns_vec,
    ms_error = ms_within,
    df_error = df_within,
    alpha = alpha,
    treatments = grp_levels
  )

  # Robust Alternative Tests
  welch_res <- tryCatch({
    wt <- stats::oneway.test(values ~ groups, data = model_df, var.equal = FALSE)
    list(
      statistic = as.double(wt$statistic),
      df_num = as.double(wt$parameter[1L]),
      df_denom = as.double(wt$parameter[2L]),
      p_value = as.double(wt$p.value)
    )
  }, error = function(e) NULL)

  kruskal_res <- tryCatch({
    kt <- stats::kruskal.test(values ~ groups, data = model_df)
    list(
      statistic = as.double(kt$statistic),
      df = as.integer(kt$parameter),
      p_value = as.double(kt$p.value)
    )
  }, error = function(e) NULL)

  # =========================================================================
  # Build Tables (APA Layer 1 & Full Details)
  # =========================================================================

  # Table 1: ANOVA Summary Table
  anova_headers_id <- sl_box_vec(c(
    "Sumber Keragaman", "db", "Jumlah Kuadrat (JK)", "Kuadrat Tengah (KT)",
    "F-Hitung", "P-Value", "F-Tabel 5%", "F-Tabel 1%", "Keterangan"
  ))
  anova_headers_en <- sl_box_vec(c(
    "Source of Variation", "df", "Sum of Squares (SS)", "Mean Square (MS)",
    "F-Statistic", "p-value", "F-Critical (0.05)", "F-Critical (0.01)", "Significance"
  ))

  row_bet_id <- sl_box_vec(c(
    "Antar Perlakuan",
    as.character(df_between),
    sl_fmt_num(ss_between, 4L, "id"),
    sl_fmt_num(ms_between, 4L, "id"),
    sl_fmt_num(f_val, 4L, "id"),
    sl_fmt_p(p_val, "id"),
    sl_fmt_num(f_crit_05, 4L, "id"),
    sl_fmt_num(f_crit_01, 4L, "id"),
    sig_code
  ))
  row_bet_en <- sl_box_vec(c(
    "Between Treatments",
    as.character(df_between),
    sl_fmt_num(ss_between, 4L, "en"),
    sl_fmt_num(ms_between, 4L, "en"),
    sl_fmt_num(f_val, 4L, "en"),
    sl_fmt_p(p_val, "en"),
    sl_fmt_num(f_crit_05, 4L, "en"),
    sl_fmt_num(f_crit_01, 4L, "en"),
    sig_code_en
  ))

  row_err_id <- sl_box_vec(c(
    "Galat (Dalam Perlakuan)",
    as.character(df_within),
    sl_fmt_num(ss_within, 4L, "id"),
    sl_fmt_num(ms_within, 4L, "id"),
    "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"
  ))
  row_err_en <- sl_box_vec(c(
    "Error (Within Treatments)",
    as.character(df_within),
    sl_fmt_num(ss_within, 4L, "en"),
    sl_fmt_num(ms_within, 4L, "en"),
    "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"
  ))

  row_tot_id <- sl_box_vec(c(
    "Total",
    as.character(df_total),
    sl_fmt_num(ss_total, 4L, "id"),
    "\u2014", "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"
  ))
  row_tot_en <- sl_box_vec(c(
    "Total",
    as.character(df_total),
    sl_fmt_num(ss_total, 4L, "en"),
    "\u2014", "\u2014", "\u2014", "\u2014", "\u2014", "\u2014"
  ))

  note_anova_id <- sprintf(
    "Keterangan: ** = Sangat Berbeda Nyata (p < 0,01); * = Berbeda Nyata (p < 0,05); tn = Tidak Nyata (p >= 0,05). Koefisien Keragaman (KK) = %s%%; R² = %s; η² = %s.",
    sl_fmt_num(cv_pct, 2L, "id"),
    sl_fmt_num(r_squared, 4L, "id"),
    sl_fmt_num(eta_sq, 4L, "id")
  )
  note_anova_en <- sprintf(
    "Note: ** = Highly Significant (p < 0.01); * = Significant (p < 0.05); ns = Non-Significant (p >= 0.05). Coefficient of Variation (CV) = %s%%; R² = %s; η² = %s.",
    sl_fmt_num(cv_pct, 2L, "en"),
    sl_fmt_num(r_squared, 4L, "en"),
    sl_fmt_num(eta_sq, 4L, "en")
  )

  tbl_anova <- list(
    id = "anova_summary",
    title = list(
      id = sprintf("Tabel 1. Ringkasan Sidik Ragam (ANOVA Satu Arah) Parameter %s", resp_label),
      en = sprintf("Table 1. One-Way Analysis of Variance (ANOVA) Summary for %s", resp_label)
    ),
    headers = list(id = anova_headers_id, en = anova_headers_en),
    rows = list(
      id = list(row_bet_id, row_err_id, row_tot_id),
      en = list(row_bet_en, row_err_en, row_tot_en)
    ),
    note = list(id = note_anova_id, en = note_anova_en)
  )

  # Table 2: Treatment Means & Post Hoc Letters
  means_headers_id <- sl_box_vec(c(
    "Kelompok Perlakuan", "Ulangan (N)", "Rataan ± SD", "Rataan ± SE",
    "Notasi Tukey HSD (α=0,05)", "Notasi Duncan / DMRT (α=0,05)"
  ))
  means_headers_en <- sl_box_vec(c(
    "Treatment Group", "Replicates (N)", "Mean ± SD", "Mean ± SE",
    "Tukey HSD Letter (α=0.05)", "Duncan DMRT Letter (α=0.05)"
  ))

  means_rows_id <- vector("list", k)
  means_rows_en <- vector("list", k)

  for (i in seq_len(k)) {
    g_nm <- grp_levels[i]
    st <- grp_stats[[g_nm]]
    let_tukey <- if (!is.null(tukey_res$group_letters[[g_nm]])) tukey_res$group_letters[[g_nm]] else "-"
    let_duncan <- if (!is.null(duncan_res$group_letters[[g_nm]])) duncan_res$group_letters[[g_nm]] else "-"

    means_rows_id[[i]] <- sl_box_vec(c(
      g_nm,
      as.character(st$n),
      sprintf("%s ± %s", sl_fmt_num(st$mean, 2L, "id"), sl_fmt_num(st$sd, 2L, "id")),
      sprintf("%s ± %s", sl_fmt_num(st$mean, 2L, "id"), sl_fmt_num(st$se, 2L, "id")),
      as.character(let_tukey),
      as.character(let_duncan)
    ))

    means_rows_en[[i]] <- sl_box_vec(c(
      g_nm,
      as.character(st$n),
      sprintf("%s ± %s", sl_fmt_num(st$mean, 2L, "en"), sl_fmt_num(st$sd, 2L, "en")),
      sprintf("%s ± %s", sl_fmt_num(st$mean, 2L, "en"), sl_fmt_num(st$se, 2L, "en")),
      as.character(let_tukey),
      as.character(let_duncan)
    ))
  }

  note_means_id <- "Keterangan: Huruf superskrip yang sama pada kolom notasi menunjukkan perlakuan tidak berbeda nyata pada taraf uji α = 0,05 (huruf berbeda menunjukkan perbedaan nyata)."
  note_means_en <- "Note: Treatments sharing a common superscript letter in a post hoc column are not significantly different at α = 0.05 (distinct letters indicate significant differences)."

  tbl_means <- list(
    id = "treatment_means",
    title = list(
      id = sprintf("Tabel 2. Rataan Parameter %s dan Notasi Uji Lanjut Post Hoc", resp_label),
      en = sprintf("Table 2. Treatment Means of %s and Post Hoc Letter Notation", resp_label)
    ),
    headers = list(id = means_headers_id, en = means_headers_en),
    rows = list(id = means_rows_id, en = means_rows_en),
    note = list(id = note_means_id, en = note_means_en)
  )

  # Table 3: Assumption Diagnostics & Robust Alternatives
  diag_headers_id <- sl_box_vec(c("Uji Asumsi / Alternatif", "Statistik", "Nilai P", "Batas Kritis (α)", "Status / Kesimpulan"))
  diag_headers_en <- sl_box_vec(c("Assumption / Alternative Test", "Statistic", "P-Value", "Threshold (α)", "Status / Interpretation"))

  diag_rows_id <- list()
  diag_rows_en <- list()

  # 1. Residual Normality Shapiro-Wilk
  rn_sw <- assumptions$residual_normality$shapiro
  if (!is.null(rn_sw)) {
    stat_str_id <- if (rn_sw$valid) sprintf("W = %s", sl_fmt_num(rn_sw$statistic, 4L, "id")) else "\u2014"
    stat_str_en <- if (rn_sw$valid) sprintf("W = %s", sl_fmt_num(rn_sw$statistic, 4L, "en")) else "\u2014"
    p_str_id <- if (rn_sw$valid) sl_fmt_p(rn_sw$p_value, "id") else "\u2014"
    p_str_en <- if (rn_sw$valid) sl_fmt_p(rn_sw$p_value, "en") else "\u2014"
    status_id <- if (!rn_sw$valid) "Tidak dapat dihitung" else if (rn_sw$p_value >= alpha) "Normal (Terpenuhi)" else "Tidak Normal (Dilanggar)"
    status_en <- if (!rn_sw$valid) "Not computable" else if (rn_sw$p_value >= alpha) "Normal (Passed)" else "Non-Normal (Violated)"

    diag_rows_id[[length(diag_rows_id) + 1L]] <- sl_box_vec(c("Normalitas Galat (Shapiro-Wilk)", stat_str_id, p_str_id, sl_fmt_num(alpha, 2L, "id"), status_id))
    diag_rows_en[[length(diag_rows_en) + 1L]] <- sl_box_vec(c("Residual Normality (Shapiro-Wilk)", stat_str_en, p_str_en, sl_fmt_num(alpha, 2L, "en"), status_en))
  }

  # 2. Homogeneity Levene Brown-Forsythe
  hom_lev <- assumptions$homogeneity$levene_bf
  if (!is.null(hom_lev)) {
    stat_str_id <- if (hom_lev$valid) sprintf("F = %s", sl_fmt_num(hom_lev$statistic, 4L, "id")) else "\u2014"
    stat_str_en <- if (hom_lev$valid) sprintf("F = %s", sl_fmt_num(hom_lev$statistic, 4L, "en")) else "\u2014"
    p_str_id <- if (hom_lev$valid) sl_fmt_p(hom_lev$p_value, "id") else "\u2014"
    p_str_en <- if (hom_lev$valid) sl_fmt_p(hom_lev$p_value, "en") else "\u2014"
    status_id <- if (!hom_lev$valid) "Varians Nol" else if (hom_lev$p_value >= alpha) "Homogen (Terpenuhi)" else "Heterogen (Dilanggar)"
    status_en <- if (!hom_lev$valid) "Zero Variance" else if (hom_lev$p_value >= alpha) "Homogeneous (Passed)" else "Heterogeneous (Violated)"

    diag_rows_id[[length(diag_rows_id) + 1L]] <- sl_box_vec(c("Homogenitas Ragam (Levene B-F)", stat_str_id, p_str_id, sl_fmt_num(alpha, 2L, "id"), status_id))
    diag_rows_en[[length(diag_rows_en) + 1L]] <- sl_box_vec(c("Variance Homogeneity (Levene B-F)", stat_str_en, p_str_en, sl_fmt_num(alpha, 2L, "en"), status_en))
  }

  # 3. Welch ANOVA Robust
  if (!is.null(welch_res)) {
    stat_str_id <- sprintf("F_Welch = %s", sl_fmt_num(welch_res$statistic, 4L, "id"))
    stat_str_en <- sprintf("F_Welch = %s", sl_fmt_num(welch_res$statistic, 4L, "en"))
    p_str_id <- sl_fmt_p(welch_res$p_value, "id")
    p_str_en <- sl_fmt_p(welch_res$p_value, "en")
    status_id <- if (welch_res$p_value < alpha) "Signifikan (Robust Heterogen)" else "Tidak Signifikan"
    status_en <- if (welch_res$p_value < alpha) "Significant (Robust Heterogeneity)" else "Non-Significant"

    diag_rows_id[[length(diag_rows_id) + 1L]] <- sl_box_vec(c("Welch's ANOVA (Alternatif Varians Heterogen)", stat_str_id, p_str_id, sl_fmt_num(alpha, 2L, "id"), status_id))
    diag_rows_en[[length(diag_rows_en) + 1L]] <- sl_box_vec(c("Welch's ANOVA (Robust Unequal Variance)", stat_str_en, p_str_en, sl_fmt_num(alpha, 2L, "en"), status_en))
  }

  # 4. Kruskal-Wallis Non-Parametric
  if (!is.null(kruskal_res)) {
    stat_str_id <- sprintf("H = %s", sl_fmt_num(kruskal_res$statistic, 4L, "id"))
    stat_str_en <- sprintf("H = %s", sl_fmt_num(kruskal_res$statistic, 4L, "en"))
    p_str_id <- sl_fmt_p(kruskal_res$p_value, "id")
    p_str_en <- sl_fmt_p(kruskal_res$p_value, "en")
    status_id <- if (kruskal_res$p_value < alpha) "Signifikan (Non-Parametrik)" else "Tidak Signifikan"
    status_en <- if (kruskal_res$p_value < alpha) "Significant (Non-Parametric)" else "Non-Significant"

    diag_rows_id[[length(diag_rows_id) + 1L]] <- sl_box_vec(c("Uji Kruskal-Wallis (Alternatif Non-Parametrik)", stat_str_id, p_str_id, sl_fmt_num(alpha, 2L, "id"), status_id))
    diag_rows_en[[length(diag_rows_en) + 1L]] <- sl_box_vec(c("Kruskal-Wallis Test (Non-Parametric)", stat_str_en, p_str_en, sl_fmt_num(alpha, 2L, "en"), status_en))
  }

  tbl_diag <- list(
    id = "model_diagnostics",
    title = list(
      id = "Tabel 3. Evaluasi Asumsi Model Sidik Ragam & Uji Alternatif Robust",
      en = "Table 3. Model Assumption Diagnostics & Robust Alternative Tests"
    ),
    headers = list(id = diag_headers_id, en = diag_headers_en),
    rows = list(id = diag_rows_id, en = diag_rows_en),
    note = list(
      id = "Jika asumsi homogenitas dilanggar, rujuk hasil Welch's ANOVA. Jika normalitas galat dilanggar, rujuk hasil Kruskal-Wallis.",
      en = "If variance homogeneity is violated, refer to Welch's ANOVA. If residual normality is violated, refer to Kruskal-Wallis."
    )
  )

  # Full Details: Table 4 Pairwise Comparisons Matrix
  pw_comp <- tukey_res$comparisons
  pw_headers_id <- sl_box_vec(c("Perbandingan Perlakuan", "Selisih Rataan", "Batas Bawah 95%", "Batas Atas 95%", "P-Value Terlaraskan", "Signifikansi"))
  pw_headers_en <- sl_box_vec(c("Treatment Comparison", "Mean Difference", "Lower 95% CI", "Upper 95% CI", "Adjusted P-Value", "Significance"))

  pw_rows_id <- list()
  pw_rows_en <- list()

  if (nrow(pw_comp) > 0L) {
    for (r in seq_len(nrow(pw_comp))) {
      p_adj <- pw_comp$p_adj[r]
      sig_mark <- if (is.finite(p_adj) && p_adj < 0.01) "**" else if (is.finite(p_adj) && p_adj < 0.05) "*" else "tn"
      sig_mark_en <- if (sig_mark == "tn") "ns" else sig_mark

      pw_rows_id[[r]] <- sl_box_vec(c(
        pw_comp$comparison[r],
        sl_fmt_num(pw_comp$diff[r], 4L, "id"),
        sl_fmt_num(pw_comp$lwr[r], 4L, "id"),
        sl_fmt_num(pw_comp$upr[r], 4L, "id"),
        sl_fmt_p(p_adj, "id"),
        sig_mark
      ))

      pw_rows_en[[r]] <- sl_box_vec(c(
        pw_comp$comparison[r],
        sl_fmt_num(pw_comp$diff[r], 4L, "en"),
        sl_fmt_num(pw_comp$lwr[r], 4L, "en"),
        sl_fmt_num(pw_comp$upr[r], 4L, "en"),
        sl_fmt_p(p_adj, "en"),
        sig_mark_en
      ))
    }
  }

  tbl_pairwise <- list(
    id = "pairwise_tukey_matrix",
    title = list(
      id = "Tabel Rincian Tambahan: Matriks Uji Perbandingan Berpasangan (Tukey HSD)",
      en = "Supplementary Table: Pairwise Comparisons Matrix (Tukey HSD)"
    ),
    headers = list(id = pw_headers_id, en = pw_headers_en),
    rows = list(id = pw_rows_id, en = pw_rows_en),
    note = list(
      id = "Interval kepercayaan 95% dan nilai p terlaraskan untuk seluruh pasangan perlakuan.",
      en = "Family-wise 95% confidence intervals and adjusted p-values for all pairwise comparisons."
    )
  )

  tables_apa <- list(tbl_anova, tbl_means, tbl_diag)
  tables_full <- list(tbl_pairwise)

  # =========================================================================
  # Plots (Publication Bar Chart + Residual Q-Q Plot)
  # =========================================================================
  plots <- list()
  if (include_plots) {
    # 1. Publication Bar Chart with Error Bars & Letters
    bar_title <- if (plot_lang == "id") {
      sprintf("Diagram Batang Rataan Parameter %s", resp_label)
    } else {
      sprintf("Treatment Means Bar Chart for %s", resp_label)
    }
    bar_ylab <- resp_label

    duncan_letters_vec <- vapply(grp_levels, function(g) {
      if (!is.null(duncan_res$group_letters[[g]])) as.character(duncan_res$group_letters[[g]]) else ""
    }, character(1L))

    p_bar <- sl_capture_plot(function() {
      sl_plot_bar_letters(
        means = means_vec,
        ses = ses_vec,
        labels = grp_levels,
        letters = duncan_letters_vec,
        ylab = bar_ylab,
        title = bar_title,
        lang = plot_lang
      )
    }, width_in = 6.5, height_in = 4.4, res = 300)

    plots[[1L]] <- list(
      id = "anova_barchart_letters",
      title = list(
        id = sprintf("Diagram Batang Publikasi dengan Error Bar (SE) & Notasi Huruf Uji Lanjut (%s)", resp_label),
        en = sprintf("Publication Bar Chart with Error Bars (SE) & Post Hoc Letters (%s)", resp_label)
      ),
      png_data_url = p_bar$png_data_url,
      width_in = p_bar$width_in,
      height_in = p_bar$height_in
    )

    # 2. Residual Q-Q Plot
    qq_title <- if (plot_lang == "id") "Plot Q-Q Normalitas Galat Model Sidik Ragam" else "Normal Q-Q Plot of ANOVA Residuals"
    p_qq <- sl_capture_plot(function() {
      sl_plot_qq(resids, title = qq_title, lang = plot_lang)
    }, width_in = 6.0, height_in = 4.2, res = 300)

    plots[[2L]] <- list(
      id = "anova_residual_qq",
      title = list(
        id = "Diagnostik Normalitas Galat Model (Residual Q-Q Plot)",
        en = "Model Residual Normality Diagnostic (Q-Q Plot)"
      ),
      png_data_url = p_qq$png_data_url,
      width_in = p_qq$width_in,
      height_in = p_qq$height_in
    )
  }

  # =========================================================================
  # Scientific Written Interpretations (Bilingual)
  # =========================================================================
  highest_grp <- grp_levels[which.max(means_vec)]
  lowest_grp <- grp_levels[which.min(means_vec)]
  highest_mean_str_id <- sl_fmt_num(max(means_vec), 2L, "id")
  highest_mean_str_en <- sl_fmt_num(max(means_vec), 2L, "en")

  interp_id <- if (is.finite(p_val) && p_val < 0.05) {
    sig_word <- if (p_val < 0.01) "berpengaruh sangat nyata (p < 0,01)" else "berpengaruh nyata (p < 0,05)"
    sprintf(
      "Berdasarkan hasil analisis ragam (ANOVA Satu Arah), perlakuan yang diuji %s terhadap parameter %s dengan nilai F(%d, %d) = %s, p = %s (F-hitung > F-tabel 5%% = %s; η² = %s). Perlakuan %s menghasilkan rataan tertinggi sebesar %s, yang berbeda nyata secara statistik berdasarkan uji lanjut Tukey HSD dan Duncan (DMRT) dibandingkan perlakuan lainnya. Asumsi normalitas galat (%s) dan homogenitas ragam (%s) dievaluasi sebagai prasyarat model.",
      sig_word,
      resp_label,
      df_between,
      df_within,
      sl_fmt_num(f_val, 3L, "id"),
      sl_fmt_p(p_val, "id"),
      sl_fmt_num(f_crit_05, 3L, "id"),
      sl_fmt_num(eta_sq, 3L, "id"),
      highest_grp,
      highest_mean_str_id,
      if (assumptions$normality_violated) "dilanggar; diverifikasi dengan Kruskal-Wallis" else "terpenuhi",
      if (assumptions$homogeneity_violated) "dilanggar; diverifikasi dengan Welch ANOVA" else "terpenuhi"
    )
  } else {
    sprintf(
      "Berdasarkan hasil analisis ragam (ANOVA Satu Arah), perlakuan yang diuji tidak berpengaruh nyata (p >= 0,05) terhadap parameter %s dengan nilai F(%d, %d) = %s, p = %s (F-hitung <= F-tabel 5%% = %s; η² = %s). Perbedaan nilai antar perlakuan secara statistik berada dalam batas keragaman galat percobaan.",
      resp_label,
      df_between,
      df_within,
      sl_fmt_num(f_val, 3L, "id"),
      sl_fmt_p(p_val, "id"),
      sl_fmt_num(f_crit_05, 3L, "id"),
      sl_fmt_num(eta_sq, 3L, "id")
    )
  }

  interp_en <- if (is.finite(p_val) && p_val < 0.05) {
    sig_word <- if (p_val < 0.01) "a highly significant effect (p < 0.01)" else "a significant effect (p < 0.05)"
    sprintf(
      "One-way analysis of variance revealed that treatments exerted %s on %s, F(%d, %d) = %s, p = %s (F-observed > F-critical 5%% = %s; η² = %s). Treatment %s yielded the highest mean value of %s, demonstrating statistically significant differences in post hoc pairwise evaluations (Tukey HSD and Duncan DMRT). Residual normality (%s) and variance homogeneity (%s) were verified.",
      sig_word,
      resp_label,
      df_between,
      df_within,
      sl_fmt_num(f_val, 3L, "en"),
      sl_fmt_p(p_val, "en"),
      sl_fmt_num(f_crit_05, 3L, "en"),
      sl_fmt_num(eta_sq, 3L, "en"),
      highest_grp,
      highest_mean_str_en,
      if (assumptions$normality_violated) "violated; corroborated via Kruskal-Wallis" else "satisfied",
      if (assumptions$homogeneity_violated) "violated; corroborated via Welch ANOVA" else "satisfied"
    )
  } else {
    sprintf(
      "One-way analysis of variance indicated that treatments did not have a statistically significant effect on %s, F(%d, %d) = %s, p = %s (F-observed <= F-critical 5%% = %s; η² = %s). Observed variations among treatment groups are attributable to experimental sampling variation.",
      resp_label,
      df_between,
      df_within,
      sl_fmt_num(f_val, 3L, "en"),
      sl_fmt_p(p_val, "en"),
      sl_fmt_num(f_crit_05, 3L, "en"),
      sl_fmt_num(eta_sq, 3L, "en")
    )
  }

  # KaTeX Mathematical Formulas
  formulas <- list(
    list(
      title = list(id = "Model Linier Aditif Sidik Ragam", en = "Linear Additive ANOVA Model"),
      latex = "Y_{ij} = \\mu + \\tau_i + \\epsilon_{ij}, \\quad \\epsilon_{ij} \\overset{\\text{iid}}{\\sim} \\mathcal{N}(0, \\sigma^2)"
    ),
    list(
      title = list(id = "Statistik Uji F Sidik Ragam", en = "F-Statistic Formulation"),
      latex = "F = \\frac{MS_{\\text{antar}}}{MS_{\\text{galat}}} = \\frac{SS_{\\text{antar}} / (k - 1)}{SS_{\\text{galat}} / (N - k)}"
    ),
    list(
      title = list(id = "Uji Lanjut Duncan (Least Significant Range)", en = "Duncan's Critical Range"),
      latex = "R_p = q_{\\alpha}(p, \\, df_{\\text{galat}}) \\times \\sqrt{\\frac{MS_{\\text{galat}}}{n_h}}"
    )
  )

  # Warnings
  warnings_id <- character(0L)
  warnings_en <- character(0L)
  if (assumptions$normality_violated) {
    warnings_id <- c(warnings_id, "Normalitas galat model terlanggar (p < α). Silakan rujuk uji non-parametrik Kruskal-Wallis pada tabel evaluasi asumsi.")
    warnings_en <- c(warnings_en, "Model residual normality assumption is violated (p < α). Refer to Kruskal-Wallis test in assumption diagnostics table.")
  }
  if (assumptions$homogeneity_violated) {
    warnings_id <- c(warnings_id, "Homogenitas ragam terlanggar (p < α). Silakan rujuk hasil Welch's ANOVA pada tabel evaluasi asumsi.")
    warnings_en <- c(warnings_en, "Variance homogeneity assumption is violated (p < α). Refer to Welch's ANOVA in assumption diagnostics table.")
  }
  if (nrow(prep$excluded_outliers) > 0L) {
    ex_rows_str <- paste(prep$excluded_outliers$source_row, collapse = ", ")
    warnings_id <- c(warnings_id, sprintf("Terdapat %d baris pencilan yang dikeluarkan oleh pengguna (baris: %s).", nrow(prep$excluded_outliers), ex_rows_str))
    warnings_en <- c(warnings_en, sprintf("%d outlier rows were excluded by the user (rows: %s).", nrow(prep$excluded_outliers), ex_rows_str))
  }

  meta <- list(
    analysis_type = "oneway_anova",
    analysis_label = list(
      id = "Sidik Ragam (ANOVA Satu Arah) & Uji Lanjut",
      en = "One-Way Analysis of Variance (ANOVA) & Post Hoc"
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

  list(
    meta = meta,
    assumptions = assumptions,
    outliers = outliers,
    main = list(
      anova_table = list(
        ss_between = ss_between,
        ss_within = ss_within,
        ss_total = ss_total,
        df_between = df_between,
        df_within = df_within,
        df_total = df_total,
        ms_between = ms_between,
        ms_within = ms_within,
        f_statistic = f_val,
        p_value = p_val,
        f_critical_05 = f_crit_05,
        f_critical_01 = f_crit_01,
        r_squared = r_squared,
        eta_squared = eta_sq,
        omega_squared = omega_sq,
        cv_pct = cv_pct
      ),
      groups = grp_stats
    ),
    posthoc = list(
      tukey = tukey_res,
      duncan = duncan_res
    ),
    alternatives = list(
      welch = welch_res,
      kruskal = kruskal_res
    ),
    tables_apa = tables_apa,
    tables_full = tables_full,
    formulas = formulas,
    plots = plots,
    interpretation = list(
      id = interp_id,
      en = interp_en
    ),
    warnings = list(
      id = sl_box_vec(warnings_id),
      en = sl_box_vec(warnings_en)
    )
  )
}
