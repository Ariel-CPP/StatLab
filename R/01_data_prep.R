# R/01_data_prep.R
# StatLab Data Preparation, Wide/Long Group Reshaping & Non-Destructive Outlier Detection
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base and stats functions (zero external/CRAN packages).

.sl_missing_tokens <- c("", "na", "n/a", "-")

.sl_trim <- function(x) {
  gsub("^[[:space:]\u00a0]+|[[:space:]\u00a0]+$", "", enc2utf8(as.character(x)), perl = TRUE)
}

#' Parse a candidate numeric vector with Indonesian comma-decimal & dirty-cell audit
#'
#' @param col_vec Atomic vector (numeric, character, factor, logical)
#' @return Named list with `values` (double), `is_blank` (logical),
#'   `is_missing` (logical), and `is_coerced` (logical)
.sl_parse_numeric_vec <- function(col_vec) {
  n <- length(col_vec)
  if (n == 0L) {
    return(list(
      values = numeric(0L),
      is_blank = logical(0L),
      is_missing = logical(0L),
      is_coerced = logical(0L)
    ))
  }

  if (is.numeric(col_vec)) {
    raw_na <- is.na(col_vec)
    vals <- as.double(col_vec)
    non_fin <- !is.finite(vals)
    if (any(non_fin)) {
      vals[non_fin] <- NA_real_
    }
    return(list(
      values = vals,
      is_blank = raw_na,
      is_missing = non_fin,
      is_coerced = rep(FALSE, n)
    ))
  }

  raw_na <- is.na(col_vec)
  vals <- rep(NA_real_, n)
  is_blank <- raw_na
  is_coerced <- rep(FALSE, n)

  idx <- which(!raw_na)
  if (length(idx) > 0L) {
    s <- .sl_trim(as.character(col_vec[idx]))
    blank_s <- !nzchar(s)
    if (any(blank_s)) {
      is_blank[idx[blank_s]] <- TRUE
    }

    std_miss <- tolower(s) %in% .sl_missing_tokens
    cand_pos <- which(!std_miss)
    if (length(cand_pos) > 0L) {
      cs <- s[cand_pos]
      norm <- rep(NA_character_, length(cs))

      # 1. Standard dot-decimal, integer, or scientific notation
      m_dot <- grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", cs)
      if (any(m_dot)) {
        norm[m_dot] <- cs[m_dot]
      }

      # 2. Indonesian comma-decimal without thousands dots (e.g. "12,5", "-0,75", "1,5e-3")
      rem <- is.na(norm)
      if (any(rem)) {
        m_com <- rem & grepl("^[+-]?([0-9]+(,[0-9]*)?|,[0-9]+)([eE][+-]?[0-9]+)?$", cs)
        if (any(m_com)) {
          norm[m_com] <- sub(",", ".", cs[m_com], fixed = TRUE)
        }
      }

      # 3. Indonesian thousands-dot + comma-decimal (e.g. "1.234,56", "-12.345.678,9")
      rem <- is.na(norm)
      if (any(rem)) {
        m_id_th <- rem & grepl("^[+-]?[0-9]{1,3}(\\.[0-9]{3})+,[0-9]+([eE][+-]?[0-9]+)?$", cs)
        if (any(m_id_th)) {
          tmp <- gsub(".", "", cs[m_id_th], fixed = TRUE)
          norm[m_id_th] <- sub(",", ".", tmp, fixed = TRUE)
        }
      }

      # 4. English thousands-comma + dot-decimal (e.g. "1,234.56")
      rem <- is.na(norm)
      if (any(rem)) {
        m_en_th <- rem & grepl("^[+-]?[0-9]{1,3}(,[0-9]{3})+\\.[0-9]+([eE][+-]?[0-9]+)?$", cs)
        if (any(m_en_th)) {
          norm[m_en_th] <- gsub(",", "", cs[m_en_th], fixed = TRUE)
        }
      }

      # 5. Unambiguous multi-separator thousands integer (2+ 3-digit groups, e.g. "1.000.000")
      rem <- is.na(norm)
      if (any(rem)) {
        m_dot_int <- rem & grepl("^[+-]?[0-9]{1,3}(\\.[0-9]{3}){2,}$", cs)
        if (any(m_dot_int)) {
          norm[m_dot_int] <- gsub(".", "", cs[m_dot_int], fixed = TRUE)
        }
        m_com_int <- rem & !m_dot_int & grepl("^[+-]?[0-9]{1,3}(,[0-9]{3}){2,}$", cs)
        if (any(m_com_int)) {
          norm[m_com_int] <- gsub(",", "", cs[m_com_int], fixed = TRUE)
        }
      }

      parsed_num <- rep(NA_real_, length(cs))
      has_norm <- !is.na(norm)
      if (any(has_norm)) {
        parsed_num[has_norm] <- suppressWarnings(as.double(norm[has_norm]))
      }

      ok_num <- is.finite(parsed_num)
      full_idx <- idx[cand_pos]
      if (any(ok_num)) {
        vals[full_idx[ok_num]] <- parsed_num[ok_num]
      }
      if (any(!ok_num)) {
        is_coerced[full_idx[!ok_num]] <- TRUE
      }
    }
  }

  list(
    values = vals,
    is_blank = is_blank,
    is_missing = is.na(vals),
    is_coerced = is_coerced
  )
}

#' Parse a categorical group column in long mode, identifying missing tokens
#'
#' @param grp_vec Atomic vector or factor of group labels
#' @return Named list with `labels` (character with NA for missing) and `is_missing` (logical)
.sl_parse_group_vec <- function(grp_vec) {
  n <- length(grp_vec)
  if (n == 0L) {
    return(list(labels = character(0L), is_missing = logical(0L)))
  }
  raw_na <- is.na(grp_vec)
  labels <- rep(NA_character_, n)
  idx <- which(!raw_na)
  if (length(idx) > 0L) {
    s <- .sl_trim(as.character(grp_vec[idx]))
    miss <- tolower(s) %in% .sl_missing_tokens
    if (any(!miss)) {
      labels[idx[!miss]] <- s[!miss]
    }
  }
  list(labels = labels, is_missing = is.na(labels))
}

#' Reshape wide or long table columns into clean grouped observations with NA/dirty-cell audit
#'
#' @param df data.frame (or named list of equal-length columns)
#' @param mode `"wide"` (each response column is a group) or `"long"` (single response + group_col)
#' @param response_cols Character vector of response column name(s) in `df`
#' @param group_col Optional character(1) group column name in `df` when `mode == "long"`
#' @param exclude_rows Optional integer vector of 1-based `df` row indices explicitly excluded by user
#' @return Structured list with `values`, `groups`, `source_rows`, `n_raw_obs`, `n_valid`,
#'   `n_missing`, `coerced_non_numeric`, and `excluded_outliers`
sl_prepare_groups <- function(
  df,
  mode = c("wide", "long"),
  response_cols,
  group_col = NULL,
  exclude_rows = integer(0)
) {
  if (!is.data.frame(df)) {
    if (is.list(df) && !is.null(names(df))) {
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      stop("`df` must be a data.frame.")
    }
  }

  mode <- match.arg(mode)

  if (missing(response_cols) || is.null(response_cols) || length(response_cols) == 0L) {
    stop("`response_cols` must name at least one column in `df`.")
  }
  if (is.numeric(response_cols)) {
    response_cols <- names(df)[as.integer(response_cols)]
  }
  response_cols <- as.character(response_cols)
  miss_cols <- setdiff(response_cols, names(df))
  if (length(miss_cols) > 0L) {
    stop(sprintf("Column(s) not found in `df`: %s", paste(miss_cols, collapse = ", ")))
  }

  excl_set <- if (is.null(exclude_rows) || length(exclude_rows) == 0L) {
    integer(0L)
  } else {
    unique(as.integer(exclude_rows[!is.na(exclude_rows)]))
  }

  nr <- nrow(df)

  if (mode == "wide") {
    response_cols <- unique(response_cols)
    val_chunks <- vector("list", length(response_cols))
    grp_chunks <- vector("list", length(response_cols))
    row_chunks <- vector("list", length(response_cols))
    coerced_rows_list <- vector("list", length(response_cols))
    coerced_cols_list <- vector("list", length(response_cols))

    n_raw_obs <- 0L
    n_missing <- 0L

    for (j in seq_along(response_cols)) {
      col_nm <- response_cols[j]
      parsed <- .sl_parse_numeric_vec(df[[col_nm]])

      occ_idx <- which(!parsed$is_blank)
      last_occ <- if (length(occ_idx) > 0L) max(occ_idx) else 0L

      if (last_occ > 0L) {
        active <- seq_len(last_occ)
        n_raw_obs <- n_raw_obs + last_occ

        miss_active <- parsed$is_missing[active]
        n_missing <- n_missing + as.integer(sum(miss_active))

        coerced_active <- which(parsed$is_coerced[active])
        if (length(coerced_active) > 0L) {
          coerced_rows_list[[j]] <- as.integer(coerced_active)
          coerced_cols_list[[j]] <- rep(col_nm, length(coerced_active))
        }

        valid_active <- which(!miss_active)
        if (length(valid_active) > 0L) {
          val_chunks[[j]] <- parsed$values[valid_active]
          grp_chunks[[j]] <- rep(col_nm, length(valid_active))
          row_chunks[[j]] <- as.integer(valid_active)
        }
      }
    }

    cand_vals <- if (length(unlist(val_chunks)) > 0L) as.double(unlist(val_chunks, use.names = FALSE)) else numeric(0L)
    cand_grps <- if (length(unlist(grp_chunks)) > 0L) as.character(unlist(grp_chunks, use.names = FALSE)) else character(0L)
    cand_rows <- if (length(unlist(row_chunks)) > 0L) as.integer(unlist(row_chunks, use.names = FALSE)) else integer(0L)

    coerced_rows <- if (length(unlist(coerced_rows_list)) > 0L) {
      as.integer(unlist(coerced_rows_list, use.names = FALSE))
    } else {
      integer(0L)
    }
    coerced_cols <- if (length(unlist(coerced_cols_list)) > 0L) {
      as.character(unlist(coerced_cols_list, use.names = FALSE))
    } else {
      character(0L)
    }
    appearance_levels <- unique(response_cols)
  } else {
    if (length(response_cols) != 1L) {
      stop("In `mode = \"long\"`, `response_cols` must specify exactly one response column.")
    }
    resp_nm <- response_cols[1L]
    parsed_y <- .sl_parse_numeric_vec(df[[resp_nm]])

    has_group_col <- !is.null(group_col) && length(group_col) > 0L &&
      !is.na(group_col[1L]) && nzchar(as.character(group_col[1L]))

    if (has_group_col) {
      grp_nm <- as.character(group_col[1L])
      if (!(grp_nm %in% names(df))) {
        stop(sprintf("Group column not found in `df`: %s", grp_nm))
      }
      parsed_g <- .sl_parse_group_vec(df[[grp_nm]])
      grp_labels <- parsed_g$labels
      grp_miss <- parsed_g$is_missing
      appearance_levels <- unique(grp_labels[!grp_miss])
    } else {
      grp_labels <- rep(resp_nm, nr)
      grp_miss <- rep(FALSE, nr)
      appearance_levels <- if (nr > 0L) resp_nm else character(0L)
    }

    n_raw_obs <- as.integer(nr)
    row_miss <- parsed_y$is_missing | grp_miss
    n_missing <- as.integer(sum(row_miss))

    coerced_idx <- which(parsed_y$is_coerced)
    coerced_rows <- as.integer(coerced_idx)
    coerced_cols <- rep(resp_nm, length(coerced_idx))

    valid_idx <- which(!row_miss)
    cand_vals <- parsed_y$values[valid_idx]
    cand_grps <- grp_labels[valid_idx]
    cand_rows <- as.integer(valid_idx)
  }

  if (length(cand_rows) > 0L && length(excl_set) > 0L) {
    is_excl <- cand_rows %in% excl_set
  } else {
    is_excl <- rep(FALSE, length(cand_rows))
  }

  excluded_outliers <- data.frame(
    source_row = as.integer(cand_rows[is_excl]),
    group = as.character(cand_grps[is_excl]),
    value = as.double(cand_vals[is_excl]),
    stringsAsFactors = FALSE
  )

  keep <- !is_excl
  final_vals <- as.double(cand_vals[keep])
  final_grps_chr <- as.character(cand_grps[keep])
  final_rows <- as.integer(cand_rows[keep])
  final_levels <- appearance_levels[appearance_levels %in% final_grps_chr]
  final_groups <- factor(final_grps_chr, levels = final_levels)

  list(
    values = final_vals,
    groups = final_groups,
    source_rows = final_rows,
    n_raw_obs = as.integer(n_raw_obs),
    n_valid = as.integer(length(final_vals)),
    n_missing = as.integer(n_missing),
    coerced_non_numeric = list(
      count = as.integer(length(coerced_rows)),
      rows = coerced_rows,
      cols = coerced_cols
    ),
    excluded_outliers = excluded_outliers
  )
}

#' Detect outliers per group via 1.5xIQR fences and two-sided Grubbs test (non-destructive)
#'
#' @param values Numeric vector of observations
#' @param groups Optional factor or character vector of group memberships (NULL = single group `"all"`)
#' @param source_rows Integer vector of 1-based original row indices matching `values`
#' @param alpha Numeric(1) significance level in (0, 1) for two-sided Grubbs test (default 0.05)
#' @return Named list with `per_group` and `flagged` data.frame (never deletes observations)
sl_detect_outliers <- function(
  values,
  groups = NULL,
  source_rows = seq_along(values),
  alpha = 0.05
) {
  if (is.null(alpha) || length(alpha) == 0L) {
    alpha <- 0.05
  }
  alpha <- as.double(alpha[1L])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a finite number strictly between 0 and 1.")
  }

  values <- as.double(values)
  n_total <- length(values)

  if (is.null(source_rows)) {
    source_rows <- seq_len(n_total)
  } else {
    source_rows <- as.integer(source_rows)
  }
  if (length(source_rows) != n_total) {
    stop("`source_rows` must have the same length as `values`.")
  }

  if (is.null(groups)) {
    grp_levels <- if (n_total > 0L) "all" else character(0L)
    groups <- factor(rep("all", n_total), levels = grp_levels)
  } else {
    if (length(groups) != n_total) {
      stop("`groups` must have the same length as `values`.")
    }
    if (!is.factor(groups)) {
      g_chr <- as.character(groups)
      groups <- factor(g_chr, levels = unique(g_chr[!is.na(g_chr)]))
    }
    grp_levels <- levels(groups)
  }

  per_group <- stats::setNames(vector("list", length(grp_levels)), grp_levels)
  by_iqr_all <- rep(FALSE, n_total)
  by_grubbs_all <- rep(FALSE, n_total)
  grp_chr_all <- as.character(groups)

  for (g_lvl in grp_levels) {
    idx_grp <- which(!is.na(groups) & grp_chr_all == g_lvl & is.finite(values))
    x <- values[idx_grp]
    r_idx <- source_rows[idx_grp]
    n <- length(x)

    if (n == 0L) {
      iqr_bounds <- c(NA_real_, NA_real_)
    } else {
      qs <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE, type = 7)
      iqr_val <- qs[2L] - qs[1L]
      low_b <- qs[1L] - 1.5 * iqr_val
      high_b <- qs[2L] + 1.5 * iqr_val
      iqr_bounds <- c(low_b, high_b)
      by_iqr_all[idx_grp] <- (x < low_b) | (x > high_b)
    }

    if (n == 0L) {
      grubbs <- list(
        valid = FALSE,
        G = NA_real_,
        p_value = NA_real_,
        suspect_value = NA_real_,
        suspect_row = NA_integer_,
        is_outlier = FALSE,
        note_key = "empty_group"
      )
    } else if (n < 3L) {
      grubbs <- list(
        valid = FALSE,
        G = NA_real_,
        p_value = NA_real_,
        suspect_value = NA_real_,
        suspect_row = NA_integer_,
        is_outlier = FALSE,
        note_key = "n_lt_3"
      )
    } else {
      m <- mean(x)
      s <- stats::sd(x)
      if (!is.finite(s) || s == 0) {
        grubbs <- list(
          valid = FALSE,
          G = NA_real_,
          p_value = NA_real_,
          suspect_value = NA_real_,
          suspect_row = NA_integer_,
          is_outlier = FALSE,
          note_key = "zero_variance"
        )
      } else {
        min_pos <- which.min(x)
        max_pos <- which.max(x)
        sus_pos <- if ((x[max_pos] - m) < (m - x[min_pos])) min_pos else max_pos
        sus_val <- x[sus_pos]
        sus_row <- as.integer(r_idx[sus_pos])
        G <- abs(sus_val - m) / s

        s_sq <- (n - 1) / sqrt(n)
        denom <- (n - 1)^2 - n * G^2
        if (G >= s_sq || denom <= 0) {
          q <- 0
        } else {
          t_stat <- G * sqrt((n * (n - 2)) / denom)
          q <- min(1, n * stats::pt(t_stat, df = n - 2, lower.tail = FALSE))
        }

        p_val <- min(1, 2 * min(q, 1 - q))
        is_out <- isTRUE(q <= 0.5 && p_val < alpha)
        note_k <- if (q > 0.5 && p_val < alpha) "lower_tail_no_outlier" else NA_character_

        if (is_out) {
          by_grubbs_all[idx_grp[sus_pos]] <- TRUE
        }

        grubbs <- list(
          valid = TRUE,
          G = G,
          p_value = p_val,
          suspect_value = sus_val,
          suspect_row = sus_row,
          is_outlier = is_out,
          note_key = note_k
        )
      }
    }

    per_group[[g_lvl]] <- list(
      iqr_bounds = sl_box_vec(iqr_bounds),
      grubbs = grubbs
    )
  }

  flag_mask <- by_iqr_all | by_grubbs_all
  flagged <- data.frame(
    source_row = as.integer(source_rows[flag_mask]),
    group = grp_chr_all[flag_mask],
    value = as.double(values[flag_mask]),
    by_iqr = as.logical(by_iqr_all[flag_mask]),
    by_grubbs = as.logical(by_grubbs_all[flag_mask]),
    stringsAsFactors = FALSE
  )

  list(
    per_group = per_group,
    flagged = flagged
  )
}
