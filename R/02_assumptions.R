# R/02_assumptions.R
# StatLab Mandatory Assumption Testing Engine
# (Shapiro-Wilk, Lilliefors Dallal-Wilkinson, Brown-Forsythe & Classic Levene, Bartlett)
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base and stats functions (zero external/CRAN packages).

.sl_clean_numeric_vec <- function(x) {
  if (is.null(x)) {
    return(numeric(0L))
  }
  if (is.logical(x) && all(is.na(x))) {
    return(numeric(0L))
  }
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }
  vals <- as.double(x)
  vals[is.finite(vals)]
}

.sl_eval_normality <- function(shapiro, lilliefors, alpha) {
  if (isTRUE(shapiro$valid)) {
    return(!(shapiro$p_value < alpha))
  }
  if (identical(shapiro$note_key, "n_gt_5000") && isTRUE(lilliefors$valid)) {
    return(!(lilliefors$p_value < alpha))
  }
  NA
}

#' Safe Shapiro-Wilk normality test wrapper (3 <= n <= 5000, non-zero variance)
#'
#' @param x Numeric vector of observations
#' @return Named list `list(valid, n, statistic, p_value, note_key)`
sl_shapiro_test <- function(x) {
  vals <- sort(.sl_clean_numeric_vec(x))
  n <- as.integer(length(vals))

  if (n == 0L) {
    return(list(
      valid = FALSE,
      n = 0L,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "empty_group"
    ))
  }
  if (n < 3L) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "n_lt_3"
    ))
  }
  if (n > 5000L) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "n_gt_5000"
    ))
  }

  rng <- vals[n] - vals[1L]
  s <- stats::sd(vals)
  if (!is.finite(rng) || rng == 0 || !is.finite(s) || s == 0) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "zero_variance"
    ))
  }

  res <- tryCatch(stats::shapiro.test(vals), error = function(e) NULL)
  if (is.null(res) || !is.finite(res$statistic) || !is.finite(res$p.value)) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "computation_failed"
    ))
  }

  list(
    valid = TRUE,
    n = n,
    statistic = as.double(unname(res$statistic)),
    p_value = as.double(res$p.value),
    note_key = NA_character_
  )
}

#' Pure Base-R Lilliefors (Kolmogorov-Smirnov) normality test
#'
#' Implements the Dallal-Wilkinson (1986) formula for `p <= 0.1` (with `n > 100`
#' scaling) and Stephens (1974) piecewise upper-tail approximation for `p > 0.1`,
#' matching `nortest::lillie.test` to `<= 1e-8`.
#'
#' @param x Numeric vector of observations
#' @return Named list `list(valid, n, statistic, p_value, note_key)`
sl_lilliefors_test <- function(x) {
  vals <- sort(.sl_clean_numeric_vec(x))
  n <- as.integer(length(vals))

  if (n == 0L) {
    return(list(
      valid = FALSE,
      n = 0L,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "empty_group"
    ))
  }
  if (n < 5L) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "n_lt_5"
    ))
  }

  rng <- vals[n] - vals[1L]
  s <- stats::sd(vals)
  if (!is.finite(rng) || rng == 0 || !is.finite(s) || s == 0) {
    return(list(
      valid = FALSE,
      n = n,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "zero_variance"
    ))
  }

  p <- stats::pnorm((vals - mean(vals)) / s)
  idx <- seq_len(n)
  d_plus <- max(idx / n - p)
  d_minus <- max(p - (idx - 1L) / n)
  K <- max(d_plus, d_minus)

  if (n <= 100L) {
    Kd <- K
    nd <- as.double(n)
  } else {
    Kd <- K * ((n / 100) ^ 0.49)
    nd <- 100.0
  }

  pvalue <- exp(
    -7.01256 * (Kd ^ 2) * (nd + 2.78019) +
      2.99587 * Kd * sqrt(nd + 2.78019) -
      0.122119 +
      0.974598 / sqrt(nd) +
      1.67997 / nd
  )

  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85 / sqrt(n)) * K
    if (KK <= 0.302) {
      pvalue <- 1.0
    } else if (KK <= 0.5) {
      pvalue <- 2.76773 - 19.828315 * KK + 80.709644 * (KK ^ 2) -
        138.55152 * (KK ^ 3) + 81.218052 * (KK ^ 4)
    } else if (KK <= 0.9) {
      pvalue <- -4.901232 + 40.662806 * KK - 97.490286 * (KK ^ 2) +
        94.029866 * (KK ^ 3) - 32.355711 * (KK ^ 4)
    } else if (KK <= 1.31) {
      pvalue <- 6.198765 - 19.558097 * KK + 23.186922 * (KK ^ 2) -
        12.234627 * (KK ^ 3) + 2.423045 * (KK ^ 4)
    } else {
      pvalue <- 0.0
    }
  }

  list(
    valid = TRUE,
    n = n,
    statistic = as.double(K),
    p_value = as.double(pvalue),
    note_key = NA_character_
  )
}

#' Pure Base-R Levene / Brown-Forsythe homogeneity of variance test
#'
#' Computes one-way ANOVA F on absolute deviations `|y_ij - c_i|` from group
#' medians (`center = "median"`, Brown-Forsythe) or group means (`center = "mean"`),
#' matching `car::leveneTest` to `<= 1e-8`.
#'
#' @param values Numeric vector of observations
#' @param groups Factor or atomic vector of group memberships
#' @param center `"median"` (default Brown-Forsythe) or `"mean"` (classic Levene)
#' @return Named list `list(valid, center, df1, df2, statistic, p_value, note_key)`
sl_levene_test <- function(values, groups, center = "median") {
  if (is.null(center) || length(center) == 0L ||
      !(as.character(center[1L]) %in% c("median", "mean"))) {
    stop("`center` must be \"median\" or \"mean\".")
  }
  center <- as.character(center[1L])

  if (is.null(values) || is.null(groups)) {
    return(list(
      valid = FALSE,
      center = center,
      df1 = NA_integer_,
      df2 = NA_integer_,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "lt_2_groups"
    ))
  }

  if (is.logical(values) && all(is.na(values))) {
    y_raw <- rep(NA_real_, length(values))
  } else if (!is.numeric(values)) {
    stop("`values` must be a numeric vector.")
  } else {
    y_raw <- as.double(values)
  }

  if (length(y_raw) != length(groups)) {
    stop("`values` and `groups` must have the same length.")
  }

  ok <- is.finite(y_raw) & !is.na(groups)
  y <- y_raw[ok]
  g <- factor(groups[ok])
  k <- nlevels(g)
  N <- length(y)

  if (k < 2L) {
    return(list(
      valid = FALSE,
      center = center,
      df1 = NA_integer_,
      df2 = NA_integer_,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "lt_2_groups"
    ))
  }

  df1 <- as.integer(k - 1L)
  df2 <- as.integer(N - k)
  if (df2 <= 0L) {
    return(list(
      valid = FALSE,
      center = center,
      df1 = df1,
      df2 = df2,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "insufficient_df"
    ))
  }

  split_y <- split(y, g)
  center_fn <- if (center == "median") stats::median else base::mean

  z_means <- numeric(k)
  n_grps <- integer(k)
  ss_within <- 0.0
  noise_tol <- 16.0 * .Machine$double.eps

  for (i in seq_len(k)) {
    yi <- split_y[[i]]
    ni <- length(yi)
    n_grps[i] <- ni

    if (max(yi) == min(yi)) {
      z_means[i] <- 0.0
    } else if (ni <= 2L) {
      z_means[i] <- 0.5 * abs(yi[2L] - yi[1L])
    } else {
      ci <- center_fn(yi)
      zi <- abs(yi - ci)
      zbar_i <- mean(zi)
      z_means[i] <- zbar_i
      ss_i <- sum((zi - zbar_i) ^ 2)
      m_i <- max(abs(yi))
      norm_yi <- m_i * sqrt(sum((yi / m_i) ^ 2))
      if (is.finite(ss_i) && sqrt(ss_i) > noise_tol * norm_yi) {
        ss_within <- ss_within + ss_i
      }
    }
  }

  if (!is.finite(ss_within) || ss_within <= 0) {
    return(list(
      valid = FALSE,
      center = center,
      df1 = df1,
      df2 = df2,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "zero_variance"
    ))
  }

  zbar_all <- sum(n_grps * z_means) / N
  ss_between <- sum(n_grps * ((z_means - zbar_all) ^ 2))

  ms_between <- ss_between / df1
  ms_within <- ss_within / df2
  f_stat <- ms_between / ms_within
  p_val <- stats::pf(f_stat, df1 = df1, df2 = df2, lower.tail = FALSE)

  if (!is.finite(f_stat) || !is.finite(p_val)) {
    return(list(
      valid = FALSE,
      center = center,
      df1 = df1,
      df2 = df2,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "computation_failed"
    ))
  }

  list(
    valid = TRUE,
    center = center,
    df1 = df1,
    df2 = df2,
    statistic = as.double(f_stat),
    p_value = as.double(p_val),
    note_key = NA_character_
  )
}

#' Safe Bartlett homogeneity of variance test wrapper
#'
#' Guards against `< 2` groups, groups with `n < 2`, and zero-variance groups
#' (where `log(0)` in `stats::bartlett.test` would yield `Inf`/`NaN`).
#'
#' @param values Numeric vector of observations
#' @param groups Factor or atomic vector of group memberships
#' @return Named list `list(valid, df, statistic, p_value, note_key)`
sl_bartlett_test <- function(values, groups) {
  if (is.null(values) || is.null(groups)) {
    return(list(
      valid = FALSE,
      df = NA_integer_,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "lt_2_groups"
    ))
  }

  if (is.logical(values) && all(is.na(values))) {
    y_raw <- rep(NA_real_, length(values))
  } else if (!is.numeric(values)) {
    stop("`values` must be a numeric vector.")
  } else {
    y_raw <- as.double(values)
  }

  if (length(y_raw) != length(groups)) {
    stop("`values` and `groups` must have the same length.")
  }

  ok <- is.finite(y_raw) & !is.na(groups)
  y <- y_raw[ok]
  g <- factor(groups[ok])
  k <- nlevels(g)

  if (k < 2L) {
    return(list(
      valid = FALSE,
      df = NA_integer_,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "lt_2_groups"
    ))
  }

  df_val <- as.integer(k - 1L)
  split_y <- split(y, g)
  ns <- lengths(split_y)
  if (any(ns < 2L)) {
    return(list(
      valid = FALSE,
      df = df_val,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "group_n_lt_2"
    ))
  }

  vars <- vapply(split_y, stats::var, numeric(1L))
  if (any(!is.finite(vars) | vars <= 0)) {
    return(list(
      valid = FALSE,
      df = df_val,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "zero_variance"
    ))
  }

  res <- tryCatch(stats::bartlett.test(y, g), error = function(e) NULL)
  if (is.null(res) || !is.finite(res$statistic) || !is.finite(res$p.value)) {
    return(list(
      valid = FALSE,
      df = df_val,
      statistic = NA_real_,
      p_value = NA_real_,
      note_key = "computation_failed"
    ))
  }

  list(
    valid = TRUE,
    df = df_val,
    statistic = as.double(unname(res$statistic)),
    p_value = as.double(res$p.value),
    note_key = NA_character_
  )
}

#' Run mandatory StatLab normality & homogeneity checks with Spec §6.2 violation rules
#'
#' @param values Numeric vector of response observations
#' @param groups Optional factor or atomic vector of group memberships (`NULL` = single group `"All"`)
#' @param residuals Optional numeric vector of model residuals (when supplied, governs `normality_violated`)
#' @param alpha Numeric(1) significance cutoff in `(0, 1)` (default `0.05`)
#' @param require_homogeneity Logical(1) whether homogeneity tests apply to this analysis (default `TRUE`)
#' @return Structured list matching the StatLab R->JS JSON contract (`per_group` is an unnamed list -> JSON array)
sl_check_assumptions <- function(
  values,
  groups = NULL,
  residuals = NULL,
  alpha = 0.05,
  require_homogeneity = TRUE
) {
  if (is.null(alpha) || length(alpha) == 0L) {
    alpha <- 0.05
  }
  alpha <- as.double(alpha[1L])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a finite number strictly between 0 and 1.")
  }

  if (is.null(values)) {
    y_raw <- numeric(0L)
  } else if (is.logical(values) && all(is.na(values))) {
    y_raw <- rep(NA_real_, length(values))
  } else if (!is.numeric(values)) {
    stop("`values` must be a numeric vector.")
  } else {
    y_raw <- as.double(values)
  }

  if (is.null(groups)) {
    sw_all <- sl_shapiro_test(y_raw)
    lf_all <- sl_lilliefors_test(y_raw)
    per_group <- list(
      list(
        group = "All",
        n = sw_all$n,
        shapiro = sw_all,
        lilliefors = lf_all,
        is_normal = .sl_eval_normality(sw_all, lf_all, alpha)
      )
    )
    non_empty_groups <- if (sw_all$n > 0L) 1L else 0L
  } else {
    if (length(y_raw) != length(groups)) {
      stop("`values` and `groups` must have the same length.")
    }
    if (is.factor(groups)) {
      grp_levels <- levels(groups)
    } else {
      g_chr_tmp <- as.character(groups)
      grp_levels <- unique(g_chr_tmp[!is.na(groups)])
    }
    g_chr <- as.character(groups)
    per_group <- vector("list", length(grp_levels))
    non_empty_groups <- 0L

    for (i in seq_along(grp_levels)) {
      g_lvl <- grp_levels[i]
      idx <- which(!is.na(groups) & g_chr == g_lvl & is.finite(y_raw))
      x_grp <- y_raw[idx]
      sw_g <- sl_shapiro_test(x_grp)
      lf_g <- sl_lilliefors_test(x_grp)
      if (sw_g$n > 0L) {
        non_empty_groups <- non_empty_groups + 1L
      }
      per_group[[i]] <- list(
        group = as.character(g_lvl),
        n = sw_g$n,
        shapiro = sw_g,
        lilliefors = lf_g,
        is_normal = .sl_eval_normality(sw_g, lf_g, alpha)
      )
    }
  }

  if (!is.null(residuals)) {
    sw_res <- sl_shapiro_test(residuals)
    lf_res <- sl_lilliefors_test(residuals)
    is_norm_res <- .sl_eval_normality(sw_res, lf_res, alpha)
    residual_normality <- list(
      shapiro = sw_res,
      lilliefors = lf_res,
      is_normal = is_norm_res
    )
    normality_rule <- "residuals"
    normality_violated <- identical(is_norm_res, FALSE)
  } else {
    residual_normality <- NULL
    normality_rule <- "per_group"
    normality_violated <- any(vapply(
      per_group,
      function(pg) identical(pg$is_normal, FALSE),
      logical(1L)
    ))
  }

  if (!is.null(groups) && isTRUE(require_homogeneity) && non_empty_groups >= 2L) {
    lev_bf <- sl_levene_test(y_raw, groups, center = "median")
    bart <- sl_bartlett_test(y_raw, groups)
    is_hom <- if (isTRUE(lev_bf$valid)) !(lev_bf$p_value < alpha) else NA
    homogeneity <- list(
      levene_bf = lev_bf,
      bartlett = bart,
      is_homogeneous = is_hom
    )
    homogeneity_violated <- isTRUE(lev_bf$valid && lev_bf$p_value < alpha)
  } else {
    homogeneity <- NULL
    homogeneity_violated <- FALSE
  }

  list(
    alpha = alpha,
    per_group = per_group,
    residual_normality = residual_normality,
    homogeneity = homogeneity,
    normality_rule = normality_rule,
    normality_violated = as.logical(normality_violated),
    homogeneity_violated = as.logical(homogeneity_violated)
  )
}
