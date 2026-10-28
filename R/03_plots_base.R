# R/03_plots_base.R
# StatLab Pure Base-R Graphics Capture (300-DPI PNG + Pure-R RFC-4648 Base64)
# Lab Notebook Aesthetic (#FFFDF8 cream, #1B365D ink, #B83227 crimson).
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base, stats, graphics, and grDevices (zero external/CRAN packages).

.sl_b64_raw <- charToRaw("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
.sl_b64_pad <- charToRaw("=")

.sl_col_bg      <- "#FFFDF8"
.sl_col_ink     <- "#1B365D"
.sl_col_crimson <- "#B83227"
.sl_col_fill    <- "#E6ECF3"
.sl_col_box     <- "#EFECE4"
.sl_col_grid    <- "#E3DDD0"

#' Pure Base-R RFC-4648 Base64 encoder (vectorized, zero external packages)
#'
#' @param raw_vec Raw vector (or length-1 character string)
#' @return character(1) Base64-encoded ASCII string
sl_base64_encode <- function(raw_vec) {
  if (is.null(raw_vec)) {
    return("")
  }
  if (is.character(raw_vec)) {
    if (length(raw_vec) == 0L) {
      return("")
    }
    if (length(raw_vec) != 1L || is.na(raw_vec)) {
      stop("`raw_vec` must be a raw vector or length-1 non-NA character string.")
    }
    raw_vec <- charToRaw(enc2utf8(raw_vec))
  }
  if (!is.raw(raw_vec)) {
    stop("`raw_vec` must be a raw vector.")
  }

  n <- length(raw_vec)
  if (n == 0L) {
    return("")
  }

  rem <- n %% 3L
  pad <- if (rem == 0L) 0L else (3L - rem)
  ints <- as.integer(raw_vec)
  if (pad > 0L) {
    ints <- c(ints, integer(pad))
  }

  n_blocks <- length(ints) %/% 3L
  i1 <- seq.int(1L, by = 3L, length.out = n_blocks)
  b1 <- ints[i1]
  b2 <- ints[i1 + 1L]
  b3 <- ints[i1 + 2L]

  c1 <- (b1 %/% 4L) + 1L
  c2 <- ((b1 %% 4L) * 16L + (b2 %/% 16L)) + 1L
  c3 <- ((b2 %% 16L) * 4L + (b3 %/% 64L)) + 1L
  c4 <- (b3 %% 64L) + 1L

  out_mat <- rbind(
    .sl_b64_raw[c1],
    .sl_b64_raw[c2],
    .sl_b64_raw[c3],
    .sl_b64_raw[c4]
  )
  out_raw <- as.vector(out_mat)
  len_out <- length(out_raw)
  if (pad == 1L) {
    out_raw[len_out] <- .sl_b64_pad
  } else if (pad == 2L) {
    out_raw[(len_out - 1L):len_out] <- .sl_b64_pad
  }
  rawToChar(out_raw)
}

.sl_open_capture_device <- function(width_in, height_in, res) {
  w_px <- as.integer(max(64L, round(width_in * res)))
  h_px <- as.integer(max(64L, round(height_in * res)))
  tmp_png <- tempfile(pattern = "statlab_plot_", fileext = ".png")

  opened_png <- tryCatch({
    grDevices::png(
      filename = tmp_png,
      width = w_px,
      height = h_px,
      units = "px",
      res = res,
      bg = .sl_col_bg
    )
    TRUE
  }, error = function(e) FALSE)

  if (!opened_png) {
    opened_png <- tryCatch({
      grDevices::png(
        filename = tmp_png,
        width = w_px,
        height = h_px,
        units = "px",
        res = res,
        bg = .sl_col_bg,
        type = "cairo"
      )
      TRUE
    }, error = function(e) FALSE)
  }

  if (!opened_png) {
    opened_png <- tryCatch({
      grDevices::bitmap(
        file = tmp_png,
        type = "png16m",
        width = width_in,
        height = height_in,
        res = res
      )
      TRUE
    }, error = function(e) FALSE)
  }

  if (opened_png) {
    return(list(path = tmp_png, mime = "image/png"))
  }

  if (file.exists(tmp_png)) {
    unlink(tmp_png)
  }
  tmp_svg <- tempfile(pattern = "statlab_plot_", fileext = ".svg")
  grDevices::svg(
    filename = tmp_svg,
    width = width_in,
    height = height_in,
    bg = .sl_col_bg
  )
  list(path = tmp_svg, mime = "image/svg+xml")
}

#' Capture a base-graphics plotting callback as a 300-DPI PNG Base64 data URL
#'
#' @param plot_fn Zero-argument function drawing to the active graphics device
#' @param width_in Plot width in inches (default 6.5)
#' @param height_in Plot height in inches (default 4.2)
#' @param res Raster resolution in DPI (default 300)
#' @return Named list `list(png_data_url, width_in, height_in)`
sl_capture_plot <- function(plot_fn, width_in = 6.5, height_in = 4.2, res = 300) {
  if (!is.function(plot_fn)) {
    stop("`plot_fn` must be a function.")
  }
  width_in <- as.double(width_in[1L])
  height_in <- as.double(height_in[1L])
  res <- as.integer(round(res[1L]))
  if (!is.finite(width_in) || width_in <= 0 || !is.finite(height_in) || height_in <= 0) {
    stop("`width_in` and `height_in` must be positive numbers.")
  }
  if (is.na(res) || res < 36L) {
    res <- 300L
  }

  prev_dev <- grDevices::dev.cur()
  dev_info <- .sl_open_capture_device(width_in, height_in, res)
  plot_dev <- grDevices::dev.cur()
  dev_closed <- FALSE

  on.exit({
    if (!dev_closed && !is.na(plot_dev) && plot_dev > 1L) {
      active_devs <- grDevices::dev.list()
      if (!is.null(active_devs) && (plot_dev %in% active_devs)) {
        try(grDevices::dev.off(plot_dev), silent = TRUE)
      }
    }
    if (!is.na(prev_dev) && prev_dev > 1L) {
      active_devs <- grDevices::dev.list()
      if (!is.null(active_devs) && (prev_dev %in% active_devs)) {
        try(grDevices::dev.set(prev_dev), silent = TRUE)
      }
    }
    if (file.exists(dev_info$path)) {
      unlink(dev_info$path)
    }
  }, add = TRUE)

  plot_fn()

  active_devs <- grDevices::dev.list()
  if (!is.null(active_devs) && (plot_dev %in% active_devs)) {
    grDevices::dev.off(plot_dev)
  }
  dev_closed <- TRUE
  if (!is.na(prev_dev) && prev_dev > 1L) {
    active_devs <- grDevices::dev.list()
    if (!is.null(active_devs) && (prev_dev %in% active_devs)) {
      grDevices::dev.set(prev_dev)
    }
  }

  f_size <- file.info(dev_info$path)$size
  if (is.na(f_size) || f_size <= 0) {
    stop("Plot capture failed: graphics device produced an empty file.")
  }
  raw_bytes <- readBin(dev_info$path, what = "raw", n = as.integer(f_size))
  b64 <- sl_base64_encode(raw_bytes)

  list(
    png_data_url = paste0("data:", dev_info$mime, ";base64,", b64),
    width_in = width_in,
    height_in = height_in
  )
}

.sl_is_en <- function(lang) {
  !is.null(lang) && length(lang) > 0L && identical(tolower(as.character(lang[1L])), "en")
}

.sl_apply_plot_theme <- function() {
  mf <- graphics::par("mfrow")
  compact <- length(mf) == 2L && prod(mf) > 1L
  mar_vec <- if (compact) c(3.6, 3.8, 2.3, 0.9) else c(4.2, 4.5, 2.6, 1.2)
  graphics::par(
    bg = .sl_col_bg,
    mar = mar_vec,
    las = 1L,
    bty = "l",
    fg = .sl_col_ink,
    col.axis = .sl_col_ink,
    col.lab = .sl_col_ink,
    col.main = .sl_col_ink,
    cex.main = if (compact) 0.92 else 1.04,
    cex.lab = if (compact) 0.84 else 0.94,
    cex.axis = if (compact) 0.78 else 0.86,
    tcl = -0.28,
    mgp = c(2.4, 0.65, 0)
  )
}

.sl_safe_ylim <- function(vals, pad_frac = 0.08) {
  y_min <- min(vals)
  y_max <- max(vals)
  if (!is.finite(y_min) || !is.finite(y_max)) {
    return(c(-1, 1))
  }
  if (y_max == y_min) {
    delta <- if (y_min == 0) 1.0 else abs(y_min) * 0.05
    return(c(y_min - delta, y_max + delta))
  }
  span <- y_max - y_min
  c(y_min - pad_frac * span, y_max + pad_frac * span)
}

#' Normal Quantile-Quantile (Q-Q) Plot in Lab Notebook style
#'
#' @param x Numeric vector of observations
#' @param title Plot title string
#' @param lang `"id"` or `"en"`
#' @param outlier_idx Optional integer vector of 1-based indices into `x` flagged as outliers
sl_plot_qq <- function(x, title, lang = "id", outlier_idx = integer(0)) {
  is_en <- .sl_is_en(lang)
  .sl_apply_plot_theme()

  xlab <- if (is_en) "Theoretical Normal Quantiles" else "Kuantil Teoritis Normal"
  ylab <- if (is_en) "Sample Quantiles" else "Kuantil Sampel"

  x_num <- if (is.null(x)) numeric(0L) else suppressWarnings(as.double(x))
  ok_pos <- which(is.finite(x_num))
  vals <- x_num[ok_pos]
  out_set <- if (is.null(outlier_idx) || length(outlier_idx) == 0L) {
    integer(0L)
  } else {
    as.integer(outlier_idx[!is.na(outlier_idx)])
  }
  is_out <- ok_pos %in% out_set
  n <- length(vals)

  if (n == 0L) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
    graphics::box(bty = "l", col = .sl_col_ink)
    graphics::title(main = title, xlab = xlab, ylab = ylab)
    graphics::text(
      0, 0,
      labels = if (is_en) "No valid observations" else "Tidak ada data valid",
      col = .sl_col_ink, cex = 0.9
    )
    return(invisible(NULL))
  }

  if (n == 1L) {
    y_lim <- .sl_safe_ylim(vals)
    graphics::plot(
      0, vals[1L],
      type = "n", xlim = c(-1, 1), ylim = y_lim,
      main = title, xlab = xlab, ylab = ylab
    )
    graphics::grid(col = .sl_col_grid, lty = "dotted")
    graphics::abline(h = vals[1L], col = .sl_col_crimson, lwd = 1.5, lty = 2)
    pt_col <- if (is_out[1L]) .sl_col_crimson else .sl_col_ink
    graphics::points(0, vals[1L], pch = 19, col = pt_col, cex = 1.0)
    return(invisible(NULL))
  }

  qq <- stats::qqnorm(vals, plot.it = FALSE)
  y_lim <- .sl_safe_ylim(qq$y)

  graphics::plot(
    qq$x, qq$y,
    type = "n", ylim = y_lim,
    main = title, xlab = xlab, ylab = ylab
  )
  graphics::grid(col = .sl_col_grid, lty = "dotted")

  if (max(vals) == min(vals)) {
    graphics::abline(h = vals[1L], col = .sl_col_crimson, lwd = 1.7, lty = 2)
    graphics::mtext(
      if (is_en) "SD = 0 (constant)" else "SD = 0 (konstan)",
      side = 3, line = 0.15, cex = 0.72, col = .sl_col_crimson
    )
  } else {
    yq <- stats::quantile(vals, probs = c(0.25, 0.75), names = FALSE, type = 7)
    if (yq[2L] > yq[1L]) {
      stats::qqline(vals, col = .sl_col_crimson, lwd = 1.8, lty = 1)
    } else {
      graphics::abline(a = mean(vals), b = stats::sd(vals), col = .sl_col_crimson, lwd = 1.8, lty = 2)
    }
  }

  in_mask <- !is_out
  if (any(in_mask)) {
    graphics::points(
      qq$x[in_mask], qq$y[in_mask],
      pch = 19,
      col = grDevices::adjustcolor(.sl_col_ink, alpha.f = 0.80),
      cex = 0.85
    )
  }
  if (any(is_out)) {
    graphics::points(
      qq$x[is_out], qq$y[is_out],
      pch = 19,
      col = .sl_col_crimson,
      cex = 1.08
    )
    graphics::points(
      qq$x[is_out], qq$y[is_out],
      pch = 1,
      col = .sl_col_crimson,
      cex = 1.45,
      lwd = 1.3
    )
  }
  invisible(NULL)
}

#' Histogram with Kernel Density & Theoretical Normal Curve in Lab Notebook style
#'
#' @param x Numeric vector of observations
#' @param title Plot title string
#' @param xlab Horizontal axis label
#' @param lang `"id"` or `"en"`
sl_plot_hist_density <- function(x, title, xlab, lang = "id") {
  is_en <- .sl_is_en(lang)
  .sl_apply_plot_theme()

  x_num <- if (is.null(x)) numeric(0L) else suppressWarnings(as.double(x))
  vals <- x_num[is.finite(x_num)]
  n <- length(vals)

  if (n == 0L) {
    ylab <- if (is_en) "Density" else "Kerapatan"
    graphics::plot.new()
    graphics::plot.window(xlim = c(-1, 1), ylim = c(0, 1))
    graphics::box(bty = "l", col = .sl_col_ink)
    graphics::title(main = title, xlab = xlab, ylab = ylab)
    graphics::text(
      0, 0.5,
      labels = if (is_en) "No valid observations" else "Tidak ada data valid",
      col = .sl_col_ink, cex = 0.9
    )
    return(invisible(NULL))
  }

  if (n == 1L || max(vals) == min(vals)) {
    v0 <- vals[1L]
    delta <- if (v0 == 0) 1.0 else abs(v0) * 0.05
    brk <- c(v0 - delta, v0 - delta / 3.0, v0 + delta / 3.0, v0 + delta)
    ylab_freq <- if (is_en) "Frequency" else "Frekuensi"
    graphics::hist(
      vals,
      breaks = brk,
      freq = TRUE,
      col = .sl_col_fill,
      border = .sl_col_ink,
      xlim = c(v0 - 1.4 * delta, v0 + 1.4 * delta),
      ylim = c(0, n * 1.25),
      main = title,
      xlab = xlab,
      ylab = ylab_freq
    )
    graphics::grid(col = .sl_col_grid, lty = "dotted")
    graphics::abline(v = v0, col = .sl_col_crimson, lwd = 1.8, lty = 2)
    graphics::mtext(
      if (is_en) "SD = 0 (constant)" else "SD = 0 (konstan)",
      side = 3, line = 0.15, cex = 0.72, col = .sl_col_crimson
    )
    return(invisible(NULL))
  }

  ylab <- if (is_en) "Density" else "Kerapatan"
  h <- graphics::hist(vals, breaks = "Sturges", plot = FALSE)
  m <- mean(vals)
  s <- stats::sd(vals)

  x_lo <- min(c(h$breaks, m - 3.2 * s))
  x_hi <- max(c(h$breaks, m + 3.2 * s))
  x_grid <- seq(x_lo, x_hi, length.out = 200L)
  y_norm <- stats::dnorm(x_grid, mean = m, sd = s)

  dens <- if (n >= 3L) {
    tryCatch(stats::density(vals), error = function(e) NULL)
  } else {
    NULL
  }

  y_max <- max(c(h$density, y_norm, if (!is.null(dens)) dens$y else 0), na.rm = TRUE) * 1.15
  if (!is.finite(y_max) || y_max <= 0) {
    y_max <- 1.0
  }

  graphics::plot(
    h,
    freq = FALSE,
    col = .sl_col_fill,
    border = .sl_col_ink,
    xlim = c(x_lo, x_hi),
    ylim = c(0, y_max),
    main = title,
    xlab = xlab,
    ylab = ylab
  )
  graphics::grid(col = .sl_col_grid, lty = "dotted")
  graphics::plot(
    h,
    freq = FALSE,
    col = .sl_col_fill,
    border = .sl_col_ink,
    add = TRUE
  )

  if (!is.null(dens) && all(is.finite(dens$y))) {
    graphics::lines(dens$x, dens$y, col = .sl_col_ink, lwd = 1.7, lty = 1)
  }
  graphics::lines(x_grid, y_norm, col = .sl_col_crimson, lwd = 1.9, lty = 2)

  leg_labels <- if (!is.null(dens)) {
    if (is_en) c("Normal PDF", "Kernel KDE") else c("Kurva Normal", " Estimasi KDE")
  } else {
    if (is_en) "Normal PDF" else "Kurva Normal"
  }
  leg_cols <- if (!is.null(dens)) c(.sl_col_crimson, .sl_col_ink) else .sl_col_crimson
  leg_ltys <- if (!is.null(dens)) c(2L, 1L) else 2L

  graphics::legend(
    "topright",
    legend = trimws(leg_labels),
    col = leg_cols,
    lty = leg_ltys,
    lwd = 1.8,
    bty = "n",
    cex = 0.74,
    text.col = .sl_col_ink
  )
  invisible(NULL)
}

#' Boxplot with Deterministic Jittered Observations & Crimson Outlier Highlights
#'
#' @param values Numeric vector of response observations
#' @param groups Factor or character vector of group memberships
#' @param title Plot title string
#' @param ylab Vertical axis label
#' @param lang `"id"` or `"en"`
#' @param outlier_rows Optional integer vector of flagged 1-based `source_rows`
#' @param source_rows Integer vector of 1-based original row indices matching `values`
sl_plot_box_jitter <- function(
  values,
  groups,
  title,
  ylab,
  lang = "id",
  outlier_rows = integer(0),
  source_rows = seq_along(values)
) {
  is_en <- .sl_is_en(lang)
  .sl_apply_plot_theme()

  y_raw <- if (is.null(values)) numeric(0L) else suppressWarnings(as.double(values))
  n_raw <- length(y_raw)
  if (is.null(source_rows)) {
    source_rows <- seq_len(n_raw)
  } else {
    source_rows <- as.integer(source_rows)
  }
  if (is.null(groups)) {
    groups <- factor(rep(if (is_en) "All" else "Semua", n_raw))
  }

  ok <- is.finite(y_raw) & !is.na(groups)
  vals <- y_raw[ok]
  rows_ok <- source_rows[ok]

  if (length(vals) == 0L) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(-1, 1))
    graphics::box(bty = "l", col = .sl_col_ink)
    graphics::title(main = title, ylab = ylab)
    graphics::text(
      0.5, 0,
      labels = if (is_en) "No valid observations" else "Tidak ada data valid",
      col = .sl_col_ink, cex = 0.9
    )
    return(invisible(NULL))
  }

  if (is.factor(groups)) {
    kept_lvls <- levels(groups)[levels(groups) %in% as.character(groups[ok])]
    grp_fac <- factor(as.character(groups[ok]), levels = kept_lvls)
  } else {
    g_chr <- as.character(groups[ok])
    grp_fac <- factor(g_chr, levels = unique(g_chr))
  }

  lvls <- levels(grp_fac)
  k <- length(lvls)
  y_lim <- .sl_safe_ylim(vals, pad_frac = 0.10)

  out_set <- if (is.null(outlier_rows) || length(outlier_rows) == 0L) {
    integer(0L)
  } else {
    as.integer(outlier_rows[!is.na(outlier_rows)])
  }
  is_out <- rows_ok %in% out_set

  graphics::boxplot(
    vals ~ grp_fac,
    outline = FALSE,
    ylim = y_lim,
    col = .sl_col_box,
    border = .sl_col_ink,
    medcol = .sl_col_ink,
    medlwd = 2.0,
    whiskcol = .sl_col_ink,
    whisklty = 1L,
    staplecol = .sl_col_ink,
    boxwex = if (k == 1L) 0.36 else 0.48,
    main = title,
    xlab = "",
    ylab = ylab,
    frame.plot = FALSE
  )
  graphics::box(bty = "l", col = .sl_col_ink)
  graphics::grid(nx = NA, ny = NULL, col = .sl_col_grid, lty = "dotted")

  # Re-draw boxes above gridlines cleanly
  graphics::boxplot(
    vals ~ grp_fac,
    outline = FALSE,
    col = .sl_col_box,
    border = .sl_col_ink,
    medcol = .sl_col_ink,
    medlwd = 2.0,
    whiskcol = .sl_col_ink,
    whisklty = 1L,
    staplecol = .sl_col_ink,
    boxwex = if (k == 1L) 0.36 else 0.48,
    add = TRUE,
    axes = FALSE,
    frame.plot = FALSE
  )

  # Deterministic golden-ratio horizontal quasi-jitter (zero RNG side-effects!)
  for (i in seq_len(k)) {
    idx_g <- which(grp_fac == lvls[i])
    ni <- length(idx_g)
    if (ni == 0L) {
      next
    }
    offsets <- if (ni == 1L) {
      0.0
    } else {
      ((((seq_len(ni) * 0.618033988749895) %% 1.0) - 0.5) * 0.22)
    }
    x_pos <- i + offsets
    y_pos <- vals[idx_g]
    out_g <- is_out[idx_g]
    rows_g <- rows_ok[idx_g]

    if (any(!out_g)) {
      graphics::points(
        x_pos[!out_g], y_pos[!out_g],
        pch = 19,
        col = grDevices::adjustcolor(.sl_col_ink, alpha.f = 0.72),
        cex = 0.82
      )
    }
    if (any(out_g)) {
      graphics::points(
        x_pos[out_g], y_pos[out_g],
        pch = 19,
        col = .sl_col_crimson,
        cex = 1.05
      )
      graphics::points(
        x_pos[out_g], y_pos[out_g],
        pch = 1,
        col = .sl_col_crimson,
        cex = 1.42,
        lwd = 1.3
      )
      if (sum(out_g) <= 6L) {
        graphics::text(
          x_pos[out_g], y_pos[out_g],
          labels = paste0("#", rows_g[out_g]),
          pos = 4, offset = 0.38,
          cex = 0.68, col = .sl_col_crimson
        )
      }
    }

    # Group mean marker (open diamond filled cream)
    m_g <- mean(y_pos)
    if (is.finite(m_g)) {
      graphics::points(
        i, m_g,
        pch = 23,
        col = .sl_col_ink,
        bg = .sl_col_bg,
        cex = 1.05,
        lwd = 1.4
      )
    }
  }

  invisible(NULL)
}
