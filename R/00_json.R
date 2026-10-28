# R/00_json.R
# StatLab Pure Base-R JSON Serializer & Bilingual Number/p-value Formatters
# Dual-use across webR (browser WASM) and Desktop R 4.5.2.
# Uses ONLY base R functions (zero external/CRAN packages).

.sl_ctrl_codes <- c(1L:7L, 11L, 14L:31L)
.sl_ctrl_chars <- vapply(.sl_ctrl_codes, intToUtf8, character(1L))
.sl_ctrl_escs  <- sprintf("\\u%04x", .sl_ctrl_codes)

#' Mark a vector/list so length-0 and length-1 values serialize as JSON arrays
#'
#' @param x Atomic vector or list (NULL becomes an empty boxed list)
#' @return Object carrying class `"sl_array"`
sl_box_vec <- function(x) {
  if (is.null(x)) {
    x <- list()
  }
  structure(x, class = unique(c("sl_array", class(x))))
}

.sl_json_quote <- function(x) {
  n <- length(x)
  if (n == 0L) {
    return(character(0L))
  }
  out <- rep("null", n)
  ok <- !is.na(x)
  if (any(ok)) {
    s <- enc2utf8(as.character(x[ok]))
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    s <- gsub("\b", "\\b", s, fixed = TRUE)
    s <- gsub("\f", "\\f", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\t", "\\t", s, fixed = TRUE)
    if (any(grepl("[\\x01-\\x07\\x0b\\x0e-\\x1f]", s, perl = TRUE))) {
      for (k in seq_along(.sl_ctrl_chars)) {
        s <- gsub(.sl_ctrl_chars[k], .sl_ctrl_escs[k], s, fixed = TRUE)
      }
    }
    out[ok] <- paste0("\"", s, "\"")
  }
  out
}

.sl_atomic_tokens <- function(x) {
  if (inherits(x, c("factor", "Date", "POSIXt"))) {
    return(.sl_json_quote(as.character(x)))
  }
  n <- length(x)
  if (n == 0L) {
    return(character(0L))
  }
  if (is.logical(x)) {
    out <- rep("null", n)
    ok <- !is.na(x)
    out[ok & x] <- "true"
    out[ok & !x] <- "false"
    return(out)
  }
  if (is.integer(x)) {
    out <- rep("null", n)
    ok <- !is.na(x)
    if (any(ok)) {
      out[ok] <- sprintf("%d", x[ok])
    }
    return(out)
  }
  if (is.numeric(x)) {
    out <- rep("null", n)
    ok <- is.finite(x)
    if (any(ok)) {
      vals <- as.double(x[ok])
      vals[vals == 0] <- 0
      toks <- sprintf("%.15g", vals)
      toks <- sub(",", ".", toks, fixed = TRUE)
      out[ok] <- toks
    }
    return(out)
  }
  if (is.character(x)) {
    return(.sl_json_quote(x))
  }
  .sl_json_quote(as.character(x))
}

#' Serialize an R object to a valid UTF-8 JSON string using pure Base R
#'
#' @param x R value (NULL, atomic vector, sl_array, list, matrix, or data.frame)
#' @return character(1) valid UTF-8 JSON
sl_to_json <- function(x) {
  if (is.null(x)) {
    return("null")
  }

  is_boxed <- inherits(x, c("sl_array", "AsIs"))

  if (is.data.frame(x)) {
    nr <- nrow(x)
    if (nr == 0L) {
      return("[]")
    }
    nc <- ncol(x)
    if (nc == 0L) {
      return(paste0("[", paste(rep("{}", nr), collapse = ","), "]"))
    }
    col_names <- names(x)
    if (is.null(col_names)) {
      col_names <- as.character(seq_len(nc))
    }
    bad_nm <- is.na(col_names) | !nzchar(col_names)
    if (any(bad_nm)) {
      col_names[bad_nm] <- as.character(which(bad_nm))
    }
    keys <- .sl_json_quote(col_names)
    pair_cols <- vector("list", nc)
    for (j in seq_len(nc)) {
      col <- x[[j]]
      if (is.data.frame(col)) {
        toks <- vapply(
          seq_len(nr),
          function(i) sl_to_json(col[i, , drop = FALSE]),
          character(1L)
        )
      } else if (is.list(col)) {
        toks <- vapply(
          seq_len(nr),
          function(i) sl_to_json(col[[i]]),
          character(1L)
        )
      } else {
        toks <- .sl_atomic_tokens(col)
      }
      pair_cols[[j]] <- paste0(keys[j], ":", toks)
    }
    row_bodies <- do.call(paste, c(unname(pair_cols), list(sep = ",")))
    rows <- paste0("{", row_bodies, "}")
    return(enc2utf8(paste0("[", paste(rows, collapse = ","), "]")))
  }

  if (is.matrix(x)) {
    nr <- nrow(x)
    if (nr == 0L) {
      return("[]")
    }
    rows <- vapply(
      seq_len(nr),
      function(i) {
        toks <- .sl_atomic_tokens(x[i, , drop = TRUE])
        paste0("[", paste(toks, collapse = ","), "]")
      },
      character(1L)
    )
    return(enc2utf8(paste0("[", paste(rows, collapse = ","), "]")))
  }

  if (is.list(x)) {
    nms <- names(x)
    is_named <- !is_boxed && !is.null(nms)
    if (length(x) == 0L) {
      return(if (is_named) "{}" else "[]")
    }
    vals <- vapply(seq_along(x), function(i) sl_to_json(x[[i]]), character(1L))
    if (is_named) {
      bad_nm <- is.na(nms) | !nzchar(nms)
      if (any(bad_nm)) {
        nms[bad_nm] <- as.character(which(bad_nm))
      }
      keys <- .sl_json_quote(nms)
      return(enc2utf8(paste0("{", paste0(keys, ":", vals, collapse = ","), "}")))
    }
    return(enc2utf8(paste0("[", paste(vals, collapse = ","), "]")))
  }

  toks <- .sl_atomic_tokens(x)
  if (!is_boxed && length(toks) == 1L) {
    return(enc2utf8(toks))
  }
  enc2utf8(paste0("[", paste(toks, collapse = ","), "]"))
}

#' Format numeric values with bilingual decimal separator (ID comma / EN dot)
#'
#' @param x Numeric scalar or vector
#' @param digits Integer number of decimal places (default 4L)
#' @param lang Language code: `"id"` (comma decimal) or `"en"` (dot decimal)
#' @return Character vector (`"\u2014"` for NA/NaN/Inf/-Inf)
sl_fmt_num <- function(x, digits = 4L, lang = "id") {
  em_dash <- enc2utf8("\u2014")
  if (is.null(x)) {
    return(em_dash)
  }
  n <- length(x)
  if (n == 0L) {
    return(character(0L))
  }
  digits <- as.integer(digits[1L])
  if (is.na(digits) || digits < 0L) {
    digits <- 4L
  }
  is_en <- !is.null(lang) && length(lang) > 0L &&
    identical(tolower(as.character(lang[1L])), "en")
  dec_mark <- if (is_en) "." else ","

  num_x <- suppressWarnings(as.double(x))
  out <- rep(em_dash, n)
  fin <- is.finite(num_x)
  if (any(fin)) {
    fmt <- formatC(num_x[fin], digits = digits, format = "f", decimal.mark = dec_mark)
    neg_zero <- grepl("^-0([.,]0+)?$", fmt)
    if (any(neg_zero)) {
      fmt[neg_zero] <- sub("^-", "", fmt[neg_zero])
    }
    out[fin] <- fmt
  }
  enc2utf8(out)
}

#' Format p-values with bilingual decimal separator and `< 0,001` / `< 0.001` floor
#'
#' @param p Numeric scalar or vector of p-values
#' @param lang Language code: `"id"` (comma decimal) or `"en"` (dot decimal)
#' @return Character vector (3 decimals when >= 0.001, `"\u2014"` for non-finite)
sl_fmt_p <- function(p, lang = "id") {
  em_dash <- enc2utf8("\u2014")
  if (is.null(p)) {
    return(em_dash)
  }
  n <- length(p)
  if (n == 0L) {
    return(character(0L))
  }
  is_en <- !is.null(lang) && length(lang) > 0L &&
    identical(tolower(as.character(lang[1L])), "en")
  dec_mark <- if (is_en) "." else ","
  small_tok <- if (is_en) "< 0.001" else "< 0,001"

  num_p <- suppressWarnings(as.double(p))
  out <- rep(em_dash, n)
  fin <- is.finite(num_p)
  if (any(fin)) {
    pf <- num_p[fin]
    is_small <- pf < 0.001
    res_fin <- character(length(pf))
    if (any(is_small)) {
      res_fin[is_small] <- small_tok
    }
    if (any(!is_small)) {
      fmt <- formatC(pf[!is_small], digits = 3L, format = "f", decimal.mark = dec_mark)
      neg_zero <- grepl("^-0([.,]0+)?$", fmt)
      if (any(neg_zero)) {
        fmt[neg_zero] <- sub("^-", "", fmt[neg_zero])
      }
      res_fin[!is_small] <- fmt
    }
    out[fin] <- res_fin
  }
  enc2utf8(out)
}
