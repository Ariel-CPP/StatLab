# tests/test_00_json.R
j1 <- sl_to_json(list(
  ok = TRUE,
  n = 10L,
  vals = sl_box_vec(c(1.5, NA_real_, Inf, -Inf, NaN)),
  single_arr = sl_box_vec("Kontrol"),
  empty_arr = sl_box_vec(character(0)),
  msg = "baris 1\n\"kutip\" \\ tab\t"
))
parsed <- jsonlite::fromJSON(j1, simplifyVector = FALSE)
assert_true(identical(parsed$ok, TRUE), "logical scalar")
assert_true(identical(parsed$n, 10L), "integer scalar")
assert_true(length(parsed$vals) == 5L && is.null(parsed$vals[[2]]) &&
            is.null(parsed$vals[[3]]) && is.null(parsed$vals[[4]]) &&
            is.null(parsed$vals[[5]]), "Review Focus #5: NA/Inf/NaN serialize to null")
assert_true(is.list(parsed$single_arr) && length(parsed$single_arr) == 1L, "boxed length-1 array")
assert_true(is.list(parsed$empty_arr) && length(parsed$empty_arr) == 0L, "boxed empty array")
assert_true(identical(parsed$msg, "baris 1\n\"kutip\" \\ tab\t"), "string escaping")

assert_true(identical(sl_fmt_num(12.3456, 2L, "id"), "12,35"), "ID comma format")
assert_true(identical(sl_fmt_num(12.3456, 2L, "en"), "12.35"), "EN dot format")
assert_true(identical(sl_fmt_num(Inf, 2L, "id"), "\u2014"), "Inf formats to em-dash")
assert_true(identical(sl_fmt_p(0.0004, "id"), "< 0,001"), "ID small p")
assert_true(identical(sl_fmt_p(0.0321, "id"), "0,032"), "ID normal p")
assert_true(identical(sl_fmt_p(0.0004, "en"), "< 0.001"), "EN small p")

# --- Expanded Edge Cases & Contracts ---

# 1. NULL and scalar non-finite / NA primitives serialize to JSON null
assert_true(identical(sl_to_json(NULL), "null"), "NULL -> null")
assert_true(identical(sl_to_json(NA), "null"), "NA logical -> null")
assert_true(identical(sl_to_json(NA_integer_), "null"), "NA_integer_ -> null")
assert_true(identical(sl_to_json(NA_real_), "null"), "NA_real_ -> null")
assert_true(identical(sl_to_json(NA_character_), "null"), "NA_character_ -> null")
assert_true(identical(sl_to_json(NaN), "null"), "NaN -> null")
assert_true(identical(sl_to_json(Inf), "null"), "Inf -> null")
assert_true(identical(sl_to_json(-Inf), "null"), "-Inf -> null")

# 2. sl_box_vec class tag and boxing behavior (including length-0, length-1, factor, NULL, AsIs)
boxed_one <- sl_box_vec(42L)
assert_true(inherits(boxed_one, "sl_array"), "sl_box_vec attaches sl_array class")
assert_true(identical(sl_to_json(boxed_one), "[42]"), "boxed integer scalar -> [42]")
assert_true(identical(sl_to_json(sl_box_vec(FALSE)), "[false]"), "boxed logical scalar -> [false]")
assert_true(identical(sl_to_json(sl_box_vec(NULL)), "[]"), "boxed NULL -> []")
assert_true(identical(sl_to_json(I("tunggal")), "[\"tunggal\"]"), "AsIs I() length-1 -> JSON array")
assert_true(identical(sl_to_json(c(W = 0.965)), "0.965"), "named length-1 numeric unboxes to scalar primitive")
assert_true(identical(sl_to_json(numeric(0)), "[]"), "unboxed length-0 atomic vector -> []")
assert_true(identical(sl_to_json(c(TRUE, NA, FALSE)), "[true,null,false]"), "logical vector with NA")

# 3. Double precision parity (1e-8 requirement via %.15g)
prec_vals <- c(pi, 1e-12, -9876543.21098765, 0.1 + 0.2, 0.0, -0.0, 1.23456789012345e18)
j_prec <- sl_to_json(list(nums = prec_vals))
p_prec <- jsonlite::fromJSON(j_prec, simplifyVector = TRUE)
assert_near(p_prec$nums, prec_vals, tol = 1e-12, msg = "double precision round-trip <= 1e-12")
assert_true(identical(sl_to_json(-0.0), "0"), "negative zero normalized to 0 in JSON")

# 4. Named vs unnamed lists & nested structures
assert_true(identical(sl_to_json(list()), "[]"), "empty unnamed list -> []")
assert_true(identical(sl_to_json(stats::setNames(list(), character(0))), "{}"), "empty named list -> {}")
j_unnamed <- sl_to_json(list(1L, "dua", FALSE, NULL, list(a = sl_box_vec(7L))))
p_unnamed <- jsonlite::fromJSON(j_unnamed, simplifyVector = FALSE)
assert_true(
  is.list(p_unnamed) && is.null(names(p_unnamed)) && length(p_unnamed) == 5L &&
    identical(p_unnamed[[1]], 1L) && identical(p_unnamed[[2]], "dua") &&
    identical(p_unnamed[[3]], FALSE) && is.null(p_unnamed[[4]]) &&
    identical(p_unnamed[[5]]$a[[1]], 7L),
  "unnamed list -> JSON array with nested object"
)

# 5. data.frame -> JSON array of row objects (multi-row, 1-row, 0-row, factors, NAs)
df_multi <- data.frame(
  perlakuan = factor(c("Kontrol", "Pakan A", NA)),
  kode = c("K0", "PA", "PB"),
  n = c(10L, NA_integer_, 12L),
  rerata = c(25.125, Inf, NA_real_),
  signif = c(FALSE, TRUE, NA),
  stringsAsFactors = FALSE
)
j_df <- sl_to_json(df_multi)
p_df <- jsonlite::fromJSON(j_df, simplifyVector = FALSE)
assert_true(is.list(p_df) && is.null(names(p_df)) && length(p_df) == 3L, "data.frame -> 3-element row array")
assert_true(
  identical(p_df[[1]]$perlakuan, "Kontrol") &&
    identical(p_df[[1]]$kode, "K0") &&
    identical(p_df[[1]]$n, 10L) &&
    identical(p_df[[1]]$signif, FALSE),
  "data.frame row 1 fields including factor -> string"
)
assert_near(p_df[[1]]$rerata, 25.125, tol = 1e-12, msg = "data.frame row 1 numeric")
assert_true(
  identical(p_df[[2]]$perlakuan, "Pakan A") &&
    is.null(p_df[[2]]$n) &&
    is.null(p_df[[2]]$rerata) &&
    identical(p_df[[2]]$signif, TRUE),
  "data.frame row 2 NA_integer_ and Inf -> null"
)
assert_true(
  is.null(p_df[[3]]$perlakuan) &&
    is.null(p_df[[3]]$rerata) &&
    is.null(p_df[[3]]$signif),
  "data.frame row 3 factor NA, NA_real_, logical NA -> null"
)

df_one <- data.frame(kelompok = "A", nilai = 3.5, stringsAsFactors = FALSE)
p_df_one <- jsonlite::fromJSON(sl_to_json(df_one), simplifyVector = FALSE)
assert_true(
  is.list(p_df_one) && is.null(names(p_df_one)) && length(p_df_one) == 1L &&
    identical(p_df_one[[1]]$kelompok, "A"),
  "1-row data.frame still serializes as length-1 JSON array of row object"
)

df_empty <- df_one[0, , drop = FALSE]
assert_true(identical(sl_to_json(df_empty), "[]"), "0-row data.frame -> []")

# 6. Full RFC 8259 control character (< 0x20) escaping and UTF-8 preservation
all_ctrl <- intToUtf8(1:31)
special_str <- paste0(
  "Quote:\" Backslash:\\ Backspace:\b Formfeed:\f Newline:\n CR:\r Tab:\t ",
  "Ctrl:[", all_ctrl, "] Unicode: \u2014 \u00b1 \u03b1=0,05 \u03b7\u00b2"
)
j_ctrl <- sl_to_json(list(text = special_str, key_test = stats::setNames(list(1L), "k\"1\n")))
p_ctrl <- jsonlite::fromJSON(j_ctrl, simplifyVector = FALSE)
assert_true(identical(p_ctrl$text, special_str), "all control chars 0x01..0x1F + quotes + UTF-8 round-trip")
assert_true(identical(p_ctrl$key_test[["k\"1\n"]], 1L), "object key escaping round-trip")

# 6b. Boxed factor, matrix, and data.frame special column names / list-columns
assert_true(identical(sl_to_json(sl_box_vec(factor("Kontrol"))), "[\"Kontrol\"]"),
            "boxed length-1 factor -> [\"Kontrol\"]")
mat2 <- matrix(c(1, 0.5, NA_real_, 1), nrow = 2L, byrow = TRUE)
assert_true(identical(sl_to_json(mat2), "[[1,0.5],[null,1]]"),
            "2x2 matrix -> array of row arrays with NA as null")
df_special <- data.frame(sep = c(1L, 2L), collapse = c("x", "y"), stringsAsFactors = FALSE)
df_special$tags <- list(sl_box_vec("a"), sl_box_vec(character(0)))
p_df_spec <- jsonlite::fromJSON(sl_to_json(df_special), simplifyVector = FALSE)
assert_true(
  length(p_df_spec) == 2L &&
    identical(p_df_spec[[1]]$sep, 1L) &&
    identical(p_df_spec[[1]]$collapse, "x") &&
    identical(p_df_spec[[1]]$tags, list("a")) &&
    identical(p_df_spec[[2]]$tags, list()),
  "data.frame with sep/collapse column names and list-column"
)

# 7. sl_fmt_num & sl_fmt_p defaults, vectorization, boundaries, and non-finite edge cases
assert_true(identical(sl_fmt_num(12.345), "12,3450"), "sl_fmt_num default digits=4, lang=id")
assert_true(identical(sl_fmt_num(12.345, lang = "en"), "12.3450"), "sl_fmt_num default digits=4, lang=en")
assert_true(
  identical(
    sl_fmt_num(c(1.2, -3.456, -1e-9, NA_real_, NaN, Inf, -Inf), digits = 2L, lang = "id"),
    c("1,20", "-3,46", "0,00", "\u2014", "\u2014", "\u2014", "\u2014")
  ),
  "sl_fmt_num vectorized with negative zero suppression and non-finite -> em-dash"
)
assert_true(identical(sl_fmt_num(numeric(0)), character(0)), "sl_fmt_num empty vector -> character(0)")
assert_true(identical(sl_fmt_num(NULL), "\u2014"), "sl_fmt_num NULL -> em-dash")

assert_true(
  identical(
    sl_fmt_p(c(0, 0.00099, 0.001, 0.0321, 1, NA_real_, NaN, Inf, -Inf), lang = "id"),
    c("< 0,001", "< 0,001", "0,001", "0,032", "1,000", "\u2014", "\u2014", "\u2014", "\u2014")
  ),
  "sl_fmt_p vectorized ID boundaries and non-finite -> em-dash"
)
assert_true(
  identical(
    sl_fmt_p(c(0, 0.00099, 0.001, 0.0321, 1, NA_real_), lang = "en"),
    c("< 0.001", "< 0.001", "0.001", "0.032", "1.000", "\u2014")
  ),
  "sl_fmt_p vectorized EN boundaries and NA -> em-dash"
)
assert_true(identical(sl_fmt_p(numeric(0)), character(0)), "sl_fmt_p empty vector -> character(0)")
assert_true(identical(sl_fmt_p(NULL), "\u2014"), "sl_fmt_p NULL -> em-dash")

# 8. Pure Base-R constraint verification on R/00_json.R source text
r00_lines <- readLines("R/00_json.R", encoding = "UTF-8", warn = FALSE)
r00_code <- paste(sub("#.*$", "", r00_lines), collapse = "\n")
assert_true(!grepl("jsonlite|library\\s*\\(|require\\s*\\(|requireNamespace\\s*\\(", r00_code),
            "R/00_json.R has zero CRAN/jsonlite/library calls")
