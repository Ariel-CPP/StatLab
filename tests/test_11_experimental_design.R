# tests/test_11_experimental_design.R
# Test suite for StatLab Experimental Design Engine (pure Base R)
# RCBD / RAKL (Randomized Complete Block Design) & Split-Plot Design

cat("\n=== Testing StatLab Experimental Design Engine (R/11_experimental_design.R) ===\n")

# ---------------------------------------------------------------------------
# Test 1: RCBD / RAKL (Randomized Complete Block Design)
# ---------------------------------------------------------------------------
cat("--- 1. Testing RCBD / RAKL Analysis ---\n")

df_rcbd_long <- data.frame(
  Kelompok = factor(rep(paste0("Blok_", 1:4), each = 3)),
  Perlakuan = factor(rep(c("Kontrol", "Pakan_A", "Pakan_B"), times = 4)),
  Biomassa_g = c(22.5, 26.0, 31.2, 23.0, 27.2, 32.5, 21.8, 25.5, 30.8, 23.5, 26.8, 33.0)
)

res_rcbd <- sl_analyze_rcbd(df_rcbd_long, list(
  roles = list(mode = "long", response_cols = "Biomassa_g", group_col = "Perlakuan", block_col = "Kelompok"),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_rcbd), "RCBD returns a list")
assert_true(!is.null(res_rcbd$main$f_treatment), "F-treatment present")
assert_true(!is.null(res_rcbd$main$f_block), "F-block present")
assert_true(res_rcbd$main$p_treatment < 0.001, "Treatment effect is highly significant")
assert_true(!is.null(res_rcbd$main$cv_error), "Coefficient of variation (KK) present")
assert_true(res_rcbd$main$cv_error < 5.0, "High experimental precision (KK < 5%)")
assert_true(length(res_rcbd$tables_apa) >= 2L, "RCBD has at least 2 tables (ANOVA + Means/Letters)")
assert_true(length(res_rcbd$plots) >= 2L, "RCBD generates at least 2 plots (Barplot letters + Profile)")
assert_true(grepl("• \\*\\*Signifikansi Perlakuan\\*\\*", res_rcbd$interpretation$id), "Key points format in RCBD")

# Test wide mode for RCBD (rows = blocks, columns = treatments)
df_rcbd_wide <- data.frame(
  Blok = paste0("Blok_", 1:4),
  Kontrol = c(22.5, 23.0, 21.8, 23.5),
  Pakan_A = c(26.0, 27.2, 25.5, 26.8),
  Pakan_B = c(31.2, 32.5, 30.8, 33.0)
)
res_rcbd_wide <- sl_analyze_rcbd(df_rcbd_wide, list(
  roles = list(mode = "wide", response_cols = c("Kontrol", "Pakan_A", "Pakan_B")),
  alpha = 0.05,
  include_plots = FALSE
))
assert_near(res_rcbd_wide$main$f_treatment, res_rcbd$main$f_treatment, tol = 1e-4, msg = "RCBD wide and long match")

# ---------------------------------------------------------------------------
# Test 2: Split-Plot Design (Rancangan Petak Terbagi)
# ---------------------------------------------------------------------------
cat("--- 2. Testing Split-Plot Design Analysis ---\n")

r <- 3; a <- 2; b <- 3
df_sp <- expand.grid(
  Anak_Petak = paste0("B", 1:b),
  Petak_Utama = paste0("A", 1:a),
  Kelompok = paste0("Blok_", 1:r)
)
df_sp$Pertumbuhan <- c(10.2, 12.5, 14.1, 15.3, 18.2, 20.1,
                       11.0, 13.1, 15.0, 16.2, 19.1, 21.0,
                       9.8, 11.9, 13.5, 14.8, 17.5, 19.4)

res_sp <- sl_analyze_split_plot(df_sp, list(
  roles = list(
    mode = "long",
    response_cols = "Pertumbuhan",
    main_plot_col = "Petak_Utama",
    sub_plot_col = "Anak_Petak",
    block_col = "Kelompok"
  ),
  alpha = 0.05,
  include_plots = TRUE
))

assert_true(is.list(res_sp), "Split-Plot returns a list")
assert_true(!is.null(res_sp$main$f_main_plot), "F Main Plot present")
assert_true(!is.null(res_sp$main$f_sub_plot), "F Sub Plot present")
assert_true(!is.null(res_sp$main$f_interaction), "F Interaction present")
assert_true(!is.null(res_sp$main$cv_a), "KK(a) present")
assert_true(!is.null(res_sp$main$cv_b), "KK(b) present")
assert_true(length(res_sp$tables_apa) >= 2L, "Split-Plot has at least 2 tables (ANOVA + Interaction Matrix)")
assert_true(length(res_sp$plots) >= 1L, "Split-Plot has interaction plot")
assert_true(grepl("• \\*\\*Petak Utama", res_sp$interpretation$id), "Key points format in Split-Plot")

# ---------------------------------------------------------------------------
# Test 3: Dispatcher Routing
# ---------------------------------------------------------------------------
cat("--- 3. Testing Dispatcher Routing ---\n")

disp_rcbd <- sl_dispatch("run_analysis", list(
  analysis_type = "rcbd",
  data = df_rcbd_wide,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_rcbd), "rcbd dispatched")

disp_rakl <- sl_dispatch("run_analysis", list(
  analysis_type = "rakl",
  data = df_rcbd_wide,
  opts = list(include_plots = FALSE)
))
assert_true(is.character(disp_rakl), "rakl alias dispatched")

disp_sp <- sl_dispatch("run_analysis", list(
  analysis_type = "split_plot",
  data = df_sp,
  opts = list(
    roles = list(
      mode = "long",
      response_cols = "Pertumbuhan",
      main_plot_col = "Petak_Utama",
      sub_plot_col = "Anak_Petak",
      block_col = "Kelompok"
    ),
    include_plots = FALSE
  )
))
assert_true(is.character(disp_sp), "split_plot dispatched")

cat("=== All Experimental Design Engine Tests Passed ===\n")
