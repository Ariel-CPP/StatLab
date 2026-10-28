# tests/test_02_assumptions.R
data(iris)
data(ToothGrowth)

# 1. Lilliefors vs nortest::lillie.test across small (n=15), medium (n=50), large (n=150), and high-p normal sample
for (vec in list(iris$Sepal.Length, iris$Sepal.Width[1:15], iris$Petal.Length, ToothGrowth$len)) {
  ours <- sl_lilliefors_test(vec)
  ref  <- nortest::lillie.test(vec)
  assert_near(ours$statistic, unname(ref$statistic), 1e-8, "Lilliefors D vs nortest")
  assert_near(ours$p_value, ref$p.value, 1e-8, "Lilliefors p vs nortest")
}

# 2. Levene Brown-Forsythe (median) & classic (mean) vs car::leveneTest on iris & ToothGrowth
lev_med <- sl_levene_test(iris$Sepal.Length, iris$Species, center = "median")
ref_med <- car::leveneTest(Sepal.Length ~ Species, data = iris, center = median)
assert_near(lev_med$statistic, ref_med$`F value`[1], 1e-8, "Levene BF F vs car")
assert_near(lev_med$p_value, ref_med$`Pr(>F)`[1], 1e-8, "Levene BF p vs car")

lev_mean <- sl_levene_test(ToothGrowth$len, factor(ToothGrowth$dose), center = "mean")
ref_mean <- car::leveneTest(len ~ factor(dose), data = ToothGrowth, center = mean)
assert_near(lev_mean$statistic, ref_mean$`F value`[1], 1e-8, "Levene mean F vs car")
assert_near(lev_mean$p_value, ref_mean$`Pr(>F)`[1], 1e-8, "Levene mean p vs car")

# 3. Spec §6.2 decision rule: residual normality overrides per-group failure, cutoff follows chosen alpha
set.seed(1)
g_skew <- c(rexp(12, 1), rnorm(12, 5, 1))
grp2   <- factor(rep(c("A", "B"), each = 12))
chk_no_res <- sl_check_assumptions(g_skew, grp2, residuals = NULL, alpha = 0.05)
assert_true(chk_no_res$normality_rule == "per_group" && chk_no_res$normality_violated, "Per-group violation detected")
chk_no_res_01 <- sl_check_assumptions(g_skew, grp2, residuals = NULL, alpha = 0.01)
assert_true(!chk_no_res_01$normality_violated, "Per-group cutoff follows chosen alpha=0.01 (p ~= 0.0253)")

fake_normal_res <- qnorm(ppoints(24))
chk_with_res <- sl_check_assumptions(g_skew, grp2, residuals = fake_normal_res, alpha = 0.05)
assert_true(chk_with_res$normality_rule == "residuals" && !chk_with_res$normality_violated,
            "Normal residuals override single-group skew per Spec §6.2")

# 4. Review Focus #2: Constant group c(100,100,100), n=2, and n=5001
assert_true(!sl_shapiro_test(c(100, 100, 100))$valid, "Constant Shapiro graceful")
assert_true(!sl_shapiro_test(c(1.2, 3.4))$valid, "n=2 Shapiro graceful")
assert_true(!sl_shapiro_test(rnorm(5001))$valid, "n=5001 Shapiro skipped gracefully")
assert_true(sl_lilliefors_test(rnorm(5001))$valid, "n=5001 Lilliefors still runs")
assert_true(!sl_bartlett_test(c(100,100,100, 90,92,91), factor(rep(c("K","P"), each=3)))$valid,
            "Zero-variance group Bartlett graceful")

# --- Expanded Oracle & Downstream Contract Suite ---

# 5. Shapiro-Wilk vs stats::shapiro.test across n=3, iris groups, ToothGrowth, n=5000, and NA/Inf filtering
set.seed(101)
shapiro_cases <- list(
  n3_boundary = c(1.0, 2.0, 4.5),
  setosa_sl   = iris$Sepal.Length[iris$Species == "setosa"],
  setosa_pw   = iris$Petal.Width[iris$Species == "setosa"],
  tg_len      = ToothGrowth$len,
  with_na_inf = c(NA_real_, iris$Sepal.Width[1:20], Inf, -Inf, NaN),
  n5000_bound = rnorm(5000)
)
for (nm in names(shapiro_cases)) {
  vec <- shapiro_cases[[nm]]
  fin_vec <- vec[is.finite(vec)]
  ours <- sl_shapiro_test(vec)
  ref  <- stats::shapiro.test(fin_vec)
  assert_true(isTRUE(ours$valid) && identical(ours$n, length(fin_vec)) && is.na(ours$note_key),
              paste("Shapiro valid & metadata:", nm))
  assert_near(ours$statistic, unname(ref$statistic), 1e-8, paste("Shapiro W parity:", nm))
  assert_near(ours$p_value, ref$p.value, 1e-8, paste("Shapiro p parity:", nm))
}

assert_true(!sl_shapiro_test(numeric(0))$valid && nzchar(sl_shapiro_test(numeric(0))$note_key),
            "Empty vector Shapiro invalid with note_key")
assert_true(!sl_shapiro_test(c(NA_real_, Inf))$valid, "All non-finite Shapiro invalid")

# 6. Lilliefors vs nortest::lillie.test across ALL piecewise branches (p <= 0.1, p > 0.1 sub-intervals, n=5, n > 100, n=5001)
set.seed(2026)
lillie_cases <- list(
  n5_boundary      = c(1.0, 2.2, 2.9, 3.7, 5.1),
  p_just_above_01  = c(0.0, 1.0, 2.0, 3.0, 9.8),
  p_just_below_01  = c(0.0, 1.0, 2.0, 3.0, 10.0),
  kk_le_0302_n20   = qnorm(ppoints(20)),
  kk_le_0302_n150  = qnorm(ppoints(150)),
  high_p_setosa_sl = iris$Sepal.Length[iris$Species == "setosa"],
  high_p_versic_sl = iris$Sepal.Length[iris$Species == "versicolor"],
  high_p_virgin_sl = iris$Sepal.Length[iris$Species == "virginica"],
  high_p_n120      = rnorm(120),
  high_p_n250      = rnorm(250),
  low_p_n30        = rexp(30, rate = 1),
  low_p_n100       = rexp(100, rate = 1),
  low_p_n250       = rexp(250, rate = 1),
  n5001_normal     = rnorm(5001),
  n5001_skewed     = rexp(5001, rate = 1)
)
for (nm in names(lillie_cases)) {
  vec <- lillie_cases[[nm]]
  ours <- sl_lilliefors_test(vec)
  ref  <- nortest::lillie.test(vec)
  assert_true(isTRUE(ours$valid) && identical(ours$n, length(vec)) && is.na(ours$note_key),
              paste("Lilliefors valid & metadata:", nm))
  assert_near(ours$statistic, unname(ref$statistic), 1e-8, paste("Lilliefors D parity:", nm))
  assert_near(ours$p_value, ref$p.value, 1e-8, paste("Lilliefors p parity:", nm))
}

assert_true(!sl_lilliefors_test(c(1.0, 2.0, 3.0, 4.0))$valid &&
            nzchar(sl_lilliefors_test(c(1.0, 2.0, 3.0, 4.0))$note_key),
            "n=4 Lilliefors invalid with note_key")
assert_true(!sl_lilliefors_test(rep(100, 6))$valid &&
            nzchar(sl_lilliefors_test(rep(100, 6))$note_key),
            "Constant n=6 Lilliefors invalid with note_key")

# 7. Expanded Levene (median & mean) vs car::leveneTest + Bartlett vs stats::bartlett.test
for (col_nm in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")) {
  y_col <- iris[[col_nm]]
  g_col <- iris$Species

  lm_ours <- sl_levene_test(y_col, g_col, center = "median")
  lm_ref  <- car::leveneTest(y_col, g_col, center = median)
  assert_true(lm_ours$valid && identical(lm_ours$center, "median") &&
              identical(lm_ours$df1, as.integer(lm_ref$Df[1])) &&
              identical(lm_ours$df2, as.integer(lm_ref$Df[2])),
              paste("Levene median metadata:", col_nm))
  assert_near(lm_ours$statistic, lm_ref$`F value`[1], 1e-8, paste("Levene median F:", col_nm))
  assert_near(lm_ours$p_value, lm_ref$`Pr(>F)`[1], 1e-8, paste("Levene median p:", col_nm))

  la_ours <- sl_levene_test(y_col, g_col, center = "mean")
  la_ref  <- car::leveneTest(y_col, g_col, center = mean)
  assert_true(la_ours$valid && identical(la_ours$center, "mean"), paste("Levene mean metadata:", col_nm))
  assert_near(la_ours$statistic, la_ref$`F value`[1], 1e-8, paste("Levene mean F:", col_nm))
  assert_near(la_ours$p_value, la_ref$`Pr(>F)`[1], 1e-8, paste("Levene mean p:", col_nm))

  bt_ours <- sl_bartlett_test(y_col, g_col)
  bt_ref  <- stats::bartlett.test(y_col, g_col)
  assert_true(bt_ours$valid && identical(bt_ours$df, as.integer(unname(bt_ref$parameter))),
              paste("Bartlett metadata:", col_nm))
  assert_near(bt_ours$statistic, unname(bt_ref$statistic), 1e-8, paste("Bartlett K^2:", col_nm))
  assert_near(bt_ours$p_value, bt_ref$p.value, 1e-8, paste("Bartlett p:", col_nm))
}

# Unbalanced groups with NAs + aquaculture 100%-survival control group Levene check
y_surv <- c(100, 100, 100, 90, 92, 91)
g_surv <- factor(rep(c("K", "P"), each = 3))
lev_surv <- sl_levene_test(y_surv, g_surv, center = "median")
ref_surv <- car::leveneTest(y_surv, g_surv, center = median)
assert_true(lev_surv$valid, "Levene BF remains valid when one group is constant and another varies")
assert_near(lev_surv$statistic, ref_surv$`F value`[1], 1e-8, "100%-survival control Levene BF F")
assert_near(lev_surv$p_value, ref_surv$`Pr(>F)`[1], 1e-8, "100%-survival control Levene BF p")

assert_true(!sl_levene_test(c(100, 100, 100, 90, 90, 90), g_surv)$valid,
            "All-constant groups Levene returns valid=FALSE instead of NaN")
assert_true(!sl_levene_test(c(10, 20, 30), factor(rep("OnlyOne", 3)))$valid,
            "Single-group Levene returns valid=FALSE")
assert_true(!sl_levene_test(c(10, 20), factor(c("A", "B")))$valid,
            "df2=0 (n=1 per group) Levene returns valid=FALSE")
assert_true(!sl_bartlett_test(c(10, 20, 22), factor(c("A", "B", "B")))$valid,
            "Group with n=1 Bartlett returns valid=FALSE")
assert_true(!sl_bartlett_test(c(10, 20, 30), factor(rep("OnlyOne", 3)))$valid,
            "Single-group Bartlett returns valid=FALSE")

# High-baseline (10^7 cells/mL) n=2 and symmetric 2-point n=4 groups (identically zero deviation variance)
y_hi_n2 <- c(1e7 + 0.1, 1e7 + 0.3, 1e7 + 0.1, 1e7 + 0.5)
g_hi_n2 <- factor(c("A", "A", "B", "B"))
lev_hi_n2 <- sl_levene_test(y_hi_n2, g_hi_n2, center = "median")
assert_true(!lev_hi_n2$valid && identical(lev_hi_n2$note_key, "zero_variance"),
            "High-baseline n=2 groups Levene returns valid=FALSE (zero_variance), not rounding noise F")

y_hi_sym4 <- c(1e7 + 0.1, 1e7 + 0.1, 1e7 + 0.3, 1e7 + 0.3,
               1e7 + 0.1, 1e7 + 0.1, 1e7 + 0.5, 1e7 + 0.5)
g_hi_sym4 <- factor(rep(c("A", "B"), each = 4))
lev_hi_sym4 <- sl_levene_test(y_hi_sym4, g_hi_sym4, center = "median")
assert_true(!lev_hi_sym4$valid && identical(lev_hi_sym4$note_key, "zero_variance"),
            "High-baseline symmetric 2-point n=4 groups Levene returns valid=FALSE (zero_variance)")

# Large between-group mean separation with small within-group SD (n >= 3) vs car::leveneTest
y_big_sep <- c(1e-4, 2e-4, 3e-4, 1e7 + 1e-4, 1e7 + 2e-4, 1e7 + 3e-4)
g_big_sep <- factor(c("A", "A", "A", "B", "B", "B"))
lev_sep_med <- sl_levene_test(y_big_sep, g_big_sep, center = "median")
ref_sep_med <- car::leveneTest(y_big_sep, g_big_sep, center = median)
assert_true(lev_sep_med$valid, "Large between-group mean separation n=3 Levene median remains valid")
assert_near(lev_sep_med$statistic, ref_sep_med$`F value`[1], 1e-8, "Large mean separation Levene BF F vs car")
assert_near(lev_sep_med$p_value, ref_sep_med$`Pr(>F)`[1], 1e-8, "Large mean separation Levene BF p vs car")

lev_sep_mn <- sl_levene_test(y_big_sep, g_big_sep, center = "mean")
ref_sep_mn <- car::leveneTest(y_big_sep, g_big_sep, center = mean)
assert_true(lev_sep_mn$valid, "Large between-group mean separation n=3 Levene mean remains valid")
assert_near(lev_sep_mn$statistic, ref_sep_mn$`F value`[1], 1e-8, "Large mean separation Levene mean F vs car")
assert_near(lev_sep_mn$p_value, ref_sep_mn$`Pr(>F)`[1], 1e-8, "Large mean separation Levene mean p vs car")

# 8. Spec §6.2 & Downstream JSON Contract for sl_check_assumptions
# 8a. Skewed residuals trigger violation even when per-group values are normal
norm_2g <- c(qnorm(ppoints(12), mean = 10, sd = 1), qnorm(ppoints(12), mean = 15, sd = 1))
skew_res <- c(rep(-0.2, 21), 4.5, 5.2, 6.8)
chk_bad_res <- sl_check_assumptions(norm_2g, grp2, residuals = skew_res, alpha = 0.05)
assert_true(identical(chk_bad_res$normality_rule, "residuals") &&
            isTRUE(chk_bad_res$normality_violated) &&
            identical(chk_bad_res$residual_normality$is_normal, FALSE),
            "Non-normal residuals violate normality even if individual groups are normal")

# 8b. Alpha sensitivity (alpha = 0.05 vs alpha = 0.01) for both normality and homogeneity
# Construct sample with 0.01 < Shapiro p < 0.05 (p ~= 0.04549)
vec_mid_p <- c(10.0, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6, 10.8, 11.1, 12.1)
sw_mid <- stats::shapiro.test(vec_mid_p)$p.value
assert_true(sw_mid > 0.01 && sw_mid < 0.05, "Fixture has 0.01 < Shapiro p < 0.05")
chk_a05 <- sl_check_assumptions(vec_mid_p, groups = NULL, alpha = 0.05)
chk_a01 <- sl_check_assumptions(vec_mid_p, groups = NULL, alpha = 0.01)
assert_true(chk_a05$normality_violated && !chk_a01$normality_violated,
            "Normality violation strictly follows user-selected alpha")
assert_true(is.null(chk_a05$homogeneity) && identical(chk_a05$homogeneity_violated, FALSE),
            "groups=NULL produces homogeneity=NULL and homogeneity_violated=FALSE")
assert_true(length(chk_a05$per_group) == 1L && identical(chk_a05$per_group[[1]]$group, "All"),
            "groups=NULL treats values as single group 'All'")

# Construct 2-group sample with 0.01 < Brown-Forsythe Levene p < 0.05 (p ~= 0.03145)
y_hom_mid <- c(9, 10, 11, 9.5, 10.5, 10, 6, 14, 8, 12, 9.5, 10.5)
g_hom_mid <- factor(rep(c("A", "B"), each = 6))
hom_a05 <- sl_check_assumptions(y_hom_mid, g_hom_mid, alpha = 0.05)
hom_a01 <- sl_check_assumptions(y_hom_mid, g_hom_mid, alpha = 0.01)
assert_true(hom_a05$homogeneity_violated && !hom_a01$homogeneity_violated &&
            identical(hom_a05$homogeneity$is_homogeneous, FALSE) &&
            identical(hom_a01$homogeneity$is_homogeneous, TRUE),
            "Homogeneity violation strictly follows user-selected alpha")

# 8c. Homogeneity decision follows Brown-Forsythe Levene (NOT Bartlett) and respects require_homogeneity=FALSE
# Heavy-tailed equal-scale groups where Bartlett p < 0.05 but Brown-Forsythe Levene p >= 0.05
g_heavy_a <- c(-8.0, -0.4, -0.2, -0.1, 0.0, 0.1, 0.2, 0.4, 8.0)
g_heavy_b <- c(-2.2, -1.8, -1.4, -0.8, 0.0, 0.8, 1.4, 1.8, 2.2)
y_heavy <- c(g_heavy_a, g_heavy_b)
grp_heavy <- factor(rep(c("Heavy", "Uniform"), each = 9), levels = c("Heavy", "Uniform"))
chk_heavy <- sl_check_assumptions(y_heavy, grp_heavy, alpha = 0.05, require_homogeneity = TRUE)
assert_true(chk_heavy$homogeneity$bartlett$p_value < 0.05 &&
            chk_heavy$homogeneity$levene_bf$p_value >= 0.05 &&
            identical(chk_heavy$homogeneity_violated, FALSE) &&
            identical(chk_heavy$homogeneity$is_homogeneous, TRUE),
            "Spec §6.2: Brown-Forsythe Levene (not Bartlett) governs homogeneity_violated")
assert_true(identical(names(chk_heavy$homogeneity), c("levene_bf", "bartlett", "is_homogeneous")),
            "homogeneity contains strictly levene_bf, bartlett, and is_homogeneous per task brief")

chk_no_hom <- sl_check_assumptions(iris$Sepal.Length, iris$Species, alpha = 0.05, require_homogeneity = FALSE)
assert_true(is.null(chk_no_hom$homogeneity) && identical(chk_no_hom$homogeneity_violated, FALSE),
            "require_homogeneity=FALSE skips homogeneity and sets homogeneity_violated=FALSE")

chk_iris <- sl_check_assumptions(iris$Sepal.Length, iris$Species, alpha = 0.05, require_homogeneity = TRUE)
assert_true(isTRUE(chk_iris$homogeneity_violated) && identical(chk_iris$homogeneity$is_homogeneous, FALSE),
            "iris Sepal.Length Brown-Forsythe Levene p < 0.05 sets homogeneity_violated=TRUE")

# 8d. n > 5000 fallback in sl_check_assumptions uses Lilliefors
set.seed(303)
big_norm <- rnorm(5001)
big_skew <- rexp(5001, rate = 1)
chk_big_norm <- sl_check_assumptions(big_norm, groups = NULL, alpha = 0.05)
chk_big_skew <- sl_check_assumptions(big_skew, groups = NULL, alpha = 0.05)
assert_true(!chk_big_norm$per_group[[1]]$shapiro$valid &&
            chk_big_norm$per_group[[1]]$lilliefors$valid &&
            isTRUE(chk_big_norm$per_group[[1]]$is_normal) &&
            !chk_big_norm$normality_violated,
            "n=5001 normal sample falls back to Lilliefors -> normal")
assert_true(!chk_big_skew$per_group[[1]]$shapiro$valid &&
            chk_big_skew$per_group[[1]]$lilliefors$valid &&
            identical(chk_big_skew$per_group[[1]]$is_normal, FALSE) &&
            chk_big_skew$normality_violated,
            "n=5001 skewed sample falls back to Lilliefors -> violated")

chk_big_res <- sl_check_assumptions(c(1, 2, 3, 4), factor(c("A", "A", "B", "B")), residuals = big_skew, alpha = 0.05)
assert_true(identical(chk_big_res$normality_rule, "residuals") &&
            !chk_big_res$residual_normality$shapiro$valid &&
            chk_big_res$residual_normality$lilliefors$valid &&
            chk_big_res$normality_violated,
            "n=5001 skewed residuals fall back to Lilliefors -> violated")

chk_one_nonempty <- sl_check_assumptions(
  c(10.1, 10.5, 10.3, NA_real_, NA_real_),
  factor(c("A", "A", "A", "B", "C"), levels = c("A", "B", "C")),
  alpha = 0.05,
  require_homogeneity = TRUE
)
assert_true(is.null(chk_one_nonempty$homogeneity) && identical(chk_one_nonempty$homogeneity_violated, FALSE),
            "< 2 non-empty groups sets homogeneity=NULL and homogeneity_violated=FALSE")

# 8e. JSON serialization contract: per_group serializes as unnamed JSON array [...] preserving group order
grp_non_alpha <- factor(rep(c("Kontrol", "Artemia"), each = 6), levels = c("Kontrol", "Artemia"))
val_non_alpha <- c(100, 100, 100, 100, 100, 100, 55.4, 58.1, 54.9, 56.2, 59.0, 53.5)
chk_contract <- sl_check_assumptions(val_non_alpha, grp_non_alpha, alpha = 0.05)
assert_true(is.null(names(chk_contract$per_group)), "per_group is an unnamed R list")
assert_true(is.na(chk_contract$per_group[[1]]$is_normal) && isTRUE(chk_contract$per_group[[2]]$is_normal),
            "Constant group has is_normal=NA while normal group has is_normal=TRUE")

json_str <- sl_to_json(chk_contract)
parsed_chk <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
assert_true(is.list(parsed_chk$per_group) && is.null(names(parsed_chk$per_group)) &&
            length(parsed_chk$per_group) == 2L &&
            identical(parsed_chk$per_group[[1]]$group, "Kontrol") &&
            identical(parsed_chk$per_group[[2]]$group, "Artemia") &&
            is.null(parsed_chk$per_group[[1]]$is_normal) &&
            identical(parsed_chk$per_group[[2]]$is_normal, TRUE),
            "per_group serializes as JSON array in factor level order with null for NA is_normal")

json_single <- sl_to_json(chk_a05)
parsed_single <- jsonlite::fromJSON(json_single, simplifyVector = FALSE)
assert_true(is.list(parsed_single$per_group) && is.null(names(parsed_single$per_group)) &&
            length(parsed_single$per_group) == 1L &&
            identical(parsed_single$per_group[[1]]$group, "All"),
            "Single-group per_group also serializes as a length-1 JSON array")

# 9. Zero warnings on degenerate/character inputs & Static Pure Base-R check
warn_count <- 0L
withCallingHandlers(
  {
    sl_shapiro_test(c(100, 100, 100))
    sl_lilliefors_test(c(100, 100, 100, 100, 100))
    sl_levene_test(c(10, 12, 11, 20, 25, 22), c("K", "K", "K", "P", "P", "P"), center = "median")
    sl_levene_test(c(90.1, 92.3, 10.0, 20.0), c("A", "A", "B", "B"), center = "median")
    sl_bartlett_test(c(100, 100, 100, 90, 92, 91), c("K", "K", "K", "P", "P", "P"))
    sl_check_assumptions(c(100, 100, 100, 1.2, 3.4), c("K", "K", "K", "T", "T"))
  },
  warning = function(w) {
    warn_count <<- warn_count + 1L
    invokeRestart("muffleWarning")
  }
)
assert_true(warn_count == 0L, "Zero warnings emitted across character groups and degenerate inputs")

r02_lines <- readLines("R/02_assumptions.R", encoding = "UTF-8", warn = FALSE)
r02_code <- paste(sub("#.*$", "", r02_lines), collapse = "\n")
assert_true(!grepl("(outliers|jsonlite|nortest|car)::|\\b(library|require|requireNamespace)\\s*\\(", r02_code),
            "R/02_assumptions.R uses strictly base and stats functions with zero external packages")
