# StatLab Phase 1 (Foundation) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the deployable Phase 1 Foundation of StatLab — a static offline-capable PWA running R inside the browser via webR (`PostMessage`), delivering editable spreadsheet + CSV input with Indonesian decimal-comma support, variable role mapping, mandatory normality & homogeneity testing, outlier flagging, descriptive biostatistics with APA & SPSS tables, R base-graphics plots, bilingual rule-based conclusions, cumulative Session Report, and vector PDF download.

**Architecture:** Dual-use pure base-R statistical modules (`R/*.R`) run identically in browser webR (`webr@0.5.4`, `ChannelType.PostMessage`) and desktop R 4.5.2 (for automated reference validation). Vanilla ES-module JavaScript (`js/*.js`) and `.sl-`-namespaced CSS (`css/statlab.css`) provide the "Lab Notebook" UI, locale-aware parsing, IndexedDB/`.statlab.json` persistence, and `pdfmake` vector PDF assembly without performing any statistical calculations in JavaScript.

**Tech Stack:** Base R (`base`, `stats`, `graphics`, `grDevices`, `utils` + vendored recommended packages `survival`, `cluster`, `MASS`), `webr@0.5.4` (WASM), Vanilla JS ES Modules, `katex@0.19.1`, `pdfmake@0.3.13`, Service Worker PWA, Desktop R 4.5.2 (`C:\Program Files\R\R-4.5.2\bin\Rscript.exe`) + Node.js v24 for test suites.

**Spec:** `C:\Users\Ariel's Device\.gemini\antigravity\scratch\statlab\docs\superpowers\specs\2026-10-28-statlab-design.md` (Phase 1 of §7; Phases 2–5 will have their own plans building on these exact interfaces).

## Global Constraints

- **Zero JS statistical math:** Every statistical quantity, test, outlier threshold, table cell number, plot, and conclusion text MUST be produced by R (`R/*.R`). JavaScript only handles UI, raw text parsing into typed columns, persistence, and PDF layout.
- **Allowed R packages in shipped app:** `base`, `stats`, `graphics`, `grDevices`, `utils`, `tools`, `methods`, plus recommended packages `survival`, `cluster`, `MASS`. No CRAN packages (including `jsonlite`) may be used by `R/*.R`.
- **Desktop test suite CRAN references allowed (§9.2):** `tests/` running on desktop R 4.5.2 (`"C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe"`) may load `car`, `nortest`, and `outliers` strictly as validation oracles (`<= 1e-8` tolerance).
- **Mandatory assumption-first rule (§2, §6.2, §6.3):** Normality (Shapiro-Wilk for `3 <= n <= 5000` + Lilliefors Dallal-Wilkinson + Q-Q plot) and homogeneity (Brown-Forsythe median-centered Levene + Bartlett when `>= 2` groups) always run and appear before main results. Violation cutoff = user-selected `alpha` (`0.05` default or `0.01`).
- **Outlier non-deletion rule (§2, §6.5):** Outliers (1.5×IQR and two-sided Grubbs test) are flagged in tables/plots and reported, NEVER removed unless the user explicitly passes row indices in `exclude_rows`, which is then recorded in the report.
- **Bilingual & Indonesian number locale (§6.7, §6.10b):** Input auto-detects `,` decimal and `;` CSV delimiter with manual override (`auto` | `id_comma` | `en_dot`). Output in `"id"` uses decimal comma (`p = 0,032`, `12,45 ± 1,20`, CSV `;`), and `"en"` uses decimal point (`p = 0.032`, `12.45 ± 1.20`, CSV `,`).
- **Static GitHub Pages & Aqua Tools merge readiness (§3, §6.11):** webR uses `ChannelType.PostMessage` (no COOP/COEP headers required); all vendor assets served locally under `./vendor/` with every file `< 50 MB` and `.nojekyll` at root; all CSS prefixed `.sl-` and JS mounted via `mountStatLab(rootEl, config)` with zero unprefixed globals.

## Review Focus

1. **Ambiguous 3-trailing-digit numbers (`"1.234"` or `"12,345"`):** When a column contains only 3-digit fractional/thousands patterns without disambiguating values (like `"1.234,5"` or `"3,5"`), `detectLocaleFormat` marks `ambiguous: true`, defaults `"1.234"` to decimal `1.234` unless `decimalMode === "id_comma"` with explicit thousands-dot rules, and triggers a UI confirmation badge rather than silently multiplying fish weights by 1000×. (Pinned in Task 5)
2. **Constant / zero-variance or tiny groups (e.g., 100% survival `c(100, 100, 100)` or `n = 2`):** `shapiro.test`, `bartlett.test`, Lilliefors, and Grubbs divide by `sd` or require minimum `n`; R functions must catch `sd == 0`, `n < 3`, `n < 5`, and `n > 5000`, returning `valid = FALSE` with a bilingual diagnostic note instead of throwing an R error. (Pinned in Task 2 and Task 3)
3. **Dirty cells in numeric columns (`"12,5 "`, `""`, `"NA"`, `"N/A"`, `"-"`, or stray text `"mati"`):** Standard missing tokens become `NA` and increment `n_missing`; unexpected non-numeric text in a numeric response role coerces to `NA` AND emits a bilingual warning naming the coerced row indices. (Pinned in Task 2 and Task 5)
4. **Unequal replicate counts (jagged columns) in Wide Format:** When Treatment A has 8 replicates and Treatment B has 10 in wide grid columns, reshaping wide→long drops empty trailing cells per column independently rather than deleting rows 9–10 of Treatment B. (Pinned in Task 2)
5. **Zero mean (`mean == 0`) or non-finite values (`NA`, `NaN`, `Inf`):** `cv_pct` returns `NA_real_` when `abs(mean) < 1e-12`, and `sl_to_json` serializes all `NA`/`NaN`/`Inf`/`-Inf` values to valid JSON `null`. (Pinned in Task 1 and Task 4)

---

## File Map

```text
C:\Users\Ariel's Device\.gemini\antigravity\scratch\statlab\
├── .gitignore
├── .nojekyll
├── index.html                  # Standalone PWA shell mounting #statlab-root
├── manifest.json               # PWA manifest (Lab Notebook theme colors)
├── sw.js                       # Offline cache for shell, R/*.R, and vendor/*
├── R/
│   ├── 00_json.R               # Pure base-R JSON serializer & bilingual number/p formatters
│   ├── 01_data_prep.R          # Wide/long group builder, listwise NA audit, IQR & Grubbs outliers
│   ├── 02_assumptions.R        # Shapiro-Wilk, Lilliefors (Dallal-Wilkinson), Levene (BF), Bartlett
│   ├── 03_plots_base.R         # Base graphics capture (300-DPI PNG + SVG): Q-Q, Hist+Normal, Box+Jitter
│   ├── 04_descriptive.R        # Descriptive stats engine, APA Layer-1 & SPSS Layer-2 tables, KaTeX formulas
│   ├── 05_conclusions.R        # Rule-based bilingual (ID/EN) per-analysis & General Conclusion templates
│   └── 99_dispatcher.R         # Single entry point sl_dispatch(action, payload) -> JSON string
├── css/
│   └── statlab.css             # .sl-* Lab Notebook theme (cream millimeter grid, ink blue, crimson, APA tables)
├── js/
│   ├── i18n.js                 # Bilingual UI dictionary (id / en)
│   ├── number-locale.js        # Auto-detect & parse comma/dot numbers, CSV/TSV import & export
│   ├── webr-client.js          # StatLabEngine (WebR PostMessage bridge, loads R/*.R)
│   ├── grid.js                 # StatLabGrid (editable table, Excel TSV paste, outlier badges & opt-in exclude)
│   ├── roles.js                # ANALYSIS_SCHEMAS & RoleMappingPanel (wide vs long role selectors)
│   ├── result-view.js          # Renders KaTeX, assumption cards, APA/SPSS tables, R plots, conclusions
│   ├── session-store.js        # Cumulative Session Report, IndexedDB/localStorage autosave, .statlab.json
│   ├── pdf-export.js           # pdfmake vector A4 PDF builder from R contract + 300-DPI PNG plots
│   └── app.js                  # mountStatLab(rootEl, config) controller & aquaculture presets
├── tools/
│   └── vendor-fetch.mjs        # Fetches pinned webr@0.5.4, katex@0.19.1, pdfmake@0.3.13 into vendor/
├── vendor/                     # Local static vendor assets (no external CDN at runtime)
└── tests/
    ├── run_r_tests.R           # Desktop R 4.5.2 test runner (loads test_*.R)
    ├── test_00_json.R
    ├── test_01_data_prep.R
    ├── test_02_assumptions.R
    ├── test_04_descriptive.R
    ├── test_webr_parity.mjs    # Node test: number-locale + webR vs R 4.5.2 parity <= 1e-8
    ├── test_ui_modules.mjs     # Node DOM/state test for grid, roles, i18n, result table export
    └── test_pdf_and_session.mjs# Node test for SessionStore, .statlab.json roundtrip, pdfmake docDefinition
```

---

### Task 1: Repository Bootstrap, Desktop R Test Runner & Pure Base-R JSON/Number Core

**Files:**
- Create: `.gitignore`
- Create: `.nojekyll`
- Create: `R/00_json.R`
- Create: `tests/run_r_tests.R`
- Test: `tests/test_00_json.R`

**Interfaces:**
- Consumes: Base R (`base`)
- Produces:
  - `sl_to_json(x)` -> `character(1)` valid UTF-8 JSON (`NULL` -> `"null"`, `NA`/`NaN`/`Inf`/`-Inf` -> `"null"`, scalar logical/numeric/character -> JSON primitive, length `!= 1` atomic vector -> JSON array unless wrapped in `I()` for length-1 array `sl_box_vec(v)`, named `list` -> JSON object, unnamed `list` -> JSON array, `data.frame` -> JSON array of row objects).
  - `sl_box_vec(x)` -> marks vector `x` with class `"sl_array"` so even length-0 or length-1 vectors serialize as `[...]` in `sl_to_json`.
  - `sl_fmt_num(x, digits = 4L, lang = "id")` -> `character` (`"12,3450"` for `"id"`, `"12.3450"` for `"en"`, `"—"` for `NA`/`NaN`/`Inf`).
  - `sl_fmt_p(p, lang = "id")` -> `character` (`"< 0,001"` / `"0,032"` for `"id"`, `"< 0.001"` / `"0.032"` for `"en"`, 3 decimals when `>= 0.001`).

- [ ] **Step 1: Initialize git repository and scaffold `.gitignore`, `.nojekyll`, and `tests/run_r_tests.R`**

Run `git init` in `C:\Users\Ariel's Device\.gemini\antigravity\scratch\statlab`. Create empty `.nojekyll`, `.gitignore` (ignoring `node_modules/`, `.Rhistory`, `.RData`, `*.tmp`), and `tests/run_r_tests.R` which sources all `R/*.R` in sorted order, defines assertion helpers `assert_true(cond, msg)` and `assert_near(actual, expected, tol = 1e-8, msg)`, executes all `tests/test_*.R` files, prints pass/fail counts, and exits with status `1` on any failure.

- [ ] **Step 2: Write the failing test `tests/test_00_json.R` (including Review Focus #5)**

```r
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
```

- [ ] **Step 3: Run desktop R test runner to verify it fails**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: FAIL with `could not find function "sl_to_json"`

- [ ] **Step 4: Implement `sl_box_vec`, `sl_to_json`, `sl_fmt_num`, and `sl_fmt_p` in `R/00_json.R`**

Use pure base R (`formatC(..., format = "f")`, `gsub`, `sprintf(..., "%.15g")` for JSON numbers so double precision `1e-8` parity holds, and recursive list/data.frame serialization without `jsonlite`). Note: `jsonlite` is used ONLY inside `tests/test_00_json.R` on desktop R to verify valid JSON output.

- [ ] **Step 5: Run desktop R test runner to verify it passes**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: PASS (`0 failures`)

- [ ] **Step 6: Commit**

```bash
git add .gitignore .nojekyll R/00_json.R tests/run_r_tests.R tests/test_00_json.R
git commit -m "feat(r-core): add base-R JSON serializer, bilingual formatters, and test runner"
```

---

### Task 2: Data Preparation, Wide/Long Reshaping & Outlier Detection (`1.5×IQR` + Grubbs)

**Files:**
- Create: `R/01_data_prep.R`
- Test: `tests/test_01_data_prep.R`

**Interfaces:**
- Consumes: `sl_box_vec` from `R/00_json.R`
- Produces:
  - `sl_prepare_groups(df, mode = c("wide", "long"), response_cols, group_col = NULL, exclude_rows = integer(0))` -> returns list:
    - `values`: `numeric` clean vector
    - `groups`: `factor` with levels in original appearance order
    - `source_rows`: `integer` 1-based original row index in `df`
    - `n_raw_obs`: `integer(1)` total candidate observations (in wide mode: non-trailing-blank cells + internal NAs; see behavior below)
    - `n_valid`: `integer(1)`
    - `n_missing`: `integer(1)`
    - `coerced_non_numeric`: `list(count = integer(1), rows = integer(), cols = character())`
    - `excluded_outliers`: `data.frame(source_row, group, value)` for user-checked `exclude_rows`
  - `sl_detect_outliers(values, groups = NULL, source_rows = seq_along(values), alpha = 0.05)` -> returns list:
    - `per_group`: list per group with `iqr_bounds = c(low, high)`, `grubbs = list(valid, G, p_value, suspect_value, suspect_row, is_outlier, note_key)`
    - `flagged`: `data.frame(source_row, group, value, by_iqr, by_grubbs)` (never deletes values!)

- [ ] **Step 1: Ensure desktop-only CRAN oracle packages `outliers` and `nortest` are installed in R 4.5.2 user library (if missing) for test suite validation**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" -e "pkgs <- c('outliers','nortest'); miss <- setdiff(pkgs, rownames(installed.packages())); if (length(miss)) install.packages(miss, repos='https://cloud.r-project.org')"`
Expected: `outliers` and `nortest` available on desktop R 4.5.2.

- [ ] **Step 2: Write the failing test `tests/test_01_data_prep.R` (including Review Focus #2, #3, #4 and Grubbs 1969 reference)**

```r
# tests/test_01_data_prep.R
# 1. Grubbs (1969) mass-spectrometer dataset vs outliers::grubbs.test to 1e-8
g_data <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)
out_res <- sl_detect_outliers(g_data, groups = factor(rep("G1", 8)), source_rows = 1:8, alpha = 0.05)
ref_g <- outliers::grubbs.test(g_data, type = 10, two.sided = TRUE)
assert_near(out_res$per_group[["G1"]]$grubbs$G, unname(ref_g$statistic["G"]), 1e-8, "Grubbs G vs CRAN outliers")
assert_near(out_res$per_group[["G1"]]$grubbs$p_value, ref_g$p.value, 1e-8, "Grubbs p vs CRAN outliers")
assert_true(out_res$per_group[["G1"]]$grubbs$is_outlier, "245.57 flagged by Grubbs")
assert_true(8L %in% out_res$flagged$source_row && any(out_res$flagged$by_iqr), "245.57 flagged by IQR too")

# 2. Review Focus #4: Jagged wide columns (Treatment A has 3 obs + 2 blanks, B has 5 obs)
df_wide <- data.frame(
  Pakan_A = c("10.2", "11.0", "10.8", "", ""),
  Pakan_B = c("14.1", "14.5", "13.9", "14.2", "14.8"),
  stringsAsFactors = FALSE
)
prep_w <- sl_prepare_groups(df_wide, mode = "wide", response_cols = c("Pakan_A", "Pakan_B"))
assert_true(prep_w$n_valid == 8L, "Jagged wide keeps all 3 + 5 = 8 valid replicates")
assert_true(sum(prep_w$groups == "Pakan_B") == 5L, "Pakan_B retains rows 4 and 5")

# 3. Review Focus #3: Dirty missing tokens ("NA", "-") vs unexpected text ("mati") in long mode
df_long <- data.frame(
  Perlakuan = c("K", "K", "K", "K", "P1", "P1", "P1"),
  Bobot = c("12.5", "NA", "-", "mati", "15.0", "15.4", "14.9"),
  stringsAsFactors = FALSE
)
prep_l <- sl_prepare_groups(df_long, mode = "long", response_cols = "Bobot", group_col = "Perlakuan")
assert_true(prep_l$n_valid == 4L, "4 numeric obs kept")
assert_true(prep_l$n_missing == 3L, "2 missing + 1 coerced counted in missing")
assert_true(identical(prep_l$coerced_non_numeric$rows, 4L), "Row 4 ('mati') flagged as unexpected text")

# 4. Explicit opt-in outlier exclusion (never silent)
prep_ex <- sl_prepare_groups(
  data.frame(Perlakuan = rep("G1", 8), Y = g_data),
  mode = "long", response_cols = "Y", group_col = "Perlakuan", exclude_rows = 8L
)
assert_true(prep_ex$n_valid == 7L && nrow(prep_ex$excluded_outliers) == 1L &&
            prep_ex$excluded_outliers$source_row[1] == 8L, "Explicit row 8 exclusion recorded")

# 5. Review Focus #2: Constant group (sd == 0) in outlier detection does not crash
const_out <- sl_detect_outliers(c(100, 100, 100, 100), groups = factor(rep("S100", 4)))
assert_true(identical(const_out$per_group[["S100"]]$grubbs$valid, FALSE), "Constant group Grubbs graceful")
assert_true(nrow(const_out$flagged) == 0L, "Constant group flags 0 outliers")
```

- [ ] **Step 3: Run desktop R test runner to verify it fails**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: FAIL with `could not find function "sl_detect_outliers"`

- [ ] **Step 4: Implement `sl_prepare_groups` and `sl_detect_outliers` in `R/01_data_prep.R`**

For two-sided Grubbs (`type = 10` matching `outliers::grubbs.test(..., type = 10, two.sided = TRUE)`): with $n \ge 3$ and $s > 0$, $G = \max_i |x_i - \bar{x}| / s$. Compute $s_{sq} = (n - 1) / \sqrt{n}$, if $G \ge s_{sq}$ then $p = 0$, else $t = G \sqrt{n (n - 2) / ((n - 1)^2 - n G^2)}$ and one-tailed upper bound $q = n \cdot \text{pt}(t, df = n - 2, \text{lower.tail} = \text{FALSE})$, two-sided $p = \min(1, 2 \cdot \min(q, 1 - q))$ — check exact `outliers::pgrubbs` two-sided transformation so `p_value` matches `outliers::grubbs.test` to `<= 1e-8` across both small and large $G$.

- [ ] **Step 5: Run desktop R test runner to verify it passes**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: PASS (`0 failures`)

- [ ] **Step 6: Commit**

```bash
git add R/01_data_prep.R tests/test_01_data_prep.R
git commit -m "feat(r-core): add wide/long group preparation and IQR/Grubbs outlier detection"
```

---

### Task 3: Assumption Testing Engine (Shapiro-Wilk, Lilliefors, Brown-Forsythe Levene, Bartlett)

**Files:**
- Create: `R/02_assumptions.R`
- Test: `tests/test_02_assumptions.R`

**Interfaces:**
- Consumes: `R/00_json.R`
- Produces:
  - `sl_shapiro_test(x)` -> `list(valid, n, statistic, p_value, note_key)`
  - `sl_lilliefors_test(x)` -> `list(valid, n, statistic, p_value, note_key)` (matches `nortest::lillie.test` to `1e-8`)
  - `sl_levene_test(values, groups, center = "median")` -> `list(valid, center, df1, df2, statistic, p_value, note_key)` (matches `car::leveneTest` to `1e-8`)
  - `sl_bartlett_test(values, groups)` -> `list(valid, df, statistic, p_value, note_key)`
  - `sl_check_assumptions(values, groups = NULL, residuals = NULL, alpha = 0.05, require_homogeneity = TRUE)` -> returns list:
    - `alpha`: `numeric(1)`
    - `per_group`: list of per-group `shapiro` + `lilliefors` + `is_normal`
    - `residual_normality`: `NULL` or `list(shapiro, lilliefors, is_normal)`
    - `homogeneity`: `NULL` or `list(levene_bf, bartlett, is_homogeneous)`
    - `normality_rule`: `"residuals"` when `residuals` supplied else `"per_group"`
    - `normality_violated`: `logical(1)` (Spec §6.2 exact rule)
    - `homogeneity_violated`: `logical(1)` (Spec §6.2 exact rule: Brown-Forsythe `p_value < alpha`)

- [ ] **Step 1: Write the failing test `tests/test_02_assumptions.R` (oracle checks vs `car`, `nortest`, `stats`, plus Review Focus #2 and Spec §6.2 violation rule)**

```r
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
set.seed(42)
g_skew <- c(rexp(12, 1), rnorm(12, 5, 1))
grp2   <- factor(rep(c("A", "B"), each = 12))
chk_no_res <- sl_check_assumptions(g_skew, grp2, residuals = NULL, alpha = 0.05)
assert_true(chk_no_res$normality_rule == "per_group" && chk_no_res$normality_violated, "Per-group violation detected")

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
```

- [ ] **Step 2: Run desktop R test runner to verify it fails**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: FAIL with `could not find function "sl_lilliefors_test"`

- [ ] **Step 3: Implement `R/02_assumptions.R`**

Match `nortest::lillie.test` exact Dallal-Wilkinson + Stephens upper-tail (`p > 0.1`) piecewise approximation so `p_value` matches `nortest::lillie.test` to `<= 1e-8` across all $p \in (0, 1]$ and $n \ge 5$ (including `n > 100`). For `sl_levene_test`, compute group centers (`median` or `mean`), absolute deviations $z_{ij} = |y_{ij} - c_i|$, and one-way ANOVA $F = \text{MS}_{\text{between}} / \text{MS}_{\text{within}}$ with `pf(F, k - 1, N - k, lower.tail = FALSE)`.

- [ ] **Step 4: Run desktop R test runner to verify it passes**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: PASS (`0 failures`)

- [ ] **Step 5: Commit**

```bash
git add R/02_assumptions.R tests/test_02_assumptions.R
git commit -m "feat(r-core): add Shapiro-Wilk, Lilliefors, Brown-Forsythe Levene, and Bartlett assumptions"
```

---

### Task 4: Base Graphics Plot Capture, Descriptive Analysis, Bilingual Conclusions & Dispatcher

**Files:**
- Create: `R/03_plots_base.R`
- Create: `R/04_descriptive.R`
- Create: `R/05_conclusions.R`
- Create: `R/99_dispatcher.R`
- Test: `tests/test_04_descriptive.R`

**Interfaces:**
- Consumes: `R/00_json.R`, `R/01_data_prep.R`, `R/02_assumptions.R`
- Produces:
  - `sl_base64_encode(raw_vec)` -> `character(1)` pure base-R RFC-4648 base64 encoder (zero packages!).
  - `sl_capture_plot(plot_fn, width_in = 6.5, height_in = 4.2, res = 300)` -> `list(png_data_url = "data:image/png;base64,...", width_in = width_in, height_in = height_in)` using `grDevices::png(tmp, width = round(width_in * res), height = round(height_in * res), res = res)` (with fallback to `bitmap`/`svg` if needed).
  - `sl_plot_qq(x, title, lang = "id", outlier_idx = integer(0))`
  - `sl_plot_hist_density(x, title, xlab, lang = "id")`
  - `sl_plot_box_jitter(values, groups, title, ylab, lang = "id", outlier_rows = integer(0), source_rows = seq_along(values))`
  - `sl_analyze_descriptive(df, opts)` -> full StatLab result contract list:
    - `meta`: `list(analysis_type = "descriptive", analysis_label = list(id=..., en=...), alpha, n_raw, n_valid, n_missing, n_excluded_outliers, group_names = sl_box_vec(...), timestamp)`
    - `assumptions`: output of `sl_check_assumptions`
    - `outliers`: output of `sl_detect_outliers`
    - `main`: list of per-group numeric summaries (`n`, `n_missing`, `mean`, `sd`, `se`, `median`, `q1`, `q3`, `iqr`, `min`, `max`, `cv_pct`, `skewness`, `kurtosis`, `ci_low`, `ci_high`)
    - `posthoc`: `NULL`, `effect_size`: `NULL`, `alternatives`: `sl_box_vec(list())`
    - `tables_apa`: list of `{ id, title: {id, en}, headers: {id, en}, rows: {id, en}, note: {id, en} }`
    - `tables_full`: list of SPSS-style full tables (`descriptives_full`, `normality_full`, `homogeneity_full`, `outliers_full`)
    - `formulas`: list of `{ label: {id, en}, latex: "..." }` (e.g., $\bar{x} \pm t_{1-\alpha/2,\,n-1}\frac{s}{\sqrt{n}}$, Shapiro-Wilk $W$, Brown-Forsythe $F_{\text{BF}}$)
    - `plots`: list of `{ id, title: {id, en}, png_data_url, width_in, height_in }`
    - `interpretation`: `list(id = "...", en = "...")`
    - `warnings`: `list(id = sl_box_vec(...), en = sl_box_vec(...))`
  - `sl_compile_general_conclusion(items, lang = c("id", "en"))` -> `list(id = "...", en = "...")`
  - `sl_dispatch(action, payload)` -> `character(1)` JSON string via `sl_to_json` (`action` in `"healthcheck"`, `"run_analysis"`, `"general_conclusion"`).

- [ ] **Step 1: Write the failing test `tests/test_04_descriptive.R` (verifying `iris` exact stats, PNG base64 capture, bilingual comma/dot conclusions, `sl_compile_general_conclusion`, and Review Focus #5 zero-mean CV%)**

```r
# tests/test_04_descriptive.R
data(iris)
res_json <- sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = iris[, c("Sepal.Length", "Species")],
  roles = list(mode = "long", response_cols = "Sepal.Length", group_col = "Species"),
  opts = list(alpha = 0.05, exclude_rows = integer(0), include_plots = TRUE)
))
res <- jsonlite::fromJSON(res_json, simplifyVector = FALSE)

# Check setosa exact descriptive numbers against base R
setosa <- iris$Sepal.Length[iris$Species == "setosa"]
g_set <- res$main$groups[[1]]
assert_true(identical(g_set$group, "setosa") && identical(g_set$n, 50L), "setosa n=50")
assert_near(g_set$mean, mean(setosa), 1e-8, "setosa mean")
assert_near(g_set$sd, sd(setosa), 1e-8, "setosa sd")
assert_near(g_set$se, sd(setosa) / sqrt(50), 1e-8, "setosa se")
assert_near(g_set$median, median(setosa), 1e-8, "setosa median")
assert_near(g_set$iqr, IQR(setosa), 1e-8, "setosa IQR")
ci_ref <- t.test(setosa, conf.level = 0.95)$conf.int
assert_near(g_set$ci_low, ci_ref[1], 1e-8, "setosa 95% CI low")
assert_near(g_set$ci_high, ci_ref[2], 1e-8, "setosa 95% CI high")

# Check plots are non-empty PNG data URLs
assert_true(length(res$plots) >= 2L, "At least Q-Q and Boxplot generated")
assert_true(startsWith(res$plots[[1]]$png_data_url, "data:image/png;base64,iVBOR"), "Valid PNG header in base64")

# Check bilingual prose & Indonesian decimal comma vs English dot
assert_true(grepl("setosa", res$interpretation$id) && grepl(",", res$interpretation$id), "ID prose uses group name + comma")
assert_true(grepl("setosa", res$interpretation$en) && grepl("\\.", res$interpretation$en), "EN prose uses group name + dot")

# Check General Conclusion compilation
gc_json <- sl_dispatch("general_conclusion", list(
  items = list(list(
    title = "Bobot Ikan",
    analysis_type = "descriptive",
    chosen_test = "Statistik Deskriptif",
    effective_test = "Statistik Deskriptif",
    assumption_violated = res$assumptions$normality_violated || res$assumptions$homogeneity_violated,
    summary_id = res$interpretation$id,
    summary_en = res$interpretation$en
  ))
))
gc <- jsonlite::fromJSON(gc_json, simplifyVector = FALSE)
assert_true(nzchar(gc$id) && nzchar(gc$en), "General conclusion bilingual output")

# Review Focus #5: Zero-mean vector produces null cv_pct in JSON without crashing
df_zero <- data.frame(G = rep("Anomali", 5), X = c(-2, -1, 0, 1, 2))
z_res <- jsonlite::fromJSON(sl_dispatch("run_analysis", list(
  analysis_type = "descriptive",
  data = df_zero,
  roles = list(mode = "long", response_cols = "X", group_col = "G"),
  opts = list(alpha = 0.05, include_plots = FALSE)
)), simplifyVector = FALSE)
assert_true(is.null(z_res$main$groups[[1]]$cv_pct), "Zero-mean CV% serializes cleanly to null")
```

- [ ] **Step 2: Run desktop R test runner to verify it fails**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: FAIL with `could not find function "sl_dispatch"`

- [ ] **Step 3: Implement `R/03_plots_base.R`, `R/04_descriptive.R`, `R/05_conclusions.R`, and `R/99_dispatcher.R`**

Implementation requirements:
- `sl_base64_encode(raw_vec)`: vectorized bitwise base64 encoder in pure base R (`rawToBits` / integer arithmetic over 3-byte blocks -> 64-character lookup table, `=` padding) — runs in `< 15 ms` for a 60 KB PNG and needs zero external packages.
- Base graphics styling in `R/03_plots_base.R`: Lab Notebook aesthetic (`bg = "#FFFDF8"`, ink `#1B365D` for data/curves, `#B83227` crimson for flagged outliers and reference lines, clean `las = 1`, `bty = "l"`). Always `on.exit(grDevices::dev.off(), add = TRUE)` before reading bytes so device locks never leak on error.
- Skewness & excess kurtosis in `R/04_descriptive.R`: adjusted Fisher-Pearson sample skewness $G_1 = \frac{\sqrt{n(n-1)}}{n-2} \frac{m_3}{m_2^{3/2}}$ (`n >= 3`) and sample excess kurtosis $G_2 = \frac{n-1}{(n-2)(n-3)} ((n+1)\frac{m_4}{m_2^2} - 3(n-1))$ (`n >= 4`), matching SPSS/Excel.
- `R/05_conclusions.R`: reports Shapiro-Wilk $W$ and $p$ per group, Levene Brown-Forsythe $F(df_1, df_2)$ and $p$ when $k \ge 2$, states whether parametric summary ($\text{Mean} \pm \text{SD}$) or non-parametric summary ($\text{Median [IQR]}$) is primary per group normality, reports any flagged/excluded outliers, and compiles numbered synthesis in `sl_compile_general_conclusion`.

- [ ] **Step 4: Run desktop R test runner to verify it passes**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
Expected: PASS (`0 failures`)

- [ ] **Step 5: Commit**

```bash
git add R/03_plots_base.R R/04_descriptive.R R/05_conclusions.R R/99_dispatcher.R tests/test_04_descriptive.R
git commit -m "feat(r-core): add base-graphics plots, descriptive engine, bilingual conclusions, and dispatcher"
```

---

### Task 5: Local Vendor Assets, Locale Number/CSV Parser & webR-vs-Desktop Parity Suite

**Files:**
- Create: `tools/vendor-fetch.mjs`
- Create: `js/i18n.js`
- Create: `js/number-locale.js`
- Create: `js/webr-client.js`
- Test: `tests/test_webr_parity.mjs`

**Interfaces:**
- Consumes: `R/*.R` via `sl_dispatch`
- Produces:
  - `vendor/webr/*` (`webr@0.5.4` dist files), `vendor/katex/*` (`katex@0.19.1` dist CSS + JS + woff2 fonts), `vendor/pdfmake/*` (`pdfmake@0.3.13` build `pdfmake.min.js` + `vfs_fonts.js`).
  - `js/i18n.js`: `export const SUPPORTED_LANGS = ["id", "en"]; export function t(key, lang = "id", vars = {})`
  - `js/number-locale.js`:
    - `export function detectLocaleFormat(rawText)` -> `{ delimiter: ";" | "," | "\t", decimalMark: "," | ".", thousandsMark: "." | "," | "", ambiguous: boolean, sampleNote: string }`
    - `export function parseCellValue(raw, mode = "auto", detected = null)` -> `{ type: "number" | "missing" | "text", value: number | null | string, raw: string }`
    - `export function parseDelimitedText(rawText, mode = "auto")` -> `{ headers: string[], rows: string[][], locale: object }`
    - `export function serializeTableDelimited(headers, rows, lang = "id", target = "csv")` -> `string`
  - `js/webr-client.js`:
    - `export class StatLabEngine` with `constructor(options = {})`, `async init(onProgress)`, `async runAnalysis(analysisType, tableData, roles, opts)`, `async compileGeneralConclusion(items, lang)`, `async close()`.

- [ ] **Step 1: Create and run `tools/vendor-fetch.mjs` to populate `vendor/` from npm registry tarballs without adding a frontend build step**

Write `tools/vendor-fetch.mjs` using Node built-in `https`/`zlib`/tar extraction (or `npm pack` into a temp folder + `tar -xzf`) to extract:
- `webr@0.5.4` `package/dist/*` -> `vendor/webr/`
- `katex@0.19.1` `package/dist/{katex.min.js,katex.min.css,contrib/auto-render.min.js,fonts/*.woff2}` -> `vendor/katex/`
- `pdfmake@0.3.13` `package/build/{pdfmake.min.js,vfs_fonts.js}` -> `vendor/pdfmake/`
Also assert in `tools/vendor-fetch.mjs` that every extracted file in `vendor/` is `< 50 * 1024 * 1024` bytes (safe for GitHub Pages) and verify whether `survival`, `cluster`, `MASS` are already pre-bundled inside `webr@0.5.4`'s default R library (if any of the three is not in default VFS `/usr/lib/R/library`, fetch its matching `.tgz`/`.data` binary from `https://repo.r-wasm.org` into `vendor/r-repo/`).
Run: `node tools/vendor-fetch.mjs`
Expected: `vendor/webr/webr.mjs`, `vendor/katex/katex.min.js`, and `vendor/pdfmake/pdfmake.min.js` exist and all files are `< 50 MB`.

- [ ] **Step 2: Write the failing Node test `tests/test_webr_parity.mjs` (covering Review Focus #1 & #3 and Spec §9.3 webR vs desktop R 4.5.2 parity to `<= 1e-8`)**

```javascript
// tests/test_webr_parity.mjs
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { detectLocaleFormat, parseCellValue, parseDelimitedText, serializeTableDelimited } from "../js/number-locale.js";
import { StatLabEngine } from "../js/webr-client.js";

// 1. Indonesian Excel CSV (';' delimiter + ',' decimal)
const idCsv = "Perlakuan;Bobot_g;SR_persen\nKontrol;12,45;95,0\nPakan_A;15,80;98,5\nPakan_A;1.234,56;100\n";
const parsedId = parseDelimitedText(idCsv, "auto");
assert.equal(parsedId.locale.delimiter, ";");
assert.equal(parsedId.locale.decimalMark, ",");
assert.equal(parseCellValue("12,45", "auto", parsedId.locale).value, 12.45);
assert.equal(parseCellValue("1.234,56", "auto", parsedId.locale).value, 1234.56);

// 2. Review Focus #1: Ambiguous 3-trailing-digit "1.234" without comma clues
const ambig = detectLocaleFormat("Bobot\n1.234\n2.345\n");
assert.equal(ambig.ambiguous, true, "3-digit dot pattern flagged ambiguous");
assert.equal(parseCellValue("1.234", "auto", ambig).value, 1.234, "Defaults safely to decimal 1.234 unless overridden");

// 3. Review Focus #3: Dirty cells ("12,5 ", "NA", "-", "")
assert.equal(parseCellValue(" 12,5 ", "id_comma").value, 12.5);
assert.equal(parseCellValue("NA", "id_comma").type, "missing");
assert.equal(parseCellValue("-", "id_comma").type, "missing");
assert.equal(parseCellValue("mati", "id_comma").type, "text");

// 4. Export formatting follows language (ID: ';' and ',', EN: ',' and '.')
assert.equal(serializeTableDelimited(["G", "Mean"], [["K", "12,45"]], "id", "csv").trim(), "G;Mean\nK;12,45");

// 5. Spec §9.3: webR vs Desktop R 4.5.2 parity <= 1e-8 on aquaculture dataset
import { fileURLToPath } from "node:url";
const vendorWebrDir = fileURLToPath(new URL("../vendor/webr/", import.meta.url));
const engine = new StatLabEngine({ baseUrl: vendorWebrDir });
await engine.init();

const sampleTable = {
  Kontrol: [45.2, 42.8, 46.1, 44.0, 43.5, 45.9, 44.8, 42.1, 46.5, 43.9],
  Artemia: [55.4, 58.1, 54.9, 56.2, 59.0, 53.5, 57.3, 55.0, 58.6, 56.8]
};
const webrRes = await engine.runAnalysis(
  "descriptive",
  sampleTable,
  { mode: "wide", response_cols: ["Kontrol", "Artemia"], group_col: null },
  { alpha: 0.05, exclude_rows: [], include_plots: true }
);
await engine.close();

// Run exact same payload in Desktop R 4.5.2
const rCmd = `
  for (f in sort(list.files("R", pattern="\\\\.R$", full.names=TRUE))) source(f, encoding="UTF-8")
  df <- data.frame(
    Kontrol = c(45.2, 42.8, 46.1, 44.0, 43.5, 45.9, 44.8, 42.1, 46.5, 43.9),
    Artemia = c(55.4, 58.1, 54.9, 56.2, 59.0, 53.5, 57.3, 55.0, 58.6, 56.8)
  )
  cat(sl_dispatch("run_analysis", list(
    analysis_type = "descriptive",
    data = df,
    roles = list(mode = "wide", response_cols = c("Kontrol", "Artemia")),
    opts = list(alpha = 0.05, exclude_rows = integer(0), include_plots = FALSE)
  )))
`;
const desktopJson = execFileSync("C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe", ["-e", rCmd], { encoding: "utf8" });
const deskRes = JSON.parse(desktopJson);

for (let i = 0; i < 2; i++) {
  for (const k of ["mean", "sd", "se", "median", "iqr", "cv_pct", "skewness", "kurtosis", "ci_low", "ci_high"]) {
    assert.ok(Math.abs(webrRes.main.groups[i][k] - deskRes.main.groups[i][k]) <= 1e-8, `Parity failed on group ${i} ${k}`);
  }
  assert.ok(Math.abs(webrRes.assumptions.per_group[i].shapiro.p_value - deskRes.assumptions.per_group[i].shapiro.p_value) <= 1e-8);
  assert.ok(Math.abs(webrRes.assumptions.per_group[i].lilliefors.p_value - deskRes.assumptions.per_group[i].lilliefors.p_value) <= 1e-8);
}
assert.ok(Math.abs(webrRes.assumptions.homogeneity.levene_bf.p_value - deskRes.assumptions.homogeneity.levene_bf.p_value) <= 1e-8);
assert.ok(webrRes.plots.length >= 2 && webrRes.plots[0].png_data_url.startsWith("data:image/png;base64,"));
console.log("PASS: webR vs Desktop R 4.5.2 parity <= 1e-8");
```

- [ ] **Step 3: Run Node parity test to verify it fails**

Run: `node tests/test_webr_parity.mjs`
Expected: FAIL with `Cannot find module '../js/number-locale.js'`

- [ ] **Step 4: Implement `js/i18n.js`, `js/number-locale.js`, and `js/webr-client.js`**

In `js/webr-client.js`, dynamically load `WebR` and `ChannelType` from `./vendor/webr/webr.mjs`, initialize with `channelType: ChannelType.PostMessage`, read `R/*.R` (via `fetch` in browser or `node:fs/promises` when running under Node.js for tests!), write them into webR VFS `/statlab/R/`, `source()` them in sorted order, verify `library(survival); library(cluster); library(MASS)` load cleanly, and serialize JS dataset columns into an R `data.frame` binding before calling `sl_dispatch(...)`. Check whether webR's built-in `grDevices::png()` or `webr::canvas(capture = TRUE)` is needed inside WASM so `sl_capture_plot` in `R/03_plots_base.R` produces valid PNG data URLs in BOTH desktop R and webR!

- [ ] **Step 5: Run both desktop R and Node webR parity tests to verify they pass**

Run: `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R ; node tests/test_webr_parity.mjs`
Expected: PASS (`0 failures` in R and `PASS: webR vs Desktop R 4.5.2 parity <= 1e-8` in Node)

- [ ] **Step 6: Commit**

```bash
git add tools/vendor-fetch.mjs vendor/ js/i18n.js js/number-locale.js js/webr-client.js R/03_plots_base.R tests/test_webr_parity.mjs
git commit -m "feat(bridge): vendor webR/KaTeX/pdfmake, add locale parser, and verify webR-desktop parity"
```

---

### Task 6: "Lab Notebook" Theme, Editable Spreadsheet Grid, Variable Role Panel & Result View

**Files:**
- Create: `css/statlab.css`
- Create: `js/grid.js`
- Create: `js/roles.js`
- Create: `js/result-view.js`
- Test: `tests/test_ui_modules.mjs`

**Interfaces:**
- Consumes: `js/i18n.js`, `js/number-locale.js`, `vendor/katex/katex.min.js`
- Produces:
  - `css/statlab.css`: `.sl-app` scoped theme variables (`--sl-paper: #FBF8F1`, `--sl-surface: #FFFDF8`, `--sl-ink: #1B365D`, `--sl-crimson: #B83227`, `--sl-grid-line: rgba(27, 54, 93, 0.055)`), millimeter paper background, margin rule, `.sl-grid-table`, `.sl-apa-table`, `.sl-spss-details`, `.sl-formula-box`, `.sl-outlier-badge`, `.sl-session-drawer`.
  - `js/grid.js`: `export class StatLabGrid`
    - `constructor(containerEl, options)` (`onChange`, `lang`, `decimalMode`)
    - `setData(headers, rows)`, `getData()` -> `{ headers, rows }`
    - `loadDelimitedText(text, decimalMode)` -> loads CSV/TSV and returns detected locale info
    - `getTypedColumns()` -> converts raw cells via `parseCellValue` into `{ columns: Record<string, (number|string|null)[]>, locale }`
    - `setOutlierFlags(flaggedRows)` -> marks flagged cells/rows with crimson badge; `getExcludedRows()` -> `number[]` (1-based row indices explicitly checked by user, empty by default!).
  - `js/roles.js`: `export const ANALYSIS_SCHEMAS`, `export class RoleMappingPanel`
    - `getMapping()` -> `{ mode: "wide" | "long", response_cols: string[], group_col: string | null }` (with automatic smart default detection: if column 1 is categorical text and column 2 is numeric -> defaults to `"long"`; if `>= 2` numeric columns -> defaults to `"wide"`).
  - `js/result-view.js`: `export function renderAnalysisResult(containerEl, result, options)`
    - Renders in strict order: (1) Meta & KaTeX Formula cards, (2) Assumption Checks banner & diagnostics (Normality + Homogeneity + Q-Q plot preview), (3) Outlier Alert & opt-in exclusion checkboxes + Re-run button (when outliers exist), (4) Layer 1 APA Journal Table with Copy-to-Word/Excel & Export-CSV buttons, (5) Collapsible `<details>` Layer 2 SPSS Full Tables with Copy/CSV buttons, (6) R Base Graphics Plot Gallery with 300-DPI PNG download, (7) Bilingual Written Interpretation + "Add to Session Report" button.

- [ ] **Step 1: Write the failing test `tests/test_ui_modules.mjs`**

Test state & serialization logic of `StatLabGrid` (Excel TSV paste expansion, outlier flag preservation without auto-exclusion, explicit opt-in `toggleExcludeRow`, Indonesian comma `getTypedColumns`), `RoleMappingPanel` (smart wide vs long auto-detection and validation), and CSS isolation check (asserting `css/statlab.css` contains `.sl-app`, `#FBF8F1`, `#1B365D`, `#B83227`, and zero unprefixed global `* {` or `body {` selectors outside `.sl-app` / `body.sl-standalone`).

- [ ] **Step 2: Run `node tests/test_ui_modules.mjs` to verify it fails**

Run: `node tests/test_ui_modules.mjs`
Expected: FAIL with `Cannot find module '../js/grid.js'`

- [ ] **Step 3: Implement `css/statlab.css`, `js/grid.js`, `js/roles.js`, and `js/result-view.js`**

Ensure `StatLabGrid` supports keyboard navigation (`Enter`, `Tab`, arrows), multi-cell paste from Excel (`\t` & `\r?\n`), editable headers, row/column selection and deletion, and non-blocking toast notifications (never browser `alert()`). Ensure `renderAnalysisResult` escapes all user column/group names against XSS while rendering KaTeX math safely via `katex.render(latex, el, { throwOnError: false, displayMode: true })`.

- [ ] **Step 4: Run `node tests/test_ui_modules.mjs` to verify it passes**

Run: `node tests/test_ui_modules.mjs`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add css/statlab.css js/grid.js js/roles.js js/result-view.js tests/test_ui_modules.mjs
git commit -m "feat(ui): add Lab Notebook CSS, editable grid, role mapping panel, and result view"
```

---

### Task 7: Cumulative Session Report, Persistence, `pdfmake` Vector PDF Export & PWA Shell

**Files:**
- Create: `js/session-store.js`
- Create: `js/pdf-export.js`
- Create: `js/app.js`
- Create: `index.html`
- Create: `manifest.json`
- Create: `sw.js`
- Test: `tests/test_pdf_and_session.mjs`

**Interfaces:**
- Consumes: `js/i18n.js`, `js/number-locale.js`, `js/webr-client.js`, `js/grid.js`, `js/roles.js`, `js/result-view.js`, `vendor/pdfmake/*`
- Produces:
  - `js/session-store.js`: `export class SessionStore`
    - `addReportItem(analysisResult, customTitle)` -> appends snapshot with unique `id`
    - `moveReportItem(id, direction)` -> reorders `-1` (up) or `+1` (down)
    - `removeReportItem(id)` -> removes item
    - `setGeneralConclusion(bilingualObj)` -> updates `{ id, en }` returned by R `sl_compile_general_conclusion`
    - `exportProjectJson()` -> `.statlab.json` string (`schema_version: 1`, `projectTitle`, `lang`, `alpha`, `decimalMode`, `grid`, `reportItems`, `generalConclusion`)
    - `importProjectJson(jsonStr)` -> validates and restores full state
    - `saveToStorage()` / `loadFromStorage()` -> IndexedDB `statlab_db` with automatic fallback to `localStorage` / memory in Node tests.
  - `js/pdf-export.js`:
    - `export function buildSessionPdfDocDefinition(sessionState, options = { lang: "id", includeAppendix: false })` -> valid `pdfmake` A4 `docDefinition` object containing: cover header (Lab Notebook title, date, α, R version), numbered sections per `reportItem` (Method & Assumptions summary, APA horizontal-rule tables via `hLineWidth`/`vLineWidth: () => 0`, optional Layer-2 SPSS tables, embedded 300-DPI R `png_data_url` images, written interpretation paragraph), and final **"Kesimpulan Umum" / "General Conclusion"** section.
    - `export function downloadSessionPdf(sessionState, options)` -> invokes `pdfMake.createPdf(docDef).download(filename)`.
  - `js/app.js`: `export async function mountStatLab(rootEl, config = {})` + Phase 1 preset datasets:
    1. `wq_ponds` — Kualitas Air Tambak Udang Vaname (Wide: DO, pH, Suhu, Amonia across 12 ponds — normal + skewed ammonia with outlier!).
    2. `tilapia_growth` — Bobot Akhir Ikan Nila 4 Perlakuan Pakan (Long: `Perlakuan`, `Bobot_g` — matches Aqua Tools feed trial data!).
    3. `shrimp_survival_100` — Sintasan Udang 100% pada Kontrol (Edge-case preset demonstrating constant-group handling + non-normal skew).

- [ ] **Step 1: Write the failing test `tests/test_pdf_and_session.mjs`**

```javascript
// tests/test_pdf_and_session.mjs
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";
import { SessionStore } from "../js/session-store.js";
import { buildSessionPdfDocDefinition } from "../js/pdf-export.js";
import { StatLabEngine } from "../js/webr-client.js";

const engine = new StatLabEngine({ baseUrl: fileURLToPath(new URL("../vendor/webr/", import.meta.url)) });
await engine.init();

const res1 = await engine.runAnalysis(
  "descriptive",
  { Kontrol: [45.2, 42.8, 46.1, 44.0, 43.5], Artemia: [55.4, 58.1, 54.9, 56.2, 59.0] },
  { mode: "wide", response_cols: ["Kontrol", "Artemia"], group_col: null },
  { alpha: 0.05, exclude_rows: [], include_plots: true }
);
const res2 = await engine.runAnalysis(
  "descriptive",
  { DO_mgL: [5.2, 5.5, 5.1, 5.4, 5.3, 2.1] },
  { mode: "wide", response_cols: ["DO_mgL"], group_col: null },
  { alpha: 0.05, exclude_rows: [], include_plots: true }
);

const store = new SessionStore({ projectTitle: "Penelitian Uji Pakan Nila", lang: "id" });
const id1 = store.addReportItem(res1, "Analisis 1: Bobot Ikan");
const id2 = store.addReportItem(res2, "Analisis 2: Oksigen Terlarut");
assert.equal(store.getState().reportItems.length, 2);

// Reorder: move item 2 up to position 0
store.moveReportItem(id2, -1);
assert.equal(store.getState().reportItems[0].id, id2);

// Compile cumulative General Conclusion in R
const gc = await engine.compileGeneralConclusion(store.getGeneralConclusionPayload(), "id");
store.setGeneralConclusion(gc);
await engine.close();

// Roundtrip .statlab.json export/import
const dumped = store.exportProjectJson();
const store2 = new SessionStore();
store2.importProjectJson(dumped);
assert.equal(store2.getState().reportItems.length, 2);
assert.equal(store2.getState().generalConclusion.id, gc.id);

// Build pdfmake docDefinition in ID and EN and verify structure & images
const docId = buildSessionPdfDocDefinition(store2.getState(), { lang: "id", includeAppendix: true });
const flatJson = JSON.stringify(docId);
assert.ok(flatJson.includes("Penelitian Uji Pakan Nila"), "Project title in PDF");
assert.ok(flatJson.includes("Kesimpulan Umum"), "General Conclusion heading in ID PDF");
assert.ok(flatJson.includes("data:image/png;base64,"), "300-DPI R plot embedded in PDF");

const docEn = buildSessionPdfDocDefinition(store2.getState(), { lang: "en", includeAppendix: false });
assert.ok(JSON.stringify(docEn).includes("General Conclusion"), "General Conclusion heading in EN PDF");
console.log("PASS: SessionStore, .statlab.json roundtrip, and pdfmake vector PDF builder verified");
```

- [ ] **Step 2: Run `node tests/test_pdf_and_session.mjs` to verify it fails**

Run: `node tests/test_pdf_and_session.mjs`
Expected: FAIL with `Cannot find module '../js/session-store.js'`

- [ ] **Step 3: Implement `js/session-store.js`, `js/pdf-export.js`, `js/app.js`, `index.html`, `manifest.json`, and `sw.js`**

Ensure `index.html` works when served from any subpath (e.g. `https://<user>.github.io/statlab/` or `/tools/statlab/` inside Aqua Tools) by using relative URLs (`./css/statlab.css`, `./js/app.js`, `./manifest.json`, `./sw.js`, `./vendor/...`, `./R/...`) — NEVER root-absolute `/js/...` paths which break GitHub Pages project sites! Generate clean SVG icons (`assets/icon-192.svg`, `assets/icon-512.svg`) with the Σ / Lab Notebook emblem referenced in `manifest.json`.

- [ ] **Step 4: Run the full automated verification suite (Desktop R + webR Parity + UI Modules + PDF/Session + Local HTTP Smoke Check)**

Run:
1. `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
2. `node tests/test_webr_parity.mjs`
3. `node tests/test_ui_modules.mjs`
4. `node tests/test_pdf_and_session.mjs`
Expected: All 4 test suites PASS with `0` errors.

- [ ] **Step 5: Commit**

```bash
git add assets/ index.html manifest.json sw.js js/session-store.js js/pdf-export.js js/app.js tests/test_pdf_and_session.mjs
git commit -m "feat(app): complete Phase 1 PWA shell, session report, .statlab.json, and pdfmake PDF export"
```
