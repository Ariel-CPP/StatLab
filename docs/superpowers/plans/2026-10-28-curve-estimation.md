# StatLab Curve Estimation & Non-Linear Regression Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement pure Base R Curve Estimation and Non-Linear Regression (Linear, Quadratic with vertex optimum, Power/Allometric with $b=3$ hypothesis test, Exponential, Logarithmic) with automated model selection, publication APA tables, 300-DPI Base graphics plot, and bilingual interpretations in StatLab.

**Architecture:** Pure Base-R statistical engine in `R/08_regression_chisq.R` routed through `R/99_dispatcher.R`, running identically in browser webR (WASM) and Desktop R 4.5.2, paired with vanilla JS UI templates and presets in `js/app.js` and `js/roles.js`.

**Tech Stack:** R (Base, stats, graphics, grDevices), WebAssembly (webR 0.5.4), Vanilla JavaScript (ES modules), CSS3 (Lab Notebook theme).

**Spec:** `docs/superpowers/specs/2026-10-28-curve-estimation-design.md`

## Global Constraints
- **Zero JS statistical math:** 100% of statistical computations, coefficients, AIC/BIC, $p$-values, and vertex calculations run in R (`R/*.R`).
- **Base R Only:** Allowed R packages in `R/*.R`: ONLY `base`, `stats`, `graphics`, `grDevices`, `utils`. Zero CRAN dependencies.
- **Numerical Parity:** webR vs Desktop R 4.5.2 tolerance $\le 10^{-8}$.
- **Zero SPSS Mentions:** User-facing tables must be labeled as APA Tables and Detailed Tables (never mention "SPSS").
- **Offline / PWA Integrity:** All assets local, relative paths, Service Worker cache updated.

## Review Focus
1. Non-positive values ($X \le 0$ or $Y \le 0$): models requiring log transforms (logarithmic, exponential, power) must be skipped gracefully with a clear bilingual warning, while linear and quadratic models succeed.
2. Degenerate / collinear cases (e.g. constant $X$ or $Y$, or $n < 4$ points): catch gracefully and return informative validation error rather than throwing an unhandled R error.
3. Quadratic vertex when $b_2 = 0$ (perfect linear): optimum is undefined / linear, avoid division by zero.
4. Allometric hypothesis test when $SE_b = 0$ or $n \le 2$: guard against division by zero.
5. Service Worker caching: bump cache to `statlab-cache-v5` so users immediately see updated scripts without stale browser caching.

---

### Task 1: Pure Base-R Curve Estimation Engine & Test Suite

**Files:**
- Modify: `R/08_regression_chisq.R`
- Modify: `R/99_dispatcher.R`
- Modify: `tests/test_07_inferential.R`

**Steps:**
- [ ] Write failing test cases in `tests/test_07_inferential.R` for `sl_analyze_curve_estimation`:
  - Fitting Linear, Quadratic, Logarithmic, Exponential, and Power models.
  - Verifying Quadratic vertex calculation ($X_{opt}, Y_{opt}$).
  - Verifying Allometric hypothesis test against $b=3$.
  - Verifying Model Selection table ($R^2$, Adj-$R^2$, AIC, BIC).
  - Verifying safe handling of $X \le 0$ or $Y \le 0$.
  - Verifying dispatcher routing via `sl_dispatch("run_analysis", list(analysis_type = "curve_estimation", ...))`.
- [ ] Run Desktop R test suite to observe RED failure:
  `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" tests/run_r_tests.R`
- [ ] Implement `sl_analyze_curve_estimation(df, opts)` in `R/08_regression_chisq.R`:
  - Data preparation and finite numeric validation for $X$ and $Y$.
  - Fitting all 5 candidate models with closed-form linear/linearized least squares.
  - Computing residuals, RSS, TSS, $R^2$, Adj-$R^2$, AIC, BIC, $F$, and $p$-value on original scale.
  - Automated best-model selection.
  - Quadratic optimum computation ($X_{opt} = -b_1 / (2b_2)$, $Y_{opt}$).
  - Allometric $t$-test ($t = |b - 3| / SE_b$, $df = n - 2$).
  - APA Model Comparison Table, Coefficients Table, and Biological Diagnostics Table.
  - 300-DPI Base Graphics scatter plot with best fitted curve and 95% confidence interval.
  - Bilingual interpretations (ID & EN).
- [ ] Register `curve_estimation` in `R/99_dispatcher.R`.
- [ ] Re-run Desktop R test suite to verify GREEN passing status (0 failures).
- [ ] Commit Task 1 changes to git.

---

### Task 2: UI Integration, Presets, Service Worker & End-to-End Verification

**Files:**
- Modify: `js/roles.js`
- Modify: `js/app.js`
- Modify: `sw.js`
- Test: `tests/test_ui_modules.mjs`

**Steps:**
- [ ] Register `curve_estimation` schema in `ANALYSIS_SCHEMAS` in `js/roles.js` (mode `wide` with 2 response columns $X$ and $Y$, or `long` with predictor and response).
- [ ] Add `curve_estimation` to `ANALYSIS_TEMPLATES` in `js/app.js` with:
  - Default preset: Allometric Growth ($W = a L^b$).
  - Second preset: Feeding Rate vs Growth Quadratic Optimum ($Y = a + bX + cX^2$).
  - Sub-model selector in options panel (`all`, `linear`, `quadratic`, `power`, `exponential`, `logarithmic`).
- [ ] Update `sw.js` cache name to `statlab-cache-v5`.
- [ ] Update and run `tests/test_ui_modules.mjs` to verify UI schema validation and preset loading.
- [ ] Execute parity check in Node.js via webR engine.
- [ ] Commit Task 2 changes to git.
- [ ] Verify local dev server at `http://localhost:8080/`.
