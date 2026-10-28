# Design Specification: StatLab Curve Estimation & Non-Linear Regression

**Date:** 2026-10-28  
**Status:** Approved by User  
**Author:** Antigravity  

---

## 1. Intent & Context
In biological, ecological, and aquaculture experiments, responses rarely follow strict linear relationships. Growth curves, temperature/feeding response curves, and morphological relationships often follow:
1. **Quadratic/Polynomial** response with an optimum point (e.g. optimum feeding rate, temperature peak).
2. **Power/Allometric** relationships (e.g. length-weight relationships $W = a L^b$, where $b = 3$ indicates isometric growth).
3. **Exponential** or **Logarithmic** kinetics.

Users need an integrated **Regresi & Estimasi Kurva (Curve Estimation)** engine that fits multiple candidate curves, compares their goodness-of-fit ($R^2$, Adj-$R^2$, AIC, BIC), recommends the best model, and calculates critical biological diagnostics (such as the vertex optimum point and allometric $b=3$ hypothesis test).

---

## 2. Goals, Non-Goals, and Anti-Goals

### Goals
- **Multi-Model Fitting:** Automatically fit and compare 5 fundamental models:
  1. *Linear*: $Y = b_0 + b_1 X$
  2. *Quadratic (Degree 2 Polynomial)*: $Y = b_0 + b_1 X + b_2 X^2$
  3. *Logarithmic*: $Y = b_0 + b_1 \ln(X)$ (for $X > 0$)
  4. *Exponential*: $Y = a e^{b X}$ (for $Y > 0$)
  5. *Power / Allometric*: $Y = a X^b$ (for $X > 0, Y > 0$)
- **Automated Model Selection:** Compare models via $R^2$, Adjusted $R^2$, AIC, BIC, $F$-statistic, and $p$-value; select and recommend the best fitting model.
- **Biological Diagnostics:**
  - *Quadratic Vertex / Optimum*: Calculate $X_{opt} = -\frac{b_1}{2 b_2}$ and $Y_{opt} = b_0 - \frac{b_1^2}{4 b_2}$, identifying whether it is a maximum (peak) or minimum.
  - *Allometric Hypothesis Test*: For power models ($W = a L^b$), test $H_0: b = 3$ vs $H_1: b \neq 3$ with $t = \frac{|b - 3|}{SE_b}$, determining Isometric vs Positive/Negative Allometric growth.
- **Zero-Error Data Handling:** If $X \le 0$ or $Y \le 0$, emit an informative bilingual warning note and skip non-linear models requiring logarithmic transforms, while keeping Linear and Quadratic models fully functional.
- **Publication Graphics:** Render 300-DPI Base Graphics scatter plot overlaying the best fitted curve with 95% confidence intervals.
- **Example Templates:** Provide 2 Aquatool-style pre-filled templates:
  - *Length-Weight Allometric Relationship* ($W = a L^b$)
  - *Feeding Rate vs Daily Growth Quadratic Optimum* ($Y = a + bX + cX^2$)

### Non-Goals
- Complex multi-parameter differential equation models (e.g. compartmental pharmacokinetic models).
- General non-linear solvers (`nls`) requiring arbitrary starting guesses that risk convergence failure in WebAssembly.

### Anti-Goals
- Zero JavaScript statistical math: 100% of coefficients, standard errors, $p$-values, AIC, and optimum points must be computed in pure Base R.
- Never crash when encountering negative or zero values in logarithmic/power models.

---

## 3. Selected Approach: Approach 1 (Integrated Extension in `R/08_regression_chisq.R`)

We extend `R/08_regression_chisq.R` with the `sl_analyze_curve_estimation(df, opts)` engine, dispatchable via `sl_dispatch("run_analysis", list(analysis_type = "curve_estimation", ...))`.
- **Pure Base R Analytics:** Solved via linear and linearizable least squares (`stats::lm`), guaranteeing 100% convergence and zero dependency on CRAN packages.
- **Seamless UI Integration:** Preserves existing simple linear regression while offering "Regresi & Estimasi Kurva" with model selection dropdown and quick-load presets.

---

## 4. Mathematical Specifications

### 4.1 Models & Transformations
1. **Linear:**
   $$\hat{Y} = b_0 + b_1 X$$
2. **Quadratic:**
   $$\hat{Y} = b_0 + b_1 X + b_2 X^2$$
   Vertex: $X_{opt} = -\frac{b_1}{2 b_2}$, $Y_{opt} = \hat{Y}(X_{opt})$.
   Nature: If $b_2 < 0 \implies$ Maksimum (Puncak Optimum); If $b_2 > 0 \implies$ Minimum.
3. **Logarithmic:**
   $$\hat{Y} = b_0 + b_1 \ln(X) \quad (X > 0)$$
4. **Exponential:**
   $$\ln(\hat{Y}) = \ln(a) + b X \implies \hat{Y} = a e^{b X} \quad (Y > 0)$$
   where $a = e^{\text{intercept}}$.
5. **Power / Allometric:**
   $$\ln(\hat{Y}) = \ln(a) + b \ln(X) \implies \hat{Y} = a X^b \quad (X > 0, Y > 0)$$
   where $a = e^{\text{intercept}}$.
   Allometric t-test: $t_{calc} = \frac{|b - 3|}{SE_b}$, $df = n - 2$, $p = 2 \times (1 - F_t(t_{calc}, df))$.
   - If $p \ge 0.05 \implies$ Isometrik ($b = 3$).
   - If $p < 0.05$ and $b > 3 \implies$ Alometrik Positif (pertumbuhan bobot lebih cepat daripada panjang).
   - If $p < 0.05$ and $b < 3 \implies$ Alometrik Negatif (pertumbuhan panjang lebih cepat daripada bobot).

### 4.2 Model Selection Criteria
For each fitted model $m$:
- Residual Sum of Squares: $RSS = \sum (Y_i - \hat{Y}_i)^2$
- $R^2 = 1 - \frac{RSS}{TSS}$ (evaluated on the original scale $Y$)
- $\text{Adj-}R^2 = 1 - \frac{RSS / (n - p)}{TSS / (n - 1)}$
- $AIC = n \ln(RSS / n) + 2p$
- $BIC = n \ln(RSS / n) + p \ln(n)$
Best model: Selected from models with valid fit by prioritizing highest $\text{Adj-}R^2$ and lowest $AIC$.

---

## 5. UI and Template Specifications
- **Dropdown Item:** `curve_estimation` labeled "Regresi & Estimasi Kurva (Curve Estimation)".
- **Sub-option Model Filter:**
  - `all` (Evaluasi Semua Kurva & Pilih Terbaik - Default)
  - `linear` (Linier Sederhana)
  - `quadratic` (Polinomial Kuadratik)
  - `power` (Power / Alometrik)
  - `exponential` (Eksponensial)
  - `logarithmic` (Logaritmik)
- **Presets:**
  - `allometric_growth`: Panjang (cm) vs Bobot (g) ikan nila (20 observasi).
  - `protein_growth_optimum`: Dosis Protein Pakan (%) vs Laju Pertumbuhan Harian (%/hari) (15 observasi).

---

## 6. Verification & Quality Gates
- **Desktop R Test Suite:** `tests/test_07_inferential.R` updated with test assertions for all 5 models, vertex calculation, allometric t-test, and model selection.
- **Numerical Parity:** $\le 10^{-8}$ against Desktop R 4.5.2.
- **PWA & Offline Integrity:** Zero CDN, local vendor assets, PWA Service Worker cache v4 updated.
