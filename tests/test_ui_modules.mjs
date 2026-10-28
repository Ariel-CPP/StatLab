// tests/test_ui_modules.mjs
// StatLab Unit Test Suite for UI Modules:
// Lab Notebook CSS, StatLabGrid, RoleMappingPanel & ResultView

import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, "..");

// Helper to create a lightweight mock DOM element for testing in Node.js
export function createMockContainer() {
  const listeners = {};
  return {
    innerHTML: "",
    tagName: "DIV",
    className: "",
    style: {},
    children: [],
    addEventListener(event, fn) {
      listeners[event] = listeners[event] || [];
      listeners[event].push(fn);
    },
    removeEventListener(event, fn) {
      if (listeners[event]) {
        listeners[event] = listeners[event].filter(cb => cb !== fn);
      }
    },
    dispatchEvent(event) {
      const cbs = listeners[event.type || event] || [];
      for (const cb of cbs) cb(event);
    },
    querySelector(sel) {
      return null;
    },
    querySelectorAll(sel) {
      return [];
    }
  };
}

console.log("=== StatLab Task 6 UI Modules Test Suite ===");

// ---------------------------------------------------------------------------
// 1. CSS Lab Notebook Theme & Scope Isolation Checks
// ---------------------------------------------------------------------------
console.log("--- 1. CSS Lab Notebook Theme & Scope Isolation ---");
const cssPath = path.join(rootDir, "css", "statlab.css");
assert.ok(fs.existsSync(cssPath), "css/statlab.css must exist");

const cssContent = fs.readFileSync(cssPath, "utf8");

// Assert theme palette variables
assert.ok(cssContent.includes("--sl-paper: #FBF8F1"), "Palette --sl-paper: #FBF8F1 required");
assert.ok(cssContent.includes("--sl-surface: #FFFDF8"), "Palette --sl-surface: #FFFDF8 required");
assert.ok(cssContent.includes("--sl-ink: #1B365D"), "Palette --sl-ink: #1B365D required");
assert.ok(cssContent.includes("--sl-crimson: #B83227"), "Palette --sl-crimson: #B83227 required");
assert.ok(cssContent.includes("--sl-grid-line: rgba(27, 54, 93, 0.055)"), "Palette --sl-grid-line required");

// Assert required component classes
const requiredClasses = [
  ".sl-grid-table",
  ".sl-apa-table",
  ".sl-spss-details",
  ".sl-formula-box",
  ".sl-outlier-badge",
  ".sl-session-drawer"
];
for (const cls of requiredClasses) {
  assert.ok(cssContent.includes(cls), `Required CSS class ${cls} must be defined`);
}

// Scope isolation check:
// Zero un-scoped global rules that would pollute host pages (like Aqua Tools).
// Strip comments and keyframes
const cssNoComments = cssContent
  .replace(/\/\*[\s\S]*?\*\//g, "")
  .replace(/@keyframes[\s\S]*?\}\s*\}/g, "");
// Find CSS selectors before '{'
const rules = cssNoComments.split("}").map(r => r.trim()).filter(Boolean);
for (const rule of rules) {
  const parts = rule.split("{");
  if (parts.length < 2) continue;
  const selectorGroup = parts[0].trim();
  if (!selectorGroup || selectorGroup.startsWith("@")) continue; // Skip @keyframes, @media, etc.

  const selectors = selectorGroup.split(",").map(s => s.trim()).filter(Boolean);
  for (const sel of selectors) {
    // Assert selector is scoped to .sl-app or body.sl-standalone or starts with .sl-
    const isScoped =
      sel.startsWith(".sl-app") ||
      sel.startsWith("body.sl-standalone") ||
      sel.startsWith(".sl-");
    assert.ok(
      isScoped,
      `Un-scoped CSS selector found in css/statlab.css: "${sel}". All rules must be scoped under .sl-app or body.sl-standalone.`
    );

    // Extra check: ensure no bare global '*' or 'body' selector without scoping
    assert.ok(
      !sel.match(/^(\*|body(?!(\.sl-standalone|\.sl-app))|html)\b/),
      `Illegal global un-scoped selector "${sel}" in css/statlab.css`
    );
  }
}
console.log("PASS: CSS isolation and theme palette variables verified.");

// ---------------------------------------------------------------------------
// 2. StatLabGrid Data Handling, Outlier Badging & Row Exclusion
// ---------------------------------------------------------------------------
console.log("--- 2. StatLabGrid: Excel Paste, Typed Columns & Outlier Flagging ---");
const { StatLabGrid } = await import("../js/grid.js");

const mockContainer = createMockContainer();
let changeEventCount = 0;
const grid = new StatLabGrid(mockContainer, {
  lang: "id",
  decimalMode: "auto",
  onChange: () => { changeEventCount++; }
});

// Set basic 2x2 data
grid.setData(["Kontrol", "Artemia"], [["12,5", "18,2"], ["13,0", "19,5"]]);
const data1 = grid.getData();
assert.deepEqual(data1.headers, ["Kontrol", "Artemia"]);
assert.equal(data1.rows.length, 2);

// Test TSV load / Excel paste expansion
const excelTsv = "P1\tP2\tP3\n10,1\t20,2\t30,3\n11,2\t21,3\t31,4\n12,3\t22,4\t32,5\n";
const detected = grid.loadDelimitedText(excelTsv, "auto");
assert.equal(detected.delimiter, "\t");
assert.equal(detected.decimalMark, ",");

const data2 = grid.getData();
assert.deepEqual(data2.headers, ["P1", "P2", "P3"]);
assert.equal(data2.rows.length, 3);
assert.deepEqual(data2.rows[0], ["10,1", "20,2", "30,3"]);

// Test getTypedColumns() using number-locale parser
const typed = grid.getTypedColumns();
assert.deepEqual(typed.columns.P1, [10.1, 11.2, 12.3]);
assert.deepEqual(typed.columns.P2, [20.2, 21.3, 22.4]);
assert.deepEqual(typed.columns.P3, [30.3, 31.4, 32.5]);

// Test dirty cells & missing tokens in grid
grid.setData(["Var1", "Var2"], [
  ["12,5", "Kontrol"],
  [" NA ", "Perlakuan_A"],
  ["-", "Perlakuan_B"],
  ["1.234,56", "Kontrol"]
]);
const typedDirty = grid.getTypedColumns();
assert.equal(typedDirty.columns.Var1[0], 12.5);
assert.equal(typedDirty.columns.Var1[1], null, "NA token parsed as null missing");
assert.equal(typedDirty.columns.Var1[2], null, "'-' token parsed as null missing");
assert.equal(typedDirty.columns.Var1[3], 1234.56, "Indonesian thousands parsed properly");
assert.equal(typedDirty.columns.Var2[0], "Kontrol", "Categorical strings preserved");

// Outlier Non-Deletion Rule (§2, §6.5):
// Flagging outliers must NEVER automatically exclude rows!
assert.deepEqual(grid.getExcludedRows(), [], "Excluded rows must be empty by default");

grid.setOutlierFlags([
  { source_row: 2, group: "Kontrol", value: 99.5, by_iqr: true, by_grubbs: true }
]);
assert.deepEqual(grid.getExcludedRows(), [], "Excluded rows must remain empty after setOutlierFlags()");

// Explicit opt-in exclusion
grid.toggleExcludeRow(2, true);
assert.deepEqual(grid.getExcludedRows(), [2], "Row 2 explicitly excluded");

grid.toggleExcludeRow(4, true);
assert.deepEqual(grid.getExcludedRows(), [2, 4], "Rows 2 and 4 explicitly excluded");

grid.toggleExcludeRow(2, false);
assert.deepEqual(grid.getExcludedRows(), [4], "Row 2 un-excluded");

console.log("PASS: StatLabGrid methods, typing, and outlier non-deletion rules verified.");

// ---------------------------------------------------------------------------
// 3. RoleMappingPanel Auto-Detection & Validation
// ---------------------------------------------------------------------------
console.log("--- 3. RoleMappingPanel: Smart Detection & Schema Validation ---");
const { ANALYSIS_SCHEMAS, RoleMappingPanel } = await import("../js/roles.js");

assert.ok(ANALYSIS_SCHEMAS.descriptive, "Schema for descriptive analysis must exist");

const roleContainer = createMockContainer();
let roleChangeCount = 0;
const rolePanel = new RoleMappingPanel(roleContainer, {
  analysisType: "descriptive",
  onChange: () => { roleChangeCount++; }
});

// Auto-detection Case 1: Column 1 is categorical text, Column 2 is numeric -> Long format
rolePanel.setColumns(
  ["Perlakuan", "Bobot_g"],
  { Perlakuan: "text", Bobot_g: "numeric" }
);
const mapping1 = rolePanel.getMapping();
assert.equal(mapping1.mode, "long", "Categorical col 1 + numeric col 2 must default to 'long'");
assert.equal(mapping1.group_col, "Perlakuan");
assert.deepEqual(mapping1.response_cols, ["Bobot_g"]);

// Auto-detection Case 2: >= 2 numeric columns -> Wide format
rolePanel.setColumns(
  ["Kontrol", "Pakan_A", "Pakan_B"],
  { Kontrol: "numeric", Pakan_A: "numeric", Pakan_B: "numeric" }
);
const mapping2 = rolePanel.getMapping();
assert.equal(mapping2.mode, "wide", ">= 2 numeric columns must default to 'wide'");
assert.equal(mapping2.group_col, null);
assert.deepEqual(mapping2.response_cols, ["Kontrol", "Pakan_A", "Pakan_B"]);

// Validation checks
const validRes = rolePanel.validate();
assert.equal(validRes.valid, true);
assert.ok(ANALYSIS_SCHEMAS.curve_estimation, "ANALYSIS_SCHEMAS must contain curve_estimation");
assert.equal(ANALYSIS_SCHEMAS.curve_estimation.minNumericCols, 2);

// Invalid case: no response columns
rolePanel.setMapping({ mode: "wide", response_cols: [], group_col: null });
const invalidRes = rolePanel.validate();
assert.equal(invalidRes.valid, false);
assert.ok(invalidRes.errors.length > 0);

console.log("PASS: RoleMappingPanel auto-detection and validation verified.");

// ---------------------------------------------------------------------------
// 4. Result View: Rendering Order, Tables, KaTeX Safe Fallback & XSS Escaping
// ---------------------------------------------------------------------------
console.log("--- 4. Result View: Strict Section Order, Tables & XSS Escaping ---");
const { renderAnalysisResult } = await import("../js/result-view.js");

const mockResult = {
  meta: {
    analysis_type: "descriptive",
    analysis_name: "Statistika Deskriptif",
    response_vars: ["Bobot_<script>alert(1)</script>"],
    group_var: "Perlakuan_<b>A</b>",
    alpha: 0.05,
    n_total_obs: 20,
    n_valid_obs: 19,
    n_missing_obs: 1,
    n_excluded_obs: 0,
    timestamp: "2026-10-28T09:00:00Z"
  },
  assumptions: {
    per_group: [
      {
        group: "Kelompok_<img src=x onerror=alert(1)>",
        n: 10,
        is_normal: true,
        shapiro: { valid: true, statistic: 0.965, p_value: 0.723, note_key: null },
        lilliefors: { valid: true, statistic: 0.12, p_value: 0.85, note_key: null }
      }
    ],
    homogeneity: {
      levene_bf: { valid: true, statistic: 1.45, df1: 1, df2: 18, p_value: 0.245, is_homo: true, note_key: null },
      bartlett: { valid: true, statistic: 1.12, df: 1, p_value: 0.29, is_homo: true, note_key: null }
    },
    overall_normal: true,
    overall_homo: true,
    violations: []
  },
  outliers: {
    flagged: [
      { source_row: 5, group: "Kelompok A", value: 99.8, by_iqr: true, by_grubbs: true }
    ],
    per_group: {}
  },
  formulas: [
    {
      label: { id: "Rerata Sampel", en: "Sample Mean" },
      latex: "\\bar{x} = \\frac{1}{n}\\sum x_i"
    }
  ],
  tables_apa: [
    {
      id: "apa_desc",
      title: { id: "Tabel 1. Statistika Deskriptif", en: "Table 1. Descriptive Statistics" },
      headers: { id: ["Kelompok", "n", "Rerata \u00b1 SD"], en: ["Group", "n", "Mean \u00b1 SD"] },
      rows: { id: [["Kelompok A", "10", "45,20 \u00b1 2,15"]], en: [["Group A", "10", "45.20 \u00b1 2.15"]] },
      note: { id: "Catatan: Nilai disajikan dalam rata-rata.", en: "Note: Values presented as mean." }
    }
  ],
  tables_full: [
    {
      id: "spss_desc",
      title: { id: "Lampiran A. Tabel Lengkap", en: "Appendix A. Full Table" },
      headers: { id: ["Kelompok", "Median", "IQR"], en: ["Group", "Median", "IQR"] },
      rows: { id: [["Kelompok A", "45,0", "3,2"]], en: [["Group A", "45.0", "3.2"]] },
      note: { id: "Catatan lengkap.", en: "Full notes." }
    }
  ],
  plots: [
    {
      id: "qq_plot",
      title: { id: "Plot Q-Q Normalitas", en: "Normal Q-Q Plot" },
      png_data_url: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY44YAAAAASUVORK5CYII=",
      width_in: 6.5,
      height_in: 4.2
    }
  ],
  interpretation: {
    id: "Data terdistribusi normal (Shapiro-Wilk p > 0,05) dan varians homogen.",
    en: "Data is normally distributed (Shapiro-Wilk p > 0.05) and variance is homogeneous."
  }
};

const resultContainer = createMockContainer();
let rerunCalled = false;
let reportAdded = false;

renderAnalysisResult(resultContainer, mockResult, {
  lang: "id",
  onRerun: () => { rerunCalled = true; },
  onAddToReport: () => { reportAdded = true; }
});

const html = resultContainer.innerHTML;
assert.ok(html.length > 0, "Rendered HTML must not be empty");

// 1. XSS Escaping Assertion
assert.ok(!html.includes("<script>alert(1)</script>"), "User script tags must be escaped!");
assert.ok(html.includes("&lt;script&gt;alert(1)&lt;/script&gt;"), "Escaped script tag expected");
assert.ok(!html.includes("<img src=x onerror=alert(1)>"), "User img onerror must be escaped!");

// 2. Strict Section Ordering Assertion (§6.1):
// 1: Meta/Formula -> 2: Assumptions -> 3: Outlier Alert -> 4: APA Table ->
// 5: SPSS Details -> 6: Plot Gallery -> 7: Interpretation + Report Button
const idxMeta = html.indexOf("sl-meta-box");
const idxFormula = html.indexOf("sl-formula-box");
const idxAssumptions = html.indexOf("sl-assumptions-banner");
const idxOutliers = html.indexOf("sl-outlier-alert");
const idxApa = html.indexOf("sl-apa-table");
const idxSpss = html.indexOf("sl-spss-details");
const idxPlots = html.indexOf("sl-plot-gallery");
const idxInterp = html.indexOf("sl-interpretation-box");
const idxAddReport = html.indexOf("sl-btn-add-report");

assert.ok(idxMeta !== -1, "sl-meta-box must be present");
assert.ok(idxFormula !== -1, "sl-formula-box must be present");
assert.ok(idxAssumptions !== -1, "sl-assumptions-banner must be present");
assert.ok(idxOutliers !== -1, "sl-outlier-alert must be present");
assert.ok(idxApa !== -1, "sl-apa-table must be present");
assert.ok(idxSpss !== -1, "sl-spss-details must be present");
assert.ok(idxPlots !== -1, "sl-plot-gallery must be present");
assert.ok(idxInterp !== -1, "sl-interpretation-box must be present");
assert.ok(idxAddReport !== -1, "sl-btn-add-report must be present");

assert.ok(idxMeta < idxAssumptions, "1. Meta/Formula must precede 2. Assumptions");
assert.ok(idxAssumptions < idxOutliers, "2. Assumptions must precede 3. Outlier Alert");
assert.ok(idxOutliers < idxApa, "3. Outlier Alert must precede 4. APA Table");
assert.ok(idxApa < idxSpss, "4. APA Table must precede 5. SPSS Details");
assert.ok(idxSpss < idxPlots, "5. SPSS Details must precede 6. Plot Gallery");
assert.ok(idxPlots < idxInterp, "6. Plot Gallery must precede 7. Interpretation");
assert.ok(idxInterp < idxAddReport, "Interpretation must include or precede Add-Report button");

console.log("PASS: Result View section ordering, APA/SPSS tables, and XSS safety verified.");
console.log("\nALL UI MODULE TESTS COMPLETED SUCCESSFULLY.");
