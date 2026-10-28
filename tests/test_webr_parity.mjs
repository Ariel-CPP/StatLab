// tests/test_webr_parity.mjs
// StatLab Parity & Unit Test Suite:
// Tests Locale Number/CSV Parser & webR-vs-Desktop R 4.5.2 Parity (<= 1e-8)

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { t, SUPPORTED_LANGS } from "../js/i18n.js";
import { detectLocaleFormat, parseCellValue, parseDelimitedText, serializeTableDelimited } from "../js/number-locale.js";
import { StatLabEngine } from "../js/webr-client.js";

console.log("--- 1. Testing i18n ---");
assert.deepEqual(SUPPORTED_LANGS, ["id", "en"]);
assert.ok(t("app.title", "id").length > 0);
assert.ok(t("app.title", "en").length > 0);
assert.equal(t("test.interpolate", "id", { name: "StatLab" }), "StatLab");

console.log("--- 2. Indonesian Excel CSV (';' delimiter + ',' decimal) ---");
const idCsv = "Perlakuan;Bobot_g;SR_persen\nKontrol;12,45;95,0\nPakan_A;15,80;98,5\nPakan_A;1.234,56;100\n";
const parsedId = parseDelimitedText(idCsv, "auto");
assert.equal(parsedId.locale.delimiter, ";");
assert.equal(parsedId.locale.decimalMark, ",");
assert.equal(parseCellValue("12,45", "auto", parsedId.locale).value, 12.45);
assert.equal(parseCellValue("1.234,56", "auto", parsedId.locale).value, 1234.56);

// Isolated dual-separator auto-detection
assert.equal(parseCellValue("1.234,56", "auto").value, 1234.56);
assert.equal(parseCellValue("1,234.56", "auto").value, 1234.56);

// Explicit mode override on delimited text
const forcedId = parseDelimitedText("X;Y\n1.234;5.678\n", "id_comma");
assert.equal(forcedId.locale.decimalMark, ",");
assert.equal(forcedId.locale.ambiguous, false);

console.log("--- 3. Review Focus #1: Ambiguous 3-trailing-digit '1.234' without comma clues ---");
const ambig = detectLocaleFormat("Bobot\n1.234\n2.345\n");
assert.equal(ambig.ambiguous, true, "3-digit dot pattern flagged ambiguous");
assert.equal(parseCellValue("1.234", "auto", ambig).value, 1.234, "Defaults safely to decimal 1.234 unless overridden");

console.log("--- 4. Review Focus #3: Dirty cells ('12,5 ', 'NA', '-', '') ---");
assert.equal(parseCellValue(" 12,5 ", "id_comma").value, 12.5);
assert.equal(parseCellValue("NA", "id_comma").type, "missing");
assert.equal(parseCellValue("-", "id_comma").type, "missing");
assert.equal(parseCellValue("mati", "id_comma").type, "text");
assert.equal(parseCellValue("", "auto").type, "missing");
assert.equal(parseCellValue(null, "auto").type, "missing");
assert.equal(parseCellValue(undefined, "auto").type, "missing");

console.log("--- 5. Export formatting follows language (ID: ';' and ',', EN: ',' and '.') ---");
assert.equal(serializeTableDelimited(["G", "Mean"], [["K", "12,45"]], "id", "csv").trim(), "G;Mean\nK;12,45");
assert.equal(serializeTableDelimited(["G", "Mean"], [["K", "12.45"]], "en", "csv").trim(), "G,Mean\nK,12.45");
assert.equal(serializeTableDelimited(["G", "Mean"], [["K", "12,45"]], "id", "tsv").trim(), "G\tMean\nK\t12,45");

console.log("--- 6. Spec §9.3: webR vs Desktop R 4.5.2 parity <= 1e-8 on aquaculture dataset ---");
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

// General conclusion test via webR
const genConclusionRes = await engine.compileGeneralConclusion(
  [{ title: "Uji Deskriptif", conclusion_md: "Data berdistribusi normal." }],
  "id"
);
assert.ok(genConclusionRes.id && genConclusionRes.id.length > 0);

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
const rootDir = fileURLToPath(new URL("../", import.meta.url));
const desktopJson = execFileSync(
  "C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe",
  ["-"],
  { input: rCmd, encoding: "utf8", cwd: rootDir }
);
const deskRes = JSON.parse(desktopJson);

for (let i = 0; i < 2; i++) {
  for (const k of ["mean", "sd", "se", "median", "iqr", "cv_pct", "skewness", "kurtosis", "ci_low", "ci_high"]) {
    const diff = Math.abs(webrRes.main.groups[i][k] - deskRes.main.groups[i][k]);
    assert.ok(diff <= 1e-8, `Parity failed on group ${i} ${k}: webr=${webrRes.main.groups[i][k]}, desk=${deskRes.main.groups[i][k]}, diff=${diff}`);
  }
  const shapiroDiff = Math.abs(webrRes.assumptions.per_group[i].shapiro.p_value - deskRes.assumptions.per_group[i].shapiro.p_value);
  assert.ok(shapiroDiff <= 1e-8, `Shapiro parity failed on group ${i}: diff=${shapiroDiff}`);
  const lillieDiff = Math.abs(webrRes.assumptions.per_group[i].lilliefors.p_value - deskRes.assumptions.per_group[i].lilliefors.p_value);
  assert.ok(lillieDiff <= 1e-8, `Lilliefors parity failed on group ${i}: diff=${lillieDiff}`);
}

const leveneDiff = Math.abs(webrRes.assumptions.homogeneity.levene_bf.p_value - deskRes.assumptions.homogeneity.levene_bf.p_value);
assert.ok(leveneDiff <= 1e-8, `Levene parity failed: diff=${leveneDiff}`);

assert.ok(webrRes.plots.length >= 2, "Expected at least 2 plots");
assert.ok(webrRes.plots[0].png_data_url.startsWith("data:image/png;base64,"), "Plot 0 should be base64 PNG data URL");

console.log("--- 7. Testing Newly Added Modules in webR (RCBD, Mann-Whitney, Probit) ---");
const rcbdRes = await engine.runAnalysis(
  "rcbd",
  {
    Blok_1: [24.5, 28.3, 33.8],
    Blok_2: [26.0, 30.1, 35.2],
    Blok_3: [25.2, 29.4, 34.6],
    Blok_4: [27.1, 31.2, 36.5]
  },
  { mode: "wide", response_cols: ["Blok_1", "Blok_2", "Blok_3", "Blok_4"], group_col: null },
  { include_plots: false }
);
assert.ok(rcbdRes && rcbdRes.main && rcbdRes.main.f_treatment !== undefined, "RCBD successfully executed in webR");

const mwRes = await engine.runAnalysis(
  "mann_whitney",
  {
    Kontrol: [12.5, 11.8, 13.0, 12.2],
    Probiotik: [15.2, 16.0, 15.8, 16.5]
  },
  { mode: "wide", response_cols: ["Kontrol", "Probiotik"], group_col: null },
  { include_plots: false }
);
assert.ok(mwRes && mwRes.main && mwRes.main.p_value !== undefined, "Mann-Whitney successfully executed in webR");

const probitRes = await engine.runAnalysis(
  "probit_lc50",
  {
    Konsentrasi_ppm: [0, 10, 25, 50, 100, 200],
    Mortalitas_Ekor: [0, 2, 6, 11, 16, 20],
    Total_Uji: [20, 20, 20, 20, 20, 20]
  },
  { mode: "wide", response_cols: ["Konsentrasi_ppm", "Mortalitas_Ekor", "Total_Uji"], group_col: null },
  { include_plots: false }
);
assert.ok(probitRes && probitRes.main && probitRes.main.lc50 !== undefined, "Probit LC50 successfully executed in webR");

const logitRes = await engine.runAnalysis(
  "binary_logistic",
  {
    Status_Sintasan: [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
    DO_mgL: [6.5, 6.0, 5.8, 5.5, 5.2, 4.5, 4.2, 3.8, 3.5, 3.2],
    TAN_ppm: [0.12, 0.18, 0.22, 0.25, 0.30, 0.55, 0.62, 0.75, 0.82, 0.90]
  },
  { mode: "wide", response_cols: ["Status_Sintasan", "DO_mgL", "TAN_ppm"], group_col: null },
  { include_plots: false }
);
assert.ok(logitRes && logitRes.main && logitRes.main.auc !== undefined, "Binary Logistic successfully executed in webR");

console.log("PASS: webR vs Desktop R 4.5.2 parity <= 1e-8 and all new modules verified in webR");
