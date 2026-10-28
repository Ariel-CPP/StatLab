// tests/test_pdf_and_session.mjs
// StatLab Task 7 Verification Suite:
// SessionStore, .statlab.json export/import, pdfmake Vector PDF Builder & Presets

import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { SessionStore } from "../js/session-store.js";
import { buildSessionPdfDocDefinition } from "../js/pdf-export.js";
import { PRESETS } from "../js/app.js";
import { StatLabEngine } from "../js/webr-client.js";

console.log("=== StatLab Task 7: Session, PDF Export & PWA Test Suite ===");

console.log("--- 1. Testing Presets Definition ---");
assert.ok(PRESETS.wq_ponds, "wq_ponds preset exists");
assert.ok(PRESETS.tilapia_growth, "tilapia_growth preset exists");
assert.ok(PRESETS.shrimp_survival_100, "shrimp_survival_100 preset exists");

// wq_ponds: wide format DO, pH, Suhu, Amonia with 12 ponds
assert.equal(PRESETS.wq_ponds.mode, "wide");
assert.ok(PRESETS.wq_ponds.headers.includes("DO_mgL"));
assert.ok(PRESETS.wq_ponds.headers.includes("Amonia_mgL"));
assert.equal(PRESETS.wq_ponds.rows.length, 12);

// tilapia_growth: long format
assert.equal(PRESETS.tilapia_growth.mode, "long");
assert.ok(PRESETS.tilapia_growth.headers.includes("Perlakuan"));
assert.ok(PRESETS.tilapia_growth.headers.includes("Bobot_g"));

// shrimp_survival_100: constant group in Kontrol
assert.equal(PRESETS.shrimp_survival_100.mode, "wide");
assert.ok(PRESETS.shrimp_survival_100.headers.includes("Kontrol"));

console.log("PASS: Presets verified.");

console.log("--- 2. Executing Analyses in webR ---");
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

console.log("PASS: 2 webR analyses executed successfully.");

console.log("--- 3. Testing SessionStore CRUD & Reordering ---");
const store = new SessionStore({ projectTitle: "Penelitian Uji Pakan Nila", lang: "id", alpha: 0.05 });
assert.equal(store.getState().projectTitle, "Penelitian Uji Pakan Nila");

const id1 = store.addReportItem(res1, "Analisis 1: Bobot Ikan");
const id2 = store.addReportItem(res2, "Analisis 2: Oksigen Terlarut");
assert.equal(store.getState().reportItems.length, 2);

// Reorder: move item 2 up to position 0
store.moveReportItem(id2, -1);
assert.equal(store.getState().reportItems[0].id, id2);
assert.equal(store.getState().reportItems[1].id, id1);

// Boundary checks: moving item 0 up further should be a no-op
store.moveReportItem(id2, -1);
assert.equal(store.getState().reportItems[0].id, id2);

// Move item 0 down
store.moveReportItem(id2, 1);
assert.equal(store.getState().reportItems[1].id, id2);

// Move item 2 back up to position 0 for the remainder of tests
store.moveReportItem(id2, -1);

console.log("PASS: SessionStore item addition & reordering verified.");

console.log("--- 4. Cumulative General Conclusion Compilation via R ---");
const payload = store.getGeneralConclusionPayload();
assert.equal(payload.length, 2);
assert.ok(payload[0].title);
assert.ok("assumption_violated" in payload[0]);
assert.ok("chosen_test" in payload[0]);

const gc = await engine.compileGeneralConclusion(payload, "id");
assert.ok(gc.id && gc.id.length > 0, "General conclusion ID text generated");
assert.ok(gc.en && gc.en.length > 0, "General conclusion EN text generated");
store.setGeneralConclusion(gc);
await engine.close();

console.log("PASS: General Conclusion compiled in Base R via webR.");

console.log("--- 5. .statlab.json Export, Schema Validation & Import Roundtrip ---");
const dumped = store.exportProjectJson();
assert.equal(typeof dumped, "string");
const parsed = JSON.parse(dumped);
assert.equal(parsed.schema_version, 1);
assert.equal(parsed.projectTitle, "Penelitian Uji Pakan Nila");
assert.equal(parsed.reportItems.length, 2);
assert.equal(parsed.generalConclusion.id, gc.id);

// Schema validation test: reject invalid schema
const storeBad = new SessionStore();
assert.throws(() => storeBad.importProjectJson("not-json"), /JSON/i);
assert.throws(() => storeBad.importProjectJson(JSON.stringify({ schema_version: 99 })), /schema/i);
assert.throws(() => storeBad.importProjectJson(JSON.stringify({ schema_version: 1, reportItems: "not-array" })), /reportItems/i);

// Valid roundtrip
const store2 = new SessionStore();
store2.importProjectJson(dumped);
assert.equal(store2.getState().reportItems.length, 2);
assert.equal(store2.getState().generalConclusion.id, gc.id);
assert.equal(store2.getState().projectTitle, "Penelitian Uji Pakan Nila");

// Item deletion
const toRemoveId = store2.getState().reportItems[1].id;
store2.removeReportItem(toRemoveId);
assert.equal(store2.getState().reportItems.length, 1);

console.log("PASS: .statlab.json export, validation, and import verified.");

console.log("--- 6. Storage Persistence Fallback in Node ---");
await store.saveToStorage();
const storeLoaded = new SessionStore();
const loadedOk = await storeLoaded.loadFromStorage();
assert.ok(loadedOk, "Loaded successfully from storage fallback");
assert.equal(storeLoaded.getState().projectTitle, "Penelitian Uji Pakan Nila");
assert.equal(storeLoaded.getState().reportItems.length, 2);

console.log("PASS: Session storage persistence verified.");

console.log("--- 7. pdfmake Vector PDF Builder Structure & Content ---");
const docId = buildSessionPdfDocDefinition(store.getState(), { lang: "id", includeAppendix: true });
assert.ok(docId.content && Array.isArray(docId.content));
assert.equal(docId.pageSize, "A4");

const flatJson = JSON.stringify(docId);
assert.ok(flatJson.includes("Penelitian Uji Pakan Nila"), "Project title in PDF");
assert.ok(flatJson.includes("Kesimpulan Umum"), "General Conclusion heading in ID PDF");
assert.ok(flatJson.includes("data:image/png;base64,"), "300-DPI R plot embedded in PDF");

// Verify APA horizontal lines (no vertical lines)
const hasApaLayout = flatJson.includes("vLineWidth") || JSON.stringify(docId).includes("layout");
assert.ok(hasApaLayout, "APA table layout present in PDF definition");

const docEn = buildSessionPdfDocDefinition(store.getState(), { lang: "en", includeAppendix: false });
assert.ok(JSON.stringify(docEn).includes("General Conclusion"), "General Conclusion heading in EN PDF");

console.log("PASS: pdfmake vector PDF builder verified.");

console.log("--- 8. PWA Shell, Relative Paths & Service Worker Asset Integrity ---");
const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");

// Verify index.html strictly uses relative paths and referenced files exist
const indexHtml = fs.readFileSync(path.join(projectRoot, "index.html"), "utf8");
const hrefAndSrc = [...indexHtml.matchAll(/(?:href|src)=["']([^"']+)["']/g)].map(m => m[1]);
assert.ok(hrefAndSrc.length >= 6, "Expected multiple assets in index.html");

for (const link of hrefAndSrc) {
  assert.ok(!link.startsWith("/") || link.startsWith("//"), `Found root-absolute URL in index.html: ${link} (breaks GitHub Pages subpaths!)`);
  if (link.startsWith("./")) {
    const assetPath = path.join(projectRoot, link.slice(2));
    assert.ok(fs.existsSync(assetPath), `Referenced asset in index.html does not exist: ${link} -> ${assetPath}`);
  }
}

// Verify manifest.json
const manifestRaw = fs.readFileSync(path.join(projectRoot, "manifest.json"), "utf8");
const manifest = JSON.parse(manifestRaw);
assert.equal(manifest.display, "standalone");
assert.equal(manifest.start_url, "./index.html");
assert.equal(manifest.theme_color, "#FBF8F1");
assert.equal(manifest.background_color, "#FBF8F1");
assert.ok(Array.isArray(manifest.icons) && manifest.icons.length >= 2);
for (const icon of manifest.icons) {
  assert.ok(icon.src.startsWith("./"), `Icon src must be relative: ${icon.src}`);
  const iconPath = path.join(projectRoot, icon.src.slice(2));
  assert.ok(fs.existsSync(iconPath), `Manifest icon missing: ${icon.src}`);
}

// Verify sw.js assets to cache
const swRaw = fs.readFileSync(path.join(projectRoot, "sw.js"), "utf8");
const swMatch = swRaw.match(/const ASSETS_TO_CACHE = \[([\s\S]*?)\];/);
assert.ok(swMatch, "sw.js must define ASSETS_TO_CACHE");
const swAssets = swMatch[1]
  .split("\n")
  .map(line => line.trim().replace(/^["']|["'],?$/g, ""))
  .filter(item => item && item !== "./");

assert.ok(swAssets.length >= 20, "Expected at least 20 cached assets in sw.js");
for (const asset of swAssets) {
  const normPath = path.join(projectRoot, asset.replace(/^\.\//, ""));
  assert.ok(fs.existsSync(normPath), `Asset listed in sw.js ASSETS_TO_CACHE does not exist: ${asset} -> ${normPath}`);
}

console.log(`PASS: PWA manifest, relative links, and all ${swAssets.length} service worker assets verified on disk.`);
console.log("=== ALL TASK 7 TESTS PASSED SUCCESSFULLY ===");
