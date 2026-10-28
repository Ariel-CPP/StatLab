// js/app.js
// StatLab Main Application Shell & PWA Controller
// Mounts the complete Lab Notebook UI, orchestrates webR engine, StatLabGrid,
// RoleMappingPanel, Result View, SessionStore, and PDF Export.

import { t, SUPPORTED_LANGS } from "./i18n.js";
import { parseCellValue } from "./number-locale.js";
import { StatLabEngine } from "./webr-client.js";
import { StatLabGrid } from "./grid.js";
import { RoleMappingPanel } from "./roles.js";
import { renderAnalysisResult, escapeHtml } from "./result-view.js";
import { SessionStore } from "./session-store.js";
import { buildSessionPdfDocDefinition, downloadSessionPdf } from "./pdf-export.js";

/**
 * Phase 1 Built-in Presets
 */
export const PRESETS = {
  wq_ponds: {
    id: "wq_ponds",
    name: {
      id: "Kualitas Air Tambak Udang Vaname (12 Tambak)",
      en: "Vaname Shrimp Pond Water Quality (12 Ponds)"
    },
    mode: "wide",
    headers: ["DO_mgL", "pH", "Suhu_C", "Amonia_mgL"],
    rows: [
      ["5.8", "7.8", "29.2", "0.04"],
      ["6.1", "7.9", "29.5", "0.05"],
      ["5.5", "8.1", "29.1", "0.03"],
      ["6.0", "7.7", "30.0", "0.06"],
      ["5.7", "8.0", "29.8", "0.04"],
      ["5.9", "7.9", "29.4", "0.05"],
      ["5.4", "8.2", "29.0", "0.45"], // Spiked outlier in pond 7!
      ["6.2", "7.8", "30.2", "0.04"],
      ["5.6", "7.9", "29.6", "0.03"],
      ["6.0", "8.0", "29.5", "0.05"],
      ["5.8", "7.7", "29.3", "0.06"],
      ["5.9", "7.9", "29.7", "0.04"]
    ],
    roles: {
      mode: "wide",
      response_cols: ["DO_mgL", "pH", "Suhu_C", "Amonia_mgL"],
      group_col: null
    }
  },
  tilapia_growth: {
    id: "tilapia_growth",
    name: {
      id: "Bobot Akhir Ikan Nila 4 Perlakuan Pakan",
      en: "Nile Tilapia Final Weight across 4 Feed Treatments"
    },
    mode: "long",
    headers: ["Perlakuan", "Bobot_g"],
    rows: [
      ["Kontrol", "45.2"], ["Kontrol", "42.8"], ["Kontrol", "46.1"], ["Kontrol", "44.0"], ["Kontrol", "43.5"],
      ["Kontrol", "45.9"], ["Kontrol", "44.8"], ["Kontrol", "42.1"], ["Kontrol", "46.5"], ["Kontrol", "43.9"],
      ["Pakan_A", "52.3"], ["Pakan_A", "50.8"], ["Pakan_A", "53.1"], ["Pakan_A", "51.5"], ["Pakan_A", "54.0"],
      ["Pakan_A", "52.9"], ["Pakan_A", "51.2"], ["Pakan_A", "53.5"], ["Pakan_A", "50.5"], ["Pakan_A", "52.1"],
      ["Pakan_B", "58.4"], ["Pakan_B", "57.1"], ["Pakan_B", "60.2"], ["Pakan_B", "59.0"], ["Pakan_B", "58.8"],
      ["Pakan_B", "57.5"], ["Pakan_B", "61.0"], ["Pakan_B", "58.2"], ["Pakan_B", "59.5"], ["Pakan_B", "57.9"],
      ["Pakan_C", "55.4"], ["Pakan_C", "58.1"], ["Pakan_C", "54.9"], ["Pakan_C", "56.2"], ["Pakan_C", "59.0"],
      ["Pakan_C", "53.5"], ["Pakan_C", "57.3"], ["Pakan_C", "55.0"], ["Pakan_C", "58.6"], ["Pakan_C", "56.8"]
    ],
    roles: {
      mode: "long",
      response_cols: ["Bobot_g"],
      group_col: "Perlakuan"
    }
  },
  shrimp_survival_100: {
    id: "shrimp_survival_100",
    name: {
      id: "Sintasan Udang 100% pada Kontrol (Data Konstan)",
      en: "Shrimp Survival 100% in Control (Constant Data Edge-Case)"
    },
    mode: "wide",
    headers: ["Kontrol", "Salinitas_15", "Salinitas_10", "Salinitas_5"],
    rows: [
      ["100", "98", "92", "68"],
      ["100", "96", "88", "62"],
      ["100", "99", "90", "70"],
      ["100", "97", "85", "65"],
      ["100", "98", "91", "58"],
      ["100", "95", "87", "64"],
      ["100", "97", "89", "71"],
      ["100", "99", "93", "60"]
    ],
    roles: {
      mode: "wide",
      response_cols: ["Kontrol", "Salinitas_15", "Salinitas_10", "Salinitas_5"],
      group_col: null
    }
  }
};

/**
 * Mount StatLab Application Shell
 *
 * @param {HTMLElement} rootEl Target DOM element
 * @param {object} [config={}] Configuration options
 */
export async function mountStatLab(rootEl, config = {}) {
  if (!rootEl) return;

  const defaultTitle = config.projectTitle || "Proyek Analisis Biostatistika";
  const defaultLang = config.lang || "id";

  const store = new SessionStore({
    projectTitle: defaultTitle,
    lang: defaultLang,
    alpha: config.alpha ?? 0.05,
    decimalMode: config.decimalMode || "auto"
  });

  const webrBaseUrl = config.webrBaseUrl || "./vendor/webr/";
  const engine = new StatLabEngine({ baseUrl: webrBaseUrl });

  // Render Skeleton Shell
  rootEl.classList.add("sl-app");
  rootEl.innerHTML = `
    <header class="sl-navbar">
      <div class="sl-nav-brand">
        <img src="./assets/icon-192.svg" alt="StatLab Emblem">
        <h1 class="sl-nav-title">StatLab</h1>
        <span class="sl-engine-badge" id="sl-engine-badge">&#9679; Memuat webR...</span>
      </div>
      <div class="sl-nav-controls">
        <input type="text" class="sl-project-title-input" id="sl-project-title-input" value="${escapeHtml(store.getState().projectTitle)}" title="Judul Proyek">
        
        <select class="sl-select" id="sl-preset-select">
          <option value="">-- Muat Data Contoh --</option>
          <option value="wq_ponds">Tambak Udang (Wide, Pencilan Amonia)</option>
          <option value="tilapia_growth">Uji Pakan Nila (Long, 4 Kelompok)</option>
          <option value="shrimp_survival_100">Sintasan Udang (Kontrol 100%, Varians 0)</option>
        </select>

        <select class="sl-select" id="sl-decimal-select" title="Format Angka / Desimal">
          <option value="auto">Desimal: Otomatis</option>
          <option value="id_comma">Koma (ID: 12,5)</option>
          <option value="en_dot">Titik (EN: 12.5)</option>
        </select>

        <select class="sl-select" id="sl-alpha-select" title="Tingkat Signifikansi">
          <option value="0.05">&alpha; = 0.05</option>
          <option value="0.01">&alpha; = 0.01</option>
          <option value="0.10">&alpha; = 0.10</option>
        </select>

        <select class="sl-select" id="sl-lang-select" title="Bahasa">
          <option value="id">Bahasa Indonesia</option>
          <option value="en">English</option>
        </select>
      </div>
    </header>

    <main class="sl-notebook-container">
      <div class="sl-tabs">
        <button class="sl-tab-btn active" data-tab="grid">
          <span>&#128202;</span> <span class="sl-tab-label-grid">Data Grid &amp; Peran</span>
        </button>
        <button class="sl-tab-btn" data-tab="result">
          <span>&#129514;</span> <span class="sl-tab-label-result">Hasil Analisis</span>
        </button>
        <button class="sl-tab-btn" data-tab="report">
          <span>&#128214;</span> <span class="sl-tab-label-report">Laporan Sesi</span>
          <span class="sl-counter-pill" id="sl-report-count">0</span>
        </button>
      </div>

      <!-- Tab 1: Data Grid & Roles -->
      <section class="sl-tab-pane active" id="sl-tab-grid">
        <div class="sl-notebook-sheet">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; flex-wrap: wrap; gap: 8px;">
            <h2>Lembar Kerja Data &amp; Pemetaan Variabel</h2>
            <div style="display: flex; gap: 8px;">
              <button class="sl-btn sl-btn-sm" id="sl-btn-export-json">&#128190; Ekspor .statlab.json</button>
              <button class="sl-btn sl-btn-sm" id="sl-btn-import-json">&#128194; Impor .statlab.json</button>
              <input type="file" id="sl-file-import-json" accept=".json,.statlab.json" style="display: none;">
              <button class="sl-btn sl-btn-primary" id="sl-btn-run-analysis">&#9654; Jalankan Analisis</button>
            </div>
          </div>
          <div id="sl-grid-container"></div>
          <div id="sl-role-container" style="margin-top: 24px;"></div>
        </div>
      </section>

      <!-- Tab 2: Result View -->
      <section class="sl-tab-pane" id="sl-tab-result">
        <div id="sl-result-container">
          <div class="sl-notebook-sheet" style="text-align: center; color: var(--sl-ink-subtle); padding: 48px 24px;">
            <p style="font-size: 16px;">Belum ada hasil analisis. Pilih variabel di tab <strong>Data Grid &amp; Peran</strong> lalu klik <strong>Jalankan Analisis</strong>.</p>
          </div>
        </div>
      </section>

      <!-- Tab 3: Session Report -->
      <section class="sl-tab-pane" id="sl-tab-report">
        <div class="sl-notebook-sheet">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px; flex-wrap: wrap; gap: 10px;">
            <h2>Laporan Sesi Kumulatif</h2>
            <div style="display: flex; gap: 8px;">
              <button class="sl-btn sl-btn-primary" id="sl-btn-compile-gc">&#9879; Sintesis Kesimpulan Umum (Base R)</button>
              <button class="sl-btn" id="sl-btn-download-pdf">&#128229; Unduh PDF Vektor</button>
            </div>
          </div>

          <div id="sl-gc-box" class="sl-interpretation-box" style="margin-bottom: 24px;">
            <h3 style="margin-top: 0; color: var(--sl-ink);">Kesimpulan Umum</h3>
            <p id="sl-gc-text" style="color: var(--sl-ink-subtle); font-style: italic;">
              Belum ada kesimpulan umum. Klik <strong>Sintesis Kesimpulan Umum</strong> untuk merangkum seluruh analisis di sesi ini secara otomatis melalui Base R.
            </p>
          </div>

          <div id="sl-report-list"></div>
        </div>
      </section>
    </main>

    <div class="sl-toast-container"></div>
  `;

  // UI Element References
  const badgeEl = rootEl.querySelector("#sl-engine-badge");
  const projectTitleInput = rootEl.querySelector("#sl-project-title-input");
  const presetSelect = rootEl.querySelector("#sl-preset-select");
  const decimalSelect = rootEl.querySelector("#sl-decimal-select");
  const alphaSelect = rootEl.querySelector("#sl-alpha-select");
  const langSelect = rootEl.querySelector("#sl-lang-select");

  const tabBtns = rootEl.querySelectorAll(".sl-tab-btn");
  const tabPanes = rootEl.querySelectorAll(".sl-tab-pane");
  const reportCountBadge = rootEl.querySelector("#sl-report-count");

  const gridContainer = rootEl.querySelector("#sl-grid-container");
  const roleContainer = rootEl.querySelector("#sl-role-container");
  const resultContainer = rootEl.querySelector("#sl-result-container");
  const reportListContainer = rootEl.querySelector("#sl-report-list");
  const gcTextEl = rootEl.querySelector("#sl-gc-text");

  const btnRun = rootEl.querySelector("#sl-btn-run-analysis");
  const btnCompileGc = rootEl.querySelector("#sl-btn-compile-gc");
  const btnDownloadPdf = rootEl.querySelector("#sl-btn-download-pdf");
  const btnExportJson = rootEl.querySelector("#sl-btn-export-json");
  const btnImportJson = rootEl.querySelector("#sl-btn-import-json");
  const fileImportInput = rootEl.querySelector("#sl-file-import-json");

  // State
  let currentResult = null;

  // Initialize StatLabGrid
  const initialGrid = store.getState().grid;
  const grid = new StatLabGrid(gridContainer, {
    lang: store.getState().lang,
    decimalMode: store.getState().decimalMode,
    headers: initialGrid.headers,
    rows: initialGrid.rows,
    onChange: () => {
      syncRolesWithGrid();
      store.setState({ grid: grid.getData() });
    }
  });

  // Initialize RoleMappingPanel
  const rolePanel = new RoleMappingPanel(roleContainer, {
    lang: store.getState().lang,
    analysisType: "descriptive",
    columns: grid.headers,
    onChange: () => {
      // mapping updated
    }
  });

  function syncRolesWithGrid() {
    const typed = grid.getTypedColumns();
    const colTypes = {};
    for (const [colName, vals] of Object.entries(typed.columns)) {
      const numCount = vals.filter(v => typeof v === "number" && isFinite(v)).length;
      colTypes[colName] = numCount >= vals.length / 2 ? "numeric" : "text";
    }
    rolePanel.setColumns(grid.headers, colTypes);
  }

  // Switch Tabs
  function switchTab(tabKey) {
    for (const btn of tabBtns) {
      btn.classList.toggle("active", btn.getAttribute("data-tab") === tabKey);
    }
    for (const pane of tabPanes) {
      pane.classList.toggle("active", pane.id === `sl-tab-${tabKey}`);
    }
  }

  for (const btn of tabBtns) {
    btn.onclick = () => {
      const tab = btn.getAttribute("data-tab");
      switchTab(tab);
    };
  }

  // Render Session Report Items & General Conclusion
  function updateReportView() {
    const state = store.getState();
    const lang = state.lang;
    const isEn = lang === "en";

    reportCountBadge.textContent = state.reportItems.length;

    // General conclusion text
    if (state.generalConclusion) {
      const text = state.generalConclusion[lang] || state.generalConclusion.id || state.generalConclusion.en || "";
      gcTextEl.style.fontStyle = "normal";
      gcTextEl.style.color = "var(--sl-ink)";
      gcTextEl.textContent = text;
    } else {
      gcTextEl.style.fontStyle = "italic";
      gcTextEl.style.color = "var(--sl-ink-subtle)";
      gcTextEl.textContent = isEn
        ? "No general conclusion compiled yet. Click 'Compile General Conclusion' above to synthesize findings across all analyses in Base R."
        : "Belum ada kesimpulan umum. Klik 'Sintesis Kesimpulan Umum' di atas untuk merangkum seluruh temuan analisis secara otomatis melalui Base R.";
    }

    // Report items list
    if (state.reportItems.length === 0) {
      reportListContainer.innerHTML = `
        <div style="text-align: center; color: var(--sl-ink-subtle); padding: 32px 16px;">
          <p>${isEn ? "No items in report yet. Run an analysis and click 'Add to Session Report'." : "Belum ada analisis dalam laporan sesi. Jalankan analisis lalu klik 'Tambah ke Laporan Sesi'."}</p>
        </div>
      `;
      return;
    }

    reportListContainer.innerHTML = "";
    state.reportItems.forEach((item, idx) => {
      const card = document.createElement("div");
      card.className = "sl-report-item-card";

      const title = item.customTitle || item.title || `${isEn ? "Analysis" : "Analisis"} ${idx + 1}`;
      const analysisType = item.result?.meta?.analysis_label?.[lang] || item.result?.meta?.analysis_type || "Descriptive";
      const respVar = item.result?.meta?.response_label || "-";

      card.innerHTML = `
        <div class="sl-report-item-card-header">
          <div>
            <h4 class="sl-report-item-title">${idx + 1}. ${escapeHtml(title)}</h4>
            <div style="font-size: 12px; color: var(--sl-ink-subtle); margin-top: 3px;">
              <span>${isEn ? "Method" : "Metode"}: <strong>${escapeHtml(analysisType)}</strong></span> &bull; 
              <span>${isEn ? "Variable" : "Variabel"}: <strong>${escapeHtml(respVar)}</strong></span>
            </div>
          </div>
          <div class="sl-report-item-actions">
            <button class="sl-btn sl-btn-sm btn-move-up" data-id="${item.id}" ${idx === 0 ? "disabled" : ""} title="Pindah ke Atas">&#9650;</button>
            <button class="sl-btn sl-btn-sm btn-move-down" data-id="${item.id}" ${idx === state.reportItems.length - 1 ? "disabled" : ""} title="Pindah ke Bawah">&#9660;</button>
            <button class="sl-btn sl-btn-sm sl-btn-crimson btn-remove" data-id="${item.id}" title="Hapus">&#10005;</button>
          </div>
        </div>
      `;

      card.querySelector(".btn-move-up").onclick = () => {
        store.moveReportItem(item.id, -1);
      };
      card.querySelector(".btn-move-down").onclick = () => {
        store.moveReportItem(item.id, 1);
      };
      card.querySelector(".btn-remove").onclick = () => {
        store.removeReportItem(item.id);
      };

      reportListContainer.appendChild(card);
    });
  }

  // Subscribe view update
  store.subscribe(() => {
    updateReportView();
  });

  // Handle Project Title Edit
  projectTitleInput.onchange = (e) => {
    store.setState({ projectTitle: e.target.value.trim() || "Proyek Tanpa Judul" });
  };

  // Handle Preset Selection
  presetSelect.onchange = (e) => {
    const key = e.target.value;
    if (!key || !PRESETS[key]) return;
    const p = PRESETS[key];

    grid.setData(p.headers, p.rows);
    rolePanel.setColumns(p.headers);
    if (p.roles) {
      rolePanel.mapping = {
        mode: p.roles.mode || "wide",
        response_cols: [...p.roles.response_cols],
        group_col: p.roles.group_col || null
      };
      rolePanel.render();
    }
    grid.showToast(store.getState().lang === "en" ? `Loaded preset: ${p.id}` : `Data contoh '${p.id}' berhasil dimuat.`, "success");
    e.target.value = "";
  };

  // Decimal Mode
  decimalSelect.onchange = (e) => {
    const mode = e.target.value;
    store.setState({ decimalMode: mode });
    grid.decimalMode = mode;
    grid.render();
  };

  // Alpha Selection
  alphaSelect.onchange = (e) => {
    store.setState({ alpha: parseFloat(e.target.value) });
  };

  // Language Selection
  langSelect.onchange = (e) => {
    const newLang = e.target.value;
    store.setState({ lang: newLang });
    grid.lang = newLang;
    grid.render();
    rolePanel.lang = newLang;
    rolePanel.render();
    updateReportView();
    if (currentResult) {
      renderAnalysisResult(resultContainer, currentResult, {
        lang: newLang,
        alpha: store.getState().alpha,
        onAddToReport: handleAddToReport
      });
    }
  };

  // Add to report handler
  function handleAddToReport(res) {
    const lang = store.getState().lang;
    const isEn = lang === "en";
    const promptMsg = isEn ? "Enter title for this analysis:" : "Masukkan judul untuk analisis ini:";
    const defTitle = typeof res.meta?.analysis_label === "object"
      ? (res.meta.analysis_label[lang] || res.meta.analysis_label.id)
      : (res.meta?.analysis_label || "Analisis");
    const customTitle = (typeof window !== "undefined" && window.prompt) ? window.prompt(promptMsg, defTitle) : defTitle;

    store.addReportItem(res, customTitle || defTitle);
    grid.showToast(isEn ? "Analysis added to Session Report!" : "Analisis ditambahkan ke Laporan Sesi!", "success");
  }

  // Run Analysis Handler
  btnRun.onclick = async () => {
    const mapping = rolePanel.getMapping();
    const val = rolePanel.validate();
    if (!val.valid) {
      grid.showToast(val.errors.join(". "), "crimson");
      return;
    }

    badgeEl.className = "sl-engine-badge sl-computing";
    badgeEl.innerHTML = "&#9679; Menghitung (Base R)...";
    btnRun.disabled = true;

    try {
      const typed = grid.getTypedColumns();
      const state = store.getState();

      const res = await engine.runAnalysis(
        "descriptive",
        typed.columns,
        mapping,
        {
          alpha: state.alpha,
          exclude_rows: grid.getExcludedRows(),
          include_plots: true,
          lang: state.lang
        }
      );

      currentResult = res;

      // Update outlier flags on grid without deleting data
      if (res.outliers?.flagged) {
        grid.setOutlierFlags(res.outliers.flagged);
      }

      // Render Result View
      renderAnalysisResult(resultContainer, res, {
        lang: state.lang,
        alpha: state.alpha,
        onAddToReport: handleAddToReport,
        onRerun: async (excludedRows) => {
          for (const r of excludedRows) {
            grid.toggleExcludeRow(r, true);
          }
          btnRun.click();
        }
      });

      // Switch to Result Tab
      switchTab("result");
      grid.showToast(state.lang === "en" ? "Analysis complete!" : "Analisis berhasil dijalankan!", "success");
    } catch (err) {
      console.error("Analysis execution error:", err);
      grid.showToast(`Error: ${err.message}`, "crimson");
    } finally {
      badgeEl.className = "sl-engine-badge sl-ready";
      badgeEl.innerHTML = "&#9679; webR Ready";
      btnRun.disabled = false;
    }
  };

  // Compile General Conclusion
  btnCompileGc.onclick = async () => {
    const payload = store.getGeneralConclusionPayload();
    if (payload.length === 0) {
      grid.showToast(
        store.getState().lang === "en"
          ? "No analyses in report to compile."
          : "Belum ada analisis dalam laporan sesi.",
        "crimson"
      );
      return;
    }

    btnCompileGc.disabled = true;
    badgeEl.className = "sl-engine-badge sl-computing";
    badgeEl.innerHTML = "&#9679; Sintesis Kesimpulan (R)...";

    try {
      const gc = await engine.compileGeneralConclusion(payload, ["id", "en"]);
      store.setGeneralConclusion(gc);
      grid.showToast(
        store.getState().lang === "en"
          ? "General Conclusion compiled by Base R!"
          : "Kesimpulan Umum berhasil dikompilasi oleh Base R!",
        "success"
      );
    } catch (err) {
      console.error("General conclusion compilation error:", err);
      grid.showToast(`Error: ${err.message}`, "crimson");
    } finally {
      btnCompileGc.disabled = false;
      badgeEl.className = "sl-engine-badge sl-ready";
      badgeEl.innerHTML = "&#9679; webR Ready";
    }
  };

  // Download PDF
  btnDownloadPdf.onclick = () => {
    try {
      downloadSessionPdf(store.getState(), { lang: store.getState().lang, includeAppendix: true });
    } catch (err) {
      console.error("PDF export error:", err);
      grid.showToast(`PDF Error: ${err.message}`, "crimson");
    }
  };

  // Export .statlab.json
  btnExportJson.onclick = () => {
    const jsonStr = store.exportProjectJson();
    const title = (store.getState().projectTitle || "statlab_project")
      .toLowerCase()
      .replace(/[^a-z0-9_-]/g, "_");
    const filename = `${title}.statlab.json`;

    if (typeof document !== "undefined") {
      const blob = new Blob([jsonStr], { type: "application/json" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      setTimeout(() => {
        a.remove();
        URL.revokeObjectURL(url);
      }, 100);
    }
  };

  // Import .statlab.json
  btnImportJson.onclick = () => {
    fileImportInput.value = "";
    fileImportInput.click();
  };

  fileImportInput.onchange = (e) => {
    const file = e.target.files?.[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (evt) => {
      try {
        const text = evt.target.result;
        store.importProjectJson(text);

        // Update UI controls
        const newState = store.getState();
        projectTitleInput.value = newState.projectTitle;
        langSelect.value = newState.lang;
        decimalSelect.value = newState.decimalMode;
        alphaSelect.value = String(newState.alpha);

        // Restore grid
        if (newState.grid) {
          grid.setData(newState.grid.headers, newState.grid.rows);
          rolePanel.setColumns(newState.grid.headers);
        }

        grid.showToast("Proyek .statlab.json berhasil dimuat!", "success");
      } catch (err) {
        console.error("Import failed:", err);
        grid.showToast(`Gagal mengimpor: ${err.message}`, "crimson");
      }
    };
    reader.readAsText(file);
  };

  // Initial webR engine warmup
  engine.init((prog) => {
    if (badgeEl) {
      badgeEl.innerHTML = `&#9679; ${escapeHtml(prog.message || "webR...")}`;
    }
  }).then(() => {
    if (badgeEl) {
      badgeEl.className = "sl-engine-badge sl-ready";
      badgeEl.innerHTML = "&#9679; webR Ready";
    }
  }).catch((err) => {
    console.error("webR initialization error:", err);
    if (badgeEl) {
      badgeEl.className = "sl-engine-badge sl-computing";
      badgeEl.innerHTML = "&#9888; webR Error";
    }
  });

  // Sync initial roles
  syncRolesWithGrid();
  updateReportView();

  return {
    store,
    engine,
    grid,
    rolePanel
  };
}
