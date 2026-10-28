// js/app.js
// StatLab Main Application Shell & PWA Controller
// Mounts the complete Lab Notebook UI, orchestrates webR engine, StatLabGrid,
// RoleMappingPanel, Result View, SessionStore, and PDF Export.
// Provides Aqua Tools-style Analysis Selector, Template Previews, and One-Click CSV Copying.

import { t, SUPPORTED_LANGS } from "./i18n.js";
import { parseCellValue, serializeTableDelimited } from "./number-locale.js";
import { StatLabEngine } from "./webr-client.js";
import { StatLabGrid } from "./grid.js";
import { RoleMappingPanel } from "./roles.js";
import { renderAnalysisResult, escapeHtml } from "./result-view.js";
import { SessionStore } from "./session-store.js";
import { buildSessionPdfDocDefinition, downloadSessionPdf } from "./pdf-export.js";

/**
 * Phase 1 Built-in Presets (Maintained for strict backward compatibility)
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
 * Complete Analysis Templates & Example Dataset Library (Aqua Tools Model)
 */
export const ANALYSIS_TEMPLATES = {
  descriptive: {
    id: "descriptive",
    name: "Statistik Deskriptif & Uji Asumsi Wajib",
    category: "Statistika Dasar & Asumsi",
    formatMode: "Format Lebar (Tiap Kolom = Parameter)",
    shortDesc: "Tiap kolom mewakili parameter air numerik. Menghitung mean, SD, SE, median, IQR, skewness, kurtosis, uji normalitas (Shapiro-Wilk + Lilliefors) & pencilan (Grubbs/IQR).",
    headers: ["DO_mgL", "pH", "Suhu_C", "Salinitas_ppt", "Amonia_mgL"],
    rows: [
      ["6.5", "7.8", "29.2", "22.5", "0.08"],
      ["6.2", "7.6", "29.5", "22.0", "0.12"],
      ["5.9", "7.9", "28.8", "23.0", "0.15"],
      ["6.8", "8.1", "29.0", "22.8", "0.05"],
      ["6.4", "7.7", "29.4", "22.2", "0.09"],
      ["6.1", "7.5", "29.8", "21.9", "0.18"],
      ["5.8", "8.0", "28.5", "23.2", "0.45"], // Spiked outlier
      ["6.7", "8.2", "29.1", "22.6", "0.07"],
      ["6.3", "7.8", "29.6", "22.4", "0.11"],
      ["6.0", "7.6", "28.9", "22.1", "0.14"]
    ],
    roles: { mode: "wide", response_cols: ["DO_mgL", "pH", "Suhu_C", "Salinitas_ppt", "Amonia_mgL"], group_col: null },
    analysisType: "descriptive"
  },
  oneway_anova: {
    id: "oneway_anova",
    name: "ANOVA Satu Arah & Post Hoc (Uji Pakan Ikan)",
    category: "Analisis Varians (ANOVA) & Post Hoc",
    formatMode: "Format Lebar (Tiap Kolom = Kelompok Perlakuan)",
    shortDesc: "Kolom 1 adalah Kontrol, kolom berikutnya adalah kelompok perlakuan pakan. Membandingkan rata-rata ≥ 3 kelompok dengan uji Post Hoc (Tukey/Duncan).",
    headers: ["Kontrol", "Pakan_Artemia", "Pakan_Pelet_A", "Pakan_Pelet_B"],
    rows: [
      ["45.2", "55.4", "48.9", "50.1"],
      ["42.8", "58.1", "46.2", "52.3"],
      ["46.1", "54.9", "49.5", "49.8"],
      ["44.0", "56.2", "47.8", "51.5"],
      ["43.5", "59.0", "48.1", "53.0"],
      ["45.9", "53.5", "46.9", "50.9"],
      ["44.8", "57.3", "49.0", "52.1"],
      ["42.1", "55.0", "47.2", "48.9"],
      ["46.5", "58.6", "48.5", "51.8"],
      ["43.9", "56.8", "47.9", "50.5"]
    ],
    roles: { mode: "wide", response_cols: ["Kontrol", "Pakan_Artemia", "Pakan_Pelet_A", "Pakan_Pelet_B"], group_col: null },
    analysisType: "descriptive"
  },
  independent_t: {
    id: "independent_t",
    name: "Uji-t Dua Sampel Independen (Probiotik vs Kontrol)",
    category: "Uji Komparasi Rata-rata",
    formatMode: "Format Lebar (2 Kolom Kelompok Independen)",
    shortDesc: "Dua kolom terpisah untuk dua kelompok perlakuan berbeda. Menguji kesamaan varians (Levene) dan perbedaan rata-rata (Student's / Welch t-test).",
    headers: ["Kontrol", "Perlakuan_Probiotik"],
    rows: [
      ["12.4", "15.2"],
      ["11.8", "14.8"],
      ["12.1", "15.5"],
      ["12.9", "15.0"],
      ["11.5", "14.3"],
      ["12.0", "15.8"],
      ["12.3", "14.9"],
      ["11.7", "15.1"],
      ["12.5", "14.7"],
      ["11.9", "15.3"]
    ],
    roles: { mode: "wide", response_cols: ["Kontrol", "Perlakuan_Probiotik"], group_col: null },
    analysisType: "descriptive"
  },
  paired_t: {
    id: "paired_t",
    name: "Uji-t Berpasangan (Sebelum vs Sesudah Perlakuan)",
    category: "Uji Komparasi Rata-rata",
    formatMode: "Format Lebar (2 Kolom Pasangan Pengukuran)",
    shortDesc: "Dua kolom pengukuran berpasangan pada subjek yang sama sebelum dan sesudah perlakuan (pre-test vs post-test).",
    headers: ["Sebelum_Perlakuan", "Sesudah_Perlakuan"],
    rows: [
      ["10.2", "12.5"],
      ["9.8", "12.1"],
      ["10.5", "13.0"],
      ["10.0", "12.8"],
      ["10.3", "12.6"],
      ["9.9", "12.4"],
      ["10.1", "12.9"],
      ["10.4", "13.2"]
    ],
    roles: { mode: "wide", response_cols: ["Sebelum_Perlakuan", "Sesudah_Perlakuan"], group_col: null },
    analysisType: "descriptive"
  },
  twoway_anova: {
    id: "twoway_anova",
    name: "ANOVA Dua Arah Faktorial (Suhu & Salinitas)",
    category: "Analisis Varians (ANOVA) & Post Hoc",
    formatMode: "Format Panjang (2 Kolom Faktor + 1 Kolom Respon)",
    shortDesc: "Dua kolom faktor kategorik (Faktor A dan Faktor B) dan satu kolom respon numerik untuk menguji pengaruh utama serta efek interaksi.",
    headers: ["Suhu", "Salinitas", "Laju_Pertumbuhan"],
    rows: [
      ["Rendah", "15_ppt", "1.8"],
      ["Rendah", "15_ppt", "2.0"],
      ["Rendah", "15_ppt", "1.9"],
      ["Rendah", "30_ppt", "2.5"],
      ["Rendah", "30_ppt", "2.3"],
      ["Rendah", "30_ppt", "2.4"],
      ["Tinggi", "15_ppt", "3.2"],
      ["Tinggi", "15_ppt", "3.4"],
      ["Tinggi", "15_ppt", "3.1"],
      ["Tinggi", "30_ppt", "3.9"],
      ["Tinggi", "30_ppt", "4.1"],
      ["Tinggi", "30_ppt", "3.8"]
    ],
    roles: { mode: "long", response_cols: ["Laju_Pertumbuhan"], group_col: "Suhu" },
    analysisType: "descriptive"
  },
  regression: {
    id: "regression",
    name: "Regresi Linear & Korelasi (Salinitas vs Sintasan)",
    category: "Regresi, Korelasi & Asosiasi",
    formatMode: "Format Lebar (Kolom X Prediktor & Kolom Y Respon)",
    shortDesc: "Kolom 1 adalah variabel independen (X), kolom 2 adalah variabel dependen (Y). Menghitung korelasi Pearson/Spearman dan garis regresi.",
    headers: ["Salinitas_ppt", "Sintasan_Persen"],
    rows: [
      ["5", "95.0"],
      ["10", "92.5"],
      ["15", "88.0"],
      ["20", "84.5"],
      ["25", "78.0"],
      ["30", "71.5"],
      ["35", "64.0"],
      ["40", "52.0"]
    ],
    roles: { mode: "wide", response_cols: ["Salinitas_ppt", "Sintasan_Persen"], group_col: null },
    analysisType: "descriptive"
  },
  chisquare_indep: {
    id: "chisquare_indep",
    name: "Uji Chi-Square Independensi (Prevalensi Penyakit)",
    category: "Uji Non-Parametrik & Kontingensi",
    formatMode: "Format Tabel Kontingensi (Frekuensi Cacah)",
    shortDesc: "Tabel kontingensi 2 arah. Tiap baris dan kolom berisi data frekuensi cacah (counts) untuk menguji independensi antar kategori.",
    headers: ["Kolam_A", "Kolam_B", "Kolam_C"],
    rows: [
      ["45", "30", "52"],
      ["5", "15", "8"]
    ],
    roles: { mode: "wide", response_cols: ["Kolam_A", "Kolam_B", "Kolam_C"], group_col: null },
    analysisType: "descriptive"
  },
  repeated_measures: {
    id: "repeated_measures",
    name: "Repeated Measures ANOVA (Pertumbuhan Waktu)",
    category: "Analisis Varians (ANOVA) & Post Hoc",
    formatMode: "Format Pengukuran Berulang (Subjek x Waktu)",
    shortDesc: "Kolom pertama ID subjek/ikan, kolom berikutnya adalah waktu observasi berulang. Menguji efek waktu intra-subjek.",
    headers: ["Ikan_ID", "Minggu_0", "Minggu_2", "Minggu_4", "Minggu_6"],
    rows: [
      ["Ikan_1", "10.2", "15.4", "22.1", "30.5"],
      ["Ikan_2", "11.0", "16.1", "23.0", "31.2"],
      ["Ikan_3", "10.5", "15.8", "22.5", "30.8"],
      ["Ikan_4", "9.8", "14.9", "21.6", "29.9"],
      ["Ikan_5", "10.7", "16.0", "22.8", "31.0"],
      ["Ikan_6", "11.2", "16.5", "23.4", "32.0"]
    ],
    roles: { mode: "wide", response_cols: ["Minggu_0", "Minggu_2", "Minggu_4", "Minggu_6"], group_col: null },
    analysisType: "descriptive"
  },
  manova: {
    id: "manova",
    name: "MANOVA (Multivariate ANOVA: Bobot, Panjang & Biomassa)",
    category: "Multivariat & Survival (Roadmap Phase 3)",
    formatMode: "Format Panjang (1 Kolom Faktor + ≥ 2 Kolom Respon)",
    shortDesc: "Format panjang dengan 1 kolom faktor perlakuan dan ≥ 2 kolom respon numerik simultan (uji Pillai, Wilks, Hotelling).",
    headers: ["Perlakuan", "Bobot_g", "Panjang_cm", "Biomassa_g"],
    rows: [
      ["Kontrol", "12.4", "10.2", "124.0"],
      ["Kontrol", "11.8", "9.9", "118.0"],
      ["Kontrol", "12.1", "10.1", "121.0"],
      ["Pakan_A", "15.2", "11.5", "152.0"],
      ["Pakan_A", "14.8", "11.2", "148.0"],
      ["Pakan_A", "15.5", "11.6", "155.0"],
      ["Pakan_B", "16.8", "12.1", "168.0"],
      ["Pakan_B", "16.2", "11.9", "162.0"],
      ["Pakan_B", "16.5", "12.0", "165.0"]
    ],
    roles: { mode: "long", response_cols: ["Bobot_g", "Panjang_cm", "Biomassa_g"], group_col: "Perlakuan" },
    analysisType: "descriptive"
  },
  kaplan_meier: {
    id: "kaplan_meier",
    name: "Survival Analysis Kaplan-Meier (Uji Tantang Vibriosis)",
    category: "Multivariat & Survival (Roadmap Phase 3)",
    formatMode: "Format Waktu-ke-Kejadian (Time, Status, Group)",
    shortDesc: "Kolom 1 waktu kejadian, kolom 2 status (1 = mati/event, 0 = sensor/selamat), kolom 3 kelompok perlakuan untuk uji Log-rank.",
    headers: ["Waktu_Jam", "Status_Sensor", "Perlakuan"],
    rows: [
      ["12", "1", "Kontrol"],
      ["24", "1", "Kontrol"],
      ["36", "1", "Kontrol"],
      ["48", "1", "Kontrol"],
      ["60", "0", "Kontrol"],
      ["72", "0", "Kontrol"],
      ["24", "1", "Probiotik"],
      ["48", "1", "Probiotik"],
      ["72", "0", "Probiotik"],
      ["72", "0", "Probiotik"],
      ["72", "0", "Probiotik"],
      ["72", "0", "Probiotik"]
    ],
    roles: { mode: "long", response_cols: ["Waktu_Jam", "Status_Sensor"], group_col: "Perlakuan" },
    analysisType: "descriptive"
  },
  pca_hca: {
    id: "pca_hca",
    name: "Analisis Multivariat (PCA, HCA & K-Means Clustering)",
    category: "Multivariat & Survival (Roadmap Phase 3)",
    formatMode: "Matriks Multivariat (Sampel x Variabel Kontinyu)",
    shortDesc: "Setiap baris adalah sampel kolam pengamatan, setiap kolom adalah variabel kualitas air terukur untuk reduksi dimensi dan dendrogram.",
    headers: ["Sampel_Kolam", "DO", "pH", "Salinitas", "Alkalinitas", "TAN"],
    rows: [
      ["Kolam_01", "6.5", "7.8", "22.5", "120", "0.08"],
      ["Kolam_02", "6.2", "7.6", "22.0", "115", "0.12"],
      ["Kolam_03", "5.9", "7.9", "23.0", "110", "0.15"],
      ["Kolam_04", "6.8", "8.1", "22.8", "125", "0.05"],
      ["Kolam_05", "5.2", "7.4", "24.5", "100", "0.35"],
      ["Kolam_06", "5.0", "7.3", "25.0", "95", "0.42"],
      ["Kolam_07", "6.4", "7.7", "22.2", "118", "0.09"],
      ["Kolam_08", "6.1", "7.5", "21.9", "112", "0.18"]
    ],
    roles: { mode: "wide", response_cols: ["DO", "pH", "Salinitas", "Alkalinitas", "TAN"], group_col: null },
    analysisType: "descriptive"
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

  // Render Shell with Aqua Tools-style Two-Column Workspace Layout
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

        <select class="sl-select" id="sl-lang-select" title="Bahasa">
          <option value="id">Bahasa Indonesia</option>
          <option value="en">English</option>
        </select>
      </div>
    </header>

    <main class="sl-notebook-container">
      <div class="sl-tabs">
        <button class="sl-tab-btn active" data-tab="grid">
          <span>📊</span> <span class="sl-tab-label-grid">Lembar Kerja &amp; Analisis</span>
        </button>
        <button class="sl-tab-btn" data-tab="report">
          <span>📖</span> <span class="sl-tab-label-report">Laporan Sesi Kumulatif</span>
          <span class="sl-counter-pill" id="sl-report-count">0</span>
        </button>
      </div>

      <!-- Tab 1: Aqua Tools-style Two-Column Workspace -->
      <section class="sl-tab-pane active" id="sl-tab-grid">
        <div class="sl-workspace-grid">

          <!-- Left Column: Workspace Control (Aqua Tools Style) -->
          <aside class="sl-control-panel">

            <!-- Card 1: Pilih Analisis & Template Guide -->
            <div class="sl-panel-card">
              <h3 class="sl-panel-card-title">
                <span>🎯</span> <span>Pilih Jenis Analisis</span>
              </h3>
              <div class="sl-form-group">
                <label for="sl-analysis-select" style="font-size:12px;font-weight:600;color:var(--sl-ink);margin-bottom:4px;display:block;">Tujuan Uji Statistik:</label>
                <select class="sl-select" id="sl-analysis-select" style="width:100%;font-size:13px;padding:6px 10px;">
                  <optgroup label="Statistika Dasar &amp; Asumsi">
                    <option value="descriptive" selected>Statistik Deskriptif &amp; Uji Asumsi</option>
                  </optgroup>
                  <optgroup label="Uji Komparasi Rata-rata">
                    <option value="independent_t">Uji-t Dua Sampel Independen</option>
                    <option value="paired_t">Uji-t Berpasangan (Sebelum vs Sesudah)</option>
                  </optgroup>
                  <optgroup label="Analisis Varians (ANOVA) &amp; Post Hoc">
                    <option value="oneway_anova">ANOVA Satu Arah &amp; Post Hoc (Tukey/Duncan)</option>
                    <option value="twoway_anova">ANOVA Dua Arah (Faktorial)</option>
                    <option value="repeated_measures">Repeated Measures ANOVA</option>
                  </optgroup>
                  <optgroup label="Regresi, Korelasi &amp; Asosiasi">
                    <option value="regression">Regresi Linear &amp; Korelasi</option>
                    <option value="chisquare_indep">Uji Chi-Square Independensi</option>
                  </optgroup>
                  <optgroup label="Multivariat &amp; Survival (Roadmap Phase 3)">
                    <option value="manova">MANOVA (Multivariate ANOVA)</option>
                    <option value="kaplan_meier">Survival Analysis (Kaplan-Meier)</option>
                    <option value="pca_hca">PCA, HCA &amp; Clustering</option>
                  </optgroup>
                </select>
              </div>

              <!-- Template Format Box (Dynamic Acuan CSV) -->
              <div id="sl-template-guide-box"></div>
            </div>

            <!-- Card 2: Pengaturan & Asumsi -->
            <div class="sl-panel-card">
              <h3 class="sl-panel-card-title">
                <span>⚙️</span> <span>Pengaturan &amp; Asumsi</span>
              </h3>
              <div class="sl-form-group" style="margin-bottom:10px;">
                <label style="font-size:12px;font-weight:600;color:var(--sl-ink);display:block;margin-bottom:4px;">Tingkat Signifikansi (&alpha;):</label>
                <select class="sl-select" id="sl-alpha-select" style="width:100%;">
                  <option value="0.05">&alpha; = 0.05 (Keyakinan 95%)</option>
                  <option value="0.01">&alpha; = 0.01 (Keyakinan 99%)</option>
                  <option value="0.10">&alpha; = 0.10 (Keyakinan 90%)</option>
                </select>
              </div>
              <div class="sl-form-group" style="margin-bottom:12px;">
                <label style="font-size:12px;font-weight:600;color:var(--sl-ink);display:block;margin-bottom:4px;">Format Angka / Desimal:</label>
                <select class="sl-select" id="sl-decimal-select" style="width:100%;">
                  <option value="auto">Otomatis (Deteksi CSV / Excel)</option>
                  <option value="id_comma">Koma (ID: 12,5 / Pemisah ;)</option>
                  <option value="en_dot">Titik (EN: 12.5 / Pemisah ,)</option>
                </select>
              </div>
              <div style="background:#FFFDF8;border:1px solid var(--sl-border);border-radius:4px;padding:8px 10px;font-size:12px;color:var(--sl-ink-subtle);">
                <label style="display:flex;align-items:center;gap:6px;cursor:pointer;">
                  <input type="checkbox" id="sl-chk-assumptions" checked disabled />
                  <span style="font-weight:600;color:var(--sl-ink);">Uji Asumsi Wajib Terlebih Dahulu</span>
                </label>
                <div style="font-size:11px;margin-top:2px;line-height:1.3;">
                  Normalitas (Shapiro-Wilk + Lilliefors) &amp; Homogenitas (Levene BF) diuji sebelum hasil akhir.
                </div>
              </div>
            </div>

            <!-- Action Buttons (Aqua Tools Style) -->
            <div style="display:flex;flex-direction:column;gap:8px;">
              <button class="sl-btn sl-btn-aqua sl-btn-block" id="sl-btn-run-analysis" style="padding:11px 16px;font-size:14px;letter-spacing:0.3px;">
                <span>▶</span> <span>Jalankan Uji Statistik</span>
              </button>
              <button class="sl-btn sl-btn-block" id="sl-btn-export-quick-pdf" style="padding:9px 16px;font-size:13px;">
                <span>📄</span> <span>Ekspor Laporan PDF</span>
              </button>
            </div>
          </aside>

          <!-- Right Column: Data Grid, Variable Roles & Report Results Area -->
          <section class="sl-workspace-area">
            <!-- Grid Sheet -->
            <div class="sl-notebook-sheet" style="margin-bottom:20px;">
              <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;flex-wrap:wrap;gap:8px;">
                <h2 style="margin:0;font-size:16px;display:flex;align-items:center;gap:8px;">
                  <span>📋</span> <span>Lembar Kerja Data (Spreadsheet)</span>
                </h2>
                <div style="display:flex;gap:6px;flex-wrap:wrap;">
                  <button class="sl-btn sl-btn-sm" id="sl-btn-import-csv" title="Impor data dari berkas CSV / TSV">📁 Impor CSV</button>
                  <input type="file" id="sl-file-import-csv" accept=".csv,.tsv,.txt" style="display:none;">
                  <button class="sl-btn sl-btn-sm" id="sl-btn-copy-template-shortcut" title="Salin format CSV contoh untuk analisa ini">📋 Salin Format Acuan</button>
                  <button class="sl-btn sl-btn-sm" id="sl-btn-export-json" title="Simpan proyek">💾 Simpan Proyek</button>
                  <button class="sl-btn sl-btn-sm" id="sl-btn-import-json" title="Buka proyek">📂 Buka Proyek</button>
                  <input type="file" id="sl-file-import-json" accept=".json,.statlab.json" style="display:none;">
                </div>
              </div>

              <!-- StatLabGrid Container -->
              <div id="sl-grid-container"></div>

              <!-- RoleMappingPanel Container -->
              <div id="sl-role-container" style="margin-top:16px;"></div>
            </div>

            <!-- Analysis Report Container (Aqua Tools Style) -->
            <div id="sl-result-container">
              <div class="sl-notebook-sheet" style="text-align:center;color:var(--sl-ink-subtle);padding:44px 20px;">
                <div style="font-size:36px;margin-bottom:12px;">📈</div>
                <h3 style="margin:0 0 6px 0;color:var(--sl-ink);">Area Laporan Ilmiah Terstandarisasi</h3>
                <p style="font-size:13px;max-width:520px;margin:0 auto;line-height:1.5;">
                  Pilih jenis pengujian pada panel kiri atau klik <strong>"Muat Contoh ke Tabel"</strong>, lalu klik tombol <strong>"Jalankan Uji Statistik"</strong> untuk memunculkan analisis asumsi, deteksi pencilan, tabel APA jurnal, rincian SPSS, grafik 300-DPI, dan interpretasi tertulis secara otomatis.
                </p>
              </div>
            </div>
          </section>
        </div>
      </section>

      <!-- Tab 2: Cumulative Session Report -->
      <section class="sl-tab-pane" id="sl-tab-report">
        <div class="sl-notebook-sheet">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px; flex-wrap: wrap; gap: 10px;">
            <h2>Laporan Sesi Kumulatif</h2>
            <div style="display: flex; gap: 8px;">
              <button class="sl-btn sl-btn-primary" id="sl-btn-compile-gc">⚗️ Sintesis Kesimpulan Umum (Base R)</button>
              <button class="sl-btn" id="sl-btn-download-pdf">📥 Unduh PDF Vektor</button>
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
  const analysisSelect = rootEl.querySelector("#sl-analysis-select");
  const templateGuideBox = rootEl.querySelector("#sl-template-guide-box");
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
  const btnExportQuickPdf = rootEl.querySelector("#sl-btn-export-quick-pdf");
  const btnCompileGc = rootEl.querySelector("#sl-btn-compile-gc");
  const btnDownloadPdf = rootEl.querySelector("#sl-btn-download-pdf");
  const btnExportJson = rootEl.querySelector("#sl-btn-export-json");
  const btnImportJson = rootEl.querySelector("#sl-btn-import-json");
  const fileImportInput = rootEl.querySelector("#sl-file-import-json");
  const btnImportCsv = rootEl.querySelector("#sl-btn-import-csv");
  const fileImportCsv = rootEl.querySelector("#sl-file-import-csv");
  const btnCopyTemplateShortcut = rootEl.querySelector("#sl-btn-copy-template-shortcut");

  // State
  let currentResult = null;
  let activeTemplateKey = "descriptive";

  // Initialize StatLabGrid with active template initial data
  const initialTpl = ANALYSIS_TEMPLATES.descriptive;
  const grid = new StatLabGrid(gridContainer, {
    lang: store.getState().lang,
    decimalMode: store.getState().decimalMode,
    headers: initialTpl.headers,
    rows: initialTpl.rows,
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

  // Clipboard helper
  async function copyTextToClipboard(text) {
    if (typeof navigator !== "undefined" && navigator.clipboard && navigator.clipboard.writeText) {
      await navigator.clipboard.writeText(text);
      return true;
    }
    if (typeof document !== "undefined") {
      const ta = document.createElement("textarea");
      ta.value = text;
      ta.style.position = "fixed";
      ta.style.opacity = "0";
      document.body.appendChild(ta);
      ta.select();
      document.execCommand("copy");
      ta.remove();
      return true;
    }
    return false;
  }

  // File download helper
  function downloadTextFile(filename, content, mime = "text/csv;charset=utf-8;") {
    if (typeof document === "undefined") return;
    const blob = new Blob(["\uFEFF" + content], { type: mime });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      a.remove();
      URL.revokeObjectURL(url);
    }, 2000);
  }

  // Render Template Guide Box (Aqua Tools Style)
  function renderTemplateGuide(key) {
    const tpl = ANALYSIS_TEMPLATES[key] || ANALYSIS_TEMPLATES.descriptive;
    activeTemplateKey = tpl.id;

    const isId = store.getState().lang === "id";
    const copyLabel = isId ? "📋 Salin Format CSV Contoh" : "📋 Copy Example CSV Format";
    const downloadLabel = isId ? "📥 Unduh CSV" : "📥 Download CSV";
    const applyLabel = isId ? "🔄 Muat ke Tabel" : "🔄 Load to Grid";

    templateGuideBox.innerHTML = `
      <div class="sl-template-card">
        <div class="sl-template-header">${escapeHtml(tpl.name)}</div>
        <div class="sl-template-badge">${escapeHtml(tpl.formatMode)}</div>
        <div class="sl-template-desc">${escapeHtml(tpl.shortDesc)}</div>

        <div style="font-size:11px;font-weight:600;color:var(--sl-ink);margin-bottom:4px;">Contoh Format Kolom &amp; Baris:</div>
        <div class="sl-template-preview-box">
          <table class="sl-template-preview-table">
            <thead>
              <tr>${tpl.headers.map(h => `<th>${escapeHtml(h)}</th>`).join("")}</tr>
            </thead>
            <tbody>
              ${tpl.rows.slice(0, 3).map(r => `<tr>${r.map(c => `<td>${escapeHtml(c)}</td>`).join("")}</tr>`).join("")}
            </tbody>
          </table>
        </div>

        <div style="display:flex;flex-direction:column;gap:6px;">
          <button type="button" class="sl-btn sl-btn-sm sl-btn-aqua" id="sl-btn-copy-template" title="Salin teks CSV contoh ke clipboard">
            <span>📋</span> <span>${copyLabel}</span>
          </button>
          <div style="display:flex;gap:6px;">
            <button type="button" class="sl-btn sl-btn-sm" id="sl-btn-download-template" style="flex:1;" title="Unduh berkas .csv acuan">
              <span>📥</span> <span>${downloadLabel}</span>
            </button>
            <button type="button" class="sl-btn sl-btn-sm sl-btn-primary" id="sl-btn-load-template" style="flex:1.2;" title="Terapkan dataset contoh ini langsung ke spreadsheet">
              <span>🔄</span> <span>${applyLabel}</span>
            </button>
          </div>
        </div>
      </div>
    `;

    // Attach guide button events
    const btnCopy = templateGuideBox.querySelector("#sl-btn-copy-template");
    const btnDown = templateGuideBox.querySelector("#sl-btn-download-template");
    const btnLoad = templateGuideBox.querySelector("#sl-btn-load-template");

    if (btnCopy) {
      btnCopy.onclick = async () => {
        const lang = store.getState().lang;
        const csvText = serializeTableDelimited(tpl.headers, tpl.rows, lang, "csv");
        await copyTextToClipboard(csvText);
        grid.showToast(
          isId
            ? `Format CSV contoh untuk ${tpl.name} berhasil disalin! Anda bisa menempelkannya (Ctrl+V) ke Excel atau Notepad sebagai acuan.`
            : `Example CSV format for ${tpl.name} copied! Paste it (Ctrl+V) into Excel or Notepad as reference.`,
          "success"
        );
      };
    }

    if (btnDown) {
      btnDown.onclick = () => {
        const lang = store.getState().lang;
        const csvText = serializeTableDelimited(tpl.headers, tpl.rows, lang, "csv");
        downloadTextFile(`format_acuan_${tpl.id}.csv`, csvText);
        grid.showToast(
          isId ? `Berkas format_acuan_${tpl.id}.csv sedang diunduh!` : `Downloading format_acuan_${tpl.id}.csv!`,
          "info"
        );
      };
    }

    if (btnLoad) {
      btnLoad.onclick = () => {
        grid.setData(tpl.headers, tpl.rows);
        syncRolesWithGrid();
        if (tpl.roles) {
          rolePanel.setMapping(tpl.roles);
        }
        grid.showToast(
          isId
            ? `Data contoh ${tpl.name} berhasil dimuat ke lembar kerja!`
            : `Example data for ${tpl.name} loaded into the grid!`,
          "success"
        );
      };
    }
  }

  // Handle Analysis Selection Change
  analysisSelect.onchange = (e) => {
    const key = e.target.value;
    renderTemplateGuide(key);
  };

  // Shortcut button in spreadsheet toolbar
  if (btnCopyTemplateShortcut) {
    btnCopyTemplateShortcut.onclick = async () => {
      const tpl = ANALYSIS_TEMPLATES[activeTemplateKey] || ANALYSIS_TEMPLATES.descriptive;
      const lang = store.getState().lang;
      const isId = lang === "id";
      const csvText = serializeTableDelimited(tpl.headers, tpl.rows, lang, "csv");
      await copyTextToClipboard(csvText);
      grid.showToast(
        isId
          ? `Format CSV acuan untuk ${tpl.name} berhasil disalin! Tempelkan (Ctrl+V) ke Excel sebagai template.`
          : `Reference CSV format for ${tpl.name} copied to clipboard!`,
        "success"
      );
    };
  }

  // CSV File Import Handler
  if (btnImportCsv && fileImportCsv) {
    btnImportCsv.onclick = () => {
      fileImportCsv.value = "";
      fileImportCsv.click();
    };

    fileImportCsv.onchange = (e) => {
      const file = e.target.files?.[0];
      if (!file) return;

      const reader = new FileReader();
      reader.onload = (evt) => {
        try {
          const text = evt.target.result;
          const decMode = store.getState().decimalMode;
          const detected = grid.loadDelimitedText(text, decMode);
          syncRolesWithGrid();
          const isId = store.getState().lang === "id";
          grid.showToast(
            isId
              ? `CSV berhasil diimpor! Pemisah: '${detected.delimiter}', Desimal: '${detected.decimalMark}'.`
              : `CSV imported successfully! Delimiter: '${detected.delimiter}', Decimal: '${detected.decimalMark}'.`,
            "success"
          );
        } catch (err) {
          console.error("CSV import error:", err);
          grid.showToast(`Gagal mengimpor CSV: ${err.message}`, "crimson");
        }
      };
      reader.readAsText(file);
    };
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
            <button class="sl-btn sl-btn-sm sl-btn-report-up" data-id="${item.id}" title="Pindah ke atas" ${idx === 0 ? "disabled" : ""}>&uarr;</button>
            <button class="sl-btn sl-btn-sm sl-btn-report-down" data-id="${item.id}" title="Pindah ke bawah" ${idx === state.reportItems.length - 1 ? "disabled" : ""}>&darr;</button>
            <button class="sl-btn sl-btn-sm sl-btn-report-del" data-id="${item.id}" title="Hapus dari laporan" style="color:var(--sl-crimson);">&times;</button>
          </div>
        </div>
      `;

      card.querySelector(".sl-btn-report-up")?.addEventListener("click", () => {
        store.moveReportItem(item.id, -1);
      });
      card.querySelector(".sl-btn-report-down")?.addEventListener("click", () => {
        store.moveReportItem(item.id, 1);
      });
      card.querySelector(".sl-btn-report-del")?.addEventListener("click", () => {
        store.removeReportItem(item.id);
      });

      reportListContainer.appendChild(card);
    });
  }

  // Subscribe to SessionStore
  store.subscribe(() => {
    updateReportView();
  });

  // Project title changes
  projectTitleInput.onchange = (e) => {
    store.setState({ projectTitle: e.target.value.trim() || defaultTitle });
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
    renderTemplateGuide(activeTemplateKey);
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
    let customTitle = defTitle;
    try {
      if (typeof window !== "undefined" && typeof window.prompt === "function") {
        customTitle = window.prompt(promptMsg, defTitle) || defTitle;
      }
    } catch {
      customTitle = defTitle;
    }

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

      // Render Result View directly in the results area below the grid
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

      // Smooth scroll to results area
      resultContainer.scrollIntoView({ behavior: "smooth", block: "start" });
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

  // Quick PDF Export from Left Panel
  if (btnExportQuickPdf) {
    btnExportQuickPdf.onclick = () => {
      btnDownloadPdf.click();
    };
  }

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

  // Initial render of template guide
  renderTemplateGuide("descriptive");

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
