// js/result-view.js
// StatLab Result View Component
// Strictly renders analysis results in standard order (§6.1):
// 1. Meta info & KaTeX Formula card
// 2. Assumption Checks banner & diagnostics (Normality + Homogeneity + Q-Q plot preview)
// 3. Outlier Alert banner (opt-in exclusion checkboxes + Re-run button)
// 4. Layer 1 APA Journal Table (Copy-to-Word/Excel & Export-CSV buttons)
// 5. Collapsible <details> Layer 2 SPSS Full Tables
// 6. R Base Graphics Plot Gallery (300-DPI PNG download)
// 7. Bilingual Written Interpretation + "Add to Session Report" trigger
//
// XSS Safety: all user-supplied text (headers, group names) is escaped.
// Zero statistical computations in JS.

import { serializeTableDelimited } from "./number-locale.js";
import { t } from "./i18n.js";

/**
 * Escape HTML to prevent XSS
 * @param {any} str Input
 * @returns {string} Sanitized string
 */
export function escapeHtml(str) {
  if (str === null || str === undefined) return "";
  return String(str)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

/**
 * Trigger text/blob download in browser
 * @param {string} filename Name of file
 * @param {string} content Text content
 * @param {string} mime Mime type
 */
function downloadFile(filename, content, mime = "text/csv;charset=utf-8;") {
  if (typeof document === "undefined") return;
  const blob = new Blob([content], { type: mime });
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

/**
 * Copy text to clipboard
 * @param {string} text Text to copy
 */
async function copyToClipboard(text) {
  if (typeof navigator !== "undefined" && navigator.clipboard && navigator.clipboard.writeText) {
    try {
      await navigator.clipboard.writeText(text);
      return true;
    } catch {
      // Fallback below
    }
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

/**
 * Render complete statistical analysis result into container
 *
 * @param {HTMLElement | object} containerEl Container element or mock
 * @param {object} result Structured analysis output from R sl_dispatch
 * @param {object} options Configuration options
 */
export function renderAnalysisResult(containerEl, result, options = {}) {
  if (!containerEl || !result) return;

  const lang = options.lang === "en" ? "en" : "id";
  const isId = lang === "id";

  const meta = result.meta || {};
  const assumptions = result.assumptions || {};
  const outliers = result.outliers || {};
  const formulas = Array.isArray(result.formulas) ? result.formulas : [];
  const tablesApa = Array.isArray(result.tables_apa) ? result.tables_apa : [];
  const tablesFull = Array.isArray(result.tables_full) ? result.tables_full : [];
  const plots = Array.isArray(result.plots) ? result.plots : [];
  const interpretation = result.interpretation || {};

  let html = `<div class="sl-result-view">`;

  // =========================================================================
  // Section 1: Meta info & KaTeX Formula Card
  // =========================================================================
  const analysisTitle = meta.analysis_name || meta.analysis_type || (isId ? "Hasil Analisis" : "Analysis Result");
  const respVars = Array.isArray(meta.response_vars) ? meta.response_vars.map(escapeHtml).join(", ") : (escapeHtml(meta.response_vars) || "-");
  const grpVar = meta.group_var ? escapeHtml(meta.group_var) : (isId ? "(Format Lebar / Tanpa Faktor)" : "(Wide Format / No Factor)");
  const alphaVal = meta.alpha !== undefined ? meta.alpha : 0.05;

  html += `
    <div class="sl-meta-box">
      <div class="sl-meta-item"><strong>${isId ? "Uji:" : "Test:"}</strong> ${escapeHtml(analysisTitle)}</div>
      <div class="sl-meta-item"><strong>${isId ? "Variabel Respon:" : "Response:"}</strong> ${respVars}</div>
      <div class="sl-meta-item"><strong>${isId ? "Faktor / Kelompok:" : "Factor / Group:"}</strong> ${grpVar}</div>
      <div class="sl-meta-item"><strong>&alpha;:</strong> ${alphaVal}</div>
      <div class="sl-meta-item"><strong>n:</strong> ${meta.n_valid_obs || 0} ${isId ? "valid" : "valid"} (${meta.n_missing_obs || 0} ${isId ? "hilang" : "missing"})</div>
      ${meta.n_excluded_obs > 0 ? `<div class="sl-meta-item" style="color:var(--sl-crimson);"><strong>${isId ? "Dikeluarkan:" : "Excluded:"}</strong> ${meta.n_excluded_obs}</div>` : ""}
    </div>
  `;

  if (formulas.length > 0) {
    html += `<div class="sl-formula-box">`;
    for (let i = 0; i < formulas.length; i++) {
      const f = formulas[i];
      const fLabel = f.label ? (f.label[lang] || f.label.id || f.label.en || "") : "";
      html += `
        <div class="sl-formula-item" data-formula-index="${i}" style="margin-bottom:10px;">
          ${fLabel ? `<div class="sl-formula-title">${escapeHtml(fLabel)}</div>` : ""}
          <div class="sl-formula-latex" data-latex="${escapeHtml(f.latex)}">
            <span class="sl-formula-raw">$$ ${escapeHtml(f.latex)} $$</span>
          </div>
        </div>
      `;
    }
    html += `</div>`;
  }

  // =========================================================================
  // Section 2: Assumption Checks Banner & Diagnostics
  // =========================================================================
  const isNormal = assumptions.overall_normal !== false;
  const isHomo = assumptions.overall_homo !== false;
  const assumptionsPassed = isNormal && isHomo;

  const bannerClass = assumptionsPassed ? "sl-passed" : "sl-violated";
  const bannerTitle = assumptionsPassed
    ? (isId ? "Asumsi Terpenuhi (Normalitas & Homogenitas)" : "Assumptions Met (Normality & Homogeneity)")
    : (isId ? "Peringatan Pelanggaran Asumsi Statistika" : "Statistical Assumption Violation Warning");

  html += `
    <div class="sl-assumptions-banner ${bannerClass}">
      <div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:8px;">
        <h4 style="margin:0;color:inherit;">${escapeHtml(bannerTitle)}</h4>
        <span class="sl-badge" style="font-size:11.5px;font-weight:600;padding:2px 8px;border-radius:12px;background:${assumptionsPassed ? 'var(--sl-success)' : 'var(--sl-warning)'};color:#FFF;">
          ${assumptionsPassed ? (isId ? "Lolos" : "Passed") : (isId ? "Perhatian" : "Warning")}
        </span>
      </div>
      <div style="font-size:13px;line-height:1.5;">
  `;

  // Normality Details per group
  if (Array.isArray(assumptions.per_group) && assumptions.per_group.length > 0) {
    html += `<div style="margin-bottom:6px;"><strong>${isId ? "Uji Normalitas (Shapiro-Wilk):" : "Normality (Shapiro-Wilk):"}</strong></div>`;
    html += `<ul style="margin:4px 0 10px 18px;padding:0;">`;
    for (const pg of assumptions.per_group) {
      const gName = escapeHtml(pg.group || "All");
      const sw = pg.shapiro || {};
      const lf = pg.lilliefors || {};
      const grpNormal = pg.is_normal;
      const statusText = grpNormal === true
        ? (isId ? "Normal" : "Normal")
        : (grpNormal === false ? (isId ? "Tidak Normal" : "Non-normal") : "-");
      const statW = sw.valid && sw.statistic !== undefined ? sw.statistic.toFixed(4) : "-";
      const pValW = sw.valid && sw.p_value !== undefined ? (sw.p_value < 0.001 ? "< 0,001" : sw.p_value.toFixed(4)) : "-";

      html += `
        <li style="margin-bottom:3px;">
          <strong>${gName}</strong> (n = ${pg.n}): W = ${statW}, p = ${pValW} &mdash;
          <span style="font-weight:600;color:${grpNormal ? 'var(--sl-success)' : 'var(--sl-crimson)'};">${statusText}</span>
        </li>
      `;
    }
    html += `</ul>`;
  }

  // Homogeneity Details
  if (assumptions.homogeneity && assumptions.homogeneity.levene_bf) {
    const lev = assumptions.homogeneity.levene_bf;
    const bart = assumptions.homogeneity.bartlett || {};
    if (lev.valid) {
      const levStat = lev.statistic !== undefined ? lev.statistic.toFixed(4) : "-";
      const levP = lev.p_value !== undefined ? (lev.p_value < 0.001 ? "< 0,001" : lev.p_value.toFixed(4)) : "-";
      const homoText = lev.is_homo
        ? (isId ? "Homogen" : "Homogeneous")
        : (isId ? "Tidak Homogen (Heterogen)" : "Heterogeneous");
      html += `
        <div>
          <strong>${isId ? "Uji Homogenitas Varians (Levene Brown-Forsythe):" : "Homogeneity of Variance (Levene BF):"}</strong>
          F = ${levStat}, p = ${levP} &mdash;
          <span style="font-weight:600;color:${lev.is_homo ? 'var(--sl-success)' : 'var(--sl-crimson)'};">${homoText}</span>
        </div>
      `;
    }
  }

  // Q-Q Plot Preview (if present)
  const qqPlot = plots.find(p => p.id === "qq_plot" || (p.title && (p.title.id?.includes("Q-Q") || p.title.en?.includes("Q-Q"))));
  if (qqPlot && qqPlot.png_data_url) {
    html += `
      <div style="margin-top:10px;display:flex;align-items:center;gap:12px;">
        <span style="font-size:12px;color:var(--sl-ink-light);">${isId ? "Pratinjau Q-Q Plot:" : "Q-Q Plot Preview:"}</span>
        <img src="${qqPlot.png_data_url}" alt="Q-Q Plot Preview" style="height:48px;border:1px solid var(--sl-border);border-radius:3px;cursor:pointer;" class="sl-qq-preview-img" />
      </div>
    `;
  }

  html += `</div></div>`;

  // =========================================================================
  // Section 3: Outlier Alert Banner (if outliers exist)
  // =========================================================================
  const flaggedOutliers = Array.isArray(outliers.flagged) ? outliers.flagged : [];
  if (flaggedOutliers.length > 0) {
    html += `
      <div class="sl-outlier-alert">
        <div style="display:flex;align-items:center;justify-content:space-between;">
          <h4 style="margin:0;color:var(--sl-crimson);">${isId ? "Perhatian: Data Pencilan Terdeteksi" : "Warning: Outliers Flagged"}</h4>
          <span class="sl-badge" style="background:var(--sl-crimson);color:#FFF;padding:2px 8px;border-radius:10px;font-size:12px;font-weight:bold;">
            ${flaggedOutliers.length} ${isId ? "pencilan" : "outlier(s)"}
          </span>
        </div>
        <p style="margin:8px 0 12px 0;font-size:13px;">
          ${isId
            ? "Pencilan ditandai secara non-destruktif dan TIDAK dihapus otomatis. Untuk mengabaikan pencilan dari analisis, centang baris di bawah ini lalu klik tombol Hitung Ulang."
            : "Outliers are flagged non-destructively and are NOT automatically removed. To exclude outliers from computation, select rows below and click Re-run Analysis."
          }
        </p>
        <table class="sl-outlier-table">
          <thead>
            <tr>
              <th style="width:40px;text-align:center;">${isId ? "Pilih" : "Select"}</th>
              <th>${isId ? "Baris #" : "Row #"}</th>
              <th>${isId ? "Kelompok" : "Group"}</th>
              <th>${isId ? "Nilai" : "Value"}</th>
              <th>${isId ? "Kriteria Terdeteksi" : "Detection Method"}</th>
            </tr>
          </thead>
          <tbody>
    `;

    for (const fl of flaggedOutliers) {
      const srcRow = fl.source_row || fl.row;
      const grp = escapeHtml(fl.group || "-");
      const val = fl.value !== undefined ? fl.value : "-";
      const reasons = [];
      if (fl.by_iqr) reasons.push("1.5\u00d7IQR Fence");
      if (fl.by_grubbs) reasons.push("Grubbs Test");
      const reasonStr = escapeHtml(reasons.join(", ") || "Outlier");

      html += `
        <tr>
          <td style="text-align:center;">
            <input type="checkbox" class="sl-outlier-exclude-chk" data-row="${srcRow}" />
          </td>
          <td><strong>#${srcRow}</strong></td>
          <td>${grp}</td>
          <td class="sl-mono">${val}</td>
          <td>${reasonStr}</td>
        </tr>
      `;
    }

    html += `
          </tbody>
        </table>
        <div style="margin-top:12px;">
          <button type="button" class="sl-btn sl-btn-crimson sl-btn-rerun">
            ${isId ? "Hitung Ulang Tanpa Baris Terpilih" : "Re-run Without Selected Rows"}
          </button>
        </div>
      </div>
    `;
  }

  // =========================================================================
  // Section 4: Layer 1 APA Journal Table
  // =========================================================================
  if (tablesApa.length > 0) {
    for (let i = 0; i < tablesApa.length; i++) {
      const tbl = tablesApa[i];
      const tTitle = tbl.title ? (tbl.title[lang] || tbl.title.id || tbl.title.en || "") : "";
      const headers = (tbl.headers && (tbl.headers[lang] || tbl.headers.id || tbl.headers.en)) || [];
      const rows = (tbl.rows && (tbl.rows[lang] || tbl.rows.id || tbl.rows.en)) || [];
      const note = tbl.note ? (tbl.note[lang] || tbl.note.id || tbl.note.en || "") : "";

      html += `
        <div class="sl-apa-table-wrapper" data-table-id="${tbl.id || i}">
          <div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:8px;">
            <h3 style="margin:0;">${escapeHtml(tTitle)}</h3>
            <div style="display:flex;gap:6px;">
              <button type="button" class="sl-btn sl-btn-sm sl-btn-copy-apa" data-table-index="${i}">
                ${isId ? "Salin ke Excel/Word" : "Copy to Word/Excel"}
              </button>
              <button type="button" class="sl-btn sl-btn-sm sl-btn-export-csv-apa" data-table-index="${i}">
                ${isId ? "Ekspor CSV" : "Export CSV"}
              </button>
            </div>
          </div>
          <table class="sl-apa-table">
            <thead>
              <tr>
      `;

      for (const h of headers) {
        html += `<th>${escapeHtml(h)}</th>`;
      }
      html += `</tr></thead><tbody>`;

      for (const r of rows) {
        html += `<tr>`;
        for (const cell of r) {
          html += `<td>${escapeHtml(cell)}</td>`;
        }
        html += `</tr>`;
      }
      html += `</tbody></table>`;

      if (note) {
        html += `<div class="sl-apa-note">${escapeHtml(note)}</div>`;
      }
      html += `</div>`;
    }
  }

  // =========================================================================
  // Section 5: Collapsible <details> Layer 2 SPSS Full Tables
  // =========================================================================
  if (tablesFull.length > 0) {
    html += `
      <details class="sl-spss-details">
        <summary>${isId ? "Lampiran Tabel Lengkap Gaya SPSS (Klik untuk Membuka)" : "SPSS-Style Complete Tables (Click to Expand)"}</summary>
        <div class="sl-spss-container" style="margin-top:16px;">
    `;

    for (let i = 0; i < tablesFull.length; i++) {
      const tbl = tablesFull[i];
      const tTitle = tbl.title ? (tbl.title[lang] || tbl.title.id || tbl.title.en || "") : "";
      const headers = (tbl.headers && (tbl.headers[lang] || tbl.headers.id || tbl.headers.en)) || [];
      const rows = (tbl.rows && (tbl.rows[lang] || tbl.rows.id || tbl.rows.en)) || [];
      const note = tbl.note ? (tbl.note[lang] || tbl.note.id || tbl.note.en || "") : "";

      html += `
        <div class="sl-spss-table-wrapper" style="margin-bottom:24px;" data-table-index="${i}">
          <div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:6px;">
            <h4 style="margin:0;">${escapeHtml(tTitle)}</h4>
            <div style="display:flex;gap:6px;">
              <button type="button" class="sl-btn sl-btn-sm sl-btn-copy-spss" data-table-index="${i}">
                ${isId ? "Salin Tabel" : "Copy Table"}
              </button>
              <button type="button" class="sl-btn sl-btn-sm sl-btn-export-csv-spss" data-table-index="${i}">
                ${isId ? "Ekspor CSV" : "Export CSV"}
              </button>
            </div>
          </div>
          <table class="sl-spss-table">
            <thead><tr>
      `;
      for (const h of headers) {
        html += `<th>${escapeHtml(h)}</th>`;
      }
      html += `</tr></thead><tbody>`;

      for (const r of rows) {
        html += `<tr>`;
        for (const cell of r) {
          html += `<td>${escapeHtml(cell)}</td>`;
        }
        html += `</tr>`;
      }
      html += `</tbody></table>`;
      if (note) {
        html += `<div style="font-size:11.5px;color:var(--sl-ink-light);margin-top:4px;">${escapeHtml(note)}</div>`;
      }
      html += `</div>`;
    }

    html += `</div></details>`;
  }

  // =========================================================================
  // Section 6: R Base Graphics Plot Gallery
  // =========================================================================
  if (plots.length > 0) {
    html += `
      <div class="sl-plot-gallery-header" style="margin-top:28px;">
        <h3 style="margin-bottom:8px;">${isId ? "Galeri Grafik Base R (300 DPI)" : "Base R Plot Gallery (300 DPI)"}</h3>
      </div>
      <div class="sl-plot-gallery">
    `;

    for (let i = 0; i < plots.length; i++) {
      const p = plots[i];
      const pTitle = p.title ? (p.title[lang] || p.title.id || p.title.en || p.id || "") : "";
      const pImg = p.png_data_url || "";
      const downloadName = `${p.id || 'plot'}_300dpi.png`;

      html += `
        <div class="sl-plot-card" data-plot-id="${escapeHtml(p.id || i)}">
          <h4 style="margin-top:0;margin-bottom:10px;font-size:14px;">${escapeHtml(pTitle)}</h4>
          ${pImg ? `<img src="${pImg}" alt="${escapeHtml(pTitle)}" loading="lazy" />` : ""}
          <div style="margin-top:12px;display:flex;justify-content:flex-end;">
            <a href="${pImg}" download="${downloadName}" class="sl-btn sl-btn-sm sl-btn-primary" style="text-decoration:none;">
              ${isId ? "Unduh PNG 300 DPI" : "Download 300 DPI PNG"}
            </a>
          </div>
        </div>
      `;
    }

    html += `</div>`;
  }

  // =========================================================================
  // Section 7: Bilingual Written Interpretation & "Add to Session Report"
  // =========================================================================
  const interpText = interpretation[lang] || interpretation.id || interpretation.en || "";

  html += `
    <div class="sl-interpretation-box">
      <h3 style="margin-top:0;margin-bottom:10px;">${isId ? "Kesimpulan & Interpretasi Biostatistika" : "Biostatistical Interpretation"}</h3>
      <div class="sl-interpretation-text">${escapeHtml(interpText)}</div>
      <div style="margin-top:18px;display:flex;justify-content:flex-start;">
        <button type="button" class="sl-btn sl-btn-primary sl-btn-add-report">
          ${isId ? "+ Tambah ke Laporan Sesi" : "+ Add to Session Report"}
        </button>
      </div>
    </div>
  `;

  html += `</div>`;
  containerEl.innerHTML = html;

  // Safely render KaTeX formulas if KaTeX is present in global window
  if (typeof window !== "undefined" && window.katex && typeof window.katex.render === "function") {
    try {
      const formulaLatexEls = containerEl.querySelectorAll
        ? containerEl.querySelectorAll(".sl-formula-latex")
        : [];
      for (const el of formulaLatexEls) {
        const latex = el.getAttribute("data-latex");
        if (latex) {
          window.katex.render(latex, el, { throwOnError: false, displayMode: true });
        }
      }
    } catch {
      // Keep raw fallback LaTeX
    }
  }

  // Attach event handlers for Re-run, Copy, Export, Add-to-report
  _attachResultEvents(containerEl, result, options);
}

function _attachResultEvents(containerEl, result, options = {}) {
  if (!containerEl || typeof containerEl.querySelector !== "function") return;

  const lang = options.lang === "en" ? "en" : "id";
  const tablesApa = Array.isArray(result.tables_apa) ? result.tables_apa : [];
  const tablesFull = Array.isArray(result.tables_full) ? result.tables_full : [];

  // Re-run Button
  const rerunBtn = containerEl.querySelector(".sl-btn-rerun");
  if (rerunBtn) {
    rerunBtn.onclick = () => {
      const chks = containerEl.querySelectorAll(".sl-outlier-exclude-chk:checked");
      const excluded = [];
      for (const c of chks) {
        const r = parseInt(c.getAttribute("data-row"), 10);
        if (Number.isInteger(r)) excluded.push(r);
      }
      if (typeof options.onRerun === "function") {
        options.onRerun(excluded);
      }
    };
  }

  // Add to Session Report
  const addReportBtn = containerEl.querySelector(".sl-btn-add-report");
  if (addReportBtn) {
    addReportBtn.onclick = () => {
      if (typeof options.onAddToReport === "function") {
        options.onAddToReport(result);
      }
    };
  }

  // Copy APA Table
  const copyApaBtns = containerEl.querySelectorAll(".sl-btn-copy-apa");
  for (const btn of copyApaBtns) {
    btn.onclick = async (e) => {
      const idx = parseInt(e.currentTarget.getAttribute("data-table-index"), 10);
      const tbl = tablesApa[idx];
      if (tbl) {
        const headers = tbl.headers[lang] || tbl.headers.id || tbl.headers.en || [];
        const rows = tbl.rows[lang] || tbl.rows.id || tbl.rows.en || [];
        const tsv = serializeTableDelimited(headers, rows, lang, "tsv");
        const ok = await copyToClipboard(tsv);
        if (ok) {
          const orig = e.currentTarget.textContent;
          e.currentTarget.textContent = lang === "id" ? "Tersalin!" : "Copied!";
          setTimeout(() => { e.currentTarget.textContent = orig; }, 2000);
        }
      }
    };
  }

  // Export CSV APA Table
  const exportApaBtns = containerEl.querySelectorAll(".sl-btn-export-csv-apa");
  for (const btn of exportApaBtns) {
    btn.onclick = (e) => {
      const idx = parseInt(e.currentTarget.getAttribute("data-table-index"), 10);
      const tbl = tablesApa[idx];
      if (tbl) {
        const headers = tbl.headers[lang] || tbl.headers.id || tbl.headers.en || [];
        const rows = tbl.rows[lang] || tbl.rows.id || tbl.rows.en || [];
        const csv = serializeTableDelimited(headers, rows, lang, "csv");
        downloadFile(`${tbl.id || 'table'}_apa.csv`, csv);
      }
    };
  }

  // Copy SPSS Table
  const copySpssBtns = containerEl.querySelectorAll(".sl-btn-copy-spss");
  for (const btn of copySpssBtns) {
    btn.onclick = async (e) => {
      const idx = parseInt(e.currentTarget.getAttribute("data-table-index"), 10);
      const tbl = tablesFull[idx];
      if (tbl) {
        const headers = tbl.headers[lang] || tbl.headers.id || tbl.headers.en || [];
        const rows = tbl.rows[lang] || tbl.rows.id || tbl.rows.en || [];
        const tsv = serializeTableDelimited(headers, rows, lang, "tsv");
        const ok = await copyToClipboard(tsv);
        if (ok) {
          const orig = e.currentTarget.textContent;
          e.currentTarget.textContent = lang === "id" ? "Tersalin!" : "Copied!";
          setTimeout(() => { e.currentTarget.textContent = orig; }, 2000);
        }
      }
    };
  }

  // Export CSV SPSS Table
  const exportSpssBtns = containerEl.querySelectorAll(".sl-btn-export-csv-spss");
  for (const btn of exportSpssBtns) {
    btn.onclick = (e) => {
      const idx = parseInt(e.currentTarget.getAttribute("data-table-index"), 10);
      const tbl = tablesFull[idx];
      if (tbl) {
        const headers = tbl.headers[lang] || tbl.headers.id || tbl.headers.en || [];
        const rows = tbl.rows[lang] || tbl.rows.id || tbl.rows.en || [];
        const csv = serializeTableDelimited(headers, rows, lang, "csv");
        downloadFile(`${tbl.id || 'table'}_spss.csv`, csv);
      }
    };
  }
}
