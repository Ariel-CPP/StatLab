// js/pdf-export.js
// StatLab Vector PDF Report Generator using pdfmake
// Generates publication-ready Lab Notebook PDF reports with APA Layer-1 tables,
// embedded 300-DPI R base graphics plots, written interpretations, and cumulative General Conclusion.

/**
 * Build a pdfmake docDefinition object from the cumulative SessionStore state.
 * Dual-use across Node.js tests and browser environments.
 *
 * @param {object} sessionState SessionStore state snapshot
 * @param {object} [options={}] Export options
 * @param {string} [options.lang="id"] "id" or "en"
 * @param {boolean} [options.includeAppendix=false] If true, includes Layer-2 full tables
 * @returns {object} Valid pdfmake docDefinition object
 */
export function buildSessionPdfDocDefinition(sessionState, options = {}) {
  const lang = (options.lang || sessionState.lang || "id").toLowerCase();
  const isEn = lang === "en";
  const includeAppendix = Boolean(options.includeAppendix);

  const projectTitle = sessionState.projectTitle || (isEn ? "StatLab Biostatistics Report" : "Laporan Analisis Biostatistika StatLab");
  const alphaVal = typeof sessionState.alpha === "number" ? sessionState.alpha : 0.05;
  const items = Array.isArray(sessionState.reportItems) ? sessionState.reportItems : [];

  // Determine R version from items or fallback
  let rVersion = "4.5.2";
  for (const it of items) {
    if (it.result?.meta?.r_version) {
      rVersion = it.result.meta.r_version;
      break;
    }
  }

  const dateStr = new Date().toLocaleDateString(isEn ? "en-US" : "id-ID", {
    year: "numeric",
    month: "long",
    day: "numeric"
  });

  const content = [];

  // --- 1. Document Header / Cover ---
  content.push({
    columns: [
      {
        width: "*",
        stack: [
          { text: projectTitle, style: "docTitle" },
          {
            text: isEn
              ? "StatLab Biostatistics Notebook \u2014 Pure Base R In-Browser Computation"
              : "Buku Catatan Laboratorium Biostatistika StatLab \u2014 Komputasi Base R Murni",
            style: "docSubtitle"
          }
        ]
      },
      {
        width: "auto",
        stack: [
          { text: "StatLab", style: "brandLogo", alignment: "right" },
          { text: `R v${rVersion}`, style: "versionBadge", alignment: "right" }
        ]
      }
    ]
  });

  // Metadata ribbon
  content.push({
    table: {
      widths: ["*", "*", "*", "*"],
      body: [
        [
          { text: `${isEn ? "Date" : "Tanggal"}: ${dateStr}`, style: "metaText" },
          { text: `${isEn ? "Significance" : "Tingkat Signifikansi"}: \u03b1 = ${alphaVal}`, style: "metaText" },
          { text: `${isEn ? "Analyses" : "Total Analisis"}: ${items.length}`, style: "metaText" },
          { text: `${isEn ? "Language" : "Bahasa"}: ${lang.toUpperCase()}`, style: "metaText" }
        ]
      ]
    },
    layout: {
      fillColor: () => "#F3F4F6",
      hLineWidth: () => 0,
      vLineWidth: () => 0,
      paddingLeft: () => 8,
      paddingRight: () => 8,
      paddingTop: () => 5,
      paddingBottom: () => 5
    },
    margin: [0, 8, 0, 16]
  });

  // Divider
  content.push({
    canvas: [
      {
        type: "line",
        x1: 0,
        y1: 0,
        x2: 515,
        y2: 0,
        lineWidth: 1.5,
        lineColor: "#0D5C75"
      }
    ],
    margin: [0, 0, 0, 14]
  });

  // --- 2. Sections per Report Item ---
  if (items.length === 0) {
    content.push({
      text: isEn
        ? "No analyses have been added to this session report yet."
        : "Belum ada analisis yang ditambahkan ke dalam laporan sesi ini.",
      italics: true,
      color: "#6B7280",
      margin: [0, 10, 0, 15]
    });
  }

  items.forEach((item, idx) => {
    const res = item.result || {};
    const meta = res.meta || {};
    const assumptions = res.assumptions || {};
    const outliers = res.outliers || {};

    const itemTitle = item.customTitle || item.title || `${isEn ? "Analysis" : "Analisis"} ${idx + 1}`;
    const analysisLabel = typeof meta.analysis_label === "object"
      ? (meta.analysis_label[lang] || meta.analysis_label.id)
      : (meta.analysis_label || meta.analysis_type || "Descriptive");
    const respLabel = meta.response_label || "-";

    // Section header
    content.push({
      text: `${idx + 1}. ${itemTitle}`,
      style: "sectionHeading",
      pageBreak: idx > 0 ? "before" : undefined
    });

    // Sub-header method info
    content.push({
      text: `${isEn ? "Method" : "Metode"}: ${analysisLabel}  |  ${isEn ? "Variable" : "Variabel"}: ${respLabel}`,
      style: "methodSubtitle"
    });

    // Assumption & Diagnostic Badge Box
    const normViolated = Boolean(assumptions.normality_violated);
    const homViolated = Boolean(assumptions.homogeneity_violated);
    const flaggedOutliers = Array.isArray(outliers.flagged) ? outliers.flagged.length : 0;

    const normText = normViolated
      ? (isEn ? "Normality: Violated (Non-parametric recommended)" : "Normalitas: Dilanggar (Non-parametrik dianjurkan)")
      : (isEn ? "Normality: Satisfied" : "Normalitas: Terpenuhi");

    const homText = homViolated
      ? (isEn ? "Homogeneity: Violated" : "Homogenitas: Dilanggar")
      : (isEn ? "Homogeneity: Satisfied" : "Homogenitas: Terpenuhi");

    const outText = flaggedOutliers > 0
      ? (isEn ? `Outliers: ${flaggedOutliers} flagged (retained)` : `Pencilan: ${flaggedOutliers} ditandai (dipertahankan)`)
      : (isEn ? "Outliers: None detected" : "Pencilan: Tidak ada");

    content.push({
      table: {
        widths: ["*", "*", "*"],
        body: [
          [
            { text: normText, color: normViolated ? "#B91C1C" : "#047857", bold: true, fontSize: 8 },
            { text: homText, color: homViolated ? "#B91C1C" : "#047857", bold: true, fontSize: 8 },
            { text: outText, color: flaggedOutliers > 0 ? "#D97706" : "#4B5563", fontSize: 8 }
          ]
        ]
      },
      layout: {
        fillColor: () => "#F9FAFB",
        hLineWidth: () => 1,
        vLineWidth: () => 0,
        hLineColor: () => "#E5E7EB",
        paddingLeft: () => 6,
        paddingRight: () => 6,
        paddingTop: () => 4,
        paddingBottom: () => 4
      },
      margin: [0, 4, 0, 10]
    });

    // APA Tables (Layer 1)
    const tablesToRender = [];
    if (Array.isArray(res.tables_apa)) {
      tablesToRender.push(...res.tables_apa);
    }
    if (includeAppendix && Array.isArray(res.tables_full)) {
      tablesToRender.push(...res.tables_full);
    }

    tablesToRender.forEach((tbl, tIdx) => {
      const tblTitle = typeof tbl.title === "object" ? (tbl.title[lang] || tbl.title.id) : (tbl.title || `Table ${tIdx + 1}`);
      const headers = Array.isArray(tbl.headers) ? tbl.headers : [];
      const rows = Array.isArray(tbl.rows) ? tbl.rows : [];

      if (headers.length > 0 && rows.length > 0) {
        // Table title in APA italics
        content.push({
          text: `${isEn ? "Table" : "Tabel"} ${idx + 1}.${tIdx + 1}: ${tblTitle}`,
          style: "apaTableTitle"
        });

        const colWidths = headers.map((_, hIdx) => (hIdx === 0 ? "auto" : "*"));

        content.push({
          table: {
            headerRows: 1,
            widths: colWidths,
            body: [
              headers.map(h => ({
                text: String(h ?? ""),
                bold: true,
                fontSize: 8.5,
                fillColor: "#FBF8F1"
              })),
              ...rows.map(row =>
                row.map((cell, cIdx) => ({
                  text: String(cell ?? ""),
                  fontSize: 8,
                  alignment: cIdx === 0 ? "left" : "right"
                }))
              )
            ]
          },
          layout: {
            // APA Style: only top border, underline below header, and bottom border. Zero vertical lines.
            hLineWidth: (i, node) => (i === 0 || i === 1 || i === node.table.body.length) ? 1 : 0,
            vLineWidth: () => 0,
            hLineColor: (i, node) => (i === 0 || i === node.table.body.length) ? "#1F2937" : "#4B5563",
            paddingLeft: () => 5,
            paddingRight: () => 5,
            paddingTop: () => 3,
            paddingBottom: () => 3
          },
          margin: [0, 2, 0, 4]
        });

        // Table note if available
        if (tbl.notes) {
          const noteStr = typeof tbl.notes === "object" ? (tbl.notes[lang] || tbl.notes.id) : tbl.notes;
          if (noteStr) {
            content.push({
              text: `${isEn ? "Note" : "Catatan"}. ${noteStr}`,
              style: "apaTableNote"
            });
          }
        }
      }
    });

    // 300-DPI Base R Plots
    if (Array.isArray(res.plots) && res.plots.length > 0) {
      res.plots.forEach(plot => {
        if (plot.png_data_url && plot.png_data_url.startsWith("data:image/png;base64,")) {
          const plotTitle = typeof plot.title === "object" ? (plot.title[lang] || plot.title.id) : (plot.title || plot.id);
          content.push({
            image: plot.png_data_url,
            width: 420,
            alignment: "center",
            margin: [0, 8, 0, 3]
          });
          content.push({
            text: plotTitle,
            style: "plotCaption",
            alignment: "center"
          });
        }
      });
    }

    // Written Interpretation
    const interpText = typeof res.interpretation === "object"
      ? (res.interpretation?.[lang] || res.interpretation?.id || res.interpretation?.en || "")
      : (res.interpretation || "");

    if (interpText) {
      content.push({
        text: isEn ? "Statistical Interpretation:" : "Interpretasi Statistik:",
        style: "interpretationHeading"
      });

      const paragraphs = interpText.split(/\n+/).filter(p => p.trim().length > 0);
      paragraphs.forEach(p => {
        content.push({
          text: p.trim(),
          style: "interpretationBody"
        });
      });
    }

    // Spacing between items
    content.push({ text: "", margin: [0, 0, 0, 10] });
  });

  // --- 3. Cumulative General Conclusion Section ---
  content.push({
    text: isEn ? "General Conclusion" : "Kesimpulan Umum",
    style: "conclusionHeading",
    pageBreak: items.length > 1 ? "before" : undefined
  });

  content.push({
    canvas: [
      {
        type: "line",
        x1: 0,
        y1: 0,
        x2: 515,
        y2: 0,
        lineWidth: 1.5,
        lineColor: "#1B365D"
      }
    ],
    margin: [0, 2, 0, 10]
  });

  const gcText = typeof sessionState.generalConclusion === "object" && sessionState.generalConclusion !== null
    ? (sessionState.generalConclusion[lang] || sessionState.generalConclusion.id || sessionState.generalConclusion.en || "")
    : (sessionState.generalConclusion || "");

  if (gcText) {
    const gcParagraphs = gcText.split(/\n\n+/).filter(p => p.trim().length > 0);
    gcParagraphs.forEach(p => {
      content.push({
        text: p.trim(),
        style: "conclusionBody"
      });
    });
  } else {
    content.push({
      text: isEn
        ? "No cumulative general conclusion has been compiled yet. Run general conclusion synthesis in the session report tab."
        : "Belum ada kesimpulan umum yang dikompilasi. Jalankan sintesis kesimpulan umum pada tab laporan sesi.",
      italics: true,
      color: "#6B7280",
      margin: [0, 4, 0, 8]
    });
  }

  // --- 4. Assembled pdfmake Document Definition ---
  return {
    info: {
      title: projectTitle,
      author: "StatLab Biostatistics Platform",
      subject: "Biostatistical Analysis Report",
      keywords: "biostatistics, aquaculture, R, webR, APA"
    },
    pageSize: "A4",
    pageMargins: [40, 45, 40, 45],
    defaultStyle: {
      font: "Roboto",
      fontSize: 9,
      color: "#1F2937",
      lineHeight: 1.3
    },
    styles: {
      docTitle: {
        fontSize: 18,
        bold: true,
        color: "#1B365D",
        margin: [0, 0, 0, 3]
      },
      docSubtitle: {
        fontSize: 9.5,
        italics: true,
        color: "#4B5563",
        margin: [0, 0, 0, 2]
      },
      brandLogo: {
        fontSize: 16,
        bold: true,
        color: "#0D5C75"
      },
      versionBadge: {
        fontSize: 8,
        color: "#6B7280"
      },
      metaText: {
        fontSize: 8,
        color: "#374151"
      },
      sectionHeading: {
        fontSize: 13,
        bold: true,
        color: "#1B365D",
        margin: [0, 10, 0, 2]
      },
      methodSubtitle: {
        fontSize: 9,
        color: "#0D5C75",
        bold: true,
        margin: [0, 0, 0, 6]
      },
      apaTableTitle: {
        fontSize: 9,
        italics: true,
        bold: true,
        color: "#1F2937",
        margin: [0, 8, 0, 3]
      },
      apaTableNote: {
        fontSize: 7.5,
        italics: true,
        color: "#6B7280",
        margin: [0, 2, 0, 6]
      },
      plotCaption: {
        fontSize: 8,
        italics: true,
        color: "#6B7280",
        margin: [0, 0, 0, 8]
      },
      interpretationHeading: {
        fontSize: 9.5,
        bold: true,
        color: "#0D5C75",
        margin: [0, 6, 0, 2]
      },
      interpretationBody: {
        fontSize: 8.5,
        lineHeight: 1.35,
        color: "#1F2937",
        margin: [0, 0, 0, 6]
      },
      conclusionHeading: {
        fontSize: 14,
        bold: true,
        color: "#1B365D",
        margin: [0, 14, 0, 4]
      },
      conclusionBody: {
        fontSize: 9,
        lineHeight: 1.4,
        color: "#1F2937",
        margin: [0, 0, 0, 8]
      }
    },
    footer: (currentPage, pageCount) => ({
      columns: [
        {
          text: "StatLab \u2014 Pure Base R WebAssembly Biostatistics Platform",
          fontSize: 7.5,
          color: "#9CA3AF",
          margin: [40, 0, 0, 0]
        },
        {
          text: `${isEn ? "Page" : "Halaman"} ${currentPage} / ${pageCount}`,
          alignment: "right",
          fontSize: 7.5,
          color: "#9CA3AF",
          margin: [0, 0, 40, 0]
        }
      ],
      margin: [0, 15, 0, 0]
    }),
    content
  };
}

/**
 * Trigger client-side PDF download using pdfmake.
 *
 * @param {object} sessionState SessionStore state snapshot
 * @param {object} [options={}] Export options
 */
export function downloadSessionPdf(sessionState, options = {}) {
  const docDef = buildSessionPdfDocDefinition(sessionState, options);
  const title = (sessionState.projectTitle || "statlab_report")
    .toLowerCase()
    .replace(/[^a-z0-9_-]/g, "_")
    .replace(/_+/g, "_");
  const lang = options.lang || sessionState.lang || "id";
  const filename = `${title}_${lang}.pdf`;

  const pdfMakeLib = typeof pdfMake !== "undefined"
    ? pdfMake
    : (typeof window !== "undefined" && window.pdfMake ? window.pdfMake : null);

  if (!pdfMakeLib || typeof pdfMakeLib.createPdf !== "function") {
    throw new Error("pdfMake library is not loaded. Ensure vendor/pdfmake scripts are loaded.");
  }

  pdfMakeLib.createPdf(docDef).download(filename);
}
