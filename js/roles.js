// js/roles.js
// StatLab Variable Role Panel & Analysis Schemas
// Configures variable mapping (Wide vs Long format) with smart auto-detection
// and validation against statistical requirements.

import { t } from "./i18n.js";

function escapeHtml(str) {
  if (str === null || str === undefined) return "";
  return String(str)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

export const ANALYSIS_SCHEMAS = {
  descriptive: {
    name: { id: "Statistika Deskriptif", en: "Descriptive Statistics" },
    description: {
      id: "Rerata, simpangan baku, median, IQR, uji normalitas (Shapiro-Wilk) & homogenitas (Levene BF).",
      en: "Mean, standard deviation, median, IQR, normality (Shapiro-Wilk) & homogeneity (Levene BF)."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 1,
    supportsGrouping: true
  },
  oneway_anova: {
    name: { id: "ANOVA Satu Arah", en: "One-Way ANOVA" },
    description: {
      id: "Uji perbandingan rata-rata untuk >= 2 kelompok independen.",
      en: "Comparison of means across >= 2 independent groups."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    supportsGrouping: true
  },
  independent_t: {
    name: { id: "Uji-t Dua Sampel Independen", en: "Independent Two-Sample t-Test" },
    description: {
      id: "Uji perbandingan rata-rata dua kelompok independen (Student / Welch).",
      en: "Comparison of means between two independent groups (Student / Welch)."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    maxNumericCols: 2,
    supportsGrouping: true
  },
  paired_t: {
    name: { id: "Uji-t Berpasangan", en: "Paired Samples t-Test" },
    description: {
      id: "Uji perbandingan dua pengukuran berpasangan (Sebelum vs Sesudah).",
      en: "Comparison of two paired measurements (Before vs After)."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    maxNumericCols: 2,
    supportsGrouping: false
  },
  curve_estimation: {
    name: { id: "Regresi & Estimasi Kurva", en: "Curve Estimation & Regression" },
    description: {
      id: "Estimasi kurva linier, kuadratik (titik optimum), power/alometrik (panjang-bobot), eksponensial, dan logaritmik.",
      en: "Curve estimation: linear, quadratic (optimum vertex), power/allometric (length-weight), exponential, and logarithmic."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  regression: {
    name: { id: "Regresi & Korelasi Linier", en: "Linear Regression & Correlation" },
    description: {
      id: "Analisis pengaruh linier (Y = a + bX) dan korelasi Pearson/Spearman.",
      en: "Linear regression model (Y = a + bX) and Pearson/Spearman correlation."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  kruskal_wallis: {
    name: { id: "Uji Kruskal-Wallis & Post-Hoc Dunn", en: "Kruskal-Wallis Test & Dunn Post-Hoc" },
    description: {
      id: "Padanan non-parametrik One-Way ANOVA untuk >= 2 kelompok perlakuan.",
      en: "Non-parametric counterpart to One-Way ANOVA for >= 2 treatment groups."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    supportsGrouping: true
  },
  mann_whitney: {
    name: { id: "Uji Mann-Whitney U", en: "Mann-Whitney U Test" },
    description: {
      id: "Uji beda median non-parametrik untuk 2 kelompok independen.",
      en: "Non-parametric median comparison for 2 independent groups."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    maxNumericCols: 2,
    supportsGrouping: true
  },
  wilcoxon_signed: {
    name: { id: "Uji Wilcoxon Bertanda", en: "Wilcoxon Signed-Rank Test" },
    description: {
      id: "Uji perubahan non-parametrik sampel berpasangan (Sebelum vs Sesudah).",
      en: "Non-parametric paired change test (Before vs After)."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    maxNumericCols: 2,
    supportsGrouping: false
  },
  friedman: {
    name: { id: "Uji Friedman", en: "Friedman Test" },
    description: {
      id: "Uji non-parametrik pengukuran berulang / rancangan acak kelompok.",
      en: "Non-parametric repeated measures / randomized block test."
    },
    allowedModes: ["wide"],
    minNumericCols: 3,
    supportsGrouping: false
  },
  repeated_measures: {
    name: { id: "Repeated Measures ANOVA", en: "Repeated Measures ANOVA" },
    description: {
      id: "Pengukuran berulang melintasi waktu dengan koreksi sferisitas Greenhouse-Geisser.",
      en: "Longitudinal repeated measures with Greenhouse-Geisser sphericity adjustment."
    },
    allowedModes: ["wide"],
    minNumericCols: 3,
    supportsGrouping: false
  },
  manova: {
    name: { id: "MANOVA (Multivariate ANOVA)", en: "MANOVA (Multivariate ANOVA)" },
    description: {
      id: "Analisis varians multivariat untuk >= 2 variabel respon secara simultan.",
      en: "Multivariate analysis of variance for >= 2 response variables simultaneously."
    },
    allowedModes: ["long"],
    minNumericCols: 2,
    supportsGrouping: true
  },
  kaplan_meier: {
    name: { id: "Survival Analysis (Kaplan-Meier)", en: "Survival Analysis (Kaplan-Meier)" },
    description: {
      id: "Analisis ketahanan hidup dengan estimator Kaplan-Meier dan uji Log-Rank.",
      en: "Survival analysis with Kaplan-Meier estimator and Log-Rank test."
    },
    allowedModes: ["long"],
    minNumericCols: 1,
    supportsGrouping: true
  },
  pca_hca: {
    name: { id: "PCA, HCA & Clustering", en: "PCA, HCA & Clustering" },
    description: {
      id: "Reduksi dimensi, klasterisasi hirarki Ward.D2, K-Means, dan ANOVA profil klaster.",
      en: "Dimension reduction, Ward.D2 hierarchical clustering, K-Means, and cluster ANOVA profiles."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  chisquare_indep: {
    name: { id: "Uji Chi-Square Independensi", en: "Chi-Square Test of Independence" },
    description: {
      id: "Uji asosiasi dan independensi tabel kontingensi dua arah.",
      en: "Association and independence test for two-way contingency tables."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  rcbd: {
    name: { id: "RAKL / RCBD (Rancangan Acak Kelompok)", en: "RCBD (Randomized Complete Block Design)" },
    description: {
      id: "Rancangan acak kelompok lengkap dengan uji pengaruh perlakuan, efisiensi blok, KK, dan BNT / Tukey.",
      en: "Randomized complete block design evaluating treatment, block effects, CV, and LSD / Tukey post-hoc."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 1,
    supportsGrouping: true
  },
  split_plot: {
    name: { id: "Rancangan Petak Terbagi (Split-Plot)", en: "Split-Plot Design" },
    description: {
      id: "Rancangan petak terbagi dengan stratifikasi galat: Petak Utama (Galat a), Anak Petak & Interaksi (Galat b).",
      en: "Split-plot design with stratified error terms: Main Plot (Error a), Sub Plot & Interaction (Error b)."
    },
    allowedModes: ["long"],
    minNumericCols: 1,
    supportsGrouping: true
  },
  probit_lc50: {
    name: { id: "Bioassay & Dosis-Respon (LC50 / LD50)", en: "Bioassay & Dose-Response (LC50 / LD50)" },
    description: {
      id: "Analisis Probit / Logit regresi binomial untuk estimasi LC10, LC50, LC90 dengan 95% selang kepercayaan.",
      en: "Probit / Logit binomial regression for LC10, LC50, LC90 lethal concentration estimation with 95% CI."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  binary_logistic: {
    name: { id: "Regresi Logistik Biner", en: "Binary Logistic Regression" },
    description: {
      id: "Regresi logistik biner dengan Odds Ratio Exp(B), Hosmer-Lemeshow, Pseudo R², dan kurva ROC/AUC.",
      en: "Binary logistic regression with Odds Ratio Exp(B), Hosmer-Lemeshow, Pseudo R², and ROC/AUC curve."
    },
    allowedModes: ["wide", "long"],
    minNumericCols: 2,
    supportsGrouping: false
  },
  correlation_matrix: {
    name: { id: "Matriks Korelasi & Heatmap", en: "Correlation Matrix & Heatmap" },
    description: {
      id: "Matriks korelasi multivariat Pearson & Spearman dengan p-value dan heatmap divergensi 300-DPI.",
      en: "Multivariate Pearson & Spearman correlation matrices with p-values and 300-DPI divergence heatmap."
    },
    allowedModes: ["wide"],
    minNumericCols: 2,
    supportsGrouping: false
  }
};

export class RoleMappingPanel {
  /**
   * @param {HTMLElement | object} containerEl Container element or mock
   * @param {object} options Configuration options
   */
  constructor(containerEl, options = {}) {
    this.containerEl = containerEl;
    this.lang = options.lang || "id";
    this.analysisType = options.analysisType || "descriptive";
    this.onChange = typeof options.onChange === "function" ? options.onChange : null;

    this.columns = [];
    this.columnTypes = {};
    this.mapping = {
      mode: "wide",
      response_cols: [],
      group_col: null
    };
    this.userCustomized = false;

    if (options.columns) {
      this.setColumns(options.columns, options.columnTypes);
    } else if (this.containerEl) {
      this.render();
    }
  }

  /**
   * Set analysis type and re-validate
   * @param {string} analysisType Analysis key (e.g. "descriptive")
   */
  setAnalysisType(analysisType) {
    if (ANALYSIS_SCHEMAS[analysisType]) {
      this.analysisType = analysisType;
      this.render();
      this._notifyChange();
    }
  }

  /**
   * Set available columns and apply smart auto-detection
   * @param {string[]} columns Column names
   * @param {Record<string, "numeric" | "text">} [columnTypes] Optional column types
   */
  setColumns(columns, columnTypes = null) {
    this.columns = Array.isArray(columns) ? [...columns] : [];
    this.columnTypes = columnTypes && typeof columnTypes === "object" ? { ...columnTypes } : {};

    // Smart default auto-detection (§6.1):
    // 1. If column 1 is categorical text and column 2 is numeric -> defaults to "long"
    // 2. If >= 2 numeric columns -> defaults to "wide"
    const colCount = this.columns.length;
    if (colCount >= 2) {
      const col1 = this.columns[0];
      const col2 = this.columns[1];
      const col1Type = this.columnTypes[col1];
      const col2Type = this.columnTypes[col2];

      const numericCols = this.columns.filter(c => this.columnTypes[c] === "numeric");

      if (col1Type === "text" && col2Type === "numeric") {
        // Long mode: group_col is col 1, response_cols is exactly 1 numeric column
        const respCols = this.columns.slice(1).filter(c => this.columnTypes[c] !== "text");
        this.mapping = {
          mode: "long",
          group_col: col1,
          response_cols: respCols.length > 0 ? [respCols[0]] : [col2]
        };
      } else if (numericCols.length >= 2) {
        // Wide mode: each numeric column is a treatment group
        this.mapping = {
          mode: "wide",
          group_col: null,
          response_cols: [...numericCols]
        };
      } else {
        // Fallback wide with all or first columns
        this.mapping = {
          mode: "wide",
          group_col: null,
          response_cols: [...this.columns]
        };
      }
    } else if (colCount === 1) {
      this.mapping = {
        mode: "wide",
        group_col: null,
        response_cols: [this.columns[0]]
      };
    } else {
      this.mapping = {
        mode: "wide",
        group_col: null,
        response_cols: []
      };
    }

    this.userCustomized = false;
    this.render();
    this._notifyChange();
  }

  /**
   * Get current variable role mapping
   * @returns {{ mode: "wide" | "long", response_cols: string[], group_col: string | null }}
   */
  getMapping() {
    return {
      mode: this.mapping.mode,
      response_cols: [...this.mapping.response_cols],
      group_col: this.mapping.group_col
    };
  }

  /**
   * Explicitly set mapping
   * @param {{ mode?: "wide" | "long", response_cols?: string[], group_col?: string | null }} mapping
   */
  setMapping(mapping) {
    if (!mapping || typeof mapping !== "object") return;
    const mode = mapping.mode === "long" ? "long" : "wide";
    let respCols = Array.isArray(mapping.response_cols) ? [...mapping.response_cols] : [];
    if (mode === "long" && respCols.length > 1) {
      respCols = [respCols[0]];
    }
    this.mapping = {
      mode: mode,
      response_cols: respCols,
      group_col: mode === "long" ? (mapping.group_col || null) : null
    };
    this.userCustomized = true;
    this.render();
    this._notifyChange();
  }

  /**
   * Validate role mapping against current schema
   * @returns {{ valid: boolean, errors: string[] }}
   */
  validate() {
    const schema = ANALYSIS_SCHEMAS[this.analysisType] || ANALYSIS_SCHEMAS.descriptive;
    const errors = [];

    if (!schema.allowedModes.includes(this.mapping.mode)) {
      errors.push(`Format data '${this.mapping.mode}' tidak didukung untuk analisis ini.`);
    }

    if (!this.mapping.response_cols || this.mapping.response_cols.length < schema.minNumericCols) {
      errors.push(`Pilih minimal ${schema.minNumericCols} variabel respon numerik.`);
    }

    if (schema.maxNumericCols && this.mapping.response_cols.length > schema.maxNumericCols) {
      errors.push(`Maksimal ${schema.maxNumericCols} variabel respon untuk analisis ini.`);
    }

    if (this.mapping.mode === "long") {
      if (!this.mapping.group_col || !this.columns.includes(this.mapping.group_col)) {
        errors.push("Pilih satu kolom faktor / kelompok untuk format panjang.");
      }
      if (this.mapping.group_col && this.mapping.response_cols.includes(this.mapping.group_col)) {
        errors.push("Kolom kelompok tidak boleh dipilih sebagai variabel respon.");
      }
      if (this.mapping.response_cols.length !== 1) {
        errors.push("Pada format data panjang (Long), pilih tepat 1 kolom nilai respon numerik.");
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  _notifyChange() {
    if (this.onChange) {
      this.onChange(this.getMapping(), this.validate());
    }
  }

  /**
   * Render HTML into container element
   */
  render() {
    if (!this.containerEl) return;

    const isId = this.lang === "id";
    const schema = ANALYSIS_SCHEMAS[this.analysisType] || ANALYSIS_SCHEMAS.descriptive;
    const validation = this.validate();

    const titleText = isId ? "Peran Variabel & Format Data" : "Variable Roles & Data Format";
    const modeLabel = isId ? "Format Data:" : "Data Format:";
    const wideLabel = isId ? "Format Lebar (Tiap kolom = perlakuan/kelompok)" : "Wide Format (Each column = group)";
    const longLabel = isId ? "Format Panjang (1 kolom perlakuan + 1 kolom nilai)" : "Long Format (1 group column + 1 value column)";
    const respLabel = isId ? "Variabel Respon (Numerik):" : "Response Variable(s) (Numeric):";
    const groupLabel = isId ? "Kolom Faktor / Kelompok:" : "Factor / Grouping Column:";

    let html = `
      <div class="sl-role-panel">
        <h3 style="margin-top:0;margin-bottom:12px;">${titleText}</h3>
        
        <div class="sl-role-group">
          <label class="sl-role-label">${modeLabel}</label>
          <div class="sl-role-mode-toggle">
            <label style="cursor:pointer;display:inline-flex;align-items:center;gap:6px;">
              <input type="radio" name="sl_role_mode" value="wide" ${this.mapping.mode === "wide" ? "checked" : ""} />
              <span>${wideLabel}</span>
            </label>
            <label style="cursor:pointer;display:inline-flex;align-items:center;gap:6px;margin-left:16px;">
              <input type="radio" name="sl_role_mode" value="long" ${this.mapping.mode === "long" ? "checked" : ""} />
              <span>${longLabel}</span>
            </label>
          </div>
        </div>
    `;

    if (this.mapping.mode === "long") {
      html += `
        <div class="sl-role-group">
          <label class="sl-role-label">${groupLabel}</label>
          <select class="sl-role-select sl-role-group-col">
            <option value="">${isId ? "-- Pilih Kolom Kelompok --" : "-- Select Group Column --"}</option>
      `;
      for (const col of this.columns) {
        const isSelected = this.mapping.group_col === col;
        html += `<option value="${escapeHtml(col)}" ${isSelected ? "selected" : ""}>${escapeHtml(col)}</option>`;
      }
      html += `</select></div>`;
    }

    html += `
      <div class="sl-role-group">
        <label class="sl-role-label">${respLabel}</label>
        <div class="sl-role-resp-list" style="display:flex;flex-wrap:wrap;gap:12px;margin-top:4px;">
    `;

    const isLongMode = this.mapping.mode === "long";
    for (const col of this.columns) {
      if (isLongMode && col === this.mapping.group_col) continue;
      const isChecked = this.mapping.response_cols.includes(col);
      const inputType = isLongMode ? "radio" : "checkbox";
      const inputName = isLongMode ? "sl_role_resp_single" : `sl_role_resp_${escapeHtml(col)}`;
      html += `
        <label style="cursor:pointer;display:inline-flex;align-items:center;gap:6px;background:var(--sl-paper-dark);padding:4px 10px;border-radius:var(--sl-radius-sm);border:1px solid var(--sl-border);">
          <input type="${inputType}" name="${inputName}" class="sl-role-resp-chk" value="${escapeHtml(col)}" ${isChecked ? "checked" : ""} />
          <span>${escapeHtml(col)}</span>
        </label>
      `;
    }
    html += `</div></div>`;

    if (!validation.valid) {
      html += `
        <div class="sl-alert-banner sl-violated" style="margin-top:12px;padding:8px 12px;color:var(--sl-crimson);font-size:12.5px;">
          ${validation.errors.map(err => `<div>&bull; ${escapeHtml(err)}</div>`).join("")}
        </div>
      `;
    }

    html += `</div>`;
    this.containerEl.innerHTML = html;
    this._attachDomEvents();
  }

  _attachDomEvents() {
    if (!this.containerEl || typeof this.containerEl.querySelector !== "function") return;

    // Mode Radios
    const modeRadios = this.containerEl.querySelectorAll('input[name="sl_role_mode"]');
    for (const r of modeRadios) {
      r.onchange = (e) => {
        if (e.target.checked) {
          const newMode = e.target.value;
          this.mapping.mode = newMode;
          if (newMode === "long" && !this.mapping.group_col && this.columns.length > 0) {
            this.mapping.group_col = this.columns[0];
            const numericOther = this.columns.slice(1).filter(c => this.columnTypes[c] === "numeric");
            this.mapping.response_cols = numericOther.length > 0 ? [numericOther[0]] : (this.columns.length > 1 ? [this.columns[1]] : []);
          } else if (newMode === "wide") {
            this.mapping.group_col = null;
            this.mapping.response_cols = [...this.columns];
          }
          this.userCustomized = true;
          this.render();
          this._notifyChange();
        }
      };
    }

    // Group select
    const groupSel = this.containerEl.querySelector(".sl-role-group-col");
    if (groupSel) {
      groupSel.onchange = (e) => {
        this.mapping.group_col = e.target.value || null;
        this.userCustomized = true;
        this.render();
        this._notifyChange();
      };
    }

    // Response Checkboxes / Radios
    const respChks = this.containerEl.querySelectorAll(".sl-role-resp-chk");
    for (const chk of respChks) {
      chk.onchange = () => {
        if (this.mapping.mode === "long") {
          this.mapping.response_cols = chk.checked ? [chk.value] : [];
        } else {
          const selected = [];
          for (const c of respChks) {
            if (c.checked) selected.push(c.value);
          }
          this.mapping.response_cols = selected;
        }
        this.userCustomized = true;
        this._updateValidationBanner();
        this._notifyChange();
      };
    }
  }

  _updateValidationBanner() {
    if (!this.containerEl || typeof this.containerEl.querySelector !== "function") return;
    const validation = this.validate();
    const panel = this.containerEl.querySelector(".sl-role-panel");
    let banner = this.containerEl.querySelector(".sl-role-panel > .sl-alert-banner");

    if (!validation.valid) {
      const errHtml = validation.errors.map(err => `<div>&bull; ${escapeHtml(err)}</div>`).join("");
      if (!banner) {
        if (typeof document !== "undefined" && typeof document.createElement === "function") {
          banner = document.createElement("div");
          banner.className = "sl-alert-banner sl-violated";
          banner.style.cssText = "margin-top:12px;padding:8px 12px;color:var(--sl-crimson);font-size:12.5px;";
          if (panel) panel.appendChild(banner);
        }
      }
      if (banner) {
        banner.innerHTML = errHtml;
        banner.style.display = "";
      }
    } else if (banner) {
      banner.style.display = "none";
    }
  }
}
