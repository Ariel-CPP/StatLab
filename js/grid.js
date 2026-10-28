// js/grid.js
// StatLab Spreadsheet Grid Component
// Editable table with Excel paste, locale-aware typed column conversion,
// and non-destructive outlier badging with opt-in row exclusion.

import { parseCellValue, parseDelimitedText } from "./number-locale.js";
import { t } from "./i18n.js";

/**
 * Escape string for safe HTML injection
 * @param {any} str Input string
 * @returns {string} Escaped string
 */
function escapeHtml(str) {
  if (str === null || str === undefined) return "";
  return String(str)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
}

export class StatLabGrid {
  /**
   * @param {HTMLElement | object} containerEl Container element or mock
   * @param {object} options Configuration options
   */
  constructor(containerEl, options = {}) {
    this.containerEl = containerEl;
    this.lang = options.lang || "id";
    this.decimalMode = options.decimalMode || "auto";
    this.onChange = typeof options.onChange === "function" ? options.onChange : null;

    this.headers = options.headers ? [...options.headers] : ["Var1", "Var2"];
    this.rows = options.rows ? options.rows.map(r => [...r]) : [["", ""], ["", ""]];

    // Outlier flags: 1-based row indices marked by R
    this.outlierFlags = new Set();
    // Excluded rows: 1-based row indices explicitly checked by user (empty by default!)
    this.excludedRows = new Set();
    this.detectedLocale = null;

    this.focusedCell = { row: 0, col: 0 };

    if (this.containerEl) {
      this.render();
    }
  }

  /**
   * Set headers and row data
   * @param {string[]} headers Column header labels
   * @param {string[][]} rows 2D array of raw cell strings
   */
  setData(headers, rows) {
    this.headers = headers && headers.length > 0 ? [...headers] : ["Var1"];
    this.rows = rows && rows.length > 0 ? rows.map(r => [...r]) : [new Array(this.headers.length).fill("")];
    this.outlierFlags.clear();
    this.excludedRows.clear();
    this.render();
    this._notifyChange();
  }

  /**
   * Get raw table data
   * @returns {{ headers: string[], rows: string[][] }}
   */
  getData() {
    return {
      headers: [...this.headers],
      rows: this.rows.map(r => [...r])
    };
  }

  /**
   * Load delimited text (CSV / TSV)
   * @param {string} text Raw CSV or TSV text
   * @param {"auto" | "id_comma" | "en_dot"} decimalMode Decimal mode
   * @returns {object} Detected locale info
   */
  loadDelimitedText(text, decimalMode = this.decimalMode) {
    const parsed = parseDelimitedText(text, decimalMode);
    this.headers = parsed.headers && parsed.headers.length > 0 ? parsed.headers : ["Var1"];
    this.rows = parsed.rows && parsed.rows.length > 0 ? parsed.rows : [new Array(this.headers.length).fill("")];
    this.detectedLocale = parsed.locale;
    this.decimalMode = decimalMode;
    this.outlierFlags.clear();
    this.excludedRows.clear();
    this.render();
    this._notifyChange();
    return parsed.locale;
  }

  /**
   * Handle pasting text (TSV from Excel or CSV) at target cell
   * Expands grid if pasted block exceeds current bounds
   * @param {string} text Raw clipboard text
   * @param {number} startRow 0-based starting row
   * @param {number} startCol 0-based starting column
   */
  pasteText(text, startRow = 0, startCol = 0) {
    if (!text || typeof text !== "string") return;

    const lines = text.split(/\r?\n/).filter((line, i, arr) => {
      // Keep empty lines except trailing empty line
      return i < arr.length - 1 || line.trim().length > 0;
    });
    if (lines.length === 0) return;

    // Detect delimiter for pasted text (tabs from Excel, else commas/semicolons)
    const hasTabs = lines.some(l => l.includes("\t"));
    const delim = hasTabs ? "\t" : (lines.some(l => l.includes(";")) ? ";" : ",");

    const block = lines.map(line => line.split(delim));
    const blockRows = block.length;
    const blockCols = Math.max(...block.map(r => r.length));

    // Expand headers if needed
    const requiredCols = startCol + blockCols;
    while (this.headers.length < requiredCols) {
      this.headers.push(`Var${this.headers.length + 1}`);
    }

    // Expand rows if needed
    const requiredRows = startRow + blockRows;
    while (this.rows.length < requiredRows) {
      this.rows.push(new Array(this.headers.length).fill(""));
    }

    // Pad existing rows to new header length
    for (const r of this.rows) {
      while (r.length < this.headers.length) {
        r.push("");
      }
    }

    // Insert block cells
    for (let r = 0; r < blockRows; r++) {
      const targetRow = startRow + r;
      for (let c = 0; c < block[r].length; c++) {
        const targetCol = startCol + c;
        this.rows[targetRow][targetCol] = block[r][c].trim();
      }
    }

    this.render();
    this._notifyChange();
  }

  /**
   * Extract typed columns for R analysis using number-locale parser
   * @returns {{ columns: Record<string, (number | string | null)[]>, locale: object | null }}
   */
  getTypedColumns() {
    const columns = {};
    for (let c = 0; c < this.headers.length; c++) {
      const header = this.headers[c];
      const colValues = [];
      for (let r = 0; r < this.rows.length; r++) {
        const cellRaw = this.rows[r][c];
        const parsed = parseCellValue(cellRaw, this.decimalMode, this.detectedLocale);
        colValues.push(parsed.value);
      }
      columns[header] = colValues;
    }
    return {
      columns,
      locale: this.detectedLocale
    };
  }

  /**
   * Set outlier flags (1-based row indices) from R result
   * Never automatically excludes rows (§2, §6.5)
   * @param {Array<number | { source_row?: number, row?: number }>} flaggedRows Flagged outliers
   */
  setOutlierFlags(flaggedRows) {
    this.outlierFlags.clear();
    if (Array.isArray(flaggedRows)) {
      for (const item of flaggedRows) {
        if (typeof item === "number" && Number.isInteger(item) && item > 0) {
          this.outlierFlags.add(item);
        } else if (item && typeof item === "object") {
          const rowIdx = item.source_row || item.row;
          if (typeof rowIdx === "number" && Number.isInteger(rowIdx) && rowIdx > 0) {
            this.outlierFlags.add(rowIdx);
          }
        }
      }
    }
    this.render();
  }

  /**
   * Get 1-based indices of explicitly excluded rows (empty by default!)
   * @returns {number[]} 1-based row indices
   */
  getExcludedRows() {
    return Array.from(this.excludedRows).sort((a, b) => a - b);
  }

  /**
   * Explicitly toggle or set exclusion for a 1-based row index
   * @param {number} rowIndex 1-based row index
   * @param {boolean} [forceValue] Optional forced boolean
   */
  toggleExcludeRow(rowIndex, forceValue) {
    const r = Number(rowIndex);
    if (!Number.isInteger(r) || r < 1) return;

    const shouldExclude = forceValue !== undefined ? Boolean(forceValue) : !this.excludedRows.has(r);
    if (shouldExclude) {
      this.excludedRows.add(r);
    } else {
      this.excludedRows.delete(r);
    }
    this.render();
    this._notifyChange();
  }

  /**
   * Add a new row to the grid
   * @param {string[]} [defaultVals] Optional initial cell values
   */
  addRow(defaultVals = []) {
    const newRow = new Array(this.headers.length).fill("");
    for (let c = 0; c < Math.min(this.headers.length, defaultVals.length); c++) {
      newRow[c] = String(defaultVals[c]);
    }
    this.rows.push(newRow);
    this.render();
    this._notifyChange();
  }

  /**
   * Delete row at 1-based index
   * @param {number} rowIndex 1-based row index
   */
  deleteRow(rowIndex) {
    const r = Number(rowIndex);
    if (!Number.isInteger(r) || r < 1 || r > this.rows.length) return;
    if (this.rows.length <= 1) {
      this.rows[0] = new Array(this.headers.length).fill("");
    } else {
      this.rows.splice(r - 1, 1);
    }

    // Adjust excluded and flagged row sets
    const updateSet = (oldSet) => {
      const newSet = new Set();
      for (const idx of oldSet) {
        if (idx < r) newSet.add(idx);
        else if (idx > r) newSet.add(idx - 1);
      }
      return newSet;
    };
    this.excludedRows = updateSet(this.excludedRows);
    this.outlierFlags = updateSet(this.outlierFlags);

    this.render();
    this._notifyChange();
  }

  /**
   * Add a new column
   * @param {string} [name] Column name
   */
  addColumn(name = "") {
    const colName = name && name.trim().length > 0 ? name.trim() : `Var${this.headers.length + 1}`;
    this.headers.push(colName);
    for (const row of this.rows) {
      row.push("");
    }
    this.render();
    this._notifyChange();
  }

  /**
   * Delete column at 0-based index
   * @param {number} colIndex 0-based column index
   */
  deleteColumn(colIndex) {
    const c = Number(colIndex);
    if (!Number.isInteger(c) || c < 0 || c >= this.headers.length) return;
    if (this.headers.length <= 1) return;

    this.headers.splice(c, 1);
    for (const row of this.rows) {
      row.splice(c, 1);
    }
    this.render();
    this._notifyChange();
  }

  /**
   * Set cell value directly
   * @param {number} r 0-based row
   * @param {number} c 0-based col
   * @param {string} val Value
   */
  setCell(r, c, val) {
    if (this.rows[r] && this.rows[r][c] !== undefined) {
      this.rows[r][c] = String(val);
      this._notifyChange();
    }
  }

  /**
   * Set header name directly
   * @param {number} c 0-based col
   * @param {string} name Header name
   */
  setHeader(c, name) {
    if (this.headers[c] !== undefined) {
      this.headers[c] = String(name).trim() || `Var${c + 1}`;
      this._notifyChange();
    }
  }

  /**
   * Show non-blocking toast notification
   * @param {string} message Text
   * @param {"info" | "success" | "crimson"} type Style
   */
  showToast(message, type = "info") {
    if (typeof document === "undefined") return;
    const parent = (this.containerEl && typeof this.containerEl.closest === "function" && this.containerEl.closest(".sl-app")) || document.body;
    let container = parent.querySelector(".sl-toast-container") || document.querySelector(".sl-toast-container");
    if (!container) {
      container = document.createElement("div");
      container.className = "sl-toast-container";
      parent.appendChild(container);
    }
    const toast = document.createElement("div");
    toast.className = `sl-toast sl-toast-${type}`;
    toast.textContent = message;
    container.appendChild(toast);
    setTimeout(() => {
      toast.remove();
    }, 3000);
  }

  _notifyChange() {
    if (this.onChange) {
      this.onChange(this.getData());
    }
  }

  /**
   * Render HTML into container element
   */
  render() {
    if (!this.containerEl) return;

    const isId = this.lang === "id";
    const addRowText = isId ? "+ Tambah Baris" : "+ Add Row";
    const addColText = isId ? "+ Tambah Kolom" : "+ Add Column";
    const clearText = isId ? "Kosongkan" : "Clear";
    const excludeTooltip = isId ? "Keluarkan baris ini dari analisis" : "Exclude row from analysis";
    const outlierTooltip = isId ? "Pencilan terdeteksi oleh R" : "Outlier flagged by R";

    let html = `
      <div class="sl-grid-toolbar" style="display:flex;gap:8px;margin-bottom:8px;align-items:center;">
        <button type="button" class="sl-btn sl-btn-sm sl-btn-add-row">${addRowText}</button>
        <button type="button" class="sl-btn sl-btn-sm sl-btn-add-col">${addColText}</button>
        <button type="button" class="sl-btn sl-btn-sm sl-btn-clear" style="margin-left:auto;">${clearText}</button>
      </div>
      <div class="sl-grid-wrapper">
        <table class="sl-grid-table">
          <thead>
            <tr>
              <th class="sl-row-header-cell" style="width:55px;">#</th>
    `;

    for (let c = 0; c < this.headers.length; c++) {
      html += `
        <th>
          <div style="display:flex;align-items:center;justify-content:space-between;">
            <input type="text" class="sl-grid-th-input" data-col="${c}" value="${escapeHtml(this.headers[c])}" />
            ${this.headers.length > 1 ? `<button type="button" class="sl-btn sl-btn-sm sl-btn-del-col" data-col="${c}" title="Hapus kolom" style="padding:0 4px;border:none;background:transparent;cursor:pointer;color:var(--sl-crimson);">&times;</button>` : ""}
          </div>
        </th>
      `;
    }
    html += `</tr></thead><tbody>`;

    for (let r = 0; r < this.rows.length; r++) {
      const rowNum = r + 1;
      const isExcluded = this.excludedRows.has(rowNum);
      const isOutlier = this.outlierFlags.has(rowNum);
      const rowClass = isExcluded ? "sl-row-excluded" : "";

      html += `<tr class="${rowClass}" data-row="${rowNum}">`;
      html += `
        <td class="sl-row-header-cell">
          <input type="checkbox" class="sl-row-exclude-chk" data-row="${rowNum}" ${isExcluded ? "checked" : ""} title="${excludeTooltip}" />
          <span>${rowNum}</span>
          ${isOutlier ? `<span class="sl-outlier-badge" title="${outlierTooltip}">!</span>` : ""}
        </td>
      `;

      for (let c = 0; c < this.headers.length; c++) {
        const val = this.rows[r][c] !== undefined ? this.rows[r][c] : "";
        html += `
          <td>
            <input type="text" class="sl-grid-cell-input" data-row="${r}" data-col="${c}" value="${escapeHtml(val)}" />
          </td>
        `;
      }
      html += `</tr>`;
    }

    html += `</tbody></table></div>`;

    this.containerEl.innerHTML = html;
    this._attachDomEvents();
  }

  _attachDomEvents() {
    if (!this.containerEl || typeof this.containerEl.querySelector !== "function") return;

    // Add Row
    const addRowBtn = this.containerEl.querySelector(".sl-btn-add-row");
    if (addRowBtn) {
      addRowBtn.onclick = () => this.addRow();
    }

    // Add Col
    const addColBtn = this.containerEl.querySelector(".sl-btn-add-col");
    if (addColBtn) {
      addColBtn.onclick = () => this.addColumn();
    }

    // Clear
    const clearBtn = this.containerEl.querySelector(".sl-btn-clear");
    if (clearBtn) {
      clearBtn.onclick = () => {
        this.setData(["Var1", "Var2"], [["", ""], ["", ""]]);
      };
    }

    // Delete Col buttons
    const delColBtns = this.containerEl.querySelectorAll(".sl-btn-del-col");
    for (const btn of delColBtns) {
      btn.onclick = (e) => {
        const c = parseInt(e.currentTarget.getAttribute("data-col"), 10);
        this.deleteColumn(c);
      };
    }

    // Exclude checkboxes
    const chks = this.containerEl.querySelectorAll(".sl-row-exclude-chk");
    for (const chk of chks) {
      chk.onchange = (e) => {
        const r = parseInt(e.currentTarget.getAttribute("data-row"), 10);
        this.toggleExcludeRow(r, e.currentTarget.checked);
      };
    }

    // Cell inputs & Header inputs
    const thInputs = this.containerEl.querySelectorAll(".sl-grid-th-input");
    for (const inp of thInputs) {
      inp.onchange = (e) => {
        const c = parseInt(e.currentTarget.getAttribute("data-col"), 10);
        this.setHeader(c, e.currentTarget.value);
      };
    }

    const cellInputs = this.containerEl.querySelectorAll(".sl-grid-cell-input");
    for (const inp of cellInputs) {
      inp.onchange = (e) => {
        const r = parseInt(e.currentTarget.getAttribute("data-row"), 10);
        const c = parseInt(e.currentTarget.getAttribute("data-col"), 10);
        this.setCell(r, c, e.currentTarget.value);
      };

      // Handle Excel paste directly into cell
      inp.onpaste = (e) => {
        if (e.clipboardData) {
          const text = e.clipboardData.getData("text");
          if (text && (text.includes("\t") || text.includes("\n"))) {
            e.preventDefault();
            const r = parseInt(e.currentTarget.getAttribute("data-row"), 10);
            const c = parseInt(e.currentTarget.getAttribute("data-col"), 10);
            this.pasteText(text, r, c);
          }
        }
      };

      // Keyboard navigation (Enter, Tab, Arrows)
      inp.onkeydown = (e) => {
        const r = parseInt(e.currentTarget.getAttribute("data-row"), 10);
        const c = parseInt(e.currentTarget.getAttribute("data-col"), 10);
        let nextR = r;
        let nextC = c;

        if (e.key === "Enter" || e.key === "ArrowDown") {
          e.preventDefault();
          nextR = r + 1;
          if (nextR >= this.rows.length) {
            this.addRow();
            nextR = this.rows.length - 1;
          }
        } else if (e.key === "ArrowUp") {
          e.preventDefault();
          nextR = Math.max(0, r - 1);
        } else if (e.key === "Tab" && !e.shiftKey) {
          // Normal Tab
          if (c === this.headers.length - 1 && r === this.rows.length - 1) {
            e.preventDefault();
            this.addRow();
            nextR = r + 1;
            nextC = 0;
          }
        } else if (e.key === "ArrowRight" && e.currentTarget.selectionStart === e.currentTarget.value.length) {
          nextC = Math.min(this.headers.length - 1, c + 1);
        } else if (e.key === "ArrowLeft" && e.currentTarget.selectionStart === 0) {
          nextC = Math.max(0, c - 1);
        }

        if (nextR !== r || nextC !== c) {
          const target = this.containerEl.querySelector(`.sl-grid-cell-input[data-row="${nextR}"][data-col="${nextC}"]`);
          if (target) {
            target.focus();
            target.select();
          }
        }
      };
    }
  }
}
