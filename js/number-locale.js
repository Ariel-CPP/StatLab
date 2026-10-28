// js/number-locale.js
// StatLab Locale Number & Delimited CSV/TSV Parser & Serializer
// Zero statistical computations. Only parsing, normalization, and serialization.

/**
 * Detect delimiter, decimal mark, thousands separator, and potential ambiguity
 *
 * @param {string} rawText Raw delimited input text
 * @returns {{ delimiter: ";" | "," | "\t", decimalMark: "," | ".", thousandsMark: "." | "," | "", ambiguous: boolean, sampleNote: string }}
 */
export function detectLocaleFormat(rawText) {
  if (!rawText || typeof rawText !== "string") {
    return {
      delimiter: ",",
      decimalMark: ".",
      thousandsMark: "",
      ambiguous: false,
      sampleNote: "Empty input text."
    };
  }

  // Strip BOM if present
  const text = rawText.startsWith("\uFEFF") ? rawText.slice(1) : rawText;

  // 1. Detect Delimiter
  // Sample up to first 40 lines
  const rawLines = text.split(/\r?\n/).filter(line => line.trim().length > 0).slice(0, 40);
  let semiCount = 0;
  let commaCount = 0;
  let tabCount = 0;

  for (const line of rawLines) {
    // Count delimiters outside of quotes
    let inQuotes = false;
    for (let i = 0; i < line.length; i++) {
      const ch = line[i];
      if (ch === '"') {
        inQuotes = !inQuotes;
      } else if (!inQuotes) {
        if (ch === ";") semiCount++;
        else if (ch === ",") commaCount++;
        else if (ch === "\t") tabCount++;
      }
    }
  }

  let delimiter = ",";
  if (tabCount > 0 && tabCount >= semiCount && tabCount >= commaCount) {
    delimiter = "\t";
  } else if (semiCount > 0 && semiCount >= commaCount) {
    delimiter = ";";
  } else {
    delimiter = ",";
  }

  // 2. Scan numeric tokens for decimal and thousands mark patterns
  // Pattern A: dot thousands + comma decimal: 1.234,56
  const dotThousCommaDec = /\b\d{1,3}(?:\.\d{3})+,\d+\b/;
  // Pattern B: comma thousands + dot decimal: 1,234.56
  const commaThousDotDec = /\b\d{1,3}(?:,\d{3})+\.\d+\b/;
  // Pattern C: comma decimal without thousands: 12,45 or 0,5
  const commaDecOnly = /\b\d+,\d+\b/;
  // Pattern D: dot decimal with non-3 digits: 12.4 or 12.45 or 0.1234
  const dotDecNonThree = /\b\d+\.(?:\d{1,2}|\d{4,})\b/;
  // Pattern E: exactly 3 digits after dot: 1.234 (Ambiguous pattern)
  const dotDecThree = /\b\d+\.\d{3}\b/;

  let hasDotThousCommaDec = false;
  let hasCommaThousDotDec = false;
  let hasCommaDec = false;
  let hasDotDecNonThree = false;
  let hasDotDecThree = false;

  // Split lines into tokens using detected delimiter
  for (const line of rawLines) {
    const tokens = line.split(delimiter).map(t => t.trim().replace(/^"(.*)"$/, "$1").trim());
    for (const tok of tokens) {
      if (dotThousCommaDec.test(tok)) hasDotThousCommaDec = true;
      if (commaThousDotDec.test(tok)) hasCommaThousDotDec = true;
      if (commaDecOnly.test(tok)) hasCommaDec = true;
      if (dotDecNonThree.test(tok)) hasDotDecNonThree = true;
      if (dotDecThree.test(tok)) hasDotDecThree = true;
    }
  }

  let decimalMark = ".";
  let thousandsMark = "";
  let ambiguous = false;
  let sampleNote = "Standard locale format detected.";

  if (hasDotThousCommaDec) {
    decimalMark = ",";
    thousandsMark = ".";
    sampleNote = "Indonesian / European format (dot thousands, comma decimal).";
  } else if (hasCommaThousDotDec) {
    decimalMark = ".";
    thousandsMark = ",";
    sampleNote = "International / US format (comma thousands, dot decimal).";
  } else if (hasCommaDec && delimiter !== ",") {
    decimalMark = ",";
    thousandsMark = ".";
    sampleNote = "Comma decimal format detected.";
  } else if (hasDotDecThree && !hasCommaDec && !hasDotDecNonThree && !hasCommaThousDotDec && !hasDotThousCommaDec) {
    // Review Focus #1: Ambiguous 3-trailing-digit "1.234" without comma clues
    ambiguous = true;
    decimalMark = ".";
    thousandsMark = "";
    sampleNote = "Ambiguous 3-trailing-digit dot pattern (e.g. 1.234). Defaults safely to decimal 1.234 unless overridden.";
  } else if (hasDotDecNonThree || hasDotDecThree) {
    decimalMark = ".";
    thousandsMark = "";
    sampleNote = "Dot decimal format detected.";
  } else if (delimiter === ";") {
    // Semicolon delimiter defaults to comma decimal in Indonesian/European CSV
    decimalMark = ",";
    thousandsMark = ".";
    sampleNote = "Semicolon delimiter defaults to comma decimal.";
  }

  return {
    delimiter,
    decimalMark,
    thousandsMark,
    ambiguous,
    sampleNote
  };
}

const MISSING_TOKENS = new Set([
  "",
  "na",
  "n/a",
  "-",
  ".",
  "nan",
  "null",
  "none",
  "nil",
  "#n/a",
  "missing"
]);

/**
 * Parse a raw table cell into typed object { type, value, raw }
 *
 * @param {any} raw Cell contents
 * @param {"auto" | "id_comma" | "id" | "en_dot" | "en"} mode Parsing mode
 * @param {object | null} detected Optional pre-detected locale format
 * @returns {{ type: "number" | "missing" | "text", value: number | null | string, raw: string }}
 */
export function parseCellValue(raw, mode = "auto", detected = null) {
  const rawStr = raw === null || raw === undefined ? "" : String(raw);
  const trimmed = rawStr.trim();

  // Check missing values
  if (MISSING_TOKENS.has(trimmed.toLowerCase())) {
    return {
      type: "missing",
      value: null,
      raw: rawStr
    };
  }

  // Determine decimal and thousands marks
  let dec = ".";
  let thous = ",";

  if (mode === "id_comma" || mode === "id") {
    dec = ",";
    thous = ".";
  } else if (mode === "en_dot" || mode === "en") {
    dec = ".";
    thous = ",";
  } else if (detected && detected.decimalMark) {
    dec = detected.decimalMark;
    thous = detected.thousandsMark || (dec === "," ? "." : ",");
  } else {
    // Auto-detect single cell value
    if (trimmed.includes(",") && !trimmed.includes(".")) {
      dec = ",";
      thous = ".";
    } else {
      dec = ".";
      thous = ",";
    }
  }

  // Normalize string for numerical parsing
  let normalized = trimmed;
  if (dec === ",") {
    // Remove dots as thousands marks, remove spaces, change comma to dot
    normalized = normalized.replace(/\./g, "").replace(/\s/g, "").replace(",", ".");
  } else {
    // Remove commas as thousands marks, remove spaces
    normalized = normalized.replace(/,/g, "").replace(/\s/g, "");
  }

  // Check if valid finite number
  if (/^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/.test(normalized)) {
    const num = parseFloat(normalized);
    if (Number.isFinite(num)) {
      return {
        type: "number",
        value: num,
        raw: rawStr
      };
    }
  }

  // Fallback to text
  return {
    type: "text",
    value: trimmed,
    raw: rawStr
  };
}

/**
 * Parse RFC 4180 delimited text into headers, row string arrays, and detected locale
 *
 * @param {string} rawText Raw CSV/TSV text
 * @param {"auto" | "id_comma" | "id" | "en_dot" | "en"} mode Delimiter/locale parsing mode
 * @returns {{ headers: string[], rows: string[][], locale: object }}
 */
export function parseDelimitedText(rawText, mode = "auto") {
  if (!rawText || typeof rawText !== "string") {
    return {
      headers: [],
      rows: [],
      locale: { delimiter: ",", decimalMark: ".", thousandsMark: "", ambiguous: false, sampleNote: "Empty input" }
    };
  }

  const text = rawText.startsWith("\uFEFF") ? rawText.slice(1) : rawText;
  const locale = detectLocaleFormat(text);
  const delim = locale.delimiter;

  // Parse lines with RFC-4180 quotes handling
  const rows = [];
  let currentRow = [];
  let currentField = "";
  let inQuotes = false;
  let i = 0;
  const len = text.length;

  while (i < len) {
    const ch = text[i];
    if (inQuotes) {
      if (ch === '"') {
        if (i + 1 < len && text[i + 1] === '"') {
          currentField += '"';
          i += 2;
          continue;
        } else {
          inQuotes = false;
          i++;
          continue;
        }
      } else {
        currentField += ch;
        i++;
        continue;
      }
    } else {
      if (ch === '"') {
        inQuotes = true;
        i++;
        continue;
      } else if (ch === delim) {
        currentRow.push(currentField.trim());
        currentField = "";
        i++;
        continue;
      } else if (ch === "\r") {
        if (i + 1 < len && text[i + 1] === "\n") {
          i++;
        }
        currentRow.push(currentField.trim());
        currentField = "";
        if (currentRow.some(c => c.length > 0)) {
          rows.push(currentRow);
        }
        currentRow = [];
        i++;
        continue;
      } else if (ch === "\n") {
        currentRow.push(currentField.trim());
        currentField = "";
        if (currentRow.some(c => c.length > 0)) {
          rows.push(currentRow);
        }
        currentRow = [];
        i++;
        continue;
      } else {
        currentField += ch;
        i++;
      }
    }
  }

  if (currentField.length > 0 || currentRow.length > 0) {
    currentRow.push(currentField.trim());
    if (currentRow.some(c => c.length > 0)) {
      rows.push(currentRow);
    }
  }

  if (rows.length === 0) {
    return { headers: [], rows: [], locale };
  }

  const headers = rows[0];
  const dataRows = rows.slice(1);

  return {
    headers,
    rows: dataRows,
    locale
  };
}

/**
 * Serialize table data into delimited text format (CSV / TSV)
 *
 * @param {string[]} headers Array of header column names
 * @param {(string | number)[][]} rows Array of data rows
 * @param {"id" | "en"} lang Target language ("id" uses ';' and ',', "en" uses ',' and '.')
 * @param {"csv" | "tsv"} target Target file format ("csv" | "tsv")
 * @returns {string} Formatted delimited text
 */
export function serializeTableDelimited(headers, rows, lang = "id", target = "csv") {
  const isEn = lang === "en";
  const isTsv = target === "tsv";

  const delim = isTsv ? "\t" : (isEn ? "," : ";");
  const decMark = isEn ? "." : ",";

  function formatCell(val) {
    if (val === null || val === undefined) return "";
    let str = "";
    if (typeof val === "number") {
      str = Number.isFinite(val) ? String(val).replace(".", decMark) : "";
    } else {
      str = String(val);
    }
    // Escape cell if needed
    if (str.includes(delim) || str.includes('"') || str.includes("\n") || str.includes("\r")) {
      str = `"${str.replace(/"/g, '""')}"`;
    }
    return str;
  }

  const out = [];
  out.push(headers.map(formatCell).join(delim));
  for (const row of rows) {
    out.push(row.map(formatCell).join(delim));
  }
  return out.join("\n") + "\n";
}
