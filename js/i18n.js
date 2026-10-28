// js/i18n.js
// StatLab Minimal Bilingual Dictionary & Interpolation Helper
// Supports Indonesian ('id') and English ('en').

export const SUPPORTED_LANGS = ["id", "en"];

const TRANSLATIONS = {
  id: {
    "app.title": "StatLab Biostatistika Komputasional",
    "app.subtitle": "Aplikasi Analisis Biostatistika Laboratorium Berbasis webR (WASM)",
    "test.interpolate": "{name}",
    "locale.auto": "Otomatis (Deteksi)",
    "locale.id_comma": "Indonesia (Koma desimal, titik ribuan)",
    "locale.en_dot": "Inggris / Internasional (Titik desimal, koma ribuan)",
    "locale.ambiguous_warning": "Format angka memiliki pemisah 3 digit ambigu (contoh: 1.234). Standar desimal diterapkan secara aman.",
    "status.initializing": "Memuat lingkungan webR WASM...",
    "status.ready": "Siap untuk analisis.",
    "status.computing": "Menjalankan komputasi statistika...",
    "status.error": "Terjadi kesalahan komputasi: {error}",
    "error.missing_data": "Data tidak boleh kosong.",
    "error.invalid_format": "Format data tabel tidak valid.",
    "error.unknown_action": "Aksi tidak dikenali: {action}"
  },
  en: {
    "app.title": "StatLab Computational Biostatistics",
    "app.subtitle": "Laboratory Biostatistical Analysis PWA Powered by webR (WASM)",
    "test.interpolate": "{name}",
    "locale.auto": "Automatic (Detected)",
    "locale.id_comma": "Indonesian (Comma decimal, dot thousands)",
    "locale.en_dot": "English / International (Dot decimal, comma thousands)",
    "locale.ambiguous_warning": "Number format has ambiguous 3-trailing-digit dot pattern (e.g. 1.234). Safely defaulting to decimal.",
    "status.initializing": "Initializing webR WASM environment...",
    "status.ready": "Ready for analysis.",
    "status.computing": "Running statistical computation...",
    "status.error": "Computation error: {error}",
    "error.missing_data": "Data cannot be empty.",
    "error.invalid_format": "Invalid table data format.",
    "error.unknown_action": "Unknown action: {action}"
  }
};

/**
 * Translate a key with variable interpolation
 *
 * @param {string} key Dictionary key
 * @param {string} lang Language code ('id' | 'en')
 * @param {Record<string, any>} vars Substitution variables
 * @returns {string} Translated and interpolated string
 */
export function t(key, lang = "id", vars = {}) {
  const selectedLang = SUPPORTED_LANGS.includes(lang) ? lang : "id";
  const dict = TRANSLATIONS[selectedLang] || TRANSLATIONS.id;
  let text = dict[key] || TRANSLATIONS.en[key] || TRANSLATIONS.id[key] || key;

  if (vars && typeof vars === "object") {
    for (const [vKey, vVal] of Object.entries(vars)) {
      text = text.replaceAll(`{${vKey}}`, vVal !== undefined && vVal !== null ? String(vVal) : "");
    }
  }
  return text;
}
