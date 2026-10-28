// js/session-store.js
// StatLab Cumulative Session Store & Persistence
// Manages report items, General Conclusion state, .statlab.json export/import,
// and storage persistence (IndexedDB statlab_db with localStorage/memory fallback).

const _inMemoryStore = new Map();

function getIndexedDbInstance() {
  if (typeof indexedDB !== "undefined") {
    return indexedDB;
  }
  if (typeof window !== "undefined" && window.indexedDB) {
    return window.indexedDB;
  }
  return null;
}

function openDb() {
  return new Promise((resolve, reject) => {
    const idb = getIndexedDbInstance();
    if (!idb) {
      return reject(new Error("IndexedDB is not supported"));
    }
    const req = idb.open("statlab_db", 1);
    req.onupgradeneeded = (e) => {
      const db = e.target.result;
      if (!db.objectStoreNames.contains("sessions")) {
        db.createObjectStore("sessions", { keyPath: "key" });
      }
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

export class SessionStore {
  /**
   * @param {object} [initialConfig={}] Initial session configuration
   */
  constructor(initialConfig = {}) {
    this.state = {
      projectTitle: initialConfig.projectTitle || "Proyek Analisis Biostatistika",
      lang: initialConfig.lang || "id",
      alpha: typeof initialConfig.alpha === "number" ? initialConfig.alpha : 0.05,
      decimalMode: initialConfig.decimalMode || "auto",
      grid: initialConfig.grid || { headers: ["Var1"], rows: [[""]] },
      reportItems: Array.isArray(initialConfig.reportItems) ? initialConfig.reportItems : [],
      generalConclusion: initialConfig.generalConclusion || null
    };

    this._listeners = new Set();
  }

  /**
   * Subscribe to state change notifications
   * @param {function} listener
   * @returns {function} unsubscribe function
   */
  subscribe(listener) {
    this._listeners.add(listener);
    return () => this._listeners.delete(listener);
  }

  _notify() {
    for (const listener of this._listeners) {
      try {
        listener(this.getState());
      } catch (err) {
        console.error("SessionStore listener error:", err);
      }
    }
  }

  /**
   * Get current session state snapshot
   * @returns {object}
   */
  getState() {
    return {
      projectTitle: this.state.projectTitle,
      lang: this.state.lang,
      alpha: this.state.alpha,
      decimalMode: this.state.decimalMode,
      grid: this.state.grid,
      reportItems: this.state.reportItems,
      generalConclusion: this.state.generalConclusion
    };
  }

  /**
   * Update state fields
   * @param {object} updates
   */
  setState(updates) {
    if (!updates || typeof updates !== "object") return;
    Object.assign(this.state, updates);
    this._notify();
  }

  /**
   * Add an analysis snapshot to the cumulative session report
   *
   * @param {object} analysisResult StatLab analysis result object
   * @param {string} [customTitle] Optional custom title
   * @returns {string} Unique item ID
   */
  addReportItem(analysisResult, customTitle = "") {
    const id = "item_" + Date.now() + "_" + Math.random().toString(36).slice(2, 8);
    const metaLabel = analysisResult?.meta?.analysis_label;
    const defaultTitle = (typeof metaLabel === "object" ? (metaLabel[this.state.lang] || metaLabel.id) : metaLabel) || "Analisis";

    const item = {
      id,
      title: customTitle || defaultTitle,
      customTitle: customTitle || "",
      result: analysisResult,
      timestamp: new Date().toISOString()
    };

    this.state.reportItems.push(item);
    this._notify();
    return id;
  }

  /**
   * Reorder a report item by moving it up (-1) or down (+1)
   *
   * @param {string} id Item ID
   * @param {number} direction -1 for up, +1 for down
   * @returns {boolean} True if swapped, false otherwise
   */
  moveReportItem(id, direction) {
    const idx = this.state.reportItems.findIndex(it => it.id === id);
    if (idx === -1) return false;

    const newIdx = idx + direction;
    if (newIdx < 0 || newIdx >= this.state.reportItems.length) return false;

    const temp = this.state.reportItems[idx];
    this.state.reportItems[idx] = this.state.reportItems[newIdx];
    this.state.reportItems[newIdx] = temp;

    this._notify();
    return true;
  }

  /**
   * Remove a report item by ID
   *
   * @param {string} id Item ID
   * @returns {boolean} True if found and removed
   */
  removeReportItem(id) {
    const initialLen = this.state.reportItems.length;
    this.state.reportItems = this.state.reportItems.filter(it => it.id !== id);
    const removed = this.state.reportItems.length < initialLen;
    if (removed) {
      this._notify();
    }
    return removed;
  }

  /**
   * Prepare payload for R `sl_compile_general_conclusion`
   *
   * @returns {object[]} Array matching R/05_conclusions.R contract
   */
  getGeneralConclusionPayload() {
    return this.state.reportItems.map((item, idx) => {
      const res = item.result || {};
      const meta = res.meta || {};
      const assumptions = res.assumptions || {};

      const normalityViolated = Boolean(assumptions.normality_violated);
      const homogeneityViolated = Boolean(assumptions.homogeneity_violated);
      const assumptionViolated = normalityViolated || homogeneityViolated;

      const analysisType = meta.analysis_type || "descriptive";
      const chosenTest = meta.chosen_test ||
        (typeof meta.analysis_label === "object" ? (meta.analysis_label[this.state.lang] || meta.analysis_label.id) : meta.analysis_label) ||
        "Statistik Deskriptif";
      const effectiveTest = meta.effective_test || chosenTest;

      const itemTitle = item.customTitle || item.title || `Analisis ${idx + 1}`;

      return {
        title: itemTitle,
        analysis_type: analysisType,
        chosen_test: chosenTest,
        effective_test: effectiveTest,
        assumption_violated: assumptionViolated,
        normality_violated: normalityViolated,
        homogeneity_violated: homogeneityViolated,
        p_value: assumptions.homogeneity?.levene_bf?.p_value ?? null,
        summary_stat: res.main?.groups?.[0]?.mean ?? null,
        key_finding: itemTitle,
        summary_id: res.interpretation?.id || "",
        summary_en: res.interpretation?.en || "",
        interpretation: res.interpretation || null
      };
    });
  }

  /**
   * Update General Conclusion text
   *
   * @param {object} bilingualObj Object with `{ id, en }`
   */
  setGeneralConclusion(bilingualObj) {
    this.state.generalConclusion = bilingualObj;
    this._notify();
  }

  /**
   * Export project state as .statlab.json formatted string
   *
   * @returns {string} Formatted JSON
   */
  exportProjectJson() {
    const payload = {
      schema_version: 1,
      projectTitle: this.state.projectTitle,
      lang: this.state.lang,
      alpha: this.state.alpha,
      decimalMode: this.state.decimalMode,
      grid: this.state.grid,
      reportItems: this.state.reportItems,
      generalConclusion: this.state.generalConclusion,
      exportedAt: new Date().toISOString()
    };
    return JSON.stringify(payload, null, 2);
  }

  /**
   * Validate and restore session state from a .statlab.json string
   *
   * @param {string} jsonStr
   * @returns {boolean} True on success
   */
  importProjectJson(jsonStr) {
    if (typeof jsonStr !== "string" || !jsonStr.trim()) {
      throw new Error("Invalid project JSON: input is empty or not a string.");
    }

    let parsed;
    try {
      parsed = JSON.parse(jsonStr);
    } catch (err) {
      throw new Error(`Failed to parse JSON: ${err.message}`);
    }

    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      throw new Error("Invalid project file: expected a JSON object root.");
    }

    if (parsed.schema_version !== 1) {
      throw new Error(`Unsupported schema_version: ${parsed.schema_version}. Expected 1.`);
    }

    if (!Array.isArray(parsed.reportItems)) {
      throw new Error("Invalid project file: missing or invalid reportItems array.");
    }

    this.state = {
      projectTitle: parsed.projectTitle || "Proyek Tanpa Judul",
      lang: parsed.lang || "id",
      alpha: typeof parsed.alpha === "number" ? parsed.alpha : 0.05,
      decimalMode: parsed.decimalMode || "auto",
      grid: parsed.grid || { headers: ["Var1"], rows: [[""]] },
      reportItems: parsed.reportItems,
      generalConclusion: parsed.generalConclusion || null
    };

    this._notify();
    return true;
  }

  /**
   * Save session state to persistent storage (IndexedDB -> localStorage -> Memory)
   *
   * @param {string} [storageKey="statlab_current_project"]
   * @returns {Promise<boolean>}
   */
  async saveToStorage(storageKey = "statlab_current_project") {
    const serialized = JSON.stringify(this.state);

    // 1. Try IndexedDB
    try {
      const db = await openDb();
      await new Promise((resolve, reject) => {
        const tx = db.transaction("sessions", "readwrite");
        const store = tx.objectStore("sessions");
        const req = store.put({ key: storageKey, data: this.state, updatedAt: Date.now() });
        req.onsuccess = () => resolve();
        req.onerror = () => reject(req.error);
      });
      return true;
    } catch {
      // IndexedDB unavailable or failed, try localStorage
    }

    // 2. Try localStorage
    try {
      if (typeof localStorage !== "undefined") {
        localStorage.setItem(storageKey, serialized);
        return true;
      }
    } catch {
      // localStorage unavailable or quota exceeded
    }

    // 3. Fallback: in-memory store
    _inMemoryStore.set(storageKey, serialized);
    return true;
  }

  /**
   * Load session state from persistent storage
   *
   * @param {string} [storageKey="statlab_current_project"]
   * @returns {Promise<boolean>} True if found and loaded
   */
  async loadFromStorage(storageKey = "statlab_current_project") {
    // 1. Try IndexedDB
    try {
      const db = await openDb();
      const record = await new Promise((resolve, reject) => {
        const tx = db.transaction("sessions", "readonly");
        const store = tx.objectStore("sessions");
        const req = store.get(storageKey);
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
      });

      if (record && record.data) {
        this.setState(record.data);
        return true;
      }
    } catch {
      // IndexedDB unavailable, check localStorage
    }

    // 2. Try localStorage
    try {
      if (typeof localStorage !== "undefined") {
        const raw = localStorage.getItem(storageKey);
        if (raw) {
          const parsed = JSON.parse(raw);
          this.setState(parsed);
          return true;
        }
      }
    } catch {
      // localStorage failed
    }

    // 3. Try in-memory store
    if (_inMemoryStore.has(storageKey)) {
      try {
        const parsed = JSON.parse(_inMemoryStore.get(storageKey));
        this.setState(parsed);
        return true;
      } catch {
        return false;
      }
    }

    return false;
  }
}
