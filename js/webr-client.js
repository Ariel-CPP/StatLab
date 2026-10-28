// js/webr-client.js
// StatLab webR Client Engine
// Connects browser/Node to webR (WASM), runs R/00..99 modules via sl_dispatch(),
// and ensures full parity with desktop R 4.5.2.

const isNode = typeof process !== "undefined" && Boolean(process.versions?.node);

/**
 * Format a JavaScript object / primitive into valid R expression syntax
 *
 * @param {any} val JavaScript value
 * @param {string} key Key or context name
 * @returns {string} R expression string
 */
function toRExpr(val, key = "") {
  if (val === null || val === undefined) return "NULL";
  if (typeof val === "boolean") return val ? "TRUE" : "FALSE";
  if (typeof val === "number") return Number.isFinite(val) ? String(val) : "NA_real_";
  if (typeof val === "string") return JSON.stringify(val);
  if (Array.isArray(val)) {
    if (val.length === 0) {
      if (key === "exclude_rows") return "integer(0)";
      if (key.includes("cols") || key === "items" || key === "lang") return "character(0)";
      return "list()";
    }
    if (val.every(x => typeof x === "number")) {
      return `c(${val.map(x => toRExpr(x)).join(", ")})`;
    }
    if (val.every(x => typeof x === "string")) {
      return `c(${val.map(x => toRExpr(x)).join(", ")})`;
    }
    if (val.every(x => typeof x === "boolean")) {
      return `c(${val.map(x => toRExpr(x)).join(", ")})`;
    }
    return `list(${val.map((x, idx) => toRExpr(x, String(idx))).join(", ")})`;
  }
  if (typeof val === "object") {
    const entries = Object.entries(val).map(([k, v]) => {
      const safeKey = /^[a-zA-Z.][a-zA-Z0-9._]*$/.test(k) ? k : `\`${k}\``;
      return `${safeKey} = ${toRExpr(v, k)}`;
    });
    return `list(${entries.join(", ")})`;
  }
  return "NULL";
}

export class StatLabEngine {
  /**
   * @param {object} options
   * @param {string} [options.baseUrl] Path to webR vendor directory (default: './vendor/webr/')
   * @param {string} [options.rPath] Path to R source directory (default: './R/')
   * @param {string} [options.repoPath] Path to vendor/r-repo directory (default: './vendor/r-repo/')
   */
  constructor(options = {}) {
    this.options = options;
    this.baseUrl = options.baseUrl || "./vendor/webr/";
    this.rPath = options.rPath || "./R/";
    this.repoPath = options.repoPath || "./vendor/r-repo/";
    this.webr = null;
    this.initialized = false;
  }

  /**
   * Initialize webR, load R/00..99 modules, and verify recommended packages
   *
   * @param {(status: { stage: string, progress: number, message: string }) => void} [onProgress]
   */
  async init(onProgress = () => {}) {
    if (this.initialized) return;

    onProgress({ stage: "init_webr", progress: 0.1, message: "Initializing webR WASM runtime..." });

    let WebR;
    let ChannelType;

    if (isNode) {
      // In Node.js environment, require webr.cjs
      const { createRequire } = await import("node:module");
      const path = await import("node:path");
      const require = createRequire(import.meta.url);

      let cjsPath;
      if (this.baseUrl.startsWith("file://")) {
        const { fileURLToPath } = await import("node:url");
        cjsPath = path.join(fileURLToPath(this.baseUrl), "webr.cjs");
      } else {
        cjsPath = path.resolve(this.baseUrl, "webr.cjs");
      }

      const webrPkg = require(cjsPath);
      WebR = webrPkg.WebR;
      ChannelType = webrPkg.ChannelType;

      // Ensure normalized baseUrl for WebR options
      let normalizedBase = this.baseUrl;
      if (!normalizedBase.endsWith(path.sep) && !normalizedBase.endsWith("/")) {
        normalizedBase += path.sep;
      }

      this.webr = new WebR({ baseUrl: normalizedBase });
    } else {
      // In Browser environment, dynamically import webr.mjs
      const mjsUrl = this.baseUrl.endsWith("/") ? `${this.baseUrl}webr.mjs` : `${this.baseUrl}/webr.mjs`;
      const webrPkg = await import(mjsUrl);
      WebR = webrPkg.WebR;
      ChannelType = webrPkg.ChannelType;

      let normalizedBase = this.baseUrl;
      if (!normalizedBase.endsWith("/")) {
        normalizedBase += "/";
      }

      this.webr = new WebR({
        baseUrl: normalizedBase,
        channelType: ChannelType.PostMessage
      });
    }

    await this.webr.init();
    onProgress({ stage: "init_webr", progress: 0.3, message: "webR runtime ready." });

    // Ensure /statlab/R exists in VFS
    try {
      await this.webr.FS.mkdir("/statlab");
    } catch {
      // already exists
    }
    try {
      await this.webr.FS.mkdir("/statlab/R");
    } catch {
      // already exists
    }

    // Load R/*.R files
    onProgress({ stage: "load_r_modules", progress: 0.4, message: "Mounting StatLab R modules..." });

    const rFileNames = [
      "00_json.R",
      "01_data_prep.R",
      "02_assumptions.R",
      "03_plots_base.R",
      "04_descriptive.R",
      "05_conclusions.R",
      "99_dispatcher.R"
    ];

    const encoder = new TextEncoder();

    if (isNode) {
      const fs = await import("node:fs/promises");
      const path = await import("node:path");
      const { fileURLToPath } = await import("node:url");
      const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
      const rDir = path.resolve(projectRoot, "R");

      for (const fileName of rFileNames) {
        const filePath = path.join(rDir, fileName);
        const code = await fs.readFile(filePath, "utf8");
        await this.webr.FS.writeFile(`/statlab/R/${fileName}`, encoder.encode(code));
        await this.webr.evalR(`source('/statlab/R/${fileName}', encoding = 'UTF-8')`);
      }
    } else {
      for (const fileName of rFileNames) {
        const fileUrl = this.rPath.endsWith("/") ? `${this.rPath}${fileName}` : `${this.rPath}/${fileName}`;
        const res = await fetch(fileUrl);
        if (!res.ok) throw new Error(`Failed to fetch R module: ${fileUrl}`);
        const code = await res.text();
        await this.webr.FS.writeFile(`/statlab/R/${fileName}`, encoder.encode(code));
        await this.webr.evalR(`source('/statlab/R/${fileName}', encoding = 'UTF-8')`);
      }
    }

    onProgress({ stage: "load_r_modules", progress: 0.6, message: "StatLab R modules loaded." });

    // Verify and load recommended packages (survival, cluster, MASS) from vendor/r-repo
    onProgress({ stage: "load_packages", progress: 0.7, message: "Verifying recommended packages..." });

    const pkgsToInstall = ["lattice", "Matrix", "survival", "cluster", "MASS"];

    if (isNode) {
      const fs = await import("node:fs/promises");
      const path = await import("node:path");
      const { fileURLToPath } = await import("node:url");
      const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
      const rRepoDir = path.resolve(projectRoot, "vendor", "r-repo");

      for (const pkg of pkgsToInstall) {
        const pkgFile = path.join(rRepoDir, `${pkg}.tgz`);
        try {
          const buf = await fs.readFile(pkgFile);
          await this.webr.FS.writeFile(`/tmp/${pkg}.tgz`, new Uint8Array(buf));
          await this.webr.evalR(`utils::untar('/tmp/${pkg}.tgz', exdir = .libPaths()[[1]], tar = 'internal')`);
          await this.webr.FS.unlink(`/tmp/${pkg}.tgz`);
        } catch (e) {
          console.warn(`Could not load vendored package ${pkg}:`, e.message);
        }
      }
    } else {
      for (const pkg of pkgsToInstall) {
        const pkgUrl = this.repoPath.endsWith("/") ? `${this.repoPath}${pkg}.tgz` : `${this.repoPath}/${pkg}.tgz`;
        try {
          const res = await fetch(pkgUrl);
          if (res.ok) {
            const buf = new Uint8Array(await res.arrayBuffer());
            await this.webr.FS.writeFile(`/tmp/${pkg}.tgz`, buf);
            await this.webr.evalR(`utils::untar('/tmp/${pkg}.tgz', exdir = .libPaths()[[1]], tar = 'internal')`);
            await this.webr.FS.unlink(`/tmp/${pkg}.tgz`);
          }
        } catch (e) {
          console.warn(`Could not load browser vendored package ${pkg}:`, e.message);
        }
      }
    }

    // Verify they load cleanly
    try {
      await this.webr.evalR("library(survival); library(cluster); library(MASS)");
    } catch (e) {
      console.warn("Warning during library(survival/cluster/MASS) check:", e.message);
    }

    this.initialized = true;
    onProgress({ stage: "ready", progress: 1.0, message: "StatLabEngine initialized." });
  }

  /**
   * Run a biostatistical analysis via sl_dispatch()
   *
   * @param {"descriptive"} analysisType Type of statistical analysis
   * @param {Record<string, (number | null)[]> | object[]} tableData Table columns or rows
   * @param {object} roles Analysis roles (e.g. { mode: 'wide', response_cols: [...] })
   * @param {object} opts Analysis options (e.g. { alpha: 0.05, exclude_rows: [], include_plots: true })
   * @returns {Promise<object>} Parsed analysis response JSON
   */
  async runAnalysis(analysisType, tableData, roles = {}, opts = {}) {
    if (!this.initialized) {
      await this.init();
    }

    // Normalize tableData to column-oriented dictionary if array of rows was provided
    let colData = tableData;
    if (Array.isArray(tableData) && tableData.length > 0) {
      colData = {};
      const keys = Object.keys(tableData[0]);
      for (const k of keys) {
        colData[k] = tableData.map(row => row[k]);
      }
    }

    // Bind data.frame to R environment
    const dfObj = await new this.webr.RObject(colData);
    await this.webr.objs.globalEnv.bind("statlab_df", dfObj);

    const rCall = `sl_dispatch("run_analysis", list(
      analysis_type = ${JSON.stringify(analysisType)},
      data = statlab_df,
      roles = ${toRExpr(roles, "roles")},
      opts = ${toRExpr(opts, "opts")}
    ))`;

    const res = await this.webr.evalR(rCall);
    const jsonStr = (await res.toJs()).values[0];
    return JSON.parse(jsonStr);
  }

  /**
   * Compile a bilingual general conclusion from analysis summary items
   *
   * @param {object[]} items List of conclusion summary items
   * @param {string | string[]} lang Language code(s) ('id' | 'en' | ['id', 'en'])
   * @returns {Promise<object>} Compiled conclusion response JSON
   */
  async compileGeneralConclusion(items, lang = "id") {
    if (!this.initialized) {
      await this.init();
    }

    const rCall = `sl_dispatch("general_conclusion", list(
      items = ${toRExpr(items, "items")},
      lang = ${toRExpr(lang, "lang")}
    ))`;

    const res = await this.webr.evalR(rCall);
    const jsonStr = (await res.toJs()).values[0];
    return JSON.parse(jsonStr);
  }

  /**
   * Close and dispose webR instance
   */
  async close() {
    if (this.webr) {
      await this.webr.close();
      this.webr = null;
      this.initialized = false;
    }
  }
}
