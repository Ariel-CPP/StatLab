// tools/vendor-fetch.mjs
// StatLab Vendor Assets Downloader & Extractor
// Fetches webr@0.5.4, katex@0.19.1, pdfmake@0.3.13, and r-wasm package binaries.
// Zero frontend build steps. Enforces < 50MB per file for GitHub Pages.

import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { execSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = path.resolve(__dirname, "..");
const VENDOR_DIR = path.join(ROOT_DIR, "vendor");

const MAX_FILE_SIZE = 50 * 1024 * 1024; // 50 MB GitHub limit

async function downloadToFile(url, destPath) {
  console.log(`Downloading: ${url} ...`);
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(`Failed to download ${url}: HTTP ${res.status} ${res.statusText}`);
  }
  const arrayBuf = await res.arrayBuffer();
  fs.mkdirSync(path.dirname(destPath), { recursive: true });
  fs.writeFileSync(destPath, Buffer.from(arrayBuf));
  console.log(`Saved: ${destPath} (${(arrayBuf.byteLength / (1024 * 1024)).toFixed(2)} MB)`);
}

function copyRecursive(src, dest) {
  const stat = fs.statSync(src);
  if (stat.isDirectory()) {
    fs.mkdirSync(dest, { recursive: true });
    for (const item of fs.readdirSync(src)) {
      copyRecursive(path.join(src, item), path.join(dest, item));
    }
  } else {
    fs.mkdirSync(path.dirname(dest), { recursive: true });
    fs.copyFileSync(src, dest);
  }
}

function assertVendorSizes(dir) {
  let fileCount = 0;
  let totalBytes = 0;
  function traverse(currentDir) {
    for (const item of fs.readdirSync(currentDir, { withFileTypes: true })) {
      const fullPath = path.join(currentDir, item.name);
      if (item.isDirectory()) {
        traverse(fullPath);
      } else {
        const sz = fs.statSync(fullPath).size;
        fileCount++;
        totalBytes += sz;
        if (sz >= MAX_FILE_SIZE) {
          throw new Error(`File exceeds 50MB limit: ${fullPath} (${(sz / (1024 * 1024)).toFixed(2)} MB)`);
        }
      }
    }
  }
  traverse(dir);
  console.log(`Verified ${fileCount} vendored files. Total size: ${(totalBytes / (1024 * 1024)).toFixed(2)} MB. All files < 50 MB.`);
}

async function main() {
  console.log("=== StatLab Vendor Assets Fetcher ===");
  fs.mkdirSync(VENDOR_DIR, { recursive: true });

  const tmpWork = path.join(os.tmpdir(), `statlab-vendor-${Date.now()}`);
  fs.mkdirSync(tmpWork, { recursive: true });

  try {
    // 1. webR 0.5.4
    console.log("\n--- Processing webR@0.5.4 ---");
    const webrTgz = path.join(tmpWork, "webr.tgz");
    await downloadToFile("https://registry.npmjs.org/webr/-/webr-0.5.4.tgz", webrTgz);
    const webrExtract = path.join(tmpWork, "webr-pkg");
    fs.mkdirSync(webrExtract, { recursive: true });
    execSync(`tar -xzf "${webrTgz}" -C "${webrExtract}"`);

    const webrDist = path.join(webrExtract, "package", "dist");
    const webrVendorDest = path.join(VENDOR_DIR, "webr");
    fs.mkdirSync(webrVendorDest, { recursive: true });
    copyRecursive(webrDist, webrVendorDest);
    // Write package.json in vendor/webr so Node treats its workers as CommonJS
    fs.writeFileSync(path.join(webrVendorDest, "package.json"), JSON.stringify({ type: "commonjs" }, null, 2));
    console.log(`Copied webR dist -> ${webrVendorDest}`);

    // Check if survival, cluster, MASS are in default library
    const vfsLib = path.join(webrVendorDest, "vfs", "usr", "lib", "R", "library");
    const hasSurvival = fs.existsSync(path.join(vfsLib, "survival"));
    const hasCluster = fs.existsSync(path.join(vfsLib, "cluster"));
    const hasMass = fs.existsSync(path.join(vfsLib, "MASS"));
    console.log(`webR default VFS check: survival=${hasSurvival}, cluster=${hasCluster}, MASS=${hasMass}`);

    // 2. KaTeX 0.19.1
    console.log("\n--- Processing katex@0.19.1 ---");
    const katexTgz = path.join(tmpWork, "katex.tgz");
    await downloadToFile("https://registry.npmjs.org/katex/-/katex-0.19.1.tgz", katexTgz);
    const katexExtract = path.join(tmpWork, "katex-pkg");
    fs.mkdirSync(katexExtract, { recursive: true });
    execSync(`tar -xzf "${katexTgz}" -C "${katexExtract}"`);

    const katexDist = path.join(katexExtract, "package", "dist");
    const katexVendorDest = path.join(VENDOR_DIR, "katex");
    fs.mkdirSync(katexVendorDest, { recursive: true });

    // katex.min.js, katex.min.css
    fs.copyFileSync(path.join(katexDist, "katex.min.js"), path.join(katexVendorDest, "katex.min.js"));
    fs.copyFileSync(path.join(katexDist, "katex.min.css"), path.join(katexVendorDest, "katex.min.css"));

    // contrib/auto-render.min.js
    const autoRenderSrc = path.join(katexDist, "contrib", "auto-render.min.js");
    if (fs.existsSync(autoRenderSrc)) {
      const contribDir = path.join(katexVendorDest, "contrib");
      fs.mkdirSync(contribDir, { recursive: true });
      fs.copyFileSync(autoRenderSrc, path.join(contribDir, "auto-render.min.js"));
      fs.copyFileSync(autoRenderSrc, path.join(katexVendorDest, "auto-render.min.js"));
    }

    // fonts/*.woff2
    const fontsSrc = path.join(katexDist, "fonts");
    const fontsDest = path.join(katexVendorDest, "fonts");
    fs.mkdirSync(fontsDest, { recursive: true });
    if (fs.existsSync(fontsSrc)) {
      for (const f of fs.readdirSync(fontsSrc)) {
        if (f.endsWith(".woff2")) {
          fs.copyFileSync(path.join(fontsSrc, f), path.join(fontsDest, f));
        }
      }
    }
    console.log(`Copied KaTeX assets -> ${katexVendorDest}`);

    // 3. pdfmake 0.3.13
    console.log("\n--- Processing pdfmake@0.3.13 ---");
    const pdfmakeTgz = path.join(tmpWork, "pdfmake.tgz");
    await downloadToFile("https://registry.npmjs.org/pdfmake/-/pdfmake-0.3.13.tgz", pdfmakeTgz);
    const pdfmakeExtract = path.join(tmpWork, "pdfmake-pkg");
    fs.mkdirSync(pdfmakeExtract, { recursive: true });
    execSync(`tar -xzf "${pdfmakeTgz}" -C "${pdfmakeExtract}"`);

    const pdfmakeBuild = path.join(pdfmakeExtract, "package", "build");
    const pdfmakeVendorDest = path.join(VENDOR_DIR, "pdfmake");
    fs.mkdirSync(pdfmakeVendorDest, { recursive: true });

    fs.copyFileSync(path.join(pdfmakeBuild, "pdfmake.min.js"), path.join(pdfmakeVendorDest, "pdfmake.min.js"));
    fs.copyFileSync(path.join(pdfmakeBuild, "vfs_fonts.js"), path.join(pdfmakeVendorDest, "vfs_fonts.js"));
    console.log(`Copied pdfmake build -> ${pdfmakeVendorDest}`);

    // 4. Recommended R-wasm packages (survival, cluster, MASS + dependencies lattice, Matrix)
    console.log("\n--- Processing R-wasm packages into vendor/r-repo/ ---");
    const rRepoDest = path.join(VENDOR_DIR, "r-repo");
    fs.mkdirSync(rRepoDest, { recursive: true });

    const rWasmRepoUrl = "https://repo.r-wasm.org/bin/emscripten/contrib/4.5";
    const rPkgs = [
      { name: "lattice", file: "lattice_0.22-9.tgz" },
      { name: "Matrix", file: "Matrix_1.7-5.tgz" },
      { name: "survival", file: "survival_3.8-6.tgz" },
      { name: "cluster", file: "cluster_2.1.8.2.tgz" },
      { name: "MASS", file: "MASS_7.3-65.tgz" }
    ];

    for (const pkg of rPkgs) {
      const dest = path.join(rRepoDest, pkg.file);
      await downloadToFile(`${rWasmRepoUrl}/${pkg.file}`, dest);
      // Convenience unversioned copy
      fs.copyFileSync(dest, path.join(rRepoDest, `${pkg.name}.tgz`));
    }

    // Also download PACKAGES file for standard CRAN/repo structure
    await downloadToFile(`${rWasmRepoUrl}/PACKAGES`, path.join(rRepoDest, "PACKAGES"));

    // Write a manifest.json
    fs.writeFileSync(
      path.join(rRepoDest, "manifest.json"),
      JSON.stringify({
        r_version: "4.5",
        packages: rPkgs.map(p => ({ name: p.name, file: p.file, generic: `${p.name}.tgz` }))
      }, null, 2)
    );
    console.log(`Saved R-wasm binaries & manifest -> ${rRepoDest}`);

    // 5. Verification: All vendor files < 50MB
    console.log("\n--- Verifying file sizes in vendor/ ---");
    assertVendorSizes(VENDOR_DIR);

    console.log("\n=== Vendor assets successfully fetched and verified ===");
  } finally {
    try {
      fs.rmSync(tmpWork, { recursive: true, force: true });
    } catch {
      // ignore
    }
  }
}

main().catch(err => {
  console.error("FATAL ERROR in vendor-fetch.mjs:", err);
  process.exit(1);
});
