// sw.js
// StatLab Service Worker for Offline PWA Execution
// Strictly uses relative paths for GitHub Pages subpath compatibility.

const CACHE_NAME = "statlab-cache-v3";

const ASSETS_TO_CACHE = [
  "./",
  "./index.html",
  "./manifest.json",
  "./css/statlab.css",
  "./assets/icon-192.svg",
  "./assets/icon-512.svg",
  "./js/i18n.js",
  "./js/number-locale.js",
  "./js/webr-client.js",
  "./js/grid.js",
  "./js/roles.js",
  "./js/result-view.js",
  "./js/session-store.js",
  "./js/pdf-export.js",
  "./js/app.js",
  "./R/00_json.R",
  "./R/01_data_prep.R",
  "./R/02_assumptions.R",
  "./R/03_plots_base.R",
  "./R/04_descriptive.R",
  "./R/05_conclusions.R",
  "./R/06_anova.R",
  "./R/07_ttest.R",
  "./R/08_regression_chisq.R",
  "./R/99_dispatcher.R",
  "./vendor/katex/katex.min.css",
  "./vendor/katex/katex.min.js",
  "./vendor/pdfmake/pdfmake.min.js",
  "./vendor/pdfmake/vfs_fonts.js",
  "./vendor/webr/webr.mjs",
  "./vendor/webr/webr-worker.js",
  "./vendor/webr/webr-serviceworker.js",
  "./vendor/webr/R.js",
  "./vendor/webr/R.wasm",
  "./vendor/webr/libRblas.so",
  "./vendor/webr/libRlapack.so"
];

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(CACHE_NAME).then((cache) => {
      return cache.addAll(ASSETS_TO_CACHE).catch((err) => {
        console.warn("Some assets failed caching during install:", err);
      });
    }).then(() => self.skipWaiting())
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches.keys().then((keys) => {
      return Promise.all(
        keys.map((key) => {
          if (key !== CACHE_NAME) {
            return caches.delete(key);
          }
        })
      );
    }).then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  // Only handle GET requests
  if (event.request.method !== "GET") return;

  const url = new URL(event.request.url);

  // Cache-first ONLY for heavy binary wasm/so files
  if (url.pathname.endsWith(".wasm") || url.pathname.endsWith(".so")) {
    event.respondWith(
      caches.match(event.request).then((cached) => {
        return cached || fetch(event.request).then((response) => {
          if (response && response.status === 200) {
            const respClone = response.clone();
            caches.open(CACHE_NAME).then((cache) => cache.put(event.request, respClone));
          }
          return response;
        });
      })
    );
    return;
  }

  // Network-first with cache fallback for all scripts and R modules
  event.respondWith(
    fetch(event.request)
      .then((networkResponse) => {
        if (networkResponse && networkResponse.status === 200) {
          const respClone = networkResponse.clone();
          caches.open(CACHE_NAME).then((cache) => cache.put(event.request, respClone));
        }
        return networkResponse;
      })
      .catch(() => caches.match(event.request))
  );
});
