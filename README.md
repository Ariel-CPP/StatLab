# 🧪 StatLab — Pure Base R Biostatistics Notebook PWA

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![webR](https://img.shields.io/badge/Powered%20by-webR%20WASM-1B365D.svg)](https://docs.r-wasm.org/webr/latest/)
[![R-Parity](https://img.shields.io/badge/Desktop%20R%20Parity-%E2%89%A4%201e--8-2E7D32.svg)]()
[![PWA Ready](https://img.shields.io/badge/PWA-Offline%20Ready-B83227.svg)]()

> **StatLab** adalah platform komputasi biostatistika dan rancangan percobaan mandiri berbasis browser (*client-side Progressive Web App*). Seluruh perhitungan matematika statistik, kurva pemodelan, pengujian asumsi, dan visualisasi grafik 300-DPI dieksekusi 100% menggunakan otoritas matematika **murni Base R** melalui teknologi **WebAssembly (webR)** secara instan tanpa memerlukan server backend eksternal.

---

## 🌟 Keunggulan Utama (Key Features)

### 1. 🔒 100% Client-Side, Privat & Offline PWA
- Data riset Anda tidak pernah dikirim ke server luar manapun. Seluruh proses analisis dijalankan langsung di RAM browser pengguna.
- Mendukung pemasangan aplikasi (*Installable PWA*) di Windows, macOS, Linux, Android, dan iOS serta dapat digunakan dalam kondisi offline tanpa koneksi internet.

### 2. 🛡️ Pengujian Asumsi Wajib Terlebih Dahulu (*Assumption-First Architecture*)
- Otomatis melakukan evaluasi normalitas residu dan kelompok (**Shapiro-Wilk** untuk $3 \le N \le 5000$ dan **Lilliefors Dallal-Wilkinson** untuk $N > 5000$).
- Otomatis menguji homogenitas varians (**Brown-Forsythe Levene** berbasis median dan **Bartlett Test**).
- **Smart Assumption Switcher**: Menyediakan tombol pintas 1-klik untuk beralih secara cerdas ke padanan uji non-parametrik jika asumsi distribusi data terlanggar.

### 3. 🔬 Rangkaian Modul Analisis Biostatistika Lengkap
- **Statistika Parametrik Dasar**:
  - Statistik Deskriptif (Mean, SD, SE, Median, IQR, Skewness, Kurtosis, Selang Kepercayaan 95%).
  - Uji-t Dua Sampel Bebas (Student's $t$ & Welch's $t$ saat varians heterogen).
  - Uji-t Berpasangan (Pre-test vs Post-test).
  - ANOVA Satu Arah (*One-Way ANOVA*) dengan uji lanjut **Tukey HSD** dan **Duncan Multiple Range Test**.
  - ANOVA Dua Arah Faktorial (*Two-Way ANOVA*) dengan evaluasi interaksi.
  - *Repeated Measures ANOVA* dengan koreksi sferisitas **Greenhouse-Geisser**.
- **Paket Non-Parametrik Lengkap (*Distribution-Free*)**:
  - **Kruskal-Wallis Test** dilengkapi *post-hoc* **Dunn Multiple Comparisons** (koreksi Bonferroni).
  - **Mann-Whitney U Test** (Wilcoxon Rank-Sum) untuk 2 sampel independen.
  - **Wilcoxon Signed-Rank Test** untuk sampel berpasangan.
  - **Friedman Test** untuk pengukuran berulang non-parametrik.
- **Rancangan Percobaan (*Experimental Design*)**:
  - **Rancangan Acak Kelompok Lengkap (RAKL / RCBD)**: Partisi ANOVA perlakuan & blok, Koefisien Keragaman ($KK$), BNT / LSD, dan Tukey HSD.
  - **Rancangan Petak Terbagi (*Split-Plot Design*)**: Stratifikasi model dua galat (Petak Utama diuji terhadap Galat a; Anak Petak & Interaksi diuji terhadap Galat b), $KK(a)$, $KK(b)$, dan grafik interaksi.
- **Toksikologi & Bioassay Dosis-Respon**:
  - Regresi Binomial Probit dan Logit untuk estimasi konsentrasi letal:
    $$\text{LC}_{10}, \quad \text{LC}_{50}, \quad \text{LC}_{90}$$
  - **Selang Kepercayaan 95% Eksak Teorema Fieller** untuk rasio parameter.
  - Uji kesesuaian Pearson Chi-Square ($\chi^2$) Goodness-of-Fit dan kurva respon sigmoidal 300-DPI.
- **Regresi, Multivariat & Survival**:
  - **Estimasi Kurva Non-Linier**: Evaluasi serentak 5 kurva (Linier, Kuadratik dengan titik puncak/optimum, Power/Alometrik $W = a L^b$, Eksponensial, Logaritmik) dengan ranking AIC dan $R^2$.
  - **Regresi Logistik Biner**: Estimasi Rasio Odds ($OR = e^\beta$), 95% CI, Hosmer-Lemeshow, Pseudo-$R^2$ Nagelkerke, dan kurva ROC/AUC.
  - **Matriks Korelasi & Heatmap**: Matriks Pearson & Spearman dengan p-value dan **Heatmap Publikasi 300-DPI** murni Base Graphics.
  - **MANOVA (Multivariate ANOVA)**: Pillai's Trace, Wilks' Lambda, Hotelling-Lawley, Roy's Largest Root dengan penanganan multikolinearitas singular.
  - **Survival Analysis (Kaplan-Meier)**: Kurva sintasan bertingkat dan uji Log-Rank.
  - **PCA, HCA & K-Means Clustering**: Biplot PCA, Dendrogram Hirarki Ward.D2, penentuan klaster otomatis, dan ANOVA profil pembeda klaster.

### 4. 📄 Standar Publikasi & Pelaporan Riset
- **Tabel 2-Lapis**: Format ringkas standar Jurnal Internasional (APA Style) dan tabel detail diagnostik lengkap.
- **Grafik Beresolusi Tinggi 300-DPI**: Format PNG berkualitas cetak dengan palet warna akademik elegan.
- **Bilingual Key Points**: Kesimpulan tersusun dalam bentuk kartu poin kunci terstruktur (Bahasa Indonesia & English) tanpa mengaburkan konteks biologis.
- **Laporan Sesi Kumulatif & Ekspor PDF Vektor**: Menggabungkan seluruh tahapan analisis ke dalam satu dokumen PDF siap cetak melalui `pdfmake`.

---

## 🚀 Menjalankan Secara Lokal

Pastikan Anda telah menginstal [Node.js](https://nodejs.org/).

```bash
# 1. Clone repositori
git clone https://github.com/<USERNAME>/statlab.git
cd statlab

# 2. Jalankan server lokal
node tools/serve.mjs

# 3. Buka di browser
# http://localhost:8080/
```

---

## 🌐 Deploy Gratis ke GitHub Pages

Proyek ini telah dikonfigurasi secara baku untuk berjalan langsung di GitHub Pages:
1. Buat repositori baru di GitHub dengan status **Public**.
2. Push repositori ini ke GitHub (`git remote add origin ...` lalu `git push -u origin main`).
3. Buka **Settings > Pages** di repositori GitHub Anda.
4. Pilih **Source: Deploy from a branch**, pilih branch `main` dan folder `/ (root)`.
5. Website Anda akan langsung online dalam 1 menit di `https://<USERNAME>.github.io/<REPO>/` tanpa biaya apapun!

---

## 📜 Lisensi & Atribusi

Dikembangkan dengan dedikasi untuk mendukung penelitian sains hayati, perikanan, biologi, kedokteran, dan agronomi yang presisi, transparan, dan berintegritas saintifik tinggi.

- **Engine Komputasi**: Pure Base R via [webR](https://docs.r-wasm.org/webr/latest/) (The R Project for Statistical Computing).
- **Lisensi**: MIT License. Bebas digunakan untuk keperluan riset akademis, praktikum, publikasi jurnal, maupun komersial.
