# METSTATII-AHA
# Dasbor Interaktif Metode Statistika II

## Deskripsi Proyek

**AHA Analytics Dashboard** adalah sebuah aplikasi web interaktif yang dibangun menggunakan R dan Shiny, dirancang sebagai alat bantu pembelajaran komprehensif untuk mata kuliah Metode Statistika II. Dasbor ini menyediakan ringkasan materi, kalkulator statistik interaktif, dan kuis latihan untuk setiap pertemuan, dari Pertemuan 1 hingga 14.

Tujuan utama proyek ini adalah untuk menciptakan pengalaman belajar yang lebih menarik dan praktis, di mana pengguna dapat langsung menerapkan konsep statistik yang dipelajari melalui analisis data secara real-time.

## Fitur Utama

Dasbor ini dilengkapi dengan berbagai fitur untuk mendukung proses pembelajaran:

* **Materi Pembelajaran Lengkap**: Ringkasan materi yang terstruktur untuk 14 pertemuan, mencakup semua topik utama dari klasifikasi statistika hingga ANCOVA.

* **Kalkulator Statistik Interaktif**: Kalkulator khusus untuk setiap metode statistik yang diajarkan di setiap pertemuan, dengan rincian sebagai berikut:

    * **Pertemuan 1**: Tidak ada kalkulator.
    * **Pertemuan 2**: Estimasi Parameter (Rata-rata, Varian, Proporsi) untuk 1 & 2 Populasi.
    * **Pertemuan 3**: Uji Hipotesis 1 Populasi (Mean, Proporsi, Varian).
    * **Pertemuan 4**: Uji Hipotesis 2 Populasi (Beda Rata-rata, Rasio Varian, Beda Proporsi).
    * **Pertemuan 5**: Uji Normalitas (Shapiro-Wilk, Lilliefors, Jarque-Bera).
    * **Pertemuan 6**: Uji Kesamaan Varians (Bartlett, Levene).
    * **Pertemuan 7**: ANOVA Satu Arah (dengan Uji Lanjut Tukey/Duncan).
    * **Pertemuan 8**: ANOVA Dua Arah (dengan & tanpa interaksi, Uji Lanjut Tukey).
    * **Pertemuan 9**: Uji Proporsi Beberapa Populasi (Chi-Square).
    * **Pertemuan 10**: Uji Nonparametrik 1 Sampel (Uji Tanda, Uji Tanda Berpasangan, Uji Keacakan).
    * **Pertemuan 11**: Uji Nonparametrik 2 Sampel (Wilcoxon Signed-Rank, Mann-Whitney, Kolmogorov-Smirnov).
    * **Pertemuan 12**: Uji Nonparametrik >2 Sampel (Kruskal-Wallis, Friedman).
    * **Pertemuan 13**: Uji Korelasi (Pearson, Spearman) & Uji Kebebasan (Chi-Square).
    * **Pertemuan 14**: Analysis of Covariance (ANCOVA).

* **Dukungan Input Data Fleksibel**: Mendukung input data secara manual maupun melalui unggah file (Excel, CSV, SPSS).

* **Laporan Analisis Detail**: Menghasilkan laporan analisis yang terstruktur, mencakup tujuan, hipotesis, statistik deskriptif, hasil uji, dan interpretasi kontekstual.

* **Visualisasi Data Dinamis**: Dilengkapi visualisasi data yang relevan untuk setiap analisis menggunakan `plotly` dan `ggplot2`.

* **Unduh Laporan Analisis**: Setiap hasil analisis dari kalkulator dapat diunduh dalam format **Microsoft Word (.docx)**, lengkap dengan visualisasi data.

* **Kuis Latihan Interaktif**:
    * Kuis pilihan ganda dengan 5 soal untuk setiap pertemuan.
    * Tampilan kuis modern yang muncul dalam jendela pop-up.
    * Sistem penilaian otomatis dengan tampilan hasil dan kunci jawaban.
    * Fitur untuk mengunduh laporan hasil kuis dalam format Word.

* **Antarmuka Modern**: Dibangun menggunakan `argonDash` untuk tampilan yang bersih, modern, dan responsif di berbagai perangkat.

## Teknologi yang Digunakan

Proyek ini dibangun di atas ekosistem R dengan memanfaatkan beberapa paket kunci:

* **Framework Aplikasi**: `shiny`, `argonDash`, `shinydashboard`
* **Visualisasi Data**: `ggplot2`, `plotly`
* **Manipulasi Data**: `dplyr`, `tidyr`
* **Pembacaan File**: `readxl`, `haven`, `foreign`
* **Analisis Statistik**: `stats` (bawaan R), `car`, `nortest`, `tseries`, `randtests`, `BSDA`, `FSA`, `agricolae`, `emmeans`
* **Pembuatan Laporan**: `rmarkdown`, `knitr`
* **Tabel Interaktif**: `DT`, `rhandsontable`

## Cara Menjalankan Aplikasi

Untuk menjalankan dasbor ini di komputer lokal, ikuti langkah-langkah berikut:

1.  **Prasyarat**:
    * Pastikan Anda telah menginstal **R** dan **RStudio** di komputer Anda.
    * Untuk fitur unduh laporan PDF (jika diaktifkan), pastikan Anda telah menginstal **TinyTeX** dengan menjalankan `tinytex::install_tinytex()` di R Console.

2.  **Instalasi Paket**:
    Buka RStudio dan jalankan perintah berikut di Console untuk menginstal semua paket yang diperlukan:
    ```r
    install.packages(c("shiny", "argonDash", "shinyWidgets", "htmltools", "shinydashboard", 
                       "DT", "plotly", "shinycssloaders", "readxl", "argonR", "randtests", 
                       "ggplot2", "stats19", "car", "DescTools", "dplyr", "tidyr", 
                       "PMCMRplus", "FSA", "ppcor", "foreign", "nortest", "haven", 
                       "agricolae", "rhandsontable", "emmeans", "rmarkdown", "tseries"))
    ```

3.  **Jalankan Aplikasi**:
    * Buka file `PROJEK KOMSTAT kelomppok 8.R` di RStudio.
    * Klik tombol **"Run App"** yang muncul di bagian atas editor RStudio.

## Kontributor

Proyek ini dikerjakan oleh Kelompok 8:

* *\[ABDUL HANIF AL-FATAH\]*
* *\[HANIF JAWAHIR\]*
* *\[M. ARKAN ANZUYE\]*
