Dasbor Interaktif Metode Statistika II
Deskripsi Proyek
AHA Analytics Dashboard adalah sebuah aplikasi web interaktif yang dibangun menggunakan R dan Shiny, dirancang sebagai alat bantu pembelajaran komprehensif untuk mata kuliah Metode Statistika II. Dasbor ini menyediakan ringkasan materi, kalkulator statistik interaktif, dan kuis latihan untuk setiap pertemuan, dari Pertemuan 1 hingga 14.

Tujuan utama proyek ini adalah untuk menciptakan pengalaman belajar yang lebih menarik dan praktis, di mana pengguna dapat langsung menerapkan konsep statistik yang dipelajari melalui analisis data secara real-time.

Fitur Utama
Dasbor ini dilengkapi dengan berbagai fitur untuk mendukung proses pembelajaran:

Materi Pembelajaran Lengkap: Ringkasan materi yang terstruktur untuk 14 pertemuan, mencakup semua topik utama dari klasifikasi statistika hingga ANCOVA.

Kalkulator Statistik Interaktif:

Kalkulator khusus untuk setiap metode statistik yang diajarkan di setiap pertemuan.

Mendukung input data secara manual maupun melalui unggah file (Excel, CSV, SPSS).

Menghasilkan laporan analisis yang detail dan terstruktur, mencakup tujuan, hipotesis, statistik deskriptif, hasil uji, dan interpretasi.

Dilengkapi visualisasi data dinamis menggunakan plotly dan ggplot2.

Unduh Laporan Analisis: Setiap hasil analisis dari kalkulator dapat diunduh dalam format Microsoft Word (.docx), lengkap dengan visualisasi data yang relevan.

Kuis Latihan Interaktif:

Kuis pilihan ganda dengan 5 soal untuk setiap pertemuan (total 14 kuis).

Tampilan kuis modern yang muncul dalam jendela pop-up.

Sistem penilaian otomatis dengan tampilan hasil dan kunci jawaban.

Fitur untuk mengunduh laporan hasil kuis dalam format Word.

Antarmuka Modern: Dibangun menggunakan argonDash untuk tampilan yang bersih, modern, dan responsif di berbagai perangkat.

Teknologi yang Digunakan
Proyek ini dibangun di atas ekosistem R dengan memanfaatkan beberapa paket kunci:

Framework Aplikasi: shiny, argonDash, shinydashboard

Visualisasi Data: ggplot2, plotly

Manipulasi Data: dplyr, tidyr

Pembacaan File: readxl, haven, foreign

Analisis Statistik: stats (bawaan R), car, nortest, tseries, randtests, BSDA, FSA, agricolae, emmeans

Pembuatan Laporan: rmarkdown, knitr

Tabel Interaktif: DT, rhandsontable

Cara Menjalankan Aplikasi
Untuk menjalankan dasbor ini di komputer lokal, ikuti langkah-langkah berikut:

Prasyarat:

Pastikan Anda telah menginstal R dan RStudio di komputer Anda.

Untuk fitur unduh laporan PDF (jika diaktifkan), pastikan Anda telah menginstal TinyTeX dengan menjalankan tinytex::install_tinytex() di R Console.

Instalasi Paket:
Buka RStudio dan jalankan perintah berikut di Console untuk menginstal semua paket yang diperlukan:

install.packages(c("shiny", "argonDash", "shinyWidgets", "htmltools", "shinydashboard", 
                   "DT", "plotly", "shinycssloaders", "readxl", "argonR", "randtests", 
                   "ggplot2", "stats19", "car", "DescTools", "dplyr", "tidyr", 
                   "PMCMRplus", "FSA", "ppcor", "foreign", "nortest", "haven", 
                   "agricolae", "rhandsontable", "emmeans", "rmarkdown", "tseries"))

Jalankan Aplikasi:

Buka file PROJEK KOMSTAT kelomppok 8.R di RStudio.

Klik tombol "Run App" yang muncul di bagian atas editor RStudio.

Kontributor
Proyek ini dikerjakan oleh Kelompok 8:

[Isi dengan nama anggota 1]

[Isi dengan nama anggota 2]

[Isi dengan nama anggota 3]

[dst...]