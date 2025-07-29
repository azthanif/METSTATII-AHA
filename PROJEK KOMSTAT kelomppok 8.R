library(shiny)
library(argonDash)
library(shinyWidgets)
library(htmltools)
library(shinydashboard) # Diperlukan untuk fungsi box()
library(DT)
library(plotly)
library(shinycssloaders)
library(readxl)
library(argonR) # Diperlukan untuk argonTabSet
library(randtests) # pertemuan 10
library(ggplot2)
library(readxl)
library(stats19)  # Pustaka untuk fungsi statistik seperti median
library(car)
library(DescTools)
library(dplyr)
library(tidyr)
library(PMCMRplus) # Untuk post-hoc tests seperti Dunn test dan Nemenyi test
library(FSA) # Diminta oleh pengguna, bisa digunakan untuk dunnTest juga, tapi PMCMRplus lebih umum untuk Nemenyi
library(ppcor) # Untuk korelasi parsial (Kendall Tau Partial)
library(foreign) # Untuk membaca file .sav
library(nortest) # Untuk uji Lilliefors
library(tseries) # Diperlukan untuk Uji Jarque-Bera
library(haven) # Untuk membaca file SPSS (.sav)
library(agricolae) # Untuk Uji Duncan
library(rhandsontable) # For direct data input
library(emmeans) # For post-hoc comparisons in ANCOVA
library(rmarkdown) # Diperlukan untuk unduh laporan PDF

# Vektor ini akan menjadi satu-satunya sumber judul untuk sidebar dan daftar isi
daftar_judul_pertemuan <- c(
  "Pertemuan 1: Klasifikasi Statistika",
  "Pertemuan 2: Estimasi Parameter",
  "Pertemuan 3: Uji Hipotesis 1 Populasi",
  "Pertemuan 4: Uji Hipotesis 2 Populasi",
  "Pertemuan 5: Uji Kesesuaian Sebaran Normal",
  "Pertemuan 6: Uji Kesamaan Varians",
  "Pertemuan 7: ANOVA Satu Arah",
  "Pertemuan 8: ANOVA Dua Arah",
  "Pertemuan 9: Uji Proporsi Beberapa Populasi",
  "Pertemuan 10: Uji Nonparametrik 1 Sampel",
  "Pertemuan 11: Uji Nonparametrik 2 Sampel",
  "Pertemuan 12: Uji Nonparametrik >2 Sampel",
  "Pertemuan 13: Uji Korelasi",
  "Pertemuan 14: Analysis of Covariance (ANCOVA)"
)

# Helper function to format p-values for display
format.p <- function(p) {
  if (is.null(p) || is.na(p)) {
    return("N/A")
  } else if (p < 0.001) {
    return("< 0.001")
  } else {
    return(as.character(round(p, 3)))
  }
}

# UI LENGKAP SUDAH DIPERBARUI
ui <- argonDashPage(
  title = "AHA Analytics Dashboard",
  header = argonDashHeader(
    h1("AHA Analytics Dashboard", style = "color: white;"),
    gradient = TRUE, color = "primary", separator = TRUE, separator_color = "secondary"
  ),
  sidebar = argonDashSidebar(
    skin = "light", id = "sidebarMenu",
    h5("Menu Utama", class = "navbar-heading p-0"),
    argonSidebarMenu(
      argonSidebarItem(tabName = "beranda", icon = icon("home"), "Beranda"),
      argonSidebarItem(
        tabName = "pertemuan_1", # Sesuaikan dengan nama tab
        icon = icon("book"),
        "Pertemuan 1"
      ),
      argonSidebarItem(
        tabName = "pertemuan_2",
        icon = icon("book"),
        "Pertemuan 2"
      ),
      argonSidebarItem(
        tabName = "pertemuan_3",
        icon = icon("book"),
        "Pertemuan 3"
      ),
      argonSidebarItem(
        tabName = "pertemuan_4",
        icon = icon("book"),
        "Pertemuan 4"
      ),
      argonSidebarItem(
        tabName = "pertemuan_5",
        icon = icon("book"),
        "Pertemuan 5"
      ),
      argonSidebarItem(
        tabName = "pertemuan_6",
        icon = icon("book"),
        "Pertemuan 6"
      ),
      argonSidebarItem(
        tabName = "pertemuan_7",
        icon = icon("book"),
        "Pertemuan 7"
      ),
      argonSidebarItem(
        tabName = "pertemuan_8",
        icon = icon("book"),
        "Pertemuan 8"
      ),
      argonSidebarItem(
        tabName = "pertemuan_9",
        icon = icon("book"),
        "Pertemuan 9"
      ),
      argonSidebarItem(
        tabName = "pertemuan_10",
        icon = icon("book"),
        "Pertemuan 10"
      ),
      argonSidebarItem(
        tabName = "pertemuan_11",
        icon = icon("book"),
        "Pertemuan 11"
      ),
      argonSidebarItem(
        tabName = "pertemuan_12",
        icon = icon("book"),
        "Pertemuan 12"
      ),
      argonSidebarItem(
        tabName = "pertemuan_13",
        icon = icon("book"),
        "Pertemuan 13"
      ),
      argonSidebarItem(
        tabName = "pertemuan_14",
        icon = icon("book"),
        "Pertemuan 14"
      ),
    )
  ),
  body = argonDashBody(
    # CSS Custom dari UI baru
    tags$head(
      tags$style(
        HTML(
          "
      .box-header { background-color: #3b82f6; color: white; }
      .btn-primary { background-color: #1e3a8a; border-color: #1e3a8a; }
      /* Modern styling untuk .nav-link-custom */
      .nav-link-custom {
        display: inline-block !important;
        padding: 12px 18px !important;
        color: #4a5568 !important;
        text-decoration: none !important;
        font-weight: 500 !important;
        font-size: 15px !important;
        border-radius: 8px !important;
        background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
        border: 1px solid #e2e8f0 !important;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
        position: relative !important;
        overflow: hidden !important;
        margin: 2px 0 !important;
        cursor: pointer !important;
      }

      /* Efek hover modern */
      .nav-link-custom:hover {
        color: #ffffff !important;
        text-decoration: none !important;
        transform: translateY(-2px) scale(1.02) !important;
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.3) !important;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        border-color: #667eea !important;
      }

      /* Efek shimmer saat hover */
      .nav-link-custom::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg,
          transparent,
          rgba(255, 255, 255, 0.2),
          transparent
        );
        transition: left 0.6s ease;
        z-index: 1;
      }

      .nav-link-custom:hover::before {
        left: 100%;
      }

      /* Efek glow saat hover */
      .nav-link-custom::after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        width: 0;
        height: 0;
        background: radial-gradient(circle, rgba(255, 255, 255, 0.3) 0%, transparent 70%);
        transform: translate(-50%, -50%);
        transition: all 0.3s ease;
        border-radius: 50%;
        z-index: 1;
      }

      .nav-link-custom:hover::after {
        width: 120px;
        height: 120px;
      }

      /* Efek active/focus */
      .nav-link-custom:active {
        transform: translateY(0) scale(0.98) !important;
        box-shadow: 0 4px 15px rgba(102, 126, 234, 0.2) !important;
      }

      .nav-link-custom:focus {
        outline: none !important;
        box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.2) !important;
      }

      /* Variasi warna untuk different states */
      .nav-link-custom.success:hover {
        background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
        border-color: #11998e !important;
        box-shadow: 0 8px 25px rgba(17, 153, 142, 0.3) !important;
      }

      .nav-link-custom.warning:hover {
        background: linear-gradient(135deg, #ff6b6b 0%, #ffa500 100%) !important;
        border-color: #ff6b6b !important;
        box-shadow: 0 8px 25px rgba(255, 107, 107, 0.3) !important;
      }

      .nav-link-custom.info:hover {
        background: linear-gradient(135deg, #2193b0 0%, #6dd5ed 100%) !important;
        border-color: #2193b0 !important;
        box-shadow: 0 8px 25px rgba(33, 147, 176, 0.3) !important;
      }

      /* Responsive adjustments */
      @media (max-width: 768px) {
        .nav-link-custom {
          padding: 10px 14px !important;
          font-size: 14px !important;
        }

        .nav-link-custom:hover {
          transform: translateY(-1px) scale(1.01) !important;
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.25) !important;
        }
      }

      /* Untuk penggunaan dalam list */
      ol li .nav-link-custom,
      ul li .nav-link-custom {
        display: block !important;
        width: 100% !important;
        text-align: left !important;
        margin: 0 0 8px 0 !important;
      }

      /* Loading state */
      .nav-link-custom.loading {
        pointer-events: none !important;
        opacity: 0.7 !important;
      }

      .nav-link-custom.loading::before {
        animation: shimmer 1.5s infinite linear;
      }

      @keyframes shimmer {
        0% { left: -100%; }
        100% { left: 100%; }
      }

      /* Aturan baru untuk menghilangkan marker angka pada list */
      ol.no-markers {
        list-style-type: none !important;
        padding-left: 0 !important; /* Menghapus padding kiri default dari list */
      }

      ol li {
        padding: 4px 0;
      }

        /* Modern Info Cards Styling */
        .argon-card {
          border-radius: 12px !important;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08) !important;
          border: none !important;
          transition: all 0.3s ease !important;
          backdrop-filter: blur(10px);
          background: rgba(255, 255, 255, 0.95) !important;
        }

        .argon-card:hover {
          transform: translateY(-5px) !important;
          box-shadow: 0 8px 30px rgba(0, 0, 0, 0.12) !important;
        }

        .card-stats .icon {
          text-align: center !important;
          margin: 0 auto 15px auto !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          width: 60px !important;
          height: 60px !important;
          border-radius: 50% !important;
          font-size: 24px !important;
        }

        .card-stats .icon.icon-shape-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          color: white !important;
        }

        .card-stats .icon.icon-shape-success {
          background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
          color: white !important;
        }

        .card-stats .icon.icon-shape-warning {
          background: linear-gradient(135deg, #ff6b6b 0%, #ffa500 100%) !important;
          color: white !important;
        }

        .card-stats .card-body {
          text-align: center !important;
          padding: 25px 20px !important;
        }

        .card-stats h3 {
          font-size: 28px !important;
          font-weight: 700 !important;
          margin-bottom: 8px !important;
          color: #2d3748 !important;
        }

        .card-stats h5 {
          font-size: 16px !important;
          font-weight: 600 !important;
          color: #4a5568 !important;
          margin-bottom: 12px !important;
          text-transform: uppercase !important;
          letter-spacing: 1px !important;
        }

        .card-stats p {
          font-size: 14px !important;
          color: #718096 !important;
          line-height: 1.6 !important;
          margin-bottom: 0 !important;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
          .card-stats .icon {
            width: 50px !important;
            height: 50px !important;
            font-size: 20px !important;
          }

          .card-stats h3 {
            font-size: 24px !important;
          }

          .card-stats h5 {
            font-size: 14px !important;
          }

          .card-stats p {
            font-size: 13px !important;
          }
        }

        /* CSS FOOTER MODERN */
        .footer {
          /* Gradient Background Modern */
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;

          /* Option 4: Dark Modern */
          /* background: linear-gradient(135deg, #232526 0%, #414345 100%) !important; */

          /* Tambahan efek visual */
          box-shadow: 0 -4px 20px rgba(0, 0, 0, 0.1);
          backdrop-filter: blur(10px);

          color: #ffffff !important;
          padding: 30px 30px !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
          border-top: 1px solid rgba(255, 255, 255, 0.1);
          width: 100vw;
          position: relative;
          left: 50%;
          transform: translateX(-50%);

          /* Animation */
          background-size: 200% 200%;
          animation: gradientShift 8s ease infinite;
        }

        /* Animasi gradient bergerak */
        @keyframes gradientShift {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }

        .footer-heading {
          color: #ffffff !important;
          text-transform: uppercase;
          letter-spacing: 2px;
          font-size: 15px;
          font-weight: 700;
          margin-bottom: 18px;
          text-shadow: 0 2px 4px rgba(0, 0, 0, 0.3);
        }

        .footer a {
          color: #e2e8f0;
          transition: all 0.3s ease-in-out;
          text-decoration: none;
        }

        .footer a:hover {
          color: #ffffff;
          text-decoration: none;
          text-shadow: 0 0 8px rgba(255, 255, 255, 0.5);
          transform: translateY(-1px);
        }

        .footer p, .footer li {
          margin-bottom: 8px;
          font-size: 14px;
          color: #e2e8f0;
          line-height: 1.6;
        }

        .footer ul {
          list-style-type: none;
          padding-left: 0;
        }

        /* Efek hover untuk list items */
        .footer ul li {
          padding: 4px 0;
          transition: all 0.2s ease;
        }

        .footer ul li:hover {
          padding-left: 8px;
          transform: translateX(4px);
        }

        /* Responsive design */
        @media (max-width: 768px) {
          .footer {
            padding: 20px 15px !important;
          }

          .footer-heading {
            font-size: 14px;
            margin-bottom: 12px;
          }

          .footer p, .footer li {
            font-size: 13px;
          }
        }

        /* CSS FOOTER MODERN END */
        /* =================================== */
        /* === CSS BARU UNTUK KUIS MODERN === */
        /* =================================== */

        /* Style untuk modal kuis secara keseluruhan */
        .quiz-modal .modal-content {
          border-radius: 15px;
          border: none;
          box-shadow: 0 10px 30px rgba(0,0,0,0.1);
          overflow: hidden; /* Mencegah gradien header keluar dari border-radius */
        }

        .quiz-modal .modal-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-bottom: none;
          padding: 20px 30px;
        }

        .quiz-modal .modal-header .modal-title {
          font-weight: 600;
          font-size: 22px;
        }

        .quiz-modal .modal-header .close {
          color: white;
          opacity: 0.8;
          text-shadow: none;
        }
        .quiz-modal .modal-header .close:hover {
          opacity: 1;
        }

        .quiz-modal .modal-body {
          padding: 30px;
          background-color: #f8f9fe;
        }

        /* Style untuk kontainer setiap pertanyaan */
        .quiz-question {
          margin-bottom: 25px;
          padding: 20px;
          background-color: #ffffff;
          border-radius: 10px;
          border: 1px solid #e9ecef;
        }

        /* Style untuk teks pertanyaan */
        .quiz-question .control-label {
          font-size: 17px;
          font-weight: 600;
          color: #32325d;
          margin-bottom: 15px;
          width: 100%;
          white-space: normal;
          line-height: 1.5;
        }

        /* Style untuk pilihan jawaban */
        .quiz-question .shiny-input-radiogroup .radio {
          display: block;
          padding: 12px 15px;
          border: 1px solid #e2e8f0;
          border-radius: 8px;
          margin-bottom: 10px;
          transition: all 0.2s ease-in-out;
          cursor: pointer;
        }

        .quiz-question .shiny-input-radiogroup .radio:hover {
          background-color: #f0f2ff;
          border-color: #667eea;
        }

        .quiz-question .shiny-input-radiogroup .radio label {
            font-weight: 500;
        }

        .quiz-question .shiny-input-radiogroup .radio input[type=radio] {
          margin-right: 12px;
          transform: scale(1.2);
        }

        /* Style untuk halaman hasil */
        .quiz-results-panel {
          text-align: center;
        }

        .quiz-results-panel h2 {
          font-weight: 700;
          color: #32325d;
        }

        .quiz-results-panel h3 {
          font-weight: 600;
          color: #667eea;
          background-color: #f0f2ff;
          padding: 10px 20px;
          border-radius: 50px;
          display: inline-block;
          margin-top: 10px;
          margin-bottom: 25px;
        }

        .result-item {
          text-align: left;
          padding: 15px;
          border: 1px solid #e9ecef;
          border-radius: 8px;
          margin-bottom: 15px;
          background-color: #fff;
        }

        .result-item.correct {
          border-left: 5px solid #28a745; /* Hijau untuk jawaban benar */
        }

        .result-item.incorrect {
          border-left: 5px solid #dc3545; /* Merah untuk jawaban salah */
        }

        .result-item p {
            margin-bottom: 5px;
        }
      "
        )
      ),
      withMathJax(),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('activateTab', function(tabName) {
          // Cari tab dengan data-value atau href yang sesuai
          $('a[data-value=\"' + tabName + '\"]').click();
          // Atau alternatif:
          // $('a[href=\"#' + tabName + '\"]').click();
        });
      "))
    ),
    do.call(argonTabItems, c(
      list(
        argonTabItem(
          tabName = "beranda",
          h1("Dashboard Metode Statistika II", style = "text-align: center; color: #32325d;"),
          p(
            "Pusat pembelajaran interaktif untuk mata kuliah Metode Statistika II, dilengkapi dengan ringkasan materi dan kalkulator statistik.",
            style = "text-align: center; font-size: 16px; color: #6c757d;"
          ),
          # -- 4 KARTU FUNGSIONAL --
          fluidRow(
            class = "mb-4",
            # Kartu 1: Total Materi (Link ke Daftar Isi)
            column(
              width = 3,
              div(
                class = "card argon-card card-stats",
                style = "cursor: pointer; height: 100%;",
                # Menambahkan tinggi 100% untuk konsistensi
                onclick = "document.getElementById('daftar-isi-anchor').scrollIntoView({ behavior: 'smooth' });",
                div(
                  class = "card-body",
                  div(class = "icon icon-shape icon-shape-primary", icon("book-open")),
                  h3("14", class = "card-title"),
                  h5("Total Materi", class = "card-category"),
                  p(
                    "Klik untuk melompat ke daftar lengkap materi dari Pertemuan 1 hingga 14."
                  )
                )
              )
            ),
            # Kartu 2: Fitur Kalkulator (Memunculkan Modal)
            column(
              width = 3,
              div(
                class = "card argon-card card-stats",
                style = "cursor: pointer; height: 100%;",
                onclick = "Shiny.setInputValue('card_kalkulator_click', Math.random());",
                div(
                  class = "card-body",
                  div(class = "icon icon-shape icon-shape-success", icon("calculator")),
                  h3("14+", class = "card-title"),
                  h5("Fitur Kalkulator", class = "card-category"),
                  p(
                    "Klik untuk melihat daftar kalkulator interaktif yang tersedia di setiap pertemuan."
                  )
                )
              )
            ),
            # Kartu 3: Sumber Referensi (Membuka Link GDrive)
            column(
              width = 3,
              tags$a(
                href = "https://drive.google.com/file/d/1FBBjX9SYMyyXj-fIZALB-zUMwzkBmH3o/view?usp=sharing",
                target = "_blank",
                style = "text-decoration: none; color: inherit;",
                div(
                  class = "card argon-card card-stats",
                  style = "height: 100%;",
                  div(
                    class = "card-body",
                    div(class = "icon icon-shape icon-shape-warning", icon("graduation-cap")),
                    h3("2KS1", class = "card-title"),
                    h5("Sumber Referensi", class = "card-category"),
                    p("Klik untuk membuka E-Book yang disusun oleh kelas 2KS1 di tab baru.")
                  )
                )
              )
            ),
            # Kartu 4: Video Tutorial (Link ke Bagian Video)
            column(
              width = 3,
              div(
                class = "card argon-card card-stats",
                style = "cursor: pointer; height: 100%;",
                onclick = "document.getElementById('video-tutorial-anchor').scrollIntoView({ behavior: 'smooth' });",
                div(
                  class = "card-body",
                  div(class = "icon icon-shape icon-shape-info", # Menggunakan warna info untuk pembeda
                      icon("video")),
                  h3("Tutorial", class = "card-title"),
                  h5("Panduan Video", class = "card-category"),
                  p("Klik untuk melihat panduan video penggunaan dashboard ini.")
                )
              )
            )
          ),
          fluidRow(column(
            width = 12,
            argonCard(
              id = "daftar-isi-anchor",
              width = 12,
              title = "Daftar Isi Materi & Kalkulator",
              icon = icon("list-ol"),
              status = "primary",
              shadow = TRUE,
              # -- TATA LETAK MULTI-KOLOM --
              tags$div(style = "column-count: 3; column-gap: 20px;",
                       # Membuat 3 kolom
                       tags$ol(style = "list-style-type: none; padding-left: 0; margin: 0;",
                               lapply(1:14, function(i) {
                                 tags$li(
                                   style = "margin-bottom: 10px; break-inside: avoid-column;",
                                   # Mencegah item terpotong antar kolom
                                   actionLink(
                                     inputId = paste0("goto_pertemuan_", i),
                                     label = daftar_judul_pertemuan[i],
                                     style = "color: #5e72e4; text-decoration: none; font-weight: 500;",
                                     class = "nav-link-custom"
                                   )
                                 )
                               })))
            )
          ), 
            column(
              width = 12,
              argonCard(
                id = "video-tutorial-anchor",
                width = 12, title = "Video Tutorial Penggunaan", icon = icon("youtube"), status = "primary", shadow = TRUE,
                HTML('<div style="position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; max-width: 100%; height: auto;">
                                <iframe style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; border-radius: 5px;"
                                        src="https://www.youtube.com/embed/OxTBDvRq9PI?si=9en1IIrwS6RAY7l5" title="YouTube video player"
                                        title="YouTube video player" frameborder="0"
                                        allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                                        allowfullscreen>
                                </iframe>
                              </div>')
              )
            )
          )
        )
      ),
      # --- KONTEN SEMUA PERTEMUAN LAINNYA ---
      # (Di sini Anda akan menempatkan argonTabItem untuk setiap pertemuan,
      #  pastikan tabName-nya cocok, contoh: tabName = "pertemuan_1")
      lapply(1:14, function(i) {
        if (i == 1) {
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),

            # --- KARTU KONTEN UTAMA ---
            argonCard(
              width = 12,
              shadow = TRUE,
              border_level = 200,

              # --- BARIS 1: KLASIFIKASI STATISTIKA & POPULASI ---
              argonRow(
                # --- KOLOM 1: KLASIFIKASI STATISTIKA ---
                argonColumn(
                  width = 6,
                  h3("Klasifikasi Statistika", class = "text-primary"),
                  HTML("
            <p><strong>Statistika</strong> adalah ilmu tentang pengumpulan, pengolahan, penyajian, dan analisis data untuk pengambilan keputusan. Urutan prosesnya adalah: <strong>Data → Statistika → Informasi</strong>.</p>

            <h6>Berdasarkan Tujuan:</h6>
            <ul>
              <li><strong>Statistik Deskriptif:</strong> Menggambarkan data tanpa menarik kesimpulan.</li>
              <li><strong>Statistik Inferensia:</strong> Menarik kesimpulan tentang populasi dari sampel, umumnya bersifat induksi.</li>
            </ul>

            <h6>Berdasarkan Asumsi Distribusi:</h6>
            <ul>
              <li><strong>Statistik Parametrik:</strong> Inferensi untuk data interval/rasio dengan asumsi distribusi tertentu.</li>
              <li><strong>Statistik Nonparametrik:</strong> Inferensi untuk data nominal/ordinal tanpa terikat asumsi distribusi.</li>
            </ul>
          ")
                ),

                # --- KOLOM 2: KLASIFIKASI POPULASI & VARIABEL ---
                argonColumn(
                  width = 6,
                  h3("Klasifikasi Populasi & Variabel", class = "text-primary"),
                  HTML("
            <h6>Berdasarkan Sumber Data Populasi:</h6>
            <ul>
              <li><strong>Terbatas:</strong> Sumber data jelas dan dapat dihitung.</li>
              <li><strong>Tak Terbatas:</strong> Sumber data tidak memiliki batasan yang jelas.</li>
            </ul>

            <h6>Berdasarkan Sifat Populasi:</h6>
            <ul>
              <li><strong>Homogen:</strong> Unsur-unsurnya memiliki sifat yang sama.</li>
              <li><strong>Heterogen:</strong> Unsur-unsurnya memiliki sifat yang beragam.</li>
            </ul>

            <h6>Berdasarkan Jumlah Variabel:</h6>
            <ul>
              <li><strong>Analisis Univariat:</strong> Menganalisis satu variabel.</li>
              <li><strong>Analisis Multivariat:</strong> Menganalisis dua atau lebih variabel bersamaan.</li>
            </ul>
          ")
                )
              ), # Akhir dari argonRow pertama

              hr(), # Garis pemisah

              # --- BARIS 2: KONSEP DISTRIBUSI SAMPLING ---
              argonRow(
                h3("Konsep Distribusi Sampling", class = "text-primary text-center", style = "width: 100%; margin-bottom: 20px;"),

                # --- KOLOM 1: RATA-RATA & BEDA RATA-RATA ---
                argonColumn(
                  width = 6,
                  HTML("
            <h5>1. Distribusi Sampling Rata-Rata</h5>
            <p>Ini adalah distribusi peluang dari semua kemungkinan rata-rata sampel yang bisa diambil dari suatu populasi.</p>
            <ul>
              <li><strong>Teorema Limit Pusat:</strong> Untuk sampel besar, distribusi rata-rata sampel akan mendekati normal.</li>
              <li><strong>Pengambilan dengan/tanpa pengembalian</strong> akan mempengaruhi perhitungan simpangan bakunya.</li>
              <li>Untuk sampel dari populasi normal, digunakan <strong>distribusi-t</strong>.</li>
            </ul>

            <h5>2. Distribusi Sampling Beda Rata-Rata</h5>
            <p>Beda antara rata-rata dua sampel independen juga akan menyebar mendekati normal.</p>
          ")
                ),

                # --- KOLOM 2: PROPORSI & BEDA PROPORSI ---
                argonColumn(
                  width = 6,
                  HTML("
            <h5>3. Distribusi Sampling Proporsi</h5>
            <p>Proporsi sampel akan mendekati distribusi normal jika ukuran sampel cukup besar (syarat: np ≥ 5 dan n(1-p) ≥ 5).</p>

            <h5>4. Distribusi Sampling Beda Proporsi</h5>
            <p>Selisih antara dua proporsi sampel independen juga akan berdistribusi mendekati normal.</p>
          ")
                )
              ) # Akhir dari argonRow kedua
            ), # Akhir dari argonCard
            
            # --- TOMBOL KUIS BARU ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton(
                  "start_quiz_p1_new",
                  "Kerjakan Kuis Interaktif",
                  icon = icon("rocket"),
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        } 
        else if (i == 2) {
          # --- KONTEN BARU UNTUK PERTEMUAN 2 ---
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: Estimasi Parameter",
              icon = icon("bullseye"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <h4 style="margin-top: 0;">Estimasi Titik</h4>
        <p>Estimasi titik adalah sebuah nilai tunggal yang digunakan untuk menduga nilai parameter populasi yang tidak diketahui.</p>
        <ul>
            <li><b>Estimator rata-rata populasi (\\(\\mu\\)):</b> Rata-rata sampel (\\(\\bar{x}\\)).</li>
            <li><b>Estimator proporsi populasi (p):</b> Proporsi sampel (\\(\\hat{p} = x/n\\)).</li>
        </ul>

        <hr>

        <h4>Estimasi Interval Satu Populasi</h4>
        <p>Estimasi interval memberikan rentang nilai di mana parameter populasi diperkirakan berada, dengan tingkat kepercayaan tertentu.</p>

        <h5>1. Interval Kepercayaan untuk Rata-rata (\\(\\mu\\))</h5>
        <ul>
            <li>
                <strong>\\(\\sigma\\) diketahui:</strong>
                $$ \\bar{x} \\pm Z_{\\alpha/2} \\frac{\\sigma}{\\sqrt{n}} $$
            </li>
            <li>
                <strong>\\(\\sigma\\) tidak diketahui:</strong>
                $$ \\bar{x} \\pm t_{(\\alpha/2; n-1)} \\frac{s}{\\sqrt{n}} $$
            </li>
        </ul>

        <h5>2. Interval Kepercayaan untuk Proporsi (p)</h5>
        <p>Digunakan untuk sampel besar, dengan pendekatan distribusi normal.</p>
        $$ \\hat{p} \\pm Z_{\\alpha/2} \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}} $$

        <h5>3. Interval Kepercayaan untuk Varians (\\(\\sigma^2\\))</h5>
        <p>Menggunakan pendekatan distribusi Chi-Square.</p>
        $$ \\frac{(n-1)S^2}{\\chi^2_{(\\alpha/2; n-1)}} < \\sigma^2 < \\frac{(n-1)S^2}{\\chi^2_{(1-\\alpha/2; n-1)}} $$

        <hr>

        <h4>Estimasi Interval Dua Populasi</h4>

        <h5>1. Perbedaan 2 Rata-rata (\\(\\mu_1 - \\mu_2\\))</h5>
        <ul>
          <li><strong>Sampel Independen:</strong>
            <ul>
                <li>Jika \\(\\sigma_1\\) & \\(\\sigma_2\\) diketahui:
                    $$ (\\bar{x}_1 - \\bar{x}_2) \\pm Z_{\\alpha/2} \\sqrt{\\frac{\\sigma_1^2}{n_1} + \\frac{\\sigma_2^2}{n_2}} $$
                </li>
                <li>Jika \\(\\sigma_1\\) & \\(\\sigma_2\\) tidak diketahui (n < 30, asumsi varians sama):
                    $$ (\\bar{x}_1 - \\bar{x}_2) \\pm t_{\\alpha/2} \\cdot s_p \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}} $$
                </li>
            </ul>
          </li>
          <li><strong>Sampel Berpasangan:</strong>
              $$ \\bar{d} \\pm t_{\\alpha/2} \\frac{s_d}{\\sqrt{n}} $$
          </li>
        </ul>

        <h5>2. Perbedaan 2 Proporsi (\\(p_1 - p_2\\))</h5>
        $$ (\\hat{p}_1 - \\hat{p}_2) \\pm Z_{\\alpha/2} \\sqrt{\\frac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\frac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} $$

        <h5>3. Rasio 2 Varians (\\(\\sigma_1^2 / \\sigma_2^2\\))</h5>
        <p>Menggunakan distribusi F.</p>
        $$ \\frac{s_1^2}{s_2^2} \\frac{1}{f_{\\alpha/2}(v_1, v_2)} < \\frac{\\sigma_1^2}{\\sigma_2^2} < \\frac{s_1^2}{s_2^2} f_{\\alpha/2}(v_2, v_1) $$
      ')
            ),
            br(),
            
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 2 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton(
                  "start_quiz_p2_new",
                  "Kerjakan Kuis Interaktif",
                  icon = icon("rocket"),
                  class = "btn-primary btn-lg"
                )
              )
            ), 

            # Menggunakan Tabset untuk memisahkan kalkulator
            argonTabSet(
              id = "tabset_pertemuan2",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              iconList = list(icon("chart-line"), icon("chart-bar")),

              # --- SUB-TAB UNTUK SATU POPULASI (UI DIPERBARUI) ---
              argonTab(
                tabName = "Satu Populasi",
                active = TRUE,
                fluidRow(
                  box(
                    title = "Input Data", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("input_method1", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File" = "file")),
                    conditionalPanel(
                      condition = "input.input_method1 == 'manual'",
                      textAreaInput("manual_data1", "Masukkan Data (pisahkan dengan koma):", placeholder = "Contoh: 12, 15, 18, 20, 22, 25, 28")
                    ),
                    conditionalPanel(
                      condition = "input.input_method1 == 'file'",
                      fileInput("file1", "Upload File CSV/Excel:", accept = c(".csv", ".xlsx", ".xls")),
                      # --- CONTOH FORMAT EXCEL DITAMBAHKAN KEMBALI ---
                      argonAlert(
                        icon = icon("info-circle"),
                        status = "info",
                        "Pastikan file Excel Anda memiliki satu kolom berisi data numerik."
                      ),
                      tags$li("Contoh format (Header di baris 1):",
                              HTML('
                          $$
                          \\begin{array}{|c|c|}
                          \\hline
                          \\textbf{No} & \\textbf{IP} \\\\
                          \\hline
                          1 & 3.28 \\\\
                          2 & 2.55 \\\\
                          ... & ... \\\\
                          \\hline
                          \\end{array}
                          $$
                        ')
                      ),
                      uiOutput("sheet_selector1"),
                      uiOutput("column_selector1")
                    ),
                    selectInput("param_type1", "Parameter yang Dianalisis:",
                                choices = list("Rata-rata" = "mean", "Varian" = "variance", "Proporsi" = "proportion")
                    ),
                    conditionalPanel(
                      condition = "input.param_type1 == 'proportion'",
                      numericInput("success_value1", "Nilai yang dianggap 'sukses':", value = 1)
                    ),
                    numericInput("confidence_level1", "Tingkat Kepercayaan (%):", value = 95, min = 80, max = 99),
                    actionButton("analyze1", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"))
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("report_output_ui1")
                  )
                ),
                fluidRow(
                  box(
                    title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(DT::dataTableOutput("data_table1"))
                  ),
                  box(
                    title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot1"))
                  )
                )
              ),
              
              # --- SUB-TAB UNTUK DUA POPULASI (UI DIPERBARUI) ---
              argonTab(
                tabName = "Dua Populasi",
                fluidRow(
                  box(
                    title = "Input Data Populasi", status = "primary", solidHeader = TRUE, width = 12,
                    radioButtons("input_method2", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File" = "file")),
                    conditionalPanel(
                      condition = "input.input_method2 == 'manual'",
                      fluidRow(
                        column(6, textAreaInput("manual_data2a", "Data Populasi 1 (pisahkan dengan koma):", placeholder = "Contoh: 12, 15, 18")),
                        column(6, textAreaInput("manual_data2b", "Data Populasi 2 (pisahkan dengan koma):", placeholder = "Contoh: 18, 21, 24"))
                      )
                    ),
                    conditionalPanel(
                      condition = "input.input_method2 == 'file'",
                      fileInput("file2_combined", "Upload File (Excel):", accept = c(".xlsx", ".xls")),
                      # --- CONTOH FORMAT EXCEL DITAMBAHKAN KEMBALI ---
                      argonAlert(
                        icon = icon("info-circle"),
                        status = "info",
                        "Pastikan file Excel Anda memiliki DUA kolom numerik untuk dua populasi."
                      ),
                      tags$li("Contoh format (Header di baris 1):",
                              HTML('
                          $$
                          \\begin{array}{|c|c|}
                          \\hline
                          \\textbf{Populasi_1} & \\textbf{Populasi_2} \\\\
                          \\hline
                          85 & 90 \\\\
                          88 & 92 \\\\
                          ... & ... \\\\
                          \\hline
                          \\end{array}
                          $$
                        ')
                      ),
                      uiOutput("sheet_selector2_combined"),
                      fluidRow(
                        column(6, uiOutput("column_selector2a_combined")),
                        column(6, uiOutput("column_selector2b_combined"))
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("param_type2", "Parameter:", choices = list("Beda Rata-rata" = "mean", "Rasio Varian" = "variance", "Beda Proporsi" = "proportion")),
                    conditionalPanel(
                      condition = "input.param_type2 == 'mean'",
                      selectInput("sample_relation2", "Hubungan Sampel:", choices = list("Independen" = "independent", "Berpasangan" = "paired"))
                    ),
                    conditionalPanel(
                      condition = "input.param_type2 == 'proportion'",
                      numericInput("success_value2a", "Nilai 'sukses' Populasi 1:", value = 1),
                      numericInput("success_value2b", "Nilai 'sukses' Populasi 2:", value = 1)
                    ),
                    numericInput("confidence_level2", "Tingkat Kepercayaan (%):", value = 95, min = 80, max = 99),
                    actionButton("analyze2", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"))
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("report_output_ui2")
                  )
                ),
                # --- ROW UNTUK TABEL DATA DAN PLOT DITAMBAHKAN KEMBALI ---
                fluidRow(
                  box(
                    title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(DT::dataTableOutput("data_table2"))
                  ),
                  box(
                    title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot2"))
                  )
                )
              )
            )
          )
        } 
        else if (i == 3) {
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: Pengujian Hipotesis Satu Populasi",
              icon = icon("check-double"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <h4 style="margin-top: 0;">Konsep Dasar Hipotesis</h4>
        <p><b>Hipotesis statistika</b> adalah suatu pernyataan atau dugaan mengenai parameter dari satu atau lebih populasi yang perlu diuji kebenarannya berdasarkan data sampel.</p>
        <ul>
            <li><b>Hipotesis Nol (\\(H_0\\)):</b> Pernyataan yang sementara dianggap benar dan ingin diuji atau ditolak.</li>
            <li><b>Hipotesis Alternatif (\\(H_1\\)):</b> Pernyataan lain yang akan diterima jika \\(H_0\\) berhasil ditolak.</li>
        </ul>
        <p>Berdasarkan nilainya, hipotesis bisa bersifat <b>tunggal</b> (menyatakan parameter dalam satu nilai) atau <b>majemuk</b> (menyatakan parameter dalam rentang nilai).</p>

        <hr>

        <h4>Tipe Kesalahan dan Kuasa Uji</h4>
        <p>Dalam pengujian hipotesis, terdapat dua kemungkinan kesalahan:</p>
        <ul>
            <li><b>Kesalahan Tipe I (\\(\\alpha\\)):</b> Menolak \\(H_0\\) padahal \\(H_0\\) benar. Peluangnya disebut <b>taraf signifikansi</b>.</li>
            <li><b>Kesalahan Tipe II (\\(\\beta\\)):</b> Gagal menolak \\(H_0\\) padahal \\(H_0\\) salah. Peluangnya (\\(1-\\beta\\)) disebut <b>kuasa uji</b>.</li>
        </ul>

        <hr>

        <h4>Prosedur Umum Pengujian Hipotesis</h4>
        <ol>
            <li>Tentukan hipotesis nol (\\(H_0\\)) dan hipotesis alternatif (\\(H_1\\)).</li>
            <li>Tentukan tingkat signifikansi (\\(\\alpha\\)).</li>
            <li>Hitung nilai statistik uji dari data sampel.</li>
            <li>Tentukan daerah kritis (daerah tolak \\(H_0\\)).</li>
            <li>Ambil keputusan: Tolak \\(H_0\\) jika statistik uji jatuh di daerah kritis, atau jika p-value < \\(\\alpha\\).</li>
        </ol>

        <hr>

        <h4>Statistik Uji untuk Hipotesis Satu Populasi</h4>

        <h5>1. Uji Rata-rata (\\(\\mu\\))</h5>
        <ul>
            <li><b>\\(\\sigma\\) Diketahui:</b> Menggunakan statistik uji Z.
                $$ Z = \\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}} $$
            </li>
            <li><b>\\(\\sigma\\) Tidak Diketahui:</b> Menggunakan statistik uji t dengan derajat bebas \\(v = n-1\\).
                $$ t = \\frac{\\bar{X}-\\mu_0}{s/\\sqrt{n}} $$
            </li>
        </ul>

        <h5>2. Uji Proporsi (p)</h5>
        <p>Menggunakan pendekatan normal jika syarat \\(np \\ge 5\\) dan \\(n(1-p) \\ge 5\\) terpenuhi.</p>
        $$ Z = \\frac{\\hat{p}-p_0}{\\sqrt{\\frac{p_0(1-p_0)}{n}}} $$

        <h5>3. Uji Varians (\\(\\sigma^2\\))</h5>
        <p>Menggunakan statistik uji Chi-Square dengan derajat bebas \\(v = n-1\\).</p>
        $$ \\chi^2 = \\frac{(n-1)s^2}{\\sigma_0^2} $$
        <p><b>Daerah kritis untuk uji varians:</b></p>
        $$
        \\begin{array}{|c|c|}
        \\hline
        \\mathbf{H_1} & \\textbf{Daerah Tolak } \\mathbf{H_0} \\\\
        \\hline
        \\sigma^2 < \\sigma_0^2 & \\chi^2 < \\chi^2_{1-\\alpha} \\\\
        \\hline
        \\sigma^2 > \\sigma_0^2 & \\chi^2 > \\chi^2_{\\alpha} \\\\
        \\hline
        \\sigma^2 \\neq \\sigma_0^2 & \\chi^2 < \\chi^2_{1-\\alpha/2} \\text{ atau } \\chi^2 > \\chi^2_{\\alpha/2} \\\\
        \\hline
        \\end{array}
        $$
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 3 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p3_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI HIPOTESIS 1 POPULASI (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p3_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File" = "file")),
                conditionalPanel(
                  condition = "input.p3_input_method == 'manual'",
                  textAreaInput("p3_manual_data", "Masukkan Data (pisahkan dengan koma):", placeholder = "Contoh: 505, 499, 501, 503, 498")
                ),
                conditionalPanel(
                  condition = "input.p3_input_method == 'file'",
                  fileInput("p3_file", "Upload File CSV/Excel:", accept = c(".csv", ".xlsx", ".xls")),
                  
                  argonAlert(
                    icon = icon("info-circle"),
                    status = "info",
                    "Pastikan file Excel Anda memiliki satu kolom berisi data numerik."
                  ),
                  tags$li("Contoh format (Header di baris 1):",
                          HTML('
                      $$
                      \\begin{array}{|c|}
                      \\hline
                      \\textbf{Nilai_Ujian} \\\\
                      \\hline
                      505 \\\\
                      499 \\\\
                      ... \\\\
                      \\hline
                      \\end{array}
                      $$
                    ')
                  ),
                  
                  uiOutput("p3_sheet_selector"),
                  uiOutput("p3_column_selector")
                ),
                hr(),
                
                # Pengaturan Uji
                selectInput("p3_param_type", "Parameter yang Diuji:",
                            choices = list("Rata-rata (μ)" = "mean", "Proporsi (p)" = "proportion", "Varian (σ²)" = "variance")
                ),
                
                # --- PERUBAHAN DI SINI ---
                # Input H0 terpisah untuk mean/varian dan proporsi
                conditionalPanel(
                  condition = "input.p3_param_type != 'proportion'",
                  numericInput("p3_h0_value_mean_var", "Nilai Hipotesis Nol (H₀):", value = 500)
                ),
                
                conditionalPanel(
                  condition = "input.p3_param_type == 'proportion'",
                  numericInput("p3_h0_value_prop", "Nilai Hipotesis Nol (p₀):", value = 0.5, min = 0.001, max = 0.999, step=0.01),
                  p("Proporsi sampel (p̂) dihitung dari data Anda menggunakan rumus berikut:"),
                  uiOutput("p3_prop_formula"),
                  uiOutput("p3_success_selector_ui")
                ),
                # --- AKHIR PERUBAHAN ---
                
                selectInput("p3_alternative", "Hipotesis Alternatif (H₁):",
                            choices = list("Tidak Sama Dengan (≠)" = "two.sided", 
                                           "Kurang Dari (<)" = "less", 
                                           "Lebih Dari (>)" = "greater")),
                
                sliderInput("p3_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                
                actionButton("p3_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p3_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p3_data_table"))
              ),
              box(
                title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p3_plot"))
              )
            )
          )
        } 
        else if (i == 4) {
          # --- Pertemuan 4: Kalkulator Uji Hipotesis 2 Populasi (DENGAN PERBAIKAN FINAL) ---
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),

            # --- KARTU KONTEN UTAMA ---
            argonCard(
              width = 12,
              title = "Materi Lengkap: Uji Hipotesis Dua Populasi",
              icon = icon("users-cog"),
              status = "primary",
              shadow = TRUE,
              HTML('
              <h4 style="margin-top: 0;">Beda Rata-Rata</h4>
              <p>Prosedur untuk menguji hipotesis selisih rata-rata antara dua populasi independen.</p>

              <h5>1. Varians Diketahui</h5>
              <p>Pengujian menggunakan statistik uji Z dengan rumus:</p>
              $$z = \\frac{(\\bar{x}_1 - \\bar{x}_2) - d_0}{\\sqrt{\\frac{\\sigma_1^2}{n_1} + \\frac{\\sigma_2^2}{n_2}}}$$

              <p><b>Tabel Hipotesis dan Daerah Tolak:</b></p>
              $$\\begin{array}{|c|c|c|}
              \\hline
              \\mathbf{H_0} & \\mathbf{H_1} & \\textbf{Daerah Tolak } \\mathbf{H_0} \\\\
              \\hline
              \\mu_1 - \\mu_2 = d_0 & \\mu_1 - \\mu_2 < d_0 & z < -z_{\\alpha} \\\\
              \\hline
              \\mu_1 - \\mu_2 = d_0 & \\mu_1 - \\mu_2 > d_0 & z > z_{\\alpha} \\\\
              \\hline
              \\mu_1 - \\mu_2 = d_0 & \\mu_1 - \\mu_2 \\neq d_0 & z < -z_{\\alpha/2} \\text{ atau } z > z_{\\alpha/2} \\\\
              \\hline
              \\end{array}$$

              <h5>2. Varians Tidak Diketahui (Asumsi Sama)</h5>
              <p>Menggunakan statistik uji-t dengan <em>pooled variance</em> \\(s_p\\) dan derajat bebas \\(v = n_1 + n_2 - 2\\).</p>
              $$t = \\frac{(\\bar{x}_1 - \\bar{x}_2) - d_0}{s_p \\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}}}$$

              <p>dimana pooled variance:</p>
              $$s_p = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2-1)s_2^2}{n_1+n_2-2}}$$

              <h5>3. Varians Tidak Diketahui (Asumsi Berbeda)</h5>
              <p>Menggunakan statistik uji-t dengan derajat bebas \\(v\\) dari pendekatan Welch-Satterthwaite.</p>
              $$t = \\frac{(\\bar{x}_1 - \\bar{x}_2) - d_0}{\\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}}}$$

              <p>dengan derajat bebas:</p>
              $$v = \\frac{\\left(\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}\\right)^2}{\\frac{\\left(\\frac{s_1^2}{n_1}\\right)^2}{n_1-1} + \\frac{\\left(\\frac{s_2^2}{n_2}\\right)^2}{n_2-1}}$$

              <hr>

              <h4>Beda Proporsi</h4>
              <p>Pengujian untuk beda proporsi antara dua populasi. Jika \\(p_1=p_2\\), digunakan proporsi gabungan \\(\\hat{p}\\).</p>
              $$Z = \\frac{\\hat{p}_1 - \\hat{p}_2}{\\sqrt{\\hat{p}(1-\\hat{p})\\left(\\frac{1}{n_1} + \\frac{1}{n_2}\\right)}}$$

              <p>dimana proporsi gabungan:</p>
              $$\\hat{p} = \\frac{x_1 + x_2}{n_1 + n_2}$$

              <hr>
              <h4>Uji Varian Dua Sampel</h4>
              <p>Menggunakan distribusi F untuk menguji kesamaan dua varians populasi \\(H_0: \\sigma_1^2 = \\sigma_2^2\\).</p>
              $$f = \\frac{S_1^2}{S_2^2}$$

              <p>dengan derajat bebas pembilang \\(v_1 = n_1 - 1\\) dan penyebut \\(v_2 = n_2 - 1\\).</p>

              <hr>
              <h4>Uji Data Berpasangan</h4>
              <p>Diterapkan pada data dependen (misal: sebelum-sesudah) dengan melakukan uji-t pada data selisih \\(d_i = x_{1i} - x_{2i}\\).</p>
              $$t = \\frac{\\bar{d} - d_0}{s_d / \\sqrt{n}}$$

              <p>dimana \\(\\bar{d}\\) adalah rata-rata selisih dan \\(s_d\\) adalah standar deviasi selisih dengan derajat bebas \\(v = n - 1\\).</p>

            ')
            ),
            br(),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 4 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p4_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI HIPOTESIS 2 POPULASI (DIPERBARUI) ---
            argonTabSet(
              id = "tabset_pertemuan4", card_wrapper = TRUE, horizontal = TRUE, circle = FALSE, size = "sm", width = 12,
              
              # --- TAB UNTUK BEDA RATA-RATA ---
              argonTab(
                tabName = "Beda Dua Rata-rata",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p4_mean_input_method", "Metode Input:", choices = list("Manual" = "manual", "Unggah File Excel" = "file")),
                    conditionalPanel(
                      condition = "input.p4_mean_input_method == 'manual'",
                      textAreaInput("p4_manual_data_a", "Data Populasi 1 (pisahkan koma):", placeholder = "Contoh: 23, 25, 21"),
                      textAreaInput("p4_manual_data_b", "Data Populasi 2 (pisahkan koma):", placeholder = "Contoh: 31, 29, 33")
                    ),
                    conditionalPanel(
                      condition = "input.p4_mean_input_method == 'file'",
                      fileInput("p4_file_mean", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(
                        icon = icon("info-circle"), status = "info",
                        "Pastikan file Excel Anda memiliki dua kolom numerik untuk dua populasi."
                      ),
                      tags$li("Contoh format (Header di baris 1):",
                              HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Metode_A} & \\textbf{Metode_B} \\\\ \\hline 85 & 90 \\\\ 88 & 92 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                      ),
                      uiOutput("p4_sheet_selector_mean"),
                      fluidRow(
                        column(6, uiOutput("p4_col_selector_a")),
                        column(6, uiOutput("p4_col_selector_b"))
                      )
                    ),
                    hr(),
                    selectInput("p4_sample_type", "Jenis Sampel:", choices = list("Independen" = "independent", "Berpasangan" = "paired")),
                    selectInput("p4_alternative_mean", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Kurang Dari (<)" = "less", "Lebih Dari (>)" = "greater")),
                    sliderInput("p4_alpha_mean", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p4_analyze_mean", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p4_report_output_ui_mean")
                  )
                ),
                fluidRow(
                  box(title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6, withSpinner(DT::dataTableOutput("p4_data_table_mean"))),
                  box(title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6, withSpinner(plotlyOutput("p4_plot_mean")))
                )
              ),
              
              # --- TAB BARU UNTUK RASIO DUA VARIANS ---
              argonTab(
                tabName = "Rasio Dua Varians",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    # Inputnya identik dengan beda rata-rata independen
                    radioButtons("p4_var_input_method", "Metode Input:", choices = list("Manual" = "manual", "Unggah File Excel" = "file")),
                    conditionalPanel(
                      condition = "input.p4_var_input_method == 'manual'",
                      textAreaInput("p4_manual_data_a_var", "Data Populasi 1 (pisahkan koma):", placeholder = "Contoh: 15, 18, 19"),
                      textAreaInput("p4_manual_data_b_var", "Data Populasi 2 (pisahkan koma):", placeholder = "Contoh: 25, 28, 31")
                    ),
                    conditionalPanel(
                      condition = "input.p4_var_input_method == 'file'",
                      fileInput("p4_file_var", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(
                        icon = icon("info-circle"), status = "info",
                        "Pastikan file Excel Anda memiliki dua kolom numerik untuk dua populasi."
                      ),
                      tags$li("Contoh format (Header di baris 1):",
                              HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Metode_A} & \\textbf{Metode_B} \\\\ \\hline 85 & 90 \\\\ 88 & 92 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                      ),
                      uiOutput("p4_sheet_selector_var"),
                      fluidRow(
                        column(6, uiOutput("p4_col_selector_a_var")),
                        column(6, uiOutput("p4_col_selector_b_var"))
                      )
                    ),
                    hr(),
                    selectInput("p4_alternative_var", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Kurang Dari (<)" = "less", "Lebih Dari (>)" = "greater")),
                    sliderInput("p4_alpha_var", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p4_analyze_var", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p4_report_output_ui_var")
                  )
                ),
                fluidRow(
                  box(title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6, withSpinner(DT::dataTableOutput("p4_data_table_var"))),
                  box(title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6, withSpinner(plotlyOutput("p4_plot_var")))
                )
              ),
              
              # --- TAB BARU UNTUK BEDA DUA PROPORSI ---
              argonTab(
                tabName = "Beda Dua Proporsi",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p4_prop_input_method", "Metode Input:", choices = list("Manual (Ringkasan)" = "manual", "Unggah File Excel (Data Mentah)" = "file")),
                    
                    conditionalPanel(
                      condition = "input.p4_prop_input_method == 'manual'",
                      h4("Data Populasi 1"),
                      numericInput("p4_success_a", "Jumlah Sukses (x₁):", 50, min = 0),
                      numericInput("p4_n_a", "Jumlah Total (n₁):", 100, min = 1),
                      hr(),
                      h4("Data Populasi 2"),
                      numericInput("p4_success_b", "Jumlah Sukses (x₂):", 60, min = 0),
                      numericInput("p4_n_b", "Jumlah Total (n₂):", 120, min = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.p4_prop_input_method == 'file'",
                      fileInput("p4_file_prop", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(
                        icon = icon("info-circle"), status = "info",
                        "Gunakan format panjang: satu kolom untuk grup, satu kolom untuk hasil (kategorik)."
                      ),
                      tags$li("Contoh format:",
                              HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Kelompok} & \\textbf{Hasil} \\\\ \\hline \\text{Obat A} & \\text{Sembuh} \\\\ \\text{Obat A} & \\text{Tidak Sembuh} \\\\ \\text{Plasebo} & \\text{Sembuh} \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                      ),
                      uiOutput("p4_sheet_selector_prop"),
                      uiOutput("p4_col_selector_group_prop"),
                      uiOutput("p4_col_selector_value_prop"),
                      textInput("p4_success_value_prop", "Tuliskan nilai yang dianggap 'Sukses':", placeholder = "Contoh: Sembuh, Lulus, Ya")
                    ),
                    
                    hr(),
                    selectInput("p4_alternative_prop", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Kurang Dari (<)" = "less", "Lebih Dari (>)" = "greater")),
                    sliderInput("p4_alpha_prop", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p4_analyze_prop", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p4_report_output_ui_prop")
                  )
                ),
                fluidRow(
                  box(title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6, withSpinner(DT::dataTableOutput("p4_data_table_prop"))),
                  box(title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6, withSpinner(plotlyOutput("p4_plot_prop")))
                )
              )
            )
          )
        } 
        else if (i == 5) {
          # === PERTEMUAN 5: UJI NORMALITAS ===
          argonTabItem(
            tabName = "pertemuan_5",
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: Uji Normalitas",
              icon = icon("chart-area"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <p>Uji kesesuaian sebaran normal digunakan untuk menentukan apakah suatu sampel data berasal dari populasi yang berdistribusi normal. Metode yang digunakan dapat dikelompokkan berdasarkan ukuran sampel.</p>

        <hr>

        <h4 style="margin-top: 0;">Metode untuk Ukuran Sampel Kecil</h4>

        <h5>1. Uji Lilliefors</h5>
        <p>Uji ini merupakan modifikasi dari uji Kolmogorov-Smirnov, khusus digunakan ketika parameter mean (\\(\\mu\\)) dan varians (\\(\\sigma^2\\)) populasi tidak diketahui. Hipotesis nol (\\(H_0\\)) menyatakan data berdistribusi normal.</p>
        <ul>
          <li><b>Statistik Uji:</b> Selisih absolut maksimum antara fungsi distribusi kumulatif empiris (\\(S(x)\\)) dan teoretis (\\(F(x)\\)).
            $$ L = \\max|F(x) - S(x)| $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika nilai statistik uji \\(L\\) lebih besar dari nilai kritis pada tabel Lilliefors (\\(L > L_{\\alpha}(n)\\)).</li>
        </ul>

        <h5>2. Uji Shapiro-Wilk</h5>
        <p>Uji ini sangat kuat untuk mendeteksi penyimpangan dari normalitas pada sampel kecil. Syaratnya, data harus berskala interval atau rasio dan diambil secara acak.</p>
        <ul>
          <li><b>Statistik Uji:</b>
            $$ T_3 = \\frac{1}{D} \\left( \\sum_{i=1}^{k} a_i (x_{n-i+1} - x_i) \\right)^2 $$
            dimana \\(D = \\sum_{i=1}^{n} (x_i - \\bar{x})^2\\) dan \\(a_i\\) adalah koefisien dari tabel Shapiro-Wilk.
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) (data tidak normal) jika nilai \\(T_3\\) lebih kecil dari nilai kritis pada tabel Shapiro-Wilk (\\(T_3 < W_{\\alpha;n}\\)).</li>
        </ul>

        <hr>

        <h4 style="margin-top: 0;">Metode untuk Ukuran Sampel Besar</h4>

        <h5>1. Uji Goodness of Fit (Chi-Square)</h5>
        <p>Uji ini membandingkan frekuensi data yang diamati (\\(o_i\\)) dengan frekuensi yang diharapkan (\\(e_i\\)) berdasarkan distribusi normal.</p>
        <ul>
          <li><b>Statistik Uji:</b>
            $$ \\chi^2_{hitung} = \\sum_{i=1}^{k} \\frac{(o_i - e_i)^2}{e_i} $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha; k-1-p}\\), dimana \\(k\\) adalah jumlah kelas dan \\(p\\) adalah banyaknya parameter yang diestimasi.</li>
        </ul>

        <h5>2. Uji Kolmogorov-Smirnov (KS)</h5>
        <p>Mirip dengan Lilliefors, namun digunakan ketika parameter populasi (mean dan varians) diketahui. Membandingkan fungsi distribusi kumulatif empiris dan teoretis.</p>
        <ul>
          <li><b>Statistik Uji:</b>
            $$ D = \\max|F(x) - S(x)| $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika nilai \\(D\\) lebih besar dari nilai kritis pada tabel Kolmogorov-Smirnov.</li>
        </ul>

        <h5>3. Uji Jarque-Bera</h5>
        <p>Uji ini memeriksa normalitas berdasarkan nilai kemiringan (<em>skewness</em>, S) dan keruncingan (<em>kurtosis</em>, K) dari distribusi data.</p>
        <ul>
          <li><b>Statistik Uji:</b>
            $$ JB = n \\left( \\frac{S^2}{6} + \\frac{(K-3)^2}{24} \\right) $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) (data tidak normal) jika nilai JB lebih besar dari nilai kritis Chi-Square dengan 2 derajat bebas (\\(JB > \\chi^2_{\\alpha,2}\\)).</li>
        </ul>
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 5 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p5_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI NORMALITAS (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p5_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File" = "file")),
                conditionalPanel(
                  condition = "input.p5_input_method == 'manual'",
                  textAreaInput("p5_manual_data", "Masukkan Data (pisahkan dengan koma):", placeholder = "Contoh: 12.5, 15.3, 18.7, 20.1")
                ),
                conditionalPanel(
                  condition = "input.p5_input_method == 'file'",
                  fileInput("p5_file", "Upload File (CSV, Excel, SPSS):", accept = c(".csv", ".xlsx", ".xls", ".sav")),
                  argonAlert(
                    icon = icon("info-circle"), status = "info",
                    "Pastikan file Anda memiliki setidaknya satu kolom numerik.",
                    tags$li("Contoh format (Header di baris 1):",
                            HTML('
                        $$
                        \\begin{array}{|c|}
                        \\hline
                        \\textbf{Nilai_Data} \\\\
                        \\hline
                        12.5 \\\\
                        15.3 \\\\
                        ... \\\\
                        \\hline
                        \\end{array}
                        $$
                      ')
                    )
                  ),
                  uiOutput("p5_sheet_selector"),
                  uiOutput("p5_column_selector")
                ),
                hr(),
                
                # Pengaturan Uji
                checkboxGroupInput("p5_selected_tests", "Pilih Uji yang Dijalankan:",
                                   choices = list(
                                     "Shapiro-Wilk" = "shapiro",
                                     "Lilliefors (Kolmogorov-Smirnov)" = "lilliefors",
                                     "Jarque-Bera" = "jarque"
                                   ),
                                   selected = c("shapiro", "lilliefors", "jarque")),
                sliderInput("p5_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                actionButton("p5_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p5_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p5_data_table"))
              ),
              box(
                title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p5_plot"))
              )
            )
          )
        } 
        else if (i == 6) { # Pertemuan 6 khusus untuk uji kesamaan varians
          argonTabItem(
            tabName = "pertemuan_6",
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            
            argonCard(
              width = 12,
              title = "Catatan Materi: Uji Kesamaan Beberapa Varians",
              icon = icon("balance-scale"),
              status = "primary",
              shadow = TRUE,
              
              HTML('
        <p>Uji kesamaan beberapa varians digunakan untuk menguji hipotesis apakah varians dari lebih dari dua populasi adalah sama.</p>
        <ul>
          <li><b>Hipotesis Nol (\\(H_0\\)):</b> \\(\\sigma_1^2 = \\sigma_2^2 = \\dots = \\sigma_k^2\\)</li>
          <li><b>Hipotesis Alternatif (\\(H_1\\)):</b> Tidak semua varians dari k populasi sama.</li>
        </ul>
        <p>Metode yang umum digunakan adalah Uji Bartlett, Uji Pendekatan Chi-Square, dan Uji Levene.</p>
        
        <hr>
        
        <h4 style="margin-top: 0;">1. Uji Bartlett (Pendekatan Chi-Square)</h4>
        <p>Uji Bartlett digunakan untuk menguji kesamaan varians dari beberapa populasi yang diasumsikan berdistribusi normal.</p>
        
        <h5>Statistik Uji</h5>
        <p>Statistik uji Bartlett dengan pendekatan Chi-Square dihitung menggunakan formula:</p>
        $$ \\chi^2_{hitung} = \\frac{M}{C} \\times 2.3026 $$
        <p>Dengan komponen perhitungan sebagai berikut:</p>
        <ul>
          <li><b>Varians Gabungan (\\(S_p^2\\)):</b>
            $$ S_p^2 = \\frac{\\sum_{i=1}^{k} (n_i - 1)S_i^2}{\\sum_{i=1}^{k} (n_i - 1)} $$
          </li>
          <li><b>Nilai M:</b>
            $$ M = \\left( \\sum_{i=1}^{k} (n_i - 1) \\right) \\log_{10}(S_p^2) - \\sum_{i=1}^{k} (n_i - 1) \\log_{10}(S_i^2) $$
          </li>
          <li><b>Faktor Koreksi (C):</b>
            $$ C = 1 + \\frac{1}{3(k-1)} \\left( \\sum_{i=1}^{k} \\frac{1}{n_i - 1} - \\frac{1}{\\sum_{i=1}^{k} (n_i - 1)} \\right) $$
          </li>
        </ul>
        
        <h5>Keputusan</h5>
        <p>Hipotesis nol (\\(H_0\\)) ditolak jika nilai statistik uji lebih besar dari nilai kritis Chi-Square:</p>
        $$ \\chi^2_{hitung} > \\chi^2_{(\\alpha, k-1)} $$

        <hr>
        
        <h4>2. Uji Levene</h4>
        <p>Uji Levene merupakan alternatif yang tidak mensyaratkan data berdistribusi normal. Uji ini pada dasarnya adalah melakukan ANOVA pada deviasi absolut data dari pusatnya (rata-rata atau median).</p>
        
        <h5>Statistik Uji</h5>
        <p>Statistik uji Levene (L) mengikuti distribusi F:</p>
        $$ L = \\frac{N-k}{k-1} \\frac{\\sum_{i=1}^{k} n_i (\\bar{Z}_{i.} - \\bar{Z}_{..})^2}{\\sum_{i=1}^{k} \\sum_{j=1}^{n_i} (Z_{ij} - \\bar{Z}_{i.})^2} $$
        <p>dimana \\(Z_{ij} = |Y_{ij} - \\bar{Y}_{i.}|\\) (deviasi dari rata-rata) atau \\(Z_{ij} = |Y_{ij} - \\text{median}(Y_i)|\\) (deviasi dari median).</p>
        
        <h5>Keputusan</h5>
        <p>Hipotesis nol (\\(H_0\\)) ditolak jika nilai statistik uji L lebih besar dari nilai kritis F:</p>
        $$ L > F_{(\\alpha, k-1, N-k)} $$
        
    
      '),
              
               ), 
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 6 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p6_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI KESAMAAN VARIANS (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p6_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File Excel" = "file")),
                conditionalPanel(
                  condition = "input.p6_input_method == 'manual'",
                  textAreaInput("p6_manual_data_group", "Masukkan Data Grup (pisahkan koma):", placeholder = "Contoh: A, A, A, B, B, B, C, C, C"),
                  textAreaInput("p6_manual_data_value", "Masukkan Data Nilai (pisahkan koma):", placeholder = "Contoh: 10, 12, 11, 15, 14, 16, 20, 21, 19")
                ),
                conditionalPanel(
                  condition = "input.p6_input_method == 'file'",
                  fileInput("p6_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                  argonAlert(
                    icon = icon("info-circle"), status = "info",
                    "Gunakan format panjang: satu kolom untuk grup (kategorik), satu kolom untuk nilai (numerik)."
                  ),
                  tags$li("Contoh format:",
                          HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Metode} & \\textbf{Skor} \\\\ \\hline \\text{A} & 10 \\\\ \\text{A} & 12 \\\\ \\text{B} & 15 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                  ),
                  uiOutput("p6_sheet_selector"),
                  uiOutput("p6_column_selector_group"),
                  uiOutput("p6_column_selector_value")
                ),
                hr(),
                
                # Pengaturan Uji
                selectInput("p6_test_type", "Pilih Metode Uji:",
                            choices = list("Uji Levene (Rekomendasi)" = "levene", "Uji Bartlett" = "bartlett")
                ),
                sliderInput("p6_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                actionButton("p6_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p6_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p6_data_table"))
              ),
              box(
                title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p6_plot"))
              )
            )
          )
        } 
        else if (i == 7) {
          # --- Pertemuan 7: Kalkulator ANOVA Satu Arah ---
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: ANOVA Satu Arah",
              icon = icon("project-diagram"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <h4 style="margin-top: 0;">Definisi dan Asumsi ANOVA</h4>
        <p>Analisis of Variance (ANOVA) adalah metode untuk menguji kesamaan rata-rata dari lebih dari dua populasi. Metode ini memiliki asumsi-asumsi sebagai berikut:</p>
        <ul>
          <li>Sampel diambil secara acak dan bersifat independen antar sampel.</li>
          <li>Setiap populasi diasumsikan berdistribusi normal.</li>
          <li>Setiap populasi memiliki varians yang sama (homogen).</li>
        </ul>

        <hr>

        <h4>Model dan Hipotesis ANOVA Satu Arah</h4>
        <p>Model statistik untuk setiap pengamatan dalam ANOVA satu arah (effects model) adalah:</p>
        $$y_{ij} = \\mu + \\tau_j + \\epsilon_{ij}$$
        <p>dimana \\(\\mu\\) adalah rata-rata keseluruhan, \\(\\tau_j\\) adalah pengaruh perlakuan ke-j, dan \\(\\epsilon_{ij}\\) adalah galat acak.</p>

        <p>Hipotesis yang diuji adalah:</p>
        <ul>
          <li>\\(H_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k\\)</li>
          <li>\\(H_1: \\text{Minimal terdapat satu rata-rata yang tidak sama}\\)</li>
        </ul>

        <hr>

        <h4>Partisi Variasi dan Tabel ANOVA</h4>
        <p>Prinsip dasar ANOVA adalah mempartisi atau memecah total variasi data (SST) menjadi variasi antar kelompok (SSB) dan variasi dalam kelompok (SSW atau galat).</p>
        $$SST = SSB + SSW$$
        <p><b>Tabel ANOVA Satu Arah</b></p>
        $$
        \\begin{array}{|l|c|c|c|c|}
        \\hline
        \\textbf{Sumber Keragaman} & \\textbf{Jumlah Kuadrat (SS)} & \\textbf{Derajat Bebas (df)} & \\textbf{Kuadrat Tengah (MS)} & \\mathbf{F_{hitung}} \\\\
        \\hline
        \\text{Antar Kelompok (Between)} & SSB & k-1 & MSB = \\frac{SSB}{k-1} & {\\frac{MSB}{MSW}} \\\\

        \\text{Dalam Kelompok (Within/Galat)} & SSW & N-k & MSW = \\frac{SSW}{N-k} & \\\\
        \\hline
        \\text{Total} & SST & N-1 & & \\\\
        \\hline
        \\end{array}
        $$
        <p>Keputusan diambil dengan membandingkan nilai \\(F_{hitung}\\) dengan nilai kritis dari tabel F. H₀ ditolak jika \\(F_{hitung} > F_{\\alpha; k-1; N-k}\\).</p>

        <hr>

        <h4>Uji Lanjut (Post-Hoc Tests)</h4>
        <p>Jika H₀ ditolak, uji lanjut dilakukan untuk mengetahui pasangan rata-rata mana yang berbeda secara signifikan.</p>
        <ul>
            <li>
                <strong>Uji Tukey (HSD):</strong> Uji ini membandingkan semua kemungkinan pasangan rata-rata. H₀ untuk pasangan (\\(\\mu_i = \\mu_j\\)) ditolak jika selisih absolut rata-rata sampelnya lebih besar dari nilai kritis Tukey (\\(T_\\alpha\\)). Nilai kritis dihitung dengan:
                $$T_{\\alpha} = q_{\\alpha}(k, N-k) \\sqrt{\\frac{MSW}{n}}$$
                <p>Formula sedikit berbeda jika ukuran sampel antar kelompok tidak sama.</p>
            </li>
            <li>
                <strong>Uji Duncan:</strong> Mirip dengan Tukey, namun menggunakan nilai pembanding yang berbeda tergantung pada jarak peringkat dua perlakuan yang dibandingkan. Nilai kritisnya adalah \\(R_k\\).
            </li>
        </ul>
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 7 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p7_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR ANOVA SATU ARAH (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p7_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File Excel" = "file")),
                conditionalPanel(
                  condition = "input.p7_input_method == 'manual'",
                  textAreaInput("p7_manual_data_group", "Masukkan Data Grup (pisahkan koma):", placeholder = "Contoh: A, A, A, B, B, B, C, C, C"),
                  textAreaInput("p7_manual_data_value", "Masukkan Data Nilai (pisahkan koma):", placeholder = "Contoh: 10, 12, 11, 15, 14, 16, 20, 21, 19")
                ),
                conditionalPanel(
                  condition = "input.p7_input_method == 'file'",
                  fileInput("p7_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                  argonAlert(
                    icon = icon("info-circle"), status = "info",
                    "Gunakan format panjang: satu kolom untuk grup (kategorik), satu kolom untuk nilai (numerik)."
                  ),
                  tags$li("Contoh format:",
                          HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Metode} & \\textbf{Skor} \\\\ \\hline \\text{A} & 10 \\\\ \\text{A} & 12 \\\\ \\text{B} & 15 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                  ),
                  uiOutput("p7_sheet_selector"),
                  uiOutput("p7_column_selector_group"),
                  uiOutput("p7_column_selector_value")
                ),
                hr(),
                
                # Pengaturan Uji
                selectInput("p7_posthoc_test", "Pilih Uji Lanjut (Post-Hoc):",
                            choices = list("Tukey HSD" = "tukey", "Duncan" = "duncan", "Tidak Ada" = "none")
                ),
                sliderInput("p7_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                actionButton("p7_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p7_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p7_data_table"))
              ),
              box(
                title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p7_plot"))
              )
            )
          )
        } 
        else if (i == 8) {
          argonTabItem(
            tabName = "pertemuan_8",
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            
            argonCard(
              width = 12,
              title = "Catatan Materi: ANOVA Dua Arah",
              icon = icon("sitemap"),
              status = "primary",
              shadow = TRUE,
              
              HTML(
                '
        <p>ANOVA dua arah digunakan untuk menguji perbedaan rata-rata suatu variabel yang dipengaruhi oleh dua faktor atau kriteria berbeda. Asumsi yang digunakan sama dengan ANOVA satu arah, yaitu independensi, normalitas, dan homogenan varians. </p>

        <hr>

        <h4 style="margin-top: 0;">1. ANOVA Dua Arah Tanpa Replikasi</h4>
        <p>Model ini digunakan ketika hanya ada satu pengamatan untuk setiap kombinasi level dari dua faktor. </p>

        <h5>Model dan Hipotesis</h5>
        <p>Model statistik yang digunakan adalah: </p>
        $$ y_{ij} = \\mu + F1_i + F2_j + \\epsilon_{ij} $$
        <p>untuk \\(i=1,2,...,r\\) dan \\(j=1,2,...,c\\)</p>
        <p>dimana \\(\\mu\\) adalah rata-rata keseluruhan, \\(F1_i\\) adalah pengaruh kategori ke-i dari faktor 1, \\(F2_j\\) adalah pengaruh kategori ke-j dari faktor 2, dan \\(\\epsilon_{ij}\\) adalah pengaruh galat acak.</p>
        <p>Hipotesis yang diuji adalah pengaruh utama dari masing-masing faktor secara terpisah:</p>
        <ul>
          <li><b>Untuk Faktor 1:</b>
            <ul>
              <li>\\(H_0\\): tidak terdapat perbedaan rata-rata berdasarkan faktor 1</li>
              <li>\\(H_1\\): minimal terdapat satu perbedaan rata-rata berdasarkan faktor 1</li>
            </ul>
          </li>
          <li><b>Untuk Faktor 2:</b>
            <ul>
              <li>\\(H_0\\): tidak terdapat perbedaan rata-rata berdasarkan faktor 2</li>
              <li>\\(H_1\\): minimal terdapat satu perbedaan rata-rata berdasarkan faktor 2</li>
            </ul>
          </li>
        </ul>

        <h5>Tabel ANOVA Tanpa Replikasi</h5>
        <p>Tabel ANOVA dua arah tanpa replikasi:</p>
        $$
        \\begin{array}{|l|c|c|c|c|}
        \\hline
        \\textbf{Sumber Keragaman} & \\textbf{Jumlah Kuadrat (JK)} & \\textbf{Derajat Bebas (df)} & \\textbf{Kuadrat Tengah (KT)} & \\mathbf{F_{hitung}} \\\\
        \\hline
        \\text{Faktor 1} & JKF1 & r-1 & \\frac{JKF1}{r-1} & \\frac{KT(F1)}{KTG} \\\\
        \\text{Faktor 2} & JKF2 & c-1 & \\frac{JKF2}{c-1} & \\frac{KT(F2)}{KTG} \\\\
        \\text{Galat (Error)} & JKG & (r-1)(c-1) & \\frac{JKG}{(r-1)(c-1)} & \\\\
        \\text{Total} & JKT & rc-1 & & \\\\
        \\hline
        \\end{array}
        $$
        <p>Tolak \\(H_0\\) jika \\(F_{hitung}\\) lebih besar dari nilai kritis F tabel.</p>

        <hr>

        <h4>2. ANOVA Dua Arah Dengan Replikasi</h4>
        <p>Model ini digunakan ketika terdapat lebih dari satu pengamatan (replikasi) untuk setiap kombinasi level faktor.</p>

        <h5>Model dan Hipotesis (Dengan Interaksi)</h5>
        <p>Model statistik dengan interaksi adalah: </p>
        $$ y_{ijk} = \\mu_{ij} + \\epsilon_{ijk} $$
        <p>Dimana \\(\\mu_{ij}=\\mu+F1_i+F2_j+(F12)_{ij}\\).</p>
        <p>Selain menguji pengaruh utama Faktor 1 dan Faktor 2, model ini juga menguji hipotesis interaksi: </p>
        <ul>
            <li>\\(H_0\\): tidak terdapat perbedaan rata-rata berdasarkan faktor 1 dan faktor 2 (tidak ada interaksi) </li>
            <li>\\(H_1\\): minimal terdapat satu perbedaan rata-rata berdasarkan faktor 1 dan faktor 2 (ada interaksi) </li>
        </ul>

        <h5>Tabel ANOVA Dengan Replikasi dan Interaksi</h5>
        <p>Tabel ANOVA dua arah dengan replikasi dan interaksi:</p>
        $$
        \\begin{array}{|l|c|c|c|c|}
        \\hline
        \\textbf{Sumber Keragaman} & \\textbf{JK} & \\textbf{df} & \\textbf{KT} & \\mathbf{F_{hit}} \\\\
        \\hline
        \\text{Faktor 1} & JKF1 & r-1 & s_{1}^2=\\frac{JKF1}{r-1} & f_{1}=\\frac{s_{1}^{2}}{s_{4}^{2}} \\\\
        \\text{Faktor 2} & JKF2 & c-1 & s_{2}^2=\\frac{JKF2}{c-1} & f_{2}=\\frac{s_{2}^{2}}{s_{4}^{2}} \\\\
        \\text{Interaksi} & JK(F12) & (r-1)(c-1) & s_{3}^2=\\frac{JK(F12)}{(r-1)(c-1)} & f_{3}=\\frac{s_{3}^{2}}{s_{4}^{2}} \\\\
        \\text{Galat (Error)} & JKG & rc(n-1) & s_{4}^2=\\frac{JKG}{rc(n-1)} & \\\\
        \\text{Total} & JKT & rcn-1 & & \\\\
        \\hline
        \\end{array}
        $$
        <p>Jika efek interaksi terbukti signifikan, interpretasi terhadap pengaruh utama masing-masing faktor harus dilakukan dengan hati-hati.</p>

        <hr>

        <h4>Uji Lanjut (Post-Hoc Tests)</h4>
        <p>Jika H₀ ditolak untuk faktor utama atau interaksi, uji lanjut seperti <b>Uji Tukey (HSD)</b> atau <b>Uji Duncan</b> dapat dilakukan untuk mengetahui pasangan rata-rata mana yang berbeda secara signifikan. </p>
      '
              )
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 8 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p8_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            
            # --- KALKULATOR ANOVA DUA ARAH (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p8_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File Excel" = "file")),
                conditionalPanel(
                  condition = "input.p8_input_method == 'manual'",
                  textAreaInput("p8_manual_data_factor1", "Masukkan Data Faktor 1 (pisahkan koma):", placeholder = "Contoh: A, A, A, B, B, B"),
                  textAreaInput("p8_manual_data_factor2", "Masukkan Data Faktor 2 (pisahkan koma):", placeholder = "Contoh: X, X, Y, Y, Z, Z"),
                  textAreaInput("p8_manual_data_value", "Masukkan Data Nilai (pisahkan koma):", placeholder = "Contoh: 10, 12, 11, 15, 14, 16")
                ),
                conditionalPanel(
                  condition = "input.p8_input_method == 'file'",
                  fileInput("p8_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                  argonAlert(
                    icon = icon("info-circle"), status = "info",
                    "Gunakan format panjang: satu kolom untuk nilai (numerik) dan dua kolom untuk faktor (kategorik).",
                    tags$li("Contoh format:",
                            HTML('$$ \\begin{array}{|c|c|c|} \\hline \\textbf{Skor} & \\textbf{Pupuk} & \\textbf{Siram} \\\\ \\hline 10 & \\text{A} & \\text{Rendah} \\\\ 12 & \\text{A} & \\text{Tinggi} \\\\ 15 & \\text{B} & \\text{Rendah} \\\\ ... & ... & ... \\\\ \\hline \\end{array} $$')
                    )
                  ),
                  uiOutput("p8_sheet_selector"),
                  uiOutput("p8_column_selector_value"),
                  uiOutput("p8_column_selector_factor1"),
                  uiOutput("p8_column_selector_factor2")
                ),
                hr(),
                
                # Pengaturan Uji
                checkboxInput("p8_include_interaction", "Sertakan Efek Interaksi", value = TRUE),
                selectInput("p8_posthoc_test", "Pilih Uji Lanjut (Post-Hoc):",
                            choices = list("Tukey HSD" = "tukey", "Tidak Ada" = "none")
                ),
                sliderInput("p8_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                actionButton("p8_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p8_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p8_data_table"))
              ),
              box(
                title = "Visualisasi Data (Interaction Plot)", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p8_plot"))
              )
            )
          )
        } 
        else if (i == 9) {
          # Pertemuan 9 khusus untuk Uji Proporsi Lebih dari Dua Populasi
          argonTabItem(tabName = "pertemuan_9",
                       h2(daftar_judul_pertemuan[9], class = "text-center mb-4"),
                       
                       argonCard(
                         width = 12,
                         title = "Catatan Materi: Uji Proporsi Beberapa Populasi",
                         icon = icon("pie-chart"),
                         status = "primary",
                         shadow = TRUE,
                         
                         HTML('
        <p>Uji proporsi beberapa populasi bertujuan untuk menguji kesamaan proporsi suatu karakteristik di semua populasi (k populasi). Asumsi yang digunakan adalah populasi saling bebas, pemilihan sampel acak, dan kategori karakteristik tidak tumpang tindih (mutually exclusive). Pengujian ini menggunakan statistik uji <b>Chi-Square</b> (\\(\\chi^2\\)).</p>
        
        <hr>
        
        <h4 style="margin-top: 0;">1. Pengujian k Populasi Binomial</h4>
        <p>Dilakukan pada kasus yang hanya memiliki dua kemungkinan hasil (misalnya: Setuju/Tidak Setuju, Sukses/Gagal).</p>
        
        <h5>A. Parameter Tidak Diketahui</h5>
        <ul>
          <li><b>Hipotesis:</b>
            <ul>
              <li>\\(H_0: p_1 = p_2 = \\dots = p_k\\) (semua populasi memiliki proporsi yang sama).</li>
              <li>\\(H_1:\\) Sedikitnya ada satu proporsi yang tidak sama.</li>
            </ul>
          </li>
          <li><b>Statistik Uji:</b>
            $$ \\chi^2_{hitung} = \\sum_{j=1}^{k} \\sum_{i=1}^{r} \\frac{(O_{ij} - e_{ij})^2}{e_{ij}} $$
            Dimana \\(O_{ij}\\) adalah frekuensi observasi dan \\(e_{ij}\\) adalah frekuensi harapan yang diestimasi dari data.
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha, (k-1)}\\). Derajat bebasnya adalah (jumlah populasi - 1).</li>
        </ul>
        
        <h5>B. Parameter Diketahui</h5>
        <ul>
          <li><b>Hipotesis:</b>
            <ul>
                <li>\\(H_0: p_1 = p_2 = \\dots = p_k = p\\) (proporsi semua populasi sama dengan nilai p tertentu).</li>
                <li>\\(H_1:\\) Setidaknya ada satu proporsi yang tidak sama dengan p.</li>
            </ul>
          </li>
          <li><b>Statistik Uji:</b> Menggunakan rumus \\(\\chi^2\\) yang sama.</li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha, k}\\). Perhatikan, derajat bebasnya adalah \\(k\\) (jumlah populasi).</li>
        </ul>
        
        <hr>
        
        <h4>2. Pengujian k Populasi Multinomial</h4>
        <p>Dilakukan pada kasus yang memiliki lebih dari dua kemungkinan hasil (misalnya: preferensi beberapa merek produk).</p>
        
        <h5>A. Parameter Tidak Diketahui</h5>
        <ul>
            <li><b>Hipotesis:</b> \\(H_0\\) menyatakan bahwa untuk setiap kategori, proporsinya sama di semua populasi (\\(p_{i1} = p_{i2} = \\dots = p_{ik}\\) untuk setiap kategori \\(i\\)).</li>
            <li><b>Prosedur:</b> Sama seperti kasus binomial (parameter tidak diketahui), menggunakan tabel kontingensi r x k dan menghitung frekuensi harapan dari data.</li>
            <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha, (r-1)(k-1)}\\).</li>
        </ul>
        
        <h5>B. Parameter Diketahui</h5>
        <ul>
            <li><b>Hipotesis:</b> \\(H_0\\) menyatakan bahwa proporsi untuk setiap kategori (i) adalah sama di semua populasi dan sama dengan nilai yang telah ditentukan (\\(p_{i1} = p_{i2} = \\dots = p_{ik} = p_{i.}\\)).</li>
            <li><b>Prosedur:</b> Frekuensi harapan dihitung berdasarkan proporsi yang diketahui: \\(e_{ij} = (n_j)(p_{i.})\\).</li>
            <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha, k(r-1)}\\). Derajat bebasnya adalah jumlah populasi dikali (jumlah kategori - 1).</li>
        </ul>
        <hr>
      
        '),  
                         
                         tags$li("Contoh format data tabel kontingensi :",
                                 # Bungkus kode LaTeX dengan HTML() dan $$...$$
                                 HTML('
                                    
                                  \\begin{array}{|c|c|c|C|}
                                  \\hline
                                   \\textbf{Main Genshin} & \\textbf{KS} & \\textbf{ST} & \\textbf{D3} \\\\
                                   \\hline
                                   Iya & 45 & 38 & 50 \\\\
                                   tidak & 15 & 22 & 10 \\\\
                                   \\hline
                                   \\end{array}
                         
                            ')
                         ),
                         tags$li("Contoh format data mentah:",
                                 # Bungkus kode LaTeX dengan HTML() dan $$...$$
                                 HTML('
                  
                                  \\begin{array}{|c|c|c|}
                                  \\hline
                                   \\textbf{NO} & \\textbf{Status Ruta} & \\textbf{Desa} \\\\
                                   \\hline
                                   1 & 1 & 1 \\\\
                                   2 & 1 & 2 \\\\
                                   3 & 2 & 1 \\\\
                                   4 & 2 & 2\\\\
                                   \\hline
                                   \\end{array}
                         
                            ')
                         ),
                       ),
                       # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 9 ---
                       argonRow(
                         center = TRUE,
                         argonColumn(
                           width = 12,
                           align = "center",
                           style = "padding: 20px;",
                           actionButton("start_quiz_p9_new", "Kerjakan Kuis Interaktif", 
                                        icon = icon("rocket"), 
                                        class = "btn-primary btn-lg")
                         )
                       ),
                       
                       # --- KALKULATOR UJI CHI-SQUARE (DIPERBARUI) ---
                       fluidRow(
                         box(
                           title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                           
                           # Input Data
                           radioButtons("p9_input_method", "Metode Input:", choices = list("Manual (Tabel Kontingensi)" = "manual", "Upload File Excel" = "file")),
                           conditionalPanel(
                             condition = "input.p9_input_method == 'manual'",
                             rHandsontableOutput("p9_manual_table")
                           ),
                           conditionalPanel(
                             condition = "input.p9_input_method == 'file'",
                             fileInput("p9_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                             radioButtons("p9_file_format", "Format Data Excel:", choices = list("Data Mentah (Long Format)" = "raw", "Tabel Kontingensi (Wide Format)" = "contingency")),
                             
                             # --- PERUBAHAN DI SINI ---
                             argonAlert(
                               icon = icon("info-circle"), status = "info",
                               "Pilih format data yang sesuai dengan file Excel Anda.",
                               tags$li(strong("Data Mentah:"), " Dua kolom, satu untuk baris (misal: 'Preferensi'), satu untuk kolom (misal: 'Kota')."),
                               HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Preferensi} & \\textbf{Kota} \\\\ \\hline \\text{Kopi} & \\text{Jakarta} \\\\ \\text{Teh} & \\text{Jakarta} \\\\ \\text{Kopi} & \\text{Bandung} \\\\ ... & ... \\\\ \\hline \\end{array} $$'),
                               tags$li(strong("Tabel Kontingensi:"), " Data sudah dalam bentuk tabel frekuensi, dengan kategori baris di kolom pertama."),
                               HTML('$$ \\begin{array}{|l|c|c|} \\hline \\textbf{Preferensi} & \\textbf{Jakarta} & \\textbf{Bandung} \\\\ \\hline \\text{Kopi} & 50 & 30 \\\\ \\text{Teh} & 45 & 40 \\\\ \\text{Susu} & 20 & 25 \\\\ \\hline \\end{array} $$')
                             ),
                             # --- AKHIR PERUBAHAN ---
                             
                             uiOutput("p9_sheet_selector"),
                             uiOutput("p9_column_selectors")
                           ),
                           hr(),
                           
                           # Pengaturan Uji
                           sliderInput("p9_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                           actionButton("p9_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
                         ),
                         
                         box(
                           title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                           uiOutput("p9_report_output_ui")
                         )
                       ),
                       fluidRow(
                         box(
                           title = "Tabel Kontingensi (Observasi)", status = "info", solidHeader = TRUE, width = 6,
                           withSpinner(DT::dataTableOutput("p9_data_table"))
                         ),
                         box(
                           title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                           withSpinner(plotlyOutput("p9_plot"))
                         )
                       )
          )
          
        } 
        else if (i == 10) {
          # --- Pertemuan 10: Kalkulator Uji Nonparametrik Satu Sampel ---
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: Uji Nonparametrik Satu Sampel",
              icon = icon("check-circle"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <p>Uji statistik nonparametrik adalah metode yang tidak memerlukan asumsi mengenai sebaran data populasi (bebas distribusi).</p>

        <hr>

        <h4 style="margin-top: 0;">Uji Tanda (Sign Test)</h4>
        <p>Uji Tanda adalah salah satu uji nonparametrik yang paling dasar, digunakan untuk menguji hipotesis tentang median populasi (M) pada satu kelompok sampel atau pada data berpasangan.</p>

        <h5>Asumsi & Prosedur</h5>
        <ul>
            <li>Variabel yang diamati bersifat kontinu dan minimal berskala ordinal.</li>
            <li>Prosedur pengujian dilakukan dengan menghitung selisih setiap data terhadap median hipotesis (\\(M_0\\)), lalu memberikan tanda \'+\' jika selisih positif, \'-\' jika negatif, dan \'0\' jika sama.</li>
        </ul>

        <h5>Hipotesis & Statistik Uji</h5>
        <p>Hipotesis yang umum diuji adalah \\(H_0: M = M_0\\) versus \\(H_1: M \\neq M_0\\), atau uji satu arah (\\(H_1: M > M_0\\) atau \\(H_1: M < M_0\\)).</p>
        <ul>
            <li>
                <strong>Sampel Kecil (N ≤ 25):</strong> Statistik uji (x) adalah jumlah tanda positif atau negatif yang lebih sedikit. Keputusan diambil berdasarkan probabilitas dari distribusi binomial \\(b(N; 0.5)\\).
            </li>
            <li>
                <strong>Sampel Besar (N > 25):</strong> Menggunakan pendekatan distribusi normal dengan statistik uji Z:
                $$ Z = \\frac{(x \\pm 0.5) - \\frac{N}{2}}{0.5\\sqrt{N}} $$
                <p>Dimana x adalah jumlah tanda (+) atau (-), dan N adalah ukuran sampel efektif (tanpa tanda \'0\').</p>
            </li>
        </ul>

        <hr>
        
        <h4>Uji Tanda Data Berpasangan(Paired Sign Test)</h4>
        <p>Uji ini digunakan untuk melihat apakah ada perbedaan antara dua kondisi pada data berpasangan tanpa melihat besarnya perbedaan yang terjadi. Skala data yang digunakan minimal ordinal.</p>

        <h5>Asumsi & Prosedur</h5>
        <ul>
            <li>Proesedur pengujian dilakukan dengan menghitung selisih antara pasangan data (misalnya, sebelum dan sesudah), lalu memberikan tanda + atau - pada selisih tersebut.</li>
            <li><b>Uji Dua Arah:</b> \\(H_0: p(X_a > X_b) = 0.5\\) vs \\(H_1: p(X_a > X_b) \neq 0.5\\).</li>
                    <li><b>Uji Satu Arah (Kanan):</b> \\(H_0: p(X_a > X_b) = 0.5\\) vs \\(H_1: p(X_a > X_b) > 0.5\\).</li>
                    <li><b>Uji Satu Arah (Kiri):</b> \\(H_0: p(X_a > X_b) = 0.5\\) vs \\(H_1: p(X_a > X_b) < 0.5\\).</li>
        </ul>

        <h5>Statistik Uji</h5>
        <ul>
            <li>
                <strong>Sampel Kecil (N ≤ 25):</strong> Statistik uji (x) adalah dari distribusi binomial, dimana x adalah jumlah tanda yang lebih sedikit (untuk uji dua arah) atau jumlah tanda + (n) atau - (m) tergantung hipotesis satu arahnya.
            </li>
            <li>
                <strong>Sampel Besar (N > 25):</strong> Menggunakan pendekatan distribusi normal dengan statistik uji Z:
                $$ Z = \\frac{(x \\pm 0.5) - \\frac{N}{2}}{0.5\\sqrt{N}} $$
                <p>Dimana x adalah jumlah tanda (+) atau (-), dan N adalah ukuran sampel efektif (tanpa tanda \'0\').</p>
            </li>
        </ul>
        
        <hr>

        <h4>Uji Keacakan (Run Test)</h4>
        <p>Uji ini digunakan untuk memeriksa apakah suatu data dalam sampel tunggal memiliki pola tertentu atau tersusun secara acak (random).</p>

        <h5>Hipotesis & Statistik Uji</h5>
        <p>Hipotesis yang diuji adalah \\(H_0\\): Urutan data bersifat acak vs. \\(H_1\\): Urutan data tidak acak.</p>
        <ul>
            <li>
                <strong>Sampel Kecil (m dan n ≤ 20):</strong> Keputusan diambil dengan membandingkan jumlah larian (runs) \\(r\\) dengan nilai kritis dari tabel G. H₀ diterima jika \\(r\\) berada di antara nilai kritis bawah dan atas.
            </li>
            <li>
                <strong>Sampel Besar (m atau n > 20):</strong> Menggunakan pendekatan distribusi normal dengan statistik uji Z:
                $$ Z = \\frac{r - \\mu_r}{\\sigma_r} $$
                <p>Dimana \\(r\\) adalah jumlah larian, serta \\(\\mu_r\\) dan \\(\\sigma_r\\) adalah rata-rata dan simpangan baku dari distribusi larian.</p>
                $$ \\mu_r = \\frac{2mn}{m+n} + 1 \\quad \\text{dan} \\quad \\sigma_r = \\sqrt{\\frac{2mn(2mn-m-n)}{(m+n)^2(m+n-1)}} $$
            </li>
        </ul>
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 10 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p10_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI NONPARAMETRIK 1 SAMPEL (DIPERBARUI) ---
            argonTabSet(
              id = "tabset_pertemuan10_calc",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              
              # --- TAB UNTUK UJI TANDA (SIGN TEST) ---
              argonTab(
                tabName = "Uji Tanda (1 Sampel)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji Tanda", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p10_st_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p10_st_input_method == 'manual'",
                      textAreaInput("p10_st_manual_data", "Masukkan Data (pisahkan koma):", placeholder = "18, 22, 25, 19")
                    ),
                    conditionalPanel(
                      "input.p10_st_input_method == 'file'",
                      fileInput("p10_st_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Pastikan file memiliki satu kolom numerik.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|} \\hline \\textbf{Nilai_Data} \\\\ \\hline 18 \\\\ 22 \\\\ 25 \\\\ ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p10_st_sheet_selector"),
                      uiOutput("p10_st_column_selector")
                    ),
                    hr(),
                    numericInput("p10_st_median_h0", "Median Hipotesis (M₀):", value = 20),
                    selectInput("p10_st_alternative", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Kurang Dari (<)" = "less", "Lebih Dari (>)" = "greater")),
                    sliderInput("p10_st_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p10_st_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p10_st_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p10_st_data_table"))),
                        column(6, withSpinner(plotlyOutput("p10_st_plot")))
                      )
                  )
                )
              ),
              
              # --- TAB BARU UNTUK UJI TANDA BERPASANGAN ---
              argonTab(
                tabName = "Uji Tanda Berpasangan",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji Tanda Berpasangan", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p10_pst_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p10_pst_input_method == 'manual'",
                      textAreaInput("p10_pst_manual_data_a", "Data Sampel 1 (misal: Sebelum):", placeholder = "75, 80, 82"),
                      textAreaInput("p10_pst_manual_data_b", "Data Sampel 2 (misal: Sesudah):", placeholder = "78, 81, 85")
                    ),
                    conditionalPanel(
                      "input.p10_pst_input_method == 'file'",
                      fileInput("p10_pst_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Pastikan file memiliki dua kolom numerik.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Sebelum} & \\textbf{Sesudah} \\\\ \\hline 75 & 78 \\\\ 80 & 81 \\\\ 82 & 85 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p10_pst_sheet_selector"),
                      fluidRow(
                        column(6, uiOutput("p10_pst_column_selector_a")),
                        column(6, uiOutput("p10_pst_column_selector_b"))
                      )
                    ),
                    hr(),
                    selectInput("p10_pst_alternative", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Sampel 1 < Sampel 2" = "less", "Sampel 1 > Sampel 2" = "greater")),
                    sliderInput("p10_pst_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p10_pst_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p10_pst_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p10_pst_data_table"))),
                        column(6, withSpinner(plotlyOutput("p10_pst_plot")))
                      )
                  )
                )
              ),
              
              # --- TAB UNTUK UJI KEACAKAN (RUNS TEST) ---
              argonTab(
                tabName = "Uji Keacakan",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji Keacakan", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p10_rt_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p10_rt_input_method == 'manual'",
                      textAreaInput("p10_rt_manual_data", "Masukkan Urutan Data (pisahkan koma):", placeholder = "15, 18, 12, 20, 11")
                    ),
                    conditionalPanel(
                      "input.p10_rt_input_method == 'file'",
                      fileInput("p10_rt_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Pastikan file memiliki satu kolom data numerik.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|} \\hline \\textbf{Urutan_Data} \\\\ \\hline 15 \\\\ 18 \\\\ 12 \\\\ ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p10_rt_sheet_selector"),
                      uiOutput("p10_rt_column_selector")
                    ),
                    hr(),
                    # --- PERUBAHAN DI SINI ---
                    radioButtons("p10_rt_cutoff_method", "Pilih Titik Potong (Cut-off):",
                                 choices = list("Median (Otomatis)" = "median",
                                                "Mean (Otomatis)" = "mean",
                                                "Manual" = "manual"),
                                 selected = "median"),
                    conditionalPanel(
                      "input.p10_rt_cutoff_method == 'manual'",
                      numericInput("p10_rt_cutoff_manual", "Masukkan Nilai Cut-off Manual:", value = 15)
                    ),
                    # --- AKHIR PERUBAHAN ---
                    sliderInput("p10_rt_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p10_rt_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p10_rt_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p10_rt_data_table"))),
                        column(6, withSpinner(plotlyOutput("p10_rt_plot")))
                      )
                  )
                )
              )
            )
          )
        } 
        else if (i == 11) {
          argonTabItem(
            tabName = "pertemuan_11",
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: Uji Nonparametrik Dua Sampel",
              icon = icon("people-arrows"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <h4 style="margin-top: 0;">1. Wilcoxon Signed Rank Test (Sampel Berpasangan)</h4>
        <p>Uji ini digunakan untuk mengukur signifikansi perbedaan antara <b>dua kelompok data berpasangan</b> (dependen). Uji ini merupakan alternatif dari uji-t berpasangan jika asumsi normalitas tidak terpenuhi. Skala data minimal adalah ordinal.</p>
        <ul>
            <li><b>Hipotesis:</b> Menguji median dari selisih data (\\(M_D\\)), contohnya \\(H_0: M_D = 0\\) vs. \\(H_1: M_D \\neq 0\\).</li>
            <li>
                <b>Statistik Uji:</b> Dihitung dari peringkat selisih absolut (\\(|d_i|\\)).
                <ul>
                    <li><b>Sampel Kecil (N ≤ 25):</b> Statistik uji \\(T\\) adalah nilai yang lebih kecil antara jumlah peringkat positif (\\(T^+\\)) dan jumlah peringkat negatif (\\(T^-\\)). Keputusan diambil dengan membandingkan \\(T\\) dengan nilai kritis dari tabel Wilcoxon.</li>
                    <li><b>Sampel Besar (N > 25):</b> Menggunakan pendekatan normal dengan statistik uji Z.
                        $$ Z = \\frac{T - \\mu_T}{\\sigma_T} $$
                        Dimana \\(\\mu_T = \\frac{N(N+1)}{4}\\) dan \\(\\sigma_T = \\sqrt{\\frac{N(N+1)(2N+1)}{24}}\\).
                    </li>
                </ul>
            </li>
        </ul>

        <hr>

        <h4>2. Mann-Whitney U Test (Sampel Independen)</h4>
        <p>Uji ini digunakan untuk membandingkan <b>dua sampel independen</b> untuk mengetahui apakah kedua kelompok berasal dari populasi yang sama. Uji ini merupakan alternatif dari uji-t independen. Skala data minimal adalah ordinal.</p>
        <ul>
            <li><b>Hipotesis:</b> Menguji kesamaan median dari dua populasi (\\(H_0: M_1 = M_2\\)).</li>
            <li>
                <b>Statistik Uji:</b> Data dari kedua kelompok digabung dan diperingkat. Statistik uji \\(U\\) adalah nilai terkecil dari \\(U_1\\) dan \\(U_2\\).
                $$ U_1 = n_1 n_2 + \\frac{n_1(n_1+1)}{2} - R_1 $$
                $$ U_2 = n_1 n_2 + \\frac{n_2(n_2+1)}{2} - R_2 $$
                Dimana \\(R_1\\) dan \\(R_2\\) adalah jumlah peringkat dari masing-masing sampel.
            </li>
            <li>
                <b>Keputusan:</b>
                <ul>
                    <li><b>Sampel Kecil (n₂ ≤ 20):</b> Bandingkan nilai \\(U\\) hitung dengan \\(U\\) tabel dari tabel Mann-Whitney. Tolak \\(H_0\\) jika \\(U_{hitung} \\le U_{tabel}\\).</li>
                    <li><b>Sampel Besar (n₂ > 20):</b> Menggunakan pendekatan normal dengan statistik uji Z.
                        $$ Z = \\frac{U - \\mu_U}{\\sigma_U} $$
                        Dimana \\(\\mu_U = \\frac{n_1 n_2}{2}\\) dan \\(\\sigma_U = \\sqrt{\\frac{n_1 n_2 (n_1+n_2+1)}{12}}\\).
                    </li>
                </ul>
            </li>
        </ul>

        <hr>

        <h4>3. Kolmogorov-Smirnov Test (Sampel Independen)</h4>
        <p>Uji ini membandingkan <b>distribusi kumulatif</b> dari dua sampel independen untuk mengetahui apakah kedua sampel berasal dari distribusi populasi yang sama. Skala data minimal adalah ordinal.</p>
        <ul>
            <li><b>Hipotesis:</b> Menguji kesamaan fungsi distribusi kumulatif (\\(H_0: F_1(x) = F_2(x)\\)).</li>
            <li><b>Statistik Uji:</b> Statistik uji \\(D\\) adalah selisih absolut maksimum antara fungsi distribusi kumulatif empiris dari kedua sampel (\\(S_1(x)\\) dan \\(S_2(x)\\)).
                $$ D = \\max|S_1(x) - S_2(x)| $$
            </li>
            <li><b>Keputusan:</b> Tolak \\(H_0\\) jika nilai \\(D\\) lebih besar dari nilai kritis pada tabel Kolmogorov-Smirnov.</li>
        </ul>
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 11 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p11_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            # --- KALKULATOR UJI NONPARAMETRIK 2 SAMPEL (DIPERBARUI) ---
            argonTabSet(
              id = "tabset_pertemuan11_calc",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              
              # --- TAB UNTUK WILCOXON SIGNED-RANK TEST ---
              argonTab(
                tabName = "Wilcoxon Signed-Rank (Berpasangan)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p11_wsr_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p11_wsr_input_method == 'manual'",
                      textAreaInput("p11_wsr_manual_data_a", "Data Sampel 1 (misal: Sebelum):", placeholder = "28, 14, 32"),
                      textAreaInput("p11_wsr_manual_data_b", "Data Sampel 2 (misal: Sesudah):", placeholder = "27, 24, 29")
                    ),
                    conditionalPanel(
                      "input.p11_wsr_input_method == 'file'",
                      fileInput("p11_wsr_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Pastikan file memiliki dua kolom numerik.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Sebelum} & \\textbf{Sesudah} \\\\ \\hline 28 & 27 \\\\ 14 & 24 \\\\ 32 & 29 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p11_wsr_sheet_selector"),
                      fluidRow(
                        column(6, uiOutput("p11_wsr_column_selector_a")),
                        column(6, uiOutput("p11_wsr_column_selector_b"))
                      )
                    ),
                    hr(),
                    selectInput("p11_wsr_alternative", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Sampel 1 < Sampel 2" = "less", "Sampel 1 > Sampel 2" = "greater")),
                    sliderInput("p11_wsr_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p11_wsr_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p11_wsr_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p11_wsr_data_table"))),
                        column(6, withSpinner(plotlyOutput("p11_wsr_plot")))
                      )
                  )
                )
              ),
              
              # --- TAB UNTUK MANN-WHITNEY U TEST ---
              argonTab(
                tabName = "Mann-Whitney U (Independen)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p11_mw_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p11_mw_input_method == 'manual'",
                      textAreaInput("p11_mw_manual_data_group", "Data Grup (pisahkan koma):", placeholder = "A, A, A, B, B, B"),
                      textAreaInput("p11_mw_manual_data_value", "Data Nilai (pisahkan koma):", placeholder = "70, 75, 80, 50, 45, 60")
                    ),
                    conditionalPanel(
                      "input.p11_mw_input_method == 'file'",
                      fileInput("p11_mw_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Gunakan format panjang: satu kolom grup (2 kategori), satu kolom nilai.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Grup} & \\textbf{Nilai} \\\\ \\hline \\text{Pria} & 70 \\\\ \\text{Pria} & 75 \\\\ \\text{Wanita} & 50 \\\\ \\text{Wanita} & 45 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p11_mw_sheet_selector"),
                      uiOutput("p11_mw_column_selector_group"),
                      uiOutput("p11_mw_column_selector_value")
                    ),
                    hr(),
                    selectInput("p11_mw_alternative", "Hipotesis Alternatif (H₁):", choices = list("Tidak Sama Dengan (≠)" = "two.sided", "Grup 1 < Grup 2" = "less", "Grup 1 > Grup 2" = "greater")),
                    sliderInput("p11_mw_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p11_mw_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p11_mw_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p11_mw_data_table"))),
                        column(6, withSpinner(plotlyOutput("p11_mw_plot")))
                      )
                  )
                )
              ),
              
              # --- TAB UNTUK KOLMOGOROV-SMIRNOV TEST ---
              argonTab(
                tabName = "Kolmogorov-Smirnov (Independen)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p11_ks_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p11_ks_input_method == 'manual'",
                      textAreaInput("p11_ks_manual_data_group", "Data Grup (pisahkan koma):", placeholder = "A, A, A, B, B, B"),
                      textAreaInput("p11_ks_manual_data_value", "Data Nilai (pisahkan koma):", placeholder = "15, 18, 12, 20, 11, 22")
                    ),
                    conditionalPanel(
                      "input.p11_ks_input_method == 'file'",
                      fileInput("p11_ks_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Gunakan format panjang: satu kolom grup (2 kategori), satu kolom nilai.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Grup} & \\textbf{Nilai} \\\\ \\hline \\text{Pria} & 70 \\\\ \\text{Pria} & 75 \\\\ \\text{Wanita} & 50 \\\\ \\text{Wanita} & 45 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p11_ks_sheet_selector"),
                      uiOutput("p11_ks_column_selector_group"),
                      uiOutput("p11_ks_column_selector_value")
                    ),
                    hr(),
                    selectInput("p11_ks_alternative", "Hipotesis Alternatif (H₁):", choices = list("Distribusi Berbeda (two-sided)" = "two.sided", "Dist. Grup 1 < Dist. Grup 2" = "less", "Dist. Grup 1 > Dist. Grup 2" = "greater")),
                    sliderInput("p11_ks_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p11_ks_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p11_ks_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p11_ks_data_table"))),
                        column(6, withSpinner(plotlyOutput("p11_ks_plot")))
                      )
                  )
                )
              )
            )
          )
        } 
        else if (i == 12) { # Pertemuan 12: Uji Non-Parametrik K Sampel
          # --- KONTEN BARU UNTUK PERTEMUAN 12 ---
          argonTabItem(
            tabName = paste0("pertemuan_", i),
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            
            argonCard(
              width = 12,
              title = "Catatan Materi: Uji Nonparametrik >2 Sampel",
              icon = icon("layer-group"),
              status = "primary",
              shadow = TRUE,
              
              HTML('
        <h4 style="margin-top: 0;">1. Uji Kruskal-Wallis (Sampel Independen)</h4>
        <p>Uji Kruskal-Wallis adalah alternatif nonparametrik untuk ANOVA satu arah. Uji ini digunakan untuk membandingkan <b>lebih dari dua kelompok sampel yang saling independen</b> untuk mengetahui apakah kelompok-kelompok tersebut berasal dari populasi yang sama. Skala data minimal adalah ordinal.</p>
        
        <h5>Hipotesis & Prosedur</h5>
        <p>Hipotesis yang diuji adalah kesamaan median antar kelompok (\\(H_0: M_1 = M_2 = \\dots = M_k\\)). Prosedurnya adalah menggabungkan semua data dari semua kelompok, lalu memberikan peringkat (ranking) dari yang terkecil hingga terbesar.</p>
        
        <h5>Statistik Uji (H)</h5>
        <p>Statistik uji Kruskal-Wallis dihitung dengan rumus:</p>
        $$ H = \\frac{12}{n(n+1)} \\sum_{i=1}^{k} \\frac{R_i^2}{n_i} - 3(n+1) $$
        <p>Dimana \\(n\\) adalah total observasi, \\(k\\) adalah jumlah kelompok, \\(R_i\\) adalah jumlah peringkat pada kelompok ke-i, dan \\(n_i\\) adalah jumlah observasi pada kelompok ke-i.</p>
        
        <h5>Keputusan</h5>
        <p>Untuk sampel yang cukup besar (setiap kelompok \\(n_i > 5\\)), distribusi statistik \\(H\\) mendekati Chi-Square. Maka, \\(H_0\\) ditolak jika:</p>
        $$ H > \\chi^2_{(\\alpha, k-1)} $$
        
        <hr>
        
        <h4>2. Uji Friedman (Sampel Dependen/Berpasangan)</h4>
        <p>Uji Friedman adalah alternatif nonparametrik untuk ANOVA dua arah. Uji ini digunakan untuk membandingkan <b>lebih dari dua kelompok sampel yang dependen atau berpasangan</b> (misalnya, pengukuran berulang pada subjek yang sama). Skala data minimal adalah ordinal.</p>
        
        <h5>Hipotesis & Prosedur</h5>
        <p>Hipotesis yang diuji adalah kesamaan median antar perlakuan (\\(H_0: M_1 = M_2 = \\dots = M_k\\)). Prosedurnya adalah memberikan peringkat pada data <b>di dalam setiap blok (kelompok dependen)</b>, bukan secara keseluruhan.</p>
        
        <h5>Statistik Uji (\\(\\chi^2_r\\))</h5>
        <p>Statistik uji Friedman dihitung dengan rumus:</p>
        $$ \\chi^2_r = \\frac{12}{nk(k+1)} \\sum_{i=1}^{k} R_i^2 - 3n(k+1) $$
        <p>Dimana \\(n\\) adalah jumlah blok/subjek, \\(k\\) adalah jumlah perlakuan, dan \\(R_i\\) adalah jumlah peringkat untuk perlakuan ke-i.</p>
        
        <h5>Keputusan</h5>
        <p>Untuk sampel yang cukup besar, distribusi statistik \\(\\chi^2_r\\) mendekati Chi-Square. Maka, \\(H_0\\) ditolak jika:</p>
        $$ \\chi^2_r > \\chi^2_{(\\alpha, k-1)} $$
        
        <hr>
        
        <h4>Uji Lanjut (Post-Hoc)</h4>
        <p>Jika hasil uji Kruskal-Wallis atau Friedman signifikan (tolak \\(H_0\\)), maka dapat dilanjutkan dengan uji perbandingan berganda seperti <b>Uji Dunn</b> untuk mengetahui pasangan kelompok mana yang secara spesifik berbeda.</p>
      ')
            ),
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 12 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p12_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            
            # --- KALKULATOR UJI NONPARAMETRIK >2 SAMPEL (DIPERBARUI) ---
            argonTabSet(
              id = "tabset_pertemuan12_calc",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              
              # --- TAB UNTUK KRUSKAL-WALLIS TEST ---
              argonTab(
                tabName = "Kruskal-Wallis (Independen)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    radioButtons("p12_kw_input_method", "Metode Input:", choices = list("Manual" = "manual", "File Excel" = "file")),
                    conditionalPanel(
                      "input.p12_kw_input_method == 'manual'",
                      textAreaInput("p12_kw_manual_data_group", "Data Grup (pisahkan koma):", placeholder = "A, A, B, B, C, C"),
                      textAreaInput("p12_kw_manual_data_value", "Data Nilai (pisahkan koma):", placeholder = "10, 12, 15, 14, 20, 21")
                    ),
                    conditionalPanel(
                      "input.p12_kw_input_method == 'file'",
                      fileInput("p12_kw_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                      argonAlert(icon="info-circle", status="info", 
                                 "Gunakan format panjang: satu kolom grup, satu kolom nilai.",
                                 tags$li("Contoh format:",
                                         HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Metode} & \\textbf{Skor} \\\\ \\hline \\text{A} & 10 \\\\ \\text{A} & 12 \\\\ \\text{B} & 15 \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                                 )
                      ),
                      uiOutput("p12_kw_sheet_selector"),
                      uiOutput("p12_kw_column_selector_group"),
                      uiOutput("p12_kw_column_selector_value")
                    ),
                    hr(),
                    selectInput("p12_kw_posthoc", "Uji Lanjut (Post-Hoc):", choices = list("Dunn" = "dunn", "Tidak Ada" = "none")),
                    sliderInput("p12_kw_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p12_kw_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p12_kw_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p12_kw_data_table"))),
                        column(6, withSpinner(plotlyOutput("p12_kw_plot")))
                      )
                  )
                )
              ),
              
              # --- TAB UNTUK FRIEDMAN TEST ---
              argonTab(
                tabName = "Friedman (Berpasangan)",
                fluidRow(
                  box(
                    title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                    fileInput("p12_fr_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                    argonAlert(icon="info-circle", status="info", 
                               "Gunakan format panjang: satu kolom nilai, satu kolom perlakuan/kondisi, dan satu kolom ID subjek/blok.",
                               tags$li("Contoh format:",
                                       HTML('$$ \\begin{array}{|c|c|c|} \\hline \\textbf{Skor} & \\textbf{Waktu} & \\textbf{Pasien_ID} \\\\ \\hline 37 & \\text{Awal} & 1 \\\\ 38 & \\text{Minggu 1} & 1 \\\\ 42 & \\text{Awal} & 2 \\\\ 41 & \\text{Minggu 1} & 2 \\\\ ... & ... & ... \\\\ \\hline \\end{array} $$')
                               )
                    ),
                    uiOutput("p12_fr_sheet_selector"),
                    uiOutput("p12_fr_column_selector_value"),
                    uiOutput("p12_fr_column_selector_treatment"),
                    uiOutput("p12_fr_column_selector_block"),
                    hr(),
                    sliderInput("p12_fr_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                    actionButton("p12_fr_analyze", "Buat Laporan Analisis", class = "btn-primary", width="100%")
                  ),
                  box(
                    title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                    uiOutput("p12_fr_report_output_ui")
                  ),
                  box(title = "Data & Visualisasi", status = "info", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6, withSpinner(DT::dataTableOutput("p12_fr_data_table"))),
                        column(6, withSpinner(plotlyOutput("p12_fr_plot")))
                      )
                  )
                )
              )
            )
          )
        } 
        else if (i == 13) { # Pertemuan 13: Uji Korelasi
          argonTabItem(tabName = "pertemuan_13",
                       h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
                       
                       argonCard(
                         width = 12,
                         title = "Catatan Materi: Uji Korelasi",
                         icon = icon("link"),
                         status = "primary",
                         shadow = TRUE,
                         
                         HTML('
        <h4 style="margin-top: 0;">1. Uji Korelasi Pearson (Parametrik)</h4>
        <p>Uji korelasi Pearson digunakan untuk mengukur keeratan dan arah <b>hubungan linier</b> antara dua variabel berskala interval atau rasio. Koefisien korelasi sampel (r) bernilai antara -1 dan +1.</p>
        <ul>
          <li><b>Hipotesis:</b> Menguji apakah koefisien korelasi populasi (\\(\\rho\\)) sama dengan nol. Contoh: \\(H_0: \\rho = 0\\) (tidak ada hubungan linier) vs. \\(H_1: \\rho \\neq 0\\).</li>
          <li><b>Statistik Uji:</b> Koefisien korelasi sampel \\(r\\) dihitung dengan:
            $$ r = \\frac{n(\\sum XY) - (\\sum X)(\\sum Y)}{\\sqrt{[n\\sum X^2 - (\\sum X)^2][n\\sum Y^2 - (\\sum Y)^2]}} $$
          </li>
          <li>Untuk menguji signifikansi, nilai \\(r\\) dapat ditransformasi menjadi statistik uji-t dengan derajat bebas \\(v=n-2\\):
            $$ t = \\frac{r\\sqrt{n-2}}{\\sqrt{1-r^2}} $$
          </li>
        </ul>
        
        <hr>

        <h4>2. Uji Korelasi Spearman (Nonparametrik)</h4>
        <p>Uji ini mengukur keeratan hubungan antara dua variabel berskala minimal ordinal. Uji ini bekerja berdasarkan peringkat (ranking) data, bukan nilai aslinya, dan merupakan alternatif jika asumsi normal bivariat untuk korelasi Pearson tidak terpenuhi.</p>
        <ul>
          <li><b>Hipotesis:</b> Menguji apakah koefisien korelasi peringkat populasi (\\(\\rho_s\\)) sama dengan nol. Contoh: \\(H_0: \\rho_s = 0\\).</li>
          <li><b>Statistik Uji (tanpa data kembar/ties):</b>
            $$ r_s = 1 - \\frac{6 \\sum d_i^2}{n(n^2-1)} $$
            dimana \\(d_i\\) adalah selisih peringkat antara pasangan data. Jika terdapat data kembar, formula koreksi digunakan. Untuk \\(n > 30\\), digunakan pendekatan normal (uji Z).
          </li>
        </ul>
        
        <hr>

        <h4>3. Uji Korelasi Kendall Tau (Nonparametrik)</h4>
        <p>Uji ini juga mengukur keeratan hubungan antara dua variabel berskala ordinal dan sangat berguna jika terdapat banyak data kembar (ties).</p>
        <ul>
          <li><b>Hipotesis:</b> Menguji apakah koefisien korelasi populasi (\\(\\tau\\)) sama dengan nol. Contoh: \\(H_0: \\tau = 0\\).</li>
          <li><b>Statistik Uji:</b> Didasarkan pada jumlah pasangan konkordan (C) dan diskordan (D).
            $$ \\tau = \\frac{C - D}{\\frac{1}{2}n(n-1)} $$
            Untuk \\(n > 10\\), digunakan pendekatan normal (uji Z).
          </li>
        </ul>

        <hr>

        <h4>4. Uji Kebebasan (Chi-Square Test of Independence)</h4>
        <p>Uji ini digunakan untuk mengetahui adanya keterkaitan (asosiasi) antara dua variabel yang keduanya bersifat kategorik/kualitatif.</p>
        <ul>
          <li><b>Hipotesis:</b>
            <ul>
                <li>\\(H_0:\\) Kedua variabel saling bebas (independen).</li>
                <li>\\(H_1:\\) Kedua variabel tidak saling bebas (dependen/terkait).</li>
            </ul>
          </li>
          <li><b>Statistik Uji:</b>
            $$ \\chi^2_{hitung} = \\sum \\frac{(O_{ij} - e_{ij})^2}{e_{ij}} $$
            dimana \\(O_{ij}\\) adalah frekuensi observasi dan \\(e_{ij}\\) adalah frekuensi harapan.
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(\\chi^2_{hitung} > \\chi^2_{\\alpha, (r-1)(k-1)}\\).</li>
        </ul>
        
        <hr>
      '),
                       ),
                       # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 13 ---
                       argonRow(
                         center = TRUE,
                         argonColumn(
                           width = 12,
                           align = "center",
                           style = "padding: 20px;",
                           actionButton("start_quiz_p13_new", "Kerjakan Kuis Interaktif", 
                                        icon = icon("rocket"), 
                                        class = "btn-primary btn-lg")
                         )
                       ),
                       # --- KALKULATOR UJI KORELASI (DIPERBARUI) ---
                       fluidRow(
                         box(
                           title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                           
                           # Pengaturan Uji
                           selectInput("p13_test_type", "Pilih Metode Uji:",
                                       choices = list("Korelasi Pearson" = "pearson", 
                                                      "Korelasi Spearman" = "spearman", 
                                                      "Uji Kebebasan (Chi-Square)" = "chisq")
                           ),
                           hr(),
                           
                           # Input Data
                           radioButtons("p13_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File Excel" = "file")),
                           conditionalPanel(
                             condition = "input.p13_input_method == 'manual'",
                             textAreaInput("p13_manual_data_x", "Masukkan Data Variabel X (pisahkan koma):", placeholder = "Contoh: 10, 12, 15, 11"),
                             textAreaInput("p13_manual_data_y", "Masukkan Data Variabel Y (pisahkan koma):", placeholder = "Contoh: 25, 28, 30, 26")
                           ),
                           conditionalPanel(
                             condition = "input.p13_input_method == 'file'",
                             fileInput("p13_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                             argonAlert(
                               icon = icon("info-circle"), status = "info",
                               "Pastikan file Anda memiliki dua kolom yang sesuai dengan jenis uji yang dipilih.",
                               tags$li(strong("Untuk Korelasi (Pearson/Spearman):"), " Dua kolom harus berisi data numerik."),
                               HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Motivasi} & \\textbf{Kinerja} \\\\ \\hline 75 & 80 \\\\ 81 & 88 \\\\ 65 & 75 \\\\ ... & ... \\\\ \\hline \\end{array} $$'),
                               tags$li(strong("Untuk Uji Kebebasan (Chi-Square):"), " Dua kolom harus berisi data kategorik/teks."),
                               HTML('$$ \\begin{array}{|c|c|} \\hline \\textbf{Gender} & \\textbf{Pilihan} \\\\ \\hline \\text{Pria} & \\text{Partai A} \\\\ \\text{Wanita} & \\text{Partai B} \\\\ \\text{Pria} & \\text{Partai B} \\\\ ... & ... \\\\ \\hline \\end{array} $$')
                             ),
                             uiOutput("p13_sheet_selector"),
                             fluidRow(
                               column(6, uiOutput("p13_column_selector_x")),
                               column(6, uiOutput("p13_column_selector_y"))
                             )
                           ),
                           hr(),
                           
                           sliderInput("p13_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                           actionButton("p13_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
                         ),
                         
                         box(
                           title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                           uiOutput("p13_report_output_ui")
                         )
                       ),
                       fluidRow(
                         box(
                           title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                           withSpinner(DT::dataTableOutput("p13_data_table"))
                         ),
                         box(
                           title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 6,
                           withSpinner(plotlyOutput("p13_plot"))
                         )
                       )
          )
        } 
        else if (i == 14) {
          argonTabItem(
            tabName = "pertemuan_14",
            h2(daftar_judul_pertemuan[i], class = "text-center mb-4"),
            argonCard(
              width = 12,
              title = "Catatan Materi: ANCOVA",
              icon = icon("cogs"),
              status = "primary",
              shadow = TRUE,
              HTML('
        <h4 style="margin-top: 0;">Definisi dan Tujuan ANCOVA</h4>
        <p><b>Analysis of Covariance (ANCOVA)</b> adalah teknik statistik yang menggabungkan Analysis of Variance (ANOVA) dan regresi linier. Tujuannya adalah untuk menganalisis perbedaan rata-rata antar kelompok dengan terlebih dahulu mengontrol pengaruh dari variabel lain yang disebut <b>kovariat</b>. Kovariat adalah variabel kuantitatif yang dapat memengaruhi variabel dependen tetapi tidak dikontrol dalam eksperimen. Dengan mengontrol pengaruh kovariat, ANCOVA dapat meningkatkan presisi analisis dan memberikan hasil yang lebih akurat.</p>

        <hr>

        <h4>Model dan Asumsi ANCOVA</h4>
        <p>Model ANCOVA dengan satu faktor perlakuan dan satu kovariat adalah:</p>
        $$ y_{ij} = \\mu + \\tau_i + \\beta(x_{ij} - \\bar{x}_{..}) + \\epsilon_{ij} $$
        <p>Dimana \\(\\tau_i\\) adalah pengaruh perlakuan ke-i, \\(\\beta\\) adalah koefisien regresi dari kovariat \\(x\\), dan \\(x_{ij} - \\bar{x}_{..}\\) adalah nilai kovariat yang terpusat.</p>

        <h5>Asumsi-asumsi dalam ANCOVA:</h5>
        <ul>
            <li>Kovariat (X) bersifat *fixed*, diukur tanpa galat, dan independen terhadap perlakuan.</li>
            <li>Galat (\\(\\epsilon_{ij}\\)) diasumsikan menyebar normal, independen, dan identik dengan varians \\(\\sigma^2\\).</li>
            <li>Terdapat hubungan linier antara kovariat (X) dan variabel dependen (Y).</li>
        </ul>

        <hr>

        <h4>Tabel dan Perhitungan ANCOVA</h4>
        <p>Perhitungan ANCOVA didasarkan pada partisi jumlah kuadrat (Sum of Squares) dan jumlah perkalian (Sum of Cross-products).</p>
        <p><b>Tabel Komponen Dasar ANCOVA</b></p>
        $$
        \\begin{array}{|l|c|c|c|}
        \\hline
        \\textbf{Sumber} & \\textbf{Jumlah Kuadrat Y} & \\textbf{Jumlah Perkalian} & \\textbf{Jumlah Kuadrat X} \\\\
        \\hline
        \\text{Antar Kelompok} & B_{yy} & B_{xy} & B_{xx} \\\\
        \\hline
        \\text{Dalam Kelompok (Error)} & E_{yy} & E_{xy} & E_{xx} \\\\
        \\hline
        \\text{Total} & S_{yy} & S_{xy} & S_{xx} \\\\
        \\hline
        \\end{array}
        $$
        <p>Dari komponen di atas, dihitung Jumlah Kuadrat Galat (JKG) dan Jumlah Kuadrat Regresi (JKR) yang telah disesuaikan dengan pengaruh kovariat.</p>
        <ul>
            <li><b>Jumlah Kuadrat Galat (Adjusted):</b>
                $$ JKG = E_{yy} - \\frac{(E_{xy})^2}{E_{xx}} $$
            </li>
            <li><b>Jumlah Kuadrat Regresi untuk Kovariat (\\(\\beta\\)):</b>
                $$ JKR(\\beta) = \\frac{(E_{xy})^2}{E_{xx}} $$
            </li>
            <li><b>Jumlah Kuadrat Regresi untuk Perlakuan (\\(\\mu_i\\)):</b>
                $$ JKR(\\mu_i) = \\left( E_{yy} + B_{yy} - \\frac{(E_{xy} + B_{xy})^2}{E_{xx} + B_{xx}} \\right) - JKG $$
            </li>
        </ul>

        <hr>

        <h4>Pengujian Hipotesis</h4>

        <h5>1. Uji Kesamaan Rata-rata Kelompok (Setelah Disesuaikan)</h5>
        <ul>
          <li><b>Hipotesis:</b> \\(H_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k\\) (setelah disesuaikan oleh kovariat).</li>
          <li><b>Statistik Uji:</b>
            $$ F_{hitung} = \\frac{JKR(\\mu_i) / (k-1)}{JKG / (N-k-1)} $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(F_{hitung} > F_{(\\alpha, k-1, N-k-1)}\\).</li>
        </ul>

        <h5>2. Uji Signifikansi Kovariat</h5>
        <ul>
          <li><b>Hipotesis:</b> \\(H_0: \\beta = 0\\) (kovariat tidak berpengaruh signifikan).</li>
          <li><b>Statistik Uji:</b>
            $$ F_{hitung} = \\frac{JKR(\\beta) / 1}{JKG / (N-k-1)} $$
          </li>
          <li><b>Keputusan:</b> Tolak \\(H_0\\) jika \\(F_{hitung} > F_{(\\alpha, 1, N-k-1)}\\).</li>
        </ul>
      ')
            ),
            
            # --- TOMBOL KUIS BARU UNTUK PERTEMUAN 14 ---
            argonRow(
              center = TRUE,
              argonColumn(
                width = 12,
                align = "center",
                style = "padding: 20px;",
                actionButton("start_quiz_p14_new", "Kerjakan Kuis Interaktif", 
                             icon = icon("rocket"), 
                             class = "btn-primary btn-lg")
              )
            ),
            
            # --- KALKULATOR ANCOVA (DIPERBARUI) ---
            fluidRow(
              box(
                title = "Input & Pengaturan Uji", status = "primary", solidHeader = TRUE, width = 4,
                
                # Input Data
                radioButtons("p14_input_method", "Metode Input:", choices = list("Manual Input" = "manual", "Upload File Excel" = "file")),
                conditionalPanel(
                  condition = "input.p14_input_method == 'manual'",
                  textAreaInput("p14_manual_data_value", "Data Nilai (Dependen):", placeholder = "85, 78, 90, 88, 76, 84"),
                  textAreaInput("p14_manual_data_factor", "Data Faktor (Grup):", placeholder = "A, A, A, B, B, B"),
                  textAreaInput("p14_manual_data_covariate", "Data Kovariat:", placeholder = "110, 105, 112, 115, 108, 111")
                ),
                conditionalPanel(
                  condition = "input.p14_input_method == 'file'",
                  fileInput("p14_file", "Unggah File Excel:", accept = c(".xlsx", ".xls")),
                  argonAlert(
                    icon = icon("info-circle"), status = "info",
                    "Gunakan format panjang: satu kolom nilai (numerik), satu kolom faktor (kategorik), dan satu kolom kovariat (numerik).",
                    tags$li("Contoh format:",
                            HTML('$$ \\begin{array}{|c|c|c|} \\hline \\textbf{Skor_Hasil} & \\textbf{Metode} & \\textbf{Skor_Awal} \\\\ \\hline 85 & \\text{A} & 110 \\\\ 78 & \\text{A} & 105 \\\\ 90 & \\text{B} & 112 \\\\ ... & ... & ... \\\\ \\hline \\end{array} $$')
                    )
                  ),
                  uiOutput("p14_sheet_selector"),
                  uiOutput("p14_column_selector_value"),
                  uiOutput("p14_column_selector_factor"),
                  uiOutput("p14_column_selector_covariate")
                ),
                hr(),
                
                # Pengaturan Uji
                selectInput("p14_posthoc_test", "Pilih Uji Lanjut (Post-Hoc):",
                            choices = list("Tukey HSD" = "tukey", "Tidak Ada" = "none")
                ),
                sliderInput("p14_alpha", "Tingkat Signifikansi (α):", min = 0.01, max = 0.20, value = 0.05, step = 0.01),
                actionButton("p14_analyze", "Buat Laporan Analisis", class = "btn-primary", icon = icon("cogs"), width = "100%")
              ),
              
              box(
                title = "Laporan Hasil Analisis", status = "primary", solidHeader = TRUE, width = 8,
                uiOutput("p14_report_output_ui")
              )
            ),
            fluidRow(
              box(
                title = "Data yang Digunakan", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(DT::dataTableOutput("p14_data_table"))
              ),
              box(
                title = "Visualisasi Data (Interaction Plot)", status = "info", solidHeader = TRUE, width = 6,
                withSpinner(plotlyOutput("p14_plot"))
              )
            )
          )
        }
      })
    ))
  ),
  footer = argonDashFooter(
    # Penambahan 'container-fluid' untuk menjaga alignment konten
    div(
      class = "container-fluid",
      fluidRow(
        # --- Kolom 1: Hak Cipta & Info ---
        column(
          width = 5,
          h5("Dashboard Metode Statistika II", class = "footer-heading"),
          p(paste0("©", format(Sys.Date(), "%Y"), ". Seluruh hak cipta dilindungi.")),
          p("Aplikasi interaktif untuk pembelajaran Mata kuliah Metode Statistika II ")
        ),

        # --- Kolom 2: Tim Pengembang ---
        column(
          width = 3,
          h5("Tim Pengembang", class = "footer-heading"),
          tags$ul(
            tags$li("Abdul Hanif Al-Fatah"),
            tags$li("Hanif Jawahir"),
            tags$li("M. Arkan Anzuye")
          )
        ),

        # --- Kolom 3: Tautan Penting ---
        column(
          width = 4,
          h5("Sumber & Tautan", class = "footer-heading"),
          # Ganti tanda '#' dengan link yang sesuai
          tags$a(href = "https://drive.google.com/file/d/1FBBjX9SYMyyXj-fIZALB-zUMwzkBmH3o/view?usp=sharing", target = "_blank", icon("book"), " E-Book Metode Statistika II - 2KS1"),
          br(),
          tags$a(href = "#", target = "_blank", icon("github"), " Lihat Kode Sumber di GitHub")
        )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  ######### MENAMPILKAN MODAL FITUR KALKULATOR #########
  observeEvent(input$card_kalkulator_click, {
    showModal(modalDialog(
      title = "Daftar Fitur Kalkulator per Pertemuan",
      tags$div(
        style = "max-height: 400px; overflow-y: auto;", # Menambahkan scroll jika konten panjang
        tags$ul(
          tags$li(strong("Pertemuan 1:"), " Tidak ada kalkulator."),
          tags$li(strong("Pertemuan 2:"), " Estimasi Interval (Rata-rata, Varian, Proporsi) untuk 1 & 2 Populasi."),
          tags$li(strong("Pertemuan 3:"), " Uji Hipotesis (Mean, Proporsi, Varian) untuk 1 Populasi."),
          tags$li(strong("Pertemuan 4:"), " Uji Hipotesis (Beda Rata-rata, Beda Proporsi, Rasio Varian) untuk 2 Populasi."),
          tags$li(strong("Pertemuan 5:"), " Uji Normalitas (Shapiro-Wilk, Lilliefors, Jarque-Bera)."),
          tags$li(strong("Pertemuan 6:"), " Uji Kesamaan Varians (Bartlett, Levene)."),
          tags$li(strong("Pertemuan 7:"), " ANOVA Satu Arah (dengan Uji Lanjut Tukey/Duncan)."),
          tags$li(strong("Pertemuan 8:"), " ANOVA Dua Arah (dengan & tanpa interaksi, Uji Lanjut Tukey)."),
          tags$li(strong("Pertemuan 9:"), " Uji Proporsi Beberapa Populasi (Chi-Square)."),
          tags$li(strong("Pertemuan 10:"), " Uji Nonparametrik 1 Sampel (Uji Tanda, Uji Keacakan)."),
          tags$li(strong("Pertemuan 11:"), " Uji Nonparametrik 2 Sampel (Wilcoxon Signed-Rank, Mann-Whitney, Kolmogorov-Smirnov)."),
          tags$li(strong("Pertemuan 12:"), " Uji Nonparametrik >2 Sampel (Kruskal-Wallis, Friedman)."),
          tags$li(strong("Pertemuan 13:"), " Uji Korelasi (Pearson, Spearman) & Uji Kebebasan (Chi-Square)."),
          tags$li(strong("Pertemuan 14:"), " Analysis of Covariance (ANCOVA).")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Tutup")
    ))
  })
  ######### MENAMPILKAN MODAL FITUR KALKULATOR #########
  
  #################### LOGIKA KUIS INTERAKTIF PERTEMUAN 1 ####################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 1
  quiz_questions_p1 <- list(
    list(
      id = "q1",
      question = "1. Manakah pernyataan yang paling tepat mendefinisikan perbedaan antara statistika deskriptif dan inferensia?",
      choices = c("A. Deskriptif menggunakan data sampel, inferensia menggunakan data populasi.",
                  "B. Deskriptif hanya menggambarkan data, sedangkan inferensia menarik kesimpulan tentang populasi.",
                  "C. Deskriptif selalu menggunakan grafik, sedangkan inferensia selalu menggunakan angka.",
                  "D. Deskriptif untuk data kualitatif, inferensia untuk data kuantitatif."),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Seorang peneliti ingin mengetahui rata-rata pendapatan bulanan seluruh kepala keluarga di sebuah kota dengan mengambil data dari 200 kepala keluarga. Jenis statistika yang digunakan adalah...",
      choices = c("A. Statistika Inferensia", "B. Statistika Deskriptif", "C. Statistika Parametrik", "D. Analisis Univariat"),
      answer = "A"
    ),
    list(
      id = "q3",
      question = "3. Manakah di bawah ini yang merupakan contoh dari populasi terbatas (finite)?",
      choices = c("A. Jumlah bintang di galaksi Bima Sakti.", "B. Jumlah lemparan dadu yang mungkin dilakukan.", "C. Jumlah mahasiswa Politeknik Statistika STIS angkatan 65.", "D. Jumlah bakteri dalam sebuah cawan petri."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Jika seorang analis memiliki data berupa skala kepuasan dan tidak ingin membuat asumsi mengenai distribusi data, metode statistik yang paling sesuai adalah...",
      choices = c("A. Statistika Deskriptif", "B. Statistika Parametrik", "C. ANOVA", "D. Statistika Nonparametrik"),
      answer = "D"
    ),
    list(
      id = "q5",
      question = "5. Teorema Limit Pusat menyatakan bahwa untuk ukuran sampel yang besar, distribusi dari rata-rata sampel akan mendekati...",
      choices = c("A. Distribusi Normal", "B. Distribusi-t", "C. Distribusi Chi-Square", "D. Distribusi F"),
      answer = "A"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 1
  quiz_state_p1 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 1
  observeEvent(input$start_quiz_p1_new, {
    quiz_state_p1$status <- "in_progress"
    quiz_state_p1$user_answers <- NULL
    quiz_state_p1$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 1",
        uiOutput("quiz_ui_p1"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 1 (dengan perbaikan label)
  output$quiz_ui_p1 <- renderUI({
    if (quiz_state_p1$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p1, function(q) {
          div(class = "quiz-question",
              # 1. Buat label pertanyaan secara manual menggunakan div
              div(class = "control-label",
                  tags$b(q$question)
              ),
              # 2. Buat radioButtons tanpa label
              radioButtons(inputId = paste0("p1_", q$id), 
                           label = NULL, # Penting: label dikosongkan
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p1_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p1$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p1$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p1$results_df), function(i) {
              row <- quiz_state_p1$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p1", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 1
  observeEvent(input$submit_quiz_p1_final, {
    
    user_answers <- sapply(quiz_questions_p1, function(q) {
      input[[paste0("p1_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p1$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p1, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p1[[i]]$choices[which(substr(quiz_questions_p1[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p1[[i]]$answer
        quiz_questions_p1[[i]]$choices[which(substr(quiz_questions_p1[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p1, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p1$results_df <- results_df
    quiz_state_p1$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 1
  output$download_quiz_report_p1 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-1-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p1$results_df)
      
      quiz_results <- quiz_state_p1$results_df
      tempReport <- file.path(tempdir(), "report_p1.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 1'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKA KUIS INTERAKTIF PERTEMUAN 1 ####################
  
  #################### LOGIKA NAVIGASI DAFTAR ISI ####################
  # Observer untuk menangani klik pada daftar isi
  lapply(1:14, function(i) {
    observeEvent(input[[paste0("goto_pertemuan_", i)]], {
      session$sendCustomMessage(
        type = "activateTab",
        message = paste0("pertemuan_", i)
      )
    })
  })
  #################### LOGIKA NAVIGASI DAFTAR ISI ####################

  
  #################### LOGIKA UNTUK PERTEMUAN 2 (ESTIMASI) ####################
  # --- LOGIKA UNTUK MEMBACA FILE ---
  raw_data_file1 <- reactiveVal(NULL)
  raw_data_file2_combined <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    ext <- tools::file_ext(input$file1$datapath)
    df <- if (ext == "csv") read.csv(input$file1$datapath) else read_excel(input$file1$datapath)
    raw_data_file1(df)
  })
  
  observeEvent(input$file2_combined, {
    req(input$file2_combined)
    ext <- tools::file_ext(input$file2_combined$datapath)
    df <- if (ext == "csv") read.csv(input$file2_combined$datapath) else read_excel(input$file2_combined$datapath)
    raw_data_file2_combined(df)
  })
  
  # --- LOGIKA UNTUK MENDAPATKAN DATA ---
  get_data1 <- reactive({
    if (input$input_method1 == "manual") {
      req(input$manual_data1)
      as.numeric(unlist(strsplit(input$manual_data1, ",")))
    } else {
      req(raw_data_file1(), input$selected_sheet1, input$column1)
      df <- read_excel(input$file1$datapath, sheet = input$selected_sheet1)
      as.numeric(df[[input$column1]])
    }
  })
  
  get_data2a <- reactive({
    if (input$input_method2 == "manual") {
      req(input$manual_data2a)
      as.numeric(unlist(strsplit(input$manual_data2a, ",")))
    } else {
      req(raw_data_file2_combined(), input$selected_sheet2_combined, input$column2a_combined)
      df <- read_excel(input$file2_combined$datapath, sheet = input$selected_sheet2_combined)
      as.numeric(df[[input$column2a_combined]])
    }
  })
  
  get_data2b <- reactive({
    if (input$input_method2 == "manual") {
      req(input$manual_data2b)
      as.numeric(unlist(strsplit(input$manual_data2b, ",")))
    } else {
      req(raw_data_file2_combined(), input$selected_sheet2_combined, input$column2b_combined)
      df <- read_excel(input$file2_combined$datapath, sheet = input$selected_sheet2_combined)
      as.numeric(df[[input$column2b_combined]])
    }
  })
  
  # --- LOGIKA UI UNTUK MEMILIH SHEET DAN KOLOM ---
  output$sheet_selector1 <- renderUI({
    req(raw_data_file1())
    sheets <- excel_sheets(input$file1$datapath)
    selectInput("selected_sheet1", "Pilih Sheet:", choices = sheets)
  })
  
  output$column_selector1 <- renderUI({
    req(raw_data_file1(), input$selected_sheet1)
    df <- read_excel(input$file1$datapath, sheet = input$selected_sheet1)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("column1", "Pilih Kolom Data:", choices = numeric_cols)
  })
  
  output$sheet_selector2_combined <- renderUI({
    req(raw_data_file2_combined())
    sheets <- excel_sheets(input$file2_combined$datapath)
    selectInput("selected_sheet2_combined", "Pilih Sheet:", choices = sheets)
  })
  
  output$column_selector2a_combined <- renderUI({
    req(raw_data_file2_combined(), input$selected_sheet2_combined)
    df <- read_excel(input$file2_combined$datapath, sheet = input$selected_sheet2_combined)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("column2a_combined", "Pilih Kolom Populasi 1:", choices = numeric_cols, selected = numeric_cols[1])
  })
  
  output$column_selector2b_combined <- renderUI({
    req(raw_data_file2_combined(), input$selected_sheet2_combined, input$column2a_combined)
    df <- read_excel(input$file2_combined$datapath, sheet = input$selected_sheet2_combined)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    available_cols <- setdiff(numeric_cols, input$column2a_combined)
    selectInput("column2b_combined", "Pilih Kolom Populasi 2:", choices = available_cols, selected = available_cols[1])
  })
  
  # --- REKOMENDASI IMPLEMENTASI ---
  # Gunakan reactiveValues untuk menyimpan teks laporan dan objek plot
  analysis_results <- reactiveValues(
    report1 = NULL, plot1_obj = NULL,
    report2 = NULL, plot2_obj = NULL
  )
  
  # --- OBSERVER UNTUK ANALISIS 1 POPULASI (DIPERBARUI TOTAL) ---
  observeEvent(input$analyze1, {
    req(get_data1())
    x <- get_data1(); x <- x[!is.na(x)]
    n <- length(x); conf_level_num <- input$confidence_level1; alpha <- (100 - conf_level_num) / 100
    validate(need(n > 1, "Data tidak cukup (minimal 2 observasi)."))
    
    report_string <- ""
    p <- NULL # Inisialisasi objek plot
    
    if (input$param_type1 == "mean") {
      xbar <- mean(x); s <- sd(x)
      margin <- if (n >= 30) qnorm(1 - alpha / 2) * s / sqrt(n) else qt(1 - alpha / 2, df = n - 1) * s / sqrt(n)
      method_desc <- if (n >= 30) "Distribusi Normal (n ≥ 30)" else "Distribusi t-Student (n < 30)"
      ci <- c(xbar - margin, xbar + margin)
      report_string <- paste0(
        "==========================================\n   HASIL ESTIMASI RATA-RATA POPULASI\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Mengestimasi rata-rata populasi (μ) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
        "2. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Rata-rata Sampel (x̄): ", round(xbar, 4), "\n   - Standar Deviasi (s): ", round(s, 4), "\n\n",
        "3. HASIL ESTIMASI:\n   - Metode: ", method_desc, "\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n\n",
        "4. INTERPRETASI:\n   Dengan tingkat kepercayaan ", conf_level_num, "%, kita yakin bahwa rata-rata populasi yang sebenarnya berada di antara ", round(ci[1], 4), " dan ", round(ci[2], 4), "."
      )
      p <- plot_ly(x = ~x, type = "histogram") %>% layout(title = "Distribusi Data Sampel")
      
    } else if (input$param_type1 == "variance") {
      s2 <- var(x)
      ci <- c((n - 1) * s2 / qchisq(1 - alpha / 2, n - 1), (n - 1) * s2 / qchisq(alpha / 2, n - 1))
      report_string <- paste0(
        "==========================================\n   HASIL ESTIMASI VARIAN POPULASI\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Mengestimasi varian populasi (σ²) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
        "2. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Varian Sampel (s²): ", round(s2, 4), "\n\n",
        "3. HASIL ESTIMASI:\n   - Metode: Distribusi Chi-Square\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n\n",
        "4. INTERPRETASI:\n   Dengan tingkat kepercayaan ", conf_level_num, "%, kita yakin bahwa varian populasi yang sebenarnya berada di antara ", round(ci[1], 4), " dan ", round(ci[2], 4), "."
      )
      p <- plot_ly(x = ~x, type = "box") %>% layout(title = "Boxplot Data Sampel")
      
    } else if (input$param_type1 == "proportion") {
      successes <- sum(x == input$success_value1); p_hat <- successes / n
      margin <- qnorm(1 - alpha / 2) * sqrt(p_hat * (1 - p_hat) / n)
      ci <- c(p_hat - margin, p_hat + margin)
      report_string <- paste0(
        "==========================================\n   HASIL ESTIMASI PROPORSI POPULASI\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Mengestimasi proporsi populasi (p) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
        "2. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Jumlah Sukses: ", successes, "\n   - Proporsi Sampel (p̂): ", round(p_hat, 4), "\n\n",
        "3. HASIL ESTIMASI:\n   - Metode: Pendekatan Normal\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n\n",
        "4. INTERPRETASI:\n   Dengan tingkat kepercayaan ", conf_level_num, "%, kita yakin bahwa proporsi populasi yang sebenarnya berada di antara ", round(ci[1], 4), " dan ", round(ci[2], 4), "."
      )
      p <- plot_ly(x = c("Sukses", "Gagal"), y = c(successes, n-successes), type = "bar") %>% layout(title = "Frekuensi Sukses vs Gagal")
    }
    
    analysis_results$report1 <- report_string
    analysis_results$plot1_obj <- p
    
    output$report_output_ui1 <- renderUI({ tagList(verbatimTextOutput("results1"), br(), downloadButton("download_report1", "Unduh Laporan (.docx)")) })
    output$results1 <- renderText({ analysis_results$report1 })
    output$plot1 <- renderPlotly({ analysis_results$plot1_obj })
    output$data_table1 <- DT::renderDataTable({ DT::datatable(data.frame(Data = x), options = list(pageLength = 5)) })
  })
  
  # --- DOWNLOAD HANDLER 1 POPULASI (DIPERBARUI) ---
  output$download_report1 <- downloadHandler(
    filename = function() { paste0("Laporan-Estimasi-1-Populasi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results$report1)
      
      # --- PERUBAHAN DI SINI: Menggunakan ggplot2 untuk menyimpan ---
      plot_path <- file.path(tempdir(), "plot.png")
      
      # Buat plot ggplot2 berdasarkan data yang sama
      data_for_plot <- get_data1()
      
      if (input$param_type1 == "mean") {
        p_gg <- ggplot(data.frame(x=data_for_plot), aes(x=x)) + geom_histogram(aes(y=..density..), fill="skyblue", color="black") + geom_density(color="red") + labs(title="Distribusi Data Sampel")
      } else if (input$param_type1 == "variance") {
        p_gg <- ggplot(data.frame(x=data_for_plot), aes(y=x)) + geom_boxplot(fill="lightgreen") + labs(title="Boxplot Data Sampel")
      } else {
        successes <- sum(data_for_plot == input$success_value1)
        df_prop <- data.frame(Kategori=c("Sukses", "Gagal"), Jumlah=c(successes, length(data_for_plot)-successes))
        p_gg <- ggplot(df_prop, aes(x=Kategori, y=Jumlah)) + geom_bar(stat="identity", fill=c("blue", "orange")) + labs(title="Frekuensi Sukses vs Gagal")
      }
      
      ggsave(plot_path, plot = p_gg, device = "png", width = 6, height = 4)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      report_md <- gsub("=", "", analysis_results$report1)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Estimasi'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## 5. VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- OBSERVER UNTUK ANALISIS 2 POPULASI (DIPERBARUI TOTAL) ---
  observeEvent(input$analyze2, {
    req(get_data2a(), get_data2b()); x1 <- get_data2a(); x2 <- get_data2b(); x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]
    n1 <- length(x1); n2 <- length(x2); conf_level_num <- input$confidence_level2; alpha <- (100 - conf_level_num) / 100
    validate(need(n1 > 1 && n2 > 1, "Setiap populasi harus memiliki minimal 2 observasi."))
    
    report_string <- ""
    p <- NULL
    
    if (input$param_type2 == "mean") {
      if (input$sample_relation2 == "independent") {
        df_levene <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(n1, n2))))
        levene_pval <- car::leveneTest(Value ~ Group, data = df_levene)$`Pr(>F)`[1]
        var_equal <- levene_pval >= 0.05
        t_test <- t.test(x1, x2, paired = FALSE, var.equal = var_equal, conf.level = 1 - alpha)
        report_string <- paste0(
          "==================================================\n   HASIL ESTIMASI BEDA RATA-RATA (INDEPENDEN)\n==================================================\n\n",
          "1. TUJUAN ANALISIS:\n   Mengestimasi selisih rata-rata (μ₁-μ₂) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
          "2. STATISTIK DESKRIPTIF:\n   - Pop 1: n=", n1, ", x̄=", round(mean(x1), 4), ", s=", round(sd(x1), 4), "\n   - Pop 2: n=", n2, ", x̄=", round(mean(x2), 4), ", s=", round(sd(x2), 4), "\n\n",
          "3. UJI ASUMSI (Homogenitas Varians):\n   - Uji Levene p-value: ", round(levene_pval, 4), "\n   - Keputusan: Asumsi kesamaan varians ", ifelse(var_equal, "TERPENUHI.", "TIDAK TERPENUHI."), "\n\n",
          "4. HASIL ESTIMASI:\n   - Metode: ", t_test$method, "\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(t_test$conf.int[1], 4), ", ", round(t_test$conf.int[2], 4), "]\n\n",
          "5. INTERPRETASI:\n   Dengan ", conf_level_num, "% keyakinan, selisih rata-rata populasi (μ₁-μ₂) berada di antara ", round(t_test$conf.int[1], 4), " dan ", round(t_test$conf.int[2], 4), "."
        )
        p <- plot_ly(df_levene, y = ~Value, color = ~Group, type = "box") %>% layout(title = "Perbandingan Distribusi")
      } else { # Paired
        validate(need(n1 == n2, "Sampel berpasangan harus memiliki ukuran yang sama."))
        t_test <- t.test(x1, x2, paired = TRUE, conf.level = 1 - alpha)
        report_string <- paste0(
          "==================================================\n   HASIL ESTIMASI BEDA RATA-RATA (BERPASANGAN)\n==================================================\n\n",
          "1. TUJUAN ANALISIS:\n   Mengestimasi rata-rata selisih (μ_d) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
          "2. STATISTIK DESKRIPTIF:\n   - Jumlah Pasangan (n): ", n1, "\n   - Rata-rata Selisih (d̄): ", round(mean(x1-x2), 4), "\n   - Std. Deviasi Selisih (s_d): ", round(sd(x1-x2), 4), "\n\n",
          "3. HASIL ESTIMASI:\n   - Metode: ", t_test$method, "\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(t_test$conf.int[1], 4), ", ", round(t_test$conf.int[2], 4), "]\n\n",
          "4. INTERPRETASI:\n   Dengan ", conf_level_num, "% keyakinan, rata-rata selisih populasi (μ_d) berada di antara ", round(t_test$conf.int[1], 4), " dan ", round(t_test$conf.int[2], 4), "."
        )
        p <- plot_ly(x = ~(x1-x2), type = "histogram") %>% layout(title = "Distribusi Selisih Data")
      }
    } else if (input$param_type2 == "variance") {
      var_test <- var.test(x1, x2, conf.level = 1 - alpha)
      report_string <- paste0(
        "==================================================\n   HASIL ESTIMASI RASIO VARIAN\n==================================================\n\n",
        "1. TUJUAN ANALISIS:\n   Mengestimasi rasio varians (σ₁²/σ₂²) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
        "2. STATISTIK DESKRIPTIF:\n   - Pop 1: n=", n1, ", s²=", round(var(x1), 4), "\n   - Pop 2: n=", n2, ", s²=", round(var(x2), 4), "\n\n",
        "3. HASIL ESTIMASI:\n   - Metode: F-test\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(var_test$conf.int[1], 4), ", ", round(var_test$conf.int[2], 4), "]\n\n",
        "4. INTERPRETASI:\n   Dengan ", conf_level_num, "% keyakinan, rasio varians populasi (σ₁²/σ₂²) berada di antara ", round(var_test$conf.int[1], 4), " dan ", round(var_test$conf.int[2], 4), "."
      )
      df_plot <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(n1, n2))))
      p <- plot_ly(df_plot, y = ~Value, color = ~Group, type = "box") %>% layout(title = "Perbandingan Distribusi")
      
    } else if (input$param_type2 == "proportion") {
      s1 <- sum(x1 == input$success_value2a); s2 <- sum(x2 == input$success_value2b)
      prop_test <- prop.test(c(s1, s2), c(n1, n2), conf.level = 1 - alpha)
      report_string <- paste0(
        "==================================================\n   HASIL ESTIMASI BEDA PROPORSI\n==================================================\n\n",
        "1. TUJUAN ANALISIS:\n   Mengestimasi selisih proporsi (p₁-p₂) dengan tingkat kepercayaan ", conf_level_num, "%.\n\n",
        "2. STATISTIK DESKRIPTIF:\n   - Pop 1: n=", n1, ", sukses=", s1, ", p̂₁=", round(s1/n1, 4), "\n   - Pop 2: n=", n2, ", sukses=", s2, ", p̂₂=", round(s2/n2, 4), "\n\n",
        "3. HASIL ESTIMASI:\n   - Metode: Pendekatan Normal\n   - Selang Kepercayaan ", conf_level_num, "%: [", round(prop_test$conf.int[1], 4), ", ", round(prop_test$conf.int[2], 4), "]\n\n",
        "4. INTERPRETASI:\n   Dengan ", conf_level_num, "% keyakinan, selisih proporsi populasi (p₁-p₂) berada di antara ", round(prop_test$conf.int[1], 4), " dan ", round(prop_test$conf.int[2], 4), "."
      )
      df_plot <- data.frame(Group=c("Pop1", "Pop2"), Prop=c(s1/n1, s2/n2))
      p <- plot_ly(df_plot, x=~Group, y=~Prop, type="bar") %>% layout(title = "Perbandingan Proporsi")
    }
    
    analysis_results$report2 <- report_string
    analysis_results$plot2_obj <- p
    
    output$report_output_ui2 <- renderUI({ tagList(verbatimTextOutput("results2"), br(), downloadButton("download_report2", "Unduh Laporan (.docx)")) })
    output$results2 <- renderText({ analysis_results$report2 })
    output$plot2 <- renderPlotly({ analysis_results$plot2_obj })
    output$data_table2 <- DT::renderDataTable({
      max_len <- max(n1, n2); df <- data.frame(`Populasi 1` = c(x1, rep(NA, max_len - n1)), `Populasi 2` = c(x2, rep(NA, max_len - n2)))
      DT::datatable(df, options = list(pageLength = 5))
    })
  })
  
  # --- DOWNLOAD HANDLER 2 POPULASI (DIPERBARUI) ---
  output$download_report2 <- downloadHandler(
    filename = function() { paste0("Laporan-Estimasi-2-Populasi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results$report2)
      
      plot_path <- file.path(tempdir(), "plot2.png")
      # --- PERUBAHAN DI SINI: Menggunakan ggplot2 untuk menyimpan ---
      
      # Ambil data lagi untuk membuat plot ggplot2
      x1 <- get_data2a(); x2 <- get_data2b()
      
      if (input$param_type2 == "mean" && input$sample_relation2 == "independent") {
        df_plot <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(length(x1), length(x2)))))
        p_gg <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi")
      } else if (input$param_type2 == "mean" && input$sample_relation2 == "paired") {
        p_gg <- ggplot(data.frame(diff=x1-x2), aes(x=diff)) + geom_histogram(fill="skyblue", color="black") + labs(title="Distribusi Selisih Data")
      } else if (input$param_type2 == "variance") {
        df_plot <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(length(x1), length(x2)))))
        p_gg <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi")
      } else { # proportion
        s1 <- sum(x1 == input$success_value2a); s2 <- sum(x2 == input$success_value2b)
        df_plot <- data.frame(Group=c("Pop1", "Pop2"), Prop=c(s1/length(x1), s2/length(x2)))
        p_gg <- ggplot(df_plot, aes(x=Group, y=Prop, fill=Group)) + geom_bar(stat="identity") + labs(title="Perbandingan Proporsi")
      }
      
      ggsave(plot_path, plot = p_gg, device = "png", width = 6, height = 4)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      report_md <- gsub("=", "", analysis_results$report2)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Estimasi'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      
      write("\n\n## 6. VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 2 (ESTIMASI) ####################

  #################### LOGIKA UNTUK KUIS INTERAKTIF PERTEMUAN 2 ###############
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 2
  quiz_questions_p2 <- list(
    list(
      id = "q1",
      question = "1. Apa perbedaan mendasar antara estimasi titik dan estimasi interval?",
      choices = c(
        "A. Estimasi titik lebih akurat daripada estimasi interval.",
        "B. Estimasi titik memberikan satu nilai dugaan, sedangkan estimasi interval memberikan rentang nilai.",
        "C. Estimasi titik hanya untuk rata-rata, estimasi interval untuk proporsi.",
        "D. Estimasi titik menggunakan distribusi-t, estimasi interval menggunakan distribusi Z."
      ),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Seorang peneliti meningkatkan tingkat kepercayaan (confidence level) dari 95% menjadi 99% untuk estimasi interval rata-rata. Apa yang akan terjadi pada lebar selang kepercayaan?",
      choices = c(
        "A. Selang akan menjadi lebih sempit.",
        "B. Selang akan menjadi lebih lebar.",
        "C. Selang akan tetap sama.",
        "D. Tidak dapat ditentukan tanpa mengetahui ukuran sampel."
      ),
      answer = "B"
    ),
    list(
      id = "q3",
      question = "3. Dalam melakukan estimasi interval untuk rata-rata populasi, kapan distribusi-t lebih tepat digunakan daripada distribusi Z?",
      choices = c(
        "A. Ketika ukuran sampel sangat besar (n > 100).",
        "B. Ketika varians populasi (σ²) diketahui.",
        "C. Ketika varians populasi (σ²) tidak diketahui dan ukuran sampel kecil (n < 30).",
        "D. Ketika data yang diestimasi adalah data proporsi."
      ),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Distribusi statistik manakah yang digunakan untuk membangun selang kepercayaan bagi varians populasi (σ²)?",
      choices = c(
        "A. Distribusi Normal (Z)",
        "B. Distribusi t-Student (t)",
        "C. Distribusi F",
        "D. Distribusi Chi-Square (χ²)"
      ),
      answer = "D"
    ),
    list(
      id = "q5",
      question = "5. Untuk mengestimasi selisih rata-rata dua populasi independen (μ₁ - μ₂) dimana varians kedua populasi tidak diketahui namun diasumsikan sama, statistik uji yang relevan melibatkan...",
      choices = c(
        "A. Varian gabungan (pooled variance) dan distribusi-t.",
        "B. Varian gabungan (pooled variance) dan distribusi Z.",
        "C. Hanya standar deviasi dari sampel pertama.",
        "D. Rata-rata dari kedua varians sampel."
      ),
      answer = "A"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 2
  quiz_state_p2 <- reactiveValues(status = "not_started",
                                  user_answers = NULL,
                                  results_df = NULL)
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 2
  observeEvent(input$start_quiz_p2_new, {
    quiz_state_p2$status <- "in_progress"
    quiz_state_p2$user_answers <- NULL
    quiz_state_p2$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 2",
        uiOutput("quiz_ui_p2"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 2 (dengan perbaikan label)
  output$quiz_ui_p2 <- renderUI({
    if (quiz_state_p2$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p2, function(q) {
          div(class = "quiz-question",
              # 1. Buat label pertanyaan secara manual menggunakan div
              div(class = "control-label",
                  tags$b(q$question)
              ),
              # 2. Buat radioButtons tanpa label
              radioButtons(inputId = paste0("p2_", q$id), 
                           label = NULL, # Penting: label dikosongkan
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p2_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p2$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p2$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p2$results_df), function(i) {
              row <- quiz_state_p2$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p2", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 2
  observeEvent(input$submit_quiz_p2_final, {
    user_answers <- sapply(quiz_questions_p2, function(q) {
      input[[paste0("p2_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p2$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p2, function(q)
        q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p2[[i]]$choices[which(substr(quiz_questions_p2[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p2[[i]]$answer
        quiz_questions_p2[[i]]$choices[which(substr(quiz_questions_p2[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(
        user_answers == sapply(quiz_questions_p2, `[[`, "answer"),
        "Benar",
        "Salah"
      ),
      check.names = FALSE
    )
    
    quiz_state_p2$results_df <- results_df
    quiz_state_p2$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 2
  output$download_quiz_report_p2 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-2-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p2$results_df)
      
      quiz_results <- quiz_state_p2$results_df
      tempReport <- file.path(tempdir(), "report_p2.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 2'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"),
            tempReport,
            append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write(
        "knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))",
        tempReport,
        append = TRUE
      )
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...",
                       duration = 3,
                       type = "message")
      rmarkdown::render(tempReport, output_file = file, envir = render_env)
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 2 ################

  #################### LOGIKA UNTUK PERTEMUAN 3  ####################
  # ReactiveValues untuk menyimpan hasil analisis
  analysis_results_p3 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL,
    plot_type = NULL # Menyimpan jenis plot yang dibuat
  )
  
  # Logika untuk membaca file
  p3_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p3_file, {
    req(input$p3_file)
    ext <- tools::file_ext(input$p3_file$datapath)
    df <- if (ext == "csv") read.csv(input$p3_file$datapath) else read_excel(input$p3_file$datapath)
    p3_raw_data_file(df)
  })
  
  # p3_get_data tidak lagi memaksa jadi numerik
  p3_get_data <- reactive({
    if (input$p3_input_method == "manual") {
      req(input$p3_manual_data)
      trimws(unlist(strsplit(input$p3_manual_data, ",")))
    } else {
      req(p3_raw_data_file(), input$p3_selected_sheet, input$p3_column)
      df <- read_excel(input$p3_file$datapath, sheet = input$p3_selected_sheet)
      df[[input$p3_column]]
    }
  })
  
  # UI dinamis untuk memilih sheet dan kolom
  output$p3_sheet_selector <- renderUI({
    req(p3_raw_data_file())
    sheets <- excel_sheets(input$p3_file$datapath)
    selectInput("p3_selected_sheet", "Pilih Sheet:", choices = sheets)
  })
  
  output$p3_column_selector <- renderUI({
    req(p3_raw_data_file(), input$p3_selected_sheet)
    df <- read_excel(input$p3_file$datapath, sheet = input$p3_selected_sheet)
    selectInput("p3_column", "Pilih Kolom Data:", choices = names(df))
  })
  
  # --- KODE BARU UNTUK MENAMPILKAN RUMUS ---
  output$p3_prop_formula <- renderUI({
    withMathJax(
      p('$$ \\hat{p} = \\frac{x}{n} = \\frac{\\text{Jumlah kejadian sukses}}{\\text{Ukuran sampel}} $$')
    )
  })
  
  # --- KODE BARU UNTUK UI PEMILIH NILAI SUKSES ---
  output$p3_success_selector_ui <- renderUI({
    data_values <- p3_get_data()
    if (is.null(data_values) || length(data_values) == 0) {
      return(p(em("Masukkan atau unggah data untuk melihat pilihan.")))
    }
    unique_values <- unique(na.omit(data_values))
    selectInput("p3_success_value_selected", "Pilih nilai yang dianggap 'sukses' (x):",
                choices = unique_values, selected = unique_values[1])
  })
  
  
  # Observer utama untuk analisis
  observeEvent(input$p3_analyze, {
    x_raw <- p3_get_data()
    x_raw <- x_raw[!is.na(x_raw) & x_raw != ""]
    
    # --- PERUBAHAN DI SINI: Mengambil nilai H0 dari input yang benar ---
    h0_val <- if (input$p3_param_type == "proportion") {
      input$p3_h0_value_prop
    } else {
      input$p3_h0_value_mean_var
    }
    
    alpha <- input$p3_alpha; alt <- input$p3_alternative
    
    report_string <- ""
    p_gg <- NULL # Menggunakan objek ggplot2
    
    # --- UJI RATA-RATA ---
    if (input$p3_param_type == "mean") {
      x <- as.numeric(x_raw)
      validate(need(!any(is.na(x)), "Data untuk uji rata-rata harus numerik."))
      n <- length(x)
      validate(need(n > 1, "Data tidak cukup (minimal 2 observasi)."))
      
      t_test_result <- t.test(x, mu = h0_val, alternative = alt, conf.level = 1 - alpha)
      report_string <- paste0(
        "==========================================\n   HASIL UJI HIPOTESIS RATA-RATA\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah rata-rata populasi (μ) secara signifikan berbeda dari ", h0_val, ".\n\n",
        "2. RUMUSAN HIPOTESIS:\n",
        "   - H₀: μ = ", h0_val, "\n",
        "   - H₁: μ ", switch(alt, two.sided="≠", less="<", greater=">"), " ", h0_val, "\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Rata-rata Sampel (x̄): ", round(mean(x), 4), "\n   - Standar Deviasi (s): ", round(sd(x), 4), "\n\n",
        "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", t_test_result$method, "\n",
        "   - Statistik Uji (t): ", round(t_test_result$statistic, 4), "\n",
        "   - Derajat Bebas (df): ", round(t_test_result$parameter, 2), "\n",
        "   - p-value: ", format.p(t_test_result$p.value), "\n\n",
        "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
        "   - Kaidah Keputusan: Tolak H₀ jika p-value < ", alpha, ".\n",
        "   - Keputusan: Karena p-value ", ifelse(t_test_result$p.value < alpha, "<", "≥"), " ", alpha, ", maka ", ifelse(t_test_result$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
        "   - Interpretasi: ", ifelse(t_test_result$p.value < alpha, "Terdapat cukup bukti statistik untuk menyatakan bahwa rata-rata populasi secara signifikan berbeda dari ", "Tidak terdapat cukup bukti statistik untuk menyatakan bahwa rata-rata populasi berbeda dari "), h0_val, "."
      )
      p_gg <- ggplot(data.frame(x=x), aes(x=x)) + geom_histogram(aes(y=..density..), fill="skyblue", color="black", bins=15) + geom_density(color="red") + geom_vline(xintercept = h0_val, color="blue", linetype="dashed", size=1) + labs(title="Distribusi Data vs Nilai H₀")
      analysis_results_p3$plot_type <- "mean"
      analysis_results_p3$data <- x
      
      # --- UJI PROPORSI (DIPERBARUI) ---
    } else if (input$p3_param_type == "proportion") {
      req(input$p3_success_value_selected)
      x <- as.character(x_raw)
      n <- length(x)
      validate(need(n > 1, "Data tidak cukup (minimal 2 observasi)."))
      
      success_value <- input$p3_success_value_selected
      successes <- sum(x == success_value)
      p_hat <- successes / n
      p0 <- h0_val
      
      # Validasi syarat pendekatan normal
      syarat_terpenuhi <- (n * p0 >= 5) && (n * (1 - p0) >= 5)
      
      # Hitung Z-statistik
      z_stat <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
      
      # Hitung p-value berdasarkan hipotesis alternatif
      p_val <- switch(alt,
                      two.sided = 2 * pnorm(-abs(z_stat)),
                      less = pnorm(z_stat),
                      greater = pnorm(z_stat, lower.tail = FALSE))
      
      report_string <- paste0(
        "==========================================\n   HASIL UJI HIPOTESIS PROPORSI (UJI Z)\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah proporsi populasi (p) secara signifikan berbeda dari ", p0, ".\n\n",
        "2. RUMUSAN HIPOTESIS:\n",
        "   - H₀: p = ", p0, "\n",
        "   - H₁: p ", switch(alt, two.sided="≠", less="<", greater=">"), " ", p0, "\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Jumlah Sukses (nilai = '", success_value, "'): ", successes, "\n   - Proporsi Sampel (p̂): ", round(p_hat, 4), "\n\n",
        "4. UJI ASUMSI (Pendekatan Normal):\n",
        "   - np₀ = ", round(n * p0, 2), "\n",
        "   - n(1-p₀) = ", round(n * (1 - p0), 2), "\n",
        "   - Kesimpulan: Asumsi pendekatan normal ", ifelse(syarat_terpenuhi, "TERPENUHI.", "TIDAK TERPENUHI (Hasil mungkin kurang akurat)."), "\n\n",
        "5. HASIL UJI STATISTIK:\n   - Metode Uji: Uji Z untuk Satu Proporsi\n",
        "   - Statistik Uji (Z): ", round(z_stat, 4), "\n",
        "   - p-value: ", format.p(p_val), "\n\n",
        "6. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
        "   - Keputusan: ", ifelse(p_val < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
        "   - Interpretasi: ", ifelse(p_val < alpha, "Terdapat cukup bukti statistik untuk menyatakan bahwa proporsi populasi secara signifikan berbeda dari ", "Tidak terdapat cukup bukti statistik untuk menyatakan bahwa proporsi populasi berbeda dari "), p0, "."
      )
      df_prop <- data.frame(Kategori=c("Sukses", "Gagal"), Jumlah=c(successes, n-successes))
      p_gg <- ggplot(df_prop, aes(x=Kategori, y=Jumlah, fill=Kategori)) + geom_bar(stat="identity") + labs(title="Frekuensi Sukses vs Gagal")
      analysis_results_p3$plot_type <- "proportion"
      analysis_results_p3$data <- x
      
      # --- UJI VARIANS ---
    } else if (input$p3_param_type == "variance") {
      x <- as.numeric(x_raw)
      validate(need(!any(is.na(x)), "Data untuk uji varians harus numerik."))
      n <- length(x)
      validate(need(n > 1, "Data tidak cukup (minimal 2 observasi)."))
      
      s2 <- var(x); chi2_stat <- (n - 1) * s2 / h0_val
      p_val <- switch(alt,
                      two.sided = 2 * min(pchisq(chi2_stat, n - 1), 1 - pchisq(chi2_stat, n - 1)),
                      less = pchisq(chi2_stat, n - 1),
                      greater = 1 - pchisq(chi2_stat, n - 1))
      report_string <- paste0(
        "==========================================\n   HASIL UJI HIPOTESIS VARIAN\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah varian populasi (σ²) secara signifikan berbeda dari ", h0_val, ".\n\n",
        "2. RUMUSAN HIPOTESIS:\n",
        "   - H₀: σ² = ", h0_val, "\n",
        "   - H₁: σ² ", switch(alt, two.sided="≠", less="<", greater=">"), " ", h0_val, "\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Varian Sampel (s²): ", round(s2, 4), "\n\n",
        "4. HASIL UJI STATISTIK:\n   - Metode Uji: Uji Chi-Square\n",
        "   - Statistik Uji (χ²): ", round(chi2_stat, 4), "\n",
        "   - Derajat Bebas (df): ", n - 1, "\n",
        "   - p-value: ", format.p(p_val), "\n\n",
        "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
        "   - Keputusan: ", ifelse(p_val < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
        "   - Interpretasi: ", ifelse(p_val < alpha, "Terdapat cukup bukti statistik untuk menyatakan bahwa varian populasi secara signifikan berbeda dari ", "Tidak terdapat cukup bukti statistik untuk menyatakan bahwa varian populasi berbeda dari "), h0_val, "."
      )
      p_gg <- ggplot(data.frame(x=x), aes(y=x)) + geom_boxplot(fill="lightgreen") + labs(title="Boxplot Data Sampel")
      analysis_results_p3$plot_type <- "variance"
      analysis_results_p3$data <- x
    }
    
    analysis_results_p3$report <- report_string
    analysis_results_p3$plot_obj <- ggplotly(p_gg)
    
    output$p3_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p3_results"), br(), downloadButton("p3_download_report", "Unduh Laporan (.docx)")) })
    output$p3_results <- renderText({ analysis_results_p3$report })
    output$p3_plot <- renderPlotly({ analysis_results_p3$plot_obj })
    output$p3_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Data = analysis_results_p3$data), options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p3_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Hipotesis-1-Populasi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p3$report, analysis_results_p3$data)
      
      plot_path <- file.path(tempdir(), "plot_p3.png")
      
      data_for_plot <- analysis_results_p3$data
      plot_type <- analysis_results_p3$plot_type
      
      # Mengambil nilai H0 dari input yang benar saat download
      h0_val <- if (plot_type == "proportion") input$p3_h0_value_prop else input$p3_h0_value_mean_var
      
      p_gg_download <- if (plot_type == "mean") {
        ggplot(data.frame(x=as.numeric(data_for_plot)), aes(x=x)) + geom_histogram(aes(y=..density..), fill="skyblue", color="black", bins=15) + geom_density(color="red") + geom_vline(xintercept = h0_val, color="blue", linetype="dashed", size=1) + labs(title="Distribusi Data vs Nilai H₀")
      } else if (plot_type == "proportion") {
        successes <- sum(data_for_plot == input$p3_success_value_selected)
        df_prop <- data.frame(Kategori=c("Sukses", "Gagal"), Jumlah=c(successes, length(data_for_plot)-successes))
        ggplot(df_prop, aes(x=Kategori, y=Jumlah, fill=Kategori)) + geom_bar(stat="identity") + labs(title="Frekuensi Sukses vs Gagal")
      } else if (plot_type == "variance") {
        ggplot(data.frame(x=as.numeric(data_for_plot)), aes(y=x)) + geom_boxplot(fill="lightgreen") + labs(title="Boxplot Data Sampel")
      }
      
      ggsave(plot_path, plot = p_gg_download, device = "png", width = 6, height = 4)
      
      tempReport <- file.path(tempdir(), "report_p3.Rmd")
      report_md <- gsub("=", "", analysis_results_p3$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Hipotesis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## 6. VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 3  ####################

  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 3 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 3
  quiz_questions_p3 <- list(
    list(
      id = "q1",
      question = "1. Dalam pengujian hipotesis, apa yang dimaksud dengan Kesalahan Tipe I (α)?",
      choices = c("A. Menerima H₀ padahal H₀ salah.",
                  "B. Menolak H₀ padahal H₀ benar.",
                  "C. Menerima H₁ padahal H₁ salah.",
                  "D. Menolak H₁ padahal H₁ benar."),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Seorang manajer pabrik ingin menguji hipotesis bahwa rata-rata berat bersih produknya adalah 500 gram. Hipotesis nol (H₀) yang paling tepat untuk pengujian ini adalah...",
      choices = c("A. H₀: μ ≠ 500",
                  "B. H₀: μ > 500",
                  "C. H₀: μ < 500",
                  "D. H₀: μ = 500"),
      answer = "D"
    ),
    list(
      id = "q3",
      question = "3. Jika p-value dari sebuah uji hipotesis adalah 0.03 dan tingkat signifikansi (α) yang ditetapkan adalah 0.05, maka keputusan yang diambil adalah...",
      choices = c("A. Gagal tolak H₀",
                  "B. Tolak H₀",
                  "C. Terima H₁ dan H₀",
                  "D. Perlu lebih banyak data"),
      answer = "B"
    ),
    list(
      id = "q4",
      question = "4. Untuk menguji hipotesis mengenai varians satu populasi (σ²), statistik uji yang digunakan mengikuti distribusi...",
      choices = c("A. Normal (Z)",
                  "B. t-Student",
                  "C. Chi-Square (χ²)",
                  "D. F"),
      answer = "C"
    ),
    list(
      id = "q5",
      question = "5. Pengujian hipotesis yang hipotesis alternatifnya (H₁) menyatakan 'parameter tidak sama dengan nilai tertentu' (misal: H₁: μ ≠ 500) disebut...",
      choices = c("A. Uji satu arah (one-tailed test)",
                  "B. Uji dua arah (two-tailed test)",
                  "C. Uji pihak kanan (right-tailed test)",
                  "D. Uji pihak kiri (left-tailed test)"),
      answer = "B"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 3
  quiz_state_p3 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 3
  observeEvent(input$start_quiz_p3_new, {
    quiz_state_p3$status <- "in_progress"
    quiz_state_p3$user_answers <- NULL
    quiz_state_p3$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 3",
        uiOutput("quiz_ui_p3"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 3
  output$quiz_ui_p3 <- renderUI({
    if (quiz_state_p3$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p3, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p3_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p3_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p3$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p3$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p3$results_df), function(i) {
              row <- quiz_state_p3$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p3", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 3
  observeEvent(input$submit_quiz_p3_final, {
    
    user_answers <- sapply(quiz_questions_p3, function(q) {
      input[[paste0("p3_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p3$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p3, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p3[[i]]$choices[which(substr(quiz_questions_p3[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p3[[i]]$answer
        quiz_questions_p3[[i]]$choices[which(substr(quiz_questions_p3[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p3, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p3$results_df <- results_df
    quiz_state_p3$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 3
  output$download_quiz_report_p3 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-3-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p3$results_df)
      
      quiz_results <- quiz_state_p3$results_df
      tempReport <- file.path(tempdir(), "report_p3.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 3'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 3 ################
  
  #################### LOGIKA UNTUK PERTEMUAN 4  ####################
  analysis_results_p4 <- reactiveValues(
    report_mean = NULL, plot_obj_mean = NULL, data1_mean = NULL, data2_mean = NULL, plot_type_mean = NULL,
    report_var = NULL, plot_obj_var = NULL, data1_var = NULL, data2_var = NULL,
    report_prop = NULL, plot_obj_prop = NULL, data_prop = NULL
  )
  
  # --- Logika untuk Beda Rata-Rata ---
  p4_raw_data_file_mean <- reactiveVal(NULL)
  observeEvent(input$p4_file_mean, { req(input$p4_file_mean); p4_raw_data_file_mean(read_excel(input$p4_file_mean$datapath)) })
  p4_get_data_mean <- reactive({
    if (input$p4_mean_input_method == "manual") {
      req(input$p4_manual_data_a, input$p4_manual_data_b)
      list(a = as.numeric(unlist(strsplit(input$p4_manual_data_a, ","))), b = as.numeric(unlist(strsplit(input$p4_manual_data_b, ","))))
    } else {
      req(p4_raw_data_file_mean(), input$p4_selected_sheet_mean, input$p4_col_a, input$p4_col_b)
      df <- read_excel(input$p4_file_mean$datapath, sheet = input$p4_selected_sheet_mean)
      list(a = as.numeric(df[[input$p4_col_a]]), b = as.numeric(df[[input$p4_col_b]]))
    }
  })
  output$p4_sheet_selector_mean <- renderUI({ req(p4_raw_data_file_mean()); selectInput("p4_selected_sheet_mean", "Pilih Sheet:", choices = excel_sheets(input$p4_file_mean$datapath)) })
  output$p4_col_selector_a <- renderUI({ req(p4_raw_data_file_mean(), input$p4_selected_sheet_mean); df <- read_excel(input$p4_file_mean$datapath, sheet = input$p4_selected_sheet_mean); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p4_col_a", "Kolom Populasi 1:", choices = numeric_cols, selected = numeric_cols[1]) })
  output$p4_col_selector_b <- renderUI({ req(p4_raw_data_file_mean(), input$p4_selected_sheet_mean, input$p4_col_a); df <- read_excel(input$p4_file_mean$datapath, sheet = input$p4_selected_sheet_mean); numeric_cols <- names(df)[sapply(df, is.numeric)]; available_cols <- setdiff(numeric_cols, input$p4_col_a); selectInput("p4_col_b", "Kolom Populasi 2:", choices = available_cols, selected = available_cols[1]) })
  
  observeEvent(input$p4_analyze_mean, {
    data_list <- p4_get_data_mean(); x1 <- data_list$a; x2 <- data_list$b; x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]; n1 <- length(x1); n2 <- length(x2); alpha <- input$p4_alpha_mean; alt <- input$p4_alternative_mean
    validate(need(n1 > 1 && n2 > 1, "Setiap populasi harus memiliki minimal 2 observasi."))
    
    report_string <- ""; p_gg <- NULL
    
    if (input$p4_sample_type == "independent") {
      df_levene <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(n1, n2)))); levene_pval <- car::leveneTest(Value ~ Group, data = df_levene)$`Pr(>F)`[1]; var_equal <- levene_pval >= 0.05; t_test <- t.test(x1, x2, paired = FALSE, var.equal = var_equal, alternative = alt, conf.level = 1 - alpha)
      report_string <- paste0(
        "==================================================\n   HASIL UJI HIPOTESIS BEDA RATA-RATA (INDEPENDEN)\n==================================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan rata-rata yang signifikan antara dua populasi independen.\n\n",
        "2. RUMUSAN HIPOTESIS:\n   - H₀: μ₁ - μ₂ = 0\n   - H₁: μ₁ - μ₂ ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Pop 1: n=", n1, ", x̄=", round(mean(x1), 4), ", s=", round(sd(x1), 4), "\n   - Pop 2: n=", n2, ", x̄=", round(mean(x2), 4), ", s=", round(sd(x2), 4), "\n\n",
        "4. UJI ASUMSI (Homogenitas Varians):\n   - Uji Levene p-value: ", round(levene_pval, 4), "\n   - Keputusan: Asumsi kesamaan varians ", ifelse(var_equal, "TERPENUHI.", "TIDAK TERPENUHI."), "\n\n",
        "5. HASIL UJI STATISTIK:\n   - Metode Uji: ", t_test$method, "\n   - Statistik Uji (t): ", round(t_test$statistic, 4), "\n   - Derajat Bebas (df): ", round(t_test$parameter, 2), "\n   - p-value: ", format.p(t_test$p.value), "\n\n",
        "6. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(t_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(t_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan rata-rata yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan rata-rata yang signifikan.")
      )
      p_gg <- ggplot(df_levene, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi"); analysis_results_p4$plot_type_mean <- "independent"
    } else { # Paired
      validate(need(n1 == n2, "Sampel berpasangan harus memiliki ukuran yang sama.")); t_test <- t.test(x1, x2, paired = TRUE, alternative = alt, conf.level = 1 - alpha)
      report_string <- paste0(
        "==================================================\n   HASIL UJI HIPOTESIS BEDA RATA-RATA (BERPASANGAN)\n==================================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan rata-rata yang signifikan antara dua pengukuran berpasangan.\n\n",
        "2. RUMUSAN HIPOTESIS:\n   - H₀: μ_d = 0\n   - H₁: μ_d ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Jumlah Pasangan (n): ", n1, "\n   - Rata-rata Selisih (d̄): ", round(mean(x1-x2), 4), "\n   - Std. Deviasi Selisih (s_d): ", round(sd(x1-x2), 4), "\n\n",
        "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", t_test$method, "\n   - Statistik Uji (t): ", round(t_test$statistic, 4), "\n   - Derajat Bebas (df): ", round(t_test$parameter, 2), "\n   - p-value: ", format.p(t_test$p.value), "\n\n",
        "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(t_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(t_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan rata-rata yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan rata-rata yang signifikan.")
      )
      p_gg <- ggplot(data.frame(diff=x1-x2), aes(x=diff)) + geom_histogram(fill="skyblue", color="black") + labs(title="Distribusi Selisih Data"); analysis_results_p4$plot_type_mean <- "paired"
    }
    
    analysis_results_p4$report_mean <- report_string; analysis_results_p4$plot_obj_mean <- ggplotly(p_gg); analysis_results_p4$data1_mean <- x1; analysis_results_p4$data2_mean <- x2
    output$p4_report_output_ui_mean <- renderUI({ tagList(verbatimTextOutput("p4_results_mean"), br(), downloadButton("p4_download_report_mean", "Unduh Laporan (.docx)")) }); output$p4_results_mean <- renderText({ analysis_results_p4$report_mean }); output$p4_plot_mean <- renderPlotly({ analysis_results_p4$plot_obj_mean })
    output$p4_data_table_mean <- DT::renderDataTable({ max_len <- max(n1, n2); df <- data.frame(`Populasi 1` = c(x1, rep(NA, max_len - n1)), `Populasi 2` = c(x2, rep(NA, max_len - n2))); DT::datatable(df, options = list(pageLength = 5)) })
  })
  
  output$p4_download_report_mean <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Beda-Rata-Rata-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p4$report_mean); plot_path <- file.path(tempdir(), "plot_p4_mean.png"); x1 <- analysis_results_p4$data1_mean; x2 <- analysis_results_p4$data2_mean; plot_type <- analysis_results_p4$plot_type_mean
      p_gg_download <- if (plot_type == "independent") { ggplot(data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(length(x1), length(x2))))), aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi") } else { ggplot(data.frame(diff=x1-x2), aes(x=diff)) + geom_histogram(fill="skyblue", color="black") + labs(title="Distribusi Selisih Data") }
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p4$report_mean)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk Rasio Dua Varians ---
  p4_raw_data_file_var <- reactiveVal(NULL)
  observeEvent(input$p4_file_var, { req(input$p4_file_var); p4_raw_data_file_var(read_excel(input$p4_file_var$datapath)) })
  p4_get_data_var <- reactive({
    if (input$p4_var_input_method == "manual") {
      req(input$p4_manual_data_a_var, input$p4_manual_data_b_var)
      list(a = as.numeric(unlist(strsplit(input$p4_manual_data_a_var, ","))), b = as.numeric(unlist(strsplit(input$p4_manual_data_b_var, ","))))
    } else {
      req(p4_raw_data_file_var(), input$p4_selected_sheet_var, input$p4_col_a_var, input$p4_col_b_var)
      df <- read_excel(input$p4_file_var$datapath, sheet = input$p4_selected_sheet_var)
      list(a = as.numeric(df[[input$p4_col_a_var]]), b = as.numeric(df[[input$p4_col_b_var]]))
    }
  })
  output$p4_sheet_selector_var <- renderUI({ req(p4_raw_data_file_var()); selectInput("p4_selected_sheet_var", "Pilih Sheet:", choices = excel_sheets(input$p4_file_var$datapath)) })
  output$p4_col_selector_a_var <- renderUI({ req(p4_raw_data_file_var(), input$p4_selected_sheet_var); df <- read_excel(input$p4_file_var$datapath, sheet = input$p4_selected_sheet_var); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p4_col_a_var", "Kolom Populasi 1:", choices = numeric_cols, selected = numeric_cols[1]) })
  output$p4_col_selector_b_var <- renderUI({ req(p4_raw_data_file_var(), input$p4_selected_sheet_var, input$p4_col_a_var); df <- read_excel(input$p4_file_var$datapath, sheet = input$p4_selected_sheet_var); numeric_cols <- names(df)[sapply(df, is.numeric)]; available_cols <- setdiff(numeric_cols, input$p4_col_a_var); selectInput("p4_col_b_var", "Kolom Populasi 2:", choices = available_cols, selected = available_cols[1]) })
  
  observeEvent(input$p4_analyze_var, {
    data_list <- p4_get_data_var(); x1 <- data_list$a; x2 <- data_list$b; x1 <- x1[!is.na(x1)]; x2 <- x2[!is.na(x2)]; n1 <- length(x1); n2 <- length(x2); alpha <- input$p4_alpha_var; alt <- input$p4_alternative_var
    validate(need(n1 > 1 && n2 > 1, "Setiap populasi harus memiliki minimal 2 observasi."))
    
    var_test <- var.test(x1, x2, alternative = alt, conf.level = 1 - alpha)
    report_string <- paste0(
      "==================================================\n   HASIL UJI HIPOTESIS RASIO DUA VARIANS\n==================================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah rasio varians dua populasi (σ₁²/σ₂²) secara signifikan berbeda dari 1.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: σ₁²/σ₂² = 1\n   - H₁: σ₁²/σ₂² ", switch(alt, two.sided="≠", less="<", greater=">"), " 1\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Pop 1: n=", n1, ", s²=", round(var(x1), 4), "\n   - Pop 2: n=", n2, ", s²=", round(var(x2), 4), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", var_test$method, "\n   - Statistik Uji (F): ", round(var_test$statistic, 4), "\n   - Derajat Bebas (df₁, df₂): ", var_test$parameter[1], ", ", var_test$parameter[2], "\n   - p-value: ", format.p(var_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(var_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(var_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan rasio varians berbeda signifikan dari 1.", "Tidak terdapat cukup bukti untuk menyatakan rasio varians berbeda signifikan dari 1.")
    )
    df_plot <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(n1, n2))))
    p_gg <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi")
    
    analysis_results_p4$report_var <- report_string; analysis_results_p4$plot_obj_var <- ggplotly(p_gg); analysis_results_p4$data1_var <- x1; analysis_results_p4$data2_var <- x2
    output$p4_report_output_ui_var <- renderUI({ tagList(verbatimTextOutput("p4_results_var"), br(), downloadButton("p4_download_report_var", "Unduh Laporan (.docx)")) }); output$p4_results_var <- renderText({ analysis_results_p4$report_var }); output$p4_plot_var <- renderPlotly({ analysis_results_p4$plot_obj_var })
    output$p4_data_table_var <- DT::renderDataTable({ max_len <- max(n1, n2); df <- data.frame(`Populasi 1` = c(x1, rep(NA, max_len - n1)), `Populasi 2` = c(x2, rep(NA, max_len - n2))); DT::datatable(df, options = list(pageLength = 5)) })
  })
  
  output$p4_download_report_var <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Rasio-Varian-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p4$report_var); plot_path <- file.path(tempdir(), "plot_p4_var.png"); x1 <- analysis_results_p4$data1_var; x2 <- analysis_results_p4$data2_var
      df_plot <- data.frame(Value = c(x1, x2), Group = factor(rep(c("Pop1", "Pop2"), c(length(x1), length(x2)))))
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p4$report_var)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk Beda Dua Proporsi ---
  p4_raw_data_file_prop <- reactiveVal(NULL)
  observeEvent(input$p4_file_prop, { req(input$p4_file_prop); p4_raw_data_file_prop(read_excel(input$p4_file_prop$datapath)) })
  
  p4_get_data_prop <- reactive({
    if (input$p4_prop_input_method == "manual") {
      list(
        s1 = input$p4_success_a, n1 = input$p4_n_a,
        s2 = input$p4_success_b, n2 = input$p4_n_b
      )
    } else {
      req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop, input$p4_col_group_prop, input$p4_col_value_prop, input$p4_success_value_prop)
      df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop)
      
      group_col <- df[[input$p4_col_group_prop]]
      value_col <- df[[input$p4_col_value_prop]]
      success_val <- input$p4_success_value_prop
      
      groups <- unique(na.omit(group_col))
      validate(need(length(groups) == 2, "Kolom grup harus memiliki tepat dua kategori unik."))
      
      group1_data <- value_col[group_col == groups[1]]
      group2_data <- value_col[group_col == groups[2]]
      
      list(
        s1 = sum(group1_data == success_val, na.rm = TRUE), n1 = length(na.omit(group1_data)),
        s2 = sum(group2_data == success_val, na.rm = TRUE), n2 = length(na.omit(group2_data)),
        group_names = groups
      )
    }
  })
  
  output$p4_sheet_selector_prop <- renderUI({ req(p4_raw_data_file_prop()); selectInput("p4_selected_sheet_prop", "Pilih Sheet:", choices = excel_sheets(input$p4_file_prop$datapath)) })
  output$p4_col_selector_group_prop <- renderUI({ req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop); df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop); selectInput("p4_col_group_prop", "Kolom Grup:", choices = names(df)) })
  output$p4_col_selector_value_prop <- renderUI({ req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop); df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop); selectInput("p4_col_value_prop", "Kolom Hasil:", choices = names(df)) })
  
  observeEvent(input$p4_analyze_prop, {
    data_list <- p4_get_data_prop()
    s1 <- data_list$s1; n1 <- data_list$n1; s2 <- data_list$s2; n2 <- data_list$n2
    alpha <- input$p4_alpha_prop; alt <- input$p4_alternative_prop
    
    validate(need(n1 > 0 && n2 > 0, "Jumlah total (n) harus lebih dari 0."), need(s1 <= n1 && s2 <= n2, "Jumlah sukses tidak boleh melebihi jumlah total."))
    
    prop_test <- prop.test(c(s1, s2), c(n1, n2), alternative = alt, conf.level = 1 - alpha, correct = FALSE)
    
    group_names <- if(!is.null(data_list$group_names)) data_list$group_names else c("Populasi 1", "Populasi 2")
    
    report_string <- paste0(
      "==================================================\n   HASIL UJI HIPOTESIS BEDA DUA PROPORSI\n==================================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan proporsi yang signifikan antara dua populasi.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: p₁ - p₂ = 0\n   - H₁: p₁ - p₂ ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
      "3. STATISTIK DESKRIPTIF:\n",
      "   - ", group_names[1], ": n₁=", n1, ", sukses₁=", s1, ", p̂₁=", round(s1/n1, 4), "\n",
      "   - ", group_names[2], ": n₂=", n2, ", sukses₂=", s2, ", p̂₂=", round(s2/n2, 4), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", prop_test$method, "\n   - Statistik Uji (χ²): ", round(prop_test$statistic, 4), "\n   - p-value: ", format.p(prop_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(prop_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(prop_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan proporsi yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan proporsi yang signifikan.")
    )
    df_plot <- data.frame(Group=group_names, Prop=c(s1/n1, s2/n2))
    p_gg <- ggplot(df_plot, aes(x=Group, y=Prop, fill=Group)) + geom_bar(stat="identity") + labs(title="Perbandingan Proporsi", y="Proporsi") + ylim(0, 1)
    
    analysis_results_p4$report_prop <- report_string; analysis_results_p4$plot_obj_prop <- ggplotly(p_gg); analysis_results_p4$data_prop <- df_plot
    output$p4_report_output_ui_prop <- renderUI({ tagList(verbatimTextOutput("p4_results_prop"), br(), downloadButton("p4_download_report_prop", "Unduh Laporan (.docx)")) }); output$p4_results_prop <- renderText({ analysis_results_p4$report_prop }); output$p4_plot_prop <- renderPlotly({ analysis_results_p4$plot_obj_prop })
    output$p4_data_table_prop <- DT::renderDataTable({ DT::datatable(data.frame(Populasi=group_names, Sukses=c(s1,s2), Total=c(n1,n2)), options = list(dom = 't')) })
  })
  
  output$p4_download_report_prop <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Beda-Proporsi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p4$report_prop); plot_path <- file.path(tempdir(), "plot_p4_prop.png"); df_plot <- analysis_results_p4$data_prop
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Prop, fill=Group)) + geom_bar(stat="identity") + labs(title="Perbandingan Proporsi", y="Proporsi") + ylim(0, 1)
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p4$report_prop)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika BARU untuk Beda Dua Proporsi ---
  p4_raw_data_file_prop <- reactiveVal(NULL)
  observeEvent(input$p4_file_prop, { req(input$p4_file_prop); p4_raw_data_file_prop(read_excel(input$p4_file_prop$datapath)) })
  
  p4_get_data_prop <- reactive({
    if (input$p4_prop_input_method == "manual") {
      list(
        s1 = input$p4_success_a, n1 = input$p4_n_a,
        s2 = input$p4_success_b, n2 = input$p4_n_b
      )
    } else {
      req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop, input$p4_col_group_prop, input$p4_col_value_prop, input$p4_success_value_prop)
      df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop)
      
      group_col <- df[[input$p4_col_group_prop]]
      value_col <- df[[input$p4_col_value_prop]]
      success_val <- input$p4_success_value_prop
      
      groups <- unique(na.omit(group_col))
      validate(need(length(groups) == 2, "Kolom grup harus memiliki tepat dua kategori unik."))
      
      group1_data <- value_col[group_col == groups[1]]
      group2_data <- value_col[group_col == groups[2]]
      
      list(
        s1 = sum(group1_data == success_val, na.rm = TRUE), n1 = length(na.omit(group1_data)),
        s2 = sum(group2_data == success_val, na.rm = TRUE), n2 = length(na.omit(group2_data)),
        group_names = groups
      )
    }
  })
  
  output$p4_sheet_selector_prop <- renderUI({ req(p4_raw_data_file_prop()); selectInput("p4_selected_sheet_prop", "Pilih Sheet:", choices = excel_sheets(input$p4_file_prop$datapath)) })
  output$p4_col_selector_group_prop <- renderUI({ req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop); df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop); selectInput("p4_col_group_prop", "Kolom Grup:", choices = names(df)) })
  output$p4_col_selector_value_prop <- renderUI({ req(p4_raw_data_file_prop(), input$p4_selected_sheet_prop); df <- read_excel(input$p4_file_prop$datapath, sheet = input$p4_selected_sheet_prop); selectInput("p4_col_value_prop", "Kolom Hasil:", choices = names(df)) })
  
  observeEvent(input$p4_analyze_prop, {
    data_list <- p4_get_data_prop()
    s1 <- data_list$s1; n1 <- data_list$n1; s2 <- data_list$s2; n2 <- data_list$n2
    alpha <- input$p4_alpha_prop; alt <- input$p4_alternative_prop
    
    validate(need(n1 > 0 && n2 > 0, "Jumlah total (n) harus lebih dari 0."), need(s1 <= n1 && s2 <= n2, "Jumlah sukses tidak boleh melebihi jumlah total."))
    
    prop_test <- prop.test(c(s1, s2), c(n1, n2), alternative = alt, conf.level = 1 - alpha, correct = FALSE)
    
    group_names <- if(!is.null(data_list$group_names)) data_list$group_names else c("Populasi 1", "Populasi 2")
    
    report_string <- paste0(
      "==================================================\n   HASIL UJI HIPOTESIS BEDA DUA PROPORSI\n==================================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan proporsi yang signifikan antara dua populasi.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: p₁ - p₂ = 0\n   - H₁: p₁ - p₂ ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
      "3. STATISTIK DESKRIPTIF:\n",
      "   - ", group_names[1], ": n₁=", n1, ", sukses₁=", s1, ", p̂₁=", round(s1/n1, 4), "\n",
      "   - ", group_names[2], ": n₂=", n2, ", sukses₂=", s2, ", p̂₂=", round(s2/n2, 4), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", prop_test$method, "\n   - Statistik Uji (χ²): ", round(prop_test$statistic, 4), "\n   - p-value: ", format.p(prop_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(prop_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(prop_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan proporsi yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan proporsi yang signifikan.")
    )
    df_plot <- data.frame(Group=group_names, Prop=c(s1/n1, s2/n2))
    p_gg <- ggplot(df_plot, aes(x=Group, y=Prop, fill=Group)) + geom_bar(stat="identity") + labs(title="Perbandingan Proporsi", y="Proporsi") + ylim(0, 1)
    
    analysis_results_p4$report_prop <- report_string; analysis_results_p4$plot_obj_prop <- ggplotly(p_gg); analysis_results_p4$data_prop <- df_plot
    output$p4_report_output_ui_prop <- renderUI({ tagList(verbatimTextOutput("p4_results_prop"), br(), downloadButton("p4_download_report_prop", "Unduh Laporan (.docx)")) }); output$p4_results_prop <- renderText({ analysis_results_p4$report_prop }); output$p4_plot_prop <- renderPlotly({ analysis_results_p4$plot_obj_prop })
    output$p4_data_table_prop <- DT::renderDataTable({ DT::datatable(data.frame(Populasi=group_names, Sukses=c(s1,s2), Total=c(n1,n2)), options = list(dom = 't')) })
  })
  
  output$p4_download_report_prop <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Beda-Proporsi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p4$report_prop); plot_path <- file.path(tempdir(), "plot_p4_prop.png"); df_plot <- analysis_results_p4$data_prop
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Prop, fill=Group)) + geom_bar(stat="identity") + labs(title="Perbandingan Proporsi", y="Proporsi") + ylim(0, 1)
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p4$report_prop)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  #################### LOGIKA UNTUK PERTEMUAN 4  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 4 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 4
  quiz_questions_p4 <- list(
    list(
      id = "q1",
      question = "1. Seorang peneliti ingin membandingkan efektivitas dua metode diet yang berbeda pada dua kelompok orang yang berbeda. Jenis uji yang paling sesuai untuk membandingkan rata-rata penurunan berat badan adalah...",
      choices = c("A. Uji-t sampel berpasangan (Paired t-test)",
                  "B. Uji-t sampel independen (Independent t-test)",
                  "C. ANOVA",
                  "D. Uji Chi-Square"),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Hipotesis nol (H₀) untuk uji rasio dua varians populasi (Uji F) adalah...",
      choices = c("A. H₀: μ₁ = μ₂",
                  "B. H₀: p₁ = p₂",
                  "C. H₀: σ₁²/σ₂² = 1",
                  "D. H₀: σ₁² ≠ σ₂²"),
      answer = "C"
    ),
    list(
      id = "q3",
      question = "3. Dalam uji beda dua rata-rata untuk sampel independen, kapan kita menggunakan 'pooled variance' (varians gabungan)?",
      choices = c("A. Ketika varians kedua populasi diketahui.",
                  "B. Ketika ukuran sampel sangat besar.",
                  "C. Ketika varians kedua populasi tidak diketahui tetapi diasumsikan sama.",
                  "D. Ketika data berpasangan."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Seorang peneliti mengukur tingkat stres karyawan sebelum dan sesudah program relaksasi. Untuk menguji apakah ada perbedaan rata-rata tingkat stres, pendekatan yang paling tepat adalah...",
      choices = c("A. Menggunakan uji-t untuk dua sampel independen.",
                  "B. Menggunakan uji-t untuk data berpasangan.",
                  "C. Menggunakan uji F untuk rasio varians.",
                  "D. Menggunakan uji Z untuk beda proporsi."),
      answer = "B"
    ),
    list(
      id = "q5",
      question = "5. Untuk menguji hipotesis mengenai perbedaan proporsi keberhasilan antara dua kelompok (misal: proporsi kesembuhan antara kelompok obat A dan kelompok plasebo), statistik uji yang digunakan umumnya mengikuti distribusi...",
      choices = c("A. t-Student",
                  "B. Chi-Square",
                  "C. Normal (Z)",
                  "D. F"),
      answer = "C"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 4
  quiz_state_p4 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 4
  observeEvent(input$start_quiz_p4_new, {
    quiz_state_p4$status <- "in_progress"
    quiz_state_p4$user_answers <- NULL
    quiz_state_p4$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 4",
        uiOutput("quiz_ui_p4"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 4
  output$quiz_ui_p4 <- renderUI({
    if (quiz_state_p4$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p4, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p4_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p4_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p4$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p4$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p4$results_df), function(i) {
              row <- quiz_state_p4$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p4", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 4
  observeEvent(input$submit_quiz_p4_final, {
    
    user_answers <- sapply(quiz_questions_p4, function(q) {
      input[[paste0("p4_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p4$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p4, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p4[[i]]$choices[which(substr(quiz_questions_p4[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p4[[i]]$answer
        quiz_questions_p4[[i]]$choices[which(substr(quiz_questions_p4[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p4, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p4$results_df <- results_df
    quiz_state_p4$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 4
  output$download_quiz_report_p4 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-4-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p4$results_df)
      
      quiz_results <- quiz_state_p4$results_df
      tempReport <- file.path(tempdir(), "report_p4.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 4'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 4 ################

  #################### LOGIKA UNTUK PERTEMUAN 5  ####################
  analysis_results_p5 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # Logika untuk membaca file
  p5_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p5_file, {
    req(input$p5_file)
    file_path <- input$p5_file$datapath
    ext <- tools::file_ext(input$p5_file$name)
    df <- switch(ext,
                 csv = read.csv(file_path),
                 xlsx = read_excel(file_path),
                 xls = read_excel(file_path),
                 sav = read.spss(file_path, to.data.frame = TRUE),
                 stop("Unsupported file type")
    )
    p5_raw_data_file(df)
  })
  
  # Logika untuk mendapatkan data
  p5_get_data <- reactive({
    if (input$p5_input_method == "manual") {
      req(input$p5_manual_data)
      as.numeric(unlist(strsplit(input$p5_manual_data, ",")))
    } else {
      req(p5_raw_data_file(), input$p5_selected_sheet, input$p5_column)
      df <- if(tools::file_ext(input$p5_file$name) %in% c("xlsx", "xls")) {
        read_excel(input$p5_file$datapath, sheet = input$p5_selected_sheet)
      } else {
        p5_raw_data_file()
      }
      as.numeric(df[[input$p5_column]])
    }
  })
  
  # UI dinamis untuk memilih sheet dan kolom
  output$p5_sheet_selector <- renderUI({
    req(p5_raw_data_file())
    if(tools::file_ext(input$p5_file$name) %in% c("xlsx", "xls")) {
      selectInput("p5_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p5_file$datapath))
    }
  })
  
  output$p5_column_selector <- renderUI({
    req(p5_raw_data_file())
    df <- if(tools::file_ext(input$p5_file$name) %in% c("xlsx", "xls")) {
      req(input$p5_selected_sheet)
      read_excel(input$p5_file$datapath, sheet = input$p5_selected_sheet)
    } else {
      p5_raw_data_file()
    }
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p5_column", "Pilih Kolom Data:", choices = numeric_cols)
  })
  
  # Observer utama untuk analisis
  observeEvent(input$p5_analyze, {
    req(p5_get_data(), input$p5_selected_tests)
    x <- p5_get_data(); x <- x[!is.na(x)]
    n <- length(x); alpha <- input$p5_alpha
    validate(need(n > 3, "Uji normalitas memerlukan lebih dari 3 observasi."))
    
    # Header Laporan
    report_string <- paste0(
      "==========================================\n   HASIL UJI KESESUAIAN SEBARAN NORMAL\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah data sampel berasal dari populasi yang berdistribusi normal.\n\n",
      "2. RUMUSAN HIPOTESIS (untuk semua uji):\n   - H₀: Data berdistribusi normal.\n   - H₁: Data tidak berdistribusi normal.\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Rata-rata (x̄): ", round(mean(x), 4), "\n   - Standar Deviasi (s): ", round(sd(x), 4), "\n\n",
      "4. HASIL UJI STATISTIK (α = ", alpha, "):\n"
    )
    
    # Lakukan uji yang dipilih
    tests_to_run <- input$p5_selected_tests
    
    if ("shapiro" %in% tests_to_run) {
      shapiro_test <- shapiro.test(x)
      report_string <- paste0(report_string,
                              "   ----------------------------------------\n   Uji Shapiro-Wilk:\n",
                              "   - Statistik Uji (W): ", round(shapiro_test$statistic, 4), "\n",
                              "   - p-value: ", format.p(shapiro_test$p.value), "\n",
                              "   - Keputusan: ", ifelse(shapiro_test$p.value < alpha, "TOLAK H₀ (Data tidak normal).", "GAGAL TOLAK H₀ (Data normal)."), "\n"
      )
    }
    if ("lilliefors" %in% tests_to_run) {
      lillie_test <- nortest::lillie.test(x)
      report_string <- paste0(report_string,
                              "   ----------------------------------------\n   Uji Lilliefors (Kolmogorov-Smirnov):\n",
                              "   - Statistik Uji (D): ", round(lillie_test$statistic, 4), "\n",
                              "   - p-value: ", format.p(lillie_test$p.value), "\n",
                              "   - Keputusan: ", ifelse(lillie_test$p.value < alpha, "TOLAK H₀ (Data tidak normal).", "GAGAL TOLAK H₀ (Data normal)."), "\n"
      )
    }
    if ("jarque" %in% tests_to_run) {
      # --- PERBAIKAN DI SINI: Memanggil dari paket tseries ---
      jarque_test <- tseries::jarque.bera.test(x)
      report_string <- paste0(report_string,
                              "   ----------------------------------------\n   Uji Jarque-Bera:\n",
                              "   - Statistik Uji (χ²): ", round(jarque_test$statistic, 4), "\n",
                              "   - p-value: ", format.p(jarque_test$p.value), "\n",
                              "   - Keputusan: ", ifelse(jarque_test$p.value < alpha, "TOLAK H₀ (Data tidak normal).", "GAGAL TOLAK H₀ (Data normal)."), "\n"
      )
    }
    
    # Plotting
    p_hist <- ggplot(data.frame(x=x), aes(x=x)) + geom_histogram(aes(y=..density..), fill="skyblue", color="black", bins=15) + geom_density(color="red") + labs(title="Histogram dan Kurva Densitas")
    p_qq <- ggplot(data.frame(x=x), aes(sample=x)) + stat_qq() + stat_qq_line(color="blue") + labs(title="Q-Q Plot")
    
    analysis_results_p5$report <- report_string
    analysis_results_p5$plot_obj <- subplot(ggplotly(p_hist), ggplotly(p_qq), nrows = 1)
    analysis_results_p5$data <- x
    
    output$p5_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p5_results"), br(), downloadButton("p5_download_report", "Unduh Laporan (.docx)")) })
    output$p5_results <- renderText({ analysis_results_p5$report })
    output$p5_plot <- renderPlotly({ analysis_results_p5$plot_obj })
    output$p5_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Data = analysis_results_p5$data), options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p5_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Normalitas-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p5$report)
      
      plot_path_hist <- file.path(tempdir(), "plot_p5_hist.png")
      plot_path_qq <- file.path(tempdir(), "plot_p5_qq.png")
      
      data_for_plot <- analysis_results_p5$data
      
      p_hist_gg <- ggplot(data.frame(x=data_for_plot), aes(x=x)) + geom_histogram(aes(y=..density..), fill="skyblue", color="black", bins=15) + geom_density(color="red") + labs(title="Histogram dan Kurva Densitas")
      p_qq_gg <- ggplot(data.frame(x=data_for_plot), aes(sample=x)) + stat_qq() + stat_qq_line(color="blue") + labs(title="Q-Q Plot")
      
      ggsave(plot_path_hist, plot = p_hist_gg, device = "png")
      ggsave(plot_path_qq, plot = p_qq_gg, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p5.Rmd")
      report_md <- gsub("=", "", analysis_results_p5$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Normalitas'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## 5. VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path_hist, ")"), tempReport, append = TRUE)
      write("\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path_qq, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 5  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 5 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 5
  quiz_questions_p5 <- list(
    list(
      id = "q1",
      question = "1. Apa tujuan utama dari melakukan uji normalitas pada sebuah set data?",
      choices = c("A. Untuk mengetahui rata-rata dari data.",
                  "B. Untuk memeriksa apakah data cocok digunakan untuk uji statistik parametrik yang berasumsi normalitas.",
                  "C. Untuk menghitung standar deviasi dari data.",
                  "D. Untuk menentukan apakah data memiliki pencilan (outlier)."),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Uji normalitas manakah yang secara spesifik merupakan modifikasi dari uji Kolmogorov-Smirnov untuk kasus di mana parameter mean dan varians populasi tidak diketahui?",
      choices = c("A. Uji Shapiro-Wilk",
                  "B. Uji Jarque-Bera",
                  "C. Uji Anderson-Darling",
                  "D. Uji Lilliefors"),
      answer = "D"
    ),
    list(
      id = "q3",
      question = "3. Bagaimana bunyi hipotesis nol (H₀) pada sebuah uji normalitas?",
      choices = c("A. H₀: Data tidak berdistribusi normal.",
                  "B. H₀: Rata-rata data sama dengan nol.",
                  "C. H₀: Data berasal dari populasi yang berdistribusi normal.",
                  "D. H₀: Varians data adalah satu."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Uji normalitas yang mengevaluasi kemiringan (skewness) dan keruncingan (kurtosis) dari distribusi data adalah...",
      choices = c("A. Uji Chi-Square Goodness of Fit",
                  "B. Uji Shapiro-Wilk",
                  "C. Uji Jarque-Bera",
                  "D. Uji Kolmogorov-Smirnov"),
      answer = "C"
    ),
    list(
      id = "q5",
      question = "5. Selain uji statistik formal, metode grafis apa yang paling umum digunakan untuk memeriksa asumsi normalitas secara visual?",
      choices = c("A. Box Plot",
                  "B. Scatter Plot",
                  "C. Pie Chart",
                  "D. Q-Q Plot (Quantile-Quantile Plot)"),
      answer = "D"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 5
  quiz_state_p5 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 5
  observeEvent(input$start_quiz_p5_new, {
    quiz_state_p5$status <- "in_progress"
    quiz_state_p5$user_answers <- NULL
    quiz_state_p5$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 5",
        uiOutput("quiz_ui_p5"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 5
  output$quiz_ui_p5 <- renderUI({
    if (quiz_state_p5$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p5, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p5_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p5_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p5$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p5$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p5$results_df), function(i) {
              row <- quiz_state_p5$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p5", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 5
  observeEvent(input$submit_quiz_p5_final, {
    
    user_answers <- sapply(quiz_questions_p5, function(q) {
      input[[paste0("p5_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p5$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p5, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p5[[i]]$choices[which(substr(quiz_questions_p5[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p5[[i]]$answer
        quiz_questions_p5[[i]]$choices[which(substr(quiz_questions_p5[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p5, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p5$results_df <- results_df
    quiz_state_p5$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 5
  output$download_quiz_report_p5 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-5-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p5$results_df)
      
      quiz_results <- quiz_state_p5$results_df
      tempReport <- file.path(tempdir(), "report_p5.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 5'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 5 ################

  #################### LOGIKA. UNTUK PERTEMUAN 6  ####################
  analysis_results_p6 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # Logika untuk membaca file
  p6_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p6_file, {
    req(input$p6_file)
    p6_raw_data_file(read_excel(input$p6_file$datapath))
  })
  
  # Logika untuk mendapatkan data
  p6_get_data <- reactive({
    if (input$p6_input_method == "manual") {
      req(input$p6_manual_data_group, input$p6_manual_data_value)
      groups <- trimws(unlist(strsplit(input$p6_manual_data_group, ",")))
      values <- as.numeric(unlist(strsplit(input$p6_manual_data_value, ",")))
      validate(need(length(groups) == length(values), "Jumlah elemen grup dan nilai harus sama."))
      data.frame(Group = as.factor(groups), Value = values)
    } else {
      req(p6_raw_data_file(), input$p6_selected_sheet, input$p6_col_group, input$p6_col_value)
      df <- read_excel(input$p6_file$datapath, sheet = input$p6_selected_sheet)
      data.frame(
        Group = as.factor(df[[input$p6_col_group]]),
        Value = as.numeric(df[[input$p6_col_value]])
      )
    }
  })
  
  # UI dinamis untuk memilih sheet dan kolom
  output$p6_sheet_selector <- renderUI({
    req(p6_raw_data_file())
    selectInput("p6_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p6_file$datapath))
  })
  output$p6_column_selector_group <- renderUI({
    req(p6_raw_data_file(), input$p6_selected_sheet)
    df <- read_excel(input$p6_file$datapath, sheet = input$p6_selected_sheet)
    selectInput("p6_col_group", "Kolom Grup (Kategorik):", choices = names(df))
  })
  output$p6_column_selector_value <- renderUI({
    req(p6_raw_data_file(), input$p6_selected_sheet)
    df <- read_excel(input$p6_file$datapath, sheet = input$p6_selected_sheet)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p6_col_value", "Kolom Nilai (Numerik):", choices = numeric_cols)
  })
  
  # Observer utama untuk analisis
  observeEvent(input$p6_analyze, {
    df <- p6_get_data()
    df <- na.omit(df)
    alpha <- input$p6_alpha
    
    validate(need(nrow(df) > 0, "Tidak ada data yang valid."),
             need(nlevels(df$Group) >= 2, "Data harus memiliki setidaknya 2 kelompok."))
    
    # Statistik Deskriptif per Grup
    desc_stats <- df %>%
      group_by(Group) %>%
      summarise(
        n = n(),
        Mean = round(mean(Value), 4),
        SD = round(sd(Value), 4),
        Variance = round(var(Value), 4),
        .groups = "drop"
      )
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI KESAMAAN VARIANS\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah varians dari beberapa kelompok sama (homogen).\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Varians semua kelompok adalah sama (σ₁² = σ₂² = ... = σₖ²).\n   - H₁: Setidaknya ada satu varians kelompok yang berbeda.\n\n",
      "3. STATISTIK DESKRIPTIF:\n"
    )
    
    # Tambahkan tabel deskriptif ke laporan
    report_string <- paste0(report_string, paste(capture.output(print(desc_stats)), collapse = "\n"), "\n\n")
    
    # Lakukan uji yang dipilih
    test_result_string <- ""
    if (input$p6_test_type == "levene") {
      test_res <- car::leveneTest(Value ~ Group, data = df)
      p_val <- test_res$`Pr(>F)`[1]
      test_result_string <- paste0(
        "4. HASIL UJI STATISTIK:\n   - Metode Uji: Uji Levene\n",
        "   - Statistik Uji (F): ", round(test_res$`F value`[1], 4), "\n",
        "   - Derajat Bebas (df₁, df₂): ", test_res$Df[1], ", ", test_res$Df[2], "\n",
        "   - p-value: ", format.p(p_val), "\n"
      )
    } else { # Bartlett
      test_res <- bartlett.test(Value ~ Group, data = df)
      p_val <- test_res$p.value
      test_result_string <- paste0(
        "4. HASIL UJI STATISTIK:\n   - Metode Uji: Uji Bartlett\n",
        "   - Asumsi: Data per kelompok harus berdistribusi normal.\n",
        "   - Statistik Uji (χ²): ", round(test_res$statistic, 4), "\n",
        "   - Derajat Bebas (df): ", test_res$parameter, "\n",
        "   - p-value: ", format.p(p_val), "\n"
      )
    }
    
    # Gabungkan hasil uji dan interpretasi
    report_string <- paste0(report_string, test_result_string,
                            "\n5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
                            "   - Keputusan: ", ifelse(p_val < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
                            "   - Interpretasi: ", ifelse(p_val < alpha, "Terdapat cukup bukti statistik untuk menyatakan bahwa varians antar kelompok TIDAK SAMA (heterogen).", "Tidak terdapat cukup bukti statistik untuk menyatakan bahwa varians antar kelompok berbeda. Asumsi homogenitas varians TERPENUHI.")
    )
    
    # Plotting
    p_gg <- ggplot(df, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
    
    analysis_results_p6$report <- report_string
    analysis_results_p6$plot_obj <- ggplotly(p_gg)
    analysis_results_p6$data <- df
    
    output$p6_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p6_results"), br(), downloadButton("p6_download_report", "Unduh Laporan (.docx)")) })
    output$p6_results <- renderText({ analysis_results_p6$report })
    output$p6_plot <- renderPlotly({ analysis_results_p6$plot_obj })
    output$p6_data_table <- DT::renderDataTable({ DT::datatable(analysis_results_p6$data, options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p6_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Kesamaan-Varians-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p6$report)
      
      plot_path <- file.path(tempdir(), "plot_p6.png")
      df_plot <- analysis_results_p6$data
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p6.Rmd")
      report_md <- gsub("=", "", analysis_results_p6$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Kesamaan Varians'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## 6. VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 6  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 6 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 6
  quiz_questions_p6 <- list(
    list(
      id = "q1",
      question = "1. Apa tujuan utama dari melakukan uji kesamaan varians seperti Uji Bartlett atau Uji Levene?",
      choices = c("A. Untuk menguji apakah rata-rata dari beberapa kelompok sama.",
                  "B. Untuk menguji apakah varians dari beberapa kelompok sama (homogenitas varians).",
                  "C. Untuk menguji apakah data mengikuti sebaran normal.",
                  "D. Untuk menguji hubungan antara dua variabel."),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Manakah dari pernyataan berikut yang merupakan asumsi penting agar Uji Bartlett dapat diandalkan?",
      choices = c("A. Data harus berasal dari populasi yang berdistribusi normal.",
                  "B. Ukuran sampel untuk setiap kelompok harus sama persis.",
                  "C. Data minimal harus berskala ordinal.",
                  "D. Rata-rata dari semua kelompok harus berbeda secara signifikan."),
      answer = "A"
    ),
    list(
      id = "q3",
      question = "3. Jika asumsi normalitas pada data Anda tidak terpenuhi, uji manakah yang menjadi alternatif yang lebih kuat (robust) dibandingkan Uji Bartlett untuk memeriksa kesamaan varians?",
      choices = c("A. ANOVA",
                  "B. Uji-t",
                  "C. Uji Levene",
                  "D. Uji Shapiro-Wilk"),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Dalam konteks prasyarat untuk melakukan ANOVA, pengujian homogenitas varians memiliki hipotesis nol (H₀) yang berbunyi...",
      choices = c("A. H₀: Rata-rata semua kelompok adalah sama.",
                  "B. H₀: Varians semua kelompok adalah sama.",
                  "C. H₀: Data berdistribusi normal.",
                  "D. H₀: Tidak ada efek interaksi antar faktor."),
      answer = "B"
    ),
    list(
      id = "q5",
      question = "5. Jika hasil dari Uji Levene memberikan nilai p-value sebesar 0.35 dan Anda menggunakan taraf signifikansi α = 0.05, apa kesimpulan Anda?",
      choices = c("A. Tolak H₀; varians antar kelompok berbeda secara signifikan.",
                  "B. Gagal tolak H₀; tidak cukup bukti untuk menyatakan varians antar kelompok berbeda.",
                  "C. Tolak H₀; rata-rata antar kelompok berbeda secara signifikan.",
                  "D. Gagal tolak H₀; data tidak berdistribusi normal."),
      answer = "B"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 6
  quiz_state_p6 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 6
  observeEvent(input$start_quiz_p6_new, {
    quiz_state_p6$status <- "in_progress"
    quiz_state_p6$user_answers <- NULL
    quiz_state_p6$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 6",
        uiOutput("quiz_ui_p6"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 6
  output$quiz_ui_p6 <- renderUI({
    if (quiz_state_p6$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p6, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p6_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p6_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p6$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p6$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p6$results_df), function(i) {
              row <- quiz_state_p6$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p6", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 6
  observeEvent(input$submit_quiz_p6_final, {
    
    user_answers <- sapply(quiz_questions_p6, function(q) {
      input[[paste0("p6_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p6$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p6, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p6[[i]]$choices[which(substr(quiz_questions_p6[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p6[[i]]$answer
        quiz_questions_p6[[i]]$choices[which(substr(quiz_questions_p6[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p6, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p6$results_df <- results_df
    quiz_state_p6$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 6
  output$download_quiz_report_p6 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-6-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p6$results_df)
      
      quiz_results <- quiz_state_p6$results_df
      tempReport <- file.path(tempdir(), "report_p6.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 6'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 6 ################

  #################### LOGIKA UNTUK PERTEMUAN 7 (ANOVA) ##################
  analysis_results_p7 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # Logika untuk membaca file
  p7_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p7_file, {
    req(input$p7_file)
    p7_raw_data_file(read_excel(input$p7_file$datapath))
  })
  
  # Logika untuk mendapatkan data
  p7_get_data <- reactive({
    if (input$p7_input_method == "manual") {
      req(input$p7_manual_data_group, input$p7_manual_data_value)
      groups <- trimws(unlist(strsplit(input$p7_manual_data_group, ",")))
      values <- as.numeric(unlist(strsplit(input$p7_manual_data_value, ",")))
      validate(need(length(groups) == length(values), "Jumlah elemen grup dan nilai harus sama."))
      data.frame(Group = as.factor(groups), Value = values)
    } else {
      req(p7_raw_data_file(), input$p7_selected_sheet, input$p7_col_group, input$p7_col_value)
      df <- read_excel(input$p7_file$datapath, sheet = input$p7_selected_sheet)
      data.frame(
        Group = as.factor(df[[input$p7_col_group]]),
        Value = as.numeric(df[[input$p7_col_value]])
      )
    }
  })
  
  # UI dinamis untuk memilih sheet dan kolom
  output$p7_sheet_selector <- renderUI({
    req(p7_raw_data_file())
    selectInput("p7_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p7_file$datapath))
  })
  output$p7_column_selector_group <- renderUI({
    req(p7_raw_data_file(), input$p7_selected_sheet)
    df <- read_excel(input$p7_file$datapath, sheet = input$p7_selected_sheet)
    selectInput("p7_col_group", "Kolom Grup (Kategorik):", choices = names(df))
  })
  output$p7_column_selector_value <- renderUI({
    req(p7_raw_data_file(), input$p7_selected_sheet)
    df <- read_excel(input$p7_file$datapath, sheet = input$p7_selected_sheet)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p7_col_value", "Kolom Nilai (Numerik):", choices = numeric_cols)
  })
  
  # Observer utama untuk analisis
  observeEvent(input$p7_analyze, {
    df <- p7_get_data()
    df <- na.omit(df)
    alpha <- input$p7_alpha
    
    validate(need(nrow(df) > 0, "Tidak ada data yang valid."),
             need(nlevels(df$Group) >= 2, "Data harus memiliki setidaknya 2 kelompok."))
    
    # --- 1. Uji Asumsi ---
    # Normalitas (Shapiro-Wilk per grup)
    normality_results <- df %>% group_by(Group) %>% summarise(p_value = shapiro.test(Value)$p.value)
    all_normal <- all(normality_results$p_value >= 0.05)
    
    # --- PERBAIKAN DI SINI ---
    # Format p-value secara manual untuk menghindari error di fungsi format.p
    formatted_p_values_normality <- sapply(normality_results$p_value, function(p) {
      if (is.na(p)) "N/A" else if (p < 0.001) "< 0.001" else as.character(round(p, 3))
    })
    
    # Homogenitas Varians (Levene)
    levene_test <- car::leveneTest(Value ~ Group, data = df)
    levene_pval <- levene_test$`Pr(>F)`[1]
    is_homogen <- levene_pval >= 0.05
    
    # --- 2. Statistik Deskriptif ---
    desc_stats <- df %>% group_by(Group) %>% summarise(n=n(), Mean=round(mean(Value),4), SD=round(sd(Value),4), .groups="drop")
    
    # --- 3. Uji Utama (ANOVA) ---
    aov_model <- aov(Value ~ Group, data = df)
    aov_summary <- summary(aov_model)
    aov_pval <- aov_summary[[1]][["Pr(>F)"]][1]
    
    # --- 4. Bangun String Laporan ---
    report_string <- paste0(
      "==========================================\n   HASIL ANALISIS VARIAN SATU ARAH (ANOVA)\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan rata-rata yang signifikan antara ", nlevels(df$Group), " kelompok.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Rata-rata semua kelompok adalah sama (μ₁ = μ₂ = ... = μₖ).\n   - H₁: Setidaknya ada satu rata-rata kelompok yang berbeda.\n\n",
      "3. STATISTIK DESKRIPTIF:\n", paste(capture.output(print(desc_stats)), collapse="\n"), "\n\n",
      "4. UJI ASUMSI:\n",
      "   a) Uji Normalitas (Shapiro-Wilk per grup):\n", paste("      - Kelompok '", normality_results$Group, "': p-value = ", formatted_p_values_normality, ifelse(normality_results$p_value >= 0.05, " (Normal)", " (Tidak Normal)"), collapse="\n"), "\n",
      "      - Kesimpulan: Asumsi normalitas ", ifelse(all_normal, "TERPENUHI.", "TIDAK TERPENUHI. Pertimbangkan uji nonparametrik (Kruskal-Wallis) jika pelanggaran signifikan."), "\n\n",
      "   b) Uji Homogenitas Varians (Levene):\n",
      "      - p-value: ", format.p(levene_pval), "\n",
      "      - Kesimpulan: Asumsi homogenitas varians ", ifelse(is_homogen, "TERPENUHI.", "TIDAK TERPENUHI."), "\n\n",
      "5. HASIL UJI ANOVA:\n", paste(capture.output(print(aov_summary)), collapse="\n"), "\n",
      "6. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
      "   - Keputusan: ", ifelse(aov_pval < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
      "   - Interpretasi: ", ifelse(aov_pval < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan rata-rata yang signifikan antara setidaknya satu pasang kelompok.", "Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan antar kelompok.")
    )
    
    # --- 5. Uji Lanjut (Post-Hoc) ---
    if (aov_pval < alpha && input$p7_posthoc_test != "none") {
      report_string <- paste0(report_string, "\n\n7. HASIL UJI LANJUT (POST-HOC):\n")
      if (input$p7_posthoc_test == "tukey") {
        posthoc_res <- TukeyHSD(aov_model)
        report_string <- paste0(report_string, "   - Metode: Tukey HSD\n", paste(capture.output(print(posthoc_res)), collapse="\n"))
      } else if (input$p7_posthoc_test == "duncan") {
        duncan_res <- agricolae::duncan.test(aov_model, "Group", alpha = alpha, console = FALSE)
        report_string <- paste0(report_string, "   - Metode: Duncan's Multiple Range Test\n", paste(capture.output(print(duncan_res$groups)), collapse="\n"))
      }
    }
    
    # Plotting
    p_gg <- ggplot(df, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
    
    analysis_results_p7$report <- report_string
    analysis_results_p7$plot_obj <- ggplotly(p_gg)
    analysis_results_p7$data <- df
    
    output$p7_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p7_results"), br(), downloadButton("p7_download_report", "Unduh Laporan (.docx)")) })
    output$p7_results <- renderText({ analysis_results_p7$report })
    output$p7_plot <- renderPlotly({ analysis_results_p7$plot_obj })
    output$p7_data_table <- DT::renderDataTable({ DT::datatable(analysis_results_p7$data, options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p7_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-ANOVA-Satu-Arah-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p7$report)
      
      plot_path <- file.path(tempdir(), "plot_p7.png")
      df_plot <- analysis_results_p7$data
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p7.Rmd")
      report_md <- gsub("=", "", analysis_results_p7$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis ANOVA Satu Arah'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  #################### LOGIKA UNTUK PERTEMUAN 7  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 7 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 7
  quiz_questions_p7 <- list(
    list(
      id = "q1",
      question = "1. Apa tujuan utama dari Analysis of Variance (ANOVA)?",
      choices = c("A. Untuk menguji hubungan antara dua variabel kuantitatif.",
                  "B. Untuk menguji perbedaan rata-rata antara dua kelompok saja.",
                  "C. Untuk menguji perbedaan rata-rata antara tiga atau lebih kelompok.",
                  "D. Untuk menguji kesamaan varians antara beberapa kelompok."),
      answer = "C"
    ),
    list(
      id = "q2",
      question = "2. Dalam ANOVA satu arah, hipotesis nol (H₀) yang diuji adalah...",
      choices = c("A. H₀: Semua varians kelompok adalah sama.",
                  "B. H₀: Semua rata-rata kelompok adalah sama (μ₁ = μ₂ = ... = μₖ).",
                  "C. H₀: Setidaknya ada satu rata-rata kelompok yang berbeda.",
                  "D. H₀: Data berdistribusi normal."),
      answer = "B"
    ),
    list(
      id = "q3",
      question = "3. Jika hasil uji ANOVA menunjukkan nilai p-value yang signifikan (misalnya, p < 0.05), apa langkah selanjutnya yang paling logis untuk dilakukan?",
      choices = c("A. Menyimpulkan bahwa semua kelompok berbeda satu sama lain.",
                  "B. Melakukan uji normalitas pada setiap kelompok.",
                  "C. Melakukan uji lanjut (post-hoc test) seperti Uji Tukey untuk mengetahui pasangan kelompok mana yang berbeda.",
                  "D. Mengulang eksperimen dengan sampel yang lebih besar."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Manakah dari berikut ini yang BUKAN merupakan asumsi dari ANOVA?",
      choices = c("A. Sampel diambil secara acak dan independen.",
                  "B. Setiap populasi berdistribusi normal.",
                  "C. Ukuran sampel untuk setiap kelompok harus sama persis.",
                  "D. Varians dari setiap populasi adalah sama (homogen)."),
      answer = "C"
    ),
    list(
      id = "q5",
      question = "5. Dalam tabel ANOVA, nilai F-hitung didapatkan dari...",
      choices = c("A. Pembagian Jumlah Kuadrat Antar Kelompok (SSB) dengan Jumlah Kuadrat Dalam Kelompok (SSW).",
                  "B. Pembagian Kuadrat Tengah Antar Kelompok (MSB) dengan Kuadrat Tengah Dalam Kelompok (MSW).",
                  "C. Pembagian Derajat Bebas Antar Kelompok dengan Derajat Bebas Dalam Kelompok.",
                  "D. Pembagian Jumlah Kuadrat Total (SST) dengan Jumlah Kuadrat Galat (SSE)."),
      answer = "B"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 7
  quiz_state_p7 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 7
  observeEvent(input$start_quiz_p7_new, {
    quiz_state_p7$status <- "in_progress"
    quiz_state_p7$user_answers <- NULL
    quiz_state_p7$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 7",
        uiOutput("quiz_ui_p7"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 7
  output$quiz_ui_p7 <- renderUI({
    if (quiz_state_p7$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p7, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p7_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p7_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p7$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p7$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p7$results_df), function(i) {
              row <- quiz_state_p7$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p7", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 7
  observeEvent(input$submit_quiz_p7_final, {
    
    user_answers <- sapply(quiz_questions_p7, function(q) {
      input[[paste0("p7_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p7$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p7, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p7[[i]]$choices[which(substr(quiz_questions_p7[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p7[[i]]$answer
        quiz_questions_p7[[i]]$choices[which(substr(quiz_questions_p7[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p7, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p7$results_df <- results_df
    quiz_state_p7$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 7
  output$download_quiz_report_p7 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-7-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p7$results_df)
      
      quiz_results <- quiz_state_p7$results_df
      tempReport <- file.path(tempdir(), "report_p7.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 7'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 7 ################

  
  #################### LOGIKA UNTUK PERTEMUAN 8  ####################
  analysis_results_p8 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # Logika untuk membaca file
  p8_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p8_file, {
    req(input$p8_file)
    p8_raw_data_file(read_excel(input$p8_file$datapath, col_names = TRUE)) # Memaksa membaca baris pertama sebagai header
  })
  
  # Logika untuk mendapatkan data
  p8_get_data <- reactive({
    if (input$p8_input_method == "manual") {
      req(input$p8_manual_data_factor1, input$p8_manual_data_factor2, input$p8_manual_data_value)
      f1 <- trimws(unlist(strsplit(input$p8_manual_data_factor1, ",")))
      f2 <- trimws(unlist(strsplit(input$p8_manual_data_factor2, ",")))
      vals <- as.numeric(unlist(strsplit(input$p8_manual_data_value, ",")))
      validate(need(length(f1) == length(f2) && length(f1) == length(vals), "Jumlah elemen di semua input manual harus sama."))
      data.frame(Factor1 = as.factor(f1), Factor2 = as.factor(f2), Value = vals)
    } else {
      req(p8_raw_data_file(), input$p8_selected_sheet, input$p8_col_value, input$p8_col_factor1, input$p8_col_factor2)
      df <- read_excel(input$p8_file$datapath, sheet = input$p8_selected_sheet, col_names = TRUE)
      data.frame(
        Value = as.numeric(df[[input$p8_col_value]]),
        Factor1 = as.factor(df[[input$p8_col_factor1]]),
        Factor2 = as.factor(df[[input$p8_col_factor2]])
      )
    }
  })
  
  # UI dinamis untuk memilih sheet dan kolom
  output$p8_sheet_selector <- renderUI({ req(p8_raw_data_file()); selectInput("p8_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p8_file$datapath)) })
  
  output$p8_column_selector_value <- renderUI({ 
    req(p8_raw_data_file(), input$p8_selected_sheet)
    df <- read_excel(input$p8_file$datapath, sheet = input$p8_selected_sheet)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p8_col_value", "Kolom Nilai (Numerik):", choices = numeric_cols) 
  })
  
  output$p8_column_selector_factor1 <- renderUI({ 
    req(p8_raw_data_file(), input$p8_selected_sheet)
    df <- read_excel(input$p8_file$datapath, sheet = input$p8_selected_sheet)
    all_cols <- names(df)
    selectInput("p8_col_factor1", "Kolom Faktor 1:", choices = all_cols, selected = all_cols[2])
  })
  
  output$p8_column_selector_factor2 <- renderUI({ 
    req(p8_raw_data_file(), input$p8_selected_sheet)
    df <- read_excel(input$p8_file$datapath, sheet = input$p8_selected_sheet)
    all_cols <- names(df)
    selectInput("p8_col_factor2", "Kolom Faktor 2:", choices = all_cols, selected = all_cols[3])
  })
  
  # Observer utama untuk analisis
  observeEvent(input$p8_analyze, {
    df <- p8_get_data()
    df <- na.omit(df)
    alpha <- input$p8_alpha
    
    validate(need(nrow(df) > 0, "Tidak ada data yang valid."),
             need(nlevels(df$Factor1) >= 2 && nlevels(df$Factor2) >= 2, "Setiap faktor harus memiliki setidaknya 2 level/kelompok."))
    
    formula <- if (input$p8_include_interaction) { Value ~ Factor1 * Factor2 } else { Value ~ Factor1 + Factor2 }
    aov_model <- aov(formula, data = df)
    
    shapiro_test_residuals <- shapiro.test(residuals(aov_model))
    levene_test <- car::leveneTest(Value ~ Factor1 * Factor2, data = df)
    
    desc_stats <- df %>% group_by(Factor1, Factor2) %>% summarise(n=n(), Mean=round(mean(Value),4), SD=round(sd(Value),4), .groups="drop")
    
    aov_summary <- car::Anova(aov_model, type = "III")
    
    report_string <- paste0(
      "==========================================\n   HASIL ANALISIS VARIAN DUA ARAH (ANOVA)\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji pengaruh dua variabel faktor (dan interaksinya) terhadap sebuah variabel dependen.\n\n",
      "2. RUMUSAN HIPOTESIS:\n",
      "   a) Efek Interaksi:\n      - H₀: Tidak ada efek interaksi antara Faktor 1 dan Faktor 2.\n      - H₁: Terdapat efek interaksi.\n",
      "   b) Efek Utama Faktor 1:\n      - H₀: Tidak ada perbedaan rata-rata antar level Faktor 1.\n      - H₁: Setidaknya ada satu perbedaan rata-rata.\n",
      "   c) Efek Utama Faktor 2:\n      - H₀: Tidak ada perbedaan rata-rata antar level Faktor 2.\n      - H₁: Setidaknya ada satu perbedaan rata-rata.\n\n",
      "3. STATISTIK DESKRIPTIF:\n", paste(capture.output(print(desc_stats)), collapse="\n"), "\n\n",
      "4. UJI ASUMSI:\n",
      "   a) Uji Normalitas Residual (Shapiro-Wilk):\n      - p-value: ", format.p(shapiro_test_residuals$p.value), ifelse(shapiro_test_residuals$p.value >= 0.05, " (Asumsi Terpenuhi)", " (Asumsi Dilanggar)"), "\n",
      "   b) Uji Homogenitas Varians (Levene):\n      - p-value: ", format.p(levene_test$`Pr(>F)`[1]), ifelse(levene_test$`Pr(>F)`[1] >= 0.05, " (Asumsi Homogenitas Terpenuhi)", " (Asumsi Homogenitas Tidak Terpenuhi)"), "\n\n",
      "5. HASIL UJI ANOVA (Tipe III SS):\n", paste(capture.output(print(aov_summary)), collapse="\n"), "\n"
    )
    
    # --- PERBAIKAN UTAMA DI SINI ---
    # Mengambil p-value dengan cara yang lebih aman
    p_interaction <- if(input$p8_include_interaction) aov_summary["Factor1:Factor2", "Pr(>F)"] else NA
    p_f1 <- aov_summary["Factor1", "Pr(>F)"]
    p_f2 <- aov_summary["Factor2", "Pr(>F)"]
    
    interpretation <- "6. KEPUTUSAN & INTERPRETASI (α = "
    interpretation <- paste0(interpretation, alpha, "):\n")
    
    if (input$p8_include_interaction && !is.na(p_interaction)) {
      interpretation <- paste0(interpretation, "   - Efek Interaksi: ", ifelse(p_interaction < alpha, "SIGNIFIKAN (Tolak H₀).", "TIDAK SIGNIFIKAN (Gagal Tolak H₀)."), "\n")
      if (p_interaction < alpha) {
        interpretation <- paste0(interpretation, "     Interpretasi: Pengaruh satu faktor bergantung pada level faktor lainnya. Efek utama harus diinterpretasikan dengan hati-hati.\n")
      }
    }
    interpretation <- paste0(interpretation, "   - Efek Utama Faktor 1: ", ifelse(p_f1 < alpha, "SIGNIFIKAN (Tolak H₀).", "TIDAK SIGNIFIKAN (Gagal Tolak H₀)."), "\n")
    interpretation <- paste0(interpretation, "   - Efek Utama Faktor 2: ", ifelse(p_f2 < alpha, "SIGNIFIKAN (Tolak H₀).", "TIDAK SIGNIFIKAN (Gagal Tolak H₀)."), "\n")
    # --- AKHIR PERBAIKAN ---
    
    report_string <- paste0(report_string, interpretation)
    
    # Uji Lanjut
    # Menggunakan semua p-value yang relevan untuk memeriksa signifikansi
    all_p_values <- c(p_f1, p_f2, p_interaction)
    if (any(na.omit(all_p_values) < alpha) && input$p8_posthoc_test == "tukey") {
      posthoc_res <- TukeyHSD(aov_model)
      report_string <- paste0(report_string, "\n\n7. HASIL UJI LANJUT (POST-HOC) TUKEY HSD:\n", paste(capture.output(print(posthoc_res)), collapse="\n"))
    }
    
    # Plotting
    p_gg <- ggplot(df, aes(x = Factor1, y = Value, color = Factor2, group = Factor2)) + 
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line") +
      labs(title = "Interaction Plot", x = "Faktor 1", y = "Rata-rata Nilai", color = "Faktor 2")
    
    analysis_results_p8$report <- report_string
    analysis_results_p8$plot_obj <- ggplotly(p_gg)
    analysis_results_p8$data <- df
    
    output$p8_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p8_results"), br(), downloadButton("p8_download_report", "Unduh Laporan (.docx)")) })
    output$p8_results <- renderText({ analysis_results_p8$report })
    output$p8_plot <- renderPlotly({ analysis_results_p8$plot_obj })
    output$p8_data_table <- DT::renderDataTable({ DT::datatable(analysis_results_p8$data, options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p8_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-ANOVA-Dua-Arah-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p8$report)
      
      plot_path <- file.path(tempdir(), "plot_p8.png")
      df_plot <- analysis_results_p8$data
      p_gg_download <- ggplot(df_plot, aes(x = Factor1, y = Value, color = Factor2, group = Factor2)) + 
        stat_summary(fun = mean, geom = "point", size = 3) +
        stat_summary(fun = mean, geom = "line") +
        labs(title = "Interaction Plot", x = "Faktor 1", y = "Rata-rata Nilai", color = "Faktor 2")
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p8.Rmd")
      report_md <- gsub("=", "", analysis_results_p8$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis ANOVA Dua Arah'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 8  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 8 ################
  quiz_questions_p8 <- list(
    list(
      id = "q1",
      question = "1. Apa perbedaan utama antara ANOVA Satu Arah dan ANOVA Dua Arah?",
      choices = c("A. ANOVA Satu Arah untuk 3 kelompok, ANOVA Dua Arah untuk 4 kelompok.",
                  "B. ANOVA Satu Arah menguji satu variabel faktor, sedangkan ANOVA Dua Arah menguji pengaruh dua variabel faktor secara bersamaan.",
                  "C. ANOVA Satu Arah menggunakan uji-F, sedangkan ANOVA Dua Arah menggunakan uji-t.",
                  "D. ANOVA Satu Arah tidak memiliki asumsi, sedangkan ANOVA Dua Arah memiliki asumsi normalitas."),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Dalam ANOVA Dua Arah, apa yang dimaksud dengan 'efek interaksi'?",
      choices = c("A. Pengaruh gabungan dari semua variabel independen terhadap variabel dependen.",
                  "B. Situasi di mana pengaruh satu variabel faktor terhadap variabel dependen bergantung pada level dari variabel faktor lainnya.",
                  "C. Kesalahan acak yang terjadi dalam model.",
                  "D. Perbedaan rata-rata antara level-level dalam satu faktor."),
      answer = "B"
    ),
    list(
      id = "q3",
      question = "3. Jika hasil uji ANOVA Dua Arah menunjukkan efek interaksi yang signifikan secara statistik, bagaimana seharusnya Anda menginterpretasikan efek utama (main effects)?",
      choices = c("A. Efek utama harus diabaikan dan fokus pada interpretasi efek interaksi.",
                  "B. Efek utama dapat diinterpretasikan secara langsung tanpa perubahan.",
                  "C. Hanya efek utama yang lebih besar yang boleh diinterpretasikan.",
                  "D. Lakukan uji-t untuk setiap efek utama."),
      answer = "A"
    ),
    list(
      id = "q4",
      question = "4. ANOVA Dua Arah 'tanpa replikasi' berarti...",
      choices = c("A. Model tersebut tidak dapat diandalkan.",
                  "B. Hanya ada satu pengamatan untuk setiap kombinasi level dari kedua faktor.",
                  "C. Efek interaksi tidak dapat dihitung.",
                  "D. Jawaban B dan C benar."),
      answer = "D"
    ),
    list(
      id = "q5",
      question = "5. Anda ingin menguji pengaruh jenis pupuk (A, B) dan tingkat penyiraman (Rendah, Tinggi) terhadap tinggi tanaman. Ini adalah contoh dari desain...",
      choices = c("A. ANOVA Satu Arah",
                  "B. Regresi Linier Sederhana",
                  "C. ANOVA Dua Arah",
                  "D. Uji Korelasi"),
      answer = "C"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 8
  quiz_state_p8 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 8
  observeEvent(input$start_quiz_p8_new, {
    quiz_state_p8$status <- "in_progress"
    quiz_state_p8$user_answers <- NULL
    quiz_state_p8$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 8",
        uiOutput("quiz_ui_p8"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 8
  output$quiz_ui_p8 <- renderUI({
    if (quiz_state_p8$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p8, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p8_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p8_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p8$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p8$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p8$results_df), function(i) {
              row <- quiz_state_p8$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p8", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 8
  observeEvent(input$submit_quiz_p8_final, {
    
    user_answers <- sapply(quiz_questions_p8, function(q) {
      input[[paste0("p8_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p8$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p8, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p8[[i]]$choices[which(substr(quiz_questions_p8[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p8[[i]]$answer
        quiz_questions_p8[[i]]$choices[which(substr(quiz_questions_p8[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p8, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p8$results_df <- results_df
    quiz_state_p8$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 8
  output$download_quiz_report_p8 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-8-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p8$results_df)
      
      quiz_results <- quiz_state_p8$results_df
      tempReport <- file.path(tempdir(), "report_p8.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 8'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 8 ################

  #################### LOGIKA UNTUK PERTEMUAN 9  ####################
  analysis_results_p9 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # --- Logika Input & UI ---
  output$p9_manual_table <- renderRHandsontable({
    df <- data.frame(
      Kategori = c("Baris 1", "Baris 2"),
      Kelompok_A = c(10, 20),
      Kelompok_B = c(15, 25)
    )
    rhandsontable(df, rowHeaders = NULL, width = "100%", height = 150)
  })
  
  p9_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p9_file, {
    req(input$p9_file)
    p9_raw_data_file(read_excel(input$p9_file$datapath))
  })
  
  output$p9_sheet_selector <- renderUI({
    req(p9_raw_data_file())
    selectInput("p9_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p9_file$datapath))
  })
  
  output$p9_column_selectors <- renderUI({
    req(p9_raw_data_file(), input$p9_selected_sheet)
    df <- read_excel(input$p9_file$datapath, sheet = input$p9_selected_sheet)
    if (input$p9_file_format == "raw") {
      tagList(
        selectInput("p9_col_row", "Kolom untuk Baris Tabel:", choices = names(df)),
        selectInput("p9_col_col", "Kolom untuk Kolom Tabel:", choices = names(df))
      )
    } else { # contingency
      tagList(
        selectInput("p9_col_cat", "Kolom Kategori Baris:", choices = names(df)),
        checkboxGroupInput("p9_cols_freq", "Pilih Kolom Frekuensi:", choices = names(df)[sapply(df, is.numeric)], selected = names(df)[sapply(df, is.numeric)])
      )
    }
  })
  
  # --- Logika Mendapatkan Data Tabel Kontingensi ---
  p9_get_contingency_table <- reactive({
    if (input$p9_input_method == "manual") {
      req(input$p9_manual_table)
      df <- hot_to_r(input$p9_manual_table)
      # Konversi ke tabel kontingensi
      mat <- as.matrix(df[,-1])
      rownames(mat) <- df[,1]
      as.table(mat)
    } else { # File upload
      req(p9_raw_data_file(), input$p9_selected_sheet)
      df <- read_excel(input$p9_file$datapath, sheet = input$p9_selected_sheet)
      
      if (input$p9_file_format == "raw") {
        req(input$p9_col_row, input$p9_col_col)
        table(df[[input$p9_col_row]], df[[input$p9_col_col]])
      } else { # contingency
        req(input$p9_col_cat, input$p9_cols_freq)
        mat <- as.matrix(df[, input$p9_cols_freq])
        rownames(mat) <- df[[input$p9_col_cat]]
        as.table(mat)
      }
    }
  })
  
  # --- Observer Utama untuk Analisis ---
  observeEvent(input$p9_analyze, {
    tbl <- p9_get_contingency_table()
    alpha <- input$p9_alpha
    
    validate(need(is.table(tbl) || is.matrix(tbl), "Format data tidak valid."),
             need(all(dim(tbl) >= 2), "Tabel kontingensi harus memiliki minimal 2 baris dan 2 kolom."))
    
    # Uji Chi-Square
    chi_test <- chisq.test(tbl)
    
    # Bangun String Laporan
    report_string <- paste0(
      "==========================================\n   HASIL UJI CHI-SQUARE\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah ada hubungan (asosiasi) antara variabel baris dan kolom, atau menguji apakah proporsi antar kelompok sama (homogen).\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Tidak ada hubungan antara variabel baris dan kolom (independen/homogen).\n   - H₁: Terdapat hubungan antara variabel baris dan kolom (dependen/tidak homogen).\n\n",
      "3. TABEL FREKUENSI OBSERVASI:\n", paste(capture.output(print(tbl)), collapse="\n"), "\n\n",
      "4. TABEL FREKUENSI HARAPAN:\n", paste(capture.output(print(round(chi_test$expected, 2))), collapse="\n"), "\n\n",
      "5. HASIL UJI STATISTIK:\n   - Metode Uji: Pearson's Chi-squared test\n",
      "   - Statistik Uji (χ²): ", round(chi_test$statistic, 4), "\n",
      "   - Derajat Bebas (df): ", chi_test$parameter, "\n",
      "   - p-value: ", format.p(chi_test$p.value), "\n\n",
      "6. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n",
      "   - Keputusan: ", ifelse(chi_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n",
      "   - Interpretasi: ", ifelse(chi_test$p.value < alpha, "Terdapat cukup bukti statistik untuk menyatakan adanya hubungan/asosiasi yang signifikan antara variabel baris dan kolom.", "Tidak terdapat cukup bukti statistik untuk menyatakan adanya hubungan/asosiasi yang signifikan antara variabel baris dan kolom.")
    )
    
    # Plotting
    df_plot <- as.data.frame(tbl)
    names(df_plot) <- c("Baris", "Kolom", "Frekuensi")
    p_gg <- ggplot(df_plot, aes(x = Kolom, y = Frekuensi, fill = Baris)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Perbandingan Frekuensi Antar Kelompok", x = names(dimnames(tbl))[2], y = "Frekuensi")
    
    analysis_results_p9$report <- report_string
    analysis_results_p9$plot_obj <- ggplotly(p_gg)
    analysis_results_p9$data <- tbl
    
    output$p9_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p9_results"), br(), downloadButton("p9_download_report", "Unduh Laporan (.docx)")) })
    output$p9_results <- renderText({ analysis_results_p9$report })
    output$p9_plot <- renderPlotly({ analysis_results_p9$plot_obj })
    output$p9_data_table <- DT::renderDataTable({ DT::datatable(as.data.frame.matrix(analysis_results_p9$data), options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p9_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Chi-Square-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p9$report)
      
      plot_path <- file.path(tempdir(), "plot_p9.png")
      df_plot <- as.data.frame(analysis_results_p9$data)
      names(df_plot) <- c("Baris", "Kolom", "Frekuensi")
      p_gg_download <- ggplot(df_plot, aes(x = Kolom, y = Frekuensi, fill = Baris)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Perbandingan Frekuensi Antar Kelompok", x = names(dimnames(analysis_results_p9$data))[2], y = "Frekuensi")
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p9.Rmd")
      report_md <- gsub("=", "", analysis_results_p9$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Chi-Square'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 9  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 9 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 9
  quiz_questions_p9 <- list(
    list(
      id = "q1",
      question = "1. Uji Chi-Square untuk beberapa proporsi digunakan untuk menguji...",
      choices = c("A. Perbedaan rata-rata antara beberapa populasi.",
                  "B. Hubungan antara dua variabel numerik.",
                  "C. Apakah proporsi suatu karakteristik sama di beberapa populasi (homogenitas).",
                  "D. Apakah data berdistribusi normal."),
      answer = "C"
    ),
    list(
      id = "q2",
      question = "2. Data untuk uji Chi-Square beberapa proporsi sering kali disajikan dalam bentuk...",
      choices = c("A. Scatter plot",
                  "B. Tabel kontingensi (Contingency Table)",
                  "C. Box plot",
                  "D. Histogram"),
      answer = "B"
    ),
    list(
      id = "q3",
      question = "3. Hipotesis nol (H₀) untuk uji homogenitas proporsi adalah...",
      choices = c("A. H₀: p₁ ≠ p₂ ≠ ... ≠ pₖ",
                  "B. H₀: Setidaknya ada satu proporsi yang berbeda.",
                  "C. H₀: p₁ = p₂ = ... = pₖ",
                  "D. H₀: Semua frekuensi observasi sama dengan frekuensi harapan."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Bagaimana cara menghitung frekuensi harapan (expected frequency) untuk sebuah sel dalam tabel kontingensi?",
      choices = c("A. (Total Baris x Total Kolom) / Total Keseluruhan",
                  "B. Total Baris / Total Kolom",
                  "C. Total Kolom / Total Keseluruhan",
                  "D. Frekuensi Observasi / Total Keseluruhan"),
      answer = "A"
    ),
    list(
      id = "q5",
      question = "5. Jika nilai statistik Chi-Square hitung lebih besar dari nilai Chi-Square kritis (atau p-value < α), apa kesimpulan yang dapat diambil?",
      choices = c("A. Gagal tolak H₀, proporsi di semua populasi adalah sama.",
                  "B. Tolak H₀, terdapat cukup bukti bahwa proporsi di semua populasi tidak sama (tidak homogen).",
                  "C. Gagal tolak H₀, tidak ada hubungan antar variabel.",
                  "D. Tolak H₀, rata-rata antar populasi berbeda."),
      answer = "B"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 9
  quiz_state_p9 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 9
  observeEvent(input$start_quiz_p9_new, {
    quiz_state_p9$status <- "in_progress"
    quiz_state_p9$user_answers <- NULL
    quiz_state_p9$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 9",
        uiOutput("quiz_ui_p9"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 9
  output$quiz_ui_p9 <- renderUI({
    if (quiz_state_p9$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p9, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p9_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p9_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p9$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p9$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p9$results_df), function(i) {
              row <- quiz_state_p9$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p9", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 9
  observeEvent(input$submit_quiz_p9_final, {
    
    user_answers <- sapply(quiz_questions_p9, function(q) {
      input[[paste0("p9_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p9$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p9, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p9[[i]]$choices[which(substr(quiz_questions_p9[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p9[[i]]$answer
        quiz_questions_p9[[i]]$choices[which(substr(quiz_questions_p9[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p9, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p9$results_df <- results_df
    quiz_state_p9$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 9
  output$download_quiz_report_p9 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-9-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p9$results_df)
      
      quiz_results <- quiz_state_p9$results_df
      tempReport <- file.path(tempdir(), "report_p9.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 9'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 9 ################

  #################### LOGIKA UNTUK PERTEMUAN 10 (UJI NONPARAMETRIK 1 SAMPEL) ##################
  analysis_results_p10 <- reactiveValues(
    report_st = NULL, plot_obj_st = NULL, data_st = NULL,
    report_pst = NULL, plot_obj_pst = NULL, data_pst_a = NULL, data_pst_b = NULL,
    report_rt = NULL, plot_obj_rt = NULL, data_rt = NULL
  )
  
  # --- Logika untuk UJI TANDA (SIGN TEST) ---
  p10_st_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p10_st_file, { req(input$p10_st_file); p10_st_raw_data_file(read_excel(input$p10_st_file$datapath)) })
  p10_st_get_data <- reactive({
    if (input$p10_st_input_method == "manual") {
      req(input$p10_st_manual_data); as.numeric(unlist(strsplit(input$p10_st_manual_data, ",")))
    } else {
      req(p10_st_raw_data_file(), input$p10_st_selected_sheet, input$p10_st_column)
      df <- read_excel(input$p10_st_file$datapath, sheet = input$p10_st_selected_sheet)
      as.numeric(df[[input$p10_st_column]])
    }
  })
  output$p10_st_sheet_selector <- renderUI({ req(p10_st_raw_data_file()); selectInput("p10_st_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p10_st_file$datapath)) })
  output$p10_st_column_selector <- renderUI({ req(p10_st_raw_data_file(), input$p10_st_selected_sheet); df <- read_excel(input$p10_st_file$datapath, sheet = input$p10_st_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p10_st_column", "Pilih Kolom Data:", choices = numeric_cols) })
  
  observeEvent(input$p10_st_analyze, {
    x <- p10_st_get_data(); x <- x[!is.na(x)]; n <- length(x); h0_val <- input$p10_st_median_h0; alpha <- input$p10_st_alpha; alt <- input$p10_st_alternative
    validate(need(n > 0, "Data tidak valid atau kosong."))
    
    sign_test <- BSDA::SIGN.test(x, md = h0_val, alternative = alt)
    n_plus <- sum(x > h0_val); n_minus <- sum(x < h0_val); n_ties <- sum(x == h0_val)
    p_val_formatted <- ifelse(is.na(sign_test$p.value), "N/A", ifelse(sign_test$p.value < 0.001, "< 0.001", as.character(round(sign_test$p.value, 4))))
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI TANDA (SIGN TEST)\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah median populasi (M) secara signifikan berbeda dari ", h0_val, ".\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: M = ", h0_val, "\n   - H₁: M ", switch(alt, two.sided="≠", less="<", greater=">"), " ", h0_val, "\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Median Sampel: ", median(x), "\n   - Jumlah Tanda Positif (+): ", n_plus, "\n   - Jumlah Tanda Negatif (-): ", n_minus, "\n   - Jumlah Ties (Nilai = H₀): ", n_ties, "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", sign_test$method, "\n   - Statistik Uji (S): ", sign_test$statistic, "\n   - p-value: ", p_val_formatted, "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(sign_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(sign_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan median populasi berbeda signifikan dari ", "Tidak terdapat cukup bukti untuk menyatakan median populasi berbeda dari "), h0_val, "."
    )
    p_gg <- ggplot(data.frame(x=x), aes(y=x)) + geom_boxplot(fill="skyblue") + geom_hline(yintercept=h0_val, color="red", linetype="dashed") + labs(title="Boxplot Data vs Median H₀")
    
    analysis_results_p10$report_st <- report_string; analysis_results_p10$plot_obj_st <- ggplotly(p_gg); analysis_results_p10$data_st <- x
    output$p10_st_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p10_st_results"), br(), downloadButton("p10_st_download_report", "Unduh Laporan (.docx)")) }); output$p10_st_results <- renderText({ analysis_results_p10$report_st }); output$p10_st_plot <- renderPlotly({ analysis_results_p10$plot_obj_st })
    output$p10_st_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Data=x), options=list(pageLength=5)) })
  })
  
  output$p10_st_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Tanda-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p10$report_st); plot_path <- file.path(tempdir(), "plot_p10_st.png"); data_for_plot <- analysis_results_p10$data_st
      p_gg_download <- ggplot(data.frame(x=data_for_plot), aes(y=x)) + geom_boxplot(fill="skyblue") + geom_hline(yintercept=input$p10_st_median_h0, color="red", linetype="dashed") + labs(title="Boxplot Data vs Median H₀")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p10$report_st)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Tanda'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk UJI TANDA BERPASANGAN (BARU) ---
  p10_pst_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p10_pst_file, { req(input$p10_pst_file); p10_pst_raw_data_file(read_excel(input$p10_pst_file$datapath)) })
  p10_pst_get_data <- reactive({
    if (input$p10_pst_input_method == "manual") {
      req(input$p10_pst_manual_data_a, input$p10_pst_manual_data_b)
      list(a = as.numeric(unlist(strsplit(input$p10_pst_manual_data_a, ","))), b = as.numeric(unlist(strsplit(input$p10_pst_manual_data_b, ","))))
    } else {
      req(p10_pst_raw_data_file(), input$p10_pst_selected_sheet, input$p10_pst_column_a, input$p10_pst_column_b)
      df <- read_excel(input$p10_pst_file$datapath, sheet = input$p10_pst_selected_sheet)
      list(a = as.numeric(df[[input$p10_pst_column_a]]), b = as.numeric(df[[input$p10_pst_column_b]]))
    }
  })
  output$p10_pst_sheet_selector <- renderUI({ req(p10_pst_raw_data_file()); selectInput("p10_pst_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p10_pst_file$datapath)) })
  output$p10_pst_column_selector_a <- renderUI({ req(p10_pst_raw_data_file(), input$p10_pst_selected_sheet); df <- read_excel(input$p10_pst_file$datapath, sheet = input$p10_pst_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p10_pst_column_a", "Kolom Sampel 1:", choices = numeric_cols, selected = numeric_cols[1]) })
  output$p10_pst_column_selector_b <- renderUI({ req(p10_pst_raw_data_file(), input$p10_pst_selected_sheet); df <- read_excel(input$p10_pst_file$datapath, sheet = input$p10_pst_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; available_cols <- setdiff(numeric_cols, input$p10_pst_column_a); selectInput("p10_pst_column_b", "Kolom Sampel 2:", choices = available_cols, selected = available_cols[1]) })
  
  observeEvent(input$p10_pst_analyze, {
    data_list <- p10_pst_get_data(); x1 <- data_list$a; x2 <- data_list$b; 
    df_paired <- na.omit(data.frame(x1, x2)); x1 <- df_paired$x1; x2 <- df_paired$x2
    n <- nrow(df_paired); alpha <- input$p10_pst_alpha; alt <- input$p10_pst_alternative
    validate(need(n > 0, "Data tidak valid atau kosong."), need(length(x1) == length(x2), "Sampel berpasangan harus memiliki panjang yang sama."))
    
    sign_test <- BSDA::SIGN.test(x1, x2, alternative = alt)
    diffs <- x1 - x2; n_plus <- sum(diffs > 0); n_minus <- sum(diffs < 0); n_ties <- sum(diffs == 0)
    p_val_formatted <- ifelse(is.na(sign_test$p.value), "N/A", ifelse(sign_test$p.value < 0.001, "< 0.001", as.character(round(sign_test$p.value, 4))))
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI TANDA BERPASANGAN\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan median yang signifikan antara dua sampel berpasangan.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Median selisih (M_d) = 0\n   - H₁: Median selisih (M_d) ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Jumlah Pasangan (n): ", n, "\n   - Jumlah Tanda Positif (+): ", n_plus, "\n   - Jumlah Tanda Negatif (-): ", n_minus, "\n   - Jumlah Ties (Selisih = 0): ", n_ties, "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", sign_test$method, "\n   - Statistik Uji (S): ", sign_test$statistic, "\n   - p-value: ", p_val_formatted, "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(sign_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(sign_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan median yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan median yang signifikan.")
    )
    p_gg <- ggplot(data.frame(diffs=diffs), aes(x=diffs)) + geom_histogram(fill="purple", color="black", bins=15) + geom_vline(xintercept=0, color="red", linetype="dashed") + labs(title="Distribusi Selisih Data (Sampel 1 - Sampel 2)")
    
    analysis_results_p10$report_pst <- report_string; analysis_results_p10$plot_obj_pst <- ggplotly(p_gg); analysis_results_p10$data_pst_a <- x1; analysis_results_p10$data_pst_b <- x2
    output$p10_pst_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p10_pst_results"), br(), downloadButton("p10_pst_download_report", "Unduh Laporan (.docx)")) }); output$p10_pst_results <- renderText({ analysis_results_p10$report_pst }); output$p10_pst_plot <- renderPlotly({ analysis_results_p10$plot_obj_pst })
    output$p10_pst_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Sampel_1=x1, Sampel_2=x2), options=list(pageLength=5)) })
  })
  
  output$p10_pst_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Tanda-Berpasangan-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p10$report_pst); plot_path <- file.path(tempdir(), "plot_p10_pst.png"); x1 <- analysis_results_p10$data_pst_a; x2 <- analysis_results_p10$data_pst_b
      p_gg_download <- ggplot(data.frame(diffs=x1-x2), aes(x=diffs)) + geom_histogram(fill="purple", color="black", bins=15) + geom_vline(xintercept=0, color="red", linetype="dashed") + labs(title="Distribusi Selisih Data (Sampel 1 - Sampel 2)")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p10$report_pst)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Tanda Berpasangan'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk UJI KEACAKAN (RUNS TEST) ---
  p10_rt_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p10_rt_file, { req(input$p10_rt_file); p10_rt_raw_data_file(read_excel(input$p10_rt_file$datapath)) })
  p10_rt_get_data <- reactive({
    if (input$p10_rt_input_method == "manual") {
      req(input$p10_rt_manual_data); as.numeric(unlist(strsplit(input$p10_rt_manual_data, ",")))
    } else {
      req(p10_rt_raw_data_file(), input$p10_rt_selected_sheet, input$p10_rt_column)
      df <- read_excel(input$p10_rt_file$datapath, sheet = input$p10_rt_selected_sheet)
      as.numeric(df[[input$p10_rt_column]])
    }
  })
  output$p10_rt_sheet_selector <- renderUI({ req(p10_rt_raw_data_file()); selectInput("p10_rt_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p10_rt_file$datapath)) })
  output$p10_rt_column_selector <- renderUI({ req(p10_rt_raw_data_file(), input$p10_rt_selected_sheet); df <- read_excel(input$p10_rt_file$datapath, sheet = input$p10_rt_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p10_rt_column", "Pilih Kolom Data:", choices = numeric_cols) })
  
  observeEvent(input$p10_rt_analyze, {
    x <- p10_rt_get_data(); x <- x[!is.na(x)]; n <- length(x); alpha <- input$p10_rt_alpha
    validate(need(n > 1, "Data tidak valid atau kurang dari 2 observasi."))
    
    # --- PERUBAHAN DI SINI: Menentukan cutoff secara dinamis ---
    cutoff <- switch(input$p10_rt_cutoff_method,
                     "median" = median(x),
                     "mean" = mean(x),
                     "manual" = input$p10_rt_cutoff_manual)
    
    runs_test <- randtests::runs.test(x, threshold = cutoff)
    p_val_formatted <- ifelse(is.na(runs_test$p.value), "N/A", ifelse(runs_test$p.value < 0.001, "< 0.001", as.character(round(runs_test$p.value, 4))))
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI KEACAKAN (RUNS TEST)\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah urutan data dalam sampel bersifat acak.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Urutan data bersifat acak.\n   - H₁: Urutan data tidak bersifat acak (memiliki pola).\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Ukuran Sampel (n): ", n, "\n   - Titik Potong (Cut-off): ", round(cutoff, 4), " (Metode: ", input$p10_rt_cutoff_method, ")\n   - Jumlah 'Runs': ", runs_test$runs, "\n   - Jumlah di atas cut-off (n₁): ", runs_test$n1, "\n   - Jumlah di bawah cut-off (n₂): ", runs_test$n2, "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: Runs Test\n",
      "   - Statistik Uji (Z): ", round(runs_test$statistic, 4), "\n   - p-value: ", p_val_formatted, "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(runs_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(runs_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan urutan data TIDAK acak.", "Tidak terdapat cukup bukti untuk menyatakan urutan data tidak acak.")
    )
    df_plot <- data.frame(Index = 1:n, Value = x, Group = ifelse(x >= cutoff, "Diatas/Sama Dengan", "Dibawah"))
    p_gg <- ggplot(df_plot, aes(x=Index, y=Value, color=Group)) + geom_line(color="grey") + geom_point(size=3) + geom_hline(yintercept=cutoff, color="blue", linetype="dashed") + labs(title="Plot Urutan Data vs Titik Potong")
    
    analysis_results_p10$report_rt <- report_string; analysis_results_p10$plot_obj_rt <- ggplotly(p_gg); analysis_results_p10$data_rt <- x
    output$p10_rt_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p10_rt_results"), br(), downloadButton("p10_rt_download_report", "Unduh Laporan (.docx)")) }); output$p10_rt_results <- renderText({ analysis_results_p10$report_rt }); output$p10_rt_plot <- renderPlotly({ analysis_results_p10$plot_obj_rt })
    output$p10_rt_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Urutan=1:n, Data=x), options=list(pageLength=5)) })
  })
  
  output$p10_rt_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Keacakan-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p10$report_rt); plot_path <- file.path(tempdir(), "plot_p10_rt.png"); data_for_plot <- analysis_results_p10$data_rt
      
      # --- PERBAIKAN DI SINI: Menggunakan cutoff yang benar untuk plot ---
      cutoff_for_plot <- switch(input$p10_rt_cutoff_method,
                                "median" = median(data_for_plot),
                                "mean" = mean(data_for_plot),
                                "manual" = input$p10_rt_cutoff_manual)
      
      df_plot <- data.frame(Index = 1:length(data_for_plot), Value = data_for_plot, Group = ifelse(data_for_plot >= cutoff_for_plot, "Diatas/Sama Dengan", "Dibawah"))
      p_gg_download <- ggplot(df_plot, aes(x=Index, y=Value, color=Group)) + geom_line(color="grey") + geom_point(size=3) + geom_hline(yintercept=cutoff_for_plot, color="blue", linetype="dashed") + labs(title="Plot Urutan Data vs Titik Potong")
      
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report_p10_rt.Rmd"); report_md <- gsub("=", "", analysis_results_p10$report_rt)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Keacakan'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 10  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 10 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 10
  quiz_questions_p10 <- list(
    list(
      id = "q1",
      question = "1. Uji Tanda (Sign Test) adalah uji nonparametrik yang paling sesuai untuk menguji hipotesis tentang...",
      choices = c("A. Rata-rata populasi",
                  "B. Median populasi",
                  "C. Varians populasi",
                  "D. Korelasi antar variabel"),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Dalam melakukan Uji Tanda, observasi yang nilainya sama persis dengan median hipotesis (menghasilkan selisih nol) akan...",
      choices = c("A. Diberi tanda positif (+).",
                  "B. Diberi tanda negatif (-).",
                  "C. Diabaikan atau tidak diikutsertakan dalam perhitungan.",
                  "D. Dihitung sebagai setengah positif dan setengah negatif."),
      answer = "C"
    ),
    list(
      id = "q3",
      question = "3. Uji Keacakan (Runs Test) digunakan untuk tujuan apa?",
      choices = c("A. Untuk menguji apakah rata-rata dua kelompok berbeda.",
                  "B. Untuk menguji apakah urutan data dalam sampel bersifat acak atau memiliki pola tertentu.",
                  "C. Untuk menguji apakah data berdistribusi normal.",
                  "D. Untuk menguji perbedaan median antara data sebelum dan sesudah perlakuan."),
      answer = "B"
    ),
    list(
      id = "q4",
      question = "4. Kapan pendekatan distribusi Normal (Uji Z) digunakan sebagai aproksimasi dalam Uji Tanda?",
      choices = c("A. Ketika ukuran sampel efektif (N, tanpa ties) lebih besar dari 25.",
                  "B. Ketika ukuran sampel efektif lebih kecil dari 10.",
                  "C. Ketika data dijamin berdistribusi normal.",
                  "D. Ketika varians populasi diketahui."),
      answer = "A"
    ),
    list(
      id = "q5",
      question = "5. Anda memiliki data berpasangan (misalnya, nilai sebelum dan sesudah pelatihan) dan ingin melihat apakah ada perbedaan tanpa mengasumsikan normalitas. Uji nonparametrik yang sesuai adalah...",
      choices = c("A. Uji Keacakan (Runs Test)",
                  "B. Uji-t Sampel Independen",
                  "C. Uji Tanda untuk Data Berpasangan (Paired Sign Test)",
                  "D. ANOVA"),
      answer = "C"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 10
  quiz_state_p10 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 10
  observeEvent(input$start_quiz_p10_new, {
    quiz_state_p10$status <- "in_progress"
    quiz_state_p10$user_answers <- NULL
    quiz_state_p10$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 10",
        uiOutput("quiz_ui_p10"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 10
  output$quiz_ui_p10 <- renderUI({
    if (quiz_state_p10$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p10, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p10_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p10_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p10$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p10$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p10$results_df), function(i) {
              row <- quiz_state_p10$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p10", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 10
  observeEvent(input$submit_quiz_p10_final, {
    
    user_answers <- sapply(quiz_questions_p10, function(q) {
      input[[paste0("p10_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p10$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p10, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p10[[i]]$choices[which(substr(quiz_questions_p10[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p10[[i]]$answer
        quiz_questions_p10[[i]]$choices[which(substr(quiz_questions_p10[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p10, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p10$results_df <- results_df
    quiz_state_p10$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 10
  output$download_quiz_report_p10 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-10-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p10$results_df)
      
      quiz_results <- quiz_state_p10$results_df
      tempReport <- file.path(tempdir(), "report_p10.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 10'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 10 ################

  #################### LOGIKA UNTUK PERTEMUAN 11  ####################
  analysis_results_p11 <- reactiveValues(
    report_wsr = NULL, plot_obj_wsr = NULL, data_wsr_a = NULL, data_wsr_b = NULL,
    report_mw = NULL, plot_obj_mw = NULL, data_mw = NULL,
    report_ks = NULL, plot_obj_ks = NULL, data_ks = NULL
  )
  
  # --- Logika untuk Wilcoxon Signed-Rank Test ---
  p11_wsr_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p11_wsr_file, { req(input$p11_wsr_file); p11_wsr_raw_data_file(read_excel(input$p11_wsr_file$datapath)) })
  p11_wsr_get_data <- reactive({
    if (input$p11_wsr_input_method == "manual") {
      req(input$p11_wsr_manual_data_a, input$p11_wsr_manual_data_b)
      list(a = as.numeric(unlist(strsplit(input$p11_wsr_manual_data_a, ","))), b = as.numeric(unlist(strsplit(input$p11_wsr_manual_data_b, ","))))
    } else {
      req(p11_wsr_raw_data_file(), input$p11_wsr_selected_sheet, input$p11_wsr_column_a, input$p11_wsr_column_b)
      df <- read_excel(input$p11_wsr_file$datapath, sheet = input$p11_wsr_selected_sheet)
      list(a = as.numeric(df[[input$p11_wsr_column_a]]), b = as.numeric(df[[input$p11_wsr_column_b]]))
    }
  })
  output$p11_wsr_sheet_selector <- renderUI({ req(p11_wsr_raw_data_file()); selectInput("p11_wsr_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p11_wsr_file$datapath)) })
  output$p11_wsr_column_selector_a <- renderUI({ req(p11_wsr_raw_data_file(), input$p11_wsr_selected_sheet); df <- read_excel(input$p11_wsr_file$datapath, sheet = input$p11_wsr_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p11_wsr_column_a", "Kolom Sampel 1:", choices = numeric_cols, selected = numeric_cols[1]) })
  output$p11_wsr_column_selector_b <- renderUI({ req(p11_wsr_raw_data_file(), input$p11_wsr_selected_sheet); df <- read_excel(input$p11_wsr_file$datapath, sheet = input$p11_wsr_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; available_cols <- setdiff(numeric_cols, input$p11_wsr_column_a); selectInput("p11_wsr_column_b", "Kolom Sampel 2:", choices = available_cols, selected = available_cols[1]) })
  
  observeEvent(input$p11_wsr_analyze, {
    data_list <- p11_wsr_get_data(); x1 <- data_list$a; x2 <- data_list$b; 
    df_paired <- na.omit(data.frame(x1, x2)); x1 <- df_paired$x1; x2 <- df_paired$x2
    n <- nrow(df_paired); alpha <- input$p11_wsr_alpha; alt <- input$p11_wsr_alternative
    validate(need(n > 0, "Data tidak valid atau kosong."), need(length(x1) == length(x2), "Sampel berpasangan harus memiliki panjang yang sama."))
    
    wilcox_test <- wilcox.test(x1, x2, paired = TRUE, alternative = alt)
    diffs <- x1 - x2
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI PERINGKAT BERTANDA WILCOXON\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan lokasi (median) yang signifikan antara dua sampel berpasangan.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Median selisih = 0\n   - H₁: Median selisih ", switch(alt, two.sided="≠", less="<", greater=">"), " 0\n\n",
      "3. STATISTIK DESKRIPTIF:\n   - Jumlah Pasangan (n): ", n, "\n   - Median Sampel 1: ", median(x1), "\n   - Median Sampel 2: ", median(x2), "\n   - Median Selisih: ", median(diffs), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", wilcox_test$method, "\n   - Statistik Uji (V): ", wilcox_test$statistic, "\n   - p-value: ", format.p(wilcox_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(wilcox_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(wilcox_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan lokasi (median) yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan lokasi (median) yang signifikan.")
    )
    p_gg <- ggplot(data.frame(diffs=diffs), aes(x=diffs)) + geom_histogram(fill="purple", color="black", bins=15) + geom_vline(xintercept=0, color="red", linetype="dashed") + labs(title="Distribusi Selisih Data")
    
    analysis_results_p11$report_wsr <- report_string; analysis_results_p11$plot_obj_wsr <- ggplotly(p_gg); analysis_results_p11$data_wsr_a <- x1; analysis_results_p11$data_wsr_b <- x2
    output$p11_wsr_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p11_wsr_results"), br(), downloadButton("p11_wsr_download_report", "Unduh Laporan (.docx)")) }); output$p11_wsr_results <- renderText({ analysis_results_p11$report_wsr }); output$p11_wsr_plot <- renderPlotly({ analysis_results_p11$plot_obj_wsr })
    output$p11_wsr_data_table <- DT::renderDataTable({ DT::datatable(data.frame(Sampel_1=x1, Sampel_2=x2), options=list(pageLength=5)) })
  })
  
  output$p11_wsr_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Wilcoxon-Signed-Rank-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p11$report_wsr); plot_path <- file.path(tempdir(), "plot_p11_wsr.png"); x1 <- analysis_results_p11$data_wsr_a; x2 <- analysis_results_p11$data_wsr_b
      p_gg_download <- ggplot(data.frame(diffs=x1-x2), aes(x=diffs)) + geom_histogram(fill="purple", color="black", bins=15) + geom_vline(xintercept=0, color="red", linetype="dashed") + labs(title="Distribusi Selisih Data")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p11$report_wsr)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk Mann-Whitney U Test ---
  p11_mw_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p11_mw_file, { req(input$p11_mw_file); p11_mw_raw_data_file(read_excel(input$p11_mw_file$datapath)) })
  p11_mw_get_data <- reactive({
    if (input$p11_mw_input_method == "manual") {
      req(input$p11_mw_manual_data_group, input$p11_mw_manual_data_value)
      groups <- trimws(unlist(strsplit(input$p11_mw_manual_data_group, ","))); values <- as.numeric(unlist(strsplit(input$p11_mw_manual_data_value, ",")))
      validate(need(length(groups) == length(values), "Jumlah elemen grup dan nilai harus sama."))
      data.frame(Group = as.factor(groups), Value = values)
    } else {
      req(p11_mw_raw_data_file(), input$p11_mw_selected_sheet, input$p11_mw_col_group, input$p11_mw_col_value)
      df <- read_excel(input$p11_mw_file$datapath, sheet = input$p11_mw_selected_sheet)
      data.frame(Group = as.factor(df[[input$p11_mw_col_group]]), Value = as.numeric(df[[input$p11_mw_col_value]]))
    }
  })
  output$p11_mw_sheet_selector <- renderUI({ req(p11_mw_raw_data_file()); selectInput("p11_mw_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p11_mw_file$datapath)) })
  output$p11_mw_column_selector_group <- renderUI({ req(p11_mw_raw_data_file(), input$p11_mw_selected_sheet); df <- read_excel(input$p11_mw_file$datapath, sheet = input$p11_mw_selected_sheet); selectInput("p11_mw_col_group", "Kolom Grup:", choices = names(df)) })
  output$p11_mw_column_selector_value <- renderUI({ req(p11_mw_raw_data_file(), input$p11_mw_selected_sheet); df <- read_excel(input$p11_mw_file$datapath, sheet = input$p11_mw_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p11_mw_col_value", "Kolom Nilai:", choices = numeric_cols) })
  
  observeEvent(input$p11_mw_analyze, {
    df <- p11_mw_get_data(); df <- na.omit(df); alpha <- input$p11_mw_alpha; alt <- input$p11_mw_alternative
    validate(need(nrow(df) > 0, "Data tidak valid."), need(nlevels(df$Group) == 2, "Data harus memiliki tepat 2 kelompok."))
    
    wilcox_test <- wilcox.test(Value ~ Group, data = df, alternative = alt)
    desc_stats <- df %>% group_by(Group) %>% summarise(n=n(), Median=median(Value), IQR=IQR(Value), .groups="drop")
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI MANN-WHITNEY U\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan lokasi (distribusi) yang signifikan antara dua sampel independen.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Distribusi kedua kelompok adalah sama.\n   - H₁: Distribusi kedua kelompok tidak sama.\n\n",
      "3. STATISTIK DESKRIPTIF:\n", paste(capture.output(print(desc_stats)), collapse="\n"), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", wilcox_test$method, "\n   - Statistik Uji (W): ", wilcox_test$statistic, "\n   - p-value: ", format.p(wilcox_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(wilcox_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(wilcox_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan lokasi yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan ada perbedaan lokasi yang signifikan.")
    )
    p_gg <- ggplot(df, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
    
    analysis_results_p11$report_mw <- report_string; analysis_results_p11$plot_obj_mw <- ggplotly(p_gg); analysis_results_p11$data_mw <- df
    output$p11_mw_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p11_mw_results"), br(), downloadButton("p11_mw_download_report", "Unduh Laporan (.docx)")) }); output$p11_mw_results <- renderText({ analysis_results_p11$report_mw }); output$p11_mw_plot <- renderPlotly({ analysis_results_p11$plot_obj_mw })
    output$p11_mw_data_table <- DT::renderDataTable({ DT::datatable(df, options=list(pageLength=5)) })
  })
  
  output$p11_mw_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Mann-Whitney-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p11$report_mw); plot_path <- file.path(tempdir(), "plot_p11_mw.png"); df_plot <- analysis_results_p11$data_mw
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p11$report_mw)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk Kolmogorov-Smirnov Test (DIPERBARUI) ---
  p11_ks_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p11_ks_file, { req(input$p11_ks_file); p11_ks_raw_data_file(read_excel(input$p11_ks_file$datapath)) })
  p11_ks_get_data <- reactive({
    if (input$p11_ks_input_method == "manual") {
      req(input$p11_ks_manual_data_group, input$p11_ks_manual_data_value)
      groups <- trimws(unlist(strsplit(input$p11_ks_manual_data_group, ","))); values <- as.numeric(unlist(strsplit(input$p11_ks_manual_data_value, ",")))
      validate(need(length(groups) == length(values), "Jumlah elemen grup dan nilai harus sama."))
      data.frame(Group = as.factor(groups), Value = values)
    } else {
      req(p11_ks_raw_data_file(), input$p11_ks_selected_sheet, input$p11_ks_col_group, input$p11_ks_col_value)
      df <- read_excel(input$p11_ks_file$datapath, sheet = input$p11_ks_selected_sheet)
      data.frame(Group = as.factor(df[[input$p11_ks_col_group]]), Value = as.numeric(df[[input$p11_ks_col_value]]))
    }
  })
  output$p11_ks_sheet_selector <- renderUI({ req(p11_ks_raw_data_file()); selectInput("p11_ks_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p11_ks_file$datapath)) })
  output$p11_ks_column_selector_group <- renderUI({ req(p11_ks_raw_data_file(), input$p11_ks_selected_sheet); df <- read_excel(input$p11_ks_file$datapath, sheet = input$p11_ks_selected_sheet); selectInput("p11_ks_col_group", "Kolom Grup:", choices = names(df)) })
  output$p11_ks_column_selector_value <- renderUI({ req(p11_ks_raw_data_file(), input$p11_ks_selected_sheet); df <- read_excel(input$p11_ks_file$datapath, sheet = input$p11_ks_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p11_ks_col_value", "Kolom Nilai:", choices = numeric_cols) })
  
  observeEvent(input$p11_ks_analyze, {
    df <- p11_ks_get_data(); df <- na.omit(df); alpha <- input$p11_ks_alpha; alt <- input$p11_ks_alternative
    validate(need(nrow(df) > 0, "Data tidak valid."), need(nlevels(df$Group) == 2, "Data harus memiliki tepat 2 kelompok."))
    
    x1 <- df$Value[df$Group == levels(df$Group)[1]]
    x2 <- df$Value[df$Group == levels(df$Group)[2]]
    
    ks_test <- ks.test(x1, x2, alternative = alt)
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI KOLMOGOROV-SMIRNOV\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah dua sampel independen berasal dari distribusi yang sama.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Kedua sampel berasal dari distribusi yang sama (F₁(x) = F₂(x)).\n   - H₁: Kedua sampel berasal dari distribusi yang berbeda.\n\n",
      "3. HASIL UJI STATISTIK:\n   - Metode Uji: Two-sample Kolmogorov-Smirnov test.\n",
      "   - Statistik Uji (D) dihitung dengan rumus: D = max|S₁(x) - S₂(x)|, di mana S(x)\n     adalah fungsi distribusi kumulatif empiris.\n",
      "   - Nilai Statistik Uji (D): ", round(ks_test$statistic, 4), "\n",
      "   - p-value: ", format.p(ks_test$p.value), "\n\n",
      "4. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(ks_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(ks_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan kedua sampel berasal dari distribusi yang berbeda.", "Tidak terdapat cukup bukti untuk menyatakan kedua sampel berasal dari distribusi yang sama.")
    )
    p_gg <- ggplot(df, aes(x = Value, color = Group)) + stat_ecdf(geom = "step") + labs(title="Empirical Cumulative Distribution Functions (ECDF)", y="F(x)")
    
    analysis_results_p11$report_ks <- report_string; analysis_results_p11$plot_obj_ks <- ggplotly(p_gg); analysis_results_p11$data_ks <- df
    output$p11_ks_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p11_ks_results"), br(), downloadButton("p11_ks_download_report", "Unduh Laporan (.docx)")) }); output$p11_ks_results <- renderText({ analysis_results_p11$report_ks }); output$p11_ks_plot <- renderPlotly({ analysis_results_p11$plot_obj_ks })
    output$p11_ks_data_table <- DT::renderDataTable({ DT::datatable(df, options=list(pageLength=5)) })
  })
  
  output$p11_ks_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Kolmogorov-Smirnov-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p11$report_ks); plot_path <- file.path(tempdir(), "plot_p11_ks.png"); df_plot <- analysis_results_p11$data_ks
      p_gg_download <- ggplot(df_plot, aes(x = Value, color = Group)) + stat_ecdf(geom = "step") + labs(title="Empirical Cumulative Distribution Functions (ECDF)", y="F(x)")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p11$report_ks)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  #################### LOGIKA UNTUK PERTEMUAN 11  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 11 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 11
  quiz_questions_p11 <- list(
    list(
      id = "q1",
      question = "1. Seorang peneliti mengukur tingkat kecemasan sekelompok pasien yang sama sebelum dan sesudah sesi terapi. Data tidak terdistribusi normal. Uji nonparametrik manakah yang paling sesuai untuk melihat apakah terapi tersebut berpengaruh?",
      choices = c("A. Uji Mann-Whitney U",
                  "B. Uji Peringkat Bertanda Wilcoxon (Wilcoxon Signed-Rank Test)",
                  "C. Uji Kruskal-Wallis",
                  "D. Uji Kolmogorov-Smirnov"),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Uji Mann-Whitney U adalah alternatif nonparametrik untuk uji parametrik manakah?",
      choices = c("A. Uji-t sampel berpasangan",
                  "B. ANOVA satu arah",
                  "C. Uji-t sampel independen",
                  "D. Korelasi Pearson"),
      answer = "C"
    ),
    list(
      id = "q3",
      question = "3. Apa yang dibandingkan oleh Uji Kolmogorov-Smirnov dua sampel antara dua sampel independen?",
      choices = c("A. Rata-ratanya",
                  "B. Mediannya",
                  "C. Variansnya",
                  "D. Fungsi distribusi kumulatifnya (ECDFs)"),
      answer = "D"
    ),
    list(
      id = "q4",
      question = "4. Anda membandingkan gaji karyawan pria dan wanita di sebuah perusahaan. Data gaji untuk kedua kelompok sangat condong (skewed). Uji manakah yang sebaiknya Anda gunakan?",
      choices = c("A. Uji Peringkat Bertanda Wilcoxon",
                  "B. Uji-t berpasangan",
                  "C. Uji Mann-Whitney U",
                  "D. Uji F untuk varians"),
      answer = "C"
    ),
    list(
      id = "q5",
      question = "5. Uji Peringkat Bertanda Wilcoxon lebih kuat (powerful) daripada Uji Tanda biasa karena ia mempertimbangkan tidak hanya arah (tanda) dari selisih tetapi juga...",
      choices = c("A. Rata-rata dari selisih",
                  "B. Jumlah data yang sama (ties)",
                  "C. Besaran (peringkat) dari selisih",
                  "D. Standar deviasi dari selisih"),
      answer = "C"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 11
  quiz_state_p11 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 11
  observeEvent(input$start_quiz_p11_new, {
    quiz_state_p11$status <- "in_progress"
    quiz_state_p11$user_answers <- NULL
    quiz_state_p11$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 11",
        uiOutput("quiz_ui_p11"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 11
  output$quiz_ui_p11 <- renderUI({
    if (quiz_state_p11$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p11, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p11_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p11_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p11$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p11$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p11$results_df), function(i) {
              row <- quiz_state_p11$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p11", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 11
  observeEvent(input$submit_quiz_p11_final, {
    
    user_answers <- sapply(quiz_questions_p11, function(q) {
      input[[paste0("p11_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p11$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p11, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p11[[i]]$choices[which(substr(quiz_questions_p11[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p11[[i]]$answer
        quiz_questions_p11[[i]]$choices[which(substr(quiz_questions_p11[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p11, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p11$results_df <- results_df
    quiz_state_p11$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 11
  output$download_quiz_report_p11 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-11-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p11$results_df)
      
      quiz_results <- quiz_state_p11$results_df
      tempReport <- file.path(tempdir(), "report_p11.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 11'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 11 ################

  #################### LOGIKA UNTUK PERTEMUAN 12  ####################
  analysis_results_p12 <- reactiveValues(
    report_kw = NULL, plot_obj_kw = NULL, data_kw = NULL,
    report_fr = NULL, plot_obj_fr = NULL, data_fr = NULL
  )
  
  # --- Logika untuk Kruskal-Wallis Test ---
  p12_kw_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p12_kw_file, { req(input$p12_kw_file); p12_kw_raw_data_file(read_excel(input$p12_kw_file$datapath)) })
  p12_kw_get_data <- reactive({
    if (input$p12_kw_input_method == "manual") {
      req(input$p12_kw_manual_data_group, input$p12_kw_manual_data_value)
      groups <- trimws(unlist(strsplit(input$p12_kw_manual_data_group, ","))); values <- as.numeric(unlist(strsplit(input$p12_kw_manual_data_value, ",")))
      validate(need(length(groups) == length(values), "Jumlah elemen grup dan nilai harus sama."))
      data.frame(Group = as.factor(groups), Value = values)
    } else {
      req(p12_kw_raw_data_file(), input$p12_kw_selected_sheet, input$p12_kw_col_group, input$p12_kw_col_value)
      df <- read_excel(input$p12_kw_file$datapath, sheet = input$p12_kw_selected_sheet)
      data.frame(Group = as.factor(df[[input$p12_kw_col_group]]), Value = as.numeric(df[[input$p12_kw_col_value]]))
    }
  })
  output$p12_kw_sheet_selector <- renderUI({ req(p12_kw_raw_data_file()); selectInput("p12_kw_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p12_kw_file$datapath)) })
  output$p12_kw_column_selector_group <- renderUI({ req(p12_kw_raw_data_file(), input$p12_kw_selected_sheet); df <- read_excel(input$p12_kw_file$datapath, sheet = input$p12_kw_selected_sheet); selectInput("p12_kw_col_group", "Kolom Grup:", choices = names(df)) })
  output$p12_kw_column_selector_value <- renderUI({ req(p12_kw_raw_data_file(), input$p12_kw_selected_sheet); df <- read_excel(input$p12_kw_file$datapath, sheet = input$p12_kw_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p12_kw_col_value", "Kolom Nilai:", choices = numeric_cols) })
  
  observeEvent(input$p12_kw_analyze, {
    df <- p12_kw_get_data(); df <- na.omit(df); alpha <- input$p12_kw_alpha
    validate(need(nrow(df) > 0, "Data tidak valid."), need(nlevels(df$Group) >= 2, "Data harus memiliki setidaknya 2 kelompok."))
    
    kw_test <- kruskal.test(Value ~ Group, data = df)
    desc_stats <- df %>% group_by(Group) %>% summarise(n=n(), Median=median(Value), IQR=IQR(Value), .groups="drop")
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI KRUSKAL-WALLIS\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan lokasi (distribusi) yang signifikan antara ", nlevels(df$Group), " kelompok independen.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Distribusi semua kelompok adalah sama.\n   - H₁: Setidaknya ada satu distribusi kelompok yang berbeda.\n\n",
      "3. STATISTIK DESKRIPTIF:\n", paste(capture.output(print(desc_stats)), collapse="\n"), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", kw_test$method, "\n   - Statistik Uji (χ²): ", round(kw_test$statistic, 4), "\n   - Derajat Bebas (df): ", kw_test$parameter, "\n   - p-value: ", format.p(kw_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(kw_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(kw_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan lokasi yang signifikan antara setidaknya satu pasang kelompok.", "Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan lokasi yang signifikan antar kelompok.")
    )
    
    if (kw_test$p.value < alpha && input$p12_kw_posthoc != "none") {
      report_string <- paste0(report_string, "\n\n6. HASIL UJI LANJUT (POST-HOC) DUNN:\n")
      dunn_test <- FSA::dunnTest(Value ~ Group, data = df, method = "bh")
      report_string <- paste0(report_string, paste(capture.output(print(dunn_test)), collapse="\n"))
    }
    
    p_gg <- ggplot(df, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
    
    analysis_results_p12$report_kw <- report_string; analysis_results_p12$plot_obj_kw <- ggplotly(p_gg); analysis_results_p12$data_kw <- df
    output$p12_kw_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p12_kw_results"), br(), downloadButton("p12_kw_download_report", "Unduh Laporan (.docx)")) }); output$p12_kw_results <- renderText({ analysis_results_p12$report_kw }); output$p12_kw_plot <- renderPlotly({ analysis_results_p12$plot_obj_kw })
    output$p12_kw_data_table <- DT::renderDataTable({ DT::datatable(df, options=list(pageLength=5)) })
  })
  
  output$p12_kw_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Kruskal-Wallis-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p12$report_kw); plot_path <- file.path(tempdir(), "plot_p12_kw.png"); df_plot <- analysis_results_p12$data_kw
      p_gg_download <- ggplot(df_plot, aes(x=Group, y=Value, fill=Group)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Kelompok")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p12$report_kw)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # --- Logika untuk Friedman Test ---
  p12_fr_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p12_fr_file, { req(input$p12_fr_file); p12_fr_raw_data_file(read_excel(input$p12_fr_file$datapath)) })
  p12_fr_get_data <- reactive({
    req(p12_fr_raw_data_file(), input$p12_fr_selected_sheet, input$p12_fr_col_value, input$p12_fr_col_treatment, input$p12_fr_col_block)
    df <- read_excel(input$p12_fr_file$datapath, sheet = input$p12_fr_selected_sheet)
    data.frame(
      Value = as.numeric(df[[input$p12_fr_col_value]]),
      Treatment = as.factor(df[[input$p12_fr_col_treatment]]),
      Block = as.factor(df[[input$p12_fr_col_block]])
    )
  })
  output$p12_fr_sheet_selector <- renderUI({ req(p12_fr_raw_data_file()); selectInput("p12_fr_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p12_fr_file$datapath)) })
  output$p12_fr_column_selector_value <- renderUI({ req(p12_fr_raw_data_file(), input$p12_fr_selected_sheet); df <- read_excel(input$p12_fr_file$datapath, sheet = input$p12_fr_selected_sheet); numeric_cols <- names(df)[sapply(df, is.numeric)]; selectInput("p12_fr_col_value", "Kolom Nilai:", choices = numeric_cols) })
  output$p12_fr_column_selector_treatment <- renderUI({ req(p12_fr_raw_data_file(), input$p12_fr_selected_sheet); df <- read_excel(input$p12_fr_file$datapath, sheet = input$p12_fr_selected_sheet); selectInput("p12_fr_col_treatment", "Kolom Perlakuan/Kondisi:", choices = names(df)) })
  output$p12_fr_column_selector_block <- renderUI({ req(p12_fr_raw_data_file(), input$p12_fr_selected_sheet); df <- read_excel(input$p12_fr_file$datapath, sheet = input$p12_fr_selected_sheet); selectInput("p12_fr_col_block", "Kolom Subjek/Blok:", choices = names(df)) })
  
  observeEvent(input$p12_fr_analyze, {
    df <- p12_fr_get_data(); df <- na.omit(df); alpha <- input$p12_fr_alpha
    validate(need(nrow(df) > 0, "Data tidak valid."), need(nlevels(df$Treatment) >= 2, "Data harus memiliki setidaknya 2 perlakuan."))
    
    # --- PERBAIKAN DI SINI: Validasi data untuk Friedman Test ---
    xtabs_check <- xtabs(~ Block + Treatment, data = df)
    validate(
      need(all(xtabs_check == 1), "Data tidak valid untuk Uji Friedman. Pastikan setiap subjek/blok memiliki tepat satu observasi untuk setiap perlakuan/kondisi (tidak ada data hilang atau duplikat).")
    )
    
    fr_test <- friedman.test(Value ~ Treatment | Block, data = df)
    desc_stats <- df %>% group_by(Treatment) %>% summarise(n=n(), Median=median(Value), IQR=IQR(Value), .groups="drop")
    
    report_string <- paste0(
      "==========================================\n   HASIL UJI FRIEDMAN\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji apakah terdapat perbedaan lokasi (distribusi) yang signifikan antara ", nlevels(df$Treatment), " kondisi/perlakuan berpasangan.\n\n",
      "2. RUMUSAN HIPOTESIS:\n   - H₀: Distribusi semua perlakuan adalah sama.\n   - H₁: Setidaknya ada satu distribusi perlakuan yang berbeda.\n\n",
      "3. STATISTIK DESKRIPTIF:\n", paste(capture.output(print(desc_stats)), collapse="\n"), "\n\n",
      "4. HASIL UJI STATISTIK:\n   - Metode Uji: ", fr_test$method, "\n   - Statistik Uji (χ²): ", round(fr_test$statistic, 4), "\n   - Derajat Bebas (df): ", fr_test$parameter, "\n   - p-value: ", format.p(fr_test$p.value), "\n\n",
      "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(fr_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(fr_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan ada perbedaan lokasi yang signifikan antara setidaknya satu pasang perlakuan.", "Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan lokasi yang signifikan antar perlakuan.")
    )
    p_gg <- ggplot(df, aes(x=Treatment, y=Value, fill=Treatment)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Perlakuan")
    
    analysis_results_p12$report_fr <- report_string; analysis_results_p12$plot_obj_fr <- ggplotly(p_gg); analysis_results_p12$data_fr <- df
    output$p12_fr_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p12_fr_results"), br(), downloadButton("p12_fr_download_report", "Unduh Laporan (.docx)")) }); output$p12_fr_results <- renderText({ analysis_results_p12$report_fr }); output$p12_fr_plot <- renderPlotly({ analysis_results_p12$plot_obj_fr })
    output$p12_fr_data_table <- DT::renderDataTable({ DT::datatable(df, options=list(pageLength=5)) })
  })
  
  output$p12_fr_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Friedman-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p12$report_fr); plot_path <- file.path(tempdir(), "plot_p12_fr.png"); df_plot <- analysis_results_p12$data_fr
      p_gg_download <- ggplot(df_plot, aes(x=Treatment, y=Value, fill=Treatment)) + geom_boxplot() + labs(title="Perbandingan Distribusi Antar Perlakuan")
      ggsave(plot_path, plot = p_gg_download, device = "png"); tempReport <- file.path(tempdir(), "report.Rmd"); report_md <- gsub("=", "", analysis_results_p12$report_fr)
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Uji Friedman'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE); write(report_md, tempReport, append = TRUE); write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE); write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 12  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 12 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 12
  quiz_questions_p12 <- list(
    list(
      id = "q1",
      question = "1. Uji Kruskal-Wallis adalah alternatif nonparametrik untuk uji parametrik manakah?",
      choices = c("A. Uji-t sampel independen",
                  "B. ANOVA Satu Arah",
                  "C. ANOVA Dua Arah",
                  "D. Korelasi Pearson"),
      answer = "B"
    ),
    list(
      id = "q2",
      question = "2. Dalam situasi manakah Uji Friedman paling tepat untuk digunakan?",
      choices = c("A. Membandingkan rata-rata tiga kelompok yang saling bebas.",
                  "B. Membandingkan tiga atau lebih pengukuran berulang pada subjek yang sama (sampel dependen).",
                  "C. Menguji hubungan antara tiga variabel atau lebih.",
                  "D. Menguji normalitas pada tiga kelompok atau lebih."),
      answer = "B"
    ),
    list(
      id = "q3",
      question = "3. Prinsip dasar dari Uji Kruskal-Wallis adalah menggabungkan semua data dari semua kelompok dan kemudian melakukan analisis berdasarkan...",
      choices = c("A. Rata-rata dari setiap kelompok.",
                  "B. Median dari setiap kelompok.",
                  "C. Peringkat (ranking) dari data gabungan tersebut.",
                  "D. Varians dari data gabungan tersebut."),
      answer = "C"
    ),
    list(
      id = "q4",
      question = "4. Jika hasil Uji Kruskal-Wallis Anda signifikan (misalnya, p < 0.05), apa artinya dan apa langkah selanjutnya?",
      choices = c("A. Artinya semua kelompok berbeda satu sama lain; tidak perlu uji lanjut.",
                  "B. Artinya setidaknya ada satu kelompok yang berbeda; perlu dilakukan uji lanjut (post-hoc) seperti Uji Dunn.",
                  "C. Artinya data tidak normal; perlu transformasi data.",
                  "D. Artinya varians tidak homogen; perlu menggunakan Uji Levene."),
      answer = "B"
    ),
    list(
      id = "q5",
      question = "5. Hipotesis nol (H₀) untuk Uji Friedman adalah...",
      choices = c("A. Rata-rata dari semua perlakuan/kondisi adalah sama.",
                  "B. Varians dari semua perlakuan/kondisi adalah sama.",
                  "C. Distribusi (atau median) dari semua perlakuan/kondisi adalah sama.",
                  "D. Semua blok (subjek) memiliki efek yang sama."),
      answer = "C"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 12
  quiz_state_p12 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 12
  observeEvent(input$start_quiz_p12_new, {
    quiz_state_p12$status <- "in_progress"
    quiz_state_p12$user_answers <- NULL
    quiz_state_p12$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 12",
        uiOutput("quiz_ui_p12"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 12
  output$quiz_ui_p12 <- renderUI({
    if (quiz_state_p12$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p12, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p12_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p12_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p12$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p12$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p12$results_df), function(i) {
              row <- quiz_state_p12$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p12", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 12
  observeEvent(input$submit_quiz_p12_final, {
    
    user_answers <- sapply(quiz_questions_p12, function(q) {
      input[[paste0("p12_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p12$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p12, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p12[[i]]$choices[which(substr(quiz_questions_p12[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p12[[i]]$answer
        quiz_questions_p12[[i]]$choices[which(substr(quiz_questions_p12[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p12, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p12$results_df <- results_df
    quiz_state_p12$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 12
  output$download_quiz_report_p12 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-12-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p12$results_df)
      
      quiz_results <- quiz_state_p12$results_df
      tempReport <- file.path(tempdir(), "report_p12.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 12'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 12 ################

  #################### LOGIKA UNTUK PERTEMUAN 13  ####################
  analysis_results_p13 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL,
    plot_type = NULL
  )
  
  # --- Logika Input & UI ---
  p13_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p13_file, { req(input$p13_file); p13_raw_data_file(read_excel(input$p13_file$datapath)) })
  
  p13_get_data <- reactive({
    if (input$p13_input_method == "manual") {
      req(input$p13_manual_data_x, input$p13_manual_data_y)
      x <- trimws(unlist(strsplit(input$p13_manual_data_x, ",")))
      y <- trimws(unlist(strsplit(input$p13_manual_data_y, ",")))
      validate(need(length(x) == length(y), "Jumlah elemen Variabel X dan Y harus sama."))
      data.frame(X = x, Y = y)
    } else {
      req(p13_raw_data_file(), input$p13_selected_sheet, input$p13_col_x, input$p13_col_y)
      df <- read_excel(input$p13_file$datapath, sheet = input$p13_selected_sheet)
      data.frame(X = df[[input$p13_col_x]], Y = df[[input$p13_col_y]])
    }
  })
  
  output$p13_sheet_selector <- renderUI({ req(p13_raw_data_file()); selectInput("p13_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p13_file$datapath)) })
  output$p13_column_selector_x <- renderUI({ req(p13_raw_data_file(), input$p13_selected_sheet); df <- read_excel(input$p13_file$datapath, sheet = input$p13_selected_sheet); selectInput("p13_col_x", "Kolom Variabel X:", choices = names(df), selected = names(df)[1]) })
  output$p13_column_selector_y <- renderUI({ req(p13_raw_data_file(), input$p13_selected_sheet); df <- read_excel(input$p13_file$datapath, sheet = input$p13_selected_sheet); selectInput("p13_col_y", "Kolom Variabel Y:", choices = names(df), selected = names(df)[2]) })
  
  # --- Observer Utama untuk Analisis ---
  observeEvent(input$p13_analyze, {
    df <- p13_get_data()
    df <- na.omit(df)
    alpha <- input$p13_alpha
    
    validate(need(nrow(df) > 1, "Data tidak cukup (minimal 2 observasi)."))
    
    report_string <- ""
    p_gg <- NULL
    
    if (input$p13_test_type %in% c("pearson", "spearman")) {
      x <- as.numeric(df$X); y <- as.numeric(df$Y)
      validate(need(!any(is.na(x)) && !any(is.na(y)), "Untuk korelasi Pearson/Spearman, kedua kolom harus numerik."))
      
      corr_test <- cor.test(x, y, method = input$p13_test_type)
      
      report_string <- paste0(
        "==========================================\n   HASIL UJI KORELASI ", toupper(input$p13_test_type), "\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji kekuatan dan arah hubungan antara dua variabel numerik.\n\n",
        "2. RUMUSAN HIPOTESIS:\n   - H₀: Tidak ada hubungan antara kedua variabel (ρ = 0).\n   - H₁: Terdapat hubungan antara kedua variabel (ρ ≠ 0).\n\n",
        "3. STATISTIK DESKRIPTIF:\n   - Jumlah Pasangan (n): ", nrow(df), "\n   - Rata-rata X: ", round(mean(x), 4), ", SD X: ", round(sd(x), 4), "\n   - Rata-rata Y: ", round(mean(y), 4), ", SD Y: ", round(sd(y), 4), "\n\n",
        "4. HASIL UJI STATISTIK:\n   - Koefisien Korelasi (r): ", round(corr_test$estimate, 4), "\n   - Statistik Uji (", names(corr_test$statistic), "): ", round(corr_test$statistic, 4), "\n   - p-value: ", format.p(corr_test$p.value), "\n\n",
        "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(corr_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(corr_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan adanya hubungan yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan adanya hubungan yang signifikan.")
      )
      p_gg <- ggplot(df, aes(x=X, y=Y)) + geom_point(color="blue") + geom_smooth(method="lm", se=FALSE, color="red") + labs(title="Scatter Plot Hubungan X dan Y")
      analysis_results_p13$plot_type <- "scatter"
      
    } else { # Chi-Square
      tbl <- table(df$X, df$Y)
      chi_test <- chisq.test(tbl)
      
      report_string <- paste0(
        "==========================================\n   HASIL UJI KEBEBASAN (CHI-SQUARE)\n==========================================\n\n",
        "1. TUJUAN ANALISIS:\n   Menguji apakah ada hubungan (asosiasi) antara dua variabel kategorik.\n\n",
        "2. RUMUSAN HIPOTESIS:\n   - H₀: Kedua variabel saling bebas (independen).\n   - H₁: Kedua variabel tidak saling bebas (dependen).\n\n",
        "3. TABEL KONTINGENSI:\n", paste(capture.output(print(tbl)), collapse="\n"), "\n\n",
        "4. HASIL UJI STATISTIK:\n   - Statistik Uji (χ²): ", round(chi_test$statistic, 4), "\n   - Derajat Bebas (df): ", chi_test$parameter, "\n   - p-value: ", format.p(chi_test$p.value), "\n\n",
        "5. KEPUTUSAN & INTERPRETASI (α = ", alpha, "):\n   - Keputusan: ", ifelse(chi_test$p.value < alpha, "TOLAK H₀.", "GAGAL TOLAK H₀."), "\n   - Interpretasi: ", ifelse(chi_test$p.value < alpha, "Terdapat cukup bukti untuk menyatakan adanya hubungan yang signifikan.", "Tidak terdapat cukup bukti untuk menyatakan adanya hubungan yang signifikan.")
      )
      df_plot <- as.data.frame(tbl)
      names(df_plot) <- c("VarX", "VarY", "Freq")
      p_gg <- ggplot(df_plot, aes(x=VarX, y=Freq, fill=VarY)) + geom_bar(stat="identity", position="dodge") + labs(title="Perbandingan Frekuensi", x= "Variabel X", fill="Variabel Y")
      analysis_results_p13$plot_type <- "bar"
    }
    
    analysis_results_p13$report <- report_string
    analysis_results_p13$plot_obj <- ggplotly(p_gg)
    analysis_results_p13$data <- df
    
    output$p13_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p13_results"), br(), downloadButton("p13_download_report", "Unduh Laporan (.docx)")) })
    output$p13_results <- renderText({ analysis_results_p13$report })
    output$p13_plot <- renderPlotly({ analysis_results_p13$plot_obj })
    output$p13_data_table <- DT::renderDataTable({ DT::datatable(analysis_results_p13$data, options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p13_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-Uji-Korelasi-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p13$report)
      
      plot_path <- file.path(tempdir(), "plot_p13.png")
      df_plot <- analysis_results_p13$data
      plot_type <- analysis_results_p13$plot_type
      
      p_gg_download <- if (plot_type == "scatter") {
        ggplot(df_plot, aes(x=as.numeric(X), y=as.numeric(Y))) + geom_point(color="blue") + geom_smooth(method="lm", se=FALSE, color="red") + labs(title="Scatter Plot Hubungan X dan Y")
      } else { # bar
        tbl <- table(df_plot$X, df_plot$Y)
        df_plot_bar <- as.data.frame(tbl)
        names(df_plot_bar) <- c("VarX", "VarY", "Freq")
        ggplot(df_plot_bar, aes(x=VarX, y=Freq, fill=VarY)) + geom_bar(stat="identity", position="dodge") + labs(title="Perbandingan Frekuensi", x= "Variabel X", fill="Variabel Y")
      }
      
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p13.Rmd")
      report_md <- gsub("=", "", analysis_results_p13$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis Korelasi'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 13  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 13 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 13
  quiz_questions_p13 <- list(
    list(
      id = "q1",
      question = "1. Uji korelasi Pearson paling tepat digunakan untuk mengukur...",
      choices = c("A. Hubungan sebab-akibat antara dua variabel.",
                  "B. Perbedaan rata-rata antara dua kelompok.",
                  "C. Kekuatan dan arah hubungan linier antara dua variabel numerik.",
                  "D. Asosiasi antara dua variabel kategorik."),
      answer = "C"
    ),
    list(
      id = "q2",
      question = "2. Kapan Uji Korelasi Rank Spearman lebih disarankan daripada Uji Pearson?",
      choices = c("A. Ketika ukuran sampel sangat besar.",
                  "B. Ketika kedua variabel diukur dalam skala nominal.",
                  "C. Ketika data tidak berdistribusi normal atau hubungannya bersifat monotonik tetapi tidak linier.",
                  "D. Ketika ingin mengetahui persentase variasi yang dijelaskan."),
      answer = "C"
    ),
    list(
      id = "q3",
      question = "3. Seorang peneliti mendapatkan koefisien korelasi (r) sebesar -0.85. Interpretasi yang paling tepat adalah...",
      choices = c("A. Ada hubungan positif yang sangat kuat.",
                  "B. Ada hubungan negatif yang sangat kuat.",
                  "C. Ada hubungan negatif yang lemah.",
                  "D. Tidak ada hubungan antara kedua variabel."),
      answer = "B"
    ),
    list(
      id = "q4",
      question = "4. Untuk menguji apakah ada hubungan (asosiasi) antara dua variabel kualitatif (misalnya, 'Jenis Kelamin' dan 'Pilihan Partai Politik'), uji yang paling sesuai adalah...",
      choices = c("A. Uji Korelasi Pearson",
                  "B. ANOVA",
                  "C. Uji Kebebasan Chi-Square (Chi-Square Test of Independence)",
                  "D. Uji Korelasi Kendall's Tau"),
      answer = "C"
    ),
    list(
      id = "q5",
      question = "5. Hipotesis nol (H₀) untuk uji signifikansi korelasi (baik Pearson maupun Spearman) umumnya adalah...",
      choices = c("A. H₀: Koefisien korelasi (ρ) = 1",
                  "B. H₀: Koefisien korelasi (ρ) = -1",
                  "C. H₀: Koefisien korelasi (ρ) ≠ 0",
                  "D. H₀: Koefisien korelasi (ρ) = 0"),
      answer = "D"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 13
  quiz_state_p13 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 13
  observeEvent(input$start_quiz_p13_new, {
    quiz_state_p13$status <- "in_progress"
    quiz_state_p13$user_answers <- NULL
    quiz_state_p13$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 13",
        uiOutput("quiz_ui_p13"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 13
  output$quiz_ui_p13 <- renderUI({
    if (quiz_state_p13$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p13, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p13_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p13_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p13$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p13$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p13$results_df), function(i) {
              row <- quiz_state_p13$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p13", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 13
  observeEvent(input$submit_quiz_p13_final, {
    
    user_answers <- sapply(quiz_questions_p13, function(q) {
      input[[paste0("p13_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p13$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p13, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p13[[i]]$choices[which(substr(quiz_questions_p13[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p13[[i]]$answer
        quiz_questions_p13[[i]]$choices[which(substr(quiz_questions_p13[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p13, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p13$results_df <- results_df
    quiz_state_p13$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 13
  output$download_quiz_report_p13 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-13-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p13$results_df)
      
      quiz_results <- quiz_state_p13$results_df
      tempReport <- file.path(tempdir(), "report_p13.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 13'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 13 ################

  #################### LOGIKA UNTUK PERTEMUAN 14  ####################
  analysis_results_p14 <- reactiveValues(
    report = NULL, 
    plot_obj = NULL,
    data = NULL
  )
  
  # --- Logika Input & UI ---
  p14_raw_data_file <- reactiveVal(NULL)
  observeEvent(input$p14_file, { req(input$p14_file); p14_raw_data_file(read_excel(input$p14_file$datapath, col_names = TRUE)) })
  
  p14_get_data <- reactive({
    if (input$p14_input_method == "manual") {
      req(input$p14_manual_data_value, input$p14_manual_data_factor, input$p14_manual_data_covariate)
      vals <- as.numeric(unlist(strsplit(input$p14_manual_data_value, ",")))
      facts <- trimws(unlist(strsplit(input$p14_manual_data_factor, ",")))
      covars <- as.numeric(unlist(strsplit(input$p14_manual_data_covariate, ",")))
      validate(need(length(vals) == length(facts) && length(vals) == length(covars), "Jumlah elemen di semua input manual harus sama."))
      data.frame(Value = vals, Factor = as.factor(facts), Covariate = covars)
    } else {
      req(p14_raw_data_file(), input$p14_selected_sheet, input$p14_col_value, input$p14_col_factor, input$p14_col_covariate)
      df <- read_excel(input$p14_file$datapath, sheet = input$p14_selected_sheet, col_names = TRUE)
      data.frame(
        Value = as.numeric(df[[input$p14_col_value]]),
        Factor = as.factor(df[[input$p14_col_factor]]),
        Covariate = as.numeric(df[[input$p14_col_covariate]])
      )
    }
  })
  
  output$p14_sheet_selector <- renderUI({ req(p14_raw_data_file()); selectInput("p14_selected_sheet", "Pilih Sheet:", choices = excel_sheets(input$p14_file$datapath)) })
  
  output$p14_column_selector_value <- renderUI({ 
    req(p14_raw_data_file(), input$p14_selected_sheet)
    df <- read_excel(input$p14_file$datapath, sheet = input$p14_selected_sheet)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p14_col_value", "Kolom Nilai (Dependen):", choices = numeric_cols) 
  })
  
  output$p14_column_selector_factor <- renderUI({ 
    req(p14_raw_data_file(), input$p14_selected_sheet)
    df <- read_excel(input$p14_file$datapath, sheet = input$p14_selected_sheet)
    # Tawarkan semua kolom sebagai pilihan faktor
    all_cols <- names(df)
    selectInput("p14_col_factor", "Kolom Faktor (Grup):", choices = all_cols, selected = all_cols[2]) # Default ke kolom kedua
  })
  
  output$p14_column_selector_covariate <- renderUI({ 
    req(p14_raw_data_file(), input$p14_selected_sheet)
    df <- read_excel(input$p14_file$datapath, sheet = input$p14_selected_sheet)
    # Tawarkan hanya kolom numerik sebagai pilihan kovariat
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("p14_col_covariate", "Kolom Kovariat:", choices = numeric_cols, selected = numeric_cols[3]) # Default ke kolom ketiga
  })
  
  # --- Observer Utama untuk Analisis ---
  observeEvent(input$p14_analyze, {
    df <- p14_get_data()
    df <- na.omit(df)
    alpha <- input$p14_alpha
    
    validate(need(nrow(df) > 0, "Tidak ada data yang valid."),
             need(nlevels(df$Factor) >= 2, "Faktor harus memiliki setidaknya 2 level/kelompok."))
    
    # Model ANCOVA
    ancova_model <- aov(Value ~ Covariate + Factor, data = df)
    ancova_summary <- car::Anova(ancova_model, type = "III")
    
    # Uji Asumsi
    shapiro_test_residuals <- shapiro.test(residuals(ancova_model))
    interaction_model <- aov(Value ~ Covariate * Factor, data = df)
    interaction_summary <- car::Anova(interaction_model, type = "III")
    interaction_pval <- interaction_summary["Covariate:Factor", "Pr(>F)"]
    
    # Bangun String Laporan
    report_string <- paste0(
      "==========================================\n   HASIL ANALYSIS OF COVARIANCE (ANCOVA)\n==========================================\n\n",
      "1. TUJUAN ANALISIS:\n   Menguji perbedaan rata-rata antar kelompok (Faktor) setelah mengontrol pengaruh variabel lain (Kovariat).\n\n",
      "2. RUMUSAN HIPOTESIS:\n",
      "   a) Untuk Faktor:\n      - H₀: Tidak ada perbedaan rata-rata antar kelompok setelah disesuaikan oleh kovariat.\n      - H₁: Setidaknya ada satu perbedaan rata-rata.\n",
      "   b) Untuk Kovariat:\n      - H₀: Tidak ada hubungan antara kovariat dan variabel dependen.\n      - H₁: Terdapat hubungan antara kovariat dan variabel dependen.\n\n",
      "3. UJI ASUMSI:\n",
      "   a) Normalitas Residual (Shapiro-Wilk):\n      - p-value: ", format.p(shapiro_test_residuals$p.value), ifelse(shapiro_test_residuals$p.value >= 0.05, " (Asumsi Terpenuhi)", " (Asumsi Dilanggar)"), "\n",
      "   b) Homogenitas Lereng Regresi (Uji Interaksi):\n      - p-value: ", format.p(interaction_pval), ifelse(interaction_pval >= 0.05, " (Asumsi Terpenuhi)", " (Asumsi Dilanggar)"), "\n\n",
      "4. HASIL UJI ANCOVA (Tipe III SS):\n", paste(capture.output(print(ancova_summary)), collapse="\n"), "\n"
    )
    
    # Interpretasi
    p_factor <- ancova_summary["Factor", "Pr(>F)"]
    p_covariate <- ancova_summary["Covariate", "Pr(>F)"]
    
    interpretation <- "5. KEPUTUSAN & INTERPRETASI (α = "
    interpretation <- paste0(interpretation, alpha, "):\n")
    interpretation <- paste0(interpretation, "   - Pengaruh Kovariat: ", ifelse(p_covariate < alpha, "SIGNIFIKAN (Tolak H₀). Kovariat memiliki pengaruh signifikan terhadap variabel dependen.", "TIDAK SIGNIFIKAN (Gagal Tolak H₀)."), "\n")
    interpretation <- paste0(interpretation, "   - Pengaruh Faktor: ", ifelse(p_factor < alpha, "SIGNIFIKAN (Tolak H₀). Terdapat perbedaan rata-rata yang signifikan antar kelompok setelah mengontrol kovariat.", "TIDAK SIGNIFIKAN (Gagal Tolak H₀)."), "\n")
    
    report_string <- paste0(report_string, interpretation)
    
    # Uji Lanjut
    if (p_factor < alpha && input$p14_posthoc_test == "tukey") {
      emmeans_obj <- emmeans::emmeans(ancova_model, ~ Factor)
      tukey_res <- pairs(emmeans_obj)
      report_string <- paste0(report_string, "\n\n6. HASIL UJI LANJUT (POST-HOC) TUKEY PADA ADJUSTED MEANS:\n", paste(capture.output(print(tukey_res)), collapse="\n"))
    }
    
    # Plotting
    p_gg <- ggplot(df, aes(x = Covariate, y = Value, color = Factor)) + 
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Hubungan Dependen vs Kovariat per Kelompok Faktor", x = "Kovariat", y = "Nilai Dependen")
    
    analysis_results_p14$report <- report_string
    analysis_results_p14$plot_obj <- ggplotly(p_gg)
    analysis_results_p14$data <- df
    
    output$p14_report_output_ui <- renderUI({ tagList(verbatimTextOutput("p14_results"), br(), downloadButton("p14_download_report", "Unduh Laporan (.docx)")) })
    output$p14_results <- renderText({ analysis_results_p14$report })
    output$p14_plot <- renderPlotly({ analysis_results_p14$plot_obj })
    output$p14_data_table <- DT::renderDataTable({ DT::datatable(analysis_results_p14$data, options = list(pageLength = 5)) })
  })
  
  # Download Handler untuk Laporan
  output$p14_download_report <- downloadHandler(
    filename = function() { paste0("Laporan-ANCOVA-", Sys.Date(), ".docx") },
    content = function(file) {
      req(analysis_results_p14$report)
      
      plot_path <- file.path(tempdir(), "plot_p14.png")
      df_plot <- analysis_results_p14$data
      p_gg_download <- ggplot(df_plot, aes(x = Covariate, y = Value, color = Factor)) + 
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Hubungan Dependen vs Kovariat per Kelompok Faktor", x = "Kovariat", y = "Nilai Dependen")
      ggsave(plot_path, plot = p_gg_download, device = "png")
      
      tempReport <- file.path(tempdir(), "report_p14.Rmd")
      report_md <- gsub("=", "", analysis_results_p14$report)
      
      write("---", tempReport); write("title: 'Laporan Hasil Analisis ANCOVA'", tempReport, append = TRUE); write("output: word_document", tempReport, append = TRUE); write("---", tempReport, append = TRUE)
      write(report_md, tempReport, append = TRUE)
      write("\n\n## VISUALISASI DATA\n", tempReport, append = TRUE)
      write(paste0("![](", plot_path, ")"), tempReport, append = TRUE)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  #################### LOGIKA UNTUK PERTEMUAN 14  ####################
  
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 14 ################
  # 1. Definisikan Pertanyaan dan Jawaban untuk Pertemuan 14
  quiz_questions_p14 <- list(
    list(
      id = "q1",
      question = "1. Apa keuntungan utama menggunakan ANCOVA dibandingkan ANOVA biasa?",
      choices = c("A. ANCOVA dapat digunakan untuk data non-normal.",
                  "B. ANCOVA dapat menangani lebih dari dua variabel faktor.",
                  "C. ANCOVA secara statistik mengontrol efek variabel perancu (kovariat), sehingga meningkatkan presisi.",
                  "D. ANCOVA tidak memerlukan asumsi homogenitas varians."),
      answer = "C"
    ),
    list(
      id = "q2",
      question = "2. Dalam konteks ANCOVA, apa yang dimaksud dengan 'kovariat'?",
      choices = c("A. Variabel dependen utama yang diukur.",
                  "B. Variabel faktor kategorikal yang menjadi fokus utama penelitian.",
                  "C. Variabel kontinu yang tidak termasuk dalam desain eksperimen tetapi berpotensi memengaruhi variabel dependen.",
                  "D. Hasil dari uji post-hoc."),
      answer = "C"
    ),
    list(
      id = "q3",
      question = "3. Manakah dari berikut ini yang merupakan asumsi penting dari ANCOVA?",
      choices = c("A. Tidak boleh ada hubungan antara kovariat dan variabel dependen.",
                  "B. Hubungan antara kovariat dan variabel dependen harus linier.",
                  "C. Kovariat harus berkorelasi kuat dengan variabel faktor (perlakuan).",
                  "D. Variabel dependen harus berskala ordinal."),
      answer = "B"
    ),
    list(
      id = "q4",
      question = "4. Jika hasil ANCOVA menunjukkan bahwa kovariat (misalnya, skor pre-test) signifikan secara statistik, apa artinya?",
      choices = c("A. Rata-rata antar kelompok perlakuan pasti berbeda secara signifikan.",
                  "B. Skor pre-test memiliki pengaruh yang signifikan terhadap variabel dependen, dan model telah berhasil mengontrol pengaruh ini.",
                  "C. Asumsi homogenitas varians telah dilanggar.",
                  "D. Tidak ada efek interaksi antara faktor dan kovariat."),
      answer = "B"
    ),
    list(
      id = "q5",
      question = "5. Anda ingin membandingkan efektivitas tiga metode pengajaran (variabel faktor) terhadap skor ujian akhir (variabel dependen), tetapi Anda tahu bahwa skor IQ siswa (variabel kontinu) juga memengaruhi hasil ujian. Metode analisis yang paling tepat adalah...",
      choices = c("A. ANOVA Satu Arah",
                  "B. ANOVA Dua Arah",
                  "C. Regresi Linier Berganda",
                  "D. ANCOVA"),
      answer = "D"
    )
  )
  
  # 2. Gunakan reactiveValues untuk melacak status kuis Pertemuan 14
  quiz_state_p14 <- reactiveValues(
    status = "not_started",
    user_answers = NULL,
    results_df = NULL
  )
  
  # 3. Observer untuk tombol "Kerjakan Kuis" Pertemuan 14
  observeEvent(input$start_quiz_p14_new, {
    quiz_state_p14$status <- "in_progress"
    quiz_state_p14$user_answers <- NULL
    quiz_state_p14$results_df <- NULL
    
    showModal(
      modalDialog(
        class = "quiz-modal",
        title = "Kuis Latihan: Pertemuan 14",
        uiOutput("quiz_ui_p14"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  # 4. UI Dinamis untuk Modal Kuis Pertemuan 14
  output$quiz_ui_p14 <- renderUI({
    if (quiz_state_p14$status == "in_progress") {
      tagList(
        lapply(quiz_questions_p14, function(q) {
          div(class = "quiz-question",
              div(class = "control-label",
                  tags$b(q$question)
              ),
              radioButtons(inputId = paste0("p14_", q$id), 
                           label = NULL,
                           choiceNames = q$choices,
                           choiceValues = substr(q$choices, 1, 1),
                           selected = character(0))
          )
        }),
        div(style = "text-align: center; margin-top: 20px;",
            actionButton("submit_quiz_p14_final", "Kumpulkan Jawaban!", icon = icon("paper-plane"), class = "btn-success btn-lg")
        )
      )
    } else if (quiz_state_p14$status == "finished") {
      tagList(
        div(class = "quiz-results-panel",
            h2("Hasil Kuis Anda"),
            h3(paste0("Total Nilai: ", sum(quiz_state_p14$results_df$Status == "Benar"), " / 5")),
            hr(),
            lapply(1:nrow(quiz_state_p14$results_df), function(i) {
              row <- quiz_state_p14$results_df[i,]
              div(class = paste("result-item", ifelse(row$Status == "Benar", "correct", "incorrect")),
                  p(strong(paste0("Soal ", row$`Nomor Soal`, ":")), " ", row$Pertanyaan),
                  p(strong("Jawaban Anda: "), tags$span(style="color: #007bff;", row$`Jawaban Peserta`)),
                  p(strong("Jawaban Benar: "), tags$span(style="color: #28a745;", row$`Jawaban Benar`))
              )
            }),
            hr(),
            p("Unduh laporan hasil kuis Anda dalam format Word."),
            downloadButton("download_quiz_report_p14", "Unduh Laporan Word", class = "btn-primary")
        )
      )
    }
  })
  
  # 5. Observer untuk tombol "Kumpulkan Jawaban" Pertemuan 14
  observeEvent(input$submit_quiz_p14_final, {
    
    user_answers <- sapply(quiz_questions_p14, function(q) {
      input[[paste0("p14_", q$id)]]
    })
    
    if (any(sapply(user_answers, is.null))) {
      showNotification("Harap jawab semua pertanyaan terlebih dahulu!", type = "warning")
      return()
    }
    
    quiz_state_p14$user_answers <- user_answers
    
    results_df <- data.frame(
      `Nomor Soal` = 1:5,
      `Pertanyaan` = sapply(quiz_questions_p14, function(q) q$question),
      `Jawaban Peserta` = sapply(1:5, function(i) {
        ans_char <- user_answers[i]
        full_choice <- quiz_questions_p14[[i]]$choices[which(substr(quiz_questions_p14[[i]]$choices, 1, 1) == ans_char)]
        ifelse(length(full_choice) > 0, full_choice, "Tidak Dijawab")
      }),
      `Jawaban Benar` = sapply(1:5, function(i) {
        ans_char <- quiz_questions_p14[[i]]$answer
        quiz_questions_p14[[i]]$choices[which(substr(quiz_questions_p14[[i]]$choices, 1, 1) == ans_char)]
      }),
      `Status` = ifelse(user_answers == sapply(quiz_questions_p14, `[[`, "answer"), "Benar", "Salah"),
      check.names = FALSE
    )
    
    quiz_state_p14$results_df <- results_df
    quiz_state_p14$status <- "finished"
  })
  
  # 6. Download Handler untuk laporan WORD (.docx) Pertemuan 14
  output$download_quiz_report_p14 <- downloadHandler(
    filename = function() {
      paste0("Laporan-Kuis-Pertemuan-14-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(quiz_state_p14$results_df)
      
      quiz_results <- quiz_state_p14$results_df
      tempReport <- file.path(tempdir(), "report_p14.Rmd")
      
      write("---", tempReport)
      write("title: 'Laporan Hasil Kuis: Pertemuan 14'", tempReport, append = TRUE)
      write("author: 'AHA Analytics Dashboard'", tempReport, append = TRUE)
      write(paste("date:", Sys.Date()), tempReport, append = TRUE)
      write("output: word_document", tempReport, append = TRUE)
      write("---", tempReport, append = TRUE)
      
      total_score <- sum(quiz_results$Status == "Benar")
      write(paste0("\n## Total Nilai: **", total_score, " / 5**\n"), tempReport, append = TRUE)
      
      report_table_data <- quiz_results[, -which(names(quiz_results) == "Status")]
      
      write("\n```{r, echo=FALSE}", tempReport, append = TRUE)
      write("knitr::kable(report_table_data, caption = 'Detail Jawaban Kuis', col.names = c('No.', 'Pertanyaan', 'Jawaban Anda', 'Jawaban Benar'))", tempReport, append = TRUE)
      write("```\n", tempReport, append = TRUE)
      
      render_env <- new.env(parent = globalenv())
      render_env$report_table_data <- report_table_data
      
      showNotification("Sedang membuat laporan Word...", duration = 3, type = "message")
      rmarkdown::render(
        tempReport, 
        output_file = file,
        envir = render_env
      )
    }
  )
  #################### LOGIKAUNTUK KUIS INTERAKTIF PERTEMUAN 14 ################
}

shinyApp(ui, server)
