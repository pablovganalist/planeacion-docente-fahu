# =============================================================================
# 04_exportar_html.R
# Convierte los informes .md a HTML y genera un index navegable
# =============================================================================

library(rmarkdown)
library(tidyverse)
library(glue)
library(readr)

source("00_base_utils.R", local = TRUE)

CARPETA_MD   <- if (exists("CARPETA_MD")) CARPETA_MD else "informes_md"
CARPETA_HTML <- if (exists("CARPETA_HTML")) CARPETA_HTML else "docs"
PERIODO      <- if (exists("PERIODO")) PERIODO else "Primer semestre 2026"
INSTITUCION  <- if (exists("INSTITUCION")) INSTITUCION else "Facultad de Humanidades"

C_TEAL   <- "#00A499"
C_ORANGE <- "#EA7600"
C_DARK   <- "#394049"
C_BLUE   <- "#498BCA"
C_GOLD   <- "#EAAA00"

css_institucional <- glue('
body {{
  font-family: "Segoe UI", Arial, sans-serif;
  font-size: 13px;
  line-height: 1.65;
  color: {C_DARK};
  max-width: 980px;
  margin: 0 auto;
  padding: 2rem 1.25rem 4rem;
  background: linear-gradient(180deg, #eef7f6 0%, #f6f7f8 220px);
}}
h2 {{
  color: {C_DARK};
  font-size: 1.15rem;
  font-weight: 700;
  border-left: 5px solid {C_ORANGE};
  padding-left: 0.6rem;
  margin-top: 2rem;
}}
h3, h3.carr-titulo {{
  color: {C_BLUE};
  font-size: 1.05rem;
  font-weight: 700;
  margin-top: 2rem;
  margin-bottom: 0.2rem;
}}
table {{
  border-collapse: collapse;
  width: 100%;
  margin: 1rem 0 1.5rem;
  font-size: 12.5px;
}}
thead tr {{
  background-color: {C_DARK};
  color: white;
}}
thead th {{
  padding: 7px 12px;
  text-align: left;
  font-weight: 600;
}}
tbody tr:nth-child(even) {{
  background-color: #F5F7F8;
}}
tbody tr:hover {{
  background-color: #E8F8F7;
}}
tbody td {{
  padding: 5px 12px;
  border-bottom: 1px solid #E0E0E0;
}}
tbody tr:has(td > strong) {{
  background-color: #FFF3CD;
  border-top: 2px solid {C_GOLD};
  border-bottom: 2px solid {C_GOLD};
}}
tbody tr:has(td > strong) td {{
  font-weight: 700;
  color: {C_DARK};
}}
tbody td:has(> span.alerta) {{
  color: #C8102E;
  font-weight: 700;
}}
blockquote {{
  border-left: 4px solid {C_TEAL};
  background: #E8F8F7;
  margin: 1rem 0;
  padding: 0.5rem 1rem;
  border-radius: 0 4px 4px 0;
  color: {C_DARK};
}}
blockquote p {{ margin: 0; }}
header {{
  border-top: 6px solid {C_TEAL};
  padding: 1.5rem 1.5rem 1.2rem;
  margin-bottom: 1.25rem;
  background: #fff;
  border-radius: 18px;
  box-shadow: 0 18px 40px rgba(57, 64, 73, 0.08);
}}
p.facultad {{
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: {C_TEAL};
  margin: 0 0 0.2rem 0;
}}
h1 {{
  color: {C_DARK};
  font-size: 1.65rem;
  font-weight: 700;
  margin: 0 0 0.3rem 0;
  border: none;
  padding: 0;
}}
.periodo {{
  color: #666;
  font-size: 0.95rem;
  margin: 0 0 1rem 0;
}}
.cod-programa {{
  font-size: 0.85rem;
  color: {C_DARK};
  background-color: #F0F0F0;
  border-left: 4px solid {C_BLUE};
  padding: 0.3rem 0.8rem;
  margin-top: -0.8rem;
  margin-bottom: 0.8rem;
  font-family: "Courier New", monospace;
  letter-spacing: 0.03em;
}}
p.cod-programa {{
  font-size: 0.82rem;
  color: {C_DARK};
  background: #F0F2F4;
  border-left: 4px solid {C_BLUE};
  padding: 0.3rem 0.8rem;
  margin-top: -0.6rem;
  margin-bottom: 0.9rem;
  font-family: "Courier New", monospace;
  letter-spacing: 0.02em;
}}
.pie {{
  margin-top: 3rem;
  padding-top: 0.8rem;
  border-top: 2px solid {C_TEAL};
  font-size: 11px;
  color: #666;
}}
.contenido {{
  background: #fff;
  border-radius: 18px;
  box-shadow: 0 18px 40px rgba(57, 64, 73, 0.08);
  padding: 1.5rem;
}}
.contenido > h2:first-child {{
  margin-top: 0;
}}
.contenido hr {{
  border: 0;
  border-top: 1px solid #e4e7ea;
  margin: 1.35rem 0;
}
.botones-header {{
  display: flex;
  gap: 0.6rem;
  flex-wrap: wrap;
  margin-top: 0.6rem;
}}
.btn-comp {{
  display: inline-block;
  padding: 0.38rem 0.9rem;
  background: {C_BLUE};
  color: white;
  border-radius: 4px;
  font-size: 0.78rem;
  font-weight: 600;
  text-decoration: none;
}}
.btn-comp:hover {{ background: #3a7ab8; }}
.btn-indice {{
  display: inline-block;
  padding: 0.38rem 0.9rem;
  background: #e9ecef;
  color: {C_DARK};
  border-radius: 4px;
  font-size: 0.78rem;
  font-weight: 600;
  text-decoration: none;
}}
.btn-pdf {{
  display: inline-block;
  padding: 0.45rem 1.1rem;
  background: {C_TEAL};
  color: white !important;
  font-size: 0.85rem;
  font-weight: 600;
  border-radius: 4px;
  text-decoration: none;
  cursor: pointer;
  border: none;
  letter-spacing: 0.03em;
}}
.btn-pdf:hover {{
  background: {C_DARK};
}}
tr.acad-row {{ cursor: pointer; transition: background .15s; }}
tr.acad-row:hover {{ background: #E8F8F7 !important; }}
tr.acad-row.abierto {{ background: #E8F8F7 !important; font-weight: 600; }}
td.td-nombre {{ display: flex; align-items: center; gap: .5rem; }}
.flecha {{
  display: inline-block; width: 14px; font-size: 10px;
  color: {C_TEAL}; transition: transform .2s; flex-shrink: 0;
}}
tr.acad-row.abierto .flecha {{ transform: rotate(90deg); }}
tr.detalle-row {{ display: none; }}
tr.detalle-row.visible {{ display: table-row; }}
tr.detalle-row > td {{
  padding: 0;
  background: #f4fbfb;
  border-bottom: 2px solid {C_TEAL};
}}
.detalle-inner {{
  padding: .5rem 1rem .7rem 2.4rem;
}}
.detalle-inner table {{
  font-size: 11.5px; width: auto; min-width: 480px;
  margin: 0; border-collapse: collapse;
}}
.detalle-inner thead tr {{ background: #555; }}
.detalle-inner thead th {{ padding: 4px 10px; font-weight: 500; color: white; text-align: left; }}
.detalle-inner thead th.num {{ text-align: right; }}
.detalle-inner tbody tr:nth-child(even) {{ background: #eef7f7; }}
.detalle-inner tbody tr:hover {{ background: #d6f0ef; }}
.detalle-inner tbody td {{ padding: 3px 10px; }}
.detalle-inner tbody td.num {{ text-align: right; }}
.detalle-total {{
  font-size: 11px; color: #555; margin-top: .35rem;
}}
@media print {{
  body {{ max-width: 100%; padding: 1cm; font-size: 11px; background: #fff; }}
  h2 {{ page-break-before: auto; }}
  table {{ font-size: 10px; }}
  tbody tr:has(td > strong) {{ background-color: #FFF3CD !important; }}
  .btn-pdf {{ display: none; }}
  p.cod-programa {{ background: #eee !important; }}
  tr.detalle-row {{ display: table-row !important; }}
  .flecha {{ display: none; }}
  header, .contenido {{ box-shadow: none; border-radius: 0; }}
}}
')

strip_front_matter <- function(lineas) {
  if (length(lineas) < 2 || !identical(lineas[[1]], "---")) {
    return(lineas)
  }

  cierre <- which(lineas[-1] == "---")[1]
  if (is.na(cierre)) {
    return(lineas)
  }

  salida <- lineas[-seq_len(cierre + 1)]
  while (length(salida) > 0 && salida[[1]] == "") {
    salida <- salida[-1]
  }
  salida
}

convertir_markdown_a_html <- function(md_texto, ruta_md) {
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(commonmark::markdown_html(md_texto, extensions = TRUE))
  }

  if (rmarkdown::pandoc_available()) {
    tmp_md <- tempfile(fileext = ".md")
    tmp_html <- tempfile(fileext = ".html")
    writeLines(md_texto, tmp_md, useBytes = TRUE)
    rmarkdown::pandoc_convert(
      input = tmp_md,
      to = "html",
      output = tmp_html,
      options = character(0)
    )
    html <- paste(readLines(tmp_html, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    unlink(c(tmp_md, tmp_html))
    return(html)
  }

  stop(glue(
    "No hay convertidor Markdown disponible para '{basename(ruta_md)}'. ",
    "Instala 'commonmark' o ejecuta el pipeline en un entorno con pandoc."
  ))
}

leer_manifest <- function(carpeta_md) {
  ruta <- file.path(carpeta_md, "_manifest_informes.csv")
  if (!file.exists(ruta)) {
    stop(glue("No existe el manifiesto de informes en '{ruta}'. Ejecuta primero el script 02."))
  }
  readr::read_csv(ruta, show_col_types = FALSE)
}

md_a_html <- function(ruta_md, ruta_html, css, fila_manifest) {
  md_lineas <- readLines(ruta_md, encoding = "UTF-8", warn = FALSE)
  md_lineas <- strip_front_matter(md_lineas)
  md_texto <- paste(md_lineas, collapse = "\n")

  titulo <- fila_manifest$nombre_largo[[1]]
  slug_comp <- fila_manifest$slug[[1]]

  html_raw <- convertir_markdown_a_html(md_texto, ruta_md)
  html_body <- html_raw |>
    str_replace_all("<h1[^>]*>.*?</h1>", "")

  botones_header <- paste0(
    '<div class="botones-header">',
    '<a href="index.html" class="btn-indice">← Índice</a> ',
    '<a href="comparativos/', slug_comp, '.html" class="btn-comp">📊 Informe comparado 2024–2026</a> ',
    '<button class="btn-pdf" onclick="window.print()">⬇ Descargar PDF</button>',
    '</div>'
  )

  header_html <- paste0(
    '<header>\n',
    '<p class="facultad">', INSTITUCION, '</p>\n',
    '<h1>', titulo, '</h1>\n',
    '<p class="periodo">', PERIODO, '</p>\n',
    botones_header, '\n',
    '</header>'
  )

  script_html <- '
<script>
function toggleAcad(row) {
  const detail = row.nextElementSibling;
  if (!detail || !detail.classList.contains("detalle-row")) return;
  row.classList.toggle("abierto");
  detail.classList.toggle("visible");
}
</script>'

  html_final <- paste0(
    '<!DOCTYPE html>\n<html lang="es">\n<head>\n',
    '  <meta charset="UTF-8">\n',
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
    '  <title>', titulo, ' — ', PERIODO, '</title>\n',
    '  <style>\n', css, '\n  </style>\n',
    '</head>\n<body>\n',
    header_html, '\n<main class="contenido">\n', html_body, '\n</main>\n',
    '<div class="pie">Base elaborada a partir de los registros de planeación docente. Código elaborado por Pablo Valenzuela con apoyo de Claude de Anthropic · ', PERIODO, '</div>\n',
    script_html, '\n',
    '</body>\n</html>\n'
  )

  writeLines(html_final, ruta_html, useBytes = TRUE)
}

crear_index <- function(manifest, carpeta_html) {
  botones_informes <- manifest |>
    mutate(
      boton = glue(
        '<a class="unidad-btn" href="{html_file}"><span class="unidad-nombre">{nombre_indice}</span><span class="unidad-link">Abrir informe</span></a>'
      )
    ) |>
    pull(boton) |>
    paste(collapse = "\n")

  fecha_gen <- format(Sys.time(), "%d/%m/%Y %H:%M")

  index_html <- glue('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Planeación docente — {PERIODO}</title>
  <style>
    :root {{
      --teal: {C_TEAL};
      --orange: {C_ORANGE};
      --dark: {C_DARK};
      --blue: {C_BLUE};
      --gold: {C_GOLD};
      --bg: #f6f7f8;
      --card: #ffffff;
    }}
    body {{
      font-family: "Segoe UI", Arial, sans-serif;
      margin: 0;
      background: linear-gradient(180deg, #eef7f6 0%, var(--bg) 220px);
      color: var(--dark);
    }}
    .wrap {{
      max-width: 1080px;
      margin: 0 auto;
      padding: 2.5rem 1.25rem 4rem;
    }}
    .hero {{
      background: var(--card);
      border-radius: 18px;
      padding: 1.5rem 1.5rem 1.2rem;
      box-shadow: 0 18px 40px rgba(57, 64, 73, 0.08);
      border-top: 6px solid var(--teal);
    }}
    .eyebrow {{
      margin: 0;
      text-transform: uppercase;
      letter-spacing: 0.12em;
      font-size: 0.78rem;
      color: var(--teal);
      font-weight: 700;
    }}
    h1 {{
      margin: 0.35rem 0 0.4rem;
      font-size: 2rem;
    }}
    .sub {{
      margin: 0;
      color: #66707a;
    }}
    .tabs {{
      display: flex;
      gap: 0.75rem;
      flex-wrap: wrap;
      margin: 1.4rem 0 0.6rem;
    }}
    .tab-btn {{
      border: none;
      border-radius: 999px;
      background: #dfe8e7;
      color: var(--dark);
      padding: 0.75rem 1rem;
      font-size: 0.95rem;
      font-weight: 700;
      cursor: pointer;
    }}
    .tab-btn.active {{
      background: var(--orange);
      color: white;
    }}
    .panel {{
      display: none;
      margin-top: 1.25rem;
      background: var(--card);
      border-radius: 18px;
      padding: 1.4rem;
      box-shadow: 0 18px 40px rgba(57, 64, 73, 0.08);
    }}
    .panel.active {{
      display: block;
    }}
    .panel h2 {{
      margin-top: 0;
      font-size: 1.1rem;
    }}
    .grid {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
      gap: 0.9rem;
      margin-top: 1rem;
    }}
    .unidad-btn, .cta {{
      display: block;
      text-decoration: none;
      background: linear-gradient(135deg, rgba(0,164,153,0.10), rgba(73,139,202,0.08));
      border: 1px solid rgba(0,164,153,0.15);
      border-radius: 14px;
      padding: 1rem;
      color: var(--dark);
      min-height: 88px;
    }}
    .unidad-btn:hover, .cta:hover {{
      border-color: var(--teal);
      transform: translateY(-1px);
    }}
    .unidad-nombre {{
      display: block;
      font-weight: 800;
      font-size: 0.95rem;
      line-height: 1.35;
      letter-spacing: 0.02em;
    }}
    .unidad-link {{
      display: block;
      margin-top: 0.45rem;
      color: #64707a;
      font-size: 0.82rem;
      font-weight: 600;
    }}
    .cta strong {{
      display: block;
      margin-bottom: 0.35rem;
      color: var(--blue);
    }}
    .about {{
      display: grid;
      gap: 0.8rem;
    }}
    .about-card {{
      background: #fbfbfc;
      border-left: 4px solid var(--gold);
      border-radius: 10px;
      padding: 0.9rem 1rem;
    }}
    .foot {{
      margin-top: 1.5rem;
      color: #6f7780;
      font-size: 0.82rem;
    }}
    @media (max-width: 700px) {{
      .hero, .panel {{ padding: 1.1rem; }}
      h1 {{ font-size: 1.55rem; }}
      .tab-btn {{ width: 100%; text-align: left; }}
    }}
  </style>
</head>
<body>
  <div class="wrap">
    <section class="hero">
      <p class="eyebrow">{INSTITUCION}</p>
      <h1>Planeación docente</h1>
      <p class="sub">{PERIODO}</p>
      <div class="tabs">
        <button class="tab-btn active" data-tab="informes">Informes 1-2026</button>
        <button class="tab-btn" data-tab="sintesis">Síntesis por carrera</button>
        <button class="tab-btn" data-tab="acerca">Acerca</button>
      </div>
    </section>

    <section class="panel active" id="tab-informes">
      <h2>Informes por unidad</h2>
      <div class="grid">
        {botones_informes}
      </div>
    </section>

    <section class="panel" id="tab-sintesis">
      <h2>Síntesis por carrera y programa</h2>
      <a class="cta" href="sintesis_programas.html">
        <strong>Abrir síntesis</strong>
        Accede a la vista consolidada por carrera, programa y nivel.
      </a>
    </section>

    <section class="panel" id="tab-acerca">
      <h2>Metodología</h2>
      <div class="about">
        <div class="about-card">La base se homologa a partir de <code>BASE_FAHU.xlsx</code>, normalizando nombres de columnas y tipos de datos antes del análisis.</div>
        <div class="about-card">Las horas del claustro se calculan por <strong>unidad del profesor</strong>, no por la unidad donde se dicta el curso. Los sellos se deduplican por profesor y sección.</div>
        <div class="about-card">Las autoridades suman horas a la unidad, pero no entran al divisor del promedio del claustro. Las secciones sin profesor y las inconsistencias TELS se reportan aparte.</div>
        <div class="about-card">La síntesis y los comparativos se generan a partir del mismo conjunto limpio, para mantener consistencia entre informes por unidad, comparativos históricos y salidas LaTeX.</div>
      </div>
    </section>

    <p class="foot">Generado el {fecha_gen} · {nrow(manifest)} informes por unidad.</p>
  </div>
  <script>
    const buttons = document.querySelectorAll(".tab-btn");
    const panels = document.querySelectorAll(".panel");
    buttons.forEach((button) => {{
      button.addEventListener("click", () => {{
        buttons.forEach((b) => b.classList.remove("active"));
        panels.forEach((p) => p.classList.remove("active"));
        button.classList.add("active");
        document.getElementById("tab-" + button.dataset.tab).classList.add("active");
      }});
    }});
  </script>
</body>
</html>')

  writeLines(index_html, file.path(carpeta_html, "index.html"), useBytes = TRUE)
}

if (!dir.exists(CARPETA_HTML)) dir.create(CARPETA_HTML, recursive = TRUE)
unlink(list.files(CARPETA_HTML, pattern = "^[0-9]{2}_.*\\.html$", full.names = TRUE))

manifest <- leer_manifest(CARPETA_MD)
archivos_md <- file.path(CARPETA_MD, manifest$md_file)

if (!all(file.exists(archivos_md))) {
  faltan <- manifest$md_file[!file.exists(archivos_md)]
  stop(glue("Faltan informes .md para exportar: {paste(faltan, collapse = ', ')}"))
}

message(glue("Convirtiendo {nrow(manifest)} archivos a HTML..."))

generados <- character(0)
for (i in seq_len(nrow(manifest))) {
  fila_manifest <- manifest[i, , drop = FALSE]
  ruta_md <- file.path(CARPETA_MD, fila_manifest$md_file[[1]])
  ruta_html <- file.path(CARPETA_HTML, fila_manifest$html_file[[1]])

  message(glue("  [{i}/{nrow(manifest)}] {basename(ruta_md)}..."))
  md_a_html(ruta_md, ruta_html, css_institucional, fila_manifest)
  generados <- c(generados, basename(ruta_html))
  message(glue("    OK {basename(ruta_html)}"))
}

crear_index(manifest, CARPETA_HTML)

cat("\n=====================================================\n")
cat("  HTML GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Carpeta: {CARPETA_HTML}/\n"))
cat(glue("  Total:   {length(generados)} archivo(s)\n\n"))
