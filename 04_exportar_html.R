# =============================================================================
# 04_exportar_html.R
# Convierte los informes .md a HTML y genera un index navegable
# =============================================================================

library(rmarkdown)
library(tidyverse)
library(glue)
library(commonmark)

CARPETA_MD   <- if (exists("CARPETA_MD")) CARPETA_MD else "informes_md"
CARPETA_HTML <- if (exists("CARPETA_HTML")) CARPETA_HTML else "docs"
PERIODO      <- if (exists("PERIODO")) PERIODO else ""
INSTITUCION  <- if (exists("INSTITUCION")) INSTITUCION else "Facultad de Humanidades"

C_TEAL   <- "#00A499"
C_ORANGE <- "#EA7600"
C_DARK   <- "#394049"
C_BLUE   <- "#498BCA"

css_institucional <- glue('
body {{
  font-family: "Segoe UI", Arial, sans-serif;
  font-size: 13px;
  line-height: 1.65;
  color: {C_DARK};
  max-width: 900px;
  margin: 0 auto;
  padding: 2rem 2.5rem 4rem;
  background: #fff;
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
tbody td {{
  padding: 5px 12px;
  border-bottom: 1px solid #E0E0E0;
}}
blockquote {{
  border-left: 4px solid {C_TEAL};
  background: #E8F8F7;
  margin: 1rem 0;
  padding: 0.5rem 1rem;
  border-radius: 0 4px 4px 0;
}}
header {{
  border-bottom: 3px solid {C_TEAL};
  padding-bottom: 1rem;
  margin-bottom: 2rem;
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
}}
.periodo {{
  color: #666;
  font-size: 0.95rem;
  margin: 0 0 1rem 0;
}}
.btn-pdf {{
  display: inline-block;
  margin-bottom: 1.5rem;
  padding: 0.45rem 1.1rem;
  background: {C_TEAL};
  color: white !important;
  font-size: 0.85rem;
  font-weight: 600;
  border-radius: 4px;
  text-decoration: none;
  cursor: pointer;
  border: none;
}}
.pie {{
  margin-top: 3rem;
  padding-top: 0.8rem;
  border-top: 2px solid {C_TEAL};
  font-size: 11px;
  color: #666;
}}
@media print {{
  .btn-pdf {{ display: none; }}
}}
')

md_a_html <- function(ruta_md, ruta_html, css) {
  md_texto <- readLines(ruta_md, encoding = "UTF-8", warn = FALSE)
  linea_h1 <- md_texto[str_detect(md_texto, "^# ")]
  titulo <- if (length(linea_h1) > 0) {
    str_remove(linea_h1[1], "^# ")
  } else {
    basename(tools::file_path_sans_ext(ruta_md))
  }

  html_raw <- commonmark::markdown_html(
    paste(md_texto, collapse = "\n"),
    extensions = TRUE
  )

  html_body <- html_raw |>
    str_replace_all("<h1[^>]*>.*?</h1>", "")

  header_html <- paste0(
    '<header>\n',
    '<p class="facultad">', INSTITUCION, '</p>\n',
    '<h1>', titulo, '</h1>\n',
    '<p class="periodo">', PERIODO, '</p>\n',
    '<button class="btn-pdf" onclick="window.print()">Descargar PDF</button>\n',
    '</header>'
  )

  html_final <- paste0(
    '<!DOCTYPE html>\n<html lang="es">\n<head>\n',
    '  <meta charset="UTF-8">\n',
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
    '  <title>', titulo, ' - ', PERIODO, '</title>\n',
    '  <style>\n', css, '\n  </style>\n',
    '</head>\n<body>\n',
    header_html, '\n<main>\n', html_body, '\n</main>\n',
    '<div class="pie">Base elaborada a partir de los registros de planeacion docente.</div>\n',
    '</body>\n</html>\n'
  )

  writeLines(html_final, ruta_html, useBytes = TRUE)
}

if (!dir.exists(CARPETA_HTML)) dir.create(CARPETA_HTML, recursive = TRUE)
unlink(list.files(CARPETA_HTML, pattern = "^[0-9]{2}_.*\\.html$", full.names = TRUE))

archivos_md <- list.files(CARPETA_MD, pattern = "\\.md$", full.names = TRUE)
if (length(archivos_md) == 0) {
  stop(glue("No hay .md en '{CARPETA_MD}/'. Ejecuta primero el script 02."))
}

message(glue("Convirtiendo {length(archivos_md)} archivos a HTML..."))

generados <- character(0)
for (i in seq_along(archivos_md)) {
  ruta_md <- archivos_md[[i]]
  nombre <- tools::file_path_sans_ext(basename(ruta_md))
  ruta_html <- file.path(CARPETA_HTML, paste0(nombre, ".html"))

  message(glue("  [{i}/{length(archivos_md)}] {basename(ruta_md)}..."))
  md_a_html(ruta_md, ruta_html, css_institucional)
  generados <- c(generados, basename(ruta_html))
  message(glue("    OK {basename(ruta_html)}"))
}

leer_titulo_unidad <- function(ruta_md) {
  lineas <- readLines(ruta_md, n = 5, encoding = "UTF-8", warn = FALSE)
  linea <- lineas[str_detect(lineas, "^# ")]
  if (length(linea) > 0) str_remove(linea[1], "^# ") else basename(ruta_md)
}

titulos <- map_chr(archivos_md, leer_titulo_unidad)
items_html <- map2_chr(generados, titulos, function(archivo, titulo) {
  glue('    <li><a href="{archivo}">{titulo}</a></li>')
}) |>
  paste(collapse = "\n")

fecha_gen <- format(Sys.time(), "%d/%m/%Y %H:%M")
index_html <- glue('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Informes de planeacion docente - {PERIODO}</title>
  <style>
    body {{ font-family: "Segoe UI", Arial, sans-serif; max-width: 700px; margin: 3rem auto; padding: 0 2rem; color: #222; }}
    h1 {{ color: {C_TEAL}; border-bottom: 3px solid {C_TEAL}; padding-bottom: 0.4rem; }}
    .subtitulo {{ color: #555; margin-top: -0.5rem; margin-bottom: 2rem; }}
    ul {{ list-style: none; padding: 0; }}
    li {{ margin: 0.5rem 0; }}
    a {{ color: {C_BLUE}; text-decoration: none; display: block; padding: 0.6rem 1rem; border-left: 4px solid #E8F8F7; border-radius: 0 4px 4px 0; }}
    a:hover {{ background: #E8F8F7; border-left-color: {C_TEAL}; color: {C_TEAL}; }}
    .pie {{ margin-top: 3rem; font-size: 11px; color: #777; border-top: 1px solid #eee; padding-top: 0.8rem; }}
  </style>
</head>
<body>
  <h1>Informes de planeacion docente</h1>
  <p class="subtitulo">{INSTITUCION} - {PERIODO}</p>
  <ul>
{items_html}
  </ul>
  <p class="pie">Generado el {fecha_gen} - {length(generados)} informes</p>
</body>
</html>')

writeLines(index_html, file.path(CARPETA_HTML, "index.html"), useBytes = TRUE)

cat("\n=====================================================\n")
cat("  HTML GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Carpeta: {CARPETA_HTML}/\n"))
cat(glue("  Total:   {length(generados)} archivo(s)\n\n"))
