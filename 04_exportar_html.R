# =============================================================================
# 04_exportar_html.R
# Convierte todos los informes .md a HTML con estilo institucional
# y los organiza en un índice navegable
# =============================================================================
# Uso: source("04_exportar_html.R")
# Resultado: carpeta informes_html/ lista para subir y compartir
# =============================================================================

library(rmarkdown)
library(readxl)
library(janitor)
library(tidyverse)
library(glue)

# ── Configuracion ─────────────────────────────────────────────────────────────

CARPETA_MD   <- "informes_md"
CARPETA_HTML <- "informes_html"
PERIODO      <- "Primer semestre 2026"
INSTITUCION  <- "Facultad de Humanidades"

# ── Colores institucionales USACH ─────────────────────────────────────────────
# Principales
C_TEAL    <- "#00A499"   # Pantone 3272 C — color primario institucional
C_ORANGE  <- "#EA7600"   # Pantone 716 C
C_DARK    <- "#394049"   # Pantone 432 C — texto y cabeceras
# PEI 2030
C_BLUE    <- "#498BCA"   # Pantone 279 C
C_GOLD    <- "#EAAA00"   # Pantone 124 C — filas de totales

# =============================================================================
# 1. CSS CON ESTILO INSTITUCIONAL USACH
# =============================================================================

css_institucional <- glue('
/* ── Tipografia y base ───────────────────────────────────── */
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

/* ── Encabezados ─────────────────────────────────────────── */
/* h1 se define en el bloque header institucional más abajo */
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

/* ── Tablas ──────────────────────────────────────────────── */
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

/* ── Filas de totales: se detectan porque tienen <strong> en todas las celdas
      Aplica a filas A, B, C de la tabla de horas y al promedio del claustro ── */
tbody tr:has(td > strong) {{
  background-color: #FFF3CD;
  border-top: 2px solid {C_GOLD};
  border-bottom: 2px solid {C_GOLD};
}}
tbody tr:has(td > strong) td {{
  font-weight: 700;
  color: {C_DARK};
}}

/* ── Cupos bajos (⚠): celda con simbolo de advertencia ───── */
tbody td:has(> span.alerta) {{
  color: #C8102E;
  font-weight: 700;
}}

/* ── Blockquotes (alertas y notas) ──────────────────────── */
blockquote {{
  border-left: 4px solid {C_TEAL};
  background: #E8F8F7;
  margin: 1rem 0;
  padding: 0.5rem 1rem;
  border-radius: 0 4px 4px 0;
  color: {C_DARK};
}}
blockquote p {{ margin: 0; }}

/* ── Header institucional ────────────────────────────────── */
header {{
  border-bottom: 3px solid {C_TEAL};
  padding-bottom: 1rem;
  margin-bottom: 2rem;
}}
/* Nombre de la facultad: supraencabezado pequeño sobre el título */
p.facultad {{
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: {C_TEAL};
  margin: 0 0 0.2rem 0;
}}
/* h1: nombre de la unidad — título principal */
h1 {{
  color: {C_DARK};
  font-size: 1.65rem;
  font-weight: 700;
  margin: 0 0 0.3rem 0;
  border: none;        /* anula el border-bottom que estaba antes en h1 */
  padding: 0;
}}
/* Subtitulo de periodo */
.periodo {{
  color: #666;
  font-size: 0.95rem;
  margin: 0 0 1rem 0;
}}

/* ── Código de programa bajo el nombre de carrera ────────── */
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

/* ── Pie de pagina ───────────────────────────────────────── */
.pie {{
  margin-top: 3rem;
  padding-top: 0.8rem;
  border-top: 2px solid {C_TEAL};
  font-size: 11px;
  color: #666;
}}

/* ── Codigo de programa ──────────────────────────────────── */
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

/* ── Boton descargar PDF ─────────────────────────────────── */
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
  letter-spacing: 0.03em;
}}
.btn-pdf:hover {{
  background: {C_DARK};
}}

/* ── Impresion ───────────────────────────────────────────── */
@media print {{
  body {{ max-width: 100%; padding: 1cm; font-size: 11px; }}
  h2 {{ page-break-before: auto; }}
  table {{ font-size: 10px; }}
  tbody tr:has(td > strong) {{ background-color: #FFF3CD !important; }}
  .btn-pdf {{ display: none; }}
  p.cod-programa {{ background: #eee !important; }}
}}
')

# =============================================================================
# 2. FUNCION: CONVERTIR UN .md A .html
# =============================================================================

md_a_html <- function(ruta_md, ruta_html, css) {

  md_texto <- readLines(ruta_md, encoding = "UTF-8", warn = FALSE)

  # El Rmd emite "# Nombre de la unidad" como primera línea de contenido.
  # La extraemos para el header institucional y la eliminamos del body.
  linea_h1 <- md_texto[str_starts(md_texto, "^# ")]
  titulo   <- if (length(linea_h1) > 0)
    str_remove(linea_h1[1], "^# ") else basename(tools::file_path_sans_ext(ruta_md))

  # Convertir md a html con pandoc (sin --standalone para obtener sólo el fragmento)
  tmp_html <- tempfile(fileext = ".html")
  rmarkdown::pandoc_convert(
    input   = normalizePath(ruta_md),
    to      = "html",
    output  = tmp_html,
    options = character(0)
  )

  html_raw <- paste(readLines(tmp_html, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  file.remove(tmp_html)

  html_body <- html_raw |>
    # Eliminar el <h1> que Pandoc genera desde el "# nombre" del md
    str_replace_all('<h1[^>]*>.*?</h1>', '')

  boton_pdf <- '<button class="btn-pdf" onclick="window.print()">⬇ Descargar PDF</button>'

  # Derivar slug desde nombre de archivo (ej: "02_historia" -> "historia")
  slug_unidad <- str_remove(nombre, "^[0-9]+_")
  boton_comp  <- paste0(
    '<a href="comparativos/', slug_unidad, '.html" class="btn-comp">',
    '📊 Informe comparado 2024–2026</a>'
  )

  header_html <- paste0(
    '<header>\n',
    '<p class="facultad">', INSTITUCION, '</p>\n',
    '<h1>', titulo, '</h1>\n',
    '<p class="periodo">', PERIODO, '</p>\n',
    '<div class="botones-header">', boton_comp, ' ', boton_pdf, '</div>\n',
    '</header>'
  )

  html_final <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', titulo, ' — ', PERIODO, '</title>
  <style>
', css, '
  </style>
</head>
<body>
', header_html, '
<main>
', html_body, '
</main>
<div class="pie">Base elaborada a partir de los registros de planeación docente. Código elaborado por Pablo Valenzuela con apoyo de Claude de Anthropic · ', PERIODO, '</div>
</body>
</html>')

  writeLines(html_final, ruta_html, useBytes = TRUE)
}

# =============================================================================
# 3. CONVERTIR TODOS LOS .md
# =============================================================================

if (!rmarkdown::pandoc_available()) {
  stop("Pandoc no encontrado. Ejecuta desde RStudio o instala pandoc.org")
}

if (!dir.exists(CARPETA_HTML)) dir.create(CARPETA_HTML, recursive = TRUE)

archivos_md <- list.files(CARPETA_MD, pattern = "\\.md$", full.names = TRUE)

if (length(archivos_md) == 0) {
  stop(glue("No hay .md en '{CARPETA_MD}/'. Ejecuta primero el script 02."))
}

message(glue("\nConvirtiendo {length(archivos_md)} archivos a HTML...\n"))

generados <- character(0)
errores   <- character(0)

for (i in seq_along(archivos_md)) {
  ruta_md   <- archivos_md[i]
  nombre    <- tools::file_path_sans_ext(basename(ruta_md))
  ruta_html <- file.path(CARPETA_HTML, paste0(nombre, ".html"))

  message(glue("[{i}/{length(archivos_md)}] {basename(ruta_md)}..."))

  tryCatch({
    md_a_html(ruta_md, ruta_html, css_institucional)
    message(glue("    OK {basename(ruta_html)}"))
    generados <- c(generados, basename(ruta_html))
  }, error = function(e) {
    message(glue("    ERROR: {conditionMessage(e)}"))
    errores <<- c(errores, basename(ruta_md))
  })
}

# =============================================================================
# 4. GENERAR PAGINA INDICE (index.html)
# =============================================================================
# Esta es la pagina de entrada: lista todos los informes con un link a cada uno

leer_titulo_unidad <- function(ruta_md) {
  lineas <- readLines(ruta_md, n = 5, encoding = "UTF-8", warn = FALSE)
  linea  <- lineas[str_starts(lineas, "# ")]
  if (length(linea) > 0) str_remove(linea[1], "^# ") else basename(ruta_md)
}

titulos <- map_chr(archivos_md[
  tools::file_path_sans_ext(basename(archivos_md)) %in%
  tools::file_path_sans_ext(generados)
], leer_titulo_unidad)

items_html <- map2_chr(generados, titulos, function(archivo, titulo) {
  glue('    <li><a href="{archivo}">{titulo}</a></li>')
}) |> paste(collapse = "\n")

fecha_gen <- format(Sys.time(), "%d/%m/%Y %H:%M")

index_html <- glue('<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Informes de planeación docente — {PERIODO}</title>
  <style>
    body {{
      font-family: "Segoe UI", Arial, sans-serif;
      max-width: 700px;
      margin: 3rem auto;
      padding: 0 2rem;
      color: #222;
    }}
    h1 {{
      color: {C_TEAL};
      border-bottom: 3px solid {C_TEAL};
      padding-bottom: 0.4rem;
    }}
    .subtitulo {{
      color: #555;
      margin-top: -0.5rem;
      margin-bottom: 2rem;
    }}
    ul {{
      list-style: none;
      padding: 0;
    }}
    li {{
      margin: 0.5rem 0;
    }}
    a {{
      color: {C_BLUE};
      text-decoration: none;
      font-size: 1rem;
      display: block;
      padding: 0.6rem 1rem;
      border-left: 4px solid #E8F8F7;
      border-radius: 0 4px 4px 0;
      transition: background 0.15s;
    }}
    a:hover {{
      background: #E8F8F7;
      border-left-color: {C_TEAL};
      color: {C_TEAL};
    }}
    .pie {{
      margin-top: 3rem;
      font-size: 11px;
      color: #aaa;
      border-top: 1px solid #eee;
      padding-top: 0.8rem;
    }}
  </style>
</head>
<body>
  <h1>Informes de planeación docente</h1>
  <p class="subtitulo">{INSTITUCION} &mdash; {PERIODO}</p>
  <ul>
{items_html}
  </ul>
  <p class="pie">Generado el {fecha_gen} &bull; {length(generados)} informes</p>
</body>
</html>')

writeLines(index_html,
           file.path(CARPETA_HTML, "index.html"),
           useBytes = TRUE)
message("\nindex.html generado")

# =============================================================================
# 5. RESUMEN Y OPCIONES DE PUBLICACION
# =============================================================================

cat("\n=====================================================\n")
cat("  HTML GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Carpeta:    {CARPETA_HTML}/\n"))
cat(glue("  Informes:   {length(generados)}\n"))
cat(glue("  Indice:     {CARPETA_HTML}/index.html\n"))

if (length(errores) > 0) {
  cat(glue("  Errores:    {paste(errores, collapse=', ')}\n"))
}

cat("
  COMO COMPARTIR LOS INFORMES
  ─────────────────────────────────────────────────
  Opcion A — Netlify Drop (MAS FACIL, gratis):
    1. Ve a https://app.netlify.com/drop
    2. Arrastra la carpeta 'informes_html' completa
    3. Netlify genera una URL del tipo:
       https://nombre-aleatorio.netlify.app/
    4. Comparte esa URL — quien la reciba ve el indice
       y puede navegar a cada informe

  Opcion B — GitHub Pages (gratis, URL permanente):
    1. Crea un repo en github.com
    2. Sube la carpeta informes_html como rama gh-pages
    3. URL: https://tu-usuario.github.io/nombre-repo/
    Desde R con el paquete usethis o simplemente
    arrastrando los archivos en la interfaz web de GitHub.

  Opcion C — Compartir archivo por correo:
    Cada .html es un archivo unico que se puede enviar
    directamente. El destinatario lo abre en el browser.
    No necesita internet ni instalar nada.
  ─────────────────────────────────────────────────
")
