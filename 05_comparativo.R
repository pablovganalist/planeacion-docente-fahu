# =============================================================================
# 05_comparativo.R
# Genera un informe HTML comparativo por unidad desde la base histórica
# Output: docs/comparativos/{slug}.html  (un archivo por unidad)
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)
library(glue)

# ── Configuración ──────────────────────────────────────────────────────────────
# setwd: no necesario en CI (wd = raíz del repo)

ARCHIVO_HIST     <- "BASE_HISTORICA_FAHU.xlsx"
CARPETA_COMP     <- file.path("docs", "comparativos")
INSTITUCION      <- "Facultad de Humanidades"
TITULO_INFORME   <- "Informe comparado 2024–2026"
NORMA_PROM_HP    <- 12

CARGOS_CLAUSTRO  <- "ACADEMICO"
CARGOS_AUTORIDAD <- c("DIRECTOR","DIRECTORA","DECANA","VICEDECANO",
                      "VIME","DIRECTORA INNED")
NO_UPRO          <- c("HORA DE CLASE","SIN PROFESOR","SP",
                      "DERECHO","FAE","IDEA")

# Mapeo: UNIDAD PROFESOR (corto) → UNIDAD (largo, en la base histórica)
MAPA_UNIDAD <- c(
  "HISTORIA"             = "DEPARTAMENTO DE HISTORIA",
  "EDUCACIÓN"            = "DEPARTAMENTO DE EDUCACIÓN",
  "ESTUDIOS POLÍTICOS"   = "DEPARTAMENTO DE ESTUDIOS POLÍTICOS",
  "FILOSOFÍA"            = "DEPARTAMENTO DE FILOSOFÍA",
  "LINGÜÍSTICA Y LITERATURA" = "DEPARTAMENTO DE LINGÜÍSTICA",
  "PERIODISMO"           = "ESCUELA DE ESCUELA DE PERIODISMO",
  "PSICOLOGÍA"           = "ESCUELA DE PSICOLOGÍA"
)

# Etiquetas legibles por periodo (AÑO-PERIODO)
ETIQ_PERIODO <- c(
  "2024-2" = "2do sem 2024",
  "2025-1" = "1er sem 2025",
  "2025-2" = "2do sem 2025",
  "2026-1" = "1er sem 2026"
)

# ── Helpers ────────────────────────────────────────────────────────────────────
slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[áàä]", "a") |>
    str_replace_all("[éèë]", "e") |>
    str_replace_all("[íìï]", "i") |>
    str_replace_all("[óòö]", "o") |>
    str_replace_all("[úùü]", "u") |>
    str_replace_all("ñ", "n") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("^_|_$")
}

titulo_esp1 <- function(x) {
  prep <- c("de","del","la","las","el","los","en","con","y","a","para")
  ws   <- str_split(str_to_lower(x), "\\s+")[[1]]
  out  <- ifelse(seq_along(ws) == 1 | !ws %in% prep,
                 str_to_title(ws), ws)
  paste(out, collapse = " ")
}
titulo_esp <- function(x) vapply(x, titulo_esp1, character(1))

fmt_hp <- function(x, na_str = "—") {
  ifelse(is.na(x), na_str, formatC(x, format = "f", digits = 0, big.mark = "."))
}
fmt_pct <- function(x, na_str = "—") {
  ifelse(is.na(x), na_str, paste0(formatC(x * 100, format = "f", digits = 1), "%"))
}

# ── Colores institucionales ────────────────────────────────────────────────────
C_TEAL  <- "#00A499"
C_ORANGE<- "#EA7600"
C_DARK  <- "#394049"
C_BLUE  <- "#498BCA"
C_GOLD  <- "#EAAA00"

# ── CSS ────────────────────────────────────────────────────────────────────────
css_comp <- glue('
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
header {{
  border-bottom: 3px solid {C_TEAL};
  padding-bottom: 1rem;
  margin-bottom: 2rem;
}}
.facultad {{
  font-size: 0.78rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: {C_TEAL};
  margin: 0 0 0.25rem;
}}
h1 {{
  font-size: 1.35rem;
  color: {C_DARK};
  margin: 0.2rem 0;
}}
.subtitulo {{
  font-size: 0.85rem;
  color: #666;
  margin: 0.2rem 0 0.6rem;
}}
.botones {{
  display: flex;
  gap: 0.75rem;
  flex-wrap: wrap;
  margin-top: 0.8rem;
}}
.btn {{
  display: inline-block;
  padding: 0.4rem 1rem;
  border-radius: 4px;
  font-size: 0.8rem;
  font-weight: 600;
  text-decoration: none;
  cursor: pointer;
  border: none;
}}
.btn-volver {{
  background: #e9ecef;
  color: {C_DARK};
}}
.btn-pdf {{
  background: {C_TEAL};
  color: white;
}}
.btn-pdf:hover {{ background: #008a80; }}
h2 {{
  font-size: 1rem;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: {C_DARK};
  border-left: 4px solid {C_ORANGE};
  padding-left: 0.7rem;
  margin: 2.5rem 0 1rem;
}}
table {{
  width: 100%;
  border-collapse: collapse;
  font-size: 0.82rem;
  margin-bottom: 1.5rem;
}}
thead th {{
  background: {C_DARK};
  color: white;
  padding: 0.5rem 0.7rem;
  text-align: center;
  font-weight: 600;
  white-space: nowrap;
}}
thead th.col-left {{
  text-align: left;
}}
tbody td {{
  padding: 0.42rem 0.7rem;
  border-bottom: 1px solid #e5e7ea;
  text-align: right;
}}
tbody td.col-left {{
  text-align: left;
  font-weight: 500;
}}
tbody tr:nth-child(even) {{
  background: #f8f9fa;
}}
tbody tr.fila-total td {{
  background: {C_GOLD}22;
  font-weight: 700;
  border-top: 2px solid {C_GOLD};
}}
.alerta {{ color: #c0392b; font-weight: 700; }}
.ok {{ color: #27ae60; }}
.nota {{
  font-size: 0.75rem;
  color: #888;
  margin-top: -0.8rem;
  margin-bottom: 1.5rem;
}}
.pie {{
  margin-top: 3rem;
  padding-top: 1rem;
  border-top: 1px solid #ddd;
  font-size: 0.72rem;
  color: #aaa;
  text-align: center;
}}
@media print {{
  .botones {{ display: none; }}
  body {{ padding: 1rem; }}
}}
')

# ── Función: generar HTML de una tabla ────────────────────────────────────────
html_tabla <- function(encabezados, filas, clases_col = NULL, fila_total = NULL) {
  n_cols <- length(encabezados)
  if (is.null(clases_col)) clases_col <- rep("", n_cols)
  clases_col[1] <- paste(clases_col[1], "col-left")

  th_html <- paste0(
    '<th class="', str_trim(clases_col), '">',
    encabezados, '</th>', collapse = "\n      "
  )

  rows_html <- map_chr(seq_len(nrow(filas)), function(i) {
    es_total <- !is.null(fila_total) && i == fila_total
    clase_tr <- if (es_total) ' class="fila-total"' else ''
    celdas   <- map2_chr(seq_len(n_cols), clases_col, function(j, cls) {
      paste0('<td class="', str_trim(cls), '">', filas[i, j], '</td>')
    })
    paste0('<tr', clase_tr, '>', paste(celdas, collapse = ""), '</tr>')
  })

  glue(
    '<table>
  <thead><tr>
      {th_html}
  </tr></thead>
  <tbody>
    {paste(rows_html, collapse = "\n    ")}
  </tbody>
</table>',
    .open = "{", .close = "}"
  )
}

# Versión simplificada sin glue anidado
construir_tabla <- function(encabezados, df_filas, fila_total_idx = NULL) {
  n <- ncol(df_filas)
  clase1 <- "col-left"

  encab_html <- paste0(
    vapply(seq_along(encabezados), function(j) {
      cls <- if (j == 1) "col-left" else ""
      paste0('<th class="', cls, '">', encabezados[j], '</th>')
    }, character(1)),
    collapse = "\n      "
  )

  filas_html <- vapply(seq_len(nrow(df_filas)), function(i) {
    es_tot <- !is.null(fila_total_idx) && i == fila_total_idx
    cls_tr <- if (es_tot) ' class="fila-total"' else ''
    celdas <- vapply(seq_len(n), function(j) {
      cls_td <- if (j == 1) "col-left" else ""
      paste0('<td class="', cls_td, '">', df_filas[i, j], '</td>')
    }, character(1))
    paste0('<tr', cls_tr, '>', paste(celdas, collapse = ""), '</tr>')
  }, character(1))

  paste0(
    '<table>\n  <thead><tr>\n      ', encab_html,
    '\n  </tr></thead>\n  <tbody>\n    ',
    paste(filas_html, collapse = "\n    "),
    '\n  </tbody>\n</table>'
  )
}

# ── Función principal: generar informe comparativo por unidad ─────────────────
generar_comparativo <- function(unidad_prof, df_hist, ruta_html) {

  unidad_larga <- MAPA_UNIDAD[unidad_prof]
  nombre_titulo <- titulo_esp(
    switch(unidad_prof,
      "LINGÜÍSTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
      "PERIODISMO"               = "Escuela de Periodismo",
      "PSICOLOGÍA"               = "Escuela de Psicología",
      paste("Departamento de", titulo_esp(unidad_prof))
    )
  )

  # Periodos disponibles para esta unidad
  periodos_disp <- df_hist |>
    filter(
      (str_detect(UNIDAD, fixed(unidad_larga)) |
       UNIDAD_PROFESOR == unidad_prof),
      !is.na(AÑO), !is.na(PERIODO)
    ) |>
    distinct(AÑO, PERIODO) |>
    arrange(AÑO, PERIODO) |>
    mutate(clave = paste0(AÑO, "-", PERIODO),
           etiq  = ETIQ_PERIODO[clave])

  if (nrow(periodos_disp) == 0) {
    message(glue("  Sin datos para {unidad_prof}, omitiendo."))
    return(invisible(NULL))
  }

  # ── TABLA 1: Horas planeadas por periodo ──────────────────────────────────
  t1_rows <- map_dfr(seq_len(nrow(periodos_disp)), function(i) {
    anio <- periodos_disp$AÑO[i]
    per  <- periodos_disp$PERIODO[i]

    # Cursos del departamento
    dc <- df_hist |>
      filter(str_detect(UNIDAD, fixed(unidad_larga)),
             AÑO == anio, PERIODO == per)

    hp_jor  <- dc |> filter(CONTRATO == "JORNADA")  |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_hora <- dc |> filter(CONTRATO == "POR HORA") |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_sp   <- dc |> filter(CONTRATO == "SP")       |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_tot  <- hp_jor + hp_hora + hp_sp

    tibble(
      Periodo   = periodos_disp$etiq[i],
      Jornada   = fmt_hp(hp_jor),
      `Por hora`= fmt_hp(hp_hora),
      SP        = fmt_hp(hp_sp),
      Total     = fmt_hp(hp_tot)
    )
  })

  tabla1_html <- construir_tabla(
    c("Periodo", "Jornada (HP)", "Por hora (HP)", "SP (HP)", "Total HP"),
    t1_rows
  )

  # ── TABLA 2: Cobertura según contrato (%) ─────────────────────────────────
  t2_rows <- map_dfr(seq_len(nrow(periodos_disp)), function(i) {
    anio <- periodos_disp$AÑO[i]
    per  <- periodos_disp$PERIODO[i]

    dc <- df_hist |>
      filter(str_detect(UNIDAD, fixed(unidad_larga)),
             AÑO == anio, PERIODO == per)

    hp_jor  <- dc |> filter(CONTRATO == "JORNADA")  |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_hora <- dc |> filter(CONTRATO == "POR HORA") |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_sp   <- dc |> filter(CONTRATO == "SP")       |> pull(HORAS_PED) |> sum(na.rm = TRUE)
    hp_tot  <- hp_jor + hp_hora + hp_sp

    tibble(
      Periodo   = periodos_disp$etiq[i],
      Jornada   = fmt_pct(if (hp_tot > 0) hp_jor / hp_tot else NA),
      `Por hora`= fmt_pct(if (hp_tot > 0) hp_hora / hp_tot else NA),
      SP        = fmt_pct(if (hp_tot > 0) hp_sp / hp_tot else NA)
    )
  })

  tabla2_html <- construir_tabla(
    c("Periodo", "Jornada", "Por hora", "SP"),
    t2_rows
  )

  # ── TABLA 3: Promedio de horas por académico ───────────────────────────────
  t3_rows <- map_dfr(seq_len(nrow(periodos_disp)), function(i) {
    anio <- periodos_disp$AÑO[i]
    per  <- periodos_disp$PERIODO[i]

    # Académicos jornada de la unidad (por UNIDAD PROFESOR)
    dp <- df_hist |>
      filter(UNIDAD_PROFESOR == unidad_prof,
             CARGO %in% c(CARGOS_CLAUSTRO),
             CONTRATO == "JORNADA",
             AÑO == anio, PERIODO == per)

    hp_acad <- dp |>
      distinct(PROFESOR, .keep_all = TRUE) |>
      pull(HORAS_PED) |>
      sum(na.rm = TRUE)

    n_acad  <- dp |> pull(PROFESOR) |> n_distinct()
    prom    <- if (n_acad > 0) round(hp_acad / n_acad, 1) else NA
    cumple  <- if (!is.na(prom)) {
      if (prom >= NORMA_PROM_HP) '<span class="ok">✓</span>'
      else '<span class="alerta">✗</span>'
    } else "—"

    tibble(
      Periodo     = periodos_disp$etiq[i],
      `N acad.`   = as.character(n_acad),
      `HP jornada`= fmt_hp(hp_acad),
      Promedio    = ifelse(is.na(prom), "—", as.character(prom)),
      `≥ 12 HP`   = cumple
    )
  })

  tabla3_html <- construir_tabla(
    c("Periodo", "N académicos", "HP jornada", "Promedio HP", paste0("≥ ", NORMA_PROM_HP, " HP")),
    t3_rows
  )

  # ── Construir HTML ────────────────────────────────────────────────────────
  slug_u    <- slug(unidad_prof)
  ruta_volver <- "../index.html"

  html_final <- paste0(
    '<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', nombre_titulo, ' — ', TITULO_INFORME, '</title>
  <style>\n', css_comp, '\n  </style>
</head>
<body>
<header>
  <p class="facultad">', INSTITUCION, '</p>
  <h1>', nombre_titulo, '</h1>
  <p class="subtitulo">', TITULO_INFORME, '</p>
  <div class="botones">
    <a href="', ruta_volver, '" class="btn btn-volver">← Volver al índice</a>
    <button class="btn btn-pdf" onclick="window.print()">⬇ Descargar PDF</button>
  </div>
</header>
<main>

<h2>Horas pedagógicas planeadas</h2>
<p class="nota">Horas totales de los cursos del departamento, agrupadas por tipo de contrato del docente asignado.</p>
', tabla1_html, '

<h2>Cobertura según tipo de contrato</h2>
<p class="nota">Porcentaje de horas pedagógicas cubiertas por cada tipo de contrato sobre el total del departamento.</p>
', tabla2_html, '

<h2>Promedio de horas por académico jornada</h2>
<p class="nota">Calculado sobre académicos de jornada (excluye autoridades y profesores por hora). La norma institucional es ≥ 12 HP por semestre.</p>
', tabla3_html, '

</main>
<div class="pie">Base elaborada a partir de los registros de planeación docente FAHU. Código elaborado por Pablo Valenzuela con apoyo de Claude de Anthropic.</div>
</body>
</html>'
  )

  writeLines(html_final, ruta_html, useBytes = TRUE)
  message(glue("    OK {basename(ruta_html)}"))
}

# =============================================================================
# EJECUCIÓN
# =============================================================================

if (!file.exists(ARCHIVO_HIST)) {
  stop(glue("No se encuentra: '{ARCHIVO_HIST}'\nDirectorio actual: {getwd()}"))
}

df_hist <- read_excel(ARCHIVO_HIST) |>
  clean_names() |>
  rename(
    UNIDAD          = unidad,
    CARRERA         = carrera,
    AÑO             = ano,
    PERIODO         = periodo,
    HORAS_PED       = horas_ped,
    PROFESOR        = profesor,
    CARGO           = cargo,
    UNIDAD_PROFESOR = unidad_profesor,
    CONTRATO        = contrato
  ) |>
  mutate(
    UNIDAD          = str_to_upper(str_squish(UNIDAD)),
    CARGO           = str_to_upper(str_squish(CARGO)),
    CONTRATO        = str_to_upper(str_squish(CONTRATO)),
    UNIDAD_PROFESOR = str_to_upper(str_squish(UNIDAD_PROFESOR)),
    PROFESOR        = str_to_title(str_squish(PROFESOR))
  )

unidades <- names(MAPA_UNIDAD)

if (!dir.exists(CARPETA_COMP)) dir.create(CARPETA_COMP, recursive = TRUE)

message(glue("\nGenerando {length(unidades)} informes comparativos...\n"))
generados <- character(0)
errores   <- character(0)

for (u in unidades) {
  nombre_arch <- paste0(slug(u), ".html")
  ruta_html   <- file.path(CARPETA_COMP, nombre_arch)
  message(glue("  {u}..."))

  tryCatch({
    generar_comparativo(u, df_hist, ruta_html)
    generados <- c(generados, nombre_arch)
  }, error = function(e) {
    message(glue("    ERROR: {conditionMessage(e)}"))
    errores <<- c(errores, u)
  })
}

cat("\n=====================================================\n")
cat("  COMPARATIVOS GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Carpeta:  {CARPETA_COMP}/\n"))
cat(glue("  Total:    {length(generados)} informe(s)\n"))
if (length(errores) > 0)
  cat(glue("  Fallaron: {paste(errores, collapse=', ')}\n"))
cat("\n")
