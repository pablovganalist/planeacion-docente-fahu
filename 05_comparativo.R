# =============================================================================
# 05_comparativo.R
# Genera informes comparativos por unidad combinando la base histórica
# (periodos cerrados) con el semestre en curso.
#
# HORAS PROF se calcula con la misma lógica exacta del Rmd:
#   - Dedup global de sellos: mismo PROFESOR + SEC cuenta una sola vez
#   - Promedio: numerador = HP jornada (académicos + autoridades)
#              denominador = N académicos (CARGO = ACADEMICO únicamente)
#
# Output: docs/comparativos/{slug}.html — uno por unidad
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)
library(glue)
source("00_base_utils.R", local = TRUE)

# ── Configuración ──────────────────────────────────────────────────────────────
ARCHIVO <- if (exists("ARCHIVO")) ARCHIVO else "BASE_FAHU.xlsx"
ARCHIVO <- resolver_archivo_base(ARCHIVO)
if (!exists("ANO_SEM") || !exists("PER_SEM")) {
  .sem <- detectar_semestre_actual(ARCHIVO)
  ANO_SEM <- .sem$ano
  PER_SEM <- .sem$periodo
  rm(.sem)
}
CARPETA_COMP   <- file.path("docs", "comparativos")
INSTITUCION    <- "Facultad de Humanidades"
TITULO_INFORME <- "Informe comparado"
NORMA_PROM_HP  <- 12

CARGOS_AUTORIDAD <- c("DIRECTOR","DIRECTORA","DECANA","VICEDECANO",
                      "VIME","DIRECTORA INNED")

UNIDADES <- c(
  "HISTORIA", "EDUCACIÓN", "ESTUDIOS POLÍTICOS", "FILOSOFÍA",
  "LINGÜÍSTICA Y LITERATURA", "PERIODISMO", "PSICOLOGÍA"
)

etiq_periodo <- function(anio, per) {
  if (is.na(per) || is.na(anio)) return(NA_character_)
  paste0(if (isTRUE(per == 1)) "1er sem " else "2do sem ", anio)
}

C_TEAL   <- "#00A499"
C_ORANGE <- "#EA7600"
C_DARK   <- "#394049"
C_BLUE   <- "#498BCA"
C_GOLD   <- "#EAAA00"

slug <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("[áàä]","a") |> str_replace_all("[éèë]","e") |>
    str_replace_all("[íìï]","i") |> str_replace_all("[óòö]","o") |>
    str_replace_all("[úùü]","u") |> str_replace_all("ñ","n") |>
    str_replace_all("[^a-z0-9]+","_") |> str_remove("^_|_$")
}

titulo_esp1 <- function(x) {
  prep <- c("de","del","la","las","el","los","en","con","y","a","para","sin")
  ws   <- str_split(str_to_lower(x), "\\s+")[[1]]
  out  <- ifelse(seq_along(ws) == 1 | !ws %in% prep, str_to_title(ws), ws)
  paste(out, collapse = " ")
}
titulo_esp <- function(x) vapply(x, titulo_esp1, character(1))

fmt_hp  <- function(x) ifelse(is.na(x) | x == 0, "\u2014",
                               formatC(x, format="f", digits=0, big.mark="."))
fmt_pct <- function(x) ifelse(is.na(x), "\u2014",
                               paste0(formatC(x*100, format="f", digits=1), "%"))

css_comp <- glue('
body {{
  font-family: "Segoe UI", Arial, sans-serif;
  font-size: 13px;
  line-height: 1.65;
  color: {C_DARK};
  max-width: 960px;
  margin: 0 auto;
  padding: 2rem 2.5rem 4rem;
  background: #fff;
}}
@media (max-width: 600px) {{
  body {{ padding: 1rem 1rem 3rem; font-size: 12px; }}
  h1 {{ font-size: 1.1rem; }}
  h2 {{ font-size: 0.85rem; }}
  .botones {{ flex-direction: column; }}
  .btn-volver, .btn-informe, .btn-pdf {{ text-align: center; }}
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
h1 {{ font-size: 1.35rem; color: {C_DARK}; margin: 0.2rem 0; }}
.subtitulo {{ font-size: 0.85rem; color: #666; margin: 0.25rem 0 0.7rem; }}
.botones {{ display: flex; gap: 0.6rem; flex-wrap: wrap; margin-top: 0.5rem; }}
.btn-volver {{
  display: inline-block; padding: 0.38rem 0.9rem;
  background: #e9ecef; color: {C_DARK}; border-radius: 4px;
  font-size: 0.78rem; font-weight: 600; text-decoration: none;
}}
.btn-informe {{
  display: inline-block; padding: 0.38rem 0.9rem;
  background: {C_ORANGE}; color: white; border-radius: 4px;
  font-size: 0.78rem; font-weight: 600; text-decoration: none;
}}
.btn-informe:hover {{ background: #c96500; }}
.btn-pdf {{
  display: inline-block; padding: 0.38rem 0.9rem;
  background: {C_TEAL}; color: white; border-radius: 4px;
  font-size: 0.78rem; font-weight: 600; cursor: pointer; border: none;
}}
.btn-pdf:hover {{ background: #008a80; }}
h2 {{
  font-size: 0.95rem; text-transform: uppercase; letter-spacing: 0.06em;
  color: {C_DARK}; border-left: 4px solid {C_ORANGE};
  padding-left: 0.7rem; margin: 2.5rem 0 0.5rem;
}}
.nota {{ font-size: 0.75rem; color: #888; margin: 0 0 0.8rem; }}
.tabla-wrap {{ overflow-x: auto; -webkit-overflow-scrolling: touch; margin-bottom: 0.5rem; }}
table {{ width: 100%; border-collapse: collapse; font-size: 0.82rem; min-width: 380px; }}
thead th {{
  background: {C_DARK}; color: white; padding: 0.5rem 0.7rem;
  text-align: center; font-weight: 600; white-space: nowrap;
}}
thead th.col-izq {{ text-align: left; }}
tbody td {{
  padding: 0.42rem 0.7rem; border-bottom: 1px solid #e5e7ea;
  text-align: right; white-space: nowrap;
}}
tbody td.col-izq {{ text-align: left; }}
tbody tr:nth-child(even) {{ background: #f8f9fa; }}
tbody tr.fila-actual td {{ background: {C_GOLD}33; font-weight: 700; }}
.alerta {{ color: #c0392b; font-weight: 700; }}
.ok     {{ color: #27ae60; }}
.pie {{
  margin-top: 3rem; padding-top: 1rem; border-top: 1px solid #ddd;
  font-size: 0.72rem; color: #aaa; text-align: center;
}}
@media print {{ .botones {{ display: none; }} body {{ padding: 1rem; }} }}
')

html_tabla <- function(encabezados, filas_df, fila_destacada = NULL) {
  n <- ncol(filas_df)
  th <- vapply(seq_along(encabezados), function(j) {
    cls <- if (j == 1) ' class="col-izq"' else ''
    paste0('<th', cls, '>', encabezados[j], '</th>')
  }, character(1))
  trs <- vapply(seq_len(nrow(filas_df)), function(i) {
    cls_tr <- if (!is.null(fila_destacada) && i == fila_destacada)
      ' class="fila-actual"' else ''
    tds <- vapply(seq_len(n), function(j) {
      cls_td <- if (j == 1) ' class="col-izq"' else ''
      paste0('<td', cls_td, '>', filas_df[i, j], '</td>')
    }, character(1))
    paste0('<tr', cls_tr, '>', paste(tds, collapse=''), '</tr>')
  }, character(1))
  paste0('<div class="tabla-wrap"><table>\n<thead><tr>', paste(th, collapse=''), '</tr></thead>\n',
         '<tbody>\n', paste(trs, collapse='\n'), '\n</tbody>\n</table></div>')
}

# =============================================================================
# CARGA Y PREPARACIÓN — misma lógica que el Rmd
# =============================================================================

cargar_base <- function(path) {
  read_excel(path) |>
    normalizar_base_planeacion() |>
    mutate(
      profesor    = str_to_title(str_squish(profesor)),
      unidad_dep  = str_to_upper(str_squish(unidad_dep)),
      unidad_prof = str_to_upper(str_squish(unidad_prof)),
      cargo       = str_to_upper(str_squish(cargo)),
      contrato    = str_to_upper(str_squish(contrato)),
      tipo        = str_to_upper(str_squish(tipo)),
      sec         = paste0(tipo, "-", nsec),
      tipo_contrato = case_when(
        contrato == "JORNADA"  ~ "Jornada",
        contrato == "POR HORA" ~ "Por Hora",
        contrato == "SP"       ~ "Sin Planta",
        TRUE                   ~ "Sin Información"
      ),
      en_claustro  = cargo == "ACADEMICO",
      es_sello     = str_starts(tipo, "S"),
      across(c(horas_ped, horas_plan, eq_cron), ~ replace_na(suppressWarnings(as.numeric(.)), 0))
    )
}

# Dedup global por periodo — misma lógica que el Rmd
aplicar_dedup <- function(df) {
  df |>
    arrange(ano, periodo, profesor, sec) |>
    group_by(ano, periodo) |>
    mutate(
      seccion_duplicada = es_sello & duplicated(paste(profesor, sec)),
      horas_prof = if_else(seccion_duplicada, 0L, as.integer(horas_ped)),
      eq_prof    = if_else(seccion_duplicada, 0,  as.numeric(eq_cron))
    ) |>
    ungroup()
}

if (!file.exists(ARCHIVO)) stop(glue("No se encuentra: '{ARCHIVO}'"))

# Cargar base unificada y separar semestre actual de históricos
df_todo_raw <- cargar_base(ARCHIVO) |> aplicar_dedup()
df_sem  <- df_todo_raw |> filter(as.integer(ano)==ANO_SEM, as.integer(periodo)==PER_SEM)
df_hist <- df_todo_raw |> filter(!(as.integer(ano)==ANO_SEM & as.integer(periodo)==PER_SEM))

sem_actual_ano <- max(df_sem$ano,     na.rm = TRUE)
sem_actual_per <- max(df_sem$periodo, na.rm = TRUE)

df_todo <- df_todo_raw  # ya unificado

periodos <- df_todo |>
  distinct(ano, periodo) |>
  filter(!is.na(ano), !is.na(periodo)) |>
  arrange(ano, periodo) |>
  mutate(etiq  = map2_chr(ano, periodo, etiq_periodo),
         clave = paste0(ano, "-", periodo))

message(glue("Periodos: {paste(periodos$etiq, collapse=' | ')}"))

# =============================================================================
# FUNCIÓN: generar comparativo de una unidad
# =============================================================================

generar_comparativo <- function(UP) {
  UP <- str_to_upper(str_squish(UP))

  nombre_titulo <- switch(UP,
    "EDUCACIÓN"                = "Departamento de Educación",
    "ESTUDIOS POLÍTICOS"       = "Departamento de Estudios Políticos",
    "FILOSOFÍA"                = "Departamento de Filosofía",
    "HISTORIA"                 = "Departamento de Historia",
    "LINGÜÍSTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
    "PERIODISMO"               = "Escuela de Periodismo",
    "PSICOLOGÍA"               = "Escuela de Psicología",
    UP
  )

  # Helper: obtener nombre largo de la unidad en un periodo
  nombre_largo_up <- function(anio, per) {
    df_todo |>
      filter(unidad_prof == UP, ano == anio, periodo == per) |>
      count(unidad_dep) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(unidad_dep)
  }

  # Helper: cursos del departamento con dedup local de sellos
  cursos_unidad <- function(anio, per) {
    nl <- nombre_largo_up(anio, per)
    if (length(nl) == 0) return(NULL)
    df_todo |>
      filter(unidad_dep == nl, ano == anio, periodo == per) |>
      arrange(profesor, sec) |>
      mutate(
        sello_dup_local = es_sello & duplicated(paste(profesor, sec)),
        horas_unidad    = if_else(sello_dup_local, 0L, as.integer(horas_plan))
      )
  }

  # ── Tabla 1: Horas planeadas ──────────────────────────────────────────────
  t1 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dc <- cursos_unidad(periodos$ano[i], periodos$periodo[i])
    if (is.null(dc))
      return(tibble(Periodo=periodos$etiq[i],Jornada="—",`Por hora`="—",SP="—",Total="—"))
    hp_j <- sum(dc$horas_unidad[dc$tipo_contrato=="Jornada"])
    hp_h <- sum(dc$horas_unidad[dc$tipo_contrato=="Por Hora"])
    hp_s <- sum(dc$horas_unidad[dc$tipo_contrato=="Sin Planta"])
    tibble(Periodo=periodos$etiq[i], Jornada=fmt_hp(hp_j),
           `Por hora`=fmt_hp(hp_h), SP=fmt_hp(hp_s), Total=fmt_hp(hp_j+hp_h+hp_s))
  })

  # ── Tabla 2: Cobertura (%) ────────────────────────────────────────────────
  t2 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dc <- cursos_unidad(periodos$ano[i], periodos$periodo[i])
    if (is.null(dc))
      return(tibble(Periodo=periodos$etiq[i],Jornada="—",`Por hora`="—",SP="—"))
    hp_j <- sum(dc$horas_unidad[dc$tipo_contrato=="Jornada"])
    hp_h <- sum(dc$horas_unidad[dc$tipo_contrato=="Por Hora"])
    hp_s <- sum(dc$horas_unidad[dc$tipo_contrato=="Sin Planta"])
    tot  <- hp_j + hp_h + hp_s
    tibble(Periodo=periodos$etiq[i],
           Jornada    = fmt_pct(if(tot>0) hp_j/tot else NA),
           `Por hora` = fmt_pct(if(tot>0) hp_h/tot else NA),
           SP         = fmt_pct(if(tot>0) hp_s/tot else NA))
  })

  # ── Tabla 3: Promedio HP claustro ─────────────────────────────────────────
  t3 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dp <- df_todo |>
      filter(unidad_prof == UP,
             ano == periodos$ano[i], periodo == periodos$periodo[i])
    if (nrow(dp) == 0)
      return(tibble(Periodo=periodos$etiq[i],`N acad.`="—",
                    `HP jornada`="—",Promedio="—",Norma="—"))

    n_cl    <- dp |> filter(en_claustro) |> distinct(profesor) |> nrow()
    hp_jor  <- dp |> filter(tipo_contrato=="Jornada") |> pull(horas_prof) |> sum(na.rm=TRUE)
    prom    <- if (n_cl > 0) round(hp_jor / n_cl, 1) else NA
    cumple  <- if (is.na(prom)) "\u2014" else
               if (prom >= NORMA_PROM_HP) '<span class="ok">\u2713</span>'
               else '<span class="alerta">\u2717</span>'

    tibble(Periodo=periodos$etiq[i], `N acad.`=as.character(n_cl),
           `HP jornada`=fmt_hp(hp_jor), Promedio=ifelse(is.na(prom),"\u2014",as.character(prom)),
           Norma=cumple)
  })

  fila_act <- which(periodos$ano == sem_actual_ano & periodos$periodo == sem_actual_per)
  anio_min <- min(periodos$ano); anio_max <- max(periodos$ano)
  rango    <- if (anio_min==anio_max) as.character(anio_min) else
              paste0(anio_min,"\u2013",anio_max)
  slug_u        <- slug(UP)
  # Buscar el archivo HTML real en docs/ para obtener el prefijo correcto
  archivos_docs <- list.files("docs", pattern = paste0("_", slug_u, "\\.html$"))
  archivo_inf   <- if (length(archivos_docs) > 0)
    paste0("../", archivos_docs[1])
  else
    paste0("../01_", slug_u, ".html")  # fallback

  html_out <- paste0(
'<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', nombre_titulo, ' \u2014 ', TITULO_INFORME, ' ', rango, '</title>
  <style>\n', css_comp, '\n</style>
</head>
<body>
<header>
  <p class="facultad">', INSTITUCION, '</p>
  <h1>', nombre_titulo, '</h1>
  <p class="subtitulo">', TITULO_INFORME, ' ', rango, '</p>
  <div class="botones">
    <a href="../index.html" class="btn-volver">\u2190 \u00cdndice</a>
    <a href="', archivo_inf, '" class="btn-informe">\u2190 Informe ', etiq_periodo(sem_actual_ano, sem_actual_per), '</a>
    <button class="btn-pdf" onclick="window.print()">\u2b07 Descargar PDF</button>
  </div>
</header>
<main>
<h2>Horas pedag\u00f3gicas planeadas</h2>
<p class="nota">Horas totales de los cursos del departamento seg\u00fan tipo de contrato. El semestre en curso aparece destacado.</p>
', html_tabla(c("Periodo","Jornada (HP)","Por hora (HP)","SP (HP)","Total HP"), t1, fila_act), '
<h2>Cobertura seg\u00fan tipo de contrato</h2>
<p class="nota">Porcentaje de horas pedag\u00f3gicas por tipo de contrato sobre el total del departamento.</p>
', html_tabla(c("Periodo","Jornada","Por hora","SP"), t2, fila_act), '
<h2>Promedio de horas por acad\u00e9mico jornada</h2>
<p class="nota">Numerador: HP de todos los jornada (acad\u00e9micos + autoridades). Denominador: solo acad\u00e9micos con CARGO = ACAD\u00c9MICO. Norma: \u2265 ', NORMA_PROM_HP, ' HP.</p>
', html_tabla(c("Periodo","N acad\u00e9micos","HP jornada","Promedio HP",
               paste0("\u2265 ",NORMA_PROM_HP," HP")), t3, fila_act), '
</main>
<div class="pie">Base elaborada a partir de los registros de planeaci\u00f3n docente FAHU. C\u00f3digo elaborado por Pablo Valenzuela con apoyo de Claude de Anthropic.</div>
</body>
</html>')

  ruta <- file.path(CARPETA_COMP, paste0(slug(UP), ".html"))
  writeLines(html_out, ruta, useBytes = TRUE)
  message(glue("    OK {basename(ruta)}"))
}

# =============================================================================
# EJECUCIÓN
# =============================================================================

if (!dir.exists(CARPETA_COMP)) dir.create(CARPETA_COMP, recursive=TRUE)
message(glue("\nGenerando {length(UNIDADES)} comparativos...\n"))
generados <- character(0); errores <- character(0)

for (u in UNIDADES) {
  message(glue("  {u}..."))
  tryCatch({
    generar_comparativo(u)
    generados <- c(generados, slug(u))
  }, error = function(e) {
    message(glue("    ERROR: {conditionMessage(e)}"))
    errores <<- c(errores, u)
  })
}

cat("\n=====================================================\n")
cat("  COMPARATIVOS GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Total: {length(generados)} informe(s)\n"))
if (length(errores) > 0)
  cat(glue("  Fallaron: {paste(errores, collapse=', ')}\n"))
cat("\n")
