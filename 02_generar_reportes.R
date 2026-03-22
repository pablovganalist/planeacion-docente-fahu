# =============================================================================
# 02_generar_reportes.R
# Genera un informe .md por cada unidad docente
# =============================================================================
# IMPORTANTE: itera sobre UNIDAD PROFESOR (valores cortos como "HISTORIA",
# "LINGÜÍSTICA") — NO sobre el nombre largo del departamento. El Rmd recibe
# ese valor corto y lo usa para filtrar correctamente.
# =============================================================================

library(rmarkdown)
library(readxl)
library(janitor)
library(tidyverse)
library(glue)

# ── Configuracion ─────────────────────────────────────────────────────────────
# setwd: no necesario en CI (wd = raíz del repo)

ARCHIVO <- if (exists("ARCHIVO")) ARCHIVO else "BASE_FAHU.xlsx"
PLANTILLA     <- "reporte_planeacion.Rmd"
PERIODO       <- "Primer semestre 2026"
CARPETA_MD    <- "informes_md"

# Valores a excluir de UNIDAD PROFESOR (no generan informe propio)
EXCLUIR_UNIDADES <- c("HORA DE CLASE", "SIN PROFESOR", "SP",
                      "DERECHO", "FAE", "IDEA")
# Ajusta EXCLUIR_UNIDADES si quieres incluir o excluir unidades externas

# ── Helper: nombre de archivo seguro ──────────────────────────────────────────
slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[\u00e1\u00e0\u00e4]", "a") |>
    str_replace_all("[\u00e9\u00e8\u00eb]", "e") |>
    str_replace_all("[\u00ed\u00ec\u00ef]", "i") |>
    str_replace_all("[\u00f3\u00f2\u00f6]", "o") |>
    str_replace_all("[\u00fa\u00f9\u00fc]", "u") |>
    str_replace_all("\u00f1", "n") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("^_|_$")
}

# ── Funcion principal ─────────────────────────────────────────────────────────
generar_informes <- function(
    archivo       = ARCHIVO,
    plantilla     = PLANTILLA,
    periodo       = PERIODO,
    carpeta_md    = CARPETA_MD,
    unidades_incl = NULL,    # NULL = todas; o vector de valores UNIDAD PROFESOR
    excluir       = EXCLUIR_UNIDADES
) {

  if (!file.exists(archivo))
    stop(glue(
      "No se encuentra: '{archivo}'\n",
      "Directorio actual: {getwd()}"
    ))

  if (!file.exists(plantilla))
    stop(glue(
      "No se encuentra la plantilla: '{plantilla}'\n",
      "Directorio actual: {getwd()}"
    ))

  if (!dir.exists(carpeta_md)) dir.create(carpeta_md, recursive = TRUE)

  # Leer unidades desde UNIDAD PROFESOR (no desde UNIDAD del curso)
  todas <- read_excel(archivo, sheet = 1) |>
    clean_names() |>
    rename(unidad_prof = unidad_profesor) |>
    mutate(unidad_prof = str_to_upper(str_squish(unidad_prof))) |>
    filter(!unidad_prof %in% str_to_upper(excluir)) |>
    pull(unidad_prof) |>
    unique() |>
    sort()

  unidades <- if (!is.null(unidades_incl)) {
    intersect(todas, str_to_upper(unidades_incl))
  } else {
    todas
  }

  if (length(unidades) == 0)
    stop("No se encontraron unidades. Revisa los filtros.")

  message(glue("Directorio: {getwd()}"))
  message(glue("Unidades a procesar: {length(unidades)}"))
  message(glue("  {paste(unidades, collapse=', ')}\n"))

  generados <- character(0)
  errores   <- character(0)

  for (i in seq_along(unidades)) {
    u      <- unidades[i]
    nombre <- glue("{str_pad(i, 2, pad='0')}_{slug(u)}.md")
    ruta   <- file.path(carpeta_md, nombre)

    message(glue("[{i}/{length(unidades)}] {u}..."))

    tryCatch({
      render(
        input       = plantilla,
        output_file = normalizePath(ruta, mustWork = FALSE),
        params      = list(
          archivo = archivo,
          unidad  = u,        # <- valor corto: "HISTORIA", "LINGÜÍSTICA", etc.
          periodo = periodo
        ),
        quiet = TRUE
      )
      message(glue("    OK {nombre}"))
      generados <- c(generados, nombre)
    },
    error = function(e) {
      message(glue("    ERROR: {conditionMessage(e)}"))
      errores <<- c(errores, u)
    })
  }

  cat("\n=====================================================\n")
  cat("  INFORMES GENERADOS\n")
  cat("=====================================================\n")
  cat(glue("  Carpeta:   {carpeta_md}/\n"))
  cat(glue("  Periodo:   {periodo}\n"))
  cat(glue("  Total:     {length(generados)} informe(s)\n"))

  if (length(errores) > 0)
    cat(glue("  Fallaron:  {paste(errores, collapse=', ')}\n"))

  cat("\n  Archivos:\n")
  walk(generados, ~ cat("    -", .x, "\n"))
  cat("\n")

  invisible(generados)
}

# =============================================================================
# EJECUCION
# =============================================================================

# Todos los departamentos propios:
generar_informes()

# Solo una unidad (usar el valor corto de UNIDAD PROFESOR):
# generar_informes(unidades_incl = "HISTORIA")

# Varias:
# generar_informes(unidades_incl = c("HISTORIA", "LINGÜÍSTICA", "PSICOLOGÍA"))

# Otro semestre:
# generar_informes(
#   archivo    = "BASE22026.xlsx",
#   periodo    = "Segundo semestre 2026",
#   carpeta_md = "informes_md_2026_2"
# )
