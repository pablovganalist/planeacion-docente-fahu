# =============================================================================
# 02_generar_reportes.R
# Genera un informe .md por cada unidad docente a partir de reporte_planeacion.Rmd
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)
library(glue)
library(knitr)

source("00_base_utils.R", local = TRUE)

# ── Configuracion ─────────────────────────────────────────────────────────────
ARCHIVO    <- if (exists("ARCHIVO")) ARCHIVO else "BASE_FAHU.xlsx"
ARCHIVO    <- resolver_archivo_base(ARCHIVO)
PLANTILLA  <- if (exists("PLANTILLA")) PLANTILLA else "reporte_planeacion.Rmd"
CARPETA_MD <- if (exists("CARPETA_MD")) CARPETA_MD else "informes_md"

if (!exists("ANO_SEM") || !exists("PER_SEM")) {
  .sem <- detectar_semestre_actual(ARCHIVO)
  ANO_SEM <- .sem$ano
  PER_SEM <- .sem$periodo
  rm(.sem)
}

if (!exists("PERIODO")) {
  PERIODO <- if (PER_SEM == 1) {
    paste("Primer semestre", ANO_SEM)
  } else {
    paste("Segundo semestre", ANO_SEM)
  }
}

EXCLUIR_UNIDADES <- if (exists("EXCLUIR_UNIDADES")) {
  EXCLUIR_UNIDADES
} else {
  c("HORA DE CLASE", "SIN PROFESOR", "SP", "POR HORA", "DERECHO", "FAE", "IDEA")
}

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

generar_informes <- function(
    archivo = ARCHIVO,
    plantilla = PLANTILLA,
    periodo = PERIODO,
    carpeta_md = CARPETA_MD,
    unidades_incl = NULL,
    excluir = EXCLUIR_UNIDADES
) {
  if (!file.exists(archivo)) {
    stop(glue("No se encuentra la base: '{archivo}'"))
  }

  if (!file.exists(plantilla)) {
    stop(glue("No se encuentra la plantilla: '{plantilla}'"))
  }

  if (!dir.exists(carpeta_md)) dir.create(carpeta_md, recursive = TRUE)
  unlink(list.files(carpeta_md, pattern = "\\.md$", full.names = TRUE))

  todas <- read_excel(archivo, sheet = 1, col_types = "text") |>
    normalizar_base_planeacion() |>
    mutate(unidad_prof = str_to_upper(str_squish(unidad_prof))) |>
    filter(!is.na(unidad_prof), unidad_prof != "") |>
    filter(!unidad_prof %in% str_to_upper(excluir)) |>
    distinct(unidad_prof) |>
    arrange(unidad_prof) |>
    pull(unidad_prof)

  unidades <- if (is.null(unidades_incl)) {
    todas
  } else {
    intersect(todas, str_to_upper(unidades_incl))
  }

  if (length(unidades) == 0) {
    stop("No se encontraron unidades para generar informes.")
  }

  generados <- character(0)
  errores <- character(0)
  manifest <- vector("list", length(unidades))

  message(glue("Generando {length(unidades)} informes .md..."))

  for (i in seq_along(unidades)) {
    unidad <- unidades[[i]]
    nombre <- glue("{str_pad(i, 2, pad = '0')}_{slug(unidad)}.md")
    ruta_salida <- file.path(carpeta_md, nombre)

    message(glue("  [{i}/{length(unidades)}] {unidad}..."))

    tryCatch({
      entorno <- new.env(parent = globalenv())
      entorno$params <- list(
        archivo = archivo,
        unidad = unidad,
        periodo = periodo
      )

      knitr::knit(
        input = plantilla,
        output = normalizePath(ruta_salida, mustWork = FALSE),
        envir = entorno,
        quiet = TRUE
      )

      lineas <- readLines(ruta_salida, encoding = "UTF-8", warn = FALSE)
      lineas <- strip_front_matter(lineas)
      writeLines(lineas, ruta_salida, useBytes = TRUE)

      titulo <- lineas[str_detect(lineas, "^# ")][1]
      titulo <- if (is.na(titulo)) nombre_largo_unidad(unidad) else str_remove(titulo, "^#\\s+")

      manifest[[i]] <- tibble(
        unidad_prof = unidad,
        nombre_largo = titulo,
        nombre_indice = str_to_upper(titulo),
        slug = slug(unidad),
        md_file = nombre,
        html_file = paste0(tools::file_path_sans_ext(nombre), ".html")
      )

      generados <- c(generados, nombre)
      message(glue("    OK {nombre}"))
    }, error = function(e) {
      errores <<- c(errores, unidad)
      message(glue("    ERROR: {conditionMessage(e)}"))
    })
  }

  cat("\n=====================================================\n")
  cat("  INFORMES MD GENERADOS\n")
  cat("=====================================================\n")
  cat(glue("  Carpeta:  {carpeta_md}/\n"))
  cat(glue("  Periodo:  {periodo}\n"))
  cat(glue("  Total:    {length(generados)} informe(s)\n"))
  if (length(errores) > 0) {
    cat(glue("  Fallaron: {paste(errores, collapse = ', ')}\n"))
  }
  cat("\n")

  manifest_df <- bind_rows(manifest)
  readr::write_csv(manifest_df, file.path(carpeta_md, "_manifest_informes.csv"))

  invisible(generados)
}

generar_informes()
