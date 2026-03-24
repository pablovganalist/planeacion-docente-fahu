# =============================================================================
# run_ci.R — Script maestro para GitHub Actions
# Ejecuta el pipeline completo:
#   1. Limpieza y análisis
#   2. Informes .md por unidad
#   3. HTML por unidad (+ HTMLs por carrera)
#   4. Comparativos históricos
#   5. Exportar LaTeX
#   6. Síntesis por programa
#   7. Tabla inscritos sellos (HTML + PDF)
#   8. Informes y presentaciones PDF por carrera (Quarto + Typst)
# =============================================================================
# El working directory ya es la raíz del repo en el entorno CI.
# Las rutas son relativas a esa raíz.
# =============================================================================

library(rmarkdown)
library(readxl)
library(janitor)
library(tidyverse)
library(glue)
library(knitr)
library(quarto)
library(withr)
source("00_base_utils.R", local = TRUE)

# ── Configuración ──────────────────────────────────────────────────────────────
ARCHIVO   <- if (exists("ARCHIVO")) ARCHIVO else "BASE_FAHU.xlsx"
ARCHIVO   <- resolver_archivo_base(ARCHIVO)
PLANTILLA <- "reporte_planeacion.Rmd"

EXCLUIR_UNIDADES <- c("HORA DE CLASE", "SIN PROFESOR", "SP", "POR HORA",
                      "DERECHO", "FAE", "IDEA")

CARPETA_MD   <- "informes_md"
CARPETA_HTML <- "docs"          # GitHub Pages sirve desde /docs

INSTITUCION  <- "Facultad de Humanidades"

# Detectar semestre actual (el más reciente en la base)
.sem_actual <- detectar_semestre_actual(ARCHIVO)
ANO_SEM    <- .sem_actual$ano
PER_SEM    <- .sem_actual$periodo
PERIODO    <- if (PER_SEM==1) paste("Primer semestre",  ANO_SEM) else
                               paste("Segundo semestre", ANO_SEM)
rm(.sem_actual)

cat("=============================================================\n")
cat("  PIPELINE PLANEACIÓN DOCENTE — FAHU USACH\n")
cat(glue("  {PERIODO} (detectado automáticamente)\n"))
cat("=============================================================\n\n")

# ── 1. Limpieza y análisis (genera Excel resumen) ─────────────────────────────
cat("── Paso 1: limpieza y análisis...\n")
source("01_limpieza_analisis.R", local = TRUE)
cat("   OK\n\n")

# ── 2. Generar informes .md por unidad ────────────────────────────────────────
cat("── Paso 2: generando informes .md...\n")
if (file.exists("02_generar_reportes.R")) {
  source("02_generar_reportes.R", local = TRUE)
} else {
  cat("   OMITIDO (no existe 02_generar_reportes.R)\n")
}
cat("   OK\n\n")

# ── 3. Convertir .md a HTML institucional ─────────────────────────────────────
cat("── Paso 3: exportando a HTML...\n")
if (file.exists("04_exportar_html.R")) {
  source("04_exportar_html.R", local = TRUE)
} else {
  cat("   OMITIDO (no existe 04_exportar_html.R)\n")
}
cat("   OK\n\n")

# ── 4. Informes comparativos históricos ──────────────────────────────────────
cat("── Paso 4: generando comparativos históricos...\n")
source("05_comparativo.R", local = TRUE)
cat("   OK\n\n")

# ── 5. Exportar archivos LaTeX ────────────────────────────────────────────────
cat("── Paso 5: exportando a LaTeX...\n")
source("06_exportar_latex.R", local = TRUE)
cat("   OK\n\n")

# ── 6. Síntesis por carrera y programa ────────────────────────────────────────
cat("── Paso 6: generando síntesis por programa...\n")
source("07_sintesis_programas.R", local = TRUE)
cat("   OK\n\n")

# ── 7. Tabla de inscritos en cursos sello ────────────────────────────────────
cat("── Paso 7: tabla inscritos sellos (HTML + PDF)...\n")
if (file.exists("exp_inscritos_sellos.R")) {
  tryCatch(
    source("exp_inscritos_sellos.R", local = TRUE),
    error = function(e) cat("   ADVERTENCIA:", conditionMessage(e), "\n")
  )
} else {
  cat("   OMITIDO (no existe exp_inscritos_sellos.R)\n")
}
cat("   OK\n\n")

# ── 8. Informes y presentaciones PDF por carrera (Quarto + Typst) ────────────
cat("── Paso 8: informes y presentaciones PDF por carrera...\n")
if (file.exists("render_informes_quarto.R")) {
  # Asegura que Quarto sea encontrado en CI y en local (RStudio embebido)
  if (!nzchar(Sys.getenv("QUARTO_PATH")) && nzchar(Sys.which("quarto"))) {
    Sys.setenv(QUARTO_PATH = Sys.which("quarto"))
  }
  tryCatch(
    source("render_informes_quarto.R", local = TRUE),
    error = function(e) cat("   ADVERTENCIA:", conditionMessage(e), "\n")
  )
} else {
  cat("   OMITIDO (no existe render_informes_quarto.R)\n")
}
cat("   OK\n\n")

cat("=============================================================\n")
cat(glue("  Informes disponibles en: {CARPETA_HTML}/\n"))
cat("=============================================================\n")
