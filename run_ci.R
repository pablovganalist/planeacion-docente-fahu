# =============================================================================
# run_ci.R — Script maestro para GitHub Actions
# Ejecuta el pipeline completo: limpieza → informes → HTML
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
library(openxlsx)

# ── Configuración ──────────────────────────────────────────────────────────────
ARCHIVO   <- "BASE_SEMESTRE.xlsx"
PERIODO   <- "Primer semestre 2026"
PLANTILLA <- "reporte_planeacion.Rmd"

EXCLUIR_UNIDADES <- c("HORA DE CLASE", "SIN PROFESOR", "SP",
                      "DERECHO", "FAE", "IDEA")

CARPETA_MD   <- "informes_md"
CARPETA_HTML <- "docs"          # GitHub Pages sirve desde /docs

INSTITUCION  <- "Facultad de Humanidades"

cat("=============================================================\n")
cat("  PIPELINE PLANEACIÓN DOCENTE — FAHU USACH\n")
cat(glue("  {PERIODO}\n"))
cat("=============================================================\n\n")

# ── 1. Limpieza y análisis (genera Excel resumen) ─────────────────────────────
cat("── Paso 1: limpieza y análisis...\n")
source("01_limpieza_analisis.R", local = TRUE)
cat("   OK\n\n")

# ── 2. Generar informes .md por unidad ────────────────────────────────────────
cat("── Paso 2: generando informes .md...\n")
source("02_generar_reportes.R", local = TRUE)
cat("   OK\n\n")

# ── 3. Convertir .md a HTML institucional ─────────────────────────────────────
cat("── Paso 3: exportando a HTML...\n")
source("04_exportar_html.R", local = TRUE)
cat("   OK\n\n")

# ── 4. Informes comparativos desde base histórica ────────────────────────────
cat("── Paso 4: generando comparativos históricos...\n")
if (file.exists("BASE_HISTORICA_FAHU.xlsx")) {
  source("05_comparativo.R", local = TRUE)
  cat("   OK\n\n")
} else {
  cat("   OMITIDO (BASE_HISTORICA_FAHU.xlsx no encontrada)\n\n")
}

# ── 5. Exportar archivos LaTeX ────────────────────────────────────────────────
cat("── Paso 5: exportando a LaTeX...\n")
source("06_exportar_latex.R", local = TRUE)
cat("   OK\n\n")

cat("=============================================================\n")
cat(glue("  Informes disponibles en: {CARPETA_HTML}/\n"))
cat("=============================================================\n")
