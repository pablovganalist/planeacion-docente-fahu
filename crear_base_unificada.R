# =============================================================================
# crear_base_unificada.R
# Crea BASE_FAHU.xlsx unificando BASE_SEMESTRE.xlsx y BASE_HISTORICA_FAHU.xlsx.
# Corre UNA SOLA VEZ localmente — no es parte del pipeline CI.
#
# Reglas:
#   - Estandariza nombres de columnas (HORAS PLAN → HORASPLAN, etc.)
#   - Verifica que no haya filas duplicadas entre bases
#   - Ordena por AÑO, PERIODO, UNIDAD, CARRERA
#   - Guarda en BASE_FAHU.xlsx
#
# Uso futuro: cuando llegue un nuevo semestre, añadir las filas nuevas
# directamente a BASE_FAHU.xlsx (el pipeline detecta el semestre más
# reciente automáticamente).
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)
library(openxlsx)

ARCHIVO_SEM  <- "BASE_SEMESTRE.xlsx"
ARCHIVO_HIST <- "BASE_HISTORICA_FAHU.xlsx"
ARCHIVO_OUT  <- "BASE_FAHU.xlsx"

# -- Función de carga con estandarización de columnas -----------------------
cargar_estandarizar <- function(path) {
  df <- read_excel(path) |>
    clean_names() |>
    # Estandarizar nombres inconsistentes entre las dos bases
    rename_with(~"horas_plan", any_of(c("horasplan", "horas_plan"))) |>
    rename_with(~"horas_ped",  any_of(c("horasped",  "horas_ped")))  |>
    # Estandarizar tipos
    mutate(
      ano     = as.integer(ano),
      periodo = as.integer(periodo),
      nsec    = as.integer(nsec),
      nivel   = as.integer(nivel),
      across(c(t, e, l, s, horas_plan, horas_ped, eq_cron, cupos),
             ~suppressWarnings(as.numeric(.))),
      inscritos = suppressWarnings(as.numeric(inscritos)),
      ocup      = suppressWarnings(as.numeric(ocup)),
      # Estandarizar strings
      across(where(is.character), ~str_squish(str_to_upper(.)))
    )
  message("  Cargado: ", path, " — ", nrow(df), " filas")
  message("  Periodos: ", paste(
    df |> distinct(ano, periodo) |> arrange(ano, periodo) |>
      mutate(e=paste0(ano,"-",periodo)) |> pull(e),
    collapse=" | "))
  df
}

# -- Cargar ----------------------------------------------------------------
message("\n── Cargando bases...")
df_sem  <- cargar_estandarizar(ARCHIVO_SEM)
df_hist <- cargar_estandarizar(ARCHIVO_HIST)

# -- Verificar que no se solapen periodos -----------------------------------
message("\n── Verificando solapamiento de periodos...")
periodos_sem  <- df_sem  |> distinct(ano, periodo)
periodos_hist <- df_hist |> distinct(ano, periodo)
solapados <- inner_join(periodos_sem, periodos_hist, by=c("ano","periodo"))

if (nrow(solapados) > 0) {
  warning("¡Periodos solapados entre bases! Revisar antes de continuar:\n",
          paste(solapados$ano, solapados$periodo, sep="-", collapse=", "))
  cat("\nPeriodos solapados detectados:\n")
  print(solapados)
  cat("\n¿Continuar de todas formas? (s/n): ")
  resp <- readline()
  if (tolower(resp) != "s") stop("Abortado.")
} else {
  message("  OK — sin solapamiento")
}

# -- Verificar columnas compatibles ----------------------------------------
message("\n── Verificando compatibilidad de columnas...")
cols_sem  <- sort(names(df_sem))
cols_hist <- sort(names(df_hist))

solo_sem  <- setdiff(cols_sem,  cols_hist)
solo_hist <- setdiff(cols_hist, cols_sem)

if (length(solo_sem) > 0)
  message("  Solo en SEMESTRE: ", paste(solo_sem, collapse=", "))
if (length(solo_hist) > 0)
  message("  Solo en HISTORICA: ", paste(solo_hist, collapse=", "))
if (length(solo_sem)==0 && length(solo_hist)==0)
  message("  OK — columnas idénticas")

# -- Unificar --------------------------------------------------------------
message("\n── Unificando...")
df_unif <- bind_rows(df_hist, df_sem) |>
  arrange(ano, periodo, unidad, carrera, codprog, nivel, codcur, nsec)

message("  Total filas unificadas: ", nrow(df_unif))
message("  Periodos resultantes:")
df_unif |> count(ano, periodo) |> arrange(ano, periodo) |>
  mutate(s=paste0("    ", ano, "-", periodo, ": ", n, " filas")) |>
  pull(s) |> message()

# -- Restaurar nombres de columnas con mayúsculas para Excel ---------------
# (igual que las bases originales para compatibilidad visual)
df_export <- df_unif |>
  rename(
    UNIDAD          = unidad,
    CARRERA         = carrera,
    CODPROG         = codprog,
    "AÑO"           = ano,
    PERIODO         = periodo,
    PREPOST         = prepost,
    NIVEL           = nivel,
    CODCUR          = codcur,
    ASIGNATURA      = asignatura,
    TIPO            = tipo,
    NSEC            = nsec,
    T               = t,
    E               = e,
    L               = l,
    S               = s,
    HORASPLAN       = horas_plan,
    HORASPED        = horas_ped,
    "EQ CRON"       = eq_cron,
    CUPOS           = cupos,
    INSCRITOS       = inscritos,
    OCUP            = ocup,
    PROFESOR        = profesor,
    CARGO           = cargo,
    "UNIDAD PROFESOR" = unidad_profesor,
    CONTRATO        = contrato
  )

# -- Guardar ---------------------------------------------------------------
message("\n── Guardando ", ARCHIVO_OUT, "...")

wb <- createWorkbook()
addWorksheet(wb, "BASE_FAHU")

# Estilo encabezado
hdr_style <- createStyle(
  fgFill     = "#394049",
  fontColour = "#FFFFFF",
  textDecoration = "bold",
  halign     = "center",
  border     = "Bottom",
  borderColour = "#00A499"
)

writeDataTable(wb, sheet=1, x=df_export,
               tableName="BASE_FAHU",
               tableStyle="TableStyleMedium2")

addStyle(wb, sheet=1, style=hdr_style,
         rows=1, cols=1:ncol(df_export), gridExpand=TRUE)

# Anchos de columna aproximados
setColWidths(wb, sheet=1,
             cols = 1:ncol(df_export),
             widths = c(22, 40, 8, 5, 7, 8, 6, 8, 50, 5, 5,
                        5, 5, 5, 5, 10, 10, 8, 6, 9, 6,
                        28, 16, 28, 10))

saveWorkbook(wb, ARCHIVO_OUT, overwrite=TRUE)

message("  OK — guardado: ", ARCHIVO_OUT)
message("\n=====================================================")
message("  BASE_FAHU.xlsx creada exitosamente")
message("  Filas: ", nrow(df_export))
message("  Periodos: ", n_distinct(paste(df_export$"AÑO", df_export$PERIODO)))
message("=====================================================\n")
message("Próximos pasos:")
message("  1. Revisar BASE_FAHU.xlsx en Excel")
message("  2. Subir al repo y eliminar BASE_SEMESTRE.xlsx y BASE_HISTORICA_FAHU.xlsx")
message("  3. Hacer commit y push — el pipeline se activa automáticamente")
