# =============================================================================
# 01_limpieza_analisis.R  —  Planeacion docente 1-2026
# =============================================================================
# Reglas de negocio implementadas:
#
#  1. HORAS EXTERNAS
#     Las horas de un academico de jornada se suman a su UNIDAD PROFESOR
#     (no a la UNIDAD donde figura el curso). Un profesor de HISTORIA que
#     dicta en ESTUDIOS POLITICOS suma sus horas a HISTORIA.
#
#  2. CLAUSTRO
#     Solo CARGO == "ACADEMICO" entra al calculo del promedio de docencia.
#     Las autoridades (DIRECTOR, DECANA, etc.) tienen horas en la unidad
#     pero no se consideran en el promedio del claustro.
#
#  3. CURSOS SELLO  (SEC empieza con "S")
#     El mismo sello puede aparecer en multiples filas para el mismo profesor
#     (una fila por UNIDAD donde se dicta). Se cuenta UNA SOLA VEZ por
#     combinacion PROFESOR + SEC al calcular las horas del docente.
#
#  4. INSC
#     Se deja como NA — se completara cuando los estudiantes inscriban.
#
#  5. UNIDAD PROFESOR
#     Ya viene completa en esta base (no hay NAs).
#     "HORA DE CLASE" = profesores por hora sin unidad de planta.
#     "SIN PROFESOR"  = secciones sin docente asignado.
# =============================================================================

# install.packages(c("tidyverse","readxl","janitor","writexl","glue","scales"))
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(glue)

# ── Configuracion ─────────────────────────────────────────────────────────────
# setwd: no necesario en CI (wd = raíz del repo)

ARCHIVO        <- "BASE_SEMESTRE.xlsx"
PERIODO        <- "Primer semestre 2026"
CARPETA_SALIDA <- "output"

if (!dir.exists(CARPETA_SALIDA)) dir.create(CARPETA_SALIDA)

# Categorias de CARGO que forman el claustro academico
CARGOS_CLAUSTRO <- "ACADEMICO"

# Cargos que tienen horas en la unidad pero NO entran al promedio
CARGOS_AUTORIDAD <- c("DIRECTOR", "DIRECTORA", "DECANA", "VICEDECANO", "VIME")

# =============================================================================
# 1. CARGA Y LIMPIEZA BASE
# =============================================================================

df_raw <- read_excel(ARCHIVO, sheet = 1) |>
  clean_names() |>
  rename(
    unidad_dep  = unidad,
    unidad_prof = unidad_profesor,
    horas_ped   = horas_ped,
    eq_cron     = eq_cron,
    curso       = asignatura,
    cod         = codcur,
    pre         = prepost,
    cupo        = cupos,
    insc        = inscritos
  ) |>
  mutate(
    # ── Texto ─────────────────────────────────────────────────────────────────
    profesor    = str_to_title(str_squish(profesor)),
    curso       = str_to_title(str_squish(curso)),
    carrera     = str_to_title(str_squish(carrera)),
    unidad_dep  = str_to_upper(str_squish(unidad_dep)),
    unidad_prof = str_to_upper(str_squish(unidad_prof)),
    cargo       = str_to_upper(str_squish(cargo)),
    pre         = str_to_title(str_squish(pre)),
    tipo        = str_to_upper(str_squish(tipo)),
    sec         = paste0(tipo, "-", nsec),

    # ── Contrato estandarizado ────────────────────────────────────────────────
    tipo_contrato = case_when(
      contrato == "JORNADA"  ~ "Jornada",
      contrato == "POR HORA" ~ "Por Hora",
      contrato == "SP"       ~ "Sin Planta",
      TRUE                   ~ "Sin Informacion"
    ),

    # ── Clasificacion de CARGO ────────────────────────────────────────────────
    tipo_cargo = case_when(
      cargo == "ACADEMICO"                  ~ "Academico",
      cargo == "ACADEMICO POR HORA DE CLASE"~ "Academico Por Hora",
      cargo == "SIN PROFESOR"               ~ "Sin Profesor",
      cargo %in% CARGOS_AUTORIDAD           ~ "Autoridad",
      TRUE                                  ~ "Otro"
    ),

    # Flag: entra al claustro (promedio normativo)
    en_claustro = cargo == CARGOS_CLAUSTRO,

    # ── Cursos sello ──────────────────────────────────────────────────────────
    # SEC que empieza con "S" = curso sello
    es_sello = str_starts(sec, "S"),

    # ── Nivel con etiqueta ────────────────────────────────────────────────────
    nivel_label = case_when(
      nivel == 1  ~ "1 - Primero",  nivel == 2  ~ "2 - Segundo",
      nivel == 3  ~ "3 - Tercero",  nivel == 4  ~ "4 - Cuarto",
      nivel == 5  ~ "5 - Quinto",   nivel == 6  ~ "6 - Sexto",
      nivel == 7  ~ "7 - Septimo",  nivel == 8  ~ "8 - Octavo",
      nivel == 9  ~ "9 - Noveno",   nivel == 10 ~ "10 - Decimo",
      TRUE ~ as.character(nivel)
    ),

    # ── Tipo de seccion ───────────────────────────────────────────────────────
    tipo_sec = case_when(
      str_starts(sec, "T") ~ "Teorica",
      str_starts(sec, "E") ~ "Estudio",
      str_starts(sec, "L") ~ "Laboratorio",
      str_starts(sec, "S") ~ "Sello",
      TRUE                 ~ "Otro"
    ),

    # ── Numericas ─────────────────────────────────────────────────────────────
    across(c(t, e, l, horas_ped, eq_cron, cupo),
           ~ replace_na(as.numeric(.), 0)),
    insc = as.numeric(insc)   # permanece NA hasta inscripcion
  )

message(glue("Base cargada: {nrow(df_raw)} filas"))
message(glue("  CARGO values: {paste(sort(unique(df_raw$cargo)), collapse=' | ')}"))


# =============================================================================
# 2. REGLA DE SELLOS
#    Marcar como duplicado cada fila de sello que repite PROFESOR+SEC
#    (se conserva solo la primera aparicion para sumar horas del profesor)
# =============================================================================

df <- df_raw |>
  mutate(
    # PROFESOR + SEC identifica una sección única.
    # T-1 en 3 carreras = 1 sección. S-27 en 4 unidades = 1 sección.
    # T/E/L: todas las filas cuentan (una seccion por carrera/programa)
    # S:     dedup por PROFESOR + SEC — el mismo sello siempre suma 4h, una vez
    seccion_duplicada = es_sello & duplicated(paste(profesor, sec)),
    horas_prof = if_else(seccion_duplicada, 0L, horas_ped),
    eq_prof    = if_else(seccion_duplicada, 0,  eq_cron)
  )

n_secs_dedup <- sum(df$seccion_duplicada)
message(glue("  Sellos deduplicados: {n_secs_dedup} filas marcadas (horas_prof = 0)"))


# =============================================================================
# 3. FUNCIONES DE ANALISIS
# =============================================================================

# ── 3.1 Horas planeadas de la unidad (estructura informe institucional) ───────
#
# REGLA CLAVE: las horas se agrupan por UNIDAD PROFESOR, no por UNIDAD del curso.
# Esto hace que las horas externas de un academico de jornada se acumulen
# en su unidad de origen.
#
# El desglose A/B/C usa UNIDAD del curso (pregrado/postgrado pertenece
# a la carrera, no al profesor), pero el total por unidad docente usa UNIDAD PROFESOR.

tabla_horas_unidad <- function(df, unidad_prof_val) {

  # Todas las secciones donde el profesor pertenece a esta unidad
  d <- df |> filter(unidad_prof == unidad_prof_val)

  # Para el desglose A/B/C usamos el PRE de la carrera donde se dicta el curso
  d_pre  <- d |> filter(pre == "Pregrado")
  d_post <- d |> filter(pre == "Postgrado")

  h <- function(data, ctrato = NULL, solo_claustro = FALSE) {
    if (!is.null(ctrato)) data <- data |> filter(tipo_contrato == ctrato)
    if (solo_claustro)    data <- data |> filter(en_claustro)
    sum(data$horas_prof, na.rm = TRUE)
  }

  # Jornada externa = jornada de otra unidad que dicta aqui
  # (desde la perspectiva de la UNIDAD DEL CURSO, no del profesor)
  d_cursos <- df |> filter(unidad_dep == unidad_prof_val)
  hext_pre <- d_cursos |>
    filter(pre == "Pregrado", tipo_contrato == "Jornada",
           unidad_prof != unidad_prof_val,
           !unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")) |>
    summarise(h = sum(horas_prof)) |> pull(h)
  hext_post <- d_cursos |>
    filter(pre == "Postgrado", tipo_contrato == "Jornada",
           unidad_prof != unidad_prof_val,
           !unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")) |>
    summarise(h = sum(horas_prof)) |> pull(h)

  tibble(
    codigo = c("A (i+ii+iii+iv)", "i", "ii", "iii", "iv",
               "B (v+vi+vii)",    "v", "vi", "vii",
               "C (A+B)"),
    descripcion = c(
      "Horas planeadas pregrado",
      "Horas profesores de jornada de la unidad",
      "Horas profesores por hora de clase",
      "Horas de profesores de jornada externos a la unidad",
      "Sin profesor asignado",
      "Horas planeadas postgrado",
      "Horas profesores de jornada de la unidad",
      "Horas profesores por hora de clase",
      "Horas de profesores de jornada externos a la unidad",
      "Total horas planeadas"
    ),
    horas = c(
      h(d_pre),
      h(d_pre,  "Jornada"),
      h(d_pre,  "Por Hora"),
      hext_pre,
      h(d_pre |> filter(cargo == "SIN PROFESOR")),
      h(d_post),
      h(d_post, "Jornada"),
      h(d_post, "Por Hora"),
      hext_post,
      h(d)
    )
  )
}

# ── 3.2 Profesores de la unidad (agrupado por UNIDAD PROFESOR) ────────────────
#
# REGLA: un academico suma TODAS sus horas aqui, aunque el curso sea de otra unidad.
# REGLA: sellos deduplicados (horas_prof ya viene con el ajuste).

tabla_profesores_unidad <- function(df, unidad_prof_val) {
  df |>
    filter(
      unidad_prof == unidad_prof_val,
      !unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")
    ) |>
    group_by(profesor, cargo, tipo_cargo, tipo_contrato) |>
    summarise(
      secciones        = n(),
      secciones_sello  = sum(es_sello & !seccion_duplicada),
      cursos_unicos    = n_distinct(cod),
      carreras         = n_distinct(carrera),
      unidades_dictado = n_distinct(unidad_dep),
      horas_ped        = sum(horas_prof, na.rm = TRUE),
      eq_cron          = sum(eq_prof,    na.rm = TRUE),
      cupo_total       = sum(cupo,       na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(en_claustro = tipo_cargo == "Academico"), desc(horas_ped))
}

# ── 3.3 Tabla global de profesores (todas las unidades) ───────────────────────
tabla_profesores_global <- function(df) {
  df |>
    filter(!unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")) |>
    group_by(profesor, cargo, tipo_cargo, tipo_contrato, unidad_prof) |>
    summarise(
      secciones       = n(),
      secciones_sello = sum(es_sello & !seccion_duplicada),
      cursos_unicos   = n_distinct(cod),
      carreras        = n_distinct(carrera),
      horas_ped       = sum(horas_prof, na.rm = TRUE),
      eq_cron         = sum(eq_prof,    na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(horas_ped))
}

# ── 3.4 Resumen por unidad ────────────────────────────────────────────────────
#
# Usa DOS perspectivas con dedup distintos:
#
#  d_profs  = filas donde UNIDAD PROFESOR == u  (perspectiva del profesor)
#             dedup GLOBAL de sellos → para claustro, promedio, % contratos
#
#  d_cursos = filas donde UNIDAD DEP == nombre largo  (perspectiva del curso)
#             dedup LOCAL de sellos → para secciones, cursos, horas de la unidad
#
tabla_resumen_unidades <- function(df) {

  # Mapa unidad_prof (corto) -> unidad_dep (largo, mayusculas)
  mapa_unidad <- df |>
    filter(!unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")) |>
    count(unidad_prof, unidad_dep) |>
    group_by(unidad_prof) |>
    slice_max(n, n = 1) |>
    ungroup() |>
    select(unidad_prof, unidad_dep)

  unidades_validas <- sort(mapa_unidad$unidad_prof)

  map_dfr(unidades_validas, function(u) {

    # Perspectiva del PROFESOR (dedup global — para promedio y claustro)
    d_profs <- df |> filter(unidad_prof == u)

    # Perspectiva de los CURSOS de la unidad (dedup local — para secciones y horas)
    dep_nombre <- mapa_unidad |> filter(unidad_prof == u) |> pull(unidad_dep)

    d_cursos <- df |>
      filter(unidad_dep == dep_nombre) |>
      group_by(profesor, sec) |>
      mutate(seccion_dup_local = es_sello & duplicated(paste(profesor, sec))) |>
      ungroup() |>
      mutate(horas_unidad = if_else(seccion_dup_local, 0L, horas_ped),
             eq_unidad    = if_else(seccion_dup_local, 0,  eq_cron))

    # Claustro y promedio (sobre d_profs con dedup global)
    claustro         <- d_profs |> filter(en_claustro)
    n_claustro       <- n_distinct(claustro$profesor)
    hp_total_jornada <- d_profs |>
      filter(tipo_contrato == "Jornada") |>
      summarise(hp = sum(horas_prof)) |> pull(hp)

    tibble(
      unidad_prof      = u,
      unidad_dep       = dep_nombre,
      # ── Profesores (perspectiva del profesor) ──────────────────────────────
      n_profesores     = n_distinct(d_profs$profesor),
      n_claustro       = n_claustro,
      n_autoridades    = n_distinct(d_profs$profesor[d_profs$tipo_cargo == "Autoridad"]),
      pct_jornada      = round(mean(d_profs$tipo_contrato == "Jornada") * 100, 1),
      pct_por_hora     = round(mean(d_profs$tipo_contrato == "Por Hora") * 100, 1),
      # ── Horas del claustro (promedio correcto) ─────────────────────────────
      horas_jornada    = hp_total_jornada,
      prom_hp_claustro = if (n_claustro > 0)
                           round(hp_total_jornada / n_claustro, 1) else NA_real_,
      # ── Secciones y horas de la unidad (perspectiva del curso, dedup local) ─
      secciones_total  = sum(!d_cursos$seccion_dup_local),
      cursos_total     = n_distinct(d_cursos$cod),
      horas_total      = sum(d_cursos$horas_unidad),
      eq_total         = sum(d_cursos$eq_unidad)
    )
  })
}

# ── 3.5 Alertas ───────────────────────────────────────────────────────────────
tabla_alertas <- function(df, umbral_alto = 20, umbral_bajo_claustro = 12) {

  carga <- df |>
    filter(!unidad_prof %in% c("HORA DE CLASE","SIN PROFESOR")) |>
    group_by(profesor, tipo_cargo, tipo_contrato, unidad_prof) |>
    summarise(
      secciones = n(),
      horas_ped = sum(horas_prof),
      eq_cron   = sum(eq_prof),
      .groups   = "drop"
    )

  list(
    carga_alta = carga |>
      filter(eq_cron >= umbral_alto) |>
      arrange(desc(eq_cron)),

    claustro_bajo = carga |>
      filter(tipo_cargo == "Academico", horas_ped < umbral_bajo_claustro) |>
      arrange(horas_ped),

    autoridades_con_horas = carga |>
      filter(tipo_cargo == "Autoridad") |>
      arrange(desc(horas_ped)),

    sin_cupo = df |>
      filter(cupo == 0, !cargo == "SIN PROFESOR") |>
      distinct(cod, curso, carrera, sec, horas_ped, tipo_cargo, profesor) |>
      arrange(carrera, cod),

    sellos_resumen = df |>
      filter(es_sello) |>
      group_by(profesor, unidad_prof, tipo_cargo) |>
      summarise(
        n_sellos_distintos = n_distinct(sec),
        n_filas_sello      = n(),
        horas_sello        = sum(horas_prof),
        .groups = "drop"
      ) |>
      filter(n_sellos_distintos > 0) |>
      arrange(desc(n_sellos_distintos))
  )
}


# =============================================================================
# 4. COMPUTAR ANALISIS
# =============================================================================

message("\n-- Computando analisis --")

unidades_validas <- df |>
  filter(!unidad_prof %in% c("HORA DE CLASE", "SIN PROFESOR")) |>
  pull(unidad_prof) |> unique() |> sort()

analisis <- list(
  profesores_global = tabla_profesores_global(df),
  resumen_unidades  = tabla_resumen_unidades(df),
  alertas           = tabla_alertas(df),
  horas_por_unidad  = map(
    set_names(unidades_validas, unidades_validas),
    ~ tabla_horas_unidad(df, .x)
  ),
  profesores_por_unidad = map(
    set_names(unidades_validas, unidades_validas),
    ~ tabla_profesores_unidad(df, .x)
  )
)

message("  Analisis completados")


# =============================================================================
# 5. EXPORTAR
# =============================================================================

hojas_excel <- list(
  "Profesores_Global"    = analisis$profesores_global,
  "Resumen_Unidades"     = analisis$resumen_unidades,
  "Alerta_CargaAlta"     = analisis$alertas$carga_alta,
  "Alerta_ClaustroLow"   = analisis$alertas$claustro_bajo,
  "Alerta_Autoridades"   = analisis$alertas$autoridades_con_horas,
  "Alerta_SinCupo"       = analisis$alertas$sin_cupo,
  "Sellos_Resumen"       = analisis$alertas$sellos_resumen
)

# Una hoja por unidad con sus profesores
for (u in unidades_validas) {
  nombre_hoja <- paste0("Profs_", str_trunc(str_to_title(u), 25, ellipsis = ""))
  hojas_excel[[nombre_hoja]] <- analisis$profesores_por_unidad[[u]]
}

write_xlsx(
  hojas_excel,
  path = file.path(CARPETA_SALIDA, "analisis_planeacion_1-2026.xlsx")
)
message(glue("  Excel exportado -> {CARPETA_SALIDA}/analisis_planeacion_1-2026.xlsx"))


# =============================================================================
# 6. RESUMEN EN CONSOLA
# =============================================================================

cat("\n=====================================================\n")
cat(glue("  PLANEACION DOCENTE -- {PERIODO}\n"))
cat("=====================================================\n\n")
cat(glue("  Secciones totales:     {nrow(df)}\n"))
cat(glue("  (secciones deduplicadas:  {sum(df$seccion_duplicada)})\n"))
cat(glue("  Profesores unicos:     {n_distinct(df$profesor)}\n"))
cat(glue("  Cursos unicos:         {n_distinct(df$cod)}\n"))
cat(glue("  Unidades:              {length(unidades_validas)}\n\n"))

cat("-- Distribucion por CARGO --\n")
df |> count(cargo, tipo_cargo) |> arrange(desc(n)) |> print(n = Inf)

cat("\n-- Resumen por unidad (claustro) --\n")
analisis$resumen_unidades |>
  select(unidad_prof, n_claustro, n_autoridades,
         horas_jornada, prom_hp_claustro) |>
  print(n = Inf)

cat("\n-- Alertas --\n")
cat(glue("  EQ >= 20:              {nrow(analisis$alertas$carga_alta)}\n"))
cat(glue("  Claustro < 12 HP:      {nrow(analisis$alertas$claustro_bajo)}\n"))
cat(glue("  Autoridades con horas: {nrow(analisis$alertas$autoridades_con_horas)}\n"))
cat(glue("  Secciones sin cupo:    {nrow(analisis$alertas$sin_cupo)}\n"))

# Exponer globalmente para los scripts siguientes
datos    <<- df
analisis <<- analisis
unidades_validas <<- unidades_validas

message("\nObjetos disponibles: `datos`, `analisis`, `unidades_validas`")
message("Siguiente paso: source('02_generar_reportes.R')\n")
