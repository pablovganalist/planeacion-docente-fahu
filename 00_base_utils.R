detectar_columna_base <- function(columnas, alias_limpios) {
  columnas_limpias <- janitor::make_clean_names(columnas)
  idx <- match(TRUE, columnas_limpias %in% alias_limpios, nomatch = 0L)
  if (idx == 0L) return(NA_character_)
  columnas[[idx]]
}

resolver_archivo_base <- function(path = "BASE_FAHU.xlsx") {
  if (file.exists(path)) return(path)

  alt <- file.path("..", path)
  if (file.exists(alt)) return(alt)

  path
}

detectar_semestre_actual <- function(path) {
  tmp <- readxl::read_excel(path, col_types = "text")
  col_ano <- detectar_columna_base(names(tmp), c("ano", "a_o", "a__o", "anio", "year"))
  col_per <- detectar_columna_base(names(tmp), c("periodo"))

  if (is.na(col_ano) || is.na(col_per)) {
    stop("No pude detectar las columnas de ano/year y periodo en la base.")
  }

  combos <- tibble::tibble(
    ano = suppressWarnings(as.integer(tmp[[col_ano]])),
    periodo = suppressWarnings(as.integer(tmp[[col_per]]))
  ) |>
    dplyr::filter(!is.na(ano), !is.na(periodo)) |>
    dplyr::distinct() |>
    dplyr::arrange(ano, periodo)

  if (nrow(combos) == 0) {
    stop("No hay combinaciones validas de ano/year y periodo en la base.")
  }

  ult <- combos[nrow(combos), , drop = FALSE]

  list(
    ano = ult$ano[[1]],
    periodo = ult$periodo[[1]]
  )
}

normalizar_base_planeacion <- function(df) {
  df |>
    janitor::clean_names() |>
    dplyr::rename_with(~ "ano", dplyr::any_of(c("ano", "a_o", "a__o", "anio", "year"))) |>
    dplyr::rename_with(~ "eq_cron", dplyr::any_of(c("eq_cron", "eqcron"))) |>
    dplyr::rename(
      unidad_dep = unidad,
      unidad_prof = unidad_profesor,
      curso = asignatura,
      cod = codcur,
      pre = prepost,
      cupo = cupos,
      insc = inscritos
    ) |>
    dplyr::rename_with(~ "horas_ped", dplyr::any_of(c("horas_ped", "horasped"))) |>
    dplyr::rename_with(~ "horas_plan", dplyr::any_of(c("horas_plan", "horasplan")))
}
