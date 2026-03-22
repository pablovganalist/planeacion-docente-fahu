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

mapa_nombres_unidad <- function() {
  c(
    "EDUCACIÓN" = "Departamento de Educación",
    "EDUCACION" = "Departamento de Educación",
    "ESTUDIOS POLÍTICOS" = "Departamento de Estudios Políticos",
    "ESTUDIOS POLITICOS" = "Departamento de Estudios Políticos",
    "FILOSOFÍA" = "Departamento de Filosofía",
    "FILOSOFIA" = "Departamento de Filosofía",
    "HISTORIA" = "Departamento de Historia",
    "LINGÜÍSTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
    "LINGUISTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
    "PERIODISMO" = "Escuela de Periodismo",
    "PSICOLOGÍA" = "Escuela de Psicología",
    "PSICOLOGIA" = "Escuela de Psicología"
  )
}

titulo_esp_base <- function(x) {
  menores <- c("de","del","en","y","e","con","para","la","el","los","las",
               "un","una","por","sin","a","o","u","al","sobre","bajo","ante",
               "desde","hasta","entre","segun","según","hacia","tras")
  x <- stringr::str_squish(as.character(x))
  palabras <- stringr::str_split(stringr::str_to_lower(x), "\\s+")[[1]]
  if (length(palabras) == 0) return(x)
  resultado <- purrr::imap_chr(palabras, function(w, i) {
    if (i == 1 || !(w %in% menores)) {
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    } else {
      w
    }
  })
  paste(resultado, collapse = " ")
}

nombre_largo_unidad <- function(unidad_prof, unidad_dep_fallback = NULL) {
  mapa <- mapa_nombres_unidad()
  clave <- stringr::str_to_upper(stringr::str_squish(as.character(unidad_prof)))
  nombre <- unname(mapa[[clave]])

  if (!is.null(nombre) && !is.na(nombre)) {
    return(nombre)
  }

  if (!is.null(unidad_dep_fallback) && !is.na(unidad_dep_fallback) &&
      nzchar(stringr::str_squish(unidad_dep_fallback))) {
    return(titulo_esp_base(unidad_dep_fallback))
  }

  titulo_esp_base(clave)
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
