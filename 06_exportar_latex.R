# =============================================================================
# 06_exportar_latex.R
# Genera un .tex por unidad combinando:
#   - Informe del semestre actual (misma lógica que reporte_planeacion.Rmd)
#   - Tablas comparativas históricas (misma lógica que 05_comparativo.R)
#
# Output: docs/latex/{slug}.tex — uno por unidad, listo para Overleaf
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)
library(glue)

# ── Configuración ──────────────────────────────────────────────────────────────
ARCHIVO_SEM    <- "BASE_SEMESTRE.xlsx"
ARCHIVO_HIST   <- "BASE_HISTORICA_FAHU.xlsx"
CARPETA_LATEX  <- file.path("docs", "latex")
INSTITUCION    <- "Facultad de Humanidades"
PERIODO        <- "Primer semestre 2026"
NORMA_PROM_HP  <- 12

CARGOS_AUTORIDAD <- c("DIRECTOR","DIRECTORA","DECANA","VICEDECANO",
                      "VIME","DIRECTORA INNED")

NOMBRES_UNIDAD <- c(
  "HISTORIA"                 = "Departamento de Historia",
  "EDUCACIÓN"                = "Departamento de Educación",
  "ESTUDIOS POLÍTICOS"       = "Departamento de Estudios Políticos",
  "FILOSOFÍA"                = "Departamento de Filosofía",
  "LINGÜÍSTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
  "PERIODISMO"               = "Escuela de Periodismo",
  "PSICOLOGÍA"               = "Escuela de Psicología"
)

# ── Helpers ────────────────────────────────────────────────────────────────────
slug <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("[áàä]","a") |> str_replace_all("[éèë]","e") |>
    str_replace_all("[íìï]","i") |> str_replace_all("[óòö]","o") |>
    str_replace_all("[úùü]","u") |> str_replace_all("ñ","n") |>
    str_replace_all("[^a-z0-9]+","_") |> str_remove("^_|_$")
}

# Escapar caracteres especiales de LaTeX
tex_esc <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\textbackslash{}")
  x <- str_replace_all(x, "&",  "\\\\&")
  x <- str_replace_all(x, "%",  "\\\\%")
  x <- str_replace_all(x, "\\$", "\\\\$")
  x <- str_replace_all(x, "#",  "\\\\#")
  x <- str_replace_all(x, "_",  "\\\\_")
  x <- str_replace_all(x, "\\{", "\\\\{")
  x <- str_replace_all(x, "\\}", "\\\\}")
  x <- str_replace_all(x, "~",  "\\\\textasciitilde{}")
  x <- str_replace_all(x, "\\^", "\\\\textasciicircum{}")
  x
}

fmt_hp  <- function(x) ifelse(is.na(x) | x == 0, "---",
                               formatC(x, format="f", digits=0, big.mark="."))
fmt_pct <- function(x) ifelse(is.na(x), "---",
                               paste0(formatC(x*100, format="f", digits=1), "\\%"))

etiq_periodo <- function(anio, per) {
  paste0(if (per == 1) "1\\textsuperscript{er} sem. " else "2\\textsuperscript{do} sem. ", anio)
}

# ── Preámbulo LaTeX ────────────────────────────────────────────────────────────
preambulo <- function(titulo_doc) {
  glue(
'\\\\documentclass[11pt, a4paper]{{article}}

% ── Codificación y fuentes ──────────────────────────────────────────────────
\\\\usepackage[utf8]{{inputenc}}
\\\\usepackage[T1]{{fontenc}}
\\\\usepackage[spanish]{{babel}}
\\\\usepackage{{lmodern}}

% ── Márgenes ────────────────────────────────────────────────────────────────
\\\\usepackage[top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{{geometry}}

% ── Colores institucionales ──────────────────────────────────────────────────
\\\\usepackage[table]{{xcolor}}
\\\\definecolor{{usachTeal}}{{HTML}}{{00A499}}
\\\\definecolor{{usachOrange}}{{HTML}}{{EA7600}}
\\\\definecolor{{usachDark}}{{HTML}}{{394049}}
\\\\definecolor{{usachBlue}}{{HTML}}{{498BCA}}
\\\\definecolor{{usachGold}}{{HTML}}{{EAAA00}}
\\\\definecolor{{rowEven}}{{HTML}}{{F5F7F8}}
\\\\definecolor{{rowHighlight}}{{HTML}}{{FFF8E1}}

% ── Tablas ──────────────────────────────────────────────────────────────────
\\\\usepackage{{booktabs}}
\\\\usepackage{{tabularx}}
\\\\usepackage{{multirow}}
\\\\usepackage{{array}}
\\\\renewcommand{{\\\\arraystretch}}{{1.35}}

% ── Encabezados y secciones ─────────────────────────────────────────────────
\\\\usepackage{{titlesec}}
\\\\titleformat{{\\\\section}}[block]
  {{\\\\normalfont\\\\large\\\\bfseries\\\\color{{usachDark}}}}
  {{}}{{0pt}}
  {{\\\\colorbox{{usachTeal}}{{\\\\textcolor{{white}}{{\\\\hspace{{4pt}}\\\\thesection.\\\\hspace{{4pt}}}}}}\\\\hspace{{6pt}}}}
\\\\titleformat{{\\\\subsection}}[block]
  {{\\\\normalfont\\\\normalsize\\\\bfseries\\\\color{{usachDark}}}}
  {{}}{{0pt}}
  {{\\\\textcolor{{usachOrange}}{{\\\\rule[-.3ex]{{4pt}}{{1.2em}}}}\\\\hspace{{6pt}}}}
\\\\titlespacing*{{\\\\section}}{{0pt}}{{1.2em}}{{0.6em}}
\\\\titlespacing*{{\\\\subsection}}{{0pt}}{{1em}}{{0.4em}}

% ── Cabeceras y pies ─────────────────────────────────────────────────────────
\\\\usepackage{{fancyhdr}}
\\\\pagestyle{{fancy}}
\\\\fancyhf{{}}
\\\\fancyhead[L]{{\\\\small\\\\color{{usachDark}} {tex_esc(INSTITUCION)}}}
\\\\fancyhead[R]{{\\\\small\\\\color{{usachDark}} {tex_esc(PERIODO)}}}
\\\\fancyfoot[C]{{\\\\small\\\\thepage}}
\\\\renewcommand{{\\\\headrulewidth}}{{0.4pt}}

% ── Misceláneos ──────────────────────────────────────────────────────────────
\\\\usepackage{{parskip}}
\\\\usepackage{{microtype}}
\\\\usepackage{{hyperref}}
\\\\hypersetup{{colorlinks=true, linkcolor=usachBlue, urlcolor=usachBlue}}

\\\\begin{{document}}

% ── Portada ──────────────────────────────────────────────────────────────────
\\\\begin{{center}}
  {{\\\\Large\\\\bfseries\\\\color{{usachDark}} {tex_esc(titulo_doc)}}}\\\\\\\\[0.4em]
  {{\\\\large\\\\color{{usachTeal}} {tex_esc(INSTITUCION)}}}\\\\\\\\[0.2em]
  {{\\\\normalsize\\\\color{{gray}} {tex_esc(PERIODO)}}}
\\\\end{{center}}
\\\\vspace{{0.5em}}
\\\\textcolor{{usachTeal}}{{\\\\rule{{\\\\linewidth}}{{2pt}}}}
\\\\vspace{{1em}}
'
  )
}

# ── Función: tabla LaTeX genérica ──────────────────────────────────────────────
# encabezados: vector de strings
# df: data.frame con las filas
# fila_dest: índice de fila a destacar (NULL = ninguna)
# col_izq: índices de columnas alineadas a la izquierda
tabla_latex <- function(encabezados, df, fila_dest = NULL, col_izq = 1) {
  n <- ncol(df)
  # Especificación de columnas
  col_spec <- vapply(seq_len(n), function(j) {
    if (j %in% col_izq) "l" else "r"
  }, character(1))
  col_spec_str <- paste(col_spec, collapse = "")

  # Cabecera
  header <- paste(
    paste0("\\\\textbf{\\\\textcolor{white}{", tex_esc(encabezados), "}}"),
    collapse = " & "
  )

  # Filas
  filas <- vapply(seq_len(nrow(df)), function(i) {
    vals <- vapply(seq_len(n), function(j) tex_esc(df[i, j]), character(1))
    fila_str <- paste(vals, collapse = " & ")
    color <- if (!is.null(fila_dest) && i == fila_dest)
      "\\\\rowcolor{rowHighlight}"
    else if (i %% 2 == 0)
      "\\\\rowcolor{rowEven}"
    else ""
    paste0(color, fila_str, " \\\\\\\\")
  }, character(1))

  paste0(
    "\\\\begin{tabular}{", col_spec_str, "}\n",
    "\\\\toprule\n",
    "\\\\rowcolor{usachDark} ", header, " \\\\\\\\\n",
    "\\\\midrule\n",
    paste(filas, collapse = "\n"), "\n",
    "\\\\bottomrule\n",
    "\\\\end{tabular}\n"
  )
}

# =============================================================================
# CARGA Y PREPARACIÓN — misma lógica que reporte_planeacion.Rmd
# =============================================================================

cargar_base <- function(path) {
  read_excel(path) |>
    clean_names() |>
    rename(
      unidad_dep  = unidad,
      unidad_prof = unidad_profesor,
      curso       = asignatura,
      cod         = codcur,
      pre         = prepost,
      cupo        = cupos,
      insc        = inscritos
    ) |>
    mutate(
      profesor    = str_to_title(str_squish(profesor)),
      unidad_dep  = str_to_upper(str_squish(unidad_dep)),
      unidad_prof = str_to_upper(str_squish(unidad_prof)),
      cargo       = str_to_upper(str_squish(cargo)),
      contrato    = str_to_upper(str_squish(contrato)),
      tipo        = str_to_upper(str_squish(tipo)),
      pre         = str_to_title(str_squish(pre)),
      sec         = paste0(tipo, "-", nsec),
      tipo_contrato = case_when(
        contrato == "JORNADA"  ~ "Jornada",
        contrato == "POR HORA" ~ "Por Hora",
        contrato == "SP"       ~ "Sin Planta",
        TRUE                   ~ "Sin Información"
      ),
      en_claustro  = cargo == "ACADEMICO",
      es_sello     = str_starts(tipo, "S"),
      across(c(horas_ped, eq_cron), ~ replace_na(as.numeric(.), 0))
    )
}

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

if (!file.exists(ARCHIVO_SEM))  stop(glue("No se encuentra: '{ARCHIVO_SEM}'"))
if (!file.exists(ARCHIVO_HIST)) stop(glue("No se encuentra: '{ARCHIVO_HIST}'"))

df_sem  <- cargar_base(ARCHIVO_SEM)  |> aplicar_dedup()
df_hist <- cargar_base(ARCHIVO_HIST) |> aplicar_dedup()
df_todo <- bind_rows(df_hist, df_sem)

sem_ano <- max(df_sem$ano,     na.rm = TRUE)
sem_per <- max(df_sem$periodo, na.rm = TRUE)

periodos <- df_todo |>
  distinct(ano, periodo) |>
  arrange(ano, periodo) |>
  mutate(etiq = map2_chr(ano, periodo, etiq_periodo))

etiq_sem_actual <- etiq_periodo(sem_ano, sem_per)

message(glue("Periodos: {paste(periodos$etiq, collapse=' | ')}"))

# =============================================================================
# FUNCIÓN PRINCIPAL: generar .tex por unidad
# =============================================================================

generar_latex <- function(UP) {
  UP <- str_to_upper(str_squish(UP))
  nombre_unidad <- NOMBRES_UNIDAD[UP]
  if (is.na(nombre_unidad)) nombre_unidad <- str_to_title(UP)

  # ── Datos del semestre actual para esta unidad ──────────────────────────────
  df_u <- df_sem |> filter(unidad_prof == UP)
  if (nrow(df_u) == 0) {
    message(glue("  OMITIDO {UP}: sin datos en el semestre actual"))
    return(invisible(NULL))
  }

  # Nombre largo (para filtrar cursos de la unidad)
  nombre_largo_upper <- df_u |>
    count(unidad_dep) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    pull(unidad_dep)

  # Cursos de la unidad (dedup local de sellos)
  df_cursos <- df_sem |>
    filter(unidad_dep == nombre_largo_upper) |>
    arrange(profesor, sec) |>
    mutate(
      sello_dup_local = es_sello & duplicated(paste(profesor, sec)),
      horas_unidad    = if_else(sello_dup_local, 0L, as.integer(horas_ped))
    )

  dc_pre  <- df_cursos |> filter(pre == "Pregrado")
  dc_post <- df_cursos |> filter(pre == "Postgrado")

  h <- function(data, ctrato = NULL) {
    if (!is.null(ctrato)) data <- data |> filter(tipo_contrato == ctrato)
    sum(data$horas_unidad, na.rm = TRUE)
  }

  # Horas por tipo y nivel
  hj_pre   <- sum(dc_pre[dc_pre$tipo_contrato=="Jornada" & dc_pre$unidad_prof==UP,  ]$horas_unidad)
  hph_pre  <- h(dc_pre,  "Por Hora")
  hext_pre <- sum(dc_pre[dc_pre$tipo_contrato=="Jornada" & dc_pre$unidad_prof!=UP,  ]$horas_unidad)
  hsp_pre  <- sum(dc_pre[dc_pre$cargo=="SIN PROFESOR", ]$horas_unidad)
  tot_pre  <- h(dc_pre)

  hj_post   <- sum(dc_post[dc_post$tipo_contrato=="Jornada" & dc_post$unidad_prof==UP, ]$horas_unidad)
  hph_post  <- h(dc_post, "Por Hora")
  hext_post <- sum(dc_post[dc_post$tipo_contrato=="Jornada" & dc_post$unidad_prof!=UP, ]$horas_unidad)
  hsp_post  <- sum(dc_post[dc_post$cargo=="SIN PROFESOR", ]$horas_unidad)
  tot_post  <- h(dc_post)
  tot_gen   <- tot_pre + tot_post

  pct <- function(n, d) if (d > 0) round(n / d * 100) else 0

  # Claustro (solo ACADEMICO)
  claustro <- df_u |>
    filter(en_claustro) |>
    group_by(profesor) |>
    summarise(hp = sum(horas_prof), .groups = "drop") |>
    arrange(profesor)

  n_claustro    <- nrow(claustro)
  hp_tot_jornada <- df_u |> filter(tipo_contrato == "Jornada") |> pull(horas_prof) |> sum()
  prom_hp       <- if (n_claustro > 0) round(hp_tot_jornada / n_claustro, 1) else 0

  # ── SECCIÓN 1: Horas planeadas ──────────────────────────────────────────────
  tab1_df <- tibble(
    Código      = c("A","i","ii","iii","iv","B","v","vi","vii","C"),
    Descripción = c(
      "Horas planeadas pregrado",
      "Jornada propia",
      "Por hora de clase",
      "Jornada externa",
      "Sin profesor asignado",
      "Horas planeadas postgrado",
      "Jornada propia",
      "Por hora de clase",
      "Jornada externa",
      "Total horas planeadas (A+B)"
    ),
    HP = c(tot_pre, hj_pre, hph_pre, hext_pre, hsp_pre,
           tot_post, hj_post, hph_post, hext_post, tot_gen) |>
      vapply(fmt_hp, character(1))
  )

  filas_dest1 <- c(1, 6, 10)

  # Tabla con filas destacadas manualmente
  tabla1 <- local({
    n <- ncol(tab1_df)
    header <- paste(
      paste0("\\textbf{\\textcolor{white}{", tex_esc(names(tab1_df)), "}}"),
      collapse = " & "
    )
    filas <- vapply(seq_len(nrow(tab1_df)), function(i) {
      vals <- vapply(seq_len(n), function(j) tex_esc(tab1_df[i, j]), character(1))
      color <- if (i %in% filas_dest1) "\\rowcolor{usachGold!25}"
               else if (i %% 2 == 0)  "\\rowcolor{rowEven}"
               else ""
      bold  <- if (i %in% filas_dest1) {
        paste0("\\textbf{", vals, "}")
      } else vals
      paste0(color, paste(bold, collapse = " & "), " \\\\")
    }, character(1))
    paste0(
      "\\begin{tabularx}{\\linewidth}{l X r}\n",
      "\\toprule\n",
      "\\rowcolor{usachDark} ", header, " \\\\\n",
      "\\midrule\n",
      paste(filas, collapse = "\n"), "\n",
      "\\bottomrule\n",
      "\\end{tabularx}\n"
    )
  })

  # ── SECCIÓN 2: Cobertura ────────────────────────────────────────────────────
  tab2_df <- tibble(
    `Tipo de contrato` = c("Jornada propia", "Por hora de clase",
                           "Jornada externa", "Sin profesor"),
    Pregrado  = c(pct(hj_pre,   tot_pre), pct(hph_pre,  tot_pre),
                  pct(hext_pre, tot_pre), pct(hsp_pre,  tot_pre)) |>
                vapply(function(x) paste0(x, "\\%"), character(1)),
    Postgrado = c(pct(hj_post,   tot_post), pct(hph_post,  tot_post),
                  pct(hext_post, tot_post), pct(hsp_post,  tot_post)) |>
                vapply(function(x) paste0(x, "\\%"), character(1)),
    Total     = c(pct(hj_pre+hj_post,     tot_gen),
                  pct(hph_pre+hph_post,   tot_gen),
                  pct(hext_pre+hext_post, tot_gen),
                  pct(hsp_pre+hsp_post,   tot_gen)) |>
                vapply(function(x) paste0(x, "\\%"), character(1))
  )
  tabla2 <- tabla_latex(names(tab2_df), tab2_df, col_izq = 1)

  # ── SECCIÓN 3: Claustro ─────────────────────────────────────────────────────
  cumple_str <- if (prom_hp >= NORMA_PROM_HP)
    paste0("\\textcolor{green!60!black}{\\textbf{Cumple}} (", prom_hp, " HP)")
  else
    paste0("\\textcolor{red!70!black}{\\textbf{No cumple}} (", prom_hp, " HP)")

  tab3_df <- claustro |>
    mutate(
      Profesor = tex_esc(profesor),
      HP       = fmt_hp(hp)
    ) |>
    select(Profesor, HP)

  tabla3 <- tabla_latex(c("Académico", "HP"), as.data.frame(tab3_df), col_izq = 1)

  # ── SECCIÓN 4: Comparativos históricos ─────────────────────────────────────
  fila_act <- which(periodos$ano == sem_ano & periodos$periodo == sem_per)

  # Helper: cursos de la unidad en un periodo con dedup local
  cursos_per <- function(anio, per) {
    nl <- df_todo |>
      filter(unidad_prof == UP, ano == anio, periodo == per) |>
      count(unidad_dep) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(unidad_dep)
    if (length(nl) == 0) return(NULL)
    df_todo |>
      filter(unidad_dep == nl, ano == anio, periodo == per) |>
      arrange(profesor, sec) |>
      mutate(
        sello_dup_local = es_sello & duplicated(paste(profesor, sec)),
        horas_unidad    = if_else(sello_dup_local, 0L, as.integer(horas_ped))
      )
  }

  # Tabla comp 1: horas planeadas
  tcomp1 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dc <- cursos_per(periodos$ano[i], periodos$periodo[i])
    if (is.null(dc))
      return(tibble(Periodo=periodos$etiq[i], Jornada="---", `Por hora`="---", SP="---", Total="---"))
    hp_j <- sum(dc$horas_unidad[dc$tipo_contrato=="Jornada"])
    hp_h <- sum(dc$horas_unidad[dc$tipo_contrato=="Por Hora"])
    hp_s <- sum(dc$horas_unidad[dc$tipo_contrato=="Sin Planta"])
    tibble(Periodo=periodos$etiq[i], Jornada=fmt_hp(hp_j),
           `Por hora`=fmt_hp(hp_h), SP=fmt_hp(hp_s), Total=fmt_hp(hp_j+hp_h+hp_s))
  })

  # Tabla comp 2: cobertura
  tcomp2 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dc <- cursos_per(periodos$ano[i], periodos$periodo[i])
    if (is.null(dc))
      return(tibble(Periodo=periodos$etiq[i], Jornada="---", `Por hora`="---", SP="---"))
    hp_j <- sum(dc$horas_unidad[dc$tipo_contrato=="Jornada"])
    hp_h <- sum(dc$horas_unidad[dc$tipo_contrato=="Por Hora"])
    hp_s <- sum(dc$horas_unidad[dc$tipo_contrato=="Sin Planta"])
    tot  <- hp_j + hp_h + hp_s
    tibble(Periodo=periodos$etiq[i],
           Jornada    = fmt_pct(if(tot>0) hp_j/tot else NA),
           `Por hora` = fmt_pct(if(tot>0) hp_h/tot else NA),
           SP         = fmt_pct(if(tot>0) hp_s/tot else NA))
  })

  # Tabla comp 3: promedio claustro
  tcomp3 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dp <- df_todo |> filter(unidad_prof==UP, ano==periodos$ano[i], periodo==periodos$periodo[i])
    if (nrow(dp) == 0)
      return(tibble(Periodo=periodos$etiq[i], `N acad.`="---",
                    `HP jornada`="---", Promedio="---", Norma="---"))
    n_cl   <- dp |> filter(en_claustro) |> distinct(profesor) |> nrow()
    hp_jor <- dp |> filter(tipo_contrato=="Jornada") |> pull(horas_prof) |> sum(na.rm=TRUE)
    prom   <- if (n_cl > 0) round(hp_jor / n_cl, 1) else NA
    norma  <- if (is.na(prom)) "---" else
              if (prom >= NORMA_PROM_HP) "$\\checkmark$" else "$\\times$"
    tibble(Periodo=periodos$etiq[i], `N acad.`=as.character(n_cl),
           `HP jornada`=fmt_hp(hp_jor),
           Promedio=ifelse(is.na(prom),"---",as.character(prom)),
           Norma=norma)
  })

  tabla_comp1 <- tabla_latex(names(tcomp1), as.data.frame(tcomp1), fila_dest=fila_act, col_izq=1)
  tabla_comp2 <- tabla_latex(names(tcomp2), as.data.frame(tcomp2), fila_dest=fila_act, col_izq=1)
  tabla_comp3 <- tabla_latex(names(tcomp3), as.data.frame(tcomp3), fila_dest=fila_act, col_izq=1)

  # ── Ensamblar documento ─────────────────────────────────────────────────────
  fecha_gen <- format(Sys.time(), "%d/%m/%Y")

  cuerpo <- paste0(
    preambulo(nombre_unidad),

    "\\section{Horas planeadas del departamento}\n\n",
    "Las horas pedagógicas (HP) planeadas para el ", tex_esc(PERIODO),
    " se distribuyen según el tipo de contrato del docente y el nivel de estudios.\n\n",
    tabla1, "\n\n",

    "\\subsection{Cobertura según tipo de contrato}\n\n",
    tabla2, "\n\n",

    "\\section{Claustro académico}\n\n",
    "El promedio de horas pedagógicas del claustro es: ", cumple_str,
    ". La norma institucional es de \\textbf{", NORMA_PROM_HP, " HP}.\n\n",
    "\\textit{Numerador}: HP de todos los jornada (académicos + autoridades). ",
    "\\textit{Denominador}: solo académicos con cargo ACADÉMICO.\n\n",
    tabla3, "\n\n",

    "\\section{Evolución histórica (", tex_esc(periodos$etiq[1]), "–",
    tex_esc(periodos$etiq[nrow(periodos)]), ")}\n\n",
    "\\textit{El semestre en curso aparece destacado en amarillo.}\n\n",

    "\\subsection{Horas planeadas por período}\n\n",
    tabla_comp1, "\n\n",

    "\\subsection{Cobertura por tipo de contrato}\n\n",
    tabla_comp2, "\n\n",

    "\\subsection{Promedio HP del claustro por período}\n\n",
    "Norma: $\\geq$ ", NORMA_PROM_HP, " HP. ",
    "$\\checkmark$ = cumple~~~$\\times$ = no cumple\n\n",
    tabla_comp3, "\n\n",

    "\\vfill\n",
    "\\textcolor{gray}{\\footnotesize Base elaborada a partir de los registros de ",
    "planeación docente FAHU. Código elaborado por Pablo Valenzuela con apoyo de ",
    "Claude de Anthropic. Generado el ", fecha_gen, ".}\n\n",

    "\\end{document}\n"
  )

  ruta <- file.path(CARPETA_LATEX, paste0(slug(UP), ".tex"))
  writeLines(cuerpo, ruta, useBytes = TRUE)
  message(glue("    OK {basename(ruta)}"))
}

# =============================================================================
# EJECUCIÓN
# =============================================================================

if (!dir.exists(CARPETA_LATEX)) dir.create(CARPETA_LATEX, recursive = TRUE)

unidades <- names(NOMBRES_UNIDAD)
message(glue("\nGenerando {length(unidades)} archivos .tex...\n"))
generados <- character(0); errores <- character(0)

for (u in unidades) {
  message(glue("  {u}..."))
  tryCatch({
    generar_latex(u)
    generados <- c(generados, slug(u))
  }, error = function(e) {
    message(glue("    ERROR: {conditionMessage(e)}"))
    errores <<- c(errores, u)
  })
}

cat("\n=====================================================\n")
cat("  ARCHIVOS LaTeX GENERADOS\n")
cat("=====================================================\n")
cat(glue("  Carpeta: {CARPETA_LATEX}/\n"))
cat(glue("  Total:   {length(generados)} archivo(s)\n"))
if (length(errores) > 0)
  cat(glue("  Fallaron: {paste(errores, collapse=', ')}\n"))
cat("\n")
