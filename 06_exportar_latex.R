# =============================================================================
# 06_exportar_latex.R
# Genera un .tex por unidad combinando informe del semestre + comparativos
# Output: docs/latex/{slug}.tex — listo para Overleaf
#
# REGLA DE ESCAPING:
#   En R string con comillas dobles:
#     "\\"   -> un backslash en el archivo  -> comando LaTeX \cmd
#     "\\\\" -> dos backslashes en archivo  -> salto de fila LaTeX \\
# =============================================================================

library(readxl)
library(janitor)
library(tidyverse)

# -- Configuracion -------------------------------------------------------------
ARCHIVO <- if (exists("ARCHIVO")) ARCHIVO else "BASE_FAHU.xlsx"
if (!exists("ANO_SEM") || !exists("PER_SEM")) {
  .tmp  <- readxl::read_excel(ARCHIVO, col_types = "text")
  .cols <- names(.tmp)
  .col_ano <- .cols[str_detect(str_to_lower(.cols), "^a.?o$")][1]
  .col_per <- .cols[str_detect(str_to_lower(.cols), "^periodo$")][1]
  ANO_SEM <- max(as.integer(.tmp[[.col_ano]]), na.rm = TRUE)
  PER_SEM <- max(as.integer(.tmp[[.col_per]]), na.rm = TRUE)
  rm(.tmp, .cols, .col_ano, .col_per)
}
CARPETA_LATEX <- file.path("docs", "latex")
INSTITUCION   <- "Facultad de Humanidades"
PERIODO       <- "Primer semestre 2026"
NORMA_PROM_HP <- 12

NOMBRES_UNIDAD <- c(
  "HISTORIA"                 = "Departamento de Historia",
  "EDUCACION"                = "Departamento de Educación",
  "EDUCACIÓN"                = "Departamento de Educación",
  "ESTUDIOS POLITICOS"       = "Departamento de Estudios Políticos",
  "ESTUDIOS POLÍTICOS"       = "Departamento de Estudios Políticos",
  "FILOSOFIA"                = "Departamento de Filosofía",
  "FILOSOFÍA"                = "Departamento de Filosofía",
  "LINGUISTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
  "LINGÜÍSTICA Y LITERATURA" = "Departamento de Lingüística y Literatura",
  "PERIODISMO"               = "Escuela de Periodismo",
  "PSICOLOGIA"               = "Escuela de Psicología",
  "PSICOLOGÍA"               = "Escuela de Psicología"
)

# -- Helpers ------------------------------------------------------------------
slug <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("[áàäâ]","a") |> str_replace_all("[éèëê]","e") |>
    str_replace_all("[íìïî]","i") |> str_replace_all("[óòöô]","o") |>
    str_replace_all("[úùüû]","u") |> str_replace_all("ñ","n")      |>
    str_replace_all("[^a-z0-9]+","_") |> str_remove("^_|_$")
}

# Escapar TEXTO PLANO para LaTeX. NO llamar sobre strings que ya son LaTeX.
tex_esc <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, fixed("\\"), "\\textbackslash{}")
  x <- str_replace_all(x, fixed("&"),  "\\&")
  x <- str_replace_all(x, fixed("%"),  "\\%")
  x <- str_replace_all(x, fixed("$"),  "\\$")
  x <- str_replace_all(x, fixed("#"),  "\\#")
  x <- str_replace_all(x, fixed("_"),  "\\_")
  x <- str_replace_all(x, fixed("{"),  "\\{")
  x <- str_replace_all(x, fixed("}"),  "\\}")
  x <- str_replace_all(x, fixed("~"),  "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"),  "\\textasciicircum{}")
  x
}

# Formateo numerico — devuelven LaTeX listo, NO pasar por tex_esc
fmt_hp  <- function(x) ifelse(is.na(x) | x == 0, "---",
                               formatC(as.numeric(x), format="f", digits=0, big.mark="."))
fmt_pct <- function(x) ifelse(is.na(x), "---",
                               paste0(formatC(x * 100, format="f", digits=1), "\\%"))

etiq_periodo <- function(anio, per) {
  if (is.na(per) || is.na(anio)) return(NA_character_)
  sem <- if (isTRUE(per == 1)) "1\\textsuperscript{er} sem." else "2\\textsuperscript{do} sem."
  paste(sem, anio)
}

# -- Tabla LaTeX ---------------------------------------------------------------
# IMPORTANTE: df debe tener valores ya listos para LaTeX (no se re-escapan).
# tex_esc solo se aplica a 'encabezados' (texto plano).
tabla_latex <- function(encabezados, df, fila_dest = NULL,
                        col_izq = 1, ancho_x = FALSE) {
  n <- ncol(df)
  if (ancho_x) {
    specs <- vapply(seq_len(n), function(j)
      if (j == col_izq[1]) "X" else if (j %in% col_izq) "l" else "r", character(1))
    env_b <- paste0("\\begin{tabularx}{\\linewidth}{", paste(specs, collapse=""), "}")
    env_e <- "\\end{tabularx}"
  } else {
    specs <- vapply(seq_len(n), function(j)
      if (j %in% col_izq) "l" else "r", character(1))
    env_b <- paste0("\\begin{tabular}{", paste(specs, collapse=""), "}")
    env_e <- "\\end{tabular}"
  }

  head_cells <- paste0("\\textbf{\\textcolor{white}{", tex_esc(encabezados), "}}")
  head_row   <- paste(head_cells, collapse = " & ")

  filas <- vapply(seq_len(nrow(df)), function(i) {
    vals <- vapply(seq_len(n), function(j) as.character(df[i, j]), character(1))
    color <- if (!is.null(fila_dest) && i == fila_dest) "\\rowcolor{rowHighlight}"
             else if (i %% 2 == 0) "\\rowcolor{rowEven}"
             else ""
    if (!is.null(fila_dest) && i == fila_dest) vals <- paste0("\\textbf{", vals, "}")
    paste0(color, paste(vals, collapse = " & "), " \\\\")
  }, character(1))

  paste(env_b, "\\toprule",
        paste0("\\rowcolor{usachDark} ", head_row, " \\\\"),
        "\\midrule",
        paste(filas, collapse = "\n"),
        "\\bottomrule", env_e, sep = "\n")
}

# -- Preambulo -----------------------------------------------------------------
# Construido linea a linea con paste() para evitar ambiguedades de escaping.
# Cada "\\cmd" en R = "\cmd" en el archivo .tex
preambulo_latex <- function(titulo_doc) {
  paste(
    "\\documentclass[11pt, a4paper]{article}",
    "",
    "% -- Codificacion y fuentes",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[spanish]{babel}",
    "\\usepackage{lmodern}",
    "",
    "% -- Margenes",
    "\\usepackage[top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}",
    "",
    "% -- Colores institucionales",
    "\\usepackage[table]{xcolor}",
    "\\definecolor{usachTeal}{HTML}{00A499}",
    "\\definecolor{usachOrange}{HTML}{EA7600}",
    "\\definecolor{usachDark}{HTML}{394049}",
    "\\definecolor{usachBlue}{HTML}{498BCA}",
    "\\definecolor{usachGold}{HTML}{EAAA00}",
    "\\definecolor{rowEven}{HTML}{F5F7F8}",
    "\\definecolor{rowHighlight}{HTML}{FFF8E1}",
    "",
    "% -- Tablas",
    "\\usepackage{booktabs}",
    "\\usepackage{tabularx}",
    "\\usepackage{array}",
    "\\renewcommand{\\arraystretch}{1.35}",
    "",
    "% -- Secciones",
    "\\usepackage{titlesec}",
    "\\titleformat{\\section}[block]",
    "  {\\normalfont\\large\\bfseries\\color{usachDark}}",
    "  {}{}",
    "  {\\colorbox{usachTeal}{\\textcolor{white}{\\hspace{4pt}\\thesection.\\hspace{4pt}}}\\hspace{6pt}}",
    "\\titleformat{\\subsection}[block]",
    "  {\\normalfont\\normalsize\\bfseries\\color{usachDark}}",
    "  {}{}",
    "  {\\textcolor{usachOrange}{\\rule[-.3ex]{4pt}{1.2em}}\\hspace{6pt}}",
    "\\titlespacing*{\\section}{0pt}{1.2em}{0.6em}",
    "\\titlespacing*{\\subsection}{0pt}{1em}{0.4em}",
    "",
    "% -- Encabezados y pies",
    "\\usepackage{fancyhdr}",
    "\\pagestyle{fancy}",
    "\\fancyhf{}",
    paste0("\\fancyhead[L]{\\small\\color{usachDark} ", tex_esc(INSTITUCION), "}"),
    paste0("\\fancyhead[R]{\\small\\color{usachDark} ", tex_esc(PERIODO), "}"),
    "\\fancyfoot[C]{\\small\\thepage}",
    "\\renewcommand{\\headrulewidth}{0.4pt}",
    "",
    "% -- Miscelaneos",
    "\\usepackage{parskip}",
    "\\usepackage{microtype}",
    "\\usepackage{amssymb}",
    "\\usepackage{hyperref}",
    "\\hypersetup{colorlinks=true, linkcolor=usachBlue, urlcolor=usachBlue}",
    "",
    "\\begin{document}",
    "",
    "% -- Portada",
    "\\begin{center}",
    paste0("  {\\Large\\bfseries\\color{usachDark} ", tex_esc(titulo_doc), "} \\\\[0.4em]"),
    paste0("  {\\large\\color{usachTeal} ", tex_esc(INSTITUCION), "} \\\\[0.2em]"),
    paste0("  {\\normalsize\\color{gray} ", tex_esc(PERIODO), "}"),
    "\\end{center}",
    "\\vspace{0.5em}",
    "\\textcolor{usachTeal}{\\rule{\\linewidth}{2pt}}",
    "\\vspace{1em}",
    "",
    sep = "\n"
  )
}

# =============================================================================
# CARGA DE DATOS
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
    rename_with(~ "horas_ped",  any_of(c("horas_ped",  "horasped")))  |>
    rename_with(~ "horas_plan", any_of(c("horas_plan", "horasplan"))) |>
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
        TRUE                   ~ "Sin Informacion"
      ),
      en_claustro = cargo == "ACADEMICO",
      es_sello    = str_starts(tipo, "S"),
      across(c(horas_ped, horas_plan, eq_cron),
             ~ replace_na(suppressWarnings(as.numeric(.)), 0))
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

if (!file.exists(ARCHIVO)) stop(paste0("No se encuentra: '", ARCHIVO, "'"))

df_todo_raw <- cargar_base(ARCHIVO) |> aplicar_dedup()
df_sem  <- df_todo_raw |> filter(as.integer(ano)==ANO_SEM, as.integer(periodo)==PER_SEM)
df_hist <- df_todo_raw |> filter(!(as.integer(ano)==ANO_SEM & as.integer(periodo)==PER_SEM))
df_todo <- df_todo_raw

sem_ano <- max(df_sem$ano,     na.rm = TRUE)
sem_per <- max(df_sem$periodo, na.rm = TRUE)

periodos <- df_todo |>
  distinct(ano, periodo) |>
  filter(!is.na(ano), !is.na(periodo)) |>
  arrange(ano, periodo) |>
  mutate(etiq = map2_chr(ano, periodo, etiq_periodo))

message("Periodos: ", paste(periodos$etiq, collapse = " | "))

# =============================================================================
# GENERADOR POR UNIDAD
# =============================================================================
generar_latex <- function(UP) {
  UP <- str_to_upper(str_squish(UP))
  nombre_unidad <- NOMBRES_UNIDAD[UP]
  if (is.na(nombre_unidad)) nombre_unidad <- str_to_title(UP)

  df_u <- df_sem |> filter(unidad_prof == UP)
  if (nrow(df_u) == 0) { message("  OMITIDO ", UP); return(invisible(NULL)) }

  nombre_largo_upper <- df_u |>
    count(unidad_dep) |> slice_max(n, n=1, with_ties=FALSE) |> pull(unidad_dep)

  dc <- df_sem |>
    filter(unidad_dep == nombre_largo_upper) |>
    arrange(profesor, sec) |>
    mutate(
      sello_dup_local = es_sello & duplicated(paste(profesor, sec)),
      horas_unidad    = if_else(sello_dup_local, 0L, as.integer(horas_ped))
    )

  dc_pre  <- dc |> filter(pre == "Pregrado")
  dc_post <- dc |> filter(pre == "Postgrado")

  hj  <- function(d) sum(d$horas_unidad[d$tipo_contrato=="Jornada"  & d$unidad_prof==UP])
  hph <- function(d) sum(d$horas_unidad[d$tipo_contrato=="Por Hora"])
  hxt <- function(d) sum(d$horas_unidad[d$tipo_contrato=="Jornada"  & d$unidad_prof!=UP])
  hsp <- function(d) sum(d$horas_unidad[d$cargo=="SIN PROFESOR"])
  ht  <- function(d) sum(d$horas_unidad)

  hj_p  <- hj(dc_pre);  hph_p <- hph(dc_pre);  hxt_p <- hxt(dc_pre)
  hsp_p <- hsp(dc_pre); tot_p <- ht(dc_pre)
  hj_g  <- hj(dc_post); hph_g <- hph(dc_post); hxt_g <- hxt(dc_post)
  hsp_g <- hsp(dc_post);tot_g <- ht(dc_post)
  tot   <- tot_p + tot_g

  pct <- function(n, d) if (!is.na(d) && d > 0) n / d else NA_real_

  claustro <- df_u |>
    filter(en_claustro) |>
    group_by(profesor) |>
    summarise(hp = sum(horas_prof), .groups="drop") |>
    arrange(profesor)

  n_cl   <- nrow(claustro)
  hp_jor <- df_u |> filter(tipo_contrato=="Jornada") |> pull(horas_prof) |> sum()
  prom   <- if (n_cl > 0) round(hp_jor / n_cl, 1) else 0

  # -- Tabla 1: horas planeadas ------------------------------------------------
  # Texto en Descripcion ya escapado con tex_esc; HP formateado con fmt_hp
  tab1 <- data.frame(
    Cod = c("A","i","ii","iii","iv","B","v","vi","vii","C"),
    Descripcion = tex_esc(c(
      "Horas planeadas pregrado",
      "Jornada propia","Por hora de clase","Jornada externa","Sin profesor asignado",
      "Horas planeadas postgrado",
      "Jornada propia","Por hora de clase","Jornada externa",
      "Total horas planeadas (A+B)"
    )),
    HP = vapply(c(tot_p,hj_p,hph_p,hxt_p,hsp_p,
                  tot_g,hj_g,hph_g,hxt_g,tot),
                fmt_hp, character(1)),
    stringsAsFactors = FALSE
  )

  # Tabla 1 manual con filas especiales en negrita
  tabla1 <- local({
    dest <- c(1, 6, 10)
    hdr  <- paste(paste0("\\textbf{\\textcolor{white}{",
                          tex_esc(c("Cód.","Descripción","HP")), "}}"),
                  collapse = " & ")
    rows <- vapply(seq_len(nrow(tab1)), function(i) {
      v <- as.character(tab1[i,])
      col <- if (i %in% dest) "\\rowcolor{rowHighlight}" else
             if (i %% 2 == 0) "\\rowcolor{rowEven}" else ""
      if (i %in% dest) v <- paste0("\\textbf{", v, "}")
      paste0(col, paste(v, collapse=" & "), " \\\\")
    }, character(1))
    paste("\\begin{tabularx}{\\linewidth}{l X r}", "\\toprule",
          paste0("\\rowcolor{usachDark} ", hdr, " \\\\"),
          "\\midrule", paste(rows, collapse="\n"),
          "\\bottomrule", "\\end{tabularx}", sep="\n")
  })

  # -- Tabla 2: cobertura ------------------------------------------------------
  tab2 <- data.frame(
    Tipo      = tex_esc(c("Jornada propia","Por hora de clase",
                           "Jornada externa","Sin profesor")),
    Pregrado  = vapply(c(pct(hj_p,tot_p),pct(hph_p,tot_p),
                          pct(hxt_p,tot_p),pct(hsp_p,tot_p)), fmt_pct, character(1)),
    Postgrado = vapply(c(pct(hj_g,tot_g),pct(hph_g,tot_g),
                          pct(hxt_g,tot_g),pct(hsp_g,tot_g)), fmt_pct, character(1)),
    Total     = vapply(c(pct(hj_p+hj_g,tot),pct(hph_p+hph_g,tot),
                          pct(hxt_p+hxt_g,tot),pct(hsp_p+hsp_g,tot)),
                       fmt_pct, character(1)),
    stringsAsFactors = FALSE
  )
  tabla2 <- tabla_latex(c("Tipo de contrato","Pregrado","Postgrado","Total"), tab2, col_izq=1)

  # -- Tabla 3: claustro -------------------------------------------------------
  tab3 <- data.frame(
    Academico = tex_esc(claustro$profesor),
    HP        = fmt_hp(claustro$hp),
    stringsAsFactors = FALSE
  )
  tabla3 <- tabla_latex(c("Académico","HP"), tab3, col_izq=1)

  cumple_str <- if (prom >= NORMA_PROM_HP)
    paste0("\\textcolor{green!60!black}{\\textbf{Cumple}} (", prom, " HP)")
  else
    paste0("\\textcolor{red!70!black}{\\textbf{No cumple}} (", prom, " HP)")

  # -- Comparativos ------------------------------------------------------------
  fila_act <- which(periodos$ano==sem_ano & periodos$periodo==sem_per)

  cursos_per <- function(anio, per) {
    dp <- df_todo |> filter(unidad_prof==UP, ano==anio, periodo==per)
    if (nrow(dp)==0) return(NULL)
    nl <- dp |> count(unidad_dep) |> slice_max(n,n=1,with_ties=FALSE) |> pull(unidad_dep)
    df_todo |> filter(unidad_dep==nl, ano==anio, periodo==per) |>
      arrange(profesor, sec) |>
      mutate(
        sello_dup_local = es_sello & duplicated(paste(profesor,sec)),
        horas_unidad    = if_else(sello_dup_local, 0L, as.integer(horas_plan))
      )
  }

  tcomp1 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    d <- cursos_per(periodos$ano[i], periodos$periodo[i])
    if (is.null(d)) return(data.frame(Periodo=periodos$etiq[i],Jornada="---",
                           `Por hora`="---",SP="---",Total="---",check.names=FALSE))
    hp_j <- sum(d$horas_unidad[d$tipo_contrato=="Jornada"])
    hp_h <- sum(d$horas_unidad[d$tipo_contrato=="Por Hora"])
    hp_s <- sum(d$horas_unidad[d$tipo_contrato=="Sin Planta"])
    data.frame(Periodo=periodos$etiq[i],Jornada=fmt_hp(hp_j),
               `Por hora`=fmt_hp(hp_h),SP=fmt_hp(hp_s),
               Total=fmt_hp(hp_j+hp_h+hp_s),check.names=FALSE)
  })

  tcomp2 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    d <- cursos_per(periodos$ano[i], periodos$periodo[i])
    if (is.null(d)) return(data.frame(Periodo=periodos$etiq[i],Jornada="---",
                           `Por hora`="---",SP="---",check.names=FALSE))
    hp_j <- sum(d$horas_unidad[d$tipo_contrato=="Jornada"])
    hp_h <- sum(d$horas_unidad[d$tipo_contrato=="Por Hora"])
    hp_s <- sum(d$horas_unidad[d$tipo_contrato=="Sin Planta"])
    tt   <- hp_j+hp_h+hp_s
    data.frame(Periodo=periodos$etiq[i],
               Jornada    = fmt_pct(if(tt>0) hp_j/tt else NA),
               `Por hora` = fmt_pct(if(tt>0) hp_h/tt else NA),
               SP         = fmt_pct(if(tt>0) hp_s/tt else NA),
               check.names=FALSE)
  })

  tcomp3 <- map_dfr(seq_len(nrow(periodos)), function(i) {
    dp <- df_todo |> filter(unidad_prof==UP,ano==periodos$ano[i],periodo==periodos$periodo[i])
    if (nrow(dp)==0) return(data.frame(Periodo=periodos$etiq[i],`N acad.`="---",
                            `HP jornada`="---",Promedio="---",Norma="---",check.names=FALSE))
    nc  <- dp |> filter(en_claustro) |> distinct(profesor) |> nrow()
    hpj <- dp |> filter(tipo_contrato=="Jornada") |> pull(horas_prof) |> sum(na.rm=TRUE)
    pr  <- if(nc>0) round(hpj/nc,1) else NA_real_
    nrm <- if(is.na(pr)) "---" else if(pr>=NORMA_PROM_HP) "$\\checkmark$" else "$\\times$"
    data.frame(Periodo=periodos$etiq[i],`N acad.`=as.character(nc),
               `HP jornada`=fmt_hp(hpj),
               Promedio=ifelse(is.na(pr),"---",as.character(pr)),
               Norma=nrm, check.names=FALSE)
  })

  tc1 <- tabla_latex(names(tcomp1), tcomp1, fila_dest=fila_act, col_izq=1)
  tc2 <- tabla_latex(names(tcomp2), tcomp2, fila_dest=fila_act, col_izq=1)
  tc3 <- tabla_latex(names(tcomp3), tcomp3, fila_dest=fila_act, col_izq=1)

  # -- Ensamblar ---------------------------------------------------------------
  fecha_gen  <- format(Sys.time(), "%d/%m/%Y")
  etiq_rango <- paste0(periodos$etiq[1], " -- ", periodos$etiq[nrow(periodos)])

  cuerpo <- paste(
    preambulo_latex(nombre_unidad),
    "\\section{Horas planeadas del departamento}",
    "",
    paste0("Las horas pedagógicas (HP) planeadas para el ",
           tex_esc(PERIODO), " se distribuyen según el tipo de",
           " contrato del docente y el nivel de estudios."),
    "",
    tabla1,
    "",
    "\\subsection{Cobertura según tipo de contrato}",
    "",
    tabla2,
    "",
    "\\section{Claustro académico}",
    "",
    paste0("El promedio de HP del claustro es: ", cumple_str,
           ". Norma institucional: \\textbf{", NORMA_PROM_HP, " HP}."),
    "",
    paste0("\\textit{Numerador}: HP de todos los jornada (académicos + autoridades). ",
           "\\textit{Denominador}: solo académicos con cargo ACADÉMICO."),
    "",
    tabla3,
    "",
    paste0("\\section{Evolución histórica (", etiq_rango, ")}"),
    "",
    "\\textit{El semestre en curso aparece destacado en amarillo.}",
    "",
    "\\subsection{Horas planeadas por período}",
    "",
    tc1,
    "",
    "\\subsection{Cobertura por tipo de contrato}",
    "",
    tc2,
    "",
    "\\subsection{Promedio HP del claustro por período}",
    "",
    paste0("Norma: $\\geq$ ", NORMA_PROM_HP, " HP.\\quad",
           "$\\checkmark$ = cumple \\quad $\\times$ = no cumple"),
    "",
    tc3,
    "",
    "\\vfill",
    paste0("{\\footnotesize\\color{gray} Elaborado a partir de los registros de",
           " planeación docente FAHU. Generado el ", fecha_gen, ".}"),
    "",
    "\\end{document}",
    sep = "\n"
  )

  ruta <- file.path(CARPETA_LATEX, paste0(slug(UP), ".tex"))
  writeLines(cuerpo, ruta, useBytes = TRUE)
  message("    OK ", basename(ruta))
}

# =============================================================================
# EJECUCION
# =============================================================================
if (!dir.exists(CARPETA_LATEX)) dir.create(CARPETA_LATEX, recursive = TRUE)

# Usar las claves con tilde — coinciden exactamente con los datos
unidades <- c(
  "HISTORIA", "EDUCACIÓN", "ESTUDIOS POLÍTICOS", "FILOSOFÍA",
  "LINGÜÍSTICA Y LITERATURA", "PERIODISMO", "PSICOLOGÍA"
)

message("\nGenerando ", length(unidades), " archivos .tex...\n")
generados <- character(0)
errores   <- character(0)

for (u in unidades) {
  message("  ", u, "...")
  tryCatch({
    generar_latex(u)
    generados <- c(generados, slug(u))
  }, error = function(e) {
    message("    ERROR: ", conditionMessage(e))
    errores <<- c(errores, u)
  })
}

cat("\n=====================================================\n")
cat("  ARCHIVOS LaTeX GENERADOS\n")
cat("=====================================================\n")
cat("  Carpeta: ", CARPETA_LATEX, "/\n", sep="")
cat("  Total:   ", length(generados), " archivo(s)\n", sep="")
if (length(errores) > 0) cat("  Fallaron: ", paste(errores, collapse=", "), "\n", sep="")
cat("\n")
