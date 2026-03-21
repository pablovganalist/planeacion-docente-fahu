# =============================================================================
# exp_04_sintesis_por_programa.R  — v4 (pre-computed metrics, no inner filter)
# =============================================================================
library(readxl); library(janitor); library(tidyverse)

# Configuración heredada del pipeline (definida en run_ci.R)
# ARCHIVO     <- BASE_SEMESTRE.xlsx
# ARCHIVO_HIST_PATH <- BASE_HISTORICA_FAHU.xlsx
# PERIODO     <- "Primer semestre 2026"
# INSTITUCION <- "Facultad de Humanidades"
# CARPETA_HTML <- "docs"

ARCHIVO_SEM  <- if (exists("ARCHIVO"))      ARCHIVO      else "BASE_SEMESTRE.xlsx"
ARCHIVO_HIST <- if (exists("ARCHIVO_HIST")) ARCHIVO_HIST else "BASE_HISTORICA_FAHU.xlsx"
CARPETA_OUT  <- if (exists("CARPETA_HTML")) CARPETA_HTML else "docs"
PERIODO_LABEL <- if (exists("PERIODO"))     PERIODO      else "Primer semestre 2026"
INST_LABEL    <- if (exists("INSTITUCION")) INSTITUCION  else "Facultad de Humanidades"
ETIQ_ACT <- "1S-2026";  ETIQ_ANT <- "1S-2025"
ANO_ANT  <- 2025;        PER_ANT  <- 1

C_TEAL="#00A499"; C_ORANGE="#EA7600"; C_DARK="#394049"
C_BLUE="#498BCA"; C_GOLD="#EAAA00"

NOMBRES_UNIDAD <- c(
  "HISTORIA"="Departamento de Historia",
  "EDUCACIÓN"="Departamento de Educación",
  "EDUCACION"="Departamento de Educación",
  "ESTUDIOS POLÍTICOS"="Departamento de Estudios Políticos",
  "ESTUDIOS POLITICOS"="Departamento de Estudios Políticos",
  "FILOSOFÍA"="Departamento de Filosofía",
  "FILOSOFIA"="Departamento de Filosofía",
  "LINGÜÍSTICA Y LITERATURA"="Departamento de Lingüística y Literatura",
  "LINGUISTICA Y LITERATURA"="Departamento de Lingüística y Literatura",
  "PERIODISMO"="Escuela de Periodismo",
  "PSICOLOGÍA"="Escuela de Psicología",
  "PSICOLOGIA"="Escuela de Psicología"
)

he <- function(x) {
  x <- as.character(x)
  x <- gsub("&","&amp;",x,fixed=T); x <- gsub("<","&lt;",x,fixed=T)
  x <- gsub(">","&gt;",x,fixed=T);  x <- gsub('"',"&quot;",x,fixed=T); x
}
fmt_n   <- function(x) ifelse(is.na(x)|x==0,"—",as.character(as.integer(x)))
fmt_hp  <- function(x) ifelse(is.na(x)|x==0,"—",formatC(as.numeric(x),format="f",digits=0))
fmt_pct <- function(n,d) if(is.na(d)||d==0) "—" else paste0(round(n/d*100),"%")
fmt_var <- function(a,b) {
  if(is.na(a)||is.na(b)||b==0) return('<span style="color:#bbb">—</span>')
  d <- a-b
  if(d>0) paste0('<span style="color:#27ae60;font-weight:700">+',round(d),'</span>')
  else if(d<0) paste0('<span style="color:#c0392b;font-weight:700">',round(d),'</span>')
  else '<span style="color:#888">0</span>'
}

cargar <- function(path) {
  read_excel(path) |> clean_names() |>
    rename(unidad_dep=unidad, unidad_prof=unidad_profesor,
           curso=asignatura, cod=codcur, pre=prepost, cupo=cupos, insc=inscritos) |>
    rename_with(~"horas_ped",  any_of(c("horas_ped","horasped")))  |>
    rename_with(~"horas_plan", any_of(c("horas_plan","horasplan"))) |>
    mutate(
      profesor    = str_to_title(str_squish(profesor)),
      curso       = str_to_title(str_squish(curso)),
      carrera     = str_to_title(str_squish(carrera)),
      cod         = str_to_upper(str_squish(as.character(cod))),
      codprog     = str_to_upper(str_squish(as.character(codprog))),
      tipo        = str_to_upper(str_squish(tipo)),
      cargo       = str_to_upper(str_squish(cargo)),
      contrato    = str_to_upper(str_squish(contrato)),
      unidad_prof = str_to_upper(str_squish(unidad_prof)),
      pre         = str_to_title(str_squish(pre)),
      sec         = paste0(tipo,"-",nsec),
      tipo_contrato = case_when(
        contrato=="JORNADA"   ~ "Jornada",
        contrato=="POR HORA"  ~ "Por Hora",
        contrato=="SP"        ~ "SP",
        TRUE                  ~ "Otro"),
      es_sello = str_starts(tipo,"S")|str_starts(str_to_upper(curso),"SELLO:"),
      sin_prof = cargo=="SIN PROFESOR"|str_to_upper(str_squish(profesor))=="SIN PROFESOR",
      across(c(horas_ped,horas_plan,eq_cron),
             ~replace_na(suppressWarnings(as.numeric(.)),0))
    )
}

df_act  <- cargar(ARCHIVO_SEM)
df_hist <- cargar(ARCHIVO_HIST)
df_ant  <- df_hist |> filter(ano==ANO_ANT, periodo==PER_ANT)


# =============================================================================
# PRE-COMPUTAR MÉTRICAS — una fila por (codprog, pre, periodo)
# =============================================================================
calcular_metricas <- function(df, etiq) {
  if (nrow(df)==0) return(tibble())
  df |>
    group_by(codprog, carrera, pre) |>
    summarise(
      etiqueta   = etiq,
      n_cursos   = n_distinct(cod[!es_sello]),
      n_secs     = n_distinct(paste(sec,cod)[!es_sello]),
      lista_cod  = paste(sort(unique(cod[!es_sello])),collapse="||"),
      lista_secs = paste(sort(unique(paste0(curso[!es_sello]," ",sec[!es_sello]))),collapse="||"),
      hp_tot     = sum(horas_plan),
      hp_j       = sum(horas_plan[tipo_contrato=="Jornada"]),
      hp_ph      = sum(horas_plan[tipo_contrato=="Por Hora"]),
      hp_sp      = sum(horas_plan[tipo_contrato=="SP"]),
      n_sel_c    = n_distinct(cod[es_sello]),
      n_sel_s    = n_distinct(paste(sec,cod)[es_sello]),
      n_sinprof  = n_distinct(paste(sec,cod)[sin_prof]),
      detalle_sp = paste(
        unique(paste0(curso[sin_prof]," (",sec[sin_prof],")")),
        collapse="||"),
      .groups    = "drop"
    )
}

m_act <- calcular_metricas(df_act, ETIQ_ACT)
m_ant <- calcular_metricas(df_ant, ETIQ_ANT)
metricas_all <- bind_rows(m_act, m_ant)


# Mapa unidad
mapa_unidad <- bind_rows(df_act,df_ant) |>
  filter(unidad_prof %in% names(NOMBRES_UNIDAD)) |>
  group_by(codprog,unidad_prof) |> summarise(n=n(),.groups="drop") |>
  group_by(codprog) |> slice_max(n,n=1,with_ties=FALSE) |> ungroup() |>
  mutate(unidad_nombre=NOMBRES_UNIDAD[unidad_prof])


# Tabla de programas ordenada
todos_prog <- metricas_all |>
  distinct(codprog,carrera,pre) |>
  left_join(mapa_unidad |> select(codprog,unidad_nombre), by="codprog") |>
  mutate(
    unidad_nombre = replace_na(unidad_nombre,"Otras unidades"),
    nivel_orden   = if_else(pre=="Pregrado",1L,2L)
  ) |>
  arrange(unidad_nombre,nivel_orden,carrera,codprog) |>
  distinct(codprog,pre,.keep_all=TRUE)  # deduplicate


# =============================================================================
# HTML
# =============================================================================
fila_html <- function(m_row, cls) {
  if (is.null(m_row) || nrow(m_row)==0) {
    return(paste0('<tr class="',cls,' row-nodata">',
                  paste(rep('<td class="num">—</td>',11),collapse=""),'</tr>'))
  }
  r <- m_row[1,]
  sp_badge <- if(r$n_sinprof==0) {
    '<span class="badge-ok">0</span>'
  } else {
    detalle <- strsplit(r$detalle_sp,"\\|\\|")[[1]]
    items   <- paste0("<li>", he(detalle), "</li>", collapse="")
    sprintf(
      '<span class="badge-warn sp-click" onclick="abrirModal(this)" data-title="Sin profesor asignado" data-items="%s">%d &#9432;</span>',
      he(items), r$n_sinprof
    )
  }
  sp_tip <- ""

  paste0('<tr class="',cls,'">',
    '<td class="etiq">',r$etiqueta,'</td>',
    '<td class="num">',fmt_n(r$n_cursos),'</td>',
    '<td class="num">',fmt_n(r$n_secs),'</td>',
    '<td class="num">',fmt_hp(r$hp_tot),'</td>',
    '<td class="num">',fmt_hp(r$hp_j),'</td>',
    '<td class="num">',fmt_pct(r$hp_j,r$hp_tot),'</td>',
    '<td class="num">',fmt_hp(r$hp_ph),'</td>',
    '<td class="num">',fmt_pct(r$hp_ph,r$hp_tot),'</td>',
    '<td class="num">',fmt_hp(r$hp_sp),'</td>',
    '<td class="num">',fmt_pct(r$hp_sp,r$hp_tot),'</td>',
    '<td class="num">',fmt_n(r$n_sel_c),' / ',fmt_n(r$n_sel_s),'</td>',
    '<td class="num">',sp_badge,sp_tip,'</td>',
    '</tr>'
  )
}

fila_var <- function(ra, rb, cls) {
  if(is.null(ra)||nrow(ra)==0||is.null(rb)||nrow(rb)==0) return("")
  a <- ra[1,]; b <- rb[1,]
  paste0('<tr class="',cls,' row-var">',
    '<td class="etiq" style="color:#aaa;font-size:10px">var.</td>',
    '<td class="num">',btn_var(a$n_cursos,b$n_cursos,a$lista_cod, b$lista_cod,"cursos"),'</td>',
    '<td class="num">',btn_var(a$n_secs,  b$n_secs,  a$lista_secs,b$lista_secs,"secs"),  '</td>',
    '<td class="num">',fmt_var(a$hp_tot,  b$hp_tot),  '</td>',
    '<td class="num">',fmt_var(a$hp_j,    b$hp_j),    '</td>',
    '<td class="num">—</td>',
    '<td class="num">',fmt_var(a$hp_ph,   b$hp_ph),   '</td>',
    '<td class="num">—</td>',
    '<td class="num">',fmt_var(a$hp_sp,   b$hp_sp),   '</td>',
    '<td class="num">—</td>',
    '<td class="num">—</td>',
    '<td class="num">—</td>',
    '</tr>'
  )
}

# Helper: botón clickable con diff de cursos/secciones entre períodos
btn_var <- function(val_a, val_b, items_a, items_b, tipo) {
  d <- val_a - val_b
  if (d == 0) return('<span style="color:#888">0</span>')

  # Calcular diff
  set_a <- if(nchar(items_a)>0) strsplit(items_a,"\\|\\|")[[1]] else character(0)
  set_b <- if(nchar(items_b)>0) strsplit(items_b,"\\|\\|")[[1]] else character(0)
  solo_act <- setdiff(set_a, set_b)
  solo_ant <- setdiff(set_b, set_a)

  items_html <- ""
  if(length(solo_act)>0)
    items_html <- paste0(items_html,
      "<li class='diff-add'><span class='diff-lbl'>Solo en 1S-2026:</span> ",
      paste(he(solo_act), collapse="</li><li class='diff-add'>"),
      "</li>")
  if(length(solo_ant)>0)
    items_html <- paste0(items_html,
      "<li class='diff-rm'><span class='diff-lbl'>Solo en 1S-2025:</span> ",
      paste(he(solo_ant), collapse="</li><li class='diff-rm'>"),
      "</li>")

  color <- if(d>0) "#27ae60" else "#c0392b"
  label <- if(d>0) paste0("+",d) else as.character(d)

  if(nchar(items_html)==0) {
    sprintf('<span style="color:%s;font-weight:700">%s</span>', color, label)
  } else {
    titulo <- if(tipo=="cursos") "Variación de cursos" else "Variación de secciones"
    sprintf(
      '<span class="sp-click" style="color:%s;font-weight:700" onclick="abrirModal(this)" data-title="%s" data-items="%s">%s &#9432;</span>',
      color, titulo, he(items_html), label
    )
  }
}

header_tabla <- paste0(
  '<thead>',
  '<tr class="hdr-main">',
  '<th>Período</th>',
  '<th colspan="2" class="num">Cursos regulares</th>',
  '<th class="num">HP total</th>',
  '<th colspan="2" class="num">Jornada</th>',
  '<th colspan="2" class="num">Por hora</th>',
  '<th colspan="2" class="num">SP</th>',
  '<th class="num">Sellos</th>',
  '<th class="num">S/Prof.</th>',
  '</tr>',
  '<tr class="hdr-sub">',
  '<th></th>',
  '<th class="num">Cursos</th><th class="num">Secc.</th>',
  '<th class="num">HP</th>',
  '<th class="num">HP</th><th class="num">%</th>',
  '<th class="num">HP</th><th class="num">%</th>',
  '<th class="num">HP</th><th class="num">%</th>',
  '<th class="num">cur./secc.</th>',
  '<th class="num">secc.</th>',
  '</tr></thead>'
)

css <- sprintf('
<style>
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:"Segoe UI",Arial,sans-serif;font-size:13px;color:%s;
     max-width:1150px;margin:0 auto;padding:2rem 2rem 4rem}
h1{color:%s;font-size:1.3rem;border-bottom:3px solid %s;padding-bottom:.4rem;margin-bottom:.3rem}
.subtitulo{color:#666;font-size:.82rem;margin-bottom:2rem}
.unidad-hdr{font-size:.78rem;font-weight:700;text-transform:uppercase;letter-spacing:.08em;
  color:white;background:%s;padding:5px 14px;margin:2.5rem 0 .5rem;border-left:5px solid %s}
h2{font-size:.85rem;color:%s;margin:.3rem 0 .3rem;display:flex;align-items:center;gap:6px}
.prog-tag{background:%s;color:white;font-size:10px;font-weight:700;padding:2px 8px;border-radius:3px}
.nivel-tag{font-size:10px;font-weight:700;padding:1px 7px;border-radius:3px}
.tag-pre{background:rgba(0,164,153,.15);color:#00706a}
.tag-post{background:rgba(120,80,180,.13);color:#6a3daa}
.tabla-wrap{overflow-x:auto;margin-bottom:1rem}
table{border-collapse:collapse;width:100%%;font-size:11.5px}
thead tr.hdr-main{background:%s;color:white}
thead tr.hdr-sub{background:#555;color:#ddd;font-size:10px}
thead th{padding:5px 9px;text-align:left;font-weight:600;white-space:nowrap}
thead th.num{text-align:right}
tbody td{padding:4px 9px;border-bottom:.5px solid #f0f0f0}
tbody td.num{text-align:right;font-variant-numeric:tabular-nums}
tbody td.etiq{font-size:10.5px;font-weight:700;white-space:nowrap;min-width:58px}
tr.row-pre.row-act td{background:rgba(0,164,153,.10)}
tr.row-pre.row-ant td{background:rgba(0,164,153,.04);color:#555}
tr.row-post.row-act td{background:rgba(120,80,180,.10)}
tr.row-post.row-ant td{background:rgba(120,80,180,.04);color:#555}
tr.row-var td{background:#fafafa}
tr.row-nodata td{color:#bbb;font-style:italic}
tbody tr:hover td{background:#E8F8F7!important}
.badge-warn{color:%s;font-weight:700}
.badge-ok{color:#27ae60;font-weight:700}
.tip{color:#999;font-size:10px;font-style:italic}
input[type=text]{padding:6px 12px;width:100%%;max-width:380px;border:1px solid #ccc;
  border-radius:4px;font-size:12px;font-family:inherit;margin-bottom:1.5rem}
.pie{margin-top:3rem;font-size:11px;color:#aaa;border-top:1px solid #eee;padding-top:.8rem}
</style>
<style>
.sp-click{cursor:pointer;text-decoration:underline dotted;font-size:12px}
.sp-click:hover{color:#a00}
#sp-modal{display:none;position:fixed;inset:0;background:rgba(0,0,0,.45);
  z-index:9999;align-items:center;justify-content:center}
#sp-modal.open{display:flex}
#sp-box{background:white;border-radius:8px;padding:1.2rem 1.5rem;
  max-width:480px;width:90%%;max-height:70vh;overflow-y:auto;
  box-shadow:0 8px 32px rgba(0,0,0,.25);position:relative}
#sp-box h3{font-size:.9rem;color:#394049;margin-bottom:.8rem;
  border-bottom:2px solid #00A499;padding-bottom:.4rem}
#sp-box ul{list-style:none;padding:0;margin:0}
#sp-box li{font-size:12px;padding:4px 0;border-bottom:.5px solid #f0f0f0;color:#333}
#sp-box li:last-child{border:none}
#sp-box li.diff-add{color:#1a7a1a}
#sp-box li.diff-rm {color:#b03030}
.diff-lbl{font-weight:700;font-size:10.5px}
#sp-close{position:absolute;top:.6rem;right:.8rem;background:none;
  border:none;font-size:1.1rem;cursor:pointer;color:#888}
#sp-close:hover{color:#333}
</style>
<div id="sp-modal" onclick="if(event.target===this)cerrarModal()">
  <div id="sp-box">
    <button id="sp-close" onclick="cerrarModal()">&#10005;</button>
    <h3>Detalle</h3>
    <ul id="sp-list"></ul>
  </div>
</div>
<script>
function abrirModal(el){
  var titulo=el.dataset.title||"Detalle";
  document.querySelector("#sp-box h3").textContent=titulo;
  document.getElementById("sp-list").innerHTML=el.dataset.items;
  document.getElementById("sp-modal").classList.add("open");
}
function cerrarModal(){
  document.getElementById("sp-modal").classList.remove("open");
}
document.addEventListener("keydown",function(e){if(e.key==="Escape")cerrarModal();});
function filtrar(){
  var q=document.getElementById("buscador").value.toLowerCase();
  document.querySelectorAll(".bloque-prog").forEach(function(b){
    b.style.display=(b.dataset.q||"").includes(q)?"":"none";
  });
  document.querySelectorAll(".unidad-hdr").forEach(function(h){
    var el=h.nextElementSibling,vis=false;
    while(el&&!el.classList.contains("unidad-hdr")){
      if(el.style.display!=="none")vis=true;el=el.nextElementSibling;
    }
    h.style.display=vis?"":"none";
  });
}
</script>
',C_DARK,C_TEAL,C_TEAL,C_DARK,C_ORANGE,C_DARK,C_BLUE,C_DARK,C_ORANGE)

# =============================================================================
# GENERAR BLOQUES
# =============================================================================
if(!dir.exists(CARPETA_OUT)) dir.create(CARPETA_OUT,recursive=TRUE)

unidades_ord <- unique(todos_prog$unidad_nombre)

bloques <- map_chr(unidades_ord, function(unidad) {
  progs_u <- todos_prog |> filter(unidad_nombre==unidad)

  progs_html <- map_chr(seq_len(nrow(progs_u)), function(i) {
    cp    <- progs_u$codprog[i]
    nivel <- progs_u$pre[i]
    carr  <- progs_u$carrera[i]
    cls   <- if(nivel=="Pregrado") "row-pre" else "row-post"
    cls_n <- if(nivel=="Pregrado") "tag-pre"  else "tag-post"

    r_act <- metricas_all |> filter(codprog==cp, pre==nivel, etiqueta==ETIQ_ACT)
    r_ant <- metricas_all |> filter(codprog==cp, pre==nivel, etiqueta==ETIQ_ANT)

    if(nrow(r_act)==0 && nrow(r_ant)==0) return("")

    filas <- paste0(
      if(nrow(r_act)>0) fila_html(r_act, paste(cls,"row-act")) else "",
      if(nrow(r_ant)>0) fila_html(r_ant, paste(cls,"row-ant")) else "",
      fila_var(r_act, r_ant, cls)
    )

    paste0(
      sprintf('<div class="bloque-prog" data-q="%s">',
              he(paste(tolower(cp),tolower(carr),tolower(nivel)))),
      sprintf('<h2><span class="prog-tag">%s</span>%s<span class="nivel-tag %s">%s</span></h2>',
              he(cp), he(carr), cls_n, nivel),
      '<div class="tabla-wrap"><table>',
      header_tabla,'<tbody>',filas,'</tbody></table></div></div>'
    )
  })

  progs_html <- progs_html[progs_html!=""]
  if(length(progs_html)==0) return("")
  paste0(sprintf('<div class="unidad-hdr">%s</div>\n',he(unidad)),
         paste(progs_html,collapse="\n"))
})

bloques    <- bloques[bloques!=""]
fecha_gen  <- format(Sys.time(),"%d/%m/%Y %H:%M")
n_prog     <- n_distinct(todos_prog$codprog)
n_carr     <- n_distinct(todos_prog$carrera)

html <- paste0(
  '<!DOCTYPE html><html lang="es"><head>',
  '<meta charset="UTF-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  paste0('<title>Síntesis por carrera — ',PERIODO_LABEL,'</title>'),
  css,'</head><body>',
  paste0('<h1>Planeación docente — síntesis por carrera y programa</h1>'),
  paste0('<p class="subtitulo">',INST_LABEL,' &mdash; '),
  ETIQ_ACT,' vs ',ETIQ_ANT,' &mdash; ',n_prog,' programas &mdash; ',n_carr,' carreras</p>',
  '<input type="text" id="buscador" placeholder="Buscar por carrera o código..." oninput="filtrar()">',
  paste(bloques,collapse="\n"),
  '<p class="pie">Generado el ',fecha_gen,'</p>',
  '</body></html>'
)

ruta <- file.path(CARPETA_OUT,"sintesis_programas.html")
writeLines(html,ruta,useBytes=TRUE)
message("OK -> ",ruta)
message("  Programas: ",n_prog," | Carreras: ",n_carr," | Bloques: ",length(bloques))
