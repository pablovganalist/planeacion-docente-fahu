# Planeación docente — Facultad de Humanidades USACH

Informes automáticos  de planeación docente generados a partir de la base de datos semestral.

## Cómo actualizar los informes

1. Abre el repositorio en GitHub
2. Haz clic en `BASE12026.xlsx` → **Edit** (ícono lápiz) → **Upload file**
3. Sube la nueva versión del archivo y haz **Commit changes**
4. GitHub Actions correrá automáticamente (~5 min) y publicará los informes actualizados

Los informes están disponibles en:
**`https://<tu-usuario>.github.io/<nombre-repo>/`**

---

## Estructura del repositorio

```
├── BASE12026.xlsx          ← base de datos (actualizar aquí)
├── reporte_planeacion.Rmd  ← plantilla de los informes
├── 01_limpieza_analisis.R  ← limpieza y análisis
├── 02_generar_reportes.R   ← genera .md por unidad
├── 04_exportar_html.R      ← convierte a HTML con estilo USACH
├── run_ci.R                ← script maestro (usado por GitHub Actions)
└── .github/workflows/
    └── build.yml           ← configuración del pipeline automático
```

## Uso local (en tu computador)

```r
# 1. Configura el directorio de trabajo
setwd("ruta/a/tu/carpeta")

# 2. Corre el pipeline
source("01_limpieza_analisis.R")
source("02_generar_reportes.R")
source("04_exportar_html.R")

# Los informes quedan en informes_html/
```

## Configuración del periodo

Para cambiar de semestre, edita las líneas al inicio de cada script:

```r
ARCHIVO  <- "BASE22026.xlsx"   # nueva base
PERIODO  <- "Segundo semestre 2026"
```

---

*Código elaborado por Pablo Valenzuela con apoyo de Claude de Anthropic.*
