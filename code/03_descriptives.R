# ESTADÍSTICAS DESCRIPTIVAS Y ANÁLISIS EXPLORATORIO

# LIMPIAR ENTORNO
rm(list = ls())

# PAQUETES
paquetes <- c(
  "tidyverse",
  "haven",
  "here",
  "skimr",
  "plotly",       # Gráficos interactivos
  "osmdata",      # Información de OpenStreetMap
  "sf",           # Manipulación de datos espaciales
  "leaflet",      # Mapas interactivos
  "gridExtra",    # Graficar en grid
  "tmaptools",    # Geocodificación
  "geosphere",    # Distancias geográficas (Haversine)
  "tidymodels",   # Modelado estadístico
  "spatialsample",# Muestreo espacial
  "knitr",        # Generación de tablas académicas
  "kableExtra"  # Formato AER y exportación a LaTeX

)
paquetes_faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}


invisible(
  lapply(paquetes, library, character.only = TRUE)
)

# 3. RUTAS
root <- here::here() # Función here sirve para buscar la raíz de proyecto

data <- here::here("data", "raw")

processed <- here::here("data", "processed")

figures <- here::here("output", "figures")

tables <- here::here("output", "tables")

# IMPORTAR BASES 
base <- readRDS(
  file.path(processed, "base.rds")
)

# Convertir a millones de pesos
base <- base %>%
  mutate(sgp_total = sgp_total / 1000000)

################ PRIMER PARTE: ESTADÍSTICAS DESCRIPTIVAS DE LAS VARIABLES PRINCIPALES

# Variables de interés
variables_tesis <- c(
  "finan_credito_desemb", "finan_credito_amort", "g_int_deudap",
  "sgp_total", "y_corr", "y_corr_tribut",
  "pib_total", "discapital", "dismdo", "oficinas"
)

# Estadísticas descriptivas
estadisticas <- base %>%
  select(all_of(variables_tesis)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    N = sum(!is.na(valor)),
    Media = if (all(is.na(valor))) NA_real_ else mean(valor, na.rm = TRUE),
    DE = sd(valor, na.rm = TRUE),
    Min = if (all(is.na(valor))) NA_real_ else min(valor, na.rm = TRUE),
    Max = if (all(is.na(valor))) NA_real_ else max(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    variable = factor(variable, levels = variables_tesis)
  ) %>%
  arrange(variable) %>%
  mutate(
    variable = recode(
      as.character(variable),
      finan_credito_desemb = "Desembolsos de crédito",
      finan_credito_amort = "Amortizaciones de crédito",
      g_int_deudap = "Intereses de deuda pública",
      sgp_total = "Transferencias SGP",
      y_corr = "Ingresos corrientes",
      y_corr_tribut = "Ingresos tributarios",
      pib_total = "PIB municipal",
      discapital = "Distancia a la capital",
      dismdo = "Distancia al mercado",
      oficinas = "Oficinas bancarias"
    )
  )

# Tabla estilo AER
tabla_aer <- estadisticas %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Tabla 1. Estadísticas descriptivas"
  ) %>%
  gt::cols_label(
    variable = "Variable",
    N = "N",
    Media = "Media",
    DE = "DE",
    Min = "Mín.",
    Max = "Máx."
  ) %>%
  gt::fmt_number(
    columns = c(Media, DE, Min, Max),
    decimals = 2,
    sep_mark = ",",
    dec_mark = "."
  ) %>%
  gt::fmt_number(
    columns = N,
    decimals = 0,
    sep_mark = ","
  ) %>%
  gt::cols_width(
    variable ~ gt::px(260),
    N ~ gt::px(90),
    Media ~ gt::px(140),
    DE ~ gt::px(140),
    Min ~ gt::px(120),
    Max ~ gt::px(170)
  ) %>%
  gt::cols_align(
    align = "left",
    columns = variable
  ) %>%
  gt::cols_align(
    align = "right",
    columns = c(N, Media, DE, Min, Max)
  ) %>%
  gt::tab_source_note(
    source_note = gt::html(
      paste0(
        "<div style='max-width:900px;",
        "white-space:normal;line-height:1.4;'>",
        "<i>Nota:</i> N: observaciones municipio-año; ",
        "DE: desviación estándar. ",
        "Variables fiscales y de crédito en millones ",
        "de pesos corrientes. ",
        "Las demás variables conservan sus unidades originales.",
        "</div>"
      )
    )
  ) %>%
  gt::tab_options(
    # Ancho y fondo
    table.width = gt::px(920),
    table.background.color = "white",
    
    # Tipografía
    table.font.names = "Times New Roman",
    table.font.size = gt::px(12),
    
    # Título
    heading.align = "left",
    heading.title.font.size = gt::px(14),
    heading.title.font.weight = "normal",
    
    # Líneas estilo AER
    table.border.top.color = "black",
    table.border.top.width = gt::px(1.5),
    table.border.bottom.style = "none",
    
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = gt::px(1),
    
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = gt::px(1.5),
    
    # Sin líneas verticales ni internas
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    column_labels.vlines.style = "none",
    heading.border.bottom.style = "none",
    
    # Espaciado
    data_row.padding = gt::px(5),
    column_labels.padding = gt::px(6),
    
    # Nota
    source_notes.font.size = gt::px(10),
    source_notes.padding = gt::px(8)
  )

# Exportar imagen PNG
gt::gtsave(
  data = tabla_aer,
  filename = file.path(tables, "estadisticas.png"),
  zoom = 2,
  expand = 25
)

# Visualizar
tabla_aer


########## SEGUNDA PARTE: TOP 10 MUNICIPIOS - CRÉDITO 2000-2023
credito_municipal <- base %>%
  group_by(codmpio) %>%
  summarise(
    municipio = first(municipio),
    depto = first(depto),
    anos_disponibles = sum(!is.na(finan_credito)),
    credito_promedio = mean(finan_credito, na.rm = TRUE),
    credito_total = sum(finan_credito, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(anos_disponibles > 0)


# Top 10: mayor crédito neto promedio

top10_mayores <- credito_municipal %>%
  slice_max(
    credito_promedio,
    n = 10,
    with_ties = FALSE
  ) %>%
  arrange(desc(credito_promedio))


# Top 10: menor crédito neto promedio

top10_menores <- credito_municipal %>%
  slice_min(
    credito_promedio,
    n = 10,
    with_ties = FALSE
  ) %>%
  arrange(credito_promedio)


# Visualizar resultados

View(top10_mayores)
View(top10_menores)

# Función para generar tablas en formato AER
crear_tabla_aer <- function(datos, titulo, archivo) {
  
  # Seleccionar y formatear columnas
  tabla <- datos %>%
    select(
      codmpio,
      municipio,
      depto,
      credito_promedio,
      credito_total
    ) %>%
    mutate(
      credito_promedio = format(
        round(credito_promedio, 2),
        big.mark = ".",
        decimal.mark = ",",
        nsmall = 2,
        trim = TRUE
      ),
      credito_total = format(
        round(credito_total, 2),
        big.mark = ".",
        decimal.mark = ",",
        nsmall = 2,
        trim = TRUE
      )
    )
  
  names(tabla) <- c(
    "Código DANE",
    "Municipio",
    "Departamento",
    "Crédito promedio",
    "Crédito total"
  )
  
  # Estilo académico AER
  estilo <- gridExtra::ttheme_minimal(
    base_size = 11,
    base_family = "serif",
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5),
      bg_params = list(fill = "white", col = NA)
    ),
    colhead = list(
      fg_params = list(
        fontface = "bold",
        hjust = 0.5,
        x = 0.5
      ),
      bg_params = list(fill = "white", col = NA)
    )
  )
  
  grafico_tabla <- gridExtra::tableGrob(
    tabla,
    rows = NULL,
    theme = estilo
  )
  
  # Líneas horizontales estilo AER
  grafico_tabla <- gtable::gtable_add_grob(
    grafico_tabla,
    grobs = grid::segmentsGrob(
      x0 = 0, x1 = 1, y0 = 1, y1 = 1,
      gp = grid::gpar(lwd = 1.5)
    ),
    t = 1, l = 1, r = ncol(grafico_tabla)
  )
  
  grafico_tabla <- gtable::gtable_add_grob(
    grafico_tabla,
    grobs = grid::segmentsGrob(
      x0 = 0, x1 = 1, y0 = 0, y1 = 0,
      gp = grid::gpar(lwd = 1)
    ),
    t = 1, l = 1, r = ncol(grafico_tabla)
  )
  
  grafico_tabla <- gtable::gtable_add_grob(
    grafico_tabla,
    grobs = grid::segmentsGrob(
      x0 = 0, x1 = 1, y0 = 0, y1 = 0,
      gp = grid::gpar(lwd = 1.5)
    ),
    t = nrow(grafico_tabla),
    l = 1, r = ncol(grafico_tabla)
  )
  
  # Guardar imagen PNG
  png(
    filename = file.path(tables, archivo),
    width = 1500,
    height = 850,
    res = 160
  )
  
  grid::grid.newpage()
  
  grid::grid.text(
    titulo,
    x = 0.5, y = 0.95,
    gp = grid::gpar(
      fontsize = 16,
      fontface = "bold",
      fontfamily = "serif"
    )
  )
  
  grid::grid.text(
    "Colombia, 2000–2023",
    x = 0.5, y = 0.90,
    gp = grid::gpar(fontsize = 12, fontfamily = "serif")
  )
  
  grid::pushViewport(
    grid::viewport(
      x = 0.5, y = 0.52,
      width = 0.95, height = 0.62
    )
  )
  
  grid::grid.draw(grafico_tabla)
  grid::popViewport()
  
  grid::grid.text(
    "Notas: Crédito neto = desembolsos - amortizaciones. Millones de pesos corrientes.",
    x = 0.04, y = 0.13, just = "left",
    gp = grid::gpar(fontsize = 10, fontfamily = "serif")
  )
  
  grid::grid.text(
    "Fuente: CEDE. Cálculos propios.",
    x = 0.04, y = 0.09, just = "left",
    gp = grid::gpar(fontsize = 10, fontfamily = "serif")
  )
  
  dev.off()
}


# TABLA 1: TOP 10 MAYORES

crear_tabla_aer(
  datos = top10_mayores,
  titulo = "Tabla 1. Municipios con mayor crédito neto promedio",
  archivo = "tabla_1_mayor_credito.png"
)


# TABLA 2: TOP 10 MENORES

crear_tabla_aer(
  datos = top10_menores,
  titulo = "Tabla 2. Municipios con menor crédito neto promedio",
  archivo = "tabla_2_menor_credito.png"
)