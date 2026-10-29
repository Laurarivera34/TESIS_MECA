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

########## TERCERA PARTE: MAGNITUD DE LA DEUDA 2000-2023

# ============================================================
# TERCERA PARTE: MAGNITUD DE LA DEUDA MUNICIPAL
# ESTADÍSTICAS DESCRIPTIVAS Y EVOLUCIÓN 2000-2022
# ============================================================


# ============================================================
# 1. ESTADÍSTICAS DESCRIPTIVAS
# ============================================================

estadisticas_deuda <- base %>%
  summarise(
    observaciones = sum(!is.na(df_deuda)),
    municipios = n_distinct(codmpio[!is.na(df_deuda)]),
    cobertura = 100 * mean(!is.na(df_deuda)),
    promedio = mean(df_deuda, na.rm = TRUE),
    desviacion = sd(df_deuda, na.rm = TRUE),
    minimo = min(df_deuda, na.rm = TRUE),
    p25 = quantile(df_deuda, 0.25, na.rm = TRUE),
    mediana = median(df_deuda, na.rm = TRUE),
    p75 = quantile(df_deuda, 0.75, na.rm = TRUE),
    maximo = max(df_deuda, na.rm = TRUE),
    porcentaje_ceros = 100 * mean(
      df_deuda[!is.na(df_deuda)] == 0
    ),
    porcentaje_negativos = 100 * mean(
      df_deuda[!is.na(df_deuda)] < 0
    )
  )

print(estadisticas_deuda, width = Inf)


# ============================================================
# 2. TABLA DE ESTADÍSTICAS - FORMATO AER
# ============================================================

tabla_deuda <- estadisticas_deuda %>%
  pivot_longer(
    cols = everything(),
    names_to = "estadistica",
    values_to = "valor"
  ) %>%
  mutate(
    estadistica = recode(
      estadistica,
      observaciones = "Observaciones disponibles",
      municipios = "Municipios con información",
      cobertura = "Cobertura del panel (%)",
      promedio = "Promedio",
      desviacion = "Desviación estándar",
      minimo = "Mínimo",
      p25 = "Percentil 25",
      mediana = "Mediana",
      p75 = "Percentil 75",
      maximo = "Máximo",
      porcentaje_ceros = "Observaciones iguales a cero (%)",
      porcentaje_negativos = "Observaciones negativas (%)"
    ),
    valor = format(
      round(valor, 2),
      big.mark = ".",
      decimal.mark = ",",
      nsmall = 2,
      trim = TRUE
    )
  )

names(tabla_deuda) <- c("Estadística", "Valor")

estilo_aer <- gridExtra::ttheme_minimal(
  base_size = 12,
  base_family = "serif",
  core = list(
    bg_params = list(fill = "white", col = NA),
    fg_params = list(hjust = 0, x = 0.05)
  ),
  colhead = list(
    fg_params = list(fontface = "bold"),
    bg_params = list(fill = "white", col = NA)
  )
)

tabla_grob <- gridExtra::tableGrob(
  tabla_deuda,
  rows = NULL,
  theme = estilo_aer
)

# Líneas horizontales estilo AER
tabla_grob <- gtable::gtable_add_grob(
  tabla_grob,
  grobs = grid::segmentsGrob(
    x0 = 0, x1 = 1, y0 = 1, y1 = 1,
    gp = grid::gpar(lwd = 1.5)
  ),
  t = 1, l = 1, r = ncol(tabla_grob)
)

tabla_grob <- gtable::gtable_add_grob(
  tabla_grob,
  grobs = grid::segmentsGrob(
    x0 = 0, x1 = 1, y0 = 0, y1 = 0,
    gp = grid::gpar(lwd = 1)
  ),
  t = 1, l = 1, r = ncol(tabla_grob)
)

tabla_grob <- gtable::gtable_add_grob(
  tabla_grob,
  grobs = grid::segmentsGrob(
    x0 = 0, x1 = 1, y0 = 0, y1 = 0,
    gp = grid::gpar(lwd = 1.5)
  ),
  t = nrow(tabla_grob),
  l = 1, r = ncol(tabla_grob)
)

# Guardar tabla como PNG
png(
  filename = file.path(tables, "estadisticas_df_deuda.png"),
  width = 1200,
  height = 1100,
  res = 150
)

grid::grid.newpage()

grid::grid.text(
  "Tabla 3. Estadísticas descriptivas de la magnitud de la deuda",
  x = 0.5, y = 0.95,
  gp = grid::gpar(
    fontsize = 15,
    fontface = "bold",
    fontfamily = "serif"
  )
)

grid::grid.text(
  "Municipios de Colombia, 2000-2022",
  x = 0.5, y = 0.91,
  gp = grid::gpar(fontsize = 12, fontfamily = "serif")
)

grid::pushViewport(
  grid::viewport(
    x = 0.5, y = 0.53,
    width = 0.85, height = 0.68
  )
)

grid::grid.draw(tabla_grob)
grid::popViewport()

grid::grid.text(
  "Nota: Cobertura calculada sobre el panel 2000-2023.",
  x = 0.06, y = 0.12, just = "left",
  gp = grid::gpar(fontsize = 9, fontfamily = "serif")
)

grid::grid.text(
  "La variable no presenta información en 2023.",
  x = 0.06, y = 0.09, just = "left",
  gp = grid::gpar(fontsize = 9, fontfamily = "serif")
)

grid::grid.text(
  "Fuente: DNP - CEDE. Cálculos propios.",
  x = 0.06, y = 0.06, just = "left",
  gp = grid::gpar(fontsize = 9, fontfamily = "serif")
)

dev.off()


# ============================================================
# 3. EVOLUCIÓN ANUAL DE LA MAGNITUD DE LA DEUDA
# ============================================================

evolucion_deuda <- base %>%
  filter(!is.na(df_deuda)) %>%
  group_by(ano) %>%
  summarise(
    municipios = n_distinct(codmpio),
    promedio = mean(df_deuda),
    mediana = median(df_deuda),
    .groups = "drop"
  ) %>%
  arrange(ano)

print(evolucion_deuda, n = Inf)


# ============================================================
# 4. GRÁFICO DE EVOLUCIÓN - FORMATO ACADÉMICO
# ============================================================

evolucion_grafico <- evolucion_deuda %>%
  pivot_longer(
    cols = c(promedio, mediana),
    names_to = "estadistico",
    values_to = "valor"
  ) %>%
  mutate(
    estadistico = recode(
      estadistico,
      promedio = "Promedio",
      mediana = "Mediana"
    )
  )

grafico_deuda <- ggplot(
  evolucion_grafico,
  aes(
    x = ano,
    y = valor,
    color = estadistico,
    linetype = estadistico
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Promedio" = "#21618C",
      "Mediana" = "#C0392B"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Promedio" = "solid",
      "Mediana" = "dashed"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, 2)
  ) +
  labs(
    title = "Evolución de la magnitud de la deuda municipal",
    subtitle = "Colombia, 2000-2022",
    x = "Año",
    y = "Magnitud de la deuda",
    color = NULL,
    linetype = NULL,
    caption = "Fuente: DNP - CEDE. Cálculos propios."
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(grafico_deuda)

# Guardar gráfico
ggsave(
  filename = file.path(figures, "evolucion_df_deuda.png"),
  plot = grafico_deuda,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)



# ============================================================
# CUARTA PARTE: INGRESOS TRIBUTARIOS SEGÚN LEY 617
# ============================================================

library(dplyr)
library(ggplot2)

# 1. Verificar cómo están registradas las categorías
base %>%
  filter(ano %in% 2021:2023) %>%
  count(ano, df2_categoria) %>%
  print(n = Inf)


# 2. Información municipal por categoría
ingresos_municipales <- base %>%
  filter(
    ano %in% 2021:2022,
    !is.na(df2_categoria),
    !is.na(y_corr_tribut)
  ) %>%
  select(
    codmpio,
    municipio,
    depto,
    ano,
    df2_categoria,
    y_corr_tribut
  ) %>%
  arrange(ano, df2_categoria, desc(y_corr_tribut))

View(ingresos_municipales)


# 3. Estadísticas descriptivas por categoría y año
ingresos_categoria <- ingresos_municipales %>%
  group_by(ano, df2_categoria) %>%
  summarise(
    municipios = n_distinct(codmpio),
    ingreso_promedio = mean(y_corr_tribut),
    ingreso_mediano = median(y_corr_tribut),
    ingreso_total = sum(y_corr_tribut),
    .groups = "drop"
  ) %>%
  arrange(ano, df2_categoria)

print(ingresos_categoria, n = Inf)


# 4. Gráfico comparativo por categoría
grafico_ingresos <- ggplot(
  ingresos_categoria,
  aes(
    x = df2_categoria,
    y = ingreso_mediano
  )
) +
  geom_col(fill = "#21618C", width = 0.7) +
  facet_wrap(~ ano, scales = "free_y") +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Ingresos tributarios municipales según categoría",
    subtitle = "Colombia, 2021–2022",
    x = "Categoría municipal",
    y = "Ingreso tributario mediano (millones de pesos corrientes)",
    caption = "Fuente: DNP - CEDE. Cálculos propios."
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold")
  )

print(grafico_ingresos)


# 5. Guardar tabla de datos y gráfico
write.csv(
  ingresos_municipales,
  file.path(tables, "ingresos_tributarios_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  ingresos_categoria,
  file.path(tables, "ingresos_tributarios_categoria.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

ggsave(
  filename = file.path(figures, "ingresos_tributarios_categoria.png"),
  plot = grafico_ingresos,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)



