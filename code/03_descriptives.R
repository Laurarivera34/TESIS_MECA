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

names(base)

################ PRIMER PARTE: ESTADÍSTICAS DESCRIPTIVAS DE LAS VARIABLES PRINCIPALES

# Variables de interés
variables_tesis <- c(
  # Transferencias del SGP
  "sgp_pc",
  "dependencia_sgp",
  
  # Esfuerzo fiscal propio
  "recaudo_tributario_pc",
  "participacion_tributaria",
  
  # Sostenibilidad de la deuda
  "sostenibilidad_proxy",
  
  # Utilización del crédito
  "dummy_credito",
  "desembolsos_pc",
  "desembolsos_ingresos",
  
  # Características municipales
  "pobl_tot",
  "pib_percapita",
  "indrural",
  "discapital",
  "dismdo",
  "oficinas"
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
      sgp_pc = "SGP por habitante",
      dependencia_sgp = "Dependencia del SGP (%)",
      recaudo_tributario_pc = "Recaudo tributario por habitante",
      participacion_tributaria = "Participación tributaria (%)",
      sostenibilidad_proxy = "Sostenibilidad aproximada (%)",
      dummy_credito = "Desembolsos positivos (0/1)",
      desembolsos_pc = "Desembolsos por habitante",
      desembolsos_ingresos = "Desembolsos / ingresos (%)",
      pobl_tot = "Población",
      pib_percapita = "PIB por habitante",
      indrural = "Índice de ruralidad",
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
        "<i>Nota:</i> N corresponde a observaciones municipio-año; ",
        "DE: desviación estándar. ",
        "Las variables monetarias por habitante se expresan ",
        "en pesos corrientes. ",
        "Los indicadores de dependencia, participación, ",
        "sostenibilidad y desembolsos sobre ingresos ",
        "se expresan en porcentajes. ",
        "La sostenibilidad es una aproximación construida ",
        "a partir de flujos de crédito y no corresponde ",
        "al indicador oficial de la Ley 358 de 1997. ",
        "La variable de desembolsos positivos toma valores ",
        "de 1 cuando se registran desembolsos y 0 en caso contrario.",
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

################ SEGUNDA PARTE: DISTRIBUCIÓN DE MUNICIPIOS SEGÚN CATEGORÍA LEY 617

# Municipios únicos por categoría en 2023
categorias_617 <- base %>%
  filter(ano == 2023, !is.na(categ_cgn)) %>%
  group_by(categ_cgn) %>%
  summarise(
    Municipios = n_distinct(codmpio),
    .groups = "drop"
  ) %>%
  mutate(
    Participacion = 100 * Municipios / sum(Municipios)
  ) %>%
  arrange(categ_cgn)

# Visualizar resultados
print(categorias_617, n = Inf)

# Tabla estilo AER
tabla_617 <- categorias_617 %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Tabla 2. Distribución de municipios por categoría Ley 617, 2023"
  ) %>%
  gt::cols_label(
    categ_cgn = "Categoría",
    Municipios = "Municipios",
    Participacion = "Participación (%)"
  ) %>%
  gt::fmt_number(
    columns = Municipios,
    decimals = 0,
    sep_mark = ","
  ) %>%
  gt::fmt_number(
    columns = Participacion,
    decimals = 2,
    dec_mark = "."
  ) %>%
  gt::cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt::tab_source_note(
    source_note = gt::html(
      paste0(
        "<i>Nota:</i> La participación corresponde al ",
        "porcentaje de municipios de cada categoría ",
        "respecto al total de municipios con información ",
        "disponible en 2023. Cada municipio se cuenta ",
        "una sola vez."
      )
    )
  ) %>%
  gt::tab_options(
    table.width = gt::px(650),
    table.background.color = "white",
    table.font.names = "Times New Roman",
    table.font.size = gt::px(12),
    
    heading.align = "left",
    heading.title.font.size = gt::px(14),
    heading.title.font.weight = "normal",
    
    table.border.top.color = "black",
    table.border.top.width = gt::px(1.5),
    table.border.bottom.style = "none",
    
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = gt::px(1),
    
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = gt::px(1.5),
    
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    column_labels.vlines.style = "none",
    heading.border.bottom.style = "none",
    
    data_row.padding = gt::px(5),
    column_labels.padding = gt::px(6),
    
    source_notes.font.size = gt::px(10),
    source_notes.padding = gt::px(8)
  )

# Exportar imagen PNG
gt::gtsave(
  data = tabla_617,
  filename = file.path(tables, "categorias_617_2023.png"),
  zoom = 2,
  expand = 25
)

# Visualizar
tabla_617
















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
  titulo = "Tabla 2. Municipios con mayor crédito neto promedio",
  archivo = "tabla_1_mayor_credito.png"
)


# TABLA 2: TOP 10 MENORES

crear_tabla_aer(
  datos = top10_menores,
  titulo = "Tabla 3. Municipios con menor crédito neto promedio",
  archivo = "tabla_2_menor_credito.png"
)

########## TERCERA PARTE: MAGNITUD DE LA DEUDA 2000-2023

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
  "Tabla 4. Estadísticas descriptivas de la magnitud de la deuda",
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


############## EVOLUCIÓN DEL CRÉDITO NETO / PIB


# 1. Crear indicador
base <- base %>%
  mutate(
    credito_pib = if_else(
      !is.na(pib_total) & pib_total > 0,
      100 * finan_credito / pib_total,
      NA_real_
    )
  )

# 2. Calcular evolución anual
evolucion_credito_pib <- base %>%
  filter(
    !is.na(finan_credito),
    !is.na(pib_total),
    pib_total > 0
  ) %>%
  group_by(ano) %>%
  summarise(
    credito_total = sum(finan_credito),
    pib_agregado = sum(pib_total),
    municipios = n_distinct(codmpio),
    credito_pib = 100 * credito_total / pib_agregado,
    .groups = "drop"
  )

# 3. Gráfico
ggplot(
  evolucion_credito_pib,
  aes(x = ano, y = credito_pib)
) +
  geom_line(color = "#21618C", linewidth = 1.2) +
  geom_point(color = "#21618C", size = 2.5) +
  scale_x_continuous(
    breaks = sort(unique(evolucion_credito_pib$ano))
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      suffix = "%",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Evolución del crédito neto municipal como porcentaje del PIB",
    subtitle = "Colombia, municipios con información disponible",
    x = "Año",
    y = "Crédito neto / PIB (%)",
    caption = "Fuente: CEDE. Elaboración propia."
  ) +
  theme_minimal(base_size = 12)


