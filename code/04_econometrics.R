# ESTADÍSTICAS DESCRIPTIVAS Y ANÁLISIS EXPLORATORIO

# LIMPIAR ENTORNO
rm(list = ls())

# PAQUETES
paquetes <- c(
  "tidyverse",
  "haven",
  "here",
  "skimr",
  "plotly",
  "osmdata",
  "sf",
  "leaflet",
  "gridExtra",
  "tmaptools",
  "geosphere",
  "tidymodels",
  "spatialsample",
  "knitr",
  "xfun",
  "kableExtra",
  "maps",       # Contorno de Colombia
  "scales",     # Etiquetas de porcentajes
  "gt",         # Tablas descriptivas
  "patchwork",
  "webshot2",   # Exportación de tablas a PNG
  "withr"       # Exportación de figuras
)

paquetes_faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(paquetes_faltantes) > 0) {
  install.packages(
    paquetes_faltantes,
    repos = "https://cloud.r-project.org"
  )
}

# usa maps a través de ggplot2::map_data().
invisible(
  lapply(setdiff(paquetes, "maps"), library, character.only = TRUE)
)

# RUTAS
root <- here::here() # Función here sirve para buscar la raíz de proyecto

data <- here::here("data", "raw")

processed <- here::here("data", "processed")

figures <- here::here("output", "figures")

tables <- here::here("output", "tables")

#############################################################################################

# 1. Cargar la misma base de 03_descriptives
base <- readRDS(here::here("data", "processed", "base.rds"))

# 2. Seleccionar el período de estudio
panel <- subset(base, ano %in% 2002:2023)


# PRIMER BLOQUE: USO DEL CRÉDITO
############################# Primer modelo 

# 3. Estimar el modelo con efectos fijos de municipio y año
modelo_desembolsos <- fixest::feols(
  desembolsos_pc_real2023 ~ sgp_pc_real2023 | codmpio + ano,
  data = panel,
  vcov = ~codmpio
)

# 4. Revisar los resultados
summary(modelo_desembolsos)

# 5. Tabla de resultados

estimacion <- fixest::coeftable(modelo_desembolsos)["sgp_pc_real2023", ]

numero <- function(x, decimales = 0) {
  formatC(x, format = "f", digits = decimales,
          decimal.mark = ",", big.mark = ".")
}

p <- estimacion[4]
estrellas <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""

resultados <- data.frame(
  Concepto = c(
    "SGP por habitante", "",
    "Efectos fijos de municipio", "Efectos fijos de año",
    "Observaciones", "Municipios", "R² intra"
  ),
  Desembolsos = c(
    numero(estimacion[1], 4),
    paste0("(", numero(estimacion[2], 4), ")", estrellas),
    "Sí", "Sí",
    numero(stats::nobs(modelo_desembolsos)),
    numero(modelo_desembolsos$fixef_sizes[["codmpio"]]),
    numero(fixest::r2(modelo_desembolsos, "wr2"), 6)
  )
)

tabla <- gt::gt(resultados) |>
  gt::tab_header(
    title = "Tabla 1. SGP y desembolsos de crédito municipal, 2002–2023",
    subtitle = "Estimación de panel con efectos fijos"
  ) |>
  gt::cols_label(
    Concepto = "Concepto",
    Desembolsos = "Desembolsos de crédito por habitante"
  ) |>
  gt::cols_align(align = "center", columns = Desembolsos) |>
  gt::tab_options(
    table.font.size = gt::px(13),
    heading.title.font.size = gt::px(15),
    heading.subtitle.font.size = gt::px(12),
    data_row.padding = gt::px(2),
    column_labels.padding = gt::px(4),
    source_notes.font.size = gt::px(11),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |>
  gt::tab_source_note(
    paste(
      "Errores estándar agrupados por municipio, entre paréntesis.",
      "*** p < 0,01, ** p < 0,05, * p < 0,10."
    )
  )

tabla

# 6. Guardar la tabla de resultados
tables <- here::here("output", "tables")
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

gt::gtsave(
  data = tabla,
  filename = "tabla_01_sgp_desembolsos.png",
  path = tables
)

############################# Segundo modelo 

# 7. Verificar la variable de uso del crédito
with(panel, table(
  dummy_credito,
  desembolso_positivo = finan_credito_desemb_real2023 > 0,
  useNA = "ifany"
))

# 8. Estimar la probabilidad de registrar algún desembolso
modelo_uso_credito <- fixest::feols(
  dummy_credito ~ sgp_pc_real2023 | codmpio + ano,
  data = panel,
  vcov = ~codmpio
)

summary(modelo_uso_credito)

# 9. Tabla del modelo de uso del crédito

estimacion_uso <- fixest::coeftable(modelo_uso_credito)["sgp_pc_real2023", ]

estrellas_uso <- if (estimacion_uso[4] < 0.01) {
  "***"
} else if (estimacion_uso[4] < 0.05) {
  "**"
} else if (estimacion_uso[4] < 0.10) {
  "*"
} else {
  ""
}

cientifico <- function(x) {
  formatC(x, format = "e", digits = 2, decimal.mark = ",")
}

resultados_uso <- data.frame(
  Concepto = c(
    "SGP por habitante", "",
    "Efectos fijos de municipio", "Efectos fijos de año",
    "Observaciones", "Municipios", "R² intra"
  ),
  Uso_credito = c(
    cientifico(estimacion_uso[1]),
    paste0("(", cientifico(estimacion_uso[2]), ")", estrellas_uso),
    "Sí", "Sí",
    numero(stats::nobs(modelo_uso_credito)),
    numero(modelo_uso_credito$fixef_sizes[["codmpio"]]),
    numero(fixest::r2(modelo_uso_credito, "wr2"), 6)
  )
)

tabla_uso <- gt::gt(resultados_uso) |>
  gt::tab_header(
    title = "Tabla 2. SGP y uso del crédito municipal, 2002–2023",
    subtitle = "Modelo lineal de probabilidad con efectos fijos"
  ) |>
  gt::cols_label(
    Uso_credito = "Probabilidad de registrar un desembolso"
  ) |>
  gt::cols_align(align = "center", columns = Uso_credito) |>
  gt::tab_options(
    table.font.size = gt::px(13),
    heading.title.font.size = gt::px(15),
    heading.subtitle.font.size = gt::px(12),
    data_row.padding = gt::px(2),
    column_labels.padding = gt::px(4),
    source_notes.font.size = gt::px(11),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |>
  gt::tab_source_note(
    paste(
      "Errores estándar agrupados por municipio, entre paréntesis.",
      "*** p < 0,01, ** p < 0,05, * p < 0,10."
    )
  )

tables <- here::here("output", "tables")
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

gt::gtsave(
  data = tabla_uso,
  filename = "tabla_02_sgp_uso_credito.png",
  path = tables
)

tabla_uso

############################# Tercer modelo 
# 10. Desembolsos por habitante entre municipios-año con crédito
modelo_monto_positivo <- fixest::feols(
  desembolsos_pc_real2023 ~ sgp_pc_real2023 | codmpio + ano,
  data = subset(panel, dummy_credito == 1),
  vcov = ~codmpio
)

summary(modelo_monto_positivo)

# 11. Tabla del monto desembolsado entre quienes usaron crédito
estimacion_monto <- fixest::coeftable(modelo_monto_positivo)["sgp_pc_real2023", ]

p_monto <- estimacion_monto[4]
estrellas_monto <- if (p_monto < 0.01) {
  "***"
} else if (p_monto < 0.05) {
  "**"
} else if (p_monto < 0.10) {
  "*"
} else {
  ""
}

resultados_monto <- data.frame(
  Concepto = resultados_uso$Concepto,
  Monto = c(
    numero(estimacion_monto[1], 4),
    paste0("(", numero(estimacion_monto[2], 4), ")", estrellas_monto),
    "Sí", "Sí",
    numero(stats::nobs(modelo_monto_positivo)),
    numero(modelo_monto_positivo$fixef_sizes[["codmpio"]]),
    numero(fixest::r2(modelo_monto_positivo, "wr2"), 6)
  )
)

tabla_monto <- gt::gt(resultados_monto) |>
  gt::tab_header(
    title = "Tabla 3. SGP y monto desembolsado entre quienes usaron crédito",
    subtitle = "Panel con efectos fijos de municipio y año, 2002–2023"
  ) |>
  gt::cols_label(Monto = "Desembolsos de crédito por habitante") |>
  gt::cols_align(align = "center", columns = Monto) |>
  gt::tab_options(
    table.font.size = gt::px(13),
    heading.title.font.size = gt::px(15),
    heading.subtitle.font.size = gt::px(12),
    data_row.padding = gt::px(2),
    column_labels.padding = gt::px(4),
    source_notes.font.size = gt::px(11),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |>
  gt::tab_source_note(
    paste(
      "Muestra de municipios-año con desembolsos positivos.",
      "Errores estándar agrupados por municipio, entre paréntesis.",
      "*** p < 0,01, ** p < 0,05, * p < 0,10."
    )
  )

gt::gtsave(
  data = tabla_monto,
  filename = "tabla_03_sgp_monto_positivo.png",
  path = tables
)

tabla_monto



# SEGUNDO BLOQUE: INDICADOR DE SOSTENIBILDIAD

# 12. Modelo de sostenibilidad aproximada de la deuda
modelo_sostenibilidad <- fixest::feols(
  sostenibilidad_proxy ~ sgp_pc_real2023 | codmpio + ano,
  data = panel,
  vcov = ~codmpio
)

summary(modelo_sostenibilidad)

# 13. Tabla del modelo de sostenibilidad
estimacion_sost <- fixest::coeftable(modelo_sostenibilidad)["sgp_pc_real2023", ]
p_sost <- estimacion_sost[4]

estrellas_sost <- if (p_sost < 0.01) {
  "***"
} else if (p_sost < 0.05) {
  "**"
} else if (p_sost < 0.10) {
  "*"
} else {
  ""
}

resultados_sost <- data.frame(
  Concepto = resultados_uso$Concepto,
  Sostenibilidad = c(
    cientifico(estimacion_sost[1]),
    paste0("(", cientifico(estimacion_sost[2]), ")", estrellas_sost),
    "Sí", "Sí",
    numero(stats::nobs(modelo_sostenibilidad)),
    numero(modelo_sostenibilidad$fixef_sizes[["codmpio"]]),
    numero(fixest::r2(modelo_sostenibilidad, "wr2"), 6)
  )
)

tabla_sost <- gt::gt(resultados_sost) |>
  gt::tab_header(
    title = "Tabla 4. SGP y sostenibilidad aproximada de la deuda, 2002–2023",
    subtitle = "Estimación de panel con efectos fijos"
  ) |>
  gt::cols_label(Sostenibilidad = "Deuda / ingresos corrientes previos (%)") |>
  gt::cols_align(align = "center", columns = Sostenibilidad) |>
  gt::tab_options(
    table.font.size = gt::px(13),
    heading.title.font.size = gt::px(15),
    heading.subtitle.font.size = gt::px(12),
    data_row.padding = gt::px(2),
    column_labels.padding = gt::px(4),
    source_notes.font.size = gt::px(11),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |>
  gt::tab_source_note(
    paste(
      "La variable dependiente usa una aproximación al saldo de deuda.",
      "Errores estándar agrupados por municipio, entre paréntesis.",
      "*** p < 0,01, ** p < 0,05, * p < 0,10."
    )
  )

gt::gtsave(
  data = tabla_sost,
  filename = "tabla_04_sgp_sostenibilidad.png",
  path = tables
)

tabla_sost

