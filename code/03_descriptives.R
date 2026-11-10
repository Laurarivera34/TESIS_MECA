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

################ PRIMERA PARTE: ESTADÍSTICAS DESCRIPTIVAS

# Variables principales de la investigación
variables_tesis <- c(
  # Transferencias del SGP
  "sgp_pc",
  "dependencia_sgp",
  
  # Esfuerzo fiscal
  "recaudo_tributario_pc",
  "participacion_tributaria",
  
  # Sostenibilidad
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

# Cobertura general del panel
cobertura_panel <- base %>%
  summarise(
    municipios = n_distinct(codmpio, na.rm = TRUE),
    anos = n_distinct(ano, na.rm = TRUE),
    ano_inicio = min(ano, na.rm = TRUE),
    ano_fin = max(ano, na.rm = TRUE),
    observaciones = n()
  )

print(cobertura_panel)

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
    Media = if (all(is.na(valor))) NA_real_
    else mean(valor, na.rm = TRUE),
    Mediana = if (all(is.na(valor))) NA_real_
    else median(valor, na.rm = TRUE),
    DE = if (sum(!is.na(valor)) < 2) NA_real_
    else sd(valor, na.rm = TRUE),
    Min = if (all(is.na(valor))) NA_real_
    else min(valor, na.rm = TRUE),
    Max = if (all(is.na(valor))) NA_real_
    else max(valor, na.rm = TRUE),
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

# Nota metodológica con cobertura automática
nota_tabla <- paste0(
  "<div style='max-width:1050px;",
  "white-space:normal;line-height:1.5;'>",
  "<i>Nota:</i> El panel comprende ",
  format(cobertura_panel$municipios, big.mark = ","),
  " municipios y ",
  cobertura_panel$anos,
  " años (",
  cobertura_panel$ano_inicio,
  "–",
  cobertura_panel$ano_fin,
  "), con ",
  format(cobertura_panel$observaciones, big.mark = ","),
  " observaciones municipio-año. ",
  "N corresponde al número de observaciones disponibles ",
  "para cada variable; DE: desviación estándar. ",
  "Las variables monetarias por habitante están expresadas ",
  "en pesos corrientes. ",
  "Los indicadores fiscales se expresan en porcentajes, ",
  "salvo la variable de desembolsos positivos, ",
  "que toma valores de 0 y 1. ",
  "La sostenibilidad aproximada se construye a partir ",
  "de flujos de crédito y no corresponde al indicador ",
  "oficial de la Ley 358 de 1997.",
  "</div>"
)

# Tabla estilo AER
tabla_aer <- estadisticas %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Tabla 1. Estadísticas descriptivas de las principales variables"
  ) %>%
  gt::cols_label(
    variable = "Variable",
    N = "N",
    Media = "Media",
    Mediana = "Mediana",
    DE = "DE",
    Min = "Mín.",
    Max = "Máx."
  ) %>%
  gt::fmt_number(
    columns = c(Media, Mediana, DE, Min, Max),
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
    variable ~ gt::px(265),
    N ~ gt::px(85),
    Media ~ gt::px(125),
    Mediana ~ gt::px(125),
    DE ~ gt::px(125),
    Min ~ gt::px(115),
    Max ~ gt::px(145)
  ) %>%
  gt::cols_align(
    align = "left",
    columns = variable
  ) %>%
  gt::cols_align(
    align = "right",
    columns = c(N, Media, Mediana, DE, Min, Max)
  ) %>%
  gt::tab_source_note(
    source_note = gt::html(nota_tabla)
  ) %>%
  gt::tab_options(
    table.width = gt::px(985),
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

# Exportar PNG
gt::gtsave(
  data = tabla_aer,
  filename = file.path(tables, "estadisticas.png"),
  zoom = 2,
  expand = 25
)

# Visualizar
tabla_aer

############## PARTE 2. Figura 1. Desembolsos de crédito municipal

panel_desembolsos <- base %>%
  filter(between(ano, 2000, 2023), !is.na(codmpio),
         is.finite(finan_credito_desemb_real2023))

top10_desembolsos <- panel_desembolsos %>%
  group_by(codmpio) %>%
  summarise(
    municipio = last(municipio, order_by = ano),
    depto = last(depto, order_by = ano, na_rm = TRUE),
    desembolsos_billones = sum(finan_credito_desemb_real2023) / 1e6,
    .groups = "drop"
  ) %>%
  arrange(desc(desembolsos_billones), codmpio) %>%
  slice_head(n = 10) %>%
  mutate(
    etiqueta = case_when(
      codmpio == 11001 ~ "Bogotá D. C.",
      codmpio == 5001  ~ "Medellín",
      codmpio == 54001 ~ "Cúcuta",
      grepl("^san vicente", municipio, ignore.case = TRUE) & !is.na(depto) ~
        paste0(stringr::str_to_title(municipio, locale = "es"), " (",
               stringr::str_to_title(depto, locale = "es"), ")"),
      TRUE ~ stringr::str_to_title(municipio, locale = "es")
    )
  )

montos <- rev(top10_desembolsos$desembolsos_billones)
colores <- ifelse(rev(top10_desembolsos$codmpio) == 11001,
                  "#193C8C", "#A3ABB5")
periodo <- paste(range(panel_desembolsos$ano), collapse = "–")

par(mar = c(5.5, 12, 4, 2), bg = "white")

pos <- barplot(
  montos,
  names.arg = rev(top10_desembolsos$etiqueta),
  horiz = TRUE, las = 1, cex.names = 0.85,
  col = colores, border = NA,
  xlim = c(0, max(montos) * 1.15),
  xlab = "Billones de pesos constantes de 2023"
)

text(montos, pos,
     labels = formatC(montos, format = "f", digits = 2, decimal.mark = ","),
     pos = 4, offset = 0.35, cex = 0.8, col = "#25282D")

mtext("Figura 1. Desembolsos de crédito municipal",
      side = 3, line = 2.2, adj = 0, font = 2, cex = 1)
mtext(paste0("Diez municipios con mayores desembolsos acumulados, ", periodo),
      side = 3, line = 1, adj = 0, col = "#6B7280", cex = 0.8)
mtext("Fuente: CEDE.",
      side = 1, line = 4.3, adj = 0, col = "#6B7280", cex = 0.72)

grafico <- recordPlot()

withr::with_png(
  file.path(figures, "figura_01_desembolsos_top10.png"),
  replayPlot(grafico),
  width = 10, height = 6, units = "in", res = 300
)

withr::with_pdf(
  file.path(figures, "figura_01_desembolsos_top10.pdf"),
  replayPlot(grafico),
  width = 10, height = 6
)

############## PARTE 3. 
# TABLA 2. Registro de desembolsos municipales, 2000–2023

grupos <- c(
  "Con algún desembolso positivo",
  "Cero desembolsos en los años con datos",
  "Sin información",
  "Registros negativos sin desembolsos positivos"
)

registro_municipal <- base %>%
  filter(between(ano, 2000, 2023), !is.na(codmpio)) %>%
  group_by(codmpio) %>%
  summarise(
    anios_con_dato = n_distinct(ano[is.finite(finan_credito_desemb_real2023)]),
    con_positivo = any(is.finite(finan_credito_desemb_real2023) &
                         finan_credito_desemb_real2023 > 0),
    con_negativo = any(is.finite(finan_credito_desemb_real2023) &
                         finan_credito_desemb_real2023 < 0),
    .groups = "drop"
  ) %>%
  mutate(grupo = case_when(
    anios_con_dato == 0 ~ grupos[3],
    con_positivo ~ grupos[1],
    con_negativo ~ grupos[4],
    TRUE ~ grupos[2]
  ))

total_municipios <- nrow(registro_municipal)
ceros_parciales <- sum(
  registro_municipal$grupo == grupos[2] &
    registro_municipal$anios_con_dato < 24
)

tabla_2 <- registro_municipal %>%
  count(grupo, name = "municipios") %>%
  arrange(match(grupo, grupos)) %>%
  mutate(proporcion = municipios / total_municipios) %>%
  bind_rows(tibble(grupo = "Total", municipios = total_municipios, proporcion = 1))

tabla_2_gt <- tabla_2 %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Tabla 2. Registro de desembolsos municipales",
    subtitle = "Colombia, 2000–2023"
  ) %>%
  gt::cols_label(
    grupo = "Registro de desembolsos",
    municipios = "Municipios",
    proporcion = "% del total"
  ) %>%
  gt::cols_width(
    grupo ~ gt::px(280),
    municipios ~ gt::px(95),
    proporcion ~ gt::px(95)
  ) %>%
  gt::cols_align(align = "left", columns = grupo) %>%
  gt::cols_align(align = "right", columns = c(municipios, proporcion)) %>%
  gt::fmt_number(columns = municipios, decimals = 0, sep_mark = ",", dec_mark = ".") %>%
  gt::fmt_percent(columns = proporcion, decimals = 1, sep_mark = ",", dec_mark = ".") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(rows = grupo == "Total")
  ) %>%
  gt::tab_source_note(gt::html(
    "<i>Fuente:</i> Panel de Buen Gobierno del CEDE. Cálculos propios."
  )) %>%
  gt::tab_source_note(gt::html(paste0(
    "<i>Notas:</i> Los porcentajes se calculan sobre los ",
    format(total_municipios, big.mark = ",", decimal.mark = "."),
    " municipios de la base. De los municipios con desembolsos en cero, ",
    ceros_parciales, " tienen datos para menos de 24 años. ",
    "«Sin información» indica que faltan datos de desembolsos para todo el periodo."
  ))) %>%
  gt::tab_options(
    table.width = gt::px(470),
    table.align = "left",
    table.background.color = "white",
    table.font.names = "Times New Roman",
    table.font.size = gt::px(12),
    heading.align = "left",
    heading.title.font.size = gt::px(14),
    heading.title.font.weight = "normal",
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.style = "none",
    heading.padding = gt::px(3),
    column_labels.padding = gt::px(4),
    data_row.padding = gt::px(3),
    source_notes.font.size = gt::px(10),
    source_notes.padding = gt::px(2)
  )

gt::gtsave(
  tabla_2_gt,
  filename = file.path(tables, "tabla_02_registro_desembolsos_compacta.png"),
  zoom = 2,
  expand = 12
)

tabla_2_gt