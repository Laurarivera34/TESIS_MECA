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
  "xfun",
  "sf",           # Manipulación de datos espaciales
  "maps",         # Contorno de Colombia para ggplot2::map_data()
  "leaflet",      # Mapas interactivos
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

############## PARTE 3. TABLA 2. Registro de desembolsos municipales, 2000–2023

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

############## PARTE 4. TABLA 3: MAYORES RECEPTORES DEL SGP POR CATEGORÍA

# Categoría de la Ley 617 registrada en 2023
categorias_2023 <- base %>%
  filter(ano == 2023, !is.na(codmpio), !is.na(categ_cgn)) %>%
  distinct(codmpio, categ_cgn)

# Indicadores del SGP por municipio, 2002–2023
ranking_sgp_categoria <- base %>%
  filter(between(ano, 2002, 2023), !is.na(codmpio)) %>%
  mutate(
    comparable = !is.na(sgp_total_real2023) &
      !is.na(y_total_real2023) & y_total_real2023 > 0
  ) %>%
  arrange(ano) %>%
  group_by(codmpio) %>%
  summarise(
    municipio = last(municipio[!is.na(municipio)]),
    depto = last(depto[!is.na(depto)]),
    anos_sgp = n_distinct(ano[!is.na(sgp_total_real2023)]),
    sgp_acumulado = sum(sgp_total_real2023, na.rm = TRUE) / 1e6,
    sgp_acumulado_pc = if (all(is.na(sgp_pc_real2023))) {
      NA_real_
    } else {
      sum(sgp_pc_real2023, na.rm = TRUE)
    },
    sgp_ingresos = if (any(comparable)) {
      100 * sum(sgp_total_real2023[comparable]) /
        sum(y_total_real2023[comparable])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(anos_sgp > 0) %>%
  inner_join(
    categorias_2023,
    by = "codmpio",
    relationship = "many-to-one"
  ) %>%
  arrange(categ_cgn, desc(sgp_acumulado), codmpio) %>%
  group_by(categ_cgn) %>%
  mutate(puesto_categoria = row_number()) %>%
  ungroup()

# Primer puesto por categoría
sgp_slide <- ranking_sgp_categoria %>%
  filter(puesto_categoria == 1) %>%
  mutate(
    municipio = recode(
      str_to_title(municipio),
      "Bogota D C" = "Bogotá, D. C.",
      "Cucuta" = "Cúcuta",
      "San Andres De Tumaco" = "Tumaco",
      "Magangue" = "Magangué"
    )
  ) %>%
  select(
    categ_cgn, municipio, depto,
    sgp_acumulado, sgp_acumulado_pc, sgp_ingresos
  )

# Tabla estilo AER para la diapositiva
tabla_sgp_aer <- sgp_slide %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Tabla 3. Mayores receptores del SGP por categoría municipal",
    subtitle = "2002–2023 · Categoría de la Ley 617 registrada en 2023"
  ) %>%
  gt::cols_label(
    categ_cgn = "Categoría",
    municipio = "Municipio",
    depto = "Departamento",
    sgp_acumulado = "SGP acumulado",
    sgp_acumulado_pc = gt::html("SGP acumulado<br>por habitante"),
    sgp_ingresos = gt::html("SGP / ingresos<br>totales (%)")
  ) %>%
  gt::fmt_number(
    columns = c(sgp_acumulado, sgp_ingresos),
    decimals = 2,
    sep_mark = ",",
    dec_mark = "."
  ) %>%
  gt::fmt_number(
    columns = sgp_acumulado_pc,
    decimals = 0,
    sep_mark = ","
  ) %>%
  gt::sub_missing(missing_text = "—") %>%
  gt::cols_width(
    categ_cgn ~ gt::px(75),
    municipio ~ gt::px(125),
    depto ~ gt::px(155),
    sgp_acumulado ~ gt::px(145),
    sgp_acumulado_pc ~ gt::px(175),
    sgp_ingresos ~ gt::px(165)
  ) %>%
  gt::cols_align(align = "center", columns = categ_cgn) %>%
  gt::cols_align(align = "left", columns = c(municipio, depto)) %>%
  gt::cols_align(
    align = "right",
    columns = c(sgp_acumulado, sgp_acumulado_pc, sgp_ingresos)
  ) %>%
  gt::tab_source_note(
    gt::md(paste(
      "*Fuente:* Panel de Buen Gobierno del CEDE y panel municipal",
      "del Banco de la República. Cálculos propios."
    ))
  ) %>%
  gt::tab_source_note(
    gt::md(paste(
      "*Nota:* Cifras en billones de pesos constantes de 2023.",
      "El acumulado por habitante suma los valores anuales en pesos de 2023.",
      "El porcentaje usa los mismos años con SGP observado e ingresos totales positivos."
    ))
  ) %>%
  gt::tab_options(
    table.width = gt::px(840),
    table.background.color = "white",
    table.font.names = "Times New Roman",
    table.font.size = gt::px(14),
    
    heading.align = "left",
    heading.title.font.size = gt::px(17),
    heading.title.font.weight = "normal",
    heading.subtitle.font.size = gt::px(12),
    heading.border.bottom.style = "none",
    
    table.border.top.color = "black",
    table.border.top.width = gt::px(1.5),
    table.border.bottom.style = "none",
    
    column_labels.font.weight = "bold",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = gt::px(1),
    column_labels.vlines.style = "none",
    
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = gt::px(1.5),
    table_body.hlines.style = "none",
    table_body.vlines.style = "none",
    
    data_row.padding = gt::px(5),
    column_labels.padding = gt::px(6),
    source_notes.font.size = gt::px(11),
    source_notes.padding = gt::px(4)
  )

# Exportar PNG
tables <- here::here("output", "tables")

gt::gtsave(
  data = tabla_sgp_aer,
  filename = "tabla_03_sgp_por_categoria.png",
  path = tables,
  vwidth = 900,
  zoom = 2,
  expand = 12
)


############## PARTE 5
################ MAPA Y DISTRIBUCIÓN DE LOS RANGOS DEL IDF

library(dplyr)
library(ggplot2)
library(scales)

# Si aún no tienes el paquete del contorno de Colombia:
# install.packages("maps")

figures <- here::here("output", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

# 1. Seleccionar el año y clasificar los puntajes del IDF
rangos_idf <- c(
  "Deterioro (<40)",
  "Riesgo (40 a <60)",
  "Vulnerable (60 a <70)",
  "Solvente (70 a <80)",
  "Sostenible (≥80)"
)

colores_idf <- setNames(
  c("#F44343", "#F59D32", "#C060AE", "#45C474", "#008B40"),
  rangos_idf
)

idf_panel <- base %>%
  transmute(
    codmpio,
    ano,
    idf = as.numeric(as.character(df2_nuevoidf)),
    latitud = as.numeric(sub(",", ".", as.character(latitud), fixed = TRUE)),
    longitud = as.numeric(sub(",", ".", as.character(longitud), fixed = TRUE))
  ) %>%
  filter(!is.na(codmpio), !is.na(ano), is.finite(idf))

if (nrow(idf_panel) == 0) {
  stop("df2_nuevoidf no tiene puntajes válidos en la base.")
}

anio_idf <- max(idf_panel$ano)  # Para un corte fijo, usa anio_idf <- 2023

idf_municipal <- idf_panel %>%
  filter(ano == anio_idf) %>%
  distinct() %>%
  mutate(
    rango_idf = cut(
      idf,
      breaks = c(-Inf, 40, 60, 70, 80, Inf),
      labels = rangos_idf,
      right = FALSE
    )
  )

if (nrow(idf_municipal) == 0) {
  stop("El año seleccionado no tiene municipios con IDF válido.")
}

if (anyDuplicated(idf_municipal$codmpio) > 0) {
  stop("Hay registros distintos para un mismo municipio en el año seleccionado.")
}

# 2. Mapa de los rangos del IDF
colombia <- map_data("world", region = "Colombia")

idf_mapa <- idf_municipal %>%
  filter(between(longitud, -83, -66), between(latitud, -5, 14))

mapa_idf <- ggplot() +
  geom_polygon(
    data = colombia,
    aes(x = long, y = lat, group = group),
    fill = "#F1F1F1", colour = "#A6A6A6", linewidth = 0.25
  ) +
  geom_point(
    data = idf_mapa,
    aes(x = longitud, y = latitud, colour = rango_idf),
    size = 1.55, alpha = 0.9
  ) +
  scale_colour_manual(values = colores_idf, drop = FALSE) +
  coord_quickmap() +
  labs(
    title = paste0("Rangos del IDF municipal, ", anio_idf),
    colour = "Rango",
    caption = paste0(
      "Municipios ubicados: ", nrow(idf_mapa),
      " de ", nrow(idf_municipal), " con IDF válido."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    legend.position = "right",
    plot.caption = element_text(colour = "grey35", margin = margin(t = 8)),
    plot.margin = margin(12, 12, 12, 12)
  )

# 3. Distribución de municipios por rango
formato_pct <- label_percent(accuracy = 0.1, decimal.mark = ",")

distribucion_idf <- idf_municipal %>%
  count(rango_idf, name = "municipios", .drop = FALSE) %>%
  arrange(municipios, rango_idf) %>%
  mutate(
    porcentaje = municipios / sum(municipios),
    etiqueta = paste0(formato_pct(porcentaje), "\n", municipios),
    rango_grafico = factor(rango_idf, levels = as.character(rango_idf))
  )

grafico_idf <- ggplot(
  distribucion_idf,
  aes(x = porcentaje, y = rango_grafico, fill = rango_idf)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = colores_idf) +
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = paste0("Distribución de municipios por rango del IDF, ", anio_idf),
    x = "Porcentaje de municipios con IDF válido",
    y = NULL,
    caption = "Fuente: base municipal. Cálculos propios."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(colour = "grey35")
  )

# 4. Mostrar y guardar las figuras
print(mapa_idf)
print(grafico_idf)

ggsave(
  filename = file.path(figures, paste0("mapa_rangos_idf_", anio_idf, ".png")),
  plot = mapa_idf,
  width = 8, height = 7, dpi = 300, bg = "white"
)

ggsave(
  filename = file.path(figures, paste0("distribucion_rangos_idf_", anio_idf, ".png")),
  plot = grafico_idf,
  width = 9, height = 4.6, dpi = 300, bg = "white"
)