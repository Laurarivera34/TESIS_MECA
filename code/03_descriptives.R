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

# La Parte 5 usa maps a través de ggplot2::map_data().
invisible(
  lapply(setdiff(paquetes, "maps"), library, character.only = TRUE)
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


############## PARTE 5. MAPA Y DISTRIBUCIÓN DE LOS RANGOS DEL IDF

# 1. Seleccionar el año y clasificar el IDF
anio_idf <- 2023

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

idf_municipal <- base %>%
  filter(ano == anio_idf, !is.na(codmpio)) %>%
  transmute(
    codigo_dane = stringr::str_pad(
      trimws(as.character(codmpio)), width = 5, pad = "0"
    ),
    idf = as.numeric(sub(",", ".", as.character(df2_nuevoidf), fixed = TRUE))
  ) %>%
  filter(is.finite(idf)) %>%
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
  stop("No hay puntajes válidos del IDF para ", anio_idf, ".")
}

if (anyDuplicated(idf_municipal$codigo_dane) > 0) {
  stop("Hay municipios con más de un puntaje del IDF para ", anio_idf, ".")
}

# 2. Cargar los límites municipales y unirlos con el IDF
ruta_municipios <- here::here(
  "data", "raw", "MGN_ADM_MPIO_GRAFICO (Geopackage).gpkg"
)

if (!file.exists(ruta_municipios)) {
  stop("No se encontró el GeoPackage en: ", ruta_municipios)
}

municipios_col <- sf::st_read(ruta_municipios, quiet = TRUE)

municipios_idf <- municipios_col %>%
  mutate(
    codigo_dane = stringr::str_pad(
      trimws(as.character(mpio_cdpmp)), width = 5, pad = "0"
    )
  ) %>%
  left_join(
    idf_municipal,
    by = "codigo_dane",
    relationship = "many-to-one"
  )

sin_poligono <- anti_join(
  idf_municipal,
  sf::st_drop_geometry(municipios_idf),
  by = "codigo_dane"
)

message("Municipios con IDF sin coincidencia en la capa: ", nrow(sin_poligono))

# 3. Dibujar el mapa de rangos del IDF
mapa_idf <- ggplot(municipios_idf) +
  geom_sf(
    aes(fill = rango_idf),
    colour = "#777777",
    linewidth = 0.12
  ) +
  scale_fill_manual(
    values = colores_idf,
    breaks = rangos_idf,
    drop = FALSE,
    na.value = "#E5E5E5"
  ) +
  labs(
    title = paste0("Rangos del IDF municipal, ", anio_idf),
    fill = "Mapa 1. Rango del IDF",
    caption = paste(
      "Fuente: CEDE (IDF) y DANE (cartografía municipal). Cálculos propios.",
      "Nota: El gris indica polígonos sin puntaje asociado.",
      sep = "\n"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    legend.position = "right",
    legend.key.size = grid::unit(4, "mm"),
    plot.caption = element_text(
      colour = "grey35", hjust = 0, margin = margin(t = 8)
    ),
    plot.margin = margin(12, 12, 12, 12)
  )

# 4. Graficar la distribución de municipios por rango
formato_pct <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")

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
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = paste0(
      "Figura 2. Distribución de municipios por rango del IDF, ", anio_idf
    ),
    x = "Porcentaje de municipios con IDF",
    y = NULL,
    caption = "Fuente: CEDE. Cálculos propios."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.caption = element_text(colour = "grey35", hjust = 0)
  )

# 5. Mostrar y guardar las dos figuras
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

############## PARTE 6.IDF MUNICIPAL PROMEDIO POR REGIÓN, 2021–2023
anios_idf <- 2021:2023

regiones_idf <- c(
  gorinoquia = "Orinoquía",
  gandina = "Andina",
  gpacifica = "Pacífica",
  gcaribe = "Caribe",
  gamazonia = "Amazonía"
)

orden_regiones <- c("Nacional", unname(regiones_idf))

figures <- here::here("output", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

# 1. Preparar un puntaje de IDF por municipio y año
idf_panel_regional <- base %>%
  filter(ano %in% anios_idf, !is.na(codmpio)) %>%
  transmute(
    codmpio,
    ano,
    idf = as.numeric(sub(",", ".", as.character(df2_nuevoidf), fixed = TRUE)),
    across(
      all_of(names(regiones_idf)),
      ~ as.character(.x) %in% c("1", "TRUE")
    )
  ) %>%
  filter(is.finite(idf)) %>%
  distinct()

if (anyDuplicated(idf_panel_regional[c("codmpio", "ano")]) > 0) {
  stop("Hay registros distintos de IDF o región para un mismo municipio y año.")
}

# 2. Identificar la región de cada municipio
idf_regiones <- idf_panel_regional %>%
  pivot_longer(
    cols = all_of(names(regiones_idf)),
    names_to = "variable_region",
    values_to = "pertenece"
  ) %>%
  filter(pertenece) %>%
  mutate(region = unname(regiones_idf[variable_region]))

if (anyDuplicated(idf_regiones[c("codmpio", "ano")]) > 0) {
  stop("Hay municipios asignados a más de una región en el mismo año.")
}

# 3. Calcular los promedios regionales y el nacional
promedio_regional <- idf_regiones %>%
  group_by(region, ano) %>%
  summarise(idf_promedio = mean(idf), .groups = "drop")

promedio_nacional <- idf_panel_regional %>%
  group_by(ano) %>%
  summarise(idf_promedio = mean(idf), .groups = "drop") %>%
  mutate(region = "Nacional")

idf_promedios <- bind_rows(promedio_nacional, promedio_regional) %>%
  mutate(
    region = factor(region, levels = orden_regiones),
    anio_grafico = factor(ano, levels = anios_idf),
    etiqueta = formatC(
      idf_promedio, format = "f", digits = 1, decimal.mark = ","
    )
  )

# 4. Graficar
grafico_idf_regional <- ggplot(
  idf_promedios,
  aes(x = anio_grafico, y = idf_promedio, fill = anio_grafico)
) +
  geom_col(width = 0.82) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.4,
    size = 3.2,
    colour = "grey20"
  ) +
  facet_wrap(
    vars(region),
    nrow = 1,
    strip.position = "bottom",
    drop = FALSE
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c(
      "2021" = "#B9BEC6",
      "2022" = "#7185A2",
      "2023" = "#193C8C"
    )
  ) +
  labs(
    title = "Figura 3. IDF municipal promedio por región, 2021–2023",
    x = NULL,
    y = "Puntaje promedio del IDF",
    caption = paste(
      "Fuente: CEDE. Cálculos propios.",
      "Nota: Promedio de los municipios con puntaje disponible en cada año.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0.5, margin = margin(b = 12)
    ),
    panel.grid = element_blank(),
    panel.spacing.x = grid::unit(0.8, "lines"),
    axis.text.x = element_text(size = 8, colour = "grey30"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(
      size = 9, face = "bold", margin = margin(t = 7, b = 4)
    ),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, colour = "grey35", size = 9)
  )

print(grafico_idf_regional)

# 5. Guardar la figura
ggsave(
  filename = file.path(figures, "idf_promedio_por_region_2021_2023.png"),
  plot = grafico_idf_regional,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

############ PARTE 7. IDF POR CATEGORIA LEY 617 DE 2000

# 1. Preparar un puntaje y una categoría por municipio y año
idf_panel_categoria <- base %>%
  filter(ano %in% anios_idf, !is.na(codmpio)) %>%
  transmute(
    codmpio,
    ano,
    idf = as.numeric(sub(",", ".", as.character(df2_nuevoidf), fixed = TRUE)),
    codigo_categoria = as.integer(categ_cgn)
  ) %>%
  filter(is.finite(idf), codigo_categoria %in% 0:6) %>%
  transmute(
    codmpio,
    ano,
    idf,
    categoria = factor(
      codigo_categoria,
      levels = 0:6,
      labels = c("Especial", paste("Categoría", 1:6))
    )
  ) %>%
  distinct()

if (anyDuplicated(idf_panel_categoria[c("codmpio", "ano")]) > 0) {
  stop("Hay registros distintos de IDF o categoría para un mismo municipio y año.")
}

# 2. Revisar la cobertura por año
cobertura_idf_categoria <- idf_panel_categoria %>%
  count(ano, name = "municipios") %>%
  complete(ano = anios_idf, fill = list(municipios = 0L)) %>%
  arrange(ano)

print(cobertura_idf_categoria)

# 3. Calcular el promedio por categoría y año
idf_promedio_categoria <- idf_panel_categoria %>%
  group_by(categoria, ano) %>%
  summarise(idf_promedio = mean(idf), .groups = "drop") %>%
  complete(categoria, ano = anios_idf) %>%
  mutate(
    anio_grafico = factor(ano, levels = anios_idf),
    etiqueta = if_else(
      is.na(idf_promedio),
      NA_character_,
      formatC(idf_promedio, format = "f", digits = 1, decimal.mark = ",")
    )
  )

# 4. Graficar barras verticales agrupadas por categoría
grafico_idf_categoria <- ggplot(
  idf_promedio_categoria,
  aes(x = anio_grafico, y = idf_promedio, fill = anio_grafico)
) +
  geom_col(width = 0.82, na.rm = TRUE) +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.5,
    size = 3.2,
    colour = "grey20",
    na.rm = TRUE
  ) +
  facet_wrap(vars(categoria), nrow = 1, strip.position = "bottom") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ","),
    expand = expansion(mult = c(0, 0.12))
  ) +
  expand_limits(y = 0) +
  scale_fill_manual(
    values = c("2021" = "#B9BEC6", "2022" = "#7185A2", "2023" = "#193C8C")
  ) +
  labs(
    title = "Figura 4. IDF municipal promedio por categoría de la Ley 617, 2021–2023",
    x = NULL,
    y = "Puntaje promedio del IDF",
    caption = paste(
      "Fuente: CEDE y Banco de la República. Cálculos propios.",
      "Nota: Promedio de los municipios con IDF y categoría disponibles en cada año.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.spacing.x = grid::unit(0.8, "lines"),
    axis.text.x = element_text(size = 8, colour = "grey30"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(
      size = 10, face = "bold", margin = margin(t = 7, b = 4)
    ),
    legend.position = "none",
    plot.caption = element_text(colour = "grey35", hjust = 0)
  )

print(grafico_idf_categoria)

# 5. Guardar el gráfico
ggsave(
  filename = file.path(figures, "idf_promedio_por_categoria_617_2021_2023.png"),
  plot = grafico_idf_categoria,
  width = 12,
  height = 4.8,
  dpi = 300,
  bg = "white"
)

################ PARTE 8. Ingresos propios, transferencias y desembolsos de crédito, 2002–2023

anios_ingresos <- 2002:2023

rubros_ingresos <- c(
  y_corr = "Ingresos corrientes",
  y_cap_transf = "Transferencias",
  y_cap_cofinan = "Cofinanciación",
  finan_credito_desemb = "Desembolsos de crédito"
)

variables_ingresos <- c("y_total", names(rubros_ingresos))
faltantes <- setdiff(c("codmpio", "ano", variables_ingresos), names(base))

if (length(faltantes) > 0) {
  stop("Faltan columnas en base: ", paste(faltantes, collapse = ", "))
}

figures <- here::here("output", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

# 1. Seleccionar municipios con datos para las cuatro series
base_ingresos_credito <- base %>%
  select(codmpio, ano, all_of(variables_ingresos)) %>%
  filter(
    ano %in% anios_ingresos,
    !is.na(codmpio),
    y_total > 0,
    if_all(all_of(variables_ingresos), is.finite)
  ) %>%
  distinct()

if (anyDuplicated(base_ingresos_credito[c("codmpio", "ano")]) > 0) {
  stop("Hay registros distintos para un mismo municipio y año.")
}

# 2. Calcular los agregados anuales y revisar la cobertura
ingresos_credito_anual <- base_ingresos_credito %>%
  group_by(ano) %>%
  summarise(
    municipios = n(),
    across(all_of(variables_ingresos), sum),
    .groups = "drop"
  ) %>%
  complete(ano = anios_ingresos, fill = list(municipios = 0L)) %>%
  arrange(ano)

print(select(ingresos_credito_anual, ano, municipios), n = Inf)

# 3. Calcular los porcentajes y preparar las etiquetas
series_ingresos <- ingresos_credito_anual %>%
  pivot_longer(
    cols = all_of(names(rubros_ingresos)),
    names_to = "rubro",
    values_to = "monto"
  ) %>%
  mutate(
    participacion = 100 * monto / y_total,
    serie = factor(
      rubro,
      levels = names(rubros_ingresos),
      labels = unname(rubros_ingresos)
    )
  )

etiquetas_ingresos <- series_ingresos %>%
  filter(ano %in% c(2022, 2023), is.finite(participacion)) %>%
  mutate(
    etiqueta = scales::label_number(
      accuracy = 0.1, decimal.mark = ",", suffix = " %"
    )(participacion),
    vjust_etiqueta = if_else(rubro == "y_cap_cofinan", 1.5, -0.8)
  )

# 4. Graficar las cuatro líneas en un solo panel
grafico_ingresos_credito <- ggplot(
  series_ingresos,
  aes(x = ano, y = participacion, colour = serie)
) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(data = etiquetas_ingresos, size = 2, show.legend = FALSE) +
  geom_text(
    data = etiquetas_ingresos,
    aes(label = etiqueta, vjust = vjust_etiqueta),
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Ingresos corrientes" = "#666666",
      "Transferencias" = "#193C8C",
      "Cofinanciación" = "#32745E",
      "Desembolsos de crédito" = "#E5AD00"
    )
  ) +
  scale_x_continuous(
    breaks = c(seq(2002, 2020, by = 3), 2022, 2023),
    expand = expansion(add = c(0.5, 0.8))
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", suffix = " %"),
    expand = expansion(mult = c(0.06, 0.12))
  ) +
  expand_limits(y = 0) +
  labs(
    title = "Figura 5. Composición de los ingresos municipales 2002–2023",
    subtitle = "% de participación en los ingresos totales",
    x = NULL,
    y = NULL,
    colour = NULL,
    caption = paste(
      "Fuente: Panel Buen Gobierno del CEDE. Cálculos propios.",
      "Nota. Los ingresos corrientes incluyen tributarios, no tributarios y transferencias corrientes.",
      "Las transferencias de capital incluyen SGP, SGR y otras. La cofinanciación proviene de la Nación o de entidades territoriales.",
      "Los desembolsos son flujos brutos de crédito interno y externo",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, colour = "grey35", size = 9)
  )

print(grafico_ingresos_credito)

# 5. Guardar en output/figures
ggsave(
  filename = file.path(figures, "ingresos_municipales_y_credito_2002_2023.png"),
  plot = grafico_ingresos_credito,
  width = 11,
  height = 6.2,
  dpi = 300,
  bg = "white"
)

################ PARTE 9. DESEMBOLSOS DE CRÉDITO MUNICIPAL, 2002–2023

anios_credito <- 2002:2023
figures <- here::here("output", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

# 1. Seleccionar los desembolsos brutos a precios constantes
base_desembolsos <- base %>%
  filter(
    ano %in% anios_credito,
    !is.na(codmpio),
    is.finite(finan_credito_desemb_real2023)
  ) %>%
  distinct(codmpio, ano, finan_credito_desemb_real2023)

if (anyDuplicated(base_desembolsos[c("codmpio", "ano")]) > 0) {
  stop("Hay desembolsos distintos para un mismo municipio y año.")
}

# 2. Sumar los desembolsos anuales y convertir de millones a billones
desembolsos_anuales <- base_desembolsos %>%
  group_by(ano) %>%
  summarise(
    municipios = n(),
    desembolsos_billones = sum(finan_credito_desemb_real2023) / 1e6,
    .groups = "drop"
  ) %>%
  complete(ano = anios_credito, fill = list(municipios = 0L)) %>%
  arrange(ano)

print(desembolsos_anuales, n = Inf)

etiquetas_desembolsos <- desembolsos_anuales %>%
  filter(is.finite(desembolsos_billones)) %>%
  mutate(
    etiqueta = scales::label_number(
      accuracy = 0.01,
      decimal.mark = ",",
      big.mark = "."
    )(desembolsos_billones)
  )

# 3. Graficar la serie y separar las etiquetas de los puntos
grafico_desembolsos <- ggplot(
  desembolsos_anuales,
  aes(x = ano, y = desembolsos_billones)
) +
  geom_line(colour = "#193C8C", linewidth = 1.1, na.rm = TRUE) +
  geom_point(
    shape = 21,
    fill = "white",
    colour = "#193C8C",
    size = 2.6,
    stroke = 0.9,
    na.rm = TRUE
  ) +
  ggrepel::geom_label_repel(
    data = etiquetas_desembolsos,
    aes(label = etiqueta),
    direction = "y",
    nudge_y = 0.22,
    seed = 617,
    max.overlaps = Inf,
    box.padding = 0.25,
    point.padding = 0.35,
    label.padding = grid::unit(0.12, "lines"),
    min.segment.length = 0,
    segment.colour = "#AAB2BE",
    colour = "#193C8C",
    fill = "white",
    size = 3
  ) +
  scale_x_continuous(
    breaks = anios_credito,
    expand = expansion(add = c(0.8, 0.8))
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0.08, 0.20))
  ) +
  expand_limits(y = 0) +
  labs(
    title = "Desembolsos de crédito municipal 2002–2023",
    subtitle = "Billones de pesos constantes de 2023",
    x = NULL,
    y = NULL,
    caption = paste(
      "Fuente: Panel Buen Gobierno del CEDE. Cálculos propios.",
      "Nota. Suma anual de los desembolsos de los municipios.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey35"),
    axis.text.x = element_text(size = 8, colour = "grey35"),
    panel.grid.major.y = element_line(colour = "#E2E5EA"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, colour = "grey35", size = 9),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(12, 16, 12, 16)
  )

print(grafico_desembolsos)

# 4. Guardar en output/figures
ggsave(
  filename = file.path(
    figures, "desembolsos_credito_municipal_real_2002_2023.png"
  ),
  plot = grafico_desembolsos,
  width = 13,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

################ PARTE 10. INTERESES DE LA DEUDA MUNICIPAL, 2002–2023

# 1. Clasificar los municipios según sus desembolsos de 2023
desembolsos_municipales_2023 <- base %>%
  filter(ano == 2023, !is.na(codmpio)) %>%
  transmute(
    codigo_dane = stringr::str_pad(
      trimws(as.character(codmpio)), width = 5, pad = "0"
    ),
    finan_credito_desemb
  ) %>%
  distinct() %>%
  mutate(
    recibio_desembolsos = case_when(
      !is.finite(finan_credito_desemb) ~ NA_character_,
      finan_credito_desemb > 0 ~ "Sí",
      finan_credito_desemb == 0 ~ "No",
      TRUE ~ NA_character_
    )
  )

if (anyDuplicated(desembolsos_municipales_2023$codigo_dane) > 0L) {
  stop("Hay municipios con más de un valor de desembolsos en 2023.")
}

# 2. Leer la misma cartografía municipal de la Parte 5
ruta_municipios <- here::here(
  "data", "raw", "MGN_ADM_MPIO_GRAFICO (Geopackage).gpkg"
)

municipios_col <- sf::st_read(ruta_municipios, quiet = TRUE)

# 3. Unir la clasificación a los polígonos
mapa_desembolsos_2023 <- municipios_col %>%
  mutate(
    codigo_dane = stringr::str_pad(
      trimws(as.character(mpio_cdpmp)), width = 5, pad = "0"
    )
  ) %>%
  left_join(
    select(desembolsos_municipales_2023, codigo_dane, recibio_desembolsos),
    by = "codigo_dane",
    relationship = "many-to-one"
  ) %>%
  mutate(
    recibio_desembolsos = factor(recibio_desembolsos, levels = c("Sí", "No"))
  )

# Revisar la cobertura del mapa
mapa_desembolsos_2023 %>%
  sf::st_drop_geometry() %>%
  distinct(codigo_dane, recibio_desembolsos) %>%
  count(recibio_desembolsos, name = "municipios") %>%
  print()

# 4. Dibujar el mapa
grafico_mapa_desembolsos_2023 <- ggplot(mapa_desembolsos_2023) +
  geom_sf(
    aes(fill = recibio_desembolsos),
    colour = "#AAB2BE",
    linewidth = 0.12
  ) +
  scale_fill_manual(
    values = c("Sí" = "#193C8C", "No" = "#C9CED6"),
    breaks = c("Sí", "No"),
    na.value = "transparent",
    na.translate = FALSE,
    drop = FALSE
  ) +
  labs(
    title = "Figura 8. Desembolsos de crédito municipal, 2023",
    fill = "¿Recibió desembolsos?",
    caption = paste(
      "Fuente: CEDE y DANE (cartografía municipal). Cálculos propios.",
      sep = "\n"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    legend.position = "right",
    legend.key.size = grid::unit(4, "mm"),
    plot.caption = element_text(
      colour = "grey35", hjust = 0, margin = margin(t = 8)
    ),
    plot.margin = margin(12, 12, 12, 12)
  )

print(grafico_mapa_desembolsos_2023)

# 5. Guardar en la carpeta existente output/figures
ggsave(
  filename = file.path(figures, "mapa_desembolsos_credito_municipal_2023.png"),
  plot = grafico_mapa_desembolsos_2023,
  width = 8,
  height = 7,
  dpi = 300,
  bg = "white"
)


############## PARTE 11. AMORTIZACIONES COMO PORCENTAJE DEL PIB MUNICIPAL

amortizaciones_pct_pib <- base %>%
  filter(
    between(ano, 2000, 2009),
    !is.na(codmpio),
    is.finite(finan_credito_amort_real2023),
    is.finite(pib_total_real2023),
    pib_total_real2023 > 0
  ) %>%
  group_by(ano) %>%
  summarise(
    municipios = n_distinct(codmpio),
    amortizaciones_millones_2023 = sum(finan_credito_amort_real2023),
    pib_millones_2023 = sum(pib_total_real2023),
    pct_pib = 100 * amortizaciones_millones_2023 / pib_millones_2023,
    .groups = "drop"
  )

print(amortizaciones_pct_pib, n = Inf)

formato_pct <- scales::label_number(
  accuracy = 0.01,
  decimal.mark = ",",
  suffix = " %"
)

figura_amortizaciones_pct_pib <- ggplot(
  amortizaciones_pct_pib,
  aes(x = factor(ano, levels = 2000:2009), y = pct_pib)
) +
  geom_col(fill = "#193C8C", width = 0.65) +
  geom_text(
    aes(label = formato_pct(pct_pib)),
    vjust = -0.5,
    color = "#193C8C",
    size = 3.5
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = formato_pct,
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Figura X.Amortizaciones de crédito como % del PIB",
    x = NULL,
    y = NULL,
    caption = "Fuente: Panel Municipal del CEDE. Cálculos propios."
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

print(figura_amortizaciones_pct_pib)

ggsave(
  file.path(figures, "amortizaciones_pct_pib_municipal_2000_2009.png"),
  plot = figura_amortizaciones_pct_pib,
  width = 9,
  height = 4.8,
  dpi = 300
)

############## PARTE 12. SGP COMO PORCENTAJE DEL PIB MUNICIPAL

sgp_pct_pib <- base %>%
  filter(
    between(ano, 2002, 2009),
    !is.na(codmpio),
    is.finite(sgp_total_real2023),
    is.finite(pib_total_real2023),
    pib_total_real2023 > 0
  ) %>%
  group_by(ano) %>%
  summarise(
    municipios = n_distinct(codmpio),
    sgp_millones_2023 = sum(sgp_total_real2023),
    pib_millones_2023 = sum(pib_total_real2023),
    pct_pib = 100 * sgp_millones_2023 / pib_millones_2023,
    .groups = "drop"
  )

print(sgp_pct_pib, n = Inf)

formato_pct <- scales::label_number(
  accuracy = 0.01,
  decimal.mark = ",",
  suffix = " %"
)

figura_sgp_pct_pib <- ggplot(
  sgp_pct_pib,
  aes(x = factor(ano, levels = 2002:2009), y = pct_pib)
) +
  geom_col(fill = "#193C8C", width = 0.65) +
  geom_text(
    aes(label = formato_pct(pct_pib)),
    vjust = -0.5,
    color = "#193C8C",
    size = 3.5
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = formato_pct,
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Figura X.Transferencias del SGP como % del PIB",
    x = NULL,
    y = NULL,
    caption = paste(
      "Fuente: Panel Municipal del CEDE. Cálculos propios.",
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

print(figura_sgp_pct_pib)

ggsave(
  file.path(figures, "sgp_pct_pib_municipal_2002_2009.png"),
  plot = figura_sgp_pct_pib,
  width = 9,
  height = 4.8,
  dpi = 300
)

############## PARTE 13. COMPOSICIÓN PROMEDIO DE LA INVERSIÓN POR CATEGORÍA

categorias_617 <- c("Especial", as.character(1:6))

# 1. Definir la paleta y el formato de las etiquetas
colores_prioridades <- c(
  "Educación"                         = "#193C8C",
  "Salud"                             = "#2C659F",
  "Agua potable y saneamiento básico" = "#76A9D2",
  "Transporte"                        = "#B5D2E9",
  "Vivienda"                          = "#4D6072",
  "Cultura"                           = "#7E8E9F",
  "Deporte y recreación"              = "#AFBCC9",
  "Otros sectores"                    = "#DFE1E4"
)

sectores_oscuros <- c("Educación", "Salud", "Vivienda")
sectores_etiqueta_vertical <- c("Vivienda", "Cultura", "Deporte y recreación")
umbral_etiqueta <- 0.05

formato_pct <- scales::label_percent(
  accuracy = 0.1,
  decimal.mark = ","
)

# 2. Calcular el total y la participación sectorial por año y categoría
composicion_anual <- inversion_larga %>%
  filter(ano %in% 2003:2010) %>%
  group_by(ano, categoria) %>%
  mutate(total_barra = sum(monto)) %>%
  ungroup()

revision_composicion <- composicion_anual %>%
  filter(
    !is.finite(monto) | monto < 0 |
      !is.finite(total_barra) | total_barra <= 0
  ) %>%
  distinct(ano, categoria)

if (nrow(revision_composicion) > 0) {
  print(revision_composicion, n = Inf)
  stop("Revisa los montos sectoriales o los totales de estos años y categorías.",
       call. = FALSE)
}

# 3. Promediar las participaciones anuales y ubicar las etiquetas
composicion_promedio <- composicion_anual %>%
  mutate(participacion = monto / total_barra) %>%
  group_by(categoria, sector) %>%
  summarise(
    participacion = mean(participacion),
    .groups = "drop"
  ) %>%
  mutate(
    categoria = factor(categoria, levels = categorias_617),
    sector = factor(sector, levels = names(colores_prioridades)),
    fila = match(as.character(categoria), rev(categorias_617)),
    etiqueta = formato_pct(participacion),
    color_etiqueta = if_else(
      sector %in% sectores_oscuros, "#FFFFFF", "#172B3E"
    )
  ) %>%
  arrange(categoria, sector) %>%
  group_by(categoria) %>%
  mutate(
    fin = cumsum(participacion),
    inicio = fin - participacion,
    centro = (inicio + fin) / 2
  ) %>%
  ungroup()

# Tabla con los porcentajes de todos los sectores
tabla_porcentajes_inversion <- composicion_promedio %>%
  select(categoria, sector, etiqueta) %>%
  pivot_wider(names_from = sector, values_from = etiqueta)

print(tabla_porcentajes_inversion, n = Inf, width = Inf)

# 4. Graficar las barras y los porcentajes
grafico_prioridades_inversion <- ggplot(composicion_promedio) +
  geom_rect(
    data = filter(composicion_promedio, participacion > 0),
    aes(
      xmin = inicio, xmax = fin,
      ymin = fila - 0.36, ymax = fila + 0.36,
      fill = sector
    ),
    colour = "white",
    linewidth = 0.25
  ) +
  # Etiquetas horizontales desde 5 %
  geom_text(
    data = filter(composicion_promedio, participacion >= umbral_etiqueta),
    aes(x = centro, y = fila, label = etiqueta, colour = color_etiqueta),
    size = 3.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Etiquetas verticales para los tres sectores con segmentos estrechos
  geom_text(
    data = filter(
      composicion_promedio,
      sector %in% sectores_etiqueta_vertical,
      participacion > 0,
      participacion < umbral_etiqueta
    ),
    aes(x = centro, y = fila, label = etiqueta, colour = color_etiqueta),
    angle = 90,
    size = 2.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_manual(
    values = colores_prioridades,
    breaks = names(colores_prioridades)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.005))
  ) +
  scale_y_continuous(
    breaks = seq_along(categorias_617),
    labels = rev(categorias_617),
    limits = c(0.5, length(categorias_617) + 0.5),
    expand = expansion(mult = 0)
  ) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  labs(
    title = "Participación sectorial promedio en la inversión municipal",
    subtitle = "Categorías de la Ley 617 de 2000 | 2003–2010",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = paste(
      "Fuente: Panel de Buen Gobierno del CEDE. Cálculos propios.",
      "Nota: Las participaciones se calculan por año y categoría. Cada año disponible tiene el mismo peso en el promedio.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      size = 19, face = "bold", colour = "#172B3E",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      colour = "#465667", margin = margin(b = 14)
    ),
    axis.text.y = element_text(face = "bold", colour = "#172B3E"),
    axis.text.x = element_text(colour = "#465667"),
    panel.grid.major.x = element_line(colour = "#E0E5EA", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(colour = "#273A4B"),
    plot.caption.position = "plot",
    plot.caption = element_text(
      hjust = 0, colour = "#465667", margin = margin(t = 8)
    ),
    plot.margin = margin(14, 18, 12, 14)
  )

print(grafico_prioridades_inversion)

# 5. Guardar la figura
ggsave(
  filename = file.path(
    figures, "composicion_promedio_inversion_categoria_617_2003_2010.png"
  ),
  plot = grafico_prioridades_inversion,
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

###############PARTE 14. EVOLUCIÓN Y COMPOSICIÓN DEL SGP MUNICIPAL

# 1. Preparar los componentes en millones de pesos de 2023
colores_sgp <- c(
  "Educación"                         = "#193C8C",
  "Salud"                             = "#2C659F",
  "Agua potable y saneamiento básico" = "#76A9D2",
  "Propósito general"                 = "#7E8E9F",
  "Asignaciones especiales"           = "#D4D9DF"
)

componentes_sgp <- c(
  "educacion", "salud", "agua", "proposito_general", "asignaciones_especiales"
)

sgp_municipal <- base %>%
  filter(between(ano, 2002, 2023), !is.na(codmpio),
         !is.na(sgp_total_real2023)) %>%
  transmute(
    codmpio,
    ano,
    total_reportado = sgp_total_real2023,
    educacion = sgp_educaciontot_real2023,
    salud = sgp_saludtot_real2023,
    agua = if_else(
      ano <= 2007, sgp_propgeneral_ap_real2023, sgp_agua_real2023
    ),
    proposito_general = if_else(
      ano <= 2007,
      sgp_propgeneraltot_real2023 - sgp_propgeneral_ap_real2023,
      sgp_propgeneraltot_real2023
    ),
    asignaciones_especiales = rowSums(
      pick(
        sgp_alimescolar_real2023,
        sgp_riberenos_real2023,
        sgp_resgindigena_real2023,
        sgp_fp_real2023,
        sgp_pinfancia_real2023
      )
    )
  )

# 2. Sumar los montos anuales y convertir de millones a billones
sgp_anual <- sgp_municipal %>%
  drop_na(all_of(componentes_sgp)) %>%
  group_by(ano) %>%
  summarise(
    municipios_grafico = n_distinct(codmpio),
    across(all_of(c(componentes_sgp, "total_reportado")), ~ sum(.x) / 1e6),
    .groups = "drop"
  )

# 3. Revisar cobertura y conciliar los componentes con el total
conciliacion_sgp <- tibble(ano = 2002:2023) %>%
  left_join(
    sgp_municipal %>%
      group_by(ano) %>%
      summarise(municipios_con_total = n_distinct(codmpio), .groups = "drop"),
    by = "ano"
  ) %>%
  left_join(sgp_anual, by = "ano") %>%
  mutate(
    municipios_con_total = coalesce(municipios_con_total, 0L),
    municipios_grafico = coalesce(municipios_grafico, 0L),
    municipios_excluidos = municipios_con_total - municipios_grafico,
    suma_componentes = rowSums(pick(all_of(componentes_sgp))),
    diferencia_billones = total_reportado - suma_componentes,
    diferencia_pct = if_else(
      total_reportado > 0,
      100 * diferencia_billones / total_reportado,
      NA_real_
    )
  ) %>%
  select(
    ano, municipios_con_total, municipios_grafico, municipios_excluidos,
    total_reportado, suma_componentes, diferencia_billones, diferencia_pct
  )

print(conciliacion_sgp, n = Inf, width = Inf)

# 4. Graficar la composición anual del SGP
sgp_grafico <- sgp_anual %>%
  pivot_longer(
    cols = all_of(componentes_sgp),
    names_to = "componente",
    values_to = "billones"
  ) %>%
  mutate(
    componente = factor(
      componente,
      levels = componentes_sgp,
      labels = names(colores_sgp)
    )
  )

grafico_composicion_sgp <- ggplot(
  sgp_grafico,
  aes(x = ano, y = billones, fill = componente)
) +
  geom_col(width = 0.78, position = position_stack(reverse = TRUE)) +
  geom_point(
    data = sgp_anual,
    aes(x = ano, y = total_reportado),
    inherit.aes = FALSE,
    colour = "#202020",
    size = 1.7
  ) +
  scale_fill_manual(values = colores_sgp) +
  scale_x_continuous(
    breaks = seq(2002, 2023, by = 3),
    limits = c(2001.5, 2023.5),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", big.mark = "."),
    expand = expansion(mult = c(0, 0.06))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución y composición del SGP municipal",
    x = NULL,
    y = "Billones de pesos de 2023",
    fill = NULL,
    caption = paste(
      "Fuente: DNP, Panel de Buen Gobierno del CEDE. Cálculos propios.",
      "Nota: Los puntos muestran el total reportado para los municipios con datos completos en cada año.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 17, face = "bold", colour = "#172B3E"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, colour = "#465667"),
    plot.margin = margin(14, 18, 12, 14)
  )

print(grafico_composicion_sgp)

# 5. Guardar la figura
ggsave(
  filename = file.path(figures, "composicion_sgp_municipal_2002_2023.png"),
  plot = grafico_composicion_sgp,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)


#####
############## PARTE 14. EVOLUCIÓN Y COMPOSICIÓN DEL SGP MUNICIPAL

library(tidyverse)

figures <- here::here("output", "figures")
dir.create(figures, recursive = TRUE, showWarnings = FALSE)

colores_sgp <- c(
  "Educación"                         = "#193C8C",
  "Salud"                             = "#2C659F",
  "Agua potable y saneamiento básico" = "#76A9D2",
  "Propósito general"                 = "#7E8E9F",
  "Asignaciones especiales"           = "#D4D9DF"
)

componentes_sgp <- c(
  "educacion", "salud", "agua", "proposito_general", "asignaciones_especiales"
)

# 1. Preparar los componentes en millones de pesos de 2023
sgp_municipal <- base %>%
  filter(between(ano, 2002, 2023), !is.na(codmpio),
         !is.na(sgp_total_real2023)) %>%
  transmute(
    codmpio,
    ano,
    total_reportado = sgp_total_real2023,
    educacion = sgp_educaciontot_real2023,
    salud = sgp_saludtot_real2023,
    agua = if_else(
      ano <= 2007, sgp_propgeneral_ap_real2023, sgp_agua_real2023
    ),
    proposito_general = if_else(
      ano <= 2007,
      sgp_propgeneraltot_real2023 - sgp_propgeneral_ap_real2023,
      sgp_propgeneraltot_real2023
    ),
    asignaciones_especiales = rowSums(
      pick(
        sgp_alimescolar_real2023,
        sgp_riberenos_real2023,
        sgp_resgindigena_real2023,
        sgp_fp_real2023,
        sgp_pinfancia_real2023
      )
    )
  )

# 2. Sumar los montos anuales para los municipios con datos completos
sgp_anual <- sgp_municipal %>%
  drop_na(all_of(componentes_sgp)) %>%
  group_by(ano) %>%
  summarise(
    municipios_grafico = n_distinct(codmpio),
    across(all_of(c(componentes_sgp, "total_reportado")), sum),
    .groups = "drop"
  )

# 3. Revisar la cobertura y la diferencia frente al total reportado
conciliacion_sgp <- sgp_municipal %>%
  group_by(ano) %>%
  summarise(municipios_con_total = n_distinct(codmpio), .groups = "drop") %>%
  complete(ano = 2002:2023, fill = list(municipios_con_total = 0L)) %>%
  left_join(sgp_anual, by = "ano") %>%
  mutate(
    municipios_grafico = coalesce(municipios_grafico, 0L),
    municipios_excluidos = municipios_con_total - municipios_grafico,
    suma_componentes = rowSums(pick(all_of(componentes_sgp))),
    diferencia_millones = total_reportado - suma_componentes,
    diferencia_pct = if_else(
      total_reportado > 0,
      100 * diferencia_millones / total_reportado,
      NA_real_
    )
  ) %>%
  select(
    ano, municipios_con_total, municipios_grafico, municipios_excluidos,
    total_reportado, suma_componentes, diferencia_millones, diferencia_pct
  )

print(conciliacion_sgp, n = Inf, width = Inf)

# 4. Graficar los componentes y el total reportado
sgp_grafico <- sgp_anual %>%
  pivot_longer(
    all_of(componentes_sgp),
    names_to = "componente",
    values_to = "millones"
  ) %>%
  mutate(
    componente = factor(
      componente,
      levels = componentes_sgp,
      labels = names(colores_sgp)
    )
  )

grafico_composicion_sgp <- ggplot(
  sgp_grafico,
  aes(x = ano, y = millones, fill = componente)
) +
  geom_col(width = 0.78, position = position_stack(reverse = TRUE)) +
  geom_point(
    data = sgp_anual,
    aes(x = ano, y = total_reportado),
    inherit.aes = FALSE,
    colour = "#202020",
    size = 1.7
  ) +
  scale_fill_manual(values = colores_sgp) +
  scale_x_continuous(
    breaks = seq(2002, 2023, by = 3),
    limits = c(2001.5, 2023.5),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1, decimal.mark = ",", big.mark = "."
    ),
    expand = expansion(mult = c(0, 0.06))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Evolución y composición del SGP municipal",
    x = NULL,
    y = "Millones de pesos de 2023",
    fill = NULL,
    caption = paste(
      "Fuente: DNP, Panel de Buen Gobierno del CEDE. Cálculos propios.",
      "Nota: Los puntos muestran el total reportado para los municipios con datos completos en cada año.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 17, face = "bold", colour = "#172B3E"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, colour = "#465667")
  )

print(grafico_composicion_sgp)

ggsave(
  filename = file.path(figures, "composicion_sgp_municipal_2002_2023.png"),
  plot = grafico_composicion_sgp,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)