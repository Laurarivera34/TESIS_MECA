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

# Crear carpeta si no existe
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

# Exportar PNG
gt::gtsave(
  data = tabla_aer,
  filename = file.path(tables, "estadisticas.png"),
  zoom = 2,
  expand = 25
)

# Visualizar
tabla_aer


################ PARTE 2  DEPENDENCIA DEL SGP Y CRÉDITO

# 1. Clasificar municipios en cuartiles según su
# dependencia mediana del SGP, 2002–2023

grupos_sgp <- dependencia_municipal %>%
  filter(!is.na(dependencia_mediana)) %>%
  mutate(
    cuartil_sgp = ntile(dependencia_mediana, 4),
    grupo_sgp = factor(
      cuartil_sgp,
      levels = 1:4,
      labels = c(
        "Baja",
        "Media-baja",
        "Media-alta",
        "Alta"
      )
    )
  ) %>%
  select(codmpio, grupo_sgp)

# 2. Unir clasificación con el panel original
credito_sgp <- base %>%
  filter(between(ano, 2002, 2023)) %>%
  left_join(grupos_sgp, by = "codmpio") %>%
  filter(
    !is.na(grupo_sgp),
    !is.na(dummy_credito),
    dummy_credito %in% c(0, 1)
  )

# 3. Calcular utilización del crédito por grupo
resumen_credito_sgp <- credito_sgp %>%
  group_by(grupo_sgp) %>%
  summarise(
    municipios = n_distinct(codmpio),
    observaciones = n(),
    observaciones_con_credito = sum(dummy_credito == 1),
    porcentaje_credito = 100 * mean(dummy_credito),
    .groups = "drop"
  )

print(resumen_credito_sgp)

# 4. Elaborar gráfico
figura_3 <- ggplot(
  resumen_credito_sgp,
  aes(
    x = grupo_sgp,
    y = porcentaje_credito
  )
) +
  geom_col(
    fill = "#8B1E2D",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        round(porcentaje_credito, 1), "%"
      )
    ),
    vjust = -0.5,
    size = 4,
    family = "serif"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(resumen_credito_sgp$porcentaje_credito) * 1.15
    ),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = paste(
      "Figura 3. Utilización del crédito municipal",
      "según dependencia del SGP"
    ),
    subtitle = "Municipios de Colombia, 2002–2023",
    x = "Nivel de dependencia del SGP",
    y = "Municipios-año con desembolsos (%)",
    caption = paste0(
      "Nota: Los municipios se clasifican en cuartiles ",
      "según su dependencia mediana del SGP durante ",
      "2002–2023. La utilización corresponde al porcentaje ",
      "de observaciones municipio-año con desembolsos ",
      "positivos. Se excluyen registros sin información ",
      "de crédito.\n",
      "Fuente: CEDE y TerriData. Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      size = 13,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    ),
    axis.title.x = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

print(figura_3)

ggsave(
  filename = file.path(
    figures,
    "figura_3_credito_dependencia_sgp.png"
  ),
  plot = figura_3,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)



################ TERCERA PARTE: SGP Y ESFUERZO FISCAL


figura_3 <- ggplot(
  resumen_esfuerzo,
  aes(
    x = grupo_sgp,
    y = recaudo_mediano / 1000
  )
) +
  geom_col(
    fill = "#8B1E2D",
    width = 0.62
  ) +
  geom_text(
    aes(
      label = scales::number(
        recaudo_mediano / 1000,
        accuracy = 1,
        decimal.mark = ",",
        big.mark = "."
      )
    ),
    vjust = -0.5,
    size = 4.2,
    family = "serif"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ",",
      big.mark = "."
    ),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "Figura 3. Dependencia del SGP y recaudo tributario municipal",
    subtitle = "Colombia, 2002–2023. Medianas municipales",
    x = "Dependencia de las transferencias del SGP",
    y = "Recaudo tributario por habitante (miles de pesos)",
    caption = paste(
      "Nota: Los municipios se clasifican en cuartiles",
      "según su dependencia mediana del SGP.",
      "Las barras representan la mediana del recaudo",
      "tributario por habitante de cada grupo.",
      "Valores corrientes.\n",
      "Fuente: CEDE y TerriData. Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    ),
    plot.caption.position = "plot",
    plot.margin = margin(15, 20, 15, 20)
  )

print(figura_3)

ggsave(
  file.path(figures, "figura_3_esfuerzo_fiscal.png"),
  figura_3,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


################ CUARTA PARTE: SGP Y UTILIZACIÓN DEL CRÉDITO

# 1. Calcular dependencia histórica por municipio
dependencia_credito <- base %>%
  filter(
    between(ano, 2002, 2023),
    !is.na(codmpio)
  ) %>%
  group_by(codmpio) %>%
  summarise(
    dependencia_mediana = if (
      any(is.finite(dependencia_sgp))
    ) {
      median(dependencia_sgp[is.finite(dependencia_sgp)])
    } else NA_real_,
    .groups = "drop"
  ) %>%
  filter(is.finite(dependencia_mediana)) %>%
  mutate(
    grupo_sgp = factor(
      ntile(dependencia_mediana, 4),
      levels = 1:4,
      labels = c("Baja", "Media-baja", "Media-alta", "Alta")
    )
  )

# 2. Identificar observaciones con desembolsos
credito_panel <- base %>%
  filter(between(ano, 2002, 2023)) %>%
  inner_join(
    dependencia_credito %>%
      select(codmpio, grupo_sgp),
    by = "codmpio"
  ) %>%
  filter(
    !is.na(dummy_credito),
    dummy_credito %in% c(0, 1)
  )

# 3. Frecuencia de utilización del crédito
# Primero por municipio, luego por grupo
credito_municipal <- credito_panel %>%
  group_by(codmpio, grupo_sgp) %>%
  summarise(
    anos_observados = n_distinct(ano),
    anos_con_credito = sum(dummy_credito == 1),
    frecuencia_credito = 100 * mean(dummy_credito),
    .groups = "drop"
  )

resumen_credito <- credito_municipal %>%
  group_by(grupo_sgp) %>%
  summarise(
    municipios = n_distinct(codmpio),
    frecuencia_mediana = median(frecuencia_credito),
    frecuencia_media = mean(frecuencia_credito),
    .groups = "drop"
  )

print(resumen_credito, n = Inf)

# 4. Figura 3
figura_3 <- ggplot(
  resumen_credito,
  aes(x = grupo_sgp, y = frecuencia_media)
) +
  geom_col(
    fill = "#8B1E2D",
    width = 0.62
  ) +
  geom_text(
    aes(
      label = paste0(
        scales::number(
          frecuencia_media,
          accuracy = 0.1,
          decimal.mark = ","
        ),
        "%"
      )
    ),
    vjust = -0.5,
    size = 4.2,
    family = "serif"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Figura 3. Dependencia del SGP y utilización del crédito municipal",
    subtitle = "Colombia, 2002–2023",
    x = "Dependencia de las transferencias del SGP",
    y = "Años con desembolsos de crédito (%)",
    caption = paste0(
      "Nota: Los municipios se agrupan en cuartiles según ",
      "su dependencia mediana del SGP. Las barras representan ",
      "el promedio municipal del porcentaje de años con ",
      "desembolsos positivos, entre los años con información.\n",
      "Fuente: CEDE y TerriData. Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    plot.caption.position = "plot"
  )

print(figura_3)

ggsave(
  file.path(figures, "figura_3_sgp_utilizacion_credito.png"),
  figura_3,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


################ PARTE 5: SOSTENIBILIDAD DE LA DEUDA

# 1. Construir indicadores por municipio
datos_sostenibilidad <- base %>%
  filter(
    between(ano, 2002, 2023),
    !is.na(codmpio)
  ) %>%
  group_by(codmpio) %>%
  summarise(
    dependencia_mediana = if (
      any(is.finite(dependencia_sgp))
    ) {
      median(dependencia_sgp[is.finite(dependencia_sgp)])
    } else NA_real_,
    
    sostenibilidad_mediana = if (
      any(is.finite(sostenibilidad_proxy))
    ) {
      median(sostenibilidad_proxy[
        is.finite(sostenibilidad_proxy)
      ])
    } else NA_real_,
    
    anos_sostenibilidad = sum(
      is.finite(sostenibilidad_proxy)
    ),
    
    .groups = "drop"
  )

# 2. Clasificar según dependencia del SGP
# Los cuartiles se calculan antes de excluir
# municipios sin información de sostenibilidad.
datos_sostenibilidad <- datos_sostenibilidad %>%
  filter(is.finite(dependencia_mediana)) %>%
  mutate(
    grupo_sgp = factor(
      ntile(dependencia_mediana, 4),
      levels = 1:4,
      labels = c(
        "Baja",
        "Media-baja",
        "Media-alta",
        "Alta"
      )
    )
  )

# 3. Resumir por grupo
resumen_sostenibilidad <- datos_sostenibilidad %>%
  group_by(grupo_sgp) %>%
  summarise(
    municipios_total = n(),
    municipios_con_datos = sum(
      is.finite(sostenibilidad_mediana)
    ),
    mediana = if (
      any(is.finite(sostenibilidad_mediana))
    ) {
      median(sostenibilidad_mediana[
        is.finite(sostenibilidad_mediana)
      ])
    } else NA_real_,
    p25 = if (
      any(is.finite(sostenibilidad_mediana))
    ) {
      quantile(sostenibilidad_mediana[
        is.finite(sostenibilidad_mediana)
      ], 0.25)
    } else NA_real_,
    p75 = if (
      any(is.finite(sostenibilidad_mediana))
    ) {
      quantile(sostenibilidad_mediana[
        is.finite(sostenibilidad_mediana)
      ], 0.75)
    } else NA_real_,
    .groups = "drop"
  )

print(resumen_sostenibilidad, n = Inf)

# 4. Figura 5
figura_5 <- ggplot(
  resumen_sostenibilidad %>%
    filter(is.finite(mediana)),
  aes(
    x = grupo_sgp,
    y = mediana
  )
) +
  geom_col(
    fill = "#8B1E2D",
    width = 0.62
  ) +
  geom_text(
    aes(
      label = paste0(
        scales::number(
          mediana,
          accuracy = 0.1,
          decimal.mark = ","
        ),
        "%"
      )
    ),
    vjust = -0.5,
    size = 4.2,
    family = "serif"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(
      scales::number(
        x,
        accuracy = 1,
        decimal.mark = ","
      ),
      "%"
    ),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = paste(
      "Figura 5. Aproximación a la sostenibilidad",
      "de la deuda según dependencia del SGP"
    ),
    subtitle = "Colombia, 2002–2023. Medianas municipales",
    x = "Dependencia de las transferencias del SGP",
    y = "Indicador aproximado de deuda (%)",
    caption = paste0(
      "Nota: Las barras representan la mediana municipal ",
      "de sostenibilidad_proxy en cada cuartil de dependencia ",
      "del SGP. Este indicador es una aproximación exploratoria ",
      "y no corresponde al indicador oficial de la Ley 358.\n",
      "Fuente: CEDE y TerriData. Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      size = 13,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    ),
    plot.caption.position = "plot",
    plot.margin = margin(15, 20, 15, 20)
  )

print(figura_5)

# 5. Exportar
ggsave(
  filename = file.path(
    figures,
    "figura_5_sostenibilidad_sgp.png"
  ),
  plot = figura_5,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)



################ PARTE 6: HETEROGENEIDAD MUNICIPAL

# 1. Preparar categoría fiscal
# Verificar primero cómo está codificada
table(base$categ_cgn, useNA = "ifany")

# 2. Construir dependencia histórica del SGP
grupos_sgp_6 <- base %>%
  filter(
    between(ano, 2002, 2023),
    !is.na(codmpio)
  ) %>%
  group_by(codmpio) %>%
  summarise(
    dependencia_mediana = if (
      any(is.finite(dependencia_sgp))
    ) median(dependencia_sgp[is.finite(dependencia_sgp)])
    else NA_real_,
    .groups = "drop"
  ) %>%
  filter(is.finite(dependencia_mediana)) %>%
  mutate(
    grupo_sgp = factor(
      ntile(dependencia_mediana, 4),
      levels = 1:4,
      labels = c("Baja", "Media-baja", "Media-alta", "Alta")
    )
  ) %>%
  select(codmpio, grupo_sgp)

# 3. Construir panel de crédito y categoría
credito_categoria <- base %>%
  filter(
    between(ano, 2002, 2023),
    !is.na(codmpio),
    !is.na(dummy_credito),
    dummy_credito %in% c(0, 1)
  ) %>%
  inner_join(grupos_sgp_6, by = "codmpio") %>%
  mutate(
    categoria_texto = toupper(trimws(as.character(categ_cgn))),
    grupo_categoria = case_when(
      categoria_texto %in% c(
        "E", "ESPECIAL", "0", "1", "2", "3"
      ) ~ "Especial–3",
      categoria_texto %in% c(
        "4", "5", "6"
      ) ~ "4–6",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(grupo_categoria))

# 4. Calcular frecuencia por municipio y grupo fiscal
credito_categoria_municipal <- credito_categoria %>%
  group_by(codmpio, grupo_sgp, grupo_categoria) %>%
  summarise(
    anos_observados = n_distinct(ano),
    frecuencia_credito = 100 * mean(dummy_credito),
    .groups = "drop"
  )

# 5. Resumir por dependencia y categoría
resumen_categoria <- credito_categoria_municipal %>%
  group_by(grupo_sgp, grupo_categoria) %>%
  summarise(
    municipios = n_distinct(codmpio),
    frecuencia_media = mean(frecuencia_credito),
    frecuencia_mediana = median(frecuencia_credito),
    .groups = "drop"
  )

print(resumen_categoria, n = Inf)

# 6. Figura 6
figura_6 <- ggplot(
  resumen_categoria,
  aes(
    x = grupo_sgp,
    y = frecuencia_media,
    group = grupo_categoria,
    color = grupo_categoria
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Especial–3" = "#8B1E2D",
      "4–6" = "#555555"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = paste(
      "Figura 6. Utilización del crédito según",
      "dependencia del SGP y categoría fiscal"
    ),
    subtitle = "Colombia, 2002–2023",
    x = "Dependencia de las transferencias del SGP",
    y = "Años con desembolsos de crédito (%)",
    color = "Categoría municipal",
    caption = paste0(
      "Nota: Las líneas representan el promedio de la frecuencia ",
      "municipal de desembolsos positivos por grupo de categoría ",
      "fiscal y cuartil de dependencia del SGP. Las categorías ",
      "se consideran según su clasificación anual.\n",
      "Fuente: CEDE y TerriData. Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    plot.caption = element_text(size = 9, hjust = 0),
    plot.caption.position = "plot"
  )

print(figura_6)

# 7. Exportar
ggsave(
  file.path(figures, "figura_6_credito_categoria_617.png"),
  figura_6,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


########## PARTE 7. TOP 10 MUNICIPIOS - CRÉDITO 2000-2023
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


################ PARTE 8: MUNICIPIOS QUE UTILIZAN CRÉDITO

library(dplyr)
library(ggplot2)

# 1. Preparar información 2002–2023
datos_credito_8 <- base %>%
  filter(
    between(ano, 2002, 2023),
    !is.na(codmpio)
  ) %>%
  group_by(codmpio) %>%
  summarise(
    municipio = first(na.omit(municipio),
                      default = NA_character_),
    depto = first(na.omit(depto),
                  default = NA_character_),
    
    anos_con_datos = sum(
      !is.na(finan_credito_desemb)
    ),
    
    anos_con_credito = sum(
      finan_credito_desemb > 0,
      na.rm = TRUE
    ),
    
    desembolsos_totales = if (
      all(is.na(finan_credito_desemb))
    ) NA_real_
    else sum(
      finan_credito_desemb,
      na.rm = TRUE
    ),
    
    dependencia_sgp_mediana = if (
      any(is.finite(dependencia_sgp))
    ) median(
      dependencia_sgp[is.finite(dependencia_sgp)]
    )
    else NA_real_,
    
    .groups = "drop"
  ) %>%
  mutate(
    estado_credito = case_when(
      anos_con_credito > 0 ~ "Con desembolsos",
      anos_con_datos > 0 ~ "Sin desembolsos observados",
      TRUE ~ "Sin información"
    )
  )

# 2. ¿Cuántos municipios hay en cada grupo?
resumen_credito_8 <- datos_credito_8 %>%
  count(estado_credito, name = "municipios") %>%
  mutate(
    porcentaje = round(
      100 * municipios / sum(municipios), 1
    )
  )

print(resumen_credito_8, n = Inf)

# 3. ¿Cuáles municipios registraron desembolsos?
municipios_con_credito <- datos_credito_8 %>%
  filter(estado_credito == "Con desembolsos") %>%
  arrange(desc(desembolsos_totales))

View(municipios_con_credito)

# 4. Los 20 municipios con mayores desembolsos
top20_credito <- municipios_con_credito %>%
  slice_head(n = 20)

print(top20_credito, n = 20)

# 5. Gráfico: 20 municipios con mayores desembolsos
figura_8 <- top20_credito %>%
  mutate(
    etiqueta = paste0(municipio, " (", depto, ")")
  ) %>%
  ggplot(
    aes(
      x = reorder(etiqueta, desembolsos_totales),
      y = desembolsos_totales
    )
  ) +
  geom_col(
    fill = "#8B1E2D",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = paste(
      "Figura 8. Municipios con mayores",
      "desembolsos de crédito"
    ),
    subtitle = "Colombia, acumulado 2002–2023",
    x = NULL,
    y = "Desembolsos acumulados (millones de pesos corrientes)",
    caption = paste0(
      "Nota: Suma de desembolsos registrados en el período. ",
      "Incluye crédito interno y externo sin desagregar. ",
      "Los montos son nominales y no están ajustados por inflación.\n",
      "Fuente: CEDE (DNP). Cálculos propios."
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      size = 13,
      face = "bold"
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    )
  )

print(figura_8)

# 6. Guardar resultados
dir.create(figures, recursive = TRUE, showWarnings = FALSE)
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(figures, "figura_8_top20_credito.png"),
  figura_8,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

write.csv(
  datos_credito_8,
  file.path(tables, "municipios_credito_2002_2023.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
