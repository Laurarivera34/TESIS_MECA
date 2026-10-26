# ESTADÍSTICAS DESCRIPTIVAS Y ANÁLISIS EXPLORATORIO

# LIMPIAR ENTORNO
rm(list = ls())

# PAQUETES
paquetes <- c(
  "tidyverse",
  "haven",
  "here",
  "skimr",
  "plotly",      # Gráficos interactivos
  "osmdata",     # Obtener información de open street maps
  "sf",          # Leer/escribir/manipular datos espaciales
  "leaflet",     # Mapas interactivos
  "gridExtra",   # Graficar en grid
  "tmaptools",   # Geocode_OMS()
  "geosphere",   # Calcular distancias geográficas como Haversine
  "tidymodels",  # Carga recipes, parsnip, rsample, tune, y otros paquetes relacionados
  "spatialsample" # Muestreo espacial
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

# ESTADÍSTICAS DESCRIPTIVAS DE LAS VARIABLES PRINCIPALES

variables_tesis <- c(
  "finan_credito_desemb", "finan_credito_amort", "g_int_deudap",
  "sgp_total", "y_corr", "y_corr_tribut",
  "pobl_tot", "pib_total", "pib_percapita",
  "indrural", "discapital", "dismdo", "oficinas"
)

estadisticas_tesis <- base %>%
  select(all_of(variables_tesis)) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    disponibles = sum(!is.na(valor)),
    faltantes = sum(is.na(valor)),
    porcentaje_faltante = 100 * mean(is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desviacion = sd(valor, na.rm = TRUE),
    minimo = if (all(is.na(valor))) NA_real_ else min(valor, na.rm = TRUE),
    p01 = quantile(valor, 0.01, na.rm = TRUE, names = FALSE),
    p25 = quantile(valor, 0.25, na.rm = TRUE, names = FALSE),
    p75 = quantile(valor, 0.75, na.rm = TRUE, names = FALSE),
    p99 = quantile(valor, 0.99, na.rm = TRUE, names = FALSE),
    maximo = if (all(is.na(valor))) NA_real_ else max(valor, na.rm = TRUE),
    .groups = "drop"
  )

View(estadisticas_tesis)

library(dplyr)
library(leaflet)

# Seleccionar un año del panel
ano_mapa <- 2023

datos_mapa <- base %>%
  filter(
    ano == ano_mapa,
    is.finite(longitud),
    is.finite(latitud),
    between(longitud, -180, 180),
    between(latitud, -90, 90),
    is.finite(finan_credito_desemb),
    finan_credito_desemb >= 0
  )

# Escala de colores
paleta <- colorNumeric(
  palette = "YlOrRd",
  domain = datos_mapa$finan_credito_desemb
)

# Mapa interactivo
mapa_credito <- leaflet(datos_mapa) %>%
  addTiles() %>%
  setView(lng = -74, lat = 4.5, zoom = 5) %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    radius = ~ifelse(
      finan_credito_desemb == 0,
      2,
      3 + 2 * log10(1 + finan_credito_desemb)
    ),
    color = ~paleta(finan_credito_desemb),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", municipio, "</b>",
      "<br>Año: ", ano,
      "<br>Desembolsos: $",
      format(
        round(finan_credito_desemb, 2),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      " millones"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = paleta,
    values = ~finan_credito_desemb,
    title = paste0("Desembolsos ", ano_mapa, "<br>Millones de COP"),
    opacity = 0.8
  )

mapa_credito


# Seleccionar año y observaciones con información
credito_ano <- base %>%
  filter(ano == 2023, !is.na(finan_credito)) %>%
  select(codmpio, municipio, depto, ano, finan_credito)

# Top 10: valores más altos
top10_mayores <- credito_ano %>%
  slice_max(finan_credito, n = 10, with_ties = FALSE) %>%
  arrange(desc(finan_credito))

# Top 10: valores más bajos
top10_menores <- credito_ano %>%
  slice_min(finan_credito, n = 10, with_ties = FALSE) %>%
  arrange(finan_credito)

View(top10_mayores)
View(top10_menores)
