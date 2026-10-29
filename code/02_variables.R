#  SELECCIÓN Y DIAGNÓSTICO UNIVARIADO DE VARIABLES

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
cede <- readRDS(
  file.path(processed, "cede.rds")
)

cede_superfinanciera <- readRDS(
  file.path(processed, "cede_superfinanciera.rds")
)

geoespacial <- readr::read_csv(
  file.path(data, "divipolageo.csv"),
  show_col_types = FALSE
)


############## PRIMER PARTE: VARIABLES DE INTERÉS

# Pasar todos los nombres a minúsculas
base_completa_final <- cede_superfinanciera %>%
  rename_with(tolower)

variables_conservar <- c(
  # Identificación
  "municipio", "codmpio", "depto", "coddepto", "ano",
  
  # Ingresos
  "y_total", "y_corr", "y_transf", "y_corr_tribut",
  "y_no_tribut", "y_transf_nal", "y_transf_otra",
  
  # Gastos
  "g_total", "g_corr", "g_func", "g_func_general",
  "g_func_personal", "g_func_trans", "g_int_deudap",
  
  # Ingresos de capital
  "y_cap", "y_cap_regalias", "y_cap_transf",
  
  # Crédito
  "finan_credito", "finan_credito_desemb", "finan_credito_amort",
  
  # Desempeño fiscal
  "df_deuda", "df_desemp_fisc",
  "df2_categoria", "df2_endeudamientolp", "df2_endeudamientolp_c",
  "df2_ahorrocorriente", "df2_ahorrocorriente_c",
  "df2_balanceprimario", "df2_balanceprimario_c",
  "df2_resultados", "df2_resultados_c",
  "df2_califi_ceingreso", "df2_califi_ceinversion",
  "df2_calificaciongestio", "df2_rango",
  
  # Sistema General de Participaciones
  "sgp_total", "sgp_educaciontot", "sgp_saludtot",
  "sgp_propgeneraltot",
  
  # Inversión
  "inv_total",
  
  # Región
  "gandina", "gcaribe", "gpacifica", "gorinoquia", "gamazonia",
  
  # Demografía
  "pobl_tot", "indrural",
  
  # Características territoriales
  "areaoficialkm2", "altura", "disbogota", "discapital", "dismdo",
  
  # PIB
  "pib_cons", "pib_total", "pib_percapita",
  
  # Pobreza y condiciones sociales
  "pobreza", "nbi", "nbicabecera", "nbiresto",
  "ipm", "ipm_hacinam_p",
  
  # Características poblacionales
  "sisben_pobl_1", "sisben_pobl_10",
  
  # Superfinanciera
  "cuenta_corriente", "cdt", "ahorro", "cartera",
  "provision_consumo", "provision_microcreditos",
  "provision_comercial", "cuentas_centralizadas",
  "oficinas", "oficinas_promedio", "trimestres_oficinas"
)

base_completa_final <- base_completa_final %>%
  filter(between(ano, 2000, 2023)) %>%
  select(all_of(variables_conservar)) %>%
  arrange(codmpio, ano)


############## SEGUNDA PARTE BASE GEOESPACIAL
geoespacial <- geoespacial %>%
  rename(
    codmpio = `Código Municipio`,
    latitud = Latitud
  ) %>%
  mutate(
    codmpio = as.integer(codmpio),
    longitud = as.numeric(str_replace(as.character(longitud), ",", ".")),
    latitud = as.numeric(str_replace(as.character(latitud), ",", ".")),
    longitud = if_else(abs(longitud) > 180, longitud / 1e6, longitud)
  )

# Se une con mi base total
base_completa_final <- base_completa_final %>%
  mutate(codmpio = as.integer(codmpio)) %>%
  left_join(
    geoespacial %>% select(codmpio, longitud, latitud),
    by = "codmpio",
    relationship = "many-to-one",
    na_matches = "never"
  )

# Comprobar si quedo alguno sin coordenadas
base_completa_final %>%
  filter(is.na(longitud) | is.na(latitud)) %>%
  distinct(codmpio, municipio)

geoespacial %>%
  filter(codmpio %in% c(23685, 94663)) 

############## TERCERA PARTE: SE CREAN VARIABLES DE LOS INDICADORES DE SOSTENIBILIDAD
base_completa_final <- base_completa_final %>%
  mutate(
    servicio_deuda = g_int_deudap + finan_credito_amort,
    carga_servicio_deuda = if_else(
      y_corr > 0,
      100 * servicio_deuda / y_corr,
      NA_real_
    )
  )


############## TERCERA PARTE: CATEGORIAS LEY 617 DE 2000
base <- read_dta(
  "C:/1. MECA/Trabajo de grado/TESIS_MECA/data/raw/Ejecuciones_Municipios_PANEL_1984_2023.dta"
)


# Se guarda base procesada 
saveRDS(
  base_completa_final,
  file.path(processed, "base.rds")
)

