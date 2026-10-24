#  SELECCIÓN Y DIAGNÓSTICO UNIVARIADO DE VARIABLES

# 1. LIMPIAR ENTORNO
rm(list = ls())

# 2. PAQUETES
paquetes <- c(
  "tidyverse",
  "haven",
  "here",
  "skimr"
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
root <- here::here()

processed <- here::here(
  "data",
  "processed"
)

output <- here::here(
  "output"
)

tables <- here::here(
  "output",
  "tables"
)

figures <- here::here(
  "output",
  "figures"
)

# 4. IMPORTAR BASES LIMPIAS
base_municipal_final <- readRDS(
  file.path(processed, "base_municipal_final.rds")
)

base_completa <- readRDS(
  file.path(processed, "base_completa.rds")
)

# 5. VARIABLES DE INTERÉS

# Pasar todos los nombres a minúsculas
base_municipal_final <- base_municipal_final %>%
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
  
  # Territorio
  "minorias", "parques",
  
  # Superfinanciera
  "cuenta_corriente", "cdt", "ahorro", "cartera",
  "provision_consumo", "provision_microcreditos",
  "provision_comercial", "cuentas_centralizadas", "oficinas"
)

base_completa_final <- base_municipal_final %>%
  filter(between(ano, 2000, 2023)) %>%
  select(all_of(variables_conservar)) %>%
  arrange(codmpio, ano)





# diagnostico univariado,missing, valores externos


