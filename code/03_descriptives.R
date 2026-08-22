library(readxl)
library(dplyr)
library(tidyr)

# Ruta del archivo
ruta <- "C:/1. MECA/Trabajo de grado/Datos/Datos CEDE/PANEL_BUEN_GOBIERNO(2024).xlsx"

# Cargar base
datos <- read_excel(ruta)

# Ver estructura
dim(datos)
names(datos)

variables_interes <- c(
  "BG1_y_total",
  "BG2_y_corr",
  "BG10_y_transf",
  "BG11_y_transf_nal",
  "BG13_g_total",
  "BG14_g_corr",
  "BG17_g_func_general",
  "BG18_g_func_trans",
  "BG19_g_int_deudap",
  "BG23_y_cap_regalias",
  "BG24_y_cap_transf",
  "BG32_finan_credito",
  "BG33_finan_credito_desemb",
  "BG34_finan_credito_amort",
  "BG37_DF_deuda",
  "BG45_DF2_categoria",
  "BG52_DF2_endeudamientolp",
  "BG53_DF2_endeudamientolp_c",
  "BG180_SGP_total",
  "BG181_SGP_educaciontot"
)

# Revisar cuáles existen realmente en la base
variables_interes[variables_interes %in% names(datos)]

# Revisar cuáles NO existen
variables_interes[!variables_interes %in% names(datos)]

# Número de municipios
n_distinct(datos$codmpio)

# Número de años
n_distinct(datos$ano)

# Años disponibles
sort(unique(datos$ano))

variables_interes <- c(
  "finan_credito",
  "finan_credito_desemb",
  "finan_credito_amort",
  "DF_deuda",
  "DF2_categoria",
  "DF2_endeudamientolp",
  "DF2_endeudamientolp_c",
  "y_total",
  "y_corr",
  "y_transf",
  "y_transf_nal",
  "g_total",
  "g_corr",
  "g_func_general",
  "g_func_trans",
  "g_int_deudap",
  "y_cap_regalias",
  "y_cap_transf",
  "SGP_total",
  "SGP_educaciontot"
)

diagnostico_na <- data.frame(
  variable = variables_interes,
  observaciones = sapply(datos[variables_interes], function(x) sum(!is.na(x))),
  cantidad_NA = sapply(datos[variables_interes], function(x) sum(is.na(x))),
  porcentaje_NA = sapply(datos[variables_interes], function(x) mean(is.na(x)) * 100)
)

diagnostico_na

diagnostico_na <- diagnostico_na %>%
  arrange(porcentaje_NA)

diagnostico_na

str(datos[variables_interes])

# Estadisticas variables numericas
# Variables numéricas
variables_numericas <- names(datos)[sapply(datos, is.numeric)]

# Tabla de estadísticas
tabla_estadisticas <- data.frame(
  variable = variables_numericas,
  
  observaciones = sapply(datos[variables_numericas], function(x) {
    sum(!is.na(x))
  }),
  
  cantidad_NA = sapply(datos[variables_numericas], function(x) {
    sum(is.na(x))
  }),
  
  porcentaje_NA = sapply(datos[variables_numericas], function(x) {
    mean(is.na(x)) * 100
  }),
  
  media = sapply(datos[variables_numericas], function(x) {
    mean(x, na.rm = TRUE)
  }),
  
  mediana = sapply(datos[variables_numericas], function(x) {
    median(x, na.rm = TRUE)
  }),
  
  minimo = sapply(datos[variables_numericas], function(x) {
    min(x, na.rm = TRUE)
  }),
  
  maximo = sapply(datos[variables_numericas], function(x) {
    max(x, na.rm = TRUE)
  })
)

# Ver tabla
tabla_estadisticas

# Guardar
library(openxlsx)

ruta <- "C:/1. MECA/Trabajo de grado/Datos/Tablas-graficos/Tabla_estadisticas_descriptivas.xlsx"

write.xlsx(tabla_estadisticas, ruta, sheetName = "Estadisticas",
           rowNames = FALSE, overwrite = TRUE)

setColWidths(loadWorkbook(ruta), "Estadisticas", cols = 1:ncol(tabla_estadisticas), widths = "auto")