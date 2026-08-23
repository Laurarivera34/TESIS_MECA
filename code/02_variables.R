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
panel_buen_gobierno <- read_csv(
  file.path(
    processed,
    "panel_buen_gobierno_clean.csv"
  ),
  show_col_types = FALSE
)

caracteristicas_generales <- read_csv(
  file.path(
    processed,
    "caracteristicas_generales_clean.csv"
  ),
  show_col_types = FALSE
)

# 5. VERIFICAR QUE LAS BASES SE CARGARON CORRECTAMENTE

dim(panel_buen_gobierno)
dim(caracteristicas_generales)

names(panel_buen_gobierno)
names(caracteristicas_generales)

# 6. VARIABLES DE INTERÉS
# 6.1. Variables de Buen Gobierno
variables_buen_gobierno <- c(
  
  # Ingresos
  "y_total",
  "y_corr",
  "y_transf",
  "y_transf_nal",
  
  # Gastos
  "g_total",
  "g_corr",
  "g_func_general",
  "g_func_trans",
  "g_int_deudap",
  
  # Ingresos de capital
  "y_cap_regalias",
  "y_cap_transf",
  
  # Crédito
  "finan_credito",
  "finan_credito_desemb",
  "finan_credito_amort",
  
  # Desempeño fiscal
  "df_deuda",
  "df_desemp_fisc",
  
  # Desempeño fiscal 2021-2023
  "df2_categoria",
  "df2_endeudamientolp",
  "df2_endeudamientolp_c",
  "df2_ahorrocorriente",
  "df2_ahorrocorriente_c",
  "df2_balanceprimario",
  "df2_balanceprimario_c",
  "df2_resultados",
  "df2_resultados_c",
  "df2_califi_ceingreso",
  "df2_califi_ceinversion",
  "df2_calificaciongestio",
  "df2_rango",
  
  # Sistema General de Participaciones
  "sgp_total",
  "sgp_educaciontot",
  "sgp_propgeneral_ld"
)

# 6.2. Variables de Características Generales
variables_caracteristicas <- c(
  
  # Identificación
  "codmpio",
  "depto",
  "provincia",
  "municipio",
  "ano",
  
  # Región
  "gandina",
  "gcaribe",
  "gpacifica",
  "gorinoquia",
  "gamazonia",
  
  # Demografía
  "pobl_tot",
  "indrural",
  
  # Características territoriales
  "areaoficialkm2",
  "altura",
  "disbogota",
  
  # PIB
  "pib_cons",
  "pib_total",
  "pib_percapita",
  
  # Pobreza y condiciones sociales
  "pobreza",
  "nbi",
  "nbicabecera",
  "nbiresto",
  "ipm",
  "ipm_hacinam_p",
  
  # Características poblacionales específicas
  "sisben_pobl_1",
  "sisben_pobl_10",
  
  # Territorio
  "minorias",
  "parques"
)

# 7. VERIFICAR QUE TODAS LAS VARIABLES EXISTAN

faltantes_buen_gobierno <- setdiff(
  variables_buen_gobierno,
  names(panel_buen_gobierno)
)

faltantes_caracteristicas <- setdiff(
  variables_caracteristicas,
  names(caracteristicas_generales)
)

cat("Variables faltantes en Buen Gobierno:\n")
print(faltantes_buen_gobierno)

cat("\nVariables faltantes en Características Generales:\n")
print(faltantes_caracteristicas)


# 8. CREAR BASES ANALÍTICAS PRELIMINARES

# 8.1. Buen Gobierno
base_buen_gobierno <- panel_buen_gobierno %>%
  select(
    codmpio,
    ano,
    all_of(variables_buen_gobierno)
  )

# 8.2. Características Generales
base_caracteristicas <- caracteristicas_generales %>%
  select(
    all_of(variables_caracteristicas)
  )

# 9. VERIFICAR ESTRUCTURA DE LAS BASES ANALÍTICAS
dim(base_buen_gobierno)
dim(base_caracteristicas)

names(base_buen_gobierno)
names(base_caracteristicas)


# 10. DIAGNÓSTICO UNIVARIADO
diagnostico_univariado <- function(data) {
  
  tibble(
    
    variable = names(data),
    
    tipo = sapply(
      data,
      function(x) class(x)[1]
    ),
    
    observaciones = sapply(
      data,
      length
    ),
    
    missing = sapply(
      data,
      function(x) sum(is.na(x))
    ),
    
    porcentaje_missing = sapply(
      data,
      function(x) mean(is.na(x)) * 100
    ),
    
    valores_unicos = sapply(
      data,
      function(x) n_distinct(x, na.rm = TRUE)
    ),
    
    minimo = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          min(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ),
    
    p1 = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          as.numeric(quantile(x, 0.01, na.rm = TRUE))
        } else {
          NA_real_
        }
      }
    ),
    
    p25 = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          as.numeric(quantile(x, 0.25, na.rm = TRUE))
        } else {
          NA_real_
        }
      }
    ),
    
    media = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          mean(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ),
    
    mediana = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          median(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ),
    
    p75 = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          as.numeric(quantile(x, 0.75, na.rm = TRUE))
        } else {
          NA_real_
        }
      }
    ),
    
    p99 = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          as.numeric(quantile(x, 0.99, na.rm = TRUE))
        } else {
          NA_real_
        }
      }
    ),
    
    maximo = sapply(
      data,
      function(x) {
        if (is.numeric(x) && any(!is.na(x))) {
          max(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ),
    
    desviacion_estandar = sapply(
      data,
      function(x) {
        if (is.numeric(x) && sum(!is.na(x)) > 1) {
          sd(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    )
  )
}


# 11. APLICAR DIAGNÓSTICO
diagnostico_buen_gobierno <- diagnostico_univariado(
  base_buen_gobierno
)

diagnostico_caracteristicas <- diagnostico_univariado(
  base_caracteristicas
)

# 12. REVISAR DIAGNÓSTICO

View(diagnostico_buen_gobierno)
View(diagnostico_caracteristicas)


# 13. IDENTIFICAR VARIABLES CON ALTO MISSING

diagnostico_buen_gobierno %>%
  arrange(desc(porcentaje_missing)) %>%
  print(n = Inf)


diagnostico_caracteristicas %>%
  arrange(desc(porcentaje_missing)) %>%
  print(n = Inf)


# 14. IDENTIFICAR POSIBLES VALORES EXTREMOS

# 14.1. Buen Gobierno
diagnostico_buen_gobierno %>%
  filter(tipo %in% c("numeric", "integer")) %>%
  select(
    variable,
    minimo,
    p1,
    p25,
    mediana,
    media,
    p75,
    p99,
    maximo,
    desviacion_estandar
  ) %>%
  print(n = Inf)


# 14.2. Características Generales
diagnostico_caracteristicas %>%
  filter(tipo %in% c("numeric", "integer")) %>%
  select(
    variable,
    minimo,
    p1,
    p25,
    mediana,
    media,
    p75,
    p99,
    maximo,
    desviacion_estandar
  ) %>%
  print(n = Inf)


# 15. GUARDAR BASES ANALÍTICAS EN CSV
write_csv(
  base_buen_gobierno,
  file.path(
    processed,
    "base_buen_gobierno_variables.csv"
  ),
  na = ""
)

write_csv(
  base_caracteristicas,
  file.path(
    processed,
    "base_caracteristicas_variables.csv"
  ),
  na = ""
)


# 16. GUARDAR DIAGNÓSTICOS EN CSV
write_csv(
  diagnostico_buen_gobierno,
  file.path(
    tables,
    "diagnostico_buen_gobierno.csv"
  ),
  na = ""
)

write_csv(
  diagnostico_caracteristicas,
  file.path(
    tables,
    "diagnostico_caracteristicas.csv"
  ),
  na = ""
)
