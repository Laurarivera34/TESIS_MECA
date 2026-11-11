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
  "y_no_tribut", "y_transf_nal", "y_transf_otra","y_corr_tribut_predial", "y_corr_tribut_iyc",
  
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
  "df2_calificaciongestio", "df2_rango","df_ing_trans", "df_ing_propios", "df_ahorro","df2_nuevoidf",
  
  # Sistema General de Participaciones
  "sgp_total", "sgp_educaciontot", "sgp_saludtot",
  "sgp_propgeneraltot","sgp_propgeneral_ld", "sgp_propgeneral_li",
  
  # Inversión
  "inv_total","g_cap", "g_cap_fbkf", "df_gast_inv",
  
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

# Convertir las transferencias del SGP de pesos corrientes a millones de pesos corrientes
base_completa_final <- base_completa_final %>%
  mutate(
    across(
      c(
        sgp_total,
        sgp_educaciontot,
        sgp_saludtot,
        sgp_propgeneraltot,
        sgp_propgeneral_ld,
        sgp_propgeneral_li
      ),
      ~ .x / 1000000
    )
  )

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

# Identificar coordenadas que requieren revisión
coordenadas_revisar <- base_completa_final %>%
  filter(
    !is.na(longitud),
    !between(longitud, -82, -66)
  ) %>%
  distinct(codmpio, municipio, longitud, latitud)

print(coordenadas_revisar, n = Inf)

# Conservar coordenadas antes de corregir
base_completa_final <- base_completa_final %>%
  mutate(
    longitud_original = longitud,
    
    longitud = case_when(
      between(longitud, -82, -66) ~ longitud,
      between(longitud * 10, -82, -66) ~ longitud * 10,
      between(longitud * 100, -82, -66) ~ longitud * 100,
      between(longitud * 1000, -82, -66) ~ longitud * 1000,
      between(longitud * 10000, -82, -66) ~ longitud * 10000,
      TRUE ~ longitud
    )
  )

# Comprobar cuáles siguen fuera del rango
coordenadas_revisar <- base_completa_final %>%
  filter(
    !is.na(longitud),
    !between(longitud, -82, -66)
  ) %>%
  distinct(codmpio, municipio, longitud_original, longitud)

print(coordenadas_revisar, n = Inf)

# Comprobar municipios sin coordenadas
base_completa_final %>%
  filter(is.na(longitud) | is.na(latitud)) %>%
  distinct(codmpio, municipio)

############## TERCERA PARTE: PROXY DE SOSTENIBILIDAD

base_completa_final <- base_completa_final %>%
  arrange(codmpio, ano) %>%
  group_by(codmpio) %>%
  mutate(
    # Flujo neto anual
    flujo_neto = finan_credito_desemb -
      finan_credito_amort,
    
    # Acumulación desde 2000
    flujo_acumulado = if (all(!is.na(flujo_neto))) {
      cumsum(flujo_neto)
    } else {
      rep(NA_real_, n())
    },
    
    # Saldo inicial mínimo compatible con los flujos
    deuda_inicial_minima = if (all(!is.na(flujo_acumulado))) {
      max(0, -min(c(0, flujo_acumulado)))
    } else {
      NA_real_
    },
    
    # Deuda reconstruida
    deuda_proxy = deuda_inicial_minima +
      flujo_acumulado,
    
    # Ingresos corrientes del año anterior
    ingresos_corrientes_previos = if_else(
      ano - lag(ano) == 1,
      lag(y_corr),
      NA_real_
    ),
    
    # Aproximación a la sostenibilidad
    sostenibilidad_proxy = if_else(
      !is.na(deuda_proxy) &
        !is.na(ingresos_corrientes_previos) &
        ingresos_corrientes_previos > 0,
      100 * deuda_proxy /
        ingresos_corrientes_previos,
      NA_real_
    )
  ) %>%
  ungroup()


# Validación para Bogotá

proxy_bogota <- base_completa_final %>%
  filter(codmpio == 11001) %>%
  select(
    ano,
    municipio,
    flujo_neto,
    flujo_acumulado,
    deuda_inicial_minima,
    deuda_proxy,
    ingresos_corrientes_previos,
    sostenibilidad_proxy
  ) %>%
  arrange(ano)

print(proxy_bogota, n = Inf)


############## CUARTA PARTE: CATEGORÍAS LEY 617 DE 2000

# 1. Importar base del Banco de la República
banrep <- read_dta(
  file.path(data, "Ejecuciones_Municipios_PANEL_1984_2023.dta")
)

# IPC anual del Banco de la República: precios constantes de 2023
ipc_anual <- banrep %>%
  transmute(ano = as.integer(year), ipc = as.numeric(ipc_2018dic)) %>%
  filter(between(ano, 2000, 2023), !is.na(ipc)) %>%
  distinct(ano, ipc)

if (anyDuplicated(ipc_anual$ano) ||
    !all(2000:2023 %in% ipc_anual$ano) ||
    any(ipc_anual$ipc <= 0)) {
  stop("Revisar IPC Banrep: faltan años o existen valores distintos por año.")
}

ipc_anual <- ipc_anual %>%
  mutate(factor_2023 = ipc[ano == 2023] / ipc)

variables_monetarias <- c(
  "y_total", "y_corr", "y_transf", "y_corr_tribut", "y_no_tribut",
  "y_transf_nal", "y_transf_otra", "y_corr_tribut_predial", "y_corr_tribut_iyc",
  "g_total", "g_corr", "g_func", "g_func_general", "g_func_personal",
  "g_func_trans", "g_int_deudap", "y_cap", "y_cap_regalias", "y_cap_transf",
  "finan_credito", "finan_credito_desemb", "finan_credito_amort",
  "sgp_total", "sgp_educaciontot", "sgp_saludtot", "sgp_propgeneraltot",
  "sgp_propgeneral_ld", "sgp_propgeneral_li", "inv_total", "g_cap", "g_cap_fbkf",
  "pib_total", "finan_credito_desemb"
)

base_completa_final <- base_completa_final %>%
  left_join(ipc_anual, by = "ano", relationship = "many-to-one") %>%
  mutate(across(all_of(variables_monetarias),
                ~ .x * factor_2023, .names = "{.col}_real2023"))

# 2. Seleccionar categorías municipales
banrep_categoria <- banrep %>%
  rename(
    codmpio = cod_mpios,
    ano = year
  ) %>%
  select(codmpio, ano, categ_cgn) %>%
  mutate(
    codmpio = as.integer(codmpio),
    ano = as.integer(ano)
  ) %>%
  filter(between(ano, 2000, 2023))

# 3. Verificar inconsistencias por municipio-año
conflictos_categoria <- banrep_categoria %>%
  group_by(codmpio, ano) %>%
  summarise(
    categorias_distintas = n_distinct(
      categ_cgn, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(categorias_distintas > 1)

print(conflictos_categoria, n = Inf)

# 4. Consolidar una categoría por municipio-año
# Detener el proceso si existen categorías contradictorias
if (nrow(conflictos_categoria) > 0) {
  stop("Existen categorías contradictorias por municipio-año.")
}

banrep_categoria <- banrep_categoria %>%
  group_by(codmpio, ano) %>%
  summarise(
    categ_cgn = first(
      categ_cgn[!is.na(categ_cgn)],
      default = NA_real_
    ),
    .groups = "drop"
  )

# 5. Unir categorías con la base principal
# Evitar columnas duplicadas si se ejecuta nuevamente
base_completa_final <- base_completa_final %>%
  select(-any_of("categ_cgn")) %>%
  mutate(
    codmpio = as.integer(codmpio),
    ano = as.integer(ano)
  ) %>%
  left_join(
    banrep_categoria,
    by = c("codmpio", "ano"),
    relationship = "many-to-one"
  )

# 6. Verificar cobertura histórica
cobertura_categoria <- base_completa_final %>%
  group_by(ano) %>%
  summarise(
    municipios = n_distinct(codmpio),
    con_categoria = sum(!is.na(categ_cgn)),
    sin_categoria = sum(is.na(categ_cgn)),
    cobertura = round(
      100 * con_categoria / n(), 2
    ),
    .groups = "drop"
  )

print(cobertura_categoria, n = Inf)

# 7. Verificar valores de las categorías
base_completa_final %>%
  filter(between(ano, 2002, 2023)) %>%
  count(categ_cgn, sort = TRUE) %>%
  print(n = Inf)

# 8. Verificar duplicados municipio-año
duplicados_categoria <- base_completa_final %>%
  count(codmpio, ano) %>%
  filter(n > 1)

print(duplicados_categoria, n = Inf)

# 9. Identificar municipios sin categoría
municipios_sin_categoria <- base_completa_final %>%
  filter(
    between(ano, 2003, 2023),
    is.na(categ_cgn)
  ) %>%
  distinct(codmpio, municipio) %>%
  arrange(codmpio)

print(municipios_sin_categoria, n = Inf)



############## QUINTA PARTE: UTILIZACIÓN DEL CRÉDITO MUNICIPAL

base_completa_final <- base_completa_final %>%
  mutate(
    # 1. Utilización efectiva del crédito (dummy)
    dummy_credito = case_when(
      is.na(finan_credito_desemb) ~ NA_integer_,
      finan_credito_desemb > 0 ~ 1L,
      TRUE ~ 0L
    ),
    
    # 2. Crédito neto anual (millones de pesos)
    credito_neto = finan_credito_desemb -
      finan_credito_amort,
    
    # 3. Desembolsos como porcentaje de ingresos corrientes
    desembolsos_ingresos = if_else(
      !is.na(y_corr) & y_corr > 0,
      100 * finan_credito_desemb / y_corr,
      NA_real_
    ),
    
    # 4. Crédito neto como porcentaje de ingresos corrientes
    credito_neto_ingresos = if_else(
      !is.na(y_corr) & y_corr > 0,
      100 * credito_neto / y_corr,
      NA_real_
    ),
    
    # 5. Desembolsos per cápita (pesos corrientes)
    desembolsos_pc = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      finan_credito_desemb * 1000000 / pobl_tot,
      NA_real_
    ),
    desembolsos_pc_real2023 = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      finan_credito_desemb_real2023 * 1000000 / pobl_tot,
      NA_real_
    ),
    credito_neto_real2023 = finan_credito_desemb_real2023 -
      finan_credito_amort_real2023
  )


############## VALIDACIÓN DE VARIABLES DE CRÉDITO

resumen_credito <- base_completa_final %>%
  filter(between(ano, 2002, 2023)) %>%
  summarise(
    municipios = n_distinct(codmpio),
    observaciones = n(),
    
    con_credito = sum(dummy_credito == 1, na.rm = TRUE),
    sin_credito = sum(dummy_credito == 0, na.rm = TRUE),
    sin_informacion = sum(is.na(dummy_credito)),
    
    desembolsos_pc_disponibles =
      sum(!is.na(desembolsos_pc)),
    
    desembolsos_ingresos_disponibles =
      sum(!is.na(desembolsos_ingresos)),
    
    credito_neto_ingresos_disponibles =
      sum(!is.na(credito_neto_ingresos))
  )

print(resumen_credito)


############## SEXTA PARTE: ESFUERZO FISCAL Y DEPENDENCIA DEL SGP

base_completa_final <- base_completa_final %>%
  mutate(
    # 1. Transferencias del SGP per cápita
    sgp_pc = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      sgp_total * 1000000 / pobl_tot,
      NA_real_
    ),
    sgp_pc_real2023 = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      sgp_total_real2023 * 1000000 / pobl_tot,
      NA_real_
    ),
    
    # 2. Recaudo tributario propio per cápita
    recaudo_tributario_pc = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      y_corr_tribut * 1000000 / pobl_tot,
      NA_real_
    ),
    recaudo_tributario_pc_real2023 = if_else(
      !is.na(pobl_tot) & pobl_tot > 0,
      y_corr_tribut_real2023 * 1000000 / pobl_tot,
      NA_real_
    ),
    
    # 3. Dependencia del SGP respecto a ingresos totales
    dependencia_sgp = if_else(
      !is.na(y_total) & y_total > 0,
      100 * sgp_total / y_total,
      NA_real_
    ),
    
    # 4. Dependencia de transferencias totales
    dependencia_transferencias = if_else(
      !is.na(y_total) & y_total > 0,
      100 * y_transf / y_total,
      NA_real_
    ),
    
    # 5. Participación del recaudo tributario
    participacion_tributaria = if_else(
      !is.na(y_corr) & y_corr > 0,
      100 * y_corr_tribut / y_corr,
      NA_real_
    )
  )


# Validación variables
resumen_fiscal <- base_completa_final %>%
  filter(between(ano, 2002, 2023)) %>%
  summarise(
    municipios = n_distinct(codmpio),
    observaciones = n(),
    sgp_pc_disponibles = sum(!is.na(sgp_pc)),
    recaudo_pc_disponibles =
      sum(!is.na(recaudo_tributario_pc)),
    dependencia_sgp_disponibles =
      sum(!is.na(dependencia_sgp)),
    dependencia_transferencias_disponibles =
      sum(!is.na(dependencia_transferencias)),
    participacion_tributaria_disponibles =
      sum(!is.na(participacion_tributaria))
  )

print(resumen_fiscal, width = Inf)

# Guardar base final
saveRDS(
  base_completa_final,
  file.path(processed, "base.rds")
)