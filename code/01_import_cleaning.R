##### IMPORTACIÓN Y DIAGNÓSTICO DE LAS BASES

#### PRIMER PARTE: IMPORTACIÓN DE LOS DATOS 

# 1. LIMPIAR ENTORNO
rm(list = ls())

# 2. PAQUETES

paquetes <- c(
  "tidyverse",
  "readxl",
  "haven",
  "janitor",
  "here",
  "skimr",
  "kableExtra",
  "modelsummary"
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


# 3. RUTAS DEL PROYECTO

root <- here::here() # Función here sirve para buscar la raíz de proyecto

data <- here::here("data", "raw")

processed <- here::here("data", "processed")

figures <- here::here("output", "figures")

tables <- here::here("output", "tables")

diagnostics <- here::here(
  "output",
  "tables",
  "diagnostics"
)


# 4. IMPORTACIÓN DE DATOS

panel_buen_gobierno <- read_excel(
  file.path(
    data,
    "PANEL_BUEN_GOBIERNO(2024).xlsx"
  )
)

caracteristicas_generales <- read_excel(
  file.path(
    data,
    "PANEL_CARACTERISTICAS_GENERALES(2024).xlsx"
  )
)

# Listado DANE: correspondencia entre municipios y departamentos
listado_departamentos <- read_excel(
  file.path(
    data,
    "Listado_Departamentos_DANE.xlsx"
  ),
  col_types = "text"
)

#### SEGUNDA PARTE: DIAGNÓSTICO DE LOS DATOS

# 1. ESTANDARIZACIÓN DE NOMBRES
panel_buen_gobierno <- panel_buen_gobierno %>%
  janitor::clean_names()

caracteristicas_generales <- caracteristicas_generales %>%
  janitor::clean_names()

listado_departamentos <- listado_departamentos %>%
  janitor::clean_names()

# Nombres modificados de acuerdo a las demás bases
listado_departamentos <- listado_departamentos %>%
  janitor::clean_names() %>%
  transmute(
    codmpio = as.integer(codigo_municipio),
    municipio = str_to_title(nombre_municipio, locale = "es"),
    coddepto = as.integer(codigo_departamento),
    departamento = nombre_departamento
  ) %>%
  distinct()

# Nombres de variables modificadas
names(panel_buen_gobierno)
names(caracteristicas_generales)

# 2. UNIÓN DE BASES

# Incorporar los datos del listado DANE
panel_buen_gobierno <- panel_buen_gobierno %>%
  mutate(codmpio = as.integer(codmpio)) %>%
  left_join(
    listado_departamentos %>%
      mutate(codmpio = as.integer(codmpio)),
    by = "codmpio",
    na_matches = "never"
  ) %>%
  relocate(codmpio, municipio, coddepto, departamento, ano)

# Revisar municipios que no encontraron correspondencia
panel_buen_gobierno %>%
  filter(is.na(departamento)) %>%
  distinct(codmpio)

# Completar la correspondencia del código histórico 23685
panel_buen_gobierno <- panel_buen_gobierno %>%
  mutate(
    municipio = if_else(
      codmpio == 23685, "San José de Uré", municipio
    ),
    coddepto = if_else(
      codmpio == 23685, 23L, coddepto
    ),
    departamento = if_else(
      codmpio == 23685, "Córdoba", departamento
    )
  )

# Confirmar que no quedan departamentos faltantes
panel_buen_gobierno %>%
  filter(is.na(departamento)) %>%
  distinct(codmpio)

# Variables con menos NA
na_buen_gobierno %>%
  arrange(porcentaje_na) %>%
  head(30)

na_caracteristicas %>%
  arrange(porcentaje_na) %>%
  head(30)

# 3. GUARDAR BASES CON NOMBRES ESTANDARIZADOS
write_csv(
  panel_buen_gobierno,
  file.path(
    processed,
    "panel_buen_gobierno_clean.csv"
  )
)

write_csv(
  caracteristicas_generales,
  file.path(
    processed,
    "caracteristicas_generales_clean.csv"
  )
)
