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

terridata <- read_excel(
  file.path(
    data,
    "TerriData_Dim7_2024.xlsx"
  ),
  col_types = "text"
)


ejecuciones_municipios <- haven::read_dta(
  file.path(
    data,
    "Ejecuciones_Municipios_PANEL_1984_2023.dta"
  )
)

ejecuciones_departamentos <- haven::read_dta(
  file.path(
    data,
    "Ejecuciones_Departamentos_PANEL_1984_2023.dta"
  )
)

#### SEGUNDA PARTE: REVISIÓN DE NOMBRES DE VARIABLES

# 1. Panel Buen Gobierno
names(panel_buen_gobierno)

# 2. Características Generales
names(caracteristicas_generales)

# 3. TerriData
names(terridata)

# 4. Ejecuciones municipales
names(ejecuciones_municipios)

# 5. Ejecuciones departamentales
names(ejecuciones_departamentos)


#### TERCERA PARTE: ESTANDARIZACIÓN DE NOMBRES DE IDENTIFICADORES 

# 1. CARACTERÍSTICAS GENERALES
caracteristicas_generales <- caracteristicas_generales %>%
  rename(
    departamento = depto
  )


# 2. EJECUCIONES DEPARTAMENTALES
ejecuciones_departamentos <- ejecuciones_departamentos %>%
  rename(
    coddepto = cod_dpto,
    ano = year
  )


# 3. EJECUCIONES MUNICIPALES
ejecuciones_municipios <- ejecuciones_municipios %>%
  rename(
    coddepto = cod_dpto,
    codmpio = cod_mpios,
    ano = year
  )


# 4. TERRIDATA
terridata <- terridata %>%
  rename(
    coddepto = `Código Departamento`,
    departamento = Departamento,
    codmpio = `Código Entidad`,
    municipio = Entidad,
    ano = `Año`
  )


#### CUARTA PARTE: UNIÓN DE LAS BASES MUNICIPALES

# Se une las dos bases de los Andes
base_municipal <- full_join(
  caracteristicas_generales,
  panel_buen_gobierno,
  by = c("codmpio", "ano"),
  na_matches = "never",
  relationship = "one-to-one"
) %>%
  relocate(codmpio, coddepto, municipio, departamento, ano) %>%
  arrange(codmpio, ano)

View(base_municipal)

# Pasar de formato largo a ancho Terridata
terridata_ancha <- terridata %>%
  select(
    codmpio,
    coddepto,
    municipio,
    departamento,
    ano,
    Indicador,
    `Dato Numérico`
  ) %>%
  pivot_wider(
    id_cols = c(
      codmpio, coddepto, municipio, departamento, ano
    ),
    names_from = Indicador,
    values_from = `Dato Numérico`
  )


# Unión con Terridata
terridata_ancha <- terridata_ancha %>%
  mutate(
    codmpio = as.character(codmpio),
    ano = as.integer(ano)
  )

base_municipal <- base_municipal %>%
  mutate(
    codmpio = as.character(codmpio),
    ano = as.integer(ano)
  ) %>%
  full_join(
    terridata_ancha,
    by = c("codmpio", "ano"),
    suffix = c("", "_td"),
    na_matches = "never",
    relationship = "one-to-one"
  ) %>%
  mutate(
    coddepto = coalesce(as.character(coddepto), as.character(coddepto_td)),
    municipio = coalesce(municipio, municipio_td),
    departamento = coalesce(departamento, departamento_td)
  ) %>%
  select(-coddepto_td, -municipio_td, -departamento_td) %>%
  relocate(codmpio, coddepto, municipio, departamento, ano) %>%
  arrange(codmpio, ano)


#### QUINTA PARTE: DIAGNÓSTICO INICIAL DE BASE_MUNICIPAL
# 1. Nombres de las variables
names(base_municipal)

# 2. Años disponibles
sort(unique(base_municipal$ano))
