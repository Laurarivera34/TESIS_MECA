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

superfinanciera <- read_excel(
  file.path(
    data,
    "Saldo_de_las_captaciones_y_colocaciones_por_municipios_20261009.xlsx"
  )
)


#### SEGUNDA PARTE: REVISIÓN DE NOMBRES DE VARIABLES

# 1. Panel Buen Gobierno
names(panel_buen_gobierno)

# 2. Características Generales
names(caracteristicas_generales)

# 3. Ejecuciones municipales
names(ejecuciones_municipios)

# 4. Ejecuciones departamentales
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

# 4. SUPERFINANCIERA
superfinanciera <- superfinanciera %>%
  rename(
    coddepto = `Código del departamento`,
    codmpio = `Código del municipio`,
    municipio = `Nombre del municipio`
  ) %>%
  mutate(
    ano = as.integer(substr(as.character(`Fecha de Corte`), 1, 4))
  )



# Municipios únicos de la Super
municipios_super <- superfinanciera %>%
  distinct(coddepto, codmpio, municipio) %>%
  arrange(coddepto, codmpio)

View(municipios_super)


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


#### QUINTA PARTE: DIAGNÓSTICO INICIAL DE BASE_MUNICIPAL
# 1. Nombres de las variables
names(base_municipal)

# 2. Años disponibles
sort(unique(base_municipal$ano))


### SEXTA PARTE: GUARDAR BASES PROCESADAS
saveRDS(
  base_municipal,
  file.path(processed, "base_municipal.rds")
)

saveRDS(
  ejecuciones_departamentos,
  file.path(processed, "ejecuciones_departamentos.rds")
)

saveRDS(
  ejecuciones_municipios,
  file.path(processed, "ejecuciones_municipios.rds")
)
