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

dane <- read_excel(
  file.path(
    data,
    "Listado_Departamentos_DANE.xlsx"
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

####Super financiera

# 1. FUNCIÓN PARA HOMOLOGAR NOMBRES

limpiar_municipio <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}


# 2. PREPARAR SUPERFINANCIERA

superfinanciera <- superfinanciera %>%
  rename(
    municipio = `Nombre del municipio`
  ) %>%
  mutate(
    municipio = limpiar_municipio(municipio),
    ano = as.integer(
      substr(as.character(`Fecha de Corte`), 1, 4)
    )
  ) %>%
  filter(
    !is.na(municipio),
    municipio != "",
    !str_detect(municipio, "\\btotal\\b")
  )


# 3. PREPARAR DANE

dane <- dane %>%
  rename(
    coddepto = `Código Departamento`,
    codmpio = `Código Municipio`,
    municipio = `Nombre Municipio`,
    departamento = `Nombre Departamento`
  ) %>%
  mutate(
    municipio = limpiar_municipio(municipio),
    codmpio = str_pad(as.character(codmpio), 5, pad = "0"),
    coddepto = str_pad(as.character(coddepto), 2, pad = "0")
  ) %>%
  filter(
    !is.na(municipio),
    municipio != "",
    !str_detect(municipio, "\\btotal\\b")
  )


# 4. CORREGIR NOMBRES DE SUPERFINANCIERA

superfinanciera <- superfinanciera %>%
  mutate(
    municipio = recode(
      municipio,
      
      "belen de los andaqui" = "belen de los andaquies",
      "bogota" = "bogota d c",
      "brice o" = "briceno",
      "ca asgordas" = "canasgordas",
      "carmen de viboral" = "el carmen de viboral",
      "cove as" = "covenas",
      "cubarral" = "san luis de cubarral",
      "don matias" = "donmatias",
      "dos quebradas" = "dosquebradas",
      "el canton de san pablo" = "el canton del san pablo",
      "el pe ol" = "el penol",
      "el pe on" = "el penon",
      "el pi on" = "el pinon",
      "guadalajara de buga buga" = "guadalajara de buga",
      "herrera" = "olaya herrera",
      "la monta ita" = "la montanita",
      "la uribe" = "uribe",
      "leguizamo puerto leguizamo" = "puerto leguizamo",
      "manaure balcon dl ce" = "manaure",
      "mariquita" = "san sebastian de mariquita",
      "mo itos" = "monitos",
      "nari o" = "narino",
      "oca a" = "ocana",
      "patia el bordo" = "patia",
      "piji o del carmen" = "pijino del carmen",
      "puerto carre o" = "puerto carreno",
      "puerto inirida" = "inirida",
      "puerto nare lamagdalena" = "puerto nare",
      "puerto nari o" = "puerto narino",
      "s antonio tequendama" = "san antonio del tequendama",
      "salda a" = "saldana",
      "san andres sotavento" = "san andres de sotavento",
      "san bernardo del vie" = "san bernardo del viento",
      "san carlos guaroa" = "san carlos de guaroa",
      "san jose de fragua" = "san jose del fragua",
      "san jose de la monta a" = "san jose de la montana",
      "san jose de risaralda" = "risaralda",
      "san sebastian buenav" = "san sebastian de buenavista",
      "san vicente del cagu" = "san vicente del caguan",
      "santa rosa de viterb" = "santa rosa de viterbo",
      "santander de quilich" = "santander de quilichao",
      "santiago de tolu tolu" = "santiago de tolu",
      "since" = "san luis de since",
      "toluviejo" = "tolu viejo",
      "tumaco" = "san andres de tumaco",
      "tutasa" = "tutaza",
      "ubate" = "villa de san diego de ubate",
      "ubate villa de sandiego de ubate" =
        "villa de san diego de ubate",
      "vista hermosa" = "vistahermosa",
      
      .default = municipio
    )
  )


# 5. PREPARAR CATÁLOGO DANE

dane_municipios <- dane %>%
  select(municipio, codmpio, coddepto, departamento) %>%
  distinct()


# 6. UNIR SUPERFINANCIERA CON DANE

superfinanciera_dane <- superfinanciera %>%
  left_join(
    dane_municipios,
    by = "municipio",
    relationship = "many-to-many"
  )


# 7. VER MUNICIPIOS QUE NO PEGARON

municipios_no_pegados <- superfinanciera_dane %>%
  filter(is.na(codmpio)) %>%
  distinct(municipio)

View(municipios_no_pegados)


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

# Unir superfinanciera con los Andes
base_municipal <- base_municipal %>%
  mutate(
    codmpio = str_pad(as.character(codmpio), 5, pad = "0"),
    coddepto = str_pad(as.character(coddepto), 2, pad = "0"),
    ano = as.integer(ano)
  )

base_municipal_final <- base_municipal %>%
  left_join(
    superfinanciera_dane,
    by = c(
      "codmpio",
      "municipio",
      "coddepto",
      "departamento",
      "ano"
    ),
    relationship = "one-to-one"
  )

#### QUINTA PARTE: DIAGNÓSTICO INICIAL DE BASE_MUNICIPAL
# 1. Nombres de las variables
names(base_municipal_final)

# 2. Años disponibles
sort(unique(base_municipal_final$ano))


### SEXTA PARTE: GUARDAR BASES PROCESADAS
saveRDS(
  base_municipal_final,
  file.path(processed, "base_municipal_final.rds")
)

saveRDS(
  ejecuciones_departamentos,
  file.path(processed, "ejecuciones_departamentos.rds")
)

saveRDS(
  ejecuciones_municipios,
  file.path(processed, "ejecuciones_municipios.rds")
)
