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

#### SEGUNDA PARTE: DIAGNÓSTICO DE LOS DATOS

# 1. INSPECCIÓN GENERAL

# Número de filas y de variables
dim(panel_buen_gobierno)
dim(caracteristicas_generales)

# Nombres de variables
names(panel_buen_gobierno)
names(caracteristicas_generales)

# 2. ESTANDARIZACIÓN DE NOMBRES
panel_buen_gobierno <- panel_buen_gobierno %>%
  janitor::clean_names()

caracteristicas_generales <- caracteristicas_generales %>%
  janitor::clean_names()

# Nombres de variables modificadas
names(panel_buen_gobierno)
names(caracteristicas_generales)

# 3. INSPECCIÓN DE VARIABLES

# Variables relacionadas con municipio
names(panel_buen_gobierno)[
  grepl("mun|municip|dane|cod", names(panel_buen_gobierno),
        ignore.case = TRUE)
]

names(caracteristicas_generales)[
  grepl("mun|municip|dane|cod", names(caracteristicas_generales),
        ignore.case = TRUE)
]

# 4. IDENTIFICACIÓN DE LA ESTRUCTURA DEL PANEL

# Revisar los primeros registros de los identificadores
panel_buen_gobierno %>%
  select(codmpio, ano) %>%
  head(20)

caracteristicas_generales %>%
  select(codmpio, municipio, ano) %>%
  head(20)

# Cobertura temporal
panel_buen_gobierno %>%
  summarise(
    ano_minimo = min(ano, na.rm = TRUE),
    ano_maximo = max(ano, na.rm = TRUE),
    numero_anios = n_distinct(ano)
  )

caracteristicas_generales %>%
  summarise(
    ano_minimo = min(ano, na.rm = TRUE),
    ano_maximo = max(ano, na.rm = TRUE),
    numero_anios = n_distinct(ano)
  )

# Número de municipios
panel_buen_gobierno %>%
  summarise(
    municipios = n_distinct(codmpio)
  )

caracteristicas_generales %>%
  summarise(
    municipios = n_distinct(codmpio)
  )

# 5. VALIDACIÓN DE LA LLAVE MUNICIPIO-AÑO
duplicados_buen_gobierno <- panel_buen_gobierno %>%
  count(codmpio, ano) %>%
  filter(n > 1)

duplicados_caracteristicas <- caracteristicas_generales %>%
  count(codmpio, ano) %>%
  filter(n > 1)

# Ver resultados
duplicados_buen_gobierno
duplicados_caracteristicas

# 6. VALORES FALTANTES 
panel_buen_gobierno %>%
  summarise(
    NA_codmpio = sum(is.na(codmpio)),
    NA_ano = sum(is.na(ano))
  )

caracteristicas_generales %>%
  summarise(
    NA_codmpio = sum(is.na(codmpio)),
    NA_ano = sum(is.na(ano))
  )

# 7. DIAGNÓSTICO DE COBERTURA DEL PANEL

# Número de años disponibles por municipio
cobertura_buen_gobierno <- panel_buen_gobierno %>%
  group_by(codmpio) %>%
  summarise(
    numero_anos = n_distinct(ano),
    ano_minimo = min(ano),
    ano_maximo = max(ano),
    .groups = "drop"
  )

cobertura_caracteristicas <- caracteristicas_generales %>%
  group_by(codmpio) %>%
  summarise(
    numero_anos = n_distinct(ano),
    ano_minimo = min(ano),
    ano_maximo = max(ano),
    .groups = "drop"
  )


# Distribución del número de años por municipio
table(cobertura_buen_gobierno$numero_anos)
table(cobertura_caracteristicas$numero_anos)

# Resumen de cobertura
cobertura_buen_gobierno %>%
  summarise(
    municipios = n(),
    promedio_anos = mean(numero_anos),
    mediana_anos = median(numero_anos),
    minimo_anos = min(numero_anos),
    maximo_anos = max(numero_anos)
  )

cobertura_caracteristicas %>%
  summarise(
    municipios = n(),
    promedio_anos = mean(numero_anos),
    mediana_anos = median(numero_anos),
    minimo_anos = min(numero_anos),
    maximo_anos = max(numero_anos)
  )


# 8 MUNICIPIOS CON COBERTURA INCOMPLETA
municipios_incompletos_bog <- cobertura_buen_gobierno %>%
  filter(numero_anos < 40) %>%
  arrange(numero_anos, codmpio)

municipios_incompletos_bog

municipios_incompletos_car <- cobertura_caracteristicas %>%
  filter(numero_anos < 31) %>%
  arrange(numero_anos, codmpio)

municipios_incompletos_car

# 9 MUNICIPIOS PRESENTES EN UNA BASE Y NO EN LA OTRO
municipios_bog <- unique(panel_buen_gobierno$codmpio)
municipios_car <- unique(caracteristicas_generales$codmpio)

municipios_solo_bog <- setdiff(
  municipios_bog,
  municipios_car
)

municipios_solo_bog

municipios_solo_car <- setdiff(
  municipios_car,
  municipios_bog
)

municipios_solo_car

# 10 DIAGNÓSTICO DE VALORES FALTANTES
diagnostico_na <- function(data) {
  
  tibble(
    variable = names(data),
    observaciones = nrow(data),
    n_na = sapply(data, function(x) sum(is.na(x))),
    porcentaje_na = sapply(data, function(x) mean(is.na(x)) * 100),
    observaciones_validas = sapply(
      data,
      function(x) sum(!is.na(x))
    ),
    porcentaje_valido = sapply(
      data,
      function(x) mean(!is.na(x)) * 100
    )
  ) %>%
    arrange(desc(porcentaje_na))
}


# Diagnóstico para cada base
na_buen_gobierno <- diagnostico_na(
  panel_buen_gobierno
)

na_caracteristicas <- diagnostico_na(
  caracteristicas_generales
)

head(na_buen_gobierno, 30)
head(na_caracteristicas, 30)

# Variables con menos NA
na_buen_gobierno %>%
  arrange(porcentaje_na) %>%
  head(30)

na_caracteristicas %>%
  arrange(porcentaje_na) %>%
  head(30)

# 11. GUARDAR BASES CON NOMBRES ESTANDARIZADOS
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
#///////////////// GENERACIÓN DE TABLAS

# 12 TABLA DE ESTRUCTURA Y COBERTURA DEL PANEL
estructura_panel <- bind_rows(
  
  panel_buen_gobierno %>%
    summarise(
      Base = "Panel de Buen Gobierno",
      Municipios = n_distinct(codmpio),
      `Año inicial` = min(ano, na.rm = TRUE),
      `Año final` = max(ano, na.rm = TRUE),
      Años = n_distinct(ano),
      Observaciones = n(),
      `Observaciones esperadas` = 
        n_distinct(codmpio) * n_distinct(ano)
    ),
  
  caracteristicas_generales %>%
    summarise(
      Base = "Características Generales",
      Municipios = n_distinct(codmpio),
      `Año inicial` = min(ano, na.rm = TRUE),
      `Año final` = max(ano, na.rm = TRUE),
      Años = n_distinct(ano),
      Observaciones = n(),
      `Observaciones esperadas` = 
        n_distinct(codmpio) * n_distinct(ano)
    )
) %>%
  mutate(
    `Cobertura (%)` =
      Observaciones / `Observaciones esperadas` * 100
  )

# 12.1 TABLA AER — ESTRUCTURA DEL PANEL
tabla_estructura_aer <- estructura_panel %>%
  mutate(
    Municipios = format(
      Municipios,
      big.mark = ",",
      scientific = FALSE
    ),
    Observaciones = format(
      Observaciones,
      big.mark = ",",
      scientific = FALSE
    ),
    `Observaciones esperadas` = format(
      `Observaciones esperadas`,
      big.mark = ",",
      scientific = FALSE
    ),
    `Cobertura (%)` = sprintf(
      "%.2f",
      `Cobertura (%)`
    )
  )

tabla_estructura_aer %>%
  kable(
    format = "html",
    booktabs = TRUE,
    align = c("l", "r", "r", "r", "r", "r", "r", "r"),
    caption = "Table 1. Structure and Coverage of the Municipal Panel",
    escape = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  row_spec(
    0,
    bold = TRUE
  ) %>%
  save_kable(
    file.path(
      diagnostics,
      "Table_A1_panel_structure.html"
    )
  )


# 13 TABLA DE VALORES FALTANTES 
tabla_na_aer <- function(
    diagnostico,
    archivo,
    titulo
) {
  
  tabla <- diagnostico %>%
    mutate(
      observaciones = format(
        observaciones,
        big.mark = ",",
        scientific = FALSE
      ),
      n_na = format(
        n_na,
        big.mark = ",",
        scientific = FALSE
      ),
      porcentaje_na = sprintf(
        "%.2f",
        porcentaje_na
      ),
      porcentaje_valido = sprintf(
        "%.2f",
        porcentaje_valido
      )
    ) %>%
    select(
      Variable = variable,
      Observations = observaciones,
      Missing = n_na,
      `Missing (%)` = porcentaje_na,
      `Valid (%)` = porcentaje_valido
    )
  
  tabla %>%
    kable(
      format = "html",
      booktabs = TRUE,
      align = c("l", "r", "r", "r", "r"),
      caption = titulo,
      escape = TRUE
    ) %>%
    kable_styling(
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    row_spec(
      0,
      bold = TRUE
    ) %>%
    save_kable(
      archivo
    )
}

tabla_na_aer(
  diagnostico = na_buen_gobierno,
  archivo = file.path(
    diagnostics,
    "Table_A1_NA_Buen_Gobierno.html"
  ),
  titulo = "Table A1. Missing Observations: Panel de Buen Gobierno"
)

tabla_na_aer(
  diagnostico = na_caracteristicas,
  archivo = file.path(
    diagnostics,
    "Table_A2_NA_Caracteristicas_Generales.html"
  ),
  titulo = "Table A2. Missing Observations: Características Generales"
)