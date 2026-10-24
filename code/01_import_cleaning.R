#### PRIMER PARTE: IMPORTACIÓN DE LOS DATOS 

# Limpiar entorno
rm(list = ls())

# Paquetes
paquetes <- c(
  "tidyverse",
  "readxl",
  "haven",
  "janitor",
  "here",
  "skimr",
  "kableExtra",
  "modelsummary",
  "stringi"
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


# Rutas del proyecto

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


# Importación de datos

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


#### SEGUNDA PARTE: UNIÓN DE BASES CEDE

base_completa <- full_join(
  caracteristicas_generales,
  panel_buen_gobierno,
  by = c("codmpio", "ano")
)

# Verificar que no hayan duplicados
base_completa |>
  count(codmpio, ano) |>
  filter(n > 1)


# Se guarda la base procesada
saveRDS(
  base_completa,
  file.path(processed, "base_completa.rds")
)


#### TERCERA PARTE: UNIÓN DATOS DE LA SUPERFINANCIERA

# 1. ESTANDARIZAR DANE
# Ejecutar desde las bases recién importadas.

dane <- dane %>%
  rename(
    codmpio = `Código Municipio`,
    municipio = `Nombre Municipio`
  ) %>%
  mutate(
    codmpio = as.integer(codmpio),
    municipio = str_to_title(
      str_to_lower(str_squish(municipio), locale = "es"),
      locale = "es"
    )
  ) %>%
  select(codmpio, municipio) %>%
  distinct()


# 2. CORREGIR NOMBRES DE SUPERFINANCIERA

superfinanciera <- superfinanciera %>%
  rename(municipio = `Nombre del municipio`) %>%
  mutate(
    municipio = str_to_title(
      str_to_lower(str_squish(municipio), locale = "es"),
      locale = "es"
    ),
    
    municipio = case_when(
      
      # Bogotá
      municipio == "Bogota" ~ "Bogotá, D.c.",
      
      # Caracteres dañados
      municipio == "Salda#A" ~ "Saldaña",
      municipio == "Oca#A" ~ "Ocaña",
      municipio == "Nari#O" ~ "Nariño",
      municipio == "San Jose De La Monta#A" ~
        "San José De La Montaña",
      municipio == "Puerto Carre#O" ~ "Puerto Carreño",
      municipio == "Mo#Itos" ~ "Moñitos",
      municipio == "El Pi#On" ~ "El Piñón",
      municipio == "El Pe#Ol" ~ "El Peñol",
      municipio == "El Pe#On" ~ "El Peñón",
      municipio == "Puerto Nari#O" ~ "Puerto Nariño",
      municipio == "Piji#O Del Carmen" ~ "Pijiño Del Carmen",
      
      # Equivalencias con nombres del DANE
      municipio == "Guadalajara De Buga (Buga)" ~
        "Guadalajara De Buga",
      municipio == "Toluviejo" ~ "Tolú Viejo",
      municipio == "Mariquita" ~ "San Sebastián De Mariquita",
      municipio == "Tumaco" ~ "San Andrés De Tumaco",
      municipio == "Carmen De Viboral" ~ "El Carmen De Viboral",
      municipio == "Santa Rosa De Viterb" ~ "Santa Rosa De Viterbo",
      municipio == "Ubate" ~ "Villa De San Diego De Ubaté",
      municipio == "Dos Quebradas" ~ "Dosquebradas",
      municipio == "San Vicente Del Cagu" ~ "San Vicente Del Caguán",
      municipio == "Santiago De Tolu (Tolu)" ~ "Santiago De Tolú",
      municipio == "Vista Hermosa" ~ "Vistahermosa",
      municipio == "Leguizamo (Puerto Leguizamo)" ~ "Puerto Leguízamo",
      municipio == "San Andres Sotavento" ~ "San Andrés De Sotavento",
      municipio == "Santander De Quilich" ~ "Santander De Quilichao",
      municipio == "S.antonio Tequendama" ~ "San Antonio Del Tequendama",
      municipio == "San Carlos Guaroa" ~ "San Carlos De Guaroa",
      municipio == "San Sebastian Buenav" ~ "San Sebastián De Buenavista",
      municipio == "Puerto Nare(Lamagdalena" ~ "Puerto Nare",
      municipio == "Belen De Los Andaqui" ~ "Belén De Los Andaquíes",
      municipio == "San Jose De Fragua" ~ "San José Del Fragua",
      municipio == "Patia(El Bordo)" ~ "Patía",
      municipio == "San Bernardo Del Vie" ~ "San Bernardo Del Viento",
      municipio == "Puerto Inirida" ~ "Inírida",
      municipio == "El Canton De San Pablo" ~ "El Cantón Del San Pablo",
      municipio == "Pijio Del Carmen" ~ "Pijiño Del Carmen",
      municipio == "La Uribe" ~ "Uribe",
      municipio == "Tutasa" ~ "Tutazá",
      municipio == "Miriti-Parana" ~ "Mirití - Paraná",
      municipio == "Since" ~ "San Luis De Sincé",
      
      # Otras tildes
      municipio == "Medellin" ~ "Medellín",
      municipio == "San Jose" ~ "San José",
      municipio == "San Andres" ~ "San Andrés",
      municipio == "Saldana" ~ "Saldaña",
      municipio == "Tulua" ~ "Tuluá",
      municipio == "Moniquira" ~ "Moniquirá",
      municipio == "Tocancipa" ~ "Tocancipá",
      municipio == "Don Matias" ~ "Donmatías",
      municipio == "Jerico" ~ "Jericó",
      municipio == "Bolivar" ~ "Bolívar",
      municipio == "Cordoba" ~ "Córdoba",
      municipio == "Alban" ~ "Albán",
      municipio == "La Union" ~ "La Unión",
      
      # Equivalencias previamente usadas:
      # ahora se delimitan también por departamento.
      municipio == "San Pedro" &
        `Código del departamento` == 1 &
        `Código del municipio` == 664 ~ "San Pedro De Los Milagros",
      
      municipio == "Cubarral" &
        `Código del departamento` == 16 &
        `Código del municipio` == 223 ~ "San Luis De Cubarral",
      
      municipio == "Manaure Balcon Dl Ce" &
        `Código del departamento` == 9 &
        `Código del municipio` == 443 ~ "Manaure",
      
      municipio == "El Carmen" &
        `Código del departamento` == 21 &
        `Código del municipio` == 235 ~ "El Carmen De Chucurí",
      
      TRUE ~ municipio
    )
  ) %>%
  filter(
    !str_detect(
      municipio,
      regex("^Total", ignore_case = TRUE)
    )
  )


# 3. FUNCIÓN AUXILIAR PARA EL CRUCE
# Crea una llave sin tildes ni puntuación.
# Los nombres oficiales se conservan en otra columna.

normalizar_municipio <- function(x) {
  x %>%
    str_squish() %>%
    str_to_lower(locale = "es") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}


# 4. EQUIVALENCIA DE DEPARTAMENTOS
# Correspondencia identificada en la tabla de referencia.

equivalencia_departamentos <- tibble(
  departamento_super = 1:33,
  coddepto_dane = c(
    5,  8, 11, 13, 15, 17, 18, 19, 20, 23, 25,
    27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70,
    73, 76, 81, 85, 86, 88, 91, 94, 95, 97, 99
  )
)


# 5. PREPARAR DANE PARA EL CRUCE

dane_union <- dane %>%
  transmute(
    coddepto_dane = as.integer(codmpio %/% 1000),
    municipio_union = normalizar_municipio(municipio),
    codmpio,
    municipio_dane = municipio
  ) %>%
  distinct()

# Verificar que departamento + nombre sea una llave única.
# Si hay ambigüedades, detener el proceso para revisarlas.

duplicados_dane <- dane_union %>%
  count(coddepto_dane, municipio_union) %>%
  filter(n > 1)

if (nrow(duplicados_dane) > 0) {
  print(duplicados_dane, n = Inf)
  stop("Hay nombres ambiguos dentro de un departamento en DANE.")
}


# 6. PEGAR DANE A SUPERFINANCIERA
# El cruce utiliza departamento + nombre.
# No exige coincidencia entre códigos municipales de ambas fuentes.

superfinanciera_dane <- superfinanciera %>%
  select(
    -any_of(c(
      "codmpio",
      "coddepto_dane",
      "municipio_super",
      "municipio_dane",
      "municipio_union",
      "codigo_aux"
    ))
  ) %>%
  mutate(
    municipio_super = municipio,
    municipio_union = normalizar_municipio(municipio)
  ) %>%
  left_join(
    equivalencia_departamentos,
    by = c(
      "Código del departamento" = "departamento_super"
    ),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  left_join(
    dane_union,
    by = c("coddepto_dane", "municipio_union"),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    municipio = coalesce(municipio_dane, municipio_super)
  ) %>%
  select(-municipio_union, -municipio_dane)


# 7. VERIFICAR QUE NO SE MULTIPLICARON OBSERVACIONES

cat(
  "Filas Superfinanciera:",
  nrow(superfinanciera),
  "\n"
)

cat(
  "Filas después del cruce:",
  nrow(superfinanciera_dane),
  "\n"
)

stopifnot(
  nrow(superfinanciera_dane) == nrow(superfinanciera)
)


# 8. REVISAR MUNICIPIOS SIN CÓDIGO DANE
# Se conservan en la base; no se eliminan.

municipios_sin_codigo <- superfinanciera_dane %>%
  filter(is.na(codmpio)) %>%
  count(
    `Código del departamento`,
    coddepto_dane,
    municipio_super,
    `Código del municipio`,
    name = "observaciones"
  ) %>%
  arrange(
    `Código del departamento`,
    municipio_super
  )

print(municipios_sin_codigo, n = Inf)


# 9. RESUMEN DEL CRUCE

resumen_cruce <- superfinanciera_dane %>%
  summarise(
    total_observaciones = n(),
    observaciones_con_codigo = sum(!is.na(codmpio)),
    observaciones_sin_codigo = sum(is.na(codmpio)),
    porcentaje_con_codigo = round(
      100 * mean(!is.na(codmpio)), 2
    )
  )

print(resumen_cruce)

#### CUARTA PARTE: BASE ANUAL DE SUPERFINANCIERA

superfinanciera_anual <- superfinanciera_dane %>%
  filter(!is.na(codmpio)) %>%
  transmute(
    codmpio,
    municipio,
    fecha = as.Date(`Fecha de Corte`),
    cuenta_corriente = `Depósitos en cuenta corriente bancaria`,
    cdt = `Certificados de depósito a término`,
    ahorro = `Depósitos de ahorro`,
    cartera = `Cartera de créditos`,
    provision_consumo = `Provisión créditos y operaciones de leasing de consumo`,
    provision_microcreditos = `Provisión microcréditos`,
    provision_comercial = `Provisión créditos y operaciones de leasing comerciales`,
    cuentas_centralizadas = `Número de cuentas centralizadas`,
    oficinas = `Número de oficinas`
  ) %>%
  # Total municipal por fecha; conservar NA si falta algún componente
  group_by(codmpio, municipio, fecha) %>%
  summarise(
    across(everything(), ~ sum(.x, na.rm = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(ano = lubridate::year(fecha)) %>%
  group_by(codmpio, municipio, ano) %>%
  mutate(
    oficinas_promedio = if (all(is.na(oficinas))) {
      NA_real_
    } else {
      mean(oficinas, na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  filter(format(fecha, "%m-%d") == "12-31") %>%
  select(codmpio, municipio, ano, everything(), -fecha) %>%
  arrange(codmpio, ano)

### QUINTA PARTE

# Unir CEDE con Superfinanciera, conservando todas las filas de CEDE

base_municipal_final <- base_completa %>%
  left_join(
    superfinanciera_anual %>% select(-municipio),
    by = c("codmpio", "ano"),
    relationship = "one-to-one",
    na_matches = "never"
  )

# Nombres de las variables
names(base_municipal_final)

# Años disponibles
sort(unique(base_municipal_final$ano))

# Verificar duplicados por municipio y año
base_municipal_final %>%
  count(codmpio, ano) %>%
  filter(n > 1) %>%
  print(n = Inf)


# Se guarda base procesada 
saveRDS(
  base_municipal_final,
  file.path(processed, "base_municipal_final.rds")
)
