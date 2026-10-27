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


################################### SEGUNDA PARTE: UNIÓN DE BASES CEDE

# 1. FUNCIÓN PARA ESTANDARIZAR NOMBRES DE MUNICIPIOS

normalizar_municipio <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}


# 2. ESTANDARIZAR NOMBRES EN CARACTERÍSTICAS GENERALES

caracteristicas_generales <- caracteristicas_generales %>%
  mutate(
    municipio = normalizar_municipio(municipio)
  )


# 3. PREPARAR NOMBRES OFICIALES DEL DANE

nombres_dane <- dane %>%
  transmute(
    codmpio = as.numeric(`Código Municipio`),
    municipio_dane = normalizar_municipio(
      `Nombre Municipio`
    )
  ) %>%
  distinct()

# Verificar que cada código DANE tenga un único nombre

duplicados_dane <- nombres_dane %>%
  count(codmpio) %>%
  filter(n > 1)

print(duplicados_dane, n = Inf)

if (nrow(duplicados_dane) > 0) {
  stop("Existen códigos DANE con más de un nombre.")
}


# 4. UNIR LAS DOS BASES CEDE

cede <- full_join(
  caracteristicas_generales,
  panel_buen_gobierno,
  by = c("codmpio", "ano")
)


# 5. UNIFICAR NOMBRES DE MUNICIPIOS CON EL DANE

cede <- cede %>%
  left_join(
    nombres_dane,
    by = "codmpio",
    relationship = "many-to-one"
  ) %>%
  group_by(codmpio) %>%
  mutate(
    municipio = coalesce(
      municipio_dane,
      first(
        municipio[!is.na(municipio)],
        default = NA_character_
      )
    )
  ) %>%
  ungroup() %>%
  select(-municipio_dane)


# 6. VERIFICAR DUPLICADOS POR MUNICIPIO Y AÑO

duplicados_cede <- cede %>%
  count(codmpio, ano) %>%
  filter(n > 1)

print(duplicados_cede, n = Inf)


# 7. VER TODOS LOS MUNICIPIOS DE CEDE

municipios_cede <- cede %>%
  select(codmpio, municipio) %>%
  distinct() %>%
  arrange(codmpio)

View(municipios_cede)


# 8. VERIFICAR MUNICIPIOS CON DIFERENTES NOMBRES

municipios_nombres_duplicados <- cede %>%
  filter(!is.na(codmpio)) %>%
  distinct(codmpio, municipio) %>%
  group_by(codmpio) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(codmpio, municipio)

print(municipios_nombres_duplicados, n = Inf)


# 9. VERIFICAR MUNICIPIOS SIN NOMBRE

municipios_sin_nombre <- cede %>%
  filter(is.na(municipio)) %>%
  distinct(codmpio, municipio) %>%
  arrange(codmpio)

print(municipios_sin_nombre, n = Inf)


# 10. REVISAR CÓDIGOS 23682 Y 23685

cede %>%
  filter(codmpio %in% c(23682, 23685)) %>%
  select(codmpio, municipio, ano) %>%
  arrange(codmpio, ano) %>%
  print(n = Inf)


# 11. RESUMEN DE LA BASE CEDE

resumen_cede <- cede %>%
  summarise(
    observaciones = n(),
    municipios = n_distinct(codmpio, na.rm = TRUE),
    anos = n_distinct(ano, na.rm = TRUE),
    municipios_sin_nombre = n_distinct(
      codmpio[is.na(municipio)],
      na.rm = TRUE
    )
  )

print(resumen_cede)

# 12. Excluir código 23685 (sin información fiscal relevante)

cede <- cede %>%
  filter(codmpio != 23685)

# 13. Verificar base final

cede %>%
  summarise(
    observaciones = n(),
    municipios = n_distinct(codmpio),
    anos = n_distinct(ano),
    municipios_sin_nombre = sum(is.na(municipio))
  ) %>%
  print()

# 14. Guardar base CEDE definitiva

saveRDS(
  cede,
  file.path(processed, "cede.rds")
)


################################### TERCERA PARTE: DATOS DANE

# 1. ESTANDARIZAR DANE

dane <- dane %>%
  rename(
    codmpio = `Código Municipio`,
    municipio = `Nombre Municipio`
  ) %>%
  mutate(
    codmpio = as.integer(codmpio),
    municipio = normalizar_municipio(municipio)
  ) %>%
  select(codmpio, municipio) %>%
  distinct()

# Ver municipios del DANE
View(dane)

# Duplicados por código DANE
dane %>%
  count(codmpio) %>%
  filter(n > 1) %>%
  print(n = Inf)

# Duplicados por nombre de municipio
dane %>%
  count(municipio) %>%
  filter(n > 1) %>%
  print(n = Inf)

# Duplicados exactos (código y municipio)
dane %>%
  count(codmpio, municipio) %>%
  filter(n > 1) %>%
  print(n = Inf)


############################################### CUARTA PARTE: DATOS SUPERFINANCIERA

# 1. FUNCIÓN PARA NORMALIZAR MUNICIPIOS

normalizar_municipio <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}


# 2. ESTANDARIZAR NOMBRES DE SUPERFINANCIERA

if ("Nombre del municipio" %in% names(superfinanciera)) {
  superfinanciera <- superfinanciera %>%
    rename(municipio = `Nombre del municipio`)
}

superfinanciera <- superfinanciera %>%
  mutate(
    municipio = normalizar_municipio(municipio)
  ) %>%
  filter(
    !is.na(municipio),
    !str_detect(municipio, "^total")
  )


# 3. CORREGIR NOMBRES Y EQUIVALENCIAS

superfinanciera <- superfinanciera %>%
  mutate(
    municipio = case_when(
      
      # Bogotá
      municipio == "bogota" ~ "bogota d c",
      
      # Caracteres dañados
      municipio == "salda a" ~ "saldana",
      municipio == "oca a" ~ "ocana",
      municipio == "nari o" ~ "narino",
      municipio == "san jose de la monta a" ~
        "san jose de la montana",
      municipio == "puerto carre o" ~ "puerto carreno",
      municipio == "mo itos" ~ "monitos",
      municipio == "el pi on" ~ "el pinon",
      municipio == "el pe ol" ~ "el penol",
      municipio == "el pe on" ~ "el penon",
      municipio == "puerto nari o" ~ "puerto narino",
      municipio == "piji o del carmen" ~ "pijino del carmen",
      
      # Equivalencias con DANE
      municipio == "guadalajara de buga buga" ~
        "guadalajara de buga",
      municipio == "toluviejo" ~ "tolu viejo",
      municipio == "mariquita" ~
        "san sebastian de mariquita",
      municipio == "tumaco" ~ "san andres de tumaco",
      municipio == "carmen de viboral" ~
        "el carmen de viboral",
      municipio == "santa rosa de viterb" ~
        "santa rosa de viterbo",
      municipio == "ubate" ~
        "villa de san diego de ubate",
      municipio == "dos quebradas" ~ "dosquebradas",
      municipio == "san vicente del cagu" ~
        "san vicente del caguan",
      municipio == "santiago de tolu tolu" ~
        "santiago de tolu",
      municipio == "vista hermosa" ~ "vistahermosa",
      municipio == "leguizamo puerto leguizamo" ~
        "puerto leguizamo",
      municipio == "san andres sotavento" ~
        "san andres de sotavento",
      municipio == "santander de quilich" ~
        "santander de quilichao",
      municipio == "s antonio tequendama" ~
        "san antonio del tequendama",
      municipio == "san carlos guaroa" ~
        "san carlos de guaroa",
      municipio == "san sebastian buenav" ~
        "san sebastian de buenavista",
      municipio == "puerto nare lamagdalena" ~
        "puerto nare",
      municipio == "belen de los andaqui" ~
        "belen de los andaquies",
      municipio == "san jose de fragua" ~
        "san jose del fragua",
      municipio == "patia el bordo" ~ "patia",
      municipio == "san bernardo del vie" ~
        "san bernardo del viento",
      municipio == "puerto inirida" ~ "inirida",
      municipio == "el canton de san pablo" ~
        "el canton del san pablo",
      municipio == "pijio del carmen" ~
        "pijino del carmen",
      municipio == "la uribe" ~ "uribe",
      municipio == "tutasa" ~ "tutaza",
      municipio == "since" ~ "san luis de since",
      
      # Equivalencias por departamento y código municipal
      
      municipio == "san pedro" &
        `Código del departamento` == 1 &
        `Código del municipio` == 664 ~
        "san pedro de los milagros",
      
      municipio == "cubarral" &
        `Código del departamento` == 16 &
        `Código del municipio` == 223 ~
        "san luis de cubarral",
      
      # Manaure, Cesar
      municipio %in% c(
        "manaure balcon dl ce",
        "manaure balcon del cesar"
      ) &
        `Código del departamento` == 9 &
        `Código del municipio` == 443 ~
        "manaure",
      
      municipio == "el carmen" &
        `Código del departamento` == 21 &
        `Código del municipio` == 235 ~
        "el carmen de chucuri",
      
      # Ciudad Bolívar, Antioquia
      municipio == "bolivar" &
        `Código del departamento` == 1 &
        `Código del municipio` == 101 ~
        "ciudad bolivar",
      
      # San Andrés de Cuerquia, Antioquia
      municipio == "san andres" &
        `Código del departamento` == 1 &
        `Código del municipio` == 647 ~
        "san andres de cuerquia",
      
      TRUE ~ municipio
    )
  )


# 4. EQUIVALENCIA DE DEPARTAMENTOS

equivalencia_departamentos <- tibble(
  departamento_super = 1:33,
  coddepto_dane = c(
    5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25,
    27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70,
    73, 76, 81, 85, 86, 88, 91, 94, 95, 97, 99
  )
)


# 5. PREPARAR DANE PARA EL CRUCE

normalizar_clave <- function(x) {
  normalizar_municipio(x) %>%
    str_replace_all("[^a-z0-9]", "")
}

dane_union <- dane %>%
  transmute(
    coddepto_dane = as.integer(codmpio %/% 1000),
    municipio_union = normalizar_clave(municipio),
    codmpio,
    municipio_dane = normalizar_municipio(municipio)
  ) %>%
  distinct()


# 6. VERIFICAR DUPLICADOS EN DANE

duplicados_dane <- dane_union %>%
  count(coddepto_dane, municipio_union) %>%
  filter(n > 1)

print(duplicados_dane, n = Inf)

if (nrow(duplicados_dane) > 0) {
  stop("Existen llaves duplicadas en DANE.")
}


# 7. UNIR SUPERFINANCIERA CON DANE

superfinanciera_dane <- superfinanciera %>%
  select(
    -any_of(c(
      "codmpio",
      "coddepto_dane",
      "municipio_super",
      "municipio_dane",
      "municipio_union"
    ))
  ) %>%
  mutate(
    municipio_super = municipio,
    municipio_union = normalizar_clave(municipio)
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


# 8. VERIFICAR QUE NO SE MULTIPLICARON OBSERVACIONES

cat(
  "Filas Superfinanciera:", nrow(superfinanciera),
  "\nFilas después del cruce:", nrow(superfinanciera_dane),
  "\n"
)

stopifnot(
  nrow(superfinanciera_dane) == nrow(superfinanciera)
)


# 9. MUNICIPIOS SIN CÓDIGO DANE

municipios_sin_codigo <- superfinanciera_dane %>%
  filter(is.na(codmpio)) %>%
  count(
    coddepto_dane,
    municipio_super,
    `Código del municipio`,
    name = "observaciones"
  ) %>%
  arrange(coddepto_dane, municipio_super)

print(municipios_sin_codigo, n = Inf)


# 10. REVISAR CÓDIGOS ORIGINALES SIN COINCIDENCIA

revision_codigos <- municipios_sin_codigo %>%
  mutate(
    codigo_dane_posible =
      coddepto_dane * 1000 + `Código del municipio`
  ) %>%
  left_join(
    dane %>%
      transmute(
        codigo_dane_posible = codmpio,
        municipio_dane = municipio
      ),
    by = "codigo_dane_posible",
    relationship = "many-to-one"
  ) %>%
  select(
    coddepto_dane,
    municipio_super,
    `Código del municipio`,
    codigo_dane_posible,
    municipio_dane,
    observaciones
  ) %>%
  arrange(coddepto_dane, municipio_super)

print(revision_codigos, n = Inf)


# 11. RESUMEN DEL CRUCE

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


# 12. VERIFICAR COINCIDENCIA DE NOMBRES CON DANE

verificacion_nombres <- superfinanciera_dane %>%
  filter(!is.na(codmpio)) %>%
  left_join(
    dane %>%
      transmute(
        codmpio,
        nombre_oficial = normalizar_municipio(municipio)
      ),
    by = "codmpio",
    relationship = "many-to-one"
  ) %>%
  filter(
    is.na(nombre_oficial) |
      municipio != nombre_oficial
  ) %>%
  distinct(codmpio, municipio, nombre_oficial)

print(verificacion_nombres, n = Inf)


############ QUINTA PARTE: BASE ANUAL DE SUPERFINANCIERA

superfinanciera_anual <- superfinanciera_dane %>%
  filter(!is.na(codmpio)) %>%
  transmute(
    codmpio,
    municipio = normalizar_municipio(municipio),
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
  group_by(codmpio, fecha) %>%
  summarise(
    municipio = first(municipio),
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = FALSE)
    ),
    .groups = "drop"
  ) %>%
  mutate(ano = lubridate::year(fecha)) %>%
  group_by(codmpio, ano) %>%
  mutate(
    trimestres_oficinas = sum(!is.na(oficinas)),
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

############################################## SEXTA PARTE: UNIÓN CEDE Y SUPERFINANCIERA

# 1. UNIR BASES POR MUNICIPIO Y AÑO

cede_superfinanciera <- cede %>%
  left_join(
    superfinanciera_anual %>%
      select(-municipio),
    by = c("codmpio", "ano"),
    relationship = "one-to-one"
  )


# 2. VERIFICAR QUE NO SE PERDIERON OBSERVACIONES

cat(
  "Observaciones CEDE:", nrow(cede),
  "\nObservaciones base final:", nrow(cede_superfinanciera),
  "\n"
)

stopifnot(
  nrow(cede_superfinanciera) == nrow(cede)
)


# 3. VERIFICAR DUPLICADOS MUNICIPIO-AÑO

cede_superfinanciera %>%
  count(codmpio, ano) %>%
  filter(n > 1) %>%
  print(n = Inf)


# 4. VERIFICAR COBERTURA DE SUPERFINANCIERA

cede_superfinanciera %>%
  group_by(ano) %>%
  summarise(
    municipios = n_distinct(codmpio),
    municipios_con_cartera = sum(!is.na(cartera)),
    municipios_con_oficinas = sum(!is.na(oficinas_promedio)),
    .groups = "drop"
  ) %>%
  print(n = Inf)


# 5. VISUALIZAR BASE FINAL

View(cede_superfinanciera)


# 6. GUARDAR BASE FINAL

saveRDS(
  cede_superfinanciera,
  file.path(processed, "cede_superfinanciera.rds")
)
