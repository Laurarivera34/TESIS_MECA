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





#### TERCERA PARTE: UNIÓN DATOS DE LA SUPERFINANCIERA

# Estandarizar nombres del Dane

dane <- dane %>%
  rename(
    codmpio = `Código Municipio`,
    municipio = `Nombre Municipio`
  ) %>%
  mutate(
    # Eliminar el cero inicial y convertir a número
    codmpio = as.numeric(codmpio),
    
    # Primera letra de cada palabra en mayúscula
    municipio = str_to_title(
      str_to_lower(
        str_squish(municipio)
      )
    )
  ) %>%
  select(codmpio, municipio) %>%
  distinct()


# Se ponen tíldes a los nombres para que quede como los del dane
superfinanciera <- superfinanciera %>%
  mutate(municipio = str_to_title(str_to_lower(str_squish(municipio))))


############ aca toca revisar 


# 3. UNIR CON DANE Y RECUPERAR NOMBRES OFICIALES
superfinanciera_dane <- superfinanciera %>%
  select(-any_of(c("Código del departamento", "Código del municipio"))) %>%
  mutate(municipio_limpio = limpiar_municipio(municipio)) %>%
  left_join(
    dane %>%
      mutate(municipio_limpio = limpiar_municipio(municipio)) %>%
      select(codmpio, municipio_limpio, municipio_dane = municipio),
    by = "municipio_limpio",
    relationship = "many-to-many"
  ) %>%
  mutate(
    municipio = coalesce(municipio_dane, municipio)
  ) %>%
  select(-municipio_limpio, -municipio_dane)

# 4. VER MUNICIPIOS SIN COINCIDENCIA
municipios_no_pegados <- superfinanciera_dane %>%
  filter(is.na(codmpio)) %>%
  distinct(municipio)

View(municipios_no_pegados)

# CORREGIR MUNICIPIOS QUE NO COINCIDEN CON DANE

superfinanciera_dane <- superfinanciera_dane %>%
  mutate(
    municipio = recode(
      municipio,
      "Guadalajara De Buga (Buga)" = "Guadalajara De Buga",
      "Toluviejo" = "Tolú Viejo",
      "Bogota" = "Bogotá",
      "Mariquita" = "San Sebastián De Mariquita",
      "Tumaco" = "San Andrés De Tumaco",
      "Salda#A" = "Saldaña",
      "Since" = "San Luis De Sincé",
      "Carmen De Viboral" = "El Carmen De Viboral",
      "Santa Rosa De Viterb" = "Santa Rosa De Viterbo",
      "Ubate" = "Villa De San Diego De Ubaté",
      "Dos Quebradas" = "Dosquebradas",
      "San Vicente Del Cagu" = "San Vicente Del Caguán",
      "Santiago De Tolu (Tolu)" = "Santiago De Tolú",
      "Vista Hermosa" = "Vistahermosa",
      "Leguizamo (Puerto Leguizamo)" = "Puerto Leguízamo",
      "Oca#A" = "Ocaña",
      "San Jose De Risaralda" = "Risaralda",
      "Don Matias" = "Donmatías",
      "Nari#O" = "Nariño",
      "San Jose De La Monta#A" = "San José De La Montaña",
      "San Andres Sotavento" = "San Andrés De Sotavento",
      "Santander De Quilich" = "Santander De Quilichao",
      "Puerto Carre#O" = "Puerto Carreño",
      "S.antonio Tequendama" = "San Antonio Del Tequendama",
      "San Carlos Guaroa" = "San Carlos De Guaroa",
      "San Sebastian Buenav" = "San Sebastián De Buenavista",
      "Cubarral" = "San Luis De Cubarral",
      "Puerto Nare(Lamagdalena" = "Puerto Nare",
      "Herrera" = "Olaya Herrera",
      "Belen De Los Andaqui" = "Belén De Los Andaquíes",
      "San Jose De Fragua" = "San José Del Fragua",
      "Mo#Itos" = "Moñitos",
      "Patia(El Bordo)" = "Patía",
      "San Bernardo Del Vie" = "San Bernardo Del Viento",
      "Puerto Inirida" = "Inírida",
      "Manaure Balcon Dl Ce" = "Manaure",
      "El Pi#On" = "El Piñón",
      "El Canton De San Pablo" = "El Cantón Del San Pablo",
      "El Pe#Ol" = "El Peñol",
      "El Pe#On" = "El Peñón",
      "Piji#O Del Carmen" = "Pijiño Del Carmen",
      "La Uribe" = "Uribe",
      "Tutasa" = "Tutazá",
      "Puerto Nari#O" = "Puerto Nariño",
      .default = municipio
    )
  )

# 4. VER MUNICIPIOS SIN COINCIDENCIA
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
