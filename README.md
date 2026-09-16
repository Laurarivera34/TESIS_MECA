# Proyecto de grado: Endeudamiento municipal en Colombia

## Descripción

Este repositorio contiene los datos, códigos y documentación asociados al proyecto de grado sobre el **endeudamiento de los gobiernos municipales en Colombia**.

El proyecto busca analizar empíricamente la relación entre las condiciones fiscales de los municipios y sus niveles de endeudamiento, utilizando información municipal estructurada como datos de panel y herramientas de econometría aplicada. La investigación se desarrolla bajo un enfoque de ciencia abierta y reproducibilidad, siguiendo buenas prácticas para la organización, documentación y reutilización de datos y código, en línea con los principios FAIR: **Findable, Accessible, Interoperable y Reusable**. El objetivo es que los principales resultados del proyecto puedan ser reproducidos a partir del código y los insumos documentados en este repositorio.

---

## Pregunta de investigación

**¿Cómo afecta una mayor disponibilidad de transferencias del Sistema General de Participaciones al endeudamiento de los municipios colombianos? Evidencia 2000-2025?**

La pregunta podrá ser ajustada a medida que avance la exploración de las fuentes de información y se determine la disponibilidad y calidad de las variables relevantes.

---

## Objetivos

### Objetivo general
Estimar la relación entre las transferencias del Sistema General de Participaciones (SGP) y el endeudamiento de los municipios colombianos durante el periodo 2000–2025, e identificar su heterogeneidad según la clasificación fiscal municipal, mediante modelos de datos de panel con efectos fijos de municipio y año

### Objetivos específicos
1. Caracterizar la evolución temporal y la distribución entre municipios del endeudamiento, las transferencias del SGP y las principales variables fiscales municipales durante el
periodo 2000–2025.
2. Estimar la relación promedio entre las transferencias del SGP y el endeudamiento municipal, a partir de la variación observada dentro de cada municipio a lo largo del tiempo,
controlando por efectos fijos de municipio y año y por otras variables fiscales relevantes.
3. Evaluar la robustez de los resultados mediante especificaciones alternativas de las variables, rezagos de las transferencias, tratamiento de valores atípicos y cambios en la muestra
de municipios.

---
## Fuentes de datos
Las principales fuentes de información del proyecto corresponden a las bases de datos del CEDE — Panel de Buen Gobierno y Características Generales, las cuales consolidan información municipal proveniente de diversas fuentes oficiales. Entre las principales fuentes se encuentran el Departamento Administrativo Nacional de Estadística (DANE), el Instituto Geográfico Agustín Codazzi (IGAC), el Departamento Nacional de Planeación (DNP), el Ministerio de Agricultura y Desarrollo Rural, AGRONET, el Ministerio de Salud y Protección Social y el Ministerio de Minas y Energía, entre otras entidades. Estas bases permiten integrar información de diferentes dimensiones de los municipios colombianos y constituyen el principal insumo para la construcción del panel utilizado en el análisis.

---
## Código
El análisis está desarrollado principalmente en **R**. Los scripts deben ejecutarse en orden:

### 1. Importación y limpieza `code/01_import_cleaning.R`

Importa las fuentes de datos, realiza las transformaciones iniciales y genera las bases procesadas utilizadas posteriormente.

### 2. Construcción de variables`code/02_variables.R`

Construye las variables utilizadas en el análisis, incluyendo las variables relacionadas con endeudamiento, ingresos, gasto, transferencias y demás características municipales.

### 3. Estadísticas descriptivas `code/03_descriptives.R`

Genera las estadísticas descriptivas y visualizaciones utilizadas para caracterizar la muestra y explorar los principales patrones de los datos.

### 4. Econometría `code/04_econometrics.R`

Contiene las especificaciones econométricas utilizadas para estudiar la relación entre las condiciones fiscales y el endeudamiento municipal.

---
## Autora
Laura Sarif Rivera Sanabria

Proyecto de grado

Maestría en Economía Aplicada (MECA)

Universidad de los Andes

Colombia

## Estructura del proyecto

```text
tesis_endeudamiento_municipal/
│
├── data/                       # Datos originales
│
├── code/
│   ├── 01_import_cleaning.R    # Importación y limpieza de datos
│   ├── 02_variables.R          # Construcción de variables
│   ├── 03_descriptives.R       # Estadísticas descriptivas y exploración
│   └── 04_econometrics.R       # Estimaciones econométricas
│
├── output/
│   ├── figures/                # Gráficos y visualizaciones
│   └── tables/                 # Tablas y resultados
│     └── diagnostics/
│           ├── Table_A1_panel_structure.html
│           ├── Table_A2_NA_Buen_Gobierno.html
│           └── Table_A3_NA_Caracteristicas_Generales.html
│
├── docs/                       # Documentación del proyecto
│   ├── data_dictionary         # Diccionario de las variables
│   └── slides                  # Entrega de las diapositivas
│
├── README.md                   # Descripción y guía del proyecto
└── environment.txt             # Entorno y paquetes utilizados
