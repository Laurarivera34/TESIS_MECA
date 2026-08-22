# Proyecto de grado: Endeudamiento municipal en Colombia

## Descripción

Este repositorio contiene los datos, códigos y documentación asociados al proyecto de grado sobre el **endeudamiento de los gobiernos municipales en Colombia**.

El proyecto busca analizar empíricamente la relación entre las condiciones fiscales de los municipios y sus niveles de endeudamiento, utilizando información municipal estructurada como datos de panel y herramientas de econometría aplicada. La investigación se desarrolla bajo un enfoque de ciencia abierta y reproducibilidad, siguiendo buenas prácticas para la organización, documentación y reutilización de datos y código, en línea con los principios FAIR: **Findable, Accessible, Interoperable y Reusable**. El objetivo es que los principales resultados del proyecto puedan ser reproducidos a partir del código y los insumos documentados en este repositorio.

---

## Pregunta de investigación

**¿Qué factores fiscales están asociados con el nivel de endeudamiento de los municipios colombianos?**

La pregunta podrá ser ajustada a medida que avance la exploración de las fuentes de información y se determine la disponibilidad y calidad de las variables relevantes.

---

## Objetivos

### Objetivo general
Analizar los factores fiscales asociados con el nivel de endeudamiento de los municipios colombianos mediante el uso de datos de panel y herramientas de econometría aplicada.

### Objetivos específicos
1. Caracterizar la evolución del endeudamiento y desempeño fiscal de las entidades territoriales colombianas durante el periodo de estudio, identificando diferencias entre entidades con y sin calificación crediticia.
3. Analizar la evolución y distribución del endeudamiento entre municipios y a través del tiempo.
4. Estimar un modelo econométrico que permita identificar cuáles variables fiscales tienen mayor incidencia sobre la calidad crediticia territorial, controlando por características individuales y efectos temporales.
5. Documentar de manera reproducible el proceso de construcción de la base, estimación de los modelos y generación de resultados basado en los principios FAIR

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
│
├── docs/                       # Documentación del proyecto
│   ├── data_dictionary         # Diccionario de las variables
│   └── slides                  # Entrega de las diapositivas
│
├── README.md                   # Descripción y guía del proyecto
└── environment.txt             # Entorno y paquetes utilizados
