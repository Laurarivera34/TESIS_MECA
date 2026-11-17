# Proyecto de grado: ¿Qué efecto tiene el Sistema General de Participaciones sobre el indicador de sostenibilidad de la deuda, definido por la Ley 358 de 1997 y sobre la utilización del crédito en los municipios colombianos? Evidencia para el período 2002–2023.

## Descripción

El proyecto busca analizar empíricamente la relación entre las condiciones fiscales de los municipios y sus niveles de endeudamiento, utilizando información municipal estructurada como datos de panel y herramientas de econometría aplicada. La investigación se desarrolla bajo un enfoque de ciencia abierta y reproducibilidad, siguiendo buenas prácticas para la organización, documentación y reutilización de datos y código, en línea con los principios FAIR: **Findable, Accessible, Interoperable y Reusable**. El objetivo es que los principales resultados del proyecto puedan ser reproducidos a partir del código y los insumos documentados en este repositorio.

---

## Objetivos

### Objetivo general
Analizar la relación entre las transferencias del Sistema General de Participaciones, el indicador de sostenibilidad de la deuda y la utilización efectiva del crédito en los municipios colombianos durante 2002–2023, considerando el papel del esfuerzo fiscal propio, la composición y destinación de los ingresos y las condiciones institucionales y financieras de los municipios.

### Objetivos específicos
1.	Analizar la relación entre las transferencias del SGP y el indicador de sostenibilidad de la deuda municipal, distinguiendo los cambios asociados a la composición de los ingresos de aquellos relacionados con el comportamiento del endeudamiento.
2.	Evaluar la relación entre las transferencias del SGP y el recaudo tributario propio de los municipios, con el propósito de explorar posibles comportamientos asociados a la pereza fiscal y su vinculación con las decisiones de endeudamiento.
3.	Estimar la relación entre las transferencias del SGP y la utilización efectiva del crédito municipal, considerando las diferencias económicas, fiscales e institucionales entre municipios y explorando una estrategia de identificación que permita establecer efectos causales cuando se cumplan los supuestos correspondientes. 


---
## Fuentes de datos
La investigación utiliza información fiscal, económica, financiera y sociodemográfica de los municipios colombianos para construir un panel municipio-año correspondiente al período 2002–2023, identificado mediante el código DIVIPOLA del DANE. Las principales fuentes son los paneles del CEDE de la Universidad de los Andes, el Banco de la República, TerriData del DNP, la Superintendencia Financiera de Colombia y el DANE. La variable explicativa principal corresponde a las transferencias del Sistema General de Participaciones (SGP), medidas per cápita y en precios constantes, tanto en su valor total como en sus componentes de educación, salud y propósito general.

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

Correo: ls.riveras1@uniandes.edu.co


## Asesores

Mauricio Villamizar Villegas (asesor). 

Correo: mvillavi@banrep.gov.co

Gerson Javier Pérez-Valbuena (coasesor). 

Correo: gperezva@banrep.gov.co


## Estructura del proyecto

```text
tesis_endeudamiento_municipal/
│
├── data/                       # Datos 
│   ├── raw                     # Datos originales
│   ├── processed               # Datos procesados
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
│   └── slides                  # Entrega a profesores y asesores
│       └── Documentos.R        # Literatura, documentos técnicos
│
├── README.md                   # Descripción y guía del proyecto
