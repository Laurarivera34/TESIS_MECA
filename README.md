# Proyecto de grado: Endeudamiento municipal en Colombia

## Descripción

Este repositorio contiene los datos, códigos y documentación asociados al proyecto de grado sobre el **endeudamiento de los gobiernos municipales en Colombia**.

El proyecto busca analizar empíricamente la relación entre las condiciones fiscales de los municipios y sus niveles de endeudamiento, utilizando información municipal estructurada como datos de panel y herramientas de econometría aplicada. La investigación se desarrolla bajo un enfoque de **ciencia abierta y reproducibilidad**, siguiendo buenas prácticas para la organización, documentación y reutilización de datos y código, en línea con los principios FAIR: **Findable, Accessible, Interoperable y Reusable**. El objetivo es que los principales resultados del proyecto puedan ser reproducidos a partir del código y los insumos documentados en este repositorio.

---

## Pregunta de investigación

**¿Qué factores fiscales están asociados con el nivel de endeudamiento de los municipios colombianos?**

La pregunta podrá ser ajustada a medida que avance la exploración de las fuentes de información y se determine la disponibilidad y calidad de las variables relevantes.

---

## Objetivos

### Objetivo general

Analizar los factores fiscales asociados con el nivel de endeudamiento de los municipios colombianos mediante el uso de datos de panel y herramientas de econometría aplicada.

### Objetivos específicos

1. Construir una base de datos panel a nivel municipal a partir de diferentes fuentes de información.
2. Identificar y caracterizar las principales variables fiscales relacionadas con el endeudamiento municipal.
3. Analizar la evolución y distribución del endeudamiento entre municipios y a través del tiempo.
4. Estimar modelos econométricos que permitan estudiar la relación entre las condiciones fiscales municipales y el endeudamiento.
5. Documentar de manera reproducible el proceso de construcción de la base, estimación de los modelos y generación de resultados.

---

## Estructura del proyecto

```text
tesis_endeudamiento_municipal/
│
├── data/
│   ├── raw/                    # Datos originales
│   └── processed/              # Datos procesados para el análisis
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
│
├── README.md                   # Descripción y guía del proyecto
└── environment.txt             # Entorno y paquetes utilizados
```

---

## Fuentes de datos

Las principales fuentes de información utilizadas en el proyecto serán documentadas en esta sección.

### CEDE — Panel de Buen Gobierno

Base de datos con información municipal utilizada como una de las principales fuentes para la construcción del panel.

La documentación de cada fuente incluirá:

* Nombre de la fuente.
* Institución responsable.
* Periodo disponible.
* Nivel de desagregación.
* Variables utilizadas.
* Fecha de descarga.
* Enlace o mecanismo de acceso.
* Restricciones de uso, cuando corresponda.

Cuando una fuente no permita redistribuir los datos originales, el repositorio contendrá los códigos y las instrucciones necesarias para obtenerlos directamente desde la fuente correspondiente.

---

## Flujo de trabajo

El análisis sigue el siguiente flujo:

```text
Fuentes originales
       │
       ▼
01_import_cleaning.R
       │
       ▼
Datos procesados
       │
       ▼
02_variables.R
       │
       ▼
Base analítica
       │
       ├───────────────┐
       ▼               ▼
03_descriptives.R   04_econometrics.R
       │               │
       ▼               ▼
   Figures           Tables
       │               │
       └───────┬───────┘
               ▼
           Resultados
```

---

## Código

El análisis está desarrollado principalmente en **R**.

Los scripts deben ejecutarse en orden:

### 1. Importación y limpieza

`code/01_import_cleaning.R`

Importa las fuentes de datos, realiza las transformaciones iniciales y genera las bases procesadas utilizadas posteriormente.

### 2. Construcción de variables

`code/02_variables.R`

Construye las variables utilizadas en el análisis, incluyendo las variables relacionadas con endeudamiento, ingresos, gasto, transferencias y demás características municipales.

### 3. Estadísticas descriptivas

`code/03_descriptives.R`

Genera las estadísticas descriptivas y visualizaciones utilizadas para caracterizar la muestra y explorar los principales patrones de los datos.

### 4. Econometría

`code/04_econometrics.R`

Contiene las especificaciones econométricas utilizadas para estudiar la relación entre las condiciones fiscales y el endeudamiento municipal.

La especificación definitiva de los modelos será documentada a medida que avance la investigación.

---

## Reproducibilidad

El proyecto busca que los resultados puedan ser reproducidos a partir de los datos disponibles y del código documentado.

Para facilitar la reproducibilidad:

* Se utilizan rutas relativas.
* Los datos originales se mantienen separados de los datos procesados.
* Las transformaciones de los datos están documentadas en el código.
* Las variables utilizadas en el análisis se construyen mediante scripts.
* Las tablas y figuras se generan mediante código.
* Se documentan las versiones de los paquetes y herramientas utilizadas.
* Se evita depender de rutas específicas del computador del investigador.

La ejecución de los scripts deberá realizarse siguiendo el orden establecido en la sección **Código**.

---

## Principios FAIR

El proyecto incorpora los principios FAIR en la medida permitida por las condiciones de acceso y uso de las fuentes de información.

### Findable — Encontrable

Los datos, códigos y documentos estarán organizados mediante una estructura clara de carpetas y archivos.

### Accessible — Accesible

Los códigos y materiales de investigación estarán disponibles públicamente cuando las condiciones de uso lo permitan.

### Interoperable — Interoperable

Se utilizarán formatos y estructuras de datos que faciliten su utilización con diferentes herramientas de análisis.

### Reusable — Reutilizable

El código, los metadatos y la documentación permitirán comprender cómo fueron construidos los resultados y facilitarán su reutilización en futuras investigaciones.

---

## Control de versiones

El proyecto utiliza **Git** para el control de versiones y **GitHub** como plataforma para gestionar el repositorio.

Los cambios relevantes en:

* código;
* datos procesados;
* documentación;
* resultados;

serán registrados mediante commits descriptivos.

---

## Resultados

Los resultados generados por el análisis se almacenarán en:

```text
output/
├── figures/
└── tables/
```

Las figuras y tablas se generarán directamente mediante los scripts de análisis para reducir la intervención manual y facilitar su reproducción.

---

## Limitaciones

La disponibilidad de información puede variar entre municipios y años. Por esta razón, antes de realizar las estimaciones econométricas se evaluará:

* cobertura temporal;
* cobertura municipal;
* porcentaje de valores faltantes;
* consistencia de las variables;
* existencia de cambios metodológicos en las fuentes;
* presencia de valores extremos;
* balance del panel.

Las decisiones relacionadas con la selección de la muestra y el tratamiento de los datos serán documentadas en el código y en los documentos metodológicos del proyecto.

---

## Estado del proyecto

**En desarrollo.**

Actualmente el proyecto se encuentra en la etapa de exploración y evaluación de las fuentes de información, definición de variables y construcción de la base de datos panel.

---

## Autora

**Laura Rivera**

Proyecto de grado
Maestría en Economía Aplicada
Universidad de los Andes
Colombia

---

## Licencia

La licencia del código, datos procesados y demás materiales será definida de acuerdo con las condiciones de uso de las fuentes de información y las políticas de la institución académica.
