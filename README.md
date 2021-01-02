![Imagen del Titanic](https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Titanic_Portside_Diagram.jpg/1280px-Titanic_Portside_Diagram.jpg)
# Proyecto análisis de datos del Titanic

Este proyecto se realiza como una práctica para la materia "Tipología y ciclo de vida de los datos" del [Master en Ciencia de Datos](http://cv.uoc.edu/estudiant/mes-uoc/es/universitat/plans/MU11/index.html) de la UOC.
Se analiza datos de supervivencia de los pasajeros del transatlántico Titanic, hundido en 1912 tras chocar con un iceberg.


***

## Integrantes del proyecto:
* Juan Manuel Penalta Rodríguez
* Michaelle Estefanía Valenzuela Sangoquiza

***
## Estructura del proyecto:

### data
> Directorio que contiene los archivos con los datos originales y con los datos analizados.

### doc
> Directorio que contiene los documentos donde se describe el trabajo realizado sobre los datos y las conclusiones a las que se llega.

### src
> Directorio que contiene los scripts en lenguaje R que se utilizaron para realizar la limpieza y el análisis de los datos.

***
## Descrición de los archivos:

### data/titanic.csv
>Este archivo contiene los **datos originales** que se utilizaron para la práctica. Los datos fueron descargados de la página [https://www.kaggle.com/c/titanic/data](https://www.kaggle.com/c/titanic/data). El dataset dispone de 891 entradas, con 12 campos que se describen el documento "doc/M285.1 Práctica 2".

### data/titanic_
>Este es el fichero de **datos utilizado para el análisis**. Se obtuvo tras la realización de las labores de selección y limpieza de los datos del fichero original.

### doc/M285.1 Práctica.pdf
>Documento donde se describe el trabajo realizado sobre los datos y las conclusiones a las que se llega tras el análisis de los mismos.

### src/titanic-cleaning.R
>Script con el código R utilizado para realizar la **selección y limpieza de los datos** originales. Usando este script con los datos originales se obtiene el conjunto de datos sobre el que se realiza el análisis.

### src/titanic-analisis.R
>Script con el código R utilizado para el **análisis de los datos**. Este script utiliza el fichero de datos data/.... para realizar el análisis de los datos.

