# PR�CTICA 2 - TIPOLOG�A Y CICLO DE VIDA DE LOS DATOS 

# INTEGRANTES:
# JUAN MANUEL PENALTA
# MICHAELLE VALENZUELA

# =====================CARGA DE LIBRER�AS ==============================
library(lubridate)
library(dplyr)
library(reshape2)

# ==================== LECTURA DEL CONJUNTO DE DATOS ========================

titanic_data <- read.csv("UOC/M2.851 - Tipologia y ciclo de vida de los datos aula 3/Practica 2/titanic.csv", stringsAsFactors = FALSE)
filas=dim(titanic_data)[1]
head(titanic_data)

# Tipo de dato asignado a cada campo
sapply(titanic_data, function(x) class(x))

# ==================== SELECCI�N DE DATOS ===================================

titanic_data <- titanic_data[, -(1:1)]

# Estructura del conjunto de datos
str(titanic_data)

# ==================== LIMPIEZA DE DATOS ====================================

# Estad�sticas de valores vac�os
colSums(is.na(titanic_data))

colSums(titanic_data=="")

# Se convierte el formato de la variable age de num�rica a entero
titanic_data$age <- as.integer(as.numeric(titanic_data$age))

# Se toma el valor "Desconocido" para los valores vac�os de la variable "country"
titanic_data<- titanic_data %>% 
               mutate(country=ifelse(is.na(country),"Desconocido",country))

# Se toma la media para valores vac�os de la variable "Age"
titanic_data$age[is.na(titanic_data$age)] <- mean(titanic_data$age,na.rm=T)

# �Qu� variables pueden pasar por un proceso de discretizaci�n?
apply(titanic_data,2, function(x) length(unique(x)))

# Se discretiza las variables con pocas clases
cols<-c("survived","class","gender","embarked")
      for (i in cols){
            titanic_data[,i] <- as.factor(titanic_data[,i])
      }

# Despu�s de los cambios, se analiza la nueva estructura del conjunto de datos
str(titanic_data)

# =============== IDENTIFICACI�N Y TRATAMIENTO DE VALORES EXTREMOS =============================

# Se utiliza boxplot.stats para identitifcar si en el conjunto de datos existe valores extremos o outliers

boxplot.stats(titanic_data$gender)$out

boxplot.stats(titanic_data$age)$out

boxplot.stats(titanic_data$class)$out

boxplot.stats(titanic_data$embarked)$out

boxplot.stats(titanic_data$ticketno)$out

boxplot.stats(titanic_data$fare)$out

boxplot.stats(titanic_data$sibsp)$out

boxplot.stats(titanic_data$parch)$out

boxplot.stats(titanic_data$survived)$out

# =============== EXPORTACI�N DE LOS DATOS PROCESADOS =============================
# Exportaci�n de los datos limpios en .csv

write.csv(titanic_data, "titanic-cleaning.csv")