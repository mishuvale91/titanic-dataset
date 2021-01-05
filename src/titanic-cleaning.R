# PRÁCTICA 2 - TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS 

# INTEGRANTES:
# JUAN MANUEL PENALTA
# MICHAELLE VALENZUELA

# =====================CARGA DE LIBRERÍAS ==============================
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)

# ==================== LECTURA DEL CONJUNTO DE DATOS ========================

titanic_data <- read.csv("UOC/M2.851 - Tipologia y ciclo de vida de los datos aula 3/Practica 2/titanic/train.csv", stringsAsFactors = FALSE)
filas=dim(titanic_data)[1]
head(titanic_data)

# Tipo de dato asignado a cada campo
sapply(titanic_data, function(x) class(x))

# ==================== SELECCIÓN DE DATOS ===================================

# Se crea una nueva variable
titanic_data$FamilySize <- titanic_data$SibSp + titanic_data$Parch +1;
titanic_data1<-titanic_data[1:filas,]

titanic_data1 <- titanic_data1 %>% 
  select(-Name,
         -Cabin,
         -SibSp,
         -Parch)  

# Estructura del conjunto de datos
str(titanic_data1)

# ==================== LIMPIEZA DE DATOS ====================================

# Estadísticas de valores vacíos
colSums(is.na(titanic_data1))

colSums(titanic_data1=="")

# Se toma la media para valores vacíos de la variable "Age"
titanic_data1$Age[is.na(titanic_data1$Age)] <- mean(titanic_data$Age,na.rm=T)

# Se convierte el formato de la variable age de numérica a entero
titanic_data1$Age <- as.integer(as.numeric(titanic_data1$Age))


# Se examina qué pasajero ha desaparecido 
titanic_data1$PassengerId[titanic_data1$Embarked == ""]

# 62 830   // Falta el pasajero 62 y 830 Embarcado.

# Ahora se sabe a qué clase pertenecen y cuánto pagaron por tarifa

titanic_data1$Pclass[titanic_data1$PassengerId == 62]

# 1

titanic_data1$Fare[titanic_data1$PassengerId == 62]

# 80


titanic_data1$Pclass[titanic_data1$PassengerId == 830]

# 1

titanic_data1$Fare[titanic_data1$PassengerId == 830]

# 80

# Se puede observar que ambos pasajeros están en clase 1 y tarifa pagada 80. 

# La tarifa mediana para el pasajero de primera clase que sale de C (Charbourg) Embarcado coincide muy bien con los $ 80 pagados 
# por los pasajeros cuyo Embarcado falta. Entonces se procede a reemplazar con seguridad el NA con C.

titanic_data1$Embarked[c(62, 830)] <- "C"

# ¿Qué variables pueden pasar por un proceso de discretización?
apply(titanic_data1,2, function(x) length(unique(x)))

# Se discretiza las variables con pocas clases
cols<-c("Survived","Pclass","Sex","Embarked")
for (i in cols){
  titanic_data1[,i] <- as.factor(titanic_data1[,i])
}

# Después de los cambios, se analiza la nueva estructura del conjunto de datos
str(titanic_data1)

# =============== IDENTIFICACIÓN Y TRATAMIENTO DE VALORES EXTREMOS =============================

# Se utiliza boxplot.stats para identitifcar si en el conjunto de datos existe valores extremos o outliers

boxplot.stats(titanic_data1$Survived)$out

# Levels: 0 1

boxplot.stats(titanic_data1$Pclass)$out

# Levels: 1 2 3

boxplot.stats(titanic_data1$Sex)$out

# Levels: female male

boxplot.stats(titanic_data1$Age)$out

# 2 58 55  2 66 65  0 59 71 70  2 55  1 61  1 56  1 58  2 59 62 58 63 65  2  0 61  2 60  1  1 64 65 56  0  2 63 58
# 55 71  2 64 62 62 60 61 57 80  2  0 56 58 70 60 60 70  0 57  1  0  2  1 62  0 74 56

# Con estos valores se puede observar que hay muchos pasajeros cuya edad excede los valores mas comunes,es decir,
# los valores superiores a 64, por lo que se puede deducir que hay personas mayores a bordo del barco es raro. 

boxplot.stats(titanic_data1$FamilySize)$out

# 5  7  6  5  7  6  4  6  4  8  6  7  8  4  5  6  4  7  5 11  6  6  6  5 11  7  4 11  5  7  7  6  6  4  4  5 11  6
# 6  5  8  4  5  4  5  6  6  4  4  4  4  8  5  4  4  7  7  5  4  4  7  4  4  6  6  6  4  8  8  4  6  4  5  5  4  4
# 5  4  6  4 11  4  7  6  6 11  7  4 11  6  4

boxplot.stats(titanic_data1$Fare)$out

# 71.2833 263.0000 146.5208  82.1708  76.7292  80.0000  83.4750  73.5000 263.0000  77.2875 247.5208  73.5000
# 77.2875  79.2000  66.6000  69.5500  69.5500 146.5208  69.5500 113.2750  76.2917  90.0000  83.4750  90.0000
# 79.2000  86.5000 512.3292  79.6500 153.4625 135.6333  77.9583  78.8500  91.0792 151.5500 247.5208 151.5500
# 110.8833 108.9000  83.1583 262.3750 164.8667 134.5000  69.5500 135.6333 153.4625 133.6500  66.6000 134.5000
# 263.0000  75.2500  69.3000 135.6333  82.1708 211.5000 227.5250  73.5000 120.0000 113.2750  90.0000 120.0000
# 263.0000  81.8583  89.1042  91.0792  90.0000  78.2667 151.5500  86.5000 108.9000  93.5000 221.7792 106.4250
# 71.0000 106.4250 110.8833 227.5250  79.6500 110.8833  79.6500  79.2000  78.2667 153.4625  77.9583  69.3000
# 76.7292  73.5000 113.2750 133.6500  73.5000 512.3292  76.7292 211.3375 110.8833 227.5250 151.5500 227.5250
# 211.3375 512.3292  78.8500 262.3750  71.0000  86.5000 120.0000  77.9583 211.3375  79.2000  69.5500 120.0000
# 93.5000  80.0000  83.1583  69.5500  89.1042 164.8667  69.5500  83.1583

boxplot.stats(titanic_data1$Embarked)$out

# Levels:  C Q S

# =============== EXPORTACIÓN DE LOS DATOS PROCESADOS =============================
# Exportación de los datos limpios en .csv

write.csv(titanic_data1, "titanic-cleaning.csv")