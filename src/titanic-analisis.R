# GRÁFICOS 

# Relación entre las variables "Sex" y "Survived":
ggplot(data=titanic_data1[1:filas,],aes(x=Sex,fill=Survived))+geom_bar()

# Se puede observar fácilmente la cantidad de mujeres que viajaban respecto a los hombres y a su vez observar los que no sobrevivieron. 
# Numéricamente el número de hombres y mujeres supervivientes es similar.


# Relación entre "Survived" como función de "Embarked":

ggplot(data = titanic_data1[1:filas,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frecuencia")

# De forma porcentual se observa que los puertos de embarque y los porcentajes de supervivencia en función del puerto. 
# Se podría trabajar con el puerto C (Cherburgo) para explicar la diferencia en los datos. 
# Quizás porcentualmente embarcaron más mujeres o niños O gente de primera clase?

t <-table(titanic_data1[1:filas,]$Embarked,titanic_data1[1:filas,]$Survived)
for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
}
t

# Se obtiene una matriz de porcentajes de frecuencia. 
# Por ejemplo que la probabilidad de sobrevivir si se embarcó en "C" es de un 55.88%

# 
#       0        1
# C 44.11765 55.88235
# Q 61.03896 38.96104
# S 66.30435 33.69565

# Family Size
ggplot(data = titanic_data1[!is.na(titanic_data[1:filas,]$FamilySize),],aes(x=FamilySize,fill=Survived))+geom_histogram(binwidth =1,position="fill")+ylab("Frecuencia")

# Relación entre "Survived" en función de "Age":
ggplot(data = titanic_data1[!(is.na(titanic_data1[1:filas,]$Age)),],aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)


# SELECCIÓN DE GRUPOS DE DATOS

titanic_analisis <- titanic_data1 %>% 
                    select(Sex,
                           Embarked,
                           Age,
                           FamilySize,
                           Survived)

# COMPROBACIÓN DE LA NORMALIDAD Y HOMOGENEIDAD DE LA VARIANZA

# Para la comprobación de la normalidad, se utilizará la prueba de normalidad de Anderson- Darling.

# Se comprueba que para que cada prueba se obtiene un p-valor superior al nivel de significación establecido = 0, 05. 
# Si esto se cumple, entonces se considera que variable en cuestión sigue una distribución normal.

# Carga de librería
library(nortest)

alpha = 0.05
col.names = colnames(titanic_analisis)
            for (i in 1:ncol(titanic_analisis)) {
              if (i == 1) cat("Variables que no siguen una distribución normal:\n")
                  if (is.integer(titanic_analisis[,i]) | is.numeric(titanic_analisis[,i])) {
                        p_val = ad.test(titanic_analisis[,i])$p.value
                  if (p_val < alpha) {
                      cat(col.names[i])
                    # Formato de salida
                      if (i < ncol(titanic_analisis) - 1) cat(", ")
                          if (i %% 3 == 0) cat("\n")
            }
          }
        }
# Variables que no siguen una distribución normal:
# Age, 
# FamilySize

# Para estudiar la homogeniedidad de varianzas se utilizará el Test Fligner-Killeen, ya que permite comparar las varianzas basándose en la mediana. 
# Es también una alternativa cuando no se cumple la condición de normalidad en las muestras.

fligner.test(Age ~ Survived, data = titanic_analisis)


# 	Fligner-Killeen test of homogeneity of variances

# data:  Age by Survived
# Fligner-Killeen:med chi-squared = 5.4693, df = 1, p-value = 0.01935


# Como se obtiene un p-valor inferior a 0.05, se rechaza la hipótesis de que las varianzas de ambas muestras son homogéneas.


fligner.test(FamilySize ~ Survived, data = titanic_analisis)

# Fligner-Killeen test of homogeneity of variances

# data:  FamilySize by Survived
# Fligner-Killeen:med chi-squared = 19.647, df = 1, p-value = 9.317e-06

# Tiene un p-valor inferior a 0.05, por lo que se rechaza la hipótesis de que las varianzas de ambas muestras son homogéneas.


# APLICACIÓN DE PRUEBAS ESTADÍSTICAS 

# Método de Correlación

# Entre Fare y Age



# Se utiliza el modelo de regresión lineal con el conjunto de datos del Titanic para predecir si cada uno de los pasajeros sobrevivió o no

modelo1 <- lm(Survived ~ Age + Sex, data = titanic_analisis)
modelo1

modelo2 <- lm(Survived ~ Age +Embarked + FamilySize, data=titanic_analisis)
modelo2