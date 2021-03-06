# GR�FICOS 

# Gr�fica 5.1 Boxplot de los campos Age y Fare
par(mfrow=c(1,3))
boxplot(titanic_data1$Age, main ="Age", width = 100)
boxplot(titanic_data1$Fare, main="Fare", width = 100)
boxplot(titanic_data1$FamilySize, main="FamiySize", width = 100)$out

# Gr�fica 5.2 Histograma de las edades de 5 en 5 a�os.
par(mfrow=c(1,1))
hist(titanic_data1$Age, breaks=20)

# Gr�fica 5.3 Boxplot del campo Fare filtrando por los tres valores de Pclass
par(mfrow=c(1,3))
for(i in 1:3){
  boxplot(filter(titanic_data1,titanic_data1$Pclass==i)$Fare, main=paste("Pclass = ",i))
}

# Gr�fica 5.4 Relaci�n entre las variables "Sex" y "Survived":
ggplot(data=titanic_data1[1:filas,],aes(x=Sex,fill=Survived))+geom_bar()

# Se puede observar f�cilmente la cantidad de mujeres que viajaban respecto a los hombres y a su vez observar los que no sobrevivieron. 
# Num�ricamente el n�mero de hombres y mujeres supervivientes es similar.


# Gr�fica 5.5 Relaci�n entre "Survived" como funci�n de "Embarked":

ggplot(data = titanic_data1[1:filas,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frecuencia")

# De forma porcentual se observa que los puertos de embarque y los porcentajes de supervivencia en funci�n del puerto. 
# Se podr�a trabajar con el puerto C (Cherburgo) para explicar la diferencia en los datos. 
# Quiz�s porcentualmente embarcaron m�s mujeres o ni�os O gente de primera clase?

t <-table(titanic_data1[1:filas,]$Embarked,titanic_data1[1:filas,]$Survived)
for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
}
t

# Se obtiene una matriz de porcentajes de frecuencia. 
# Por ejemplo que la probabilidad de sobrevivir si se embarc� en "C" es de un 55.88%

# 
#       0        1
# C 44.11765 55.88235
# Q 61.03896 38.96104
# S 66.30435 33.69565

# Gr�fica 5.6 Realci�n entre "Survived" y "Family Size"
ggplot(data = titanic_data1[!is.na(titanic_data[1:filas,]$FamilySize),],aes(x=FamilySize,fill=Survived))+geom_histogram(binwidth =1,position="fill")+ylab("Frecuencia")

# Gr�fica 5.7 Relaci�n entre "Survived" en funci�n de "Age":
ggplot(data = titanic_data1[!(is.na(titanic_data1[1:filas,]$Age)),],aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)


# SELECCI�N DE GRUPOS DE DATOS

titanic_analisis <- titanic_data1 %>% 
                    select(PassengerId,
                           Sex,
                           Embarked,
                           Age,
                           Pclass,
                           Fare,
                           FamilySize,
                           Survived)

# COMPROBACI�N DE LA NORMALIDAD Y HOMOGENEIDAD DE LA VARIANZA

# Para la comprobaci�n de la normalidad, se utilizar� la prueba de normalidad de Anderson- Darling.

# Se comprueba que para que cada prueba se obtiene un p-valor superior al nivel de significaci�n establecido = 0, 05. 
# Si esto se cumple, entonces se considera que variable en cuesti�n sigue una distribuci�n normal.

# Carga de librer�a
library(nortest)

alpha = 0.05
col.names = colnames(titanic_analisis)
            for (i in 1:ncol(titanic_analisis)) {
              if (i == 1) cat("Variables que no siguen una distribuci�n normal:\n")
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
# Variables que no siguen una distribuci�n normal:
# Age, 
# FamilySize
# Fare

# Para estudiar la homogeniedidad de varianzas se utilizar� el Test Fligner-Killeen, ya que permite comparar las varianzas bas�ndose en la mediana. 
# Es tambi�n una alternativa cuando no se cumple la condici�n de normalidad en las muestras.

fligner.test(Age ~ Survived, data = titanic_analisis)

# 	Fligner-Killeen test of homogeneity of variances

# data:  Age by Survived
# Fligner-Killeen:med chi-squared = 5.4693, df = 1, p-value = 0.01935


# Como se obtiene un p-valor inferior a 0.05, se rechaza la hip�tesis de que las varianzas de ambas muestras son homog�neas.


fligner.test(FamilySize ~ Survived, data = titanic_analisis)

# Fligner-Killeen test of homogeneity of variances

# data:  FamilySize by Survived
# Fligner-Killeen:med chi-squared = 19.647, df = 1, p-value = 9.317e-06

# Tiene un p-valor inferior a 0.05, por lo se rechaza la hip�tesis de que las varianzas de ambas muestras son homog�neas.

fligner.test(Fare ~ Survived, data = titanic_analisis)

# Fligner-Killeen test of homogeneity of variances

# data:  Fare by Survived
# Fligner-Killeen:med chi-squared = 96.253, df = 1, p-value < 2.2e-16

# Tiene un p-valor inferior a 0.05, por lo se rechaza la hip�tesis de que las varianzas de ambas muestras son homog�neas.


# APLICACI�N DE PRUEBAS ESTAD�STICAS 

# An�lisis de Correlaci�n

calculo_numericas <- titanic_analisis %>% 
                     select(PassengerId,
                            Age,
                            Fare,
                            FamilySize)

corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlaci�n para cada variable cuantitativa con respecto al campo "Survived"

for (i in 1:(ncol(calculo_numericas) - 1)) {
  if (is.integer(calculo_numericas[,i]) | is.numeric(calculo_numericas[,i])) {
    spearman_test = cor.test(calculo_numericas[,i],
                             calculo_numericas[,length(calculo_numericas)],
                             method = "spearman")
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    
    # A�adir la fila de matriz
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix <- rbind(corr_matrix, pair)
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(calculo_numericas)[i]
  }
}

print (corr_matrix)

# estimate      p-value
# PassengerId -0.05041556 1.326511e-01
# Age         -0.18421248 3.052168e-08
# Fare         0.52890733 2.269544e-65

# Con la matriz de correlaci�n se puede identificar cu�les son las variables m�s correlacionadas con la supervivencia en funci�n de su
# proximidad con los valores -1 y +1. 

# Modelo de regresi�n l�g�stica

# Se utiliza el modelo de regresi�n log�stica con el conjunto de datos del Titanic para predecir si cada uno de los pasajeros sobrevivi� o no

modelo <- glm(Survived~., family=binomial(link='logit'), data=titanic_analisis)
summary(modelo)

# Ejecuci�n del modelo

fitted.probabilities <- predict(log.model, titanic_data1, type='response')

# Comprobaci�n de la eficacia del modelo

library(caTools)
sample <- sample.split(titanic_analisis, 0.7)
train_final <- subset(titanic_analisis, sample=TRUE)
test_final <- subset(titanic_analisis, sample=FALSE)


final_model <- glm(Survived ~., family=binomial(link='logit'),train_final)
                       summary(final_model)
                       
                       
# Igual que en el modelo incial se puede observar que tanto pertenecer a la clase 2 como a la clase 3 parece estar relacionado con la variable survived, 
# as� como ser hombre. 
        
# Modelo para testear 
fitted.probabilities.final <- predict(final_model, test_final, type='response')
fitted.results.final <- ifelse(fitted.probabilities.final>0.5, 1,0)

# Eficacia del modelo

1-mean(fitted.results.final != test_final$Survived)

# 0.8013468

# Se ha acertado en un 80% de los datos


# Aplicando RadonmForest

library(randomForest) 

# Conjunto train y test

train <- titanic_analisis[1:474,]
test <- titanic_analisis[475:891,]

# Establecer una semilla aleatoria 
set.seed(754)

modelo1 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + FamilySize + Fare + Embarked, data = train)

# Error del modelo

plot(modelo1, ylim = c(0, 0.36))
legend("topright", colnames(modelo1$err.rate), col = 1:3, fill = 1:3)

# En el gr�fico correspondiente se puede observar que la l�nea negra indica la tasa de error global que cae por debajo del 20%.
# Las l�neas roja y verde muestran la tasa de error para el que "muri�" y "sobrevivi�" respectivamente.

# Predicci�n del modelo
prediccion <- predict(modelo1, test)

solucion <- data.frame(PassengerID = test$PassengerId, Survived = prediccion)

table(solucion$Survived)

# 0   1 
# 296 121

# Se puede concluir que este analizando el conjunto de datos Titanic en el que se predice que 296 murieron de 417 pasajeros en el conjunto de prueba. 
# Entonces, el total de pasajeros que murieron en el Titanic son 632 de 891 pasajeros.