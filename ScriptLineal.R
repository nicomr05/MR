################################################################################
################################################################################
## EJERCICIO REGRESIÓN LINEAL
################################################################################

# Datos elegidos -> OzonoLA.rda (Ejercicio 5)
Datos <- OzonoLA

################################################################################
## 1) EXPLORACIÓN DEL CONJUNTO 

head(Datos)
str(Datos)
dim(Datos)
attach(Datos)

## Análisis descritivo de cada variables ########################################

# ANÁLISIS DESCRIPTIVO VARIABLE 'Mes'

summary(Mes)

sd(Mes) #desviación típica
IQR(Mes) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Mes, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Mes, na.rm = FALSE) # distribución normal
boxplot.stats(Mes)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Mes, breaks=5,freq=FALSE, main = "", xlab="Mes",
     cex.lab=1.4, ylab = "Densidad Mes", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Mes),sd=sd(Mes)), 
       col="magenta", lwd=3, add=TRUE)



# Diagrama de cajas con boxplot()
boxplot(Mes, main = "", xlab="Mes",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'DiaMes'

summary(DiaMes)

sd(DiaMes) #desviación típica
IQR(DiaMes) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(DiaMes, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(DiaMes, na.rm = FALSE) # distribución normal
boxplot.stats(DiaMes)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(DiaMes, breaks=5,freq=FALSE, main = "", xlab="DiaMes",
     cex.lab=1.4, ylab = "Densidad DiaMes", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(DiaMes),sd=sd(DiaMes)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(DiaMes, main = "", xlab="DiaMes",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'DiaMes'

summary(DiaMes)

sd(DiaMes) #desviación típica
IQR(DiaMes) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(DiaMes, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(DiaMes, na.rm = FALSE) # distribución normal
boxplot.stats(DiaMes)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(DiaMes, breaks=5,freq=FALSE, main = "", xlab="DiaMes",
     cex.lab=1.4, ylab = "Densidad DiaMes", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(DiaMes),sd=sd(DiaMes)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(DiaMes, main = "", xlab="DiaMes",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'DiaSemana'

summary(DiaSemana)

sd(DiaSemana) #desviación típica
IQR(DiaSemana) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(DiaSemana, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(DiaSemana, na.rm = FALSE) # distribución normal
boxplot.stats(DiaSemana)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(DiaSemana, breaks=5,freq=FALSE, main = "", xlab="DiaSemana",
     cex.lab=1.4, ylab = "Densidad DiaSemana", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(DiaSemana),sd=sd(DiaSemana)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(DiaSemana, main = "", xlab="DiaSemana",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'Ozono'

summary(Ozono)

sd(Ozono) #desviación típica
IQR(Ozono) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Ozono, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Ozono, na.rm = FALSE) # distribución normal
boxplot.stats(Ozono)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Ozono, breaks=5,freq=FALSE, main = "", xlab="Ozono",
     cex.lab=1.4, ylab = "Densidad Ozono", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Ozono),sd=sd(Ozono)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Ozono, main = "", xlab="Ozono",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'Pres_Alt'

summary(Pres_Alt)

sd(Pres_Alt) #desviación típica
IQR(Pres_Alt) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Pres_Alt, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Pres_Alt, na.rm = FALSE) # distribución normal
boxplot.stats(Pres_Alt)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Pres_Alt, breaks=5,freq=FALSE, main = "", xlab="Pres_Alt",
     cex.lab=1.4, ylab = "Densidad Pres_Alt", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Pres_Alt),sd=sd(Pres_Alt)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Pres_Alt, main = "", xlab="Pres_Alt",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1)) 

# ANÁLISIS DESCRIPTIVO VARIABLE 'Vel_Viento'

summary(Vel_Viento)

sd(Vel_Viento) #desviación típica
IQR(Vel_Viento) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Vel_Viento, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Vel_Viento, na.rm = FALSE) # distribución normal
boxplot.stats(Vel_Viento)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Vel_Viento, breaks=5,freq=FALSE, main = "", xlab="Vel_Viento",
     cex.lab=1.4, ylab = "Densidad Vel_Viento", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Vel_Viento),sd=sd(Vel_Viento)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Vel_Viento, main = "", xlab="Vel_Viento",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'Humedad'

summary(Humedad)

sd(Humedad) #desviación típica
IQR(Humedad) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Humedad, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Humedad, na.rm = FALSE) # distribución normal
boxplot.stats(Humedad)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Humedad, breaks=5,freq=FALSE, main = "", xlab="Humedad",
     cex.lab=1.4, ylab = "Densidad Humedad", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Humedad),sd=sd(Humedad)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Humedad, main = "", xlab="Humedad",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'T_Sandburg'

summary(T_Sandburg)

sd(T_Sandburg) #desviación típica
IQR(T_Sandburg) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(T_Sandburg, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(T_Sandburg, na.rm = FALSE) # distribución normal
boxplot.stats(T_Sandburg)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(T_Sandburg, breaks=5,freq=FALSE, main = "", xlab="T_Sandburg",
     cex.lab=1.4, ylab = "Densidad T_Sandburg", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(T_Sandburg),sd=sd(T_Sandburg)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(T_Sandburg, main = "", xlab="T_Sandburg",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'T_ElMonte'

summary(T_ElMonte)

sd(T_ElMonte) #desviación típica
IQR(T_ElMonte) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(T_ElMonte, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(T_ElMonte, na.rm = FALSE) # distribución normal
boxplot.stats(T_ElMonte)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(T_ElMonte, breaks=5,freq=FALSE, main = "", xlab="T_ElMonte",
     cex.lab=1.4, ylab = "Densidad T_ElMonte", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(T_ElMonte),sd=sd(T_ElMonte)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(T_ElMonte, main = "", xlab="T_ElMonte",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'Inv_Alt_b'

summary(Inv_Alt_b)

sd(Inv_Alt_b) #desviación típica
IQR(Inv_Alt_b) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Inv_Alt_b, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Inv_Alt_b, na.rm = FALSE) # distribución normal
boxplot.stats(Inv_Alt_b)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Inv_Alt_b, breaks=5,freq=FALSE, main = "", xlab="Inv_Alt_b",
     cex.lab=1.4, ylab = "Densidad Inv_Alt_b", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Inv_Alt_b),sd=sd(Inv_Alt_b)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Inv_Alt_b, main = "", xlab="Inv_Alt_b",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'Grad_Pres'

summary(Grad_Pres)

sd(Grad_Pres) #desviación típica
IQR(Grad_Pres) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Grad_Pres, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Grad_Pres, na.rm = FALSE) # distribución normal
boxplot.stats(Grad_Pres)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Grad_Pres, breaks=5,freq=FALSE, main = "", xlab="Grad_Pres",
     cex.lab=1.4, ylab = "Densidad Grad_Pres", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Grad_Pres),sd=sd(Grad_Pres)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Grad_Pres, main = "", xlab="Grad_Pres",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'Inv_T_b'

summary(Inv_T_b)

sd(Inv_T_b) #desviación típica
IQR(Inv_T_b) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Inv_T_b, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Inv_T_b, na.rm = FALSE) # distribución normal
boxplot.stats(Inv_T_b)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Inv_T_b, breaks=5,freq=FALSE, main = "", xlab="Inv_T_b",
     cex.lab=1.4, ylab = "Densidad Inv_T_b", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Inv_T_b),sd=sd(Inv_T_b)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Inv_T_b, main = "", xlab="Inv_T_b",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

# ANÁLISIS DESCRIPTIVO VARIABLE 'Visibilidad'

summary(Visibilidad)

sd(Visibilidad) #desviación típica
IQR(Visibilidad) #rango intercuartílico: Q3 - Q1: dispersión central de los datos

library(moments)   # Para evaluar coeficiente asimetría y kurtosis
skewness(Visibilidad, na.rm = FALSE) # asimetría moderada hacia la derecha
kurtosis(Visibilidad, na.rm = FALSE) # distribución normal
boxplot.stats(Visibilidad)$out  # No hay registros atípicos

par(mfrow=c(1,2)) 

# Histograma y ajuste normal con hist()+curve() y dnorm()
hist(Visibilidad, breaks=5,freq=FALSE, main = "", xlab="Visibilidad",
     cex.lab=1.4, ylab = "Densidad Visibilidad", col = "lightblue")

#sobre el propio histograma, dibujamos la normal más adecuada.
curve( dnorm(x,mean=mean(Visibilidad),sd=sd(Visibilidad)), 
       col="magenta", lwd=3, add=TRUE)

# Diagrama de cajas con boxplot()
boxplot(Visibilidad, main = "", xlab="Visibilidad",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
par(mfrow=c(1,1))

################################################################################
## 2) ANÁLISIS DE CORRELACIÓN

# CORRELACIONES BIVARIANTES
library(psych)
pairs.panels(OzonoLA, smooth = TRUE, density=TRUE, digits = 2, 
             ellipses=TRUE, method="pearson", pch = 20, 
             lm=TRUE, cor=TRUE)
cor(OzonoLA)

# CORRELACIONES PARCIALES
partial.r(OzonoLA)

# hacer comparación de las parciales y las bivariantes.
# si las bivariantes son mayores que las parciales -> multicolinealidad.

################################################################################
## 3) MODELO MATEMÁTICO

MOD_FULL <- lm(Ozono~., data=OzonoLA)
MOD_FULL 
coef(MOD_FULL )
# Ozono = 55.428 - 0.343*Mes + 0.012*Diames - 0.047*DiaSeman - 0.0133*Pres_Alt
# - 0.096*Vel_Viento + 0.088*Humedad + 0.1366*T_Sandburg + 0.5598*T_ElMonte 
# - 0.0006*Inv_Alt_b + 0.0004*Grad_Pres - 0.124*Inv_T_b - 0.005*Visibilidad + error_i
# error_i  i.i.d.  N(0,sigma)

( MSSR <- summary(MOD_FULL )$sigma^2 )
( gl.R <- MOD_FULL $df )
( gl.E <- MOD_FULL $rank )

################################################################################
## 4) ANÁLISIS DE MULTICOLINEALIDAD

summary(MOD_FULL ) 
# Obtenemos que muchos de los coeficientes son no significativos, por lo que 
# debemos hacer una selección de las variables. 
# No obstante, como esto se puede deber a la presencia de multicolinealidad, 
# vamos a analizarla.

# Para ello, utilizaré la librería "mctest", que  proporciona un análisis completo 
# de multicolinealidad:
library(mctest)
mctest(MOD_FULL, type="o")  
# Este test proporciona 6 medidas, de las cuales 4 indican que estamos ante un caso
# en el que la multicolinealidad está presente.

# Para solucionar esto y conseguir un ajuste correcto, sobre el que hacer inferencia
# debemos hacer una selección de variables.

################################################################################
## 5) SELECCIÓN DEL MODELO

# Para hacer la selección del modelo, utilizaremos la selección sistemática por 
# STEPWISE, utilizando como criterio el AIC del modelo. Elegimos este método de 
# selección por ser el mejor, al permitir incluir y eliminar variables a lo largo
# del proceso. 

Mod_NULL <- lm(Ozono ~ 1, data = Datos) # modelo solo con intercept
stepMod <- step(Mod_NULL, direction = "both", trace = 1,
                scope = list(lower = Mod_NULL, 
                             upper = MOD_FULL) ) 
summary((stepMod))
# ESCRIBIR EL MODELO RESULTANTE

# Comprobaremos si es mejor que nuestro modelo inicial, utilizando un anova
# de modelos anidados:
anova(stepMod, MOD_FULL) # no significativo -> nos quedamos con el modelo nuevo

ajuste <- stepMod

################################################################################
## 6) INFERENCIA MODELO SELECCIONADO

################################################################################
## 7) VALIDACIÓN MODELO SELECCIONADO

# Por abreviar la notación, tenemos:
MS <- ajuste  # Ajuste modelo elegido: consumo~peso+cilindros
MC <- MOD_FULL # Ajuste modelo completo: consumo~.

# Primero, calculamos el coeficiente de robusted del ajuste:
library(DAAG)
( B2 <- sum(residuals(MS)^2)/press(MS) )
# Elevado y superior al del modelo completo
sum(residuals(MC)^2)/press(MC) 

# Haremos una validación del tipo LOOCV (Leave One Out Cross Validation):

# Primero, para MS:

class(OzonoLA) # ya es un data frame
set.seed(5198) 
cv_k3_MS <- cv.lm(data=OzonoLA,form.lm= formula(MS),m=length(OzonoLA))  
# Se calcula la raíz cuadrada de la media de los cuadrados de las diferencias entre predicciones y observaciones:
errores <- cv_k3_MS$cvpred - cv_k3_MS$Ozono # predicho por cv - predicción real
( error_cv_k3_MS <- sqrt(mean(errores^2)) ) # estimador RMSE (raiz media suma residuos al cuadrado)

# Finalmente, para MC:
set.seed(5198) 
cv_k3_MC <- cv.lm(data=OzonoLA,form.lm=formula(MC),m=length(OzonoLA))   
errores <- cv_k3_MC$cvpred - cv_k3_MC$Ozono
( error_cv_k3_MC <- sqrt(mean(errores^2)) )

# Obtenemos un comportamiento mejor con el MS que con MC, pues tenemos un menor error.

################################################################################
## 8) ANÁLISIS DE RESIDUOS MODELO SELECCIONADO


################################################################################
## 9) ANÁLISIS DE INFLUENCIA MODELO SELECCIONADO


################################################################################
## 10) ESTIMACIÓN MEDIA CONDICIONADA Y PREDICCIÓN

# Finalmente, obtengamos el intervalo de confianza y de predicción para el nivel 
# de ozono medio al 95% de confianza con el modelo seleccionado con todas las 
# variables fijadas en su valor medio.

new.dat <- data.frame(T_Sandburg = mean(T_Sandburg), Humedad = mean(Humedad), 
                      T_ElMonte = mean(T_ElMonte), Mes = mean(Mes),
                      Pres_Alt = mean(Pres_Alt), Inv_Alt_b = mean(Inv_Alt_b)) # tiene que aparecer valores de las vbles que están en el modelo.
predict(ajuste, newdata = new.dat, interval="confidence", level = 0.95)
predict(ajuste, newdata = new.dat, interval="prediction", level = 0.95)




















