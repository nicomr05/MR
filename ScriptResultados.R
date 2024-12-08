#' ---
#' title: '**MR - Trabajo**'
#' author:
#' - Alicia Losada | <alicia.losada.sanchez@udc.es>
#' - María Cardoso | <m.cardoso@udc.es>
#' - Nicolás Muñiz | <nicolas.muniz@udc.es>
#' date: '**11/12/2024**'
#' output: pdf_document
#' ---
#' $\newline$
#' 
#' # **Regresión Lineal Múltiple**
#' 
#' - Antes de empezar, cargamos los datos *OzonoLA.rda*
load("Datos/OzonoLA.rda")
attach(OzonoLA)
#' 
#' ## **1.** Análisis descriptivo
#' Para el análisis descriptivo de las variables podemos comenzar con una visión
#' general de las variables mediante las funciones `str()` y `summary()`.
str(OzonoLA)
#'  La salida de `str()` nos dice que los datos constan de 203 observaciones de 13 variables:
#'  
#' - `Mes`: Número del mes en el que se hicieron las observaciones (Entero)
#' - `DiaMes`: Número del día del mes en el que se hicieron las observaciones (Entero)
#' - `DíaSemana`: Número del día de la semana en el que se hicieron las observaciones (Entero)
#' - `Ozono`: Nivel de Ozono medido (Numérica)
#' - `Pres_Alt`: Altura en metros a la que se alcanza una presion de 500 milibares (Entero)
#' - `Vel_Viento`: Velocidad del viento en millas por hora en el Aeropuerto Internacional de Los Angeles (Entero)
#' - `Humedad`: Humedad en porcentaje en LAX (Entero)
#' - `T_Sandburg`: Temperatura (F) en Sandburg, CA (Entero)
#' - `T_ElMonte`: Temperatura (F) en El Monte, CA (Numérica)
#' - `Inv_ALt_b`: Inversion de la altura base (en pies) en LAX (Entero)
#' - `Grand_Pres`: Gradiente de presion de LAX a Daggett, CA (Entero)
#' - `Inv_T_b`: Inversion de la temperatura base (F) en LAX (Numérica)
#' - `Visibilidad`: Visibilidad (millas) evaluada en LAX (Entero)
#' 
summary(OzonoLA)
#'
#' Ahora realizaremos un análisis descriptivo de cada variable:
#' 
#' ### Análisis descriptivo de la variable `Mes` :
summary(Mes)
#' Desviación típica y rango intercuartílico:
sd(Mes) 
IQR(Mes) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Mes, na.rm = FALSE) 
kurtosis(Mes, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis menor que tres, las colas
#' de la variable comparadas con una normal son más ligeras.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Mes)$out  
#' Como podemos ver no existe ningún registro atípico
#' 
par(mfrow=c(1,2)) 
hist(Mes, breaks=5,freq=FALSE, main = "", xlab="Mes",
     cex.lab=1.4, ylab = "Densidad Mes", col = "lightblue")
curve( dnorm(x,mean=mean(Mes),sd=sd(Mes)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Mes, main = "", xlab="Mes",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `DiaMes` :
summary(Mes)
#' Desviación típica y rango intercuartílico:
sd(DiaMes) 
IQR(DiaMes) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(DiaMes, na.rm = FALSE) 
kurtosis(DiaMes, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis menor que tres, las colas
#' de la variable comparadas con una normal son más ligeras.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(DiaMes)$out  
#' Como podemos ver no existe ningún registro atípico
#' 
par(mfrow=c(1,2)) 
hist(DiaMes, breaks=5,freq=FALSE, main = "", xlab="DiaMes",
     cex.lab=1.4, ylab = "Densidad DiaMes", col = "lightblue")
curve( dnorm(x,mean=mean(DiaMes),sd=sd(DiaMes)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(DiaMes, main = "", xlab="DiaMes",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `DiaSemana` :
summary(DiaSemana)
#' Desviación típica y rango intercuartílico:
sd(DiaSemana) 
IQR(DiaSemana) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(DiaSemana, na.rm = FALSE) 
kurtosis(DiaSemana, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis menor que tres, las colas
#' de la variable comparadas con una normal son más ligeras.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(DiaSemana)$out  
#' Como podemos ver no existe ningún registro atípico
#' 
par(mfrow=c(1,2)) 
hist(DiaSemana, breaks=5,freq=FALSE, main = "", xlab="DiaSemana",
     cex.lab=1.4, ylab = "Densidad DiaSemana", col = "lightblue")
curve( dnorm(x,mean=mean(DiaSemana),sd=sd(DiaSemana)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(DiaSemana, main = "", xlab="DiaSemana",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Ozono` :
summary(Ozono)
#' Desviación típica y rango intercuartílico:
sd(Ozono) 
IQR(Ozono) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Ozono, na.rm = FALSE) 
kurtosis(Ozono, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es próximo a tres, las colas
#' de la variable son similares a las de una normal
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Ozono)$out  
#' Como podemos ver existen 4 registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Ozono, breaks=5,freq=FALSE, main = "", xlab="Ozono",
     cex.lab=1.4, ylab = "Densidad Ozono", col = "lightblue")
curve( dnorm(x,mean=mean(Ozono),sd=sd(Ozono)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Ozono, main = "", xlab="Ozono",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Pres_Alt` :
summary(Pres_Alt)
#' Desviación típica y rango intercuartílico:
sd(Pres_Alt) 
IQR(Pres_Alt) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Pres_Alt, na.rm = FALSE) 
kurtosis(Pres_Alt, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es mayor a tres, las colas
#' de la variable son más grandes que las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Pres_Alt)$out  
#' Como podemos ver existen 5 registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Pres_Alt, breaks=5,freq=FALSE, main = "", xlab="Pres_Alt",
     cex.lab=1.4, ylab = "Densidad Pres_Alt", col = "lightblue")
curve( dnorm(x,mean=mean(Pres_Alt),sd=sd(Pres_Alt)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Pres_Alt, main = "", xlab="Pres_Alt",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Vel_Viento` :
summary(Vel_Viento)
#' Desviación típica y rango intercuartílico:
sd(Vel_Viento) 
IQR(Vel_Viento) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Vel_Viento, na.rm = FALSE) 
kurtosis(Vel_Viento, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es próximo a tres, las colas
#' de la variable son similares a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Vel_Viento)$out  
#' Como podemos ver existen 2 registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Vel_Viento, breaks=5,freq=FALSE, main = "", xlab="Vel_Viento",
     cex.lab=1.4, ylab = "Densidad Vel_Viento", col = "lightblue")
curve( dnorm(x,mean=mean(Vel_Viento),sd=sd(Vel_Viento)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Vel_Viento, main = "", xlab="Vel_Viento",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Humedad` :
summary(Humedad)
#' Desviación típica y rango intercuartílico:
sd(Humedad) 
IQR(Humedad) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Humedad, na.rm = FALSE) 
kurtosis(Humedad, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es próximo a tres, las colas
#' de la variable son similares a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Humedad)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Humedad, breaks=5,freq=FALSE, main = "", xlab="Humedad",
     cex.lab=1.4, ylab = "Densidad Humedad", col = "lightblue")
curve( dnorm(x,mean=mean(Humedad),sd=sd(Humedad)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Humedad, main = "", xlab="Humedad",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `T_Sandburg` :
summary(T_Sandburg)
#' Desviación típica y rango intercuartílico:
sd(T_Sandburg) 
IQR(T_Sandburg) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(T_Sandburg, na.rm = FALSE) 
kurtosis(T_Sandburg, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es próximo a tres, las colas
#' de la variable son similares a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(T_Sandburg)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(T_Sandburg, breaks=5,freq=FALSE, main = "", xlab="T_Sandburg",
     cex.lab=1.4, ylab = "Densidad T_Sandburg", col = "lightblue")
curve( dnorm(x,mean=mean(T_Sandburg),sd=sd(T_Sandburg)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(T_Sandburg, main = "", xlab="T_Sandburg",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' - ANÁLISIS DESCRIPTIVO VARIABLE 'T_ElMonte'
summary(T_ElMonte)
#' Desviación típica y rango intercuartílico:
sd(T_ElMonte) 
IQR(T_ElMonte) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(T_ElMonte, na.rm = FALSE) 
kurtosis(T_ElMonte, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es próximo a tres, las colas
#' de la variable son similares a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(T_ElMonte)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(T_ElMonte, breaks=5,freq=FALSE, main = "", xlab="T_ElMonte",
     cex.lab=1.4, ylab = "Densidad T_ElMonte", col = "lightblue")
curve( dnorm(x,mean=mean(T_ElMonte),sd=sd(T_ElMonte)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(T_ElMonte, main = "", xlab="T_ElMonte",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Inv_Alt_b` :
summary(Inv_Alt_b)
#' Desviación típica y rango intercuartílico:
sd(Inv_Alt_b) 
IQR(Inv_Alt_b) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Inv_Alt_b, na.rm = FALSE) 
kurtosis(Inv_Alt_b, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es menor a tres, las colas
#' de la variable son más ligeras a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Inv_Alt_b)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Inv_Alt_b, breaks=5,freq=FALSE, main = "", xlab="Inv_Alt_b",
     cex.lab=1.4, ylab = "Densidad Inv_Alt_b", col = "lightblue")
curve( dnorm(x,mean=mean(Inv_Alt_b),sd=sd(Inv_Alt_b)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Inv_Alt_b, main = "", xlab="Inv_Alt_b",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Grad_Pres` :
summary(Grad_Pres)
#' Desviación típica y rango intercuartílico:
sd(Grad_Pres) 
IQR(Grad_Pres) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Grad_Pres, na.rm = FALSE) 
kurtosis(Grad_Pres, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es menor a tres, las colas
#' de la variable son más ligeras a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Grad_Pres)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Grad_Pres, breaks=5,freq=FALSE, main = "", xlab="Grad_Pres",
     cex.lab=1.4, ylab = "Densidad Grad_Pres", col = "lightblue")
curve( dnorm(x,mean=mean(Grad_Pres),sd=sd(Grad_Pres)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Grad_Pres, main = "", xlab="Grad_Pres",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Inv_T_b` :
summary(Inv_T_b)
#' Desviación típica y rango intercuartílico:
sd(Inv_T_b) 
IQR(Inv_T_b) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Inv_T_b, na.rm = FALSE) 
kurtosis(Inv_T_b, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis es menor a tres, las colas
#' de la variable son más ligeras a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Inv_T_b)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Inv_T_b, breaks=5,freq=FALSE, main = "", xlab="Inv_T_b",
     cex.lab=1.4, ylab = "Densidad Inv_T_b", col = "lightblue")
curve( dnorm(x,mean=mean(Inv_T_b),sd=sd(Inv_T_b)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Inv_T_b, main = "", xlab="Inv_T_b",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ### Análisis descriptivo de la variable `Visibilidad` :
summary(Visibilidad)
#' Desviación típica y rango intercuartílico:
sd(Visibilidad) 
IQR(Visibilidad) 
#' 
#' Evaluamos la asimetría y kurtoisis 
library(moments)   
skewness(Visibilidad, na.rm = FALSE) 
kurtosis(Visibilidad, na.rm = FALSE) 
#' Podemos ver que al ser el coeficiente de asimetría cercano a 0 que puede ser una 
#' variable simética y al ser el coeficiente de Kurtosis próximo a tres, las colas
#' de la variable son próximas a las de una normal.
#' 
#' Vemos si hay registros atípicos
boxplot.stats(Visibilidad)$out  
#' Como podemos ver no existen registros atípicos
#' 
par(mfrow=c(1,2)) 
hist(Visibilidad, breaks=5,freq=FALSE, main = "", xlab="Visibilidad",
     cex.lab=1.4, ylab = "Densidad Visibilidad", col = "lightblue")
curve( dnorm(x,mean=mean(Visibilidad),sd=sd(Visibilidad)), 
       col="magenta", lwd=3, add=TRUE)
boxplot(Visibilidad, main = "", xlab="Visibilidad",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#' 
#' ## **2.** Análisis de correlación
#' - Correlaciones simples bivariantes(análisis gráfico y numérico):
library(psych)
pairs.panels(OzonoLA, smooth = TRUE, density=TRUE, digits = 2, 
             ellipses=TRUE, method="pearson", pch = 20, 
             lm=TRUE, cor=TRUE)
cor(OzonoLA)
#' - Correlaciones parciales:
partial.r(OzonoLA)
#' 
#' ## **3.** Modelo matemático
#' \begin{equation} \label{linearmodel}
#' \mathbb{E}(\vec{Y}|\boldsymbol{X}) = \beta_0 + \sum_{i=1}^{n}\beta_iX_{ij}
#' \end{equation}
MOD_FULL <- lm(Ozono~., data=OzonoLA)
MOD_FULL
coef(MOD_FULL)
#' Ozono = 55.428 - 0.343*Mes* + 0.012*Diames* - 0.047*DiaSeman* - 0.0133*Pres_Alt*
#' - 0.096*Vel_Viento* + 0.088*Humedad* + 0.1366*T_Sandburg* + 0.5598*T_ElMonte* 
#' - 0.0006*Inv_Alt_b* + 0.0004*Grad_Pres* - 0.124*Inv_T_b* - 0.005*Visibilidad*
( MSSR <- summary(MOD_FULL)$sigma^2 )
( gl.R <- MOD_FULL$df )
( gl.E <- MOD_FULL$rank )
#'
#'
#' ## **4.** Análisis de multicolinealidad 
#' 
summary(MOD_FULL) 
#' Obtenemos que muchos de los coeficientes son no significativos, por lo que 
#' debemos hacer una selección de las variables. 
#' No obstante, como esto se puede deber a la presencia de multicolinealidad, 
#' vamos a analizarla.
#'
#' Para ello, utilizaremos la librería "mctest", que  proporciona un análisis completo 
#' de multicolinealidad:
library(mctest)
mctest(MOD_FULL, type="o")  
#' Este test proporciona 6 medidas, de las cuales 4 indican que estamos ante un caso
#' en el que la multicolinealidad está presente.
#'
#' Para solucionar esto y conseguir un ajuste correcto, sobre el que hacer inferencia
#' debemos hacer una selección de variables.
#'
#'
#' ## **5.** Selección del modelo 
#' 
#' Para hacer la selección del modelo, utilizaremos la selección sistemática por 
#' STEPWISE, utilizando como criterio el AIC del modelo. Elegimos este método de 
#' selección por ser el mejor, al permitir incluir y eliminar variables a lo largo
#' del proceso. 
#'
#' Primero, definimos el modelo con solo el intercept.
Mod_NULL <- lm(Ozono ~ 1, data = Datos) 
#' Ahora, aplicaremos la siguiente función para obtener el modelo óptimo:
stepMod <- step(Mod_NULL, direction = "both", trace = 1,
                scope = list(lower = Mod_NULL, 
                             upper = MOD_FULL) ) 
summary((stepMod))
#' El modelo resultante de la selección secuencial es:
#' Ozono = 51.3444845 - 0.3324536*Mes* - 0.0134013 *Pres_Alt*
#' + 0.0975694*Humedad* + 0.1242673*T_Sandburg* + 0.4743962*T_ElMonte* 
#' - 0.0003211*Inv_Alt_b*
#' 
#' No obstante, con un 10% de significación, la variable Inv_Alt_b no es significativa,
#' por lo que examinaremos si se debe excluir del modelo:
ajuste_sin_inv_alt_b <- update(stepMod, .~.-Inv_Alt_b) 
#' Lo comprobaremos con un anova de modelos anidados:
anova(ajuste_sin_inv_alt_b, stepMod)
#' Prueba no significativa, por lo que nos quedamos con el modelo sin la variable.
ajuste <- ajuste_sin_inv_alt_b
#' Comprobaremos si es mejor que el modelo completo, utilizando un anova
#' de modelos anidados:
anova(ajuste, MOD_FULL) 
#' El resultado es no significativo, por lo que la selección ha merecido la pena.
#'
#' ## **6.** Inferencia modelo 
#' 
#' Ahora ya podemos comenzar la inferencia.
summary(ajuste)
#' Las únicas variables que parecen ser significativas son Mes, Humedad y T_ElMonte.
#' También podemos considerar que son bastante significativas, pero no tanto, las 
#' variables T_Sandburg y Pres_Alt. Por otra parte, según el coeficiente de bondad, 
#' con este ajuste podemos explicar el 73,04% de la variabilidad de los datos. Por último, 
#' gracias a la última linea del summary deducimos que es mejor este ajuste en comparación 
#' al modelo que contiene únicamente el intercept, debido al p-valor < 2.2e-16. 
#' 
#'
#' ## **7.** Validación modelo seleccionado 
#' 
#' Por abreviar la notación, tenemos:
MS <- ajuste  # Ajuste modelo elegido.
MC <- MOD_FULL # Ajuste modelo completo

#' Primero, calculamos el coeficiente de robusted del ajuste:
library(DAAG)
( B2 <- sum(residuals(MS)^2)/press(MS) )
#' Elevado y superior al del modelo completo
sum(residuals(MC)^2)/press(MC) 

#' Haremos una validación del tipo LOOCV (Leave One Out Cross Validation):

#' Primero, para MS:

class(OzonoLA) # ya es un data frame
set.seed(5198) 
cv_k3_MS <- cv.lm(data=OzonoLA,form.lm= formula(MS),m=length(OzonoLA))  
#' Se calcula la raíz cuadrada de la media de los cuadrados de las diferencias entre predicciones y observaciones:
errores <- cv_k3_MS$cvpred - cv_k3_MS$Ozono # predicho por cv - predicción real
( error_cv_k3_MS <- sqrt(mean(errores^2)) ) # estimador RMSE (raiz media suma residuos al cuadrado)

#' Finalmente, para MC:
set.seed(5198) 
cv_k3_MC <- cv.lm(data=OzonoLA,form.lm=formula(MC),m=length(OzonoLA))   
errores <- cv_k3_MC$cvpred - cv_k3_MC$Ozono
( error_cv_k3_MC <- sqrt(mean(errores^2)) )

#' Obtenemos un comportamiento mejor con el MS que con MC, pues tenemos un menor error.
#' 
#' ## **8.** Análisis de residuos modelo seleccionado 
#' 
#'
#' ## **9.** Análisis de influencia modelo seleccionado
#' 
#' 
#' ## **10.** Estimación media condicionada y predicción 
#' Finalmente, obtengamos el intervalo de confianza y de predicción para el nivel 
#' de ozono medio al 95% de confianza con el modelo seleccionado con todas las 
#' variables fijadas en su valor medio.

new.dat <- data.frame(T_Sandburg = mean(T_Sandburg), Humedad = mean(Humedad), 
                      T_ElMonte = mean(T_ElMonte), Mes = mean(Mes),
                      Pres_Alt = mean(Pres_Alt), Inv_Alt_b = mean(Inv_Alt_b)) # tiene que aparecer valores de las vbles que están en el modelo.
predict(ajuste, newdata = new.dat, interval="confidence", level = 0.95)
predict(ajuste, newdata = new.dat, interval="prediction", level = 0.95)
#' 
#' $\newline$
#' 
#' # **Regresión Logística**
#' 
#' - Antes de empezar, cargamos los datos *Oro.rda*
load("Datos/Oro.rda")
attach(Oro)
explicativas.oro <- Oro[,1:3]    # Almacenamos las explicativas
respuesta.oro <- Proximidad      # Almacenamos la variable de respuesta
#'
#'
#' ## **1.** Análisis descriptivo
#' Para el análisis descriptivo de las variables podemos comenzar con una visión
#' general de las variables mediante las funciones `str()` y `summary()`.
str(Oro)
#' La salida de `str()` nos dice que los datos constan de 64 observaciones de 4 variables:
#' 
#' - `As`: Nivel de concentración de arsénico en la muestra de agua. (numérica)
#' - `Sb`: Nivel de concentración de antimonio en la muestra de agua. (numérica)
#' - `Corredor`: Variable binaria indicando si la zona muestreada está (1) o no
#'   está (0) en alguno de los corredores delimitados por las lineas sobre el
#'   mapa. (categórica)
#' 
#' Más la variable de respuesta `Proximidad`, que toma los valores 1 o 0 según
#' que el depósito esté próximo o esté muy lejano al lugar.
summary(Oro)
plot(explicativas.oro, pch=18,
     main="Representación por parejas de las explicativas")

boxplot(explicativas.oro, horizontal=T, pch=5,
        main="Diagrama de cajas de las explicativas")

old.par <- par(mfrow=c(1,2))
hist(As, main="Concentración de Arsénico")
hist(Sb, main="Concentración de Antimonio")
#' \newpage
par(old.par)
hist(Corredor, main="Histograma de la variable Corredor")
#' 
#' ## **2.** Modelo matemático
#' Dado que la variable de respuesta, `Proximidad`, es binaria (0 o 1),
#' deberemos de elegir un modelo que tenga esto en cuenta. En nuestro caso hemos
#' elegido una transformación del modelo lineal, definida por la distribución
#' logística de la ecuación \ref{logdistribution}
#' 
#' \begin{equation} \label{logdistribution}
#' F(z) = \frac{e^{z}}{1 + e^{z}} = \frac{1}{1 + e^{-z}}
#' \end{equation}
#' 
#' Por tanto, nuestro modelo logístico quedaría de la forma
#' 
#' \begin{equation} \label{p_i}
#' \mathbb{E}(Y|\vec{X_i}) = p_i = \mathbb{P}(Y = 1|\vec{X_i}) =
#' \frac{1}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' tal que $\eta = \vec\beta^t\vec{X_i}$ . Además,
#' 
#' \begin{equation} \label{1-p_i}
#' 1 - p_i = \mathbb{P}(Y = 0|\vec{X_i}) = 
#' 1 - \frac{1}{1 + e^{-\eta}} = 
#' \frac{e^{-\eta}}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' 
#' \newpage
#' ## **3.** Interpretación del modelo
#' Para una mejor interpretación del modelo, podemos definir el **odds**$_i$ de
#' manera que 
#' 
#' \begin{equation} \label{odds}
#' odds_i = odds(Y|\vec{X_i}) = \frac{p_i}{1 - p_i} =
#' e^{\eta} = e^{\vec\beta^t\vec{X_i}} =
#' e^{\beta_0}e^{\beta_1 X_{i1}} \cdots\: e^{\beta_k X_{ik}} =
#' e^{\beta_0}\prod_{j=1}^{k}e^{\beta_j X_{ij}} \enspace,\enspace {1}\leq{i}\leq{n}
#' \end{equation}
#' 
#' Este es un modelo multiplicativo, en el cual $e^{\beta_0}$ es la respuesta
#' cuando $\vec{X_i} = \vec{0}$, mientras que $e^{\beta_j}$, para $1 \leq j \leq k$, es el
#' incremento multiplicativo $(e^{\beta_j})^l$ en el odds para algún incremento
#' $l$ en $X_j$
#' 
#' También podemos expresar el modelo aplicando logaritmos a la ecuación
#' \ref{odds}, de manera que
#' 
#' \begin{equation} \label{logit}
#' \ln(\frac{p_i}{1 - p_i}) = \eta = \vec\beta^t\vec{X_i}
#' \end{equation}
#' 
#' Los cuales denominaremos como **logit**$_i$. Estos logits son interpretables
#' mucho más fácilmente, aunque debido a que
#' 
#' 
#' ## **4.** Inferencia
#' 
#' 
#' 
#' ## **5.** Bondad del ajuste
#' 
#' 