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
#' Ozono_i = 55.428 - 0.343*Mes_i* + 0.012*Diames_i* - 0.047*DiaSemana_i* - 0.0133*Pres_Alt_i*
#' - 0.096*Vel_Viento_i* + 0.088*Humedad_i* + 0.1366*T_Sandburg_i* + 0.5598*T_ElMonte_i* 
#' - 0.0006*Inv_Alt_b_i* + 0.0004*Grad_Pres_i* - 0.124*Inv_T_b_i* - 0.005*Visibilidad_i*
#' 
#' Suma de residuos al cuadrado media:
( MSSR <- summary(MOD_FULL)$sigma^2 )
#' Grados de libertad de los residuos:
( gl.R <- MOD_FULL$df )
#' Número de parámetros:
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
#' Para ello, utilizaremos la librería "mctest", que proporciona un análisis completo 
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
Mod_NULL <- lm(Ozono ~ 1, data = OzonoLA) 
#' Ahora, aplicaremos la siguiente función para obtener el modelo óptimo:
stepMod <- step(Mod_NULL, direction = "both", trace = 1,
                scope = list(lower = Mod_NULL, 
                             upper = MOD_FULL) ) 
summary((stepMod))
#' El modelo resultante de la selección secuencial es:
#' Ozono_i = 51.3444845 - 0.3324536*Mes_i* - 0.0134013 *Pres_Alt_i*
#' + 0.0975694*Humedad_i* + 0.1242673*T_Sandburg_i* + 0.4743962*T_ElMonte_i* 
#' - 0.0003211*Inv_Alt_b_i*
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
#'
#' ## **6.** Posible Interacción
#' 
#' Debido a la posible necesidad de interacción, decidimos probar si un modelo
#' que incluya interacción es mejor que nuestro modelo completo.
#' 
#' Comenzamos definiendo este modelo, con todas las interacciones posibles:
ajuste.i <- update(ajuste_completo,.~.^3, family=binomial, data=Oro)
summary(ajuste.i)
#' Ningún coeficiente es significativo, por lo que consideramos que esto se 
#' puede deber a la presencia de multicolinealidad debido a las interacciones.
#' 
#' Decidimos hacer una selección de variables, por si alguna interacción
#' entre variables originales resultase significativa. La haremos igual
#' que en el apartado anterior: 
step(M0, direction="forward", trace=1,
     scope = list(lower=M0,upper=ajuste.i))
#' Finalmente, vemos que en este caso, la interacción de las variables
#' no aporta nada a nuestro ajuste.
#' 
#' 
#' ## **7.** Inferencia modelo 
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
#'
#' ## **8.** Validación modelo seleccionado 
#' 
#' Por abreviar la notación, tenemos:
MS <- ajuste  # Ajuste modelo elegido.
MC <- MOD_FULL # Ajuste modelo completo

#' Primero, calculamos el coeficiente de robusted del ajuste:
library(DAAG)
( B2 <- sum(residuals(MS)^2)/press(MS) )
#' Elevado y superior al del modelo completo
sum(residuals(MC)^2)/press(MC) 
#'
#' Haremos una validación del tipo LOOCV (Leave One Out Cross Validation):
#'
#' Primero, para MS:
class(OzonoLA) # ya es un data frame
set.seed(5198) 
cv_k3_MS <- cv.lm(data=OzonoLA,form.lm= formula(MS),m=length(OzonoLA))  
#' Se calcula la raíz cuadrada de la media de los cuadrados de las diferencias entre predicciones y observaciones:
errores <- cv_k3_MS$cvpred - cv_k3_MS$Ozono # predicho por cv - predicción real
( error_cv_k3_MS <- sqrt(mean(errores^2)) ) # estimador RMSE (raiz media suma residuos al cuadrado)
#'
#' Finalmente, para MC:
set.seed(5198) 
cv_k3_MC <- cv.lm(data=OzonoLA,form.lm=formula(MC),m=length(OzonoLA))   
errores <- cv_k3_MC$cvpred - cv_k3_MC$Ozono
( error_cv_k3_MC <- sqrt(mean(errores^2)) )
par(mfrow=c(1,1)) 
#'
#' Obtenemos un comportamiento mejor con el MS que con MC, pues tenemos un menor error.
#' 
#' ## **9.** Análisis de residuos modelo seleccionado 
#' Para realizar el análisis de los residuos usaremos los residuos estandarizados
library(MASS)
res.est  <- stdres(ajuste) 
#' - Linealidad:
scatter.smooth(ajuste$fit, res.est, main="Residuos ~ Ajustados", 
               xlab="Ajustados",ylab="Residuos", pch = 21, 
               bg = "green", cex.lab=1.5, cex=1.4, cex.main=1.5, 
               lpars = list(col = "magenta", lwd = 3) )
abline(h=0,lty=2,lwd=2)
#' Como podemos observar en el gráfico no sería correcto afirmar linealidad
#' 
#' -Aleatoriedad:
acf(res.est, lag.max = 10, type = "correlation")$acf  
#' Como podemos ver en la matriz de correlaciones, las correlaciones entre un dato
#' y el anterior son muy bajas, con lo cual, si sería correcto asimir aleatoriedad,
#' 
#' - Normalidad:
par(mfrow=c(1,3))

hist(res.est, breaks=6,freq=FALSE, main = "", xlab="Residuos", cex.lab=1.4, 
     ylab = "Densidad", col = "lightblue", ylim=c(0,0.6))
curve( dnorm(x), col="magenta", lwd=3, add=TRUE)
etiquetas <- c("Histograma","Ajuste normal")
legend("topright",etiquetas, lwd=2, col=c("lightblue","magenta"), 
       lty=c(1,1), cex=1.3, inset=0.02, box.lty=0)

boxplot(res.est, main = "", xlab="Residuos",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)

plot(density(res.est, bw=0.4),main="",lwd=3,col="blue",
     ylab="Densidad stimada residuos", cex.lab=1.4, cex.lab=1.4)
polygon(density(res.est,bw=0.4), col="lightblue")
curve( dnorm(x), col="magenta", lwd=3, add=TRUE)
par(mfrow=c(1,1))
#' 
#' Gráficamente podemos deducir que nuestros datos no siguen exactamente una distribución
#' normal pero si muy semejante.
#' 
library(nortest)
lillie.test(res.est)  
cvm.test(res.est)    
ad.test(res.est)     
shapiro.test(res.est)
#'
#'Analíticamente se confirma la teoría anterior ya que los p-vlores, en todos los 
#'tests nos dan lo suficientemente grandes cómo para no rechazar la hipótesis nula,
#'la cual se refiere a la normalidad.
#'
#'- Homoscedasticidad:
#'
#' H0: sigma^2=cte  vs H1: sigma^2!=cte
#' Test de Breusch-Pagan
library(lmtest)
bptest(ajuste) 
plot(ajuste, which=3)
#'
#'Tanto con el test de Breusch-Pagan cómo con el gráfico de los residuos podemos 
#'concluir que se rechaza la hipótesis nula de homoscedasticidad
#'
#' ## **10.** Análisis de influencia modelo seleccionado
#' 
influencia <- influence(ajuste)
#' 
#' - Leverages:
( lev <- influencia$hat )
plot(lev, xlab = "Indice en la muestra", ylab = "Leverage", 
     cex = 1.2, pch=19, col=4, cex.lab=1.4)
#' 
#' - Distancias de Cook:
cooks.distance(ajuste)
plot(ajuste,which=4)
#' 
#' - DFFITs
DFFITs <- dffits(ajuste)
#' 
#' - DFBETAs
DFBETAS <- dfbeta(ajuste) 
par(mfrow = c(2,1), pch=19, col=1, cex.lab = 1.5, cex.axis = 1.5)
plot(dfbeta(ajuste)[,1],xlab = "Indice omitido", 
     ylab = expression(DFBETA[0]),col=4)
plot(dfbeta(ajuste)[,2], xlab = "Indice omitido", col=4,
     ylab = expression(DFBETA[1]))
par(mfrow = c(1,1))
#' 
#' 
#' ## **11.** Estimación media condicionada y predicción 
#' Finalmente, obtengamos el intervalo de confianza y de predicción para el nivel 
#' de ozono medio al 95% de confianza con el modelo seleccionado para cada mes con 
#' todas las demás variables fijadas en su valor medio .
#' 
new.dat <- data.frame(T_Sandburg = mean(T_Sandburg), Humedad = mean(Humedad), 
                      T_ElMonte = mean(T_ElMonte), Mes = c(1:12),
                      Pres_Alt = mean(Pres_Alt), Inv_Alt_b = mean(Inv_Alt_b)) 
predict(ajuste, newdata = new.dat, interval="confidence", level = 0.95)
predict(ajuste, newdata = new.dat, interval="prediction", level = 0.95)
#' 
rm(list = ls())
#'
#'
#' \newpage
#' # **Regresión Logística**
#' 
#' - Antes de empezar, cargamos los datos *Oro.rda*
load("Datos/Oro.rda")
#'
#'
#' ## **1.** Análisis descriptivo
#' Para el análisis descriptivo de las variables podemos comenzar con una visión
#' general de las variables mediante las funciones `str()` y `summary()`.
str(Oro)
#' 
#' La salida de `str()` nos dice que los datos constan de 64 observaciones de 4 variables:
#' 
#' - `As`: Nivel de concentración de arsénico en la muestra de agua. (numérica)
#' - `Sb`: Nivel de concentración de antimonio en la muestra de agua. (numérica)
#' - `Corredor`: Variable binaria indicando si la zona muestreada está (1) o no
#'   está (0) en alguno de los corredores delimitados por las lineas sobre el
#'   mapa. (categórica)
#' - `Proximidad` : Variable de respuesta que toma los valores 1 o 0 según que
#'   el depósito esté próximo o esté muy lejano al lugar.
#' 
attach(Oro)
Oro$Corredor <- as.factor(Oro$Corredor) # Convertimos la variable Corredor a factor
numericas.oro <- Oro[1:2]               # Almacenamos las variables numéricas
respuesta.oro <- Proximidad             # Almacenamos la variable de respuesta
#' 
#' Con la salida de `summary()` y graficando `As` frente a `Sb` podemos ver que,
#' basándonos en la diferencia entre las medias y las medianas, las variables
#' numéricas se concentran en valores bajos, aunque deben de existir registros
#' con valores relativamente altos:
summary(Oro)
#' \newpage
plot(numericas.oro, pch=18,
     main="Representación de la variables As y Sb")
#' 
#' Este hecho se confirma también al mirar los histogramas y diagramas de cajas:
old.par <- par(mfrow=c(1,2))
hist(As, freq=F, xlab="As", ylab = "Densidad",
     main="Concentración de Arsénico")
curve(dnorm(x,mean=mean(As), sd=sd(As)), 
      col="blue", lwd=3, add=TRUE)

hist(Sb, freq=F, xlab="Sb", ylab = "Densidad",
     main="Concentración de Antimonio")
curve(dnorm(x,mean=mean(Sb), sd=sd(Sb)), 
      col="blue", lwd=3, add=TRUE)
par(old.par)

boxplot(numericas.oro, horizontal=T, pch=5,
        main="Diagrama de cajas de las variables numéricas")
#' 
#' Distribución de la variable `Proximidad`:
table(Proximidad); table(Proximidad)/nrow(Oro)
#' 
#' Distribución de la variable `Corredor`:
table(Corredor)
#' 
#' Observamos que si los datos se encuentran en alguno de los corredores, suelen
#' estar próximos a un depósito de oro y lejanos si no es así:
xtabs(~Proximidad + Corredor, data=Oro)
#' 
#' 
#' ## **2.** Modelo matemático
#' Dado que contamos con una muestra de n realizaciones $(\vec{X}^t,Y)$ con
#' $\vec{X}^t = (X_1, \ldots, X_k)$ que asumimos independientes, y que la variable
#' respuesta, `Proximidad`, es binaria (0 o 1), debemos de elegir un modelo que
#' tenga esto en cuenta. En nuestro caso hemos elegido una transformación del
#' modelo lineal, definida por la distribución logística de la ecuación
#' \ref{logdistribution}.
#' 
#' \begin{equation} \label{logdistribution}
#' F(z) = \frac{e^{z}}{1 + e^{z}} = \frac{1}{1 + e^{-z}}
#' \end{equation}
#' 
#' Por tanto, nuestro modelo logístico quedaría de la forma
#' 
#' \begin{equation} \label{p_i}
#' Y|(\vec{X}=\vec{X_i}) \sim{Be(p_i)}, \quad p_i = \mathbb{P}(Y = 1|\vec{X_i}) =
#' \frac{1}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' Tal que
#' 
#' \begin{equation} \label{eta}
#' \eta = \beta_0 + \beta_1 As + \beta_2 Sb + \tau I(Corredor=1)
#' \end{equation}
#' 
#' siendo $I(Corredor = 1)$ la variable indicadora para cuando Corredor toma el
#' valor 1. Además,
#' 
#' \begin{equation} \label{1-p_i}
#' 1 - p_i = \mathbb{P}(Y = 0|\vec{X_i}) = 
#' 1 - \frac{1}{1 + e^{-\eta}} = 
#' \frac{e^{-\eta}}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' 
#' ## **3.** Interpretación del modelo
#' Para una mejor interpretación del modelo, podemos definir el **odds**$_i$ de
#' manera que 
#' 
#' \begin{equation} \label{odds}
#' odds_i = odds(Y|\vec{X_i}) = \frac{p_i}{1 - p_i} =
#' e^{\eta} = e^{\vec\beta^t\vec{X_i}} =
#' e^{\beta_0}e^{\beta_1 X_{i1}} \cdots\: e^{\beta_k X_{ik}}
#' \enspace,\enspace {1}\leq{i}\leq{n}
#' \end{equation}
#' 
#' Este es un modelo multiplicativo, en el cual $e^{\beta_0}$ es la respuesta
#' cuando $\vec{X_i} = \vec{0}$, mientras que $e^{\beta_j}$, para $1 \leq j \leq k$, es el
#' incremento multiplicativo $(e^{\beta_j})^l$ en el odds para algún incremento
#' $l$ en $X_j$
#' 
#' Si resulta que existe una variable binaria podemos utilizar el **odds-ratio**,
#' que indica en qué medida el suceso $Y = 1$ es más posible que $Y = 0$ si
#' $X = 1$ que si $X = 0$:
#' \begin{equation} \label{OR}
#' OR = \frac{\mathbb{P}(Y = 1 | X = 1) / \mathbb{P}(Y = 0 | X = 1)}
#'           {\mathbb{P}(Y = 1 | X = 0) / \mathbb{P}(Y = 0 | X = 0)} = 
#'      \frac{e^{\beta_0 + \beta_1}}{e^{\beta_0}}
#' \end{equation}
#' 
#' Si $X$ es cualitativa podemos seguir aplicando el *OR* con $g - 1$ variables
#' *dummy*, siendo $g$ el número de categorías.
#' 
#' También podemos expresar el modelo aplicando logaritmos a la ecuación
#' \ref{odds}, de manera que
#' 
#' \begin{equation} \label{logit}
#' \ln(\frac{p_i}{1 - p_i}) = \eta = \vec\beta^t\vec{X_i}
#' \end{equation}
#' 
#' Los cuales denominaremos como **logit**$_i$. Estos logits son interpretables
#' mucho más fácilmente ya que son interpretables linealmente.
#' 
#' Finalmente, por lo comentado en el apartado del modelo matemático y en este, este
#' modelo sigue las tres siguientes hipótesis estructurales:
#' 
#' 1. Linealidad de los logits.
#' 2. Respuesta binaria de la $Y$.
#' 3. Independencia de las observaciones.
#' 
#' 
#' ## **4.** Análisis de multicolinealidad
#' 
#' Debemos analizar si estamos ante un caso de multicolinealidad. Si así fuera, 
#' las estimaciones de los parámetros no serían correctos, y nuestro modelo 
#' solo serviría para predecir, no para explicar el comportamiento de la respuesta.
#' 
#' Utilizaremos los factores de inflacción de la varianza generalizada, para
#' ver si nos encontramos con variables correlacionadas:
ajuste_completo <- glm(Proximidad~., data = Oro, family = "binomial")
library(car)
vif(ajuste_completo)
#' Los factores de inflacción de la varianza son todos menores que 10,
#' por lo que no estamos ante un caso de multicolinealidad.
#' 
#' ## **5.** Selección del modelo
#' 
#' A pesar de no tener multicolinealidad en los datos, decidimos hacer una selección
#' de variables, debido a la no significación de todas las variables.
#' 
#' Para ello, decidimos utilizar un método de selección exhaustiva con el BIC,
#' ya que esta medida de selección de modelos 'castiga' a modelos con un número
#' elevado de variables:
library(bestglm)
help(bestglm)
M1.exh.AIC <- bestglm(Oro, IC = "BIC", family = binomial, 
                      method = "exhaustive")
M1.exh.AIC$Subsets
# La fila con el asterisco indica el modelo seleccionado.
# Aquí el modelo es el modelo sin corredor.
# Esto también nos lo indicaba el p-valor inicial.
#' 
#' 
#' Por lo tanto, definimos el ajuste sin corredor y vemos la significación
#' del resto de las variables: 
ajuste_sin_corredor <- update(ajuste_completo,.~.-Corredor)
summary(ajuste_sin_corredor)
#' 
#' ## **6.** Posible Interacción
#' 
#' Debido a la posible necesidad de interacción, decidimos probar si un modelo
#' que incluya interacción es mejor que nuestro modelo completo.
#' 
#' Comenzamos definiendo este modelo, con todas las interacciones posibles:
ajuste.i <- update(ajuste_completo,.~.^3, family=binomial, data=Oro)
summary(ajuste.i)
#' Ningún coeficiente es significativo, por lo que consideramos que esto se 
#' puede deber a la presencia de multicolinealidad debido a las interacciones.
#' 
#' Decidimos hacer una selección de variables, por si alguna interacción
#' entre variables originales resultase significativa. La haremos igual
#' que en el apartado anterior: 
step(M0, direction="forward", trace=1,
     scope = list(lower=M0,upper=ajuste.i))
#' Finalmente, vemos que en este caso, la interacción de las variables
#' no aporta nada a nuestro ajuste.
#' 
#' 
#' ## **7.** Inferencia
#' 
ajuste <- ajuste_sin_corredor
summary(ajuste)
#' 
#' Teniendo en cuenta la ecuación \ref{logit}, los coeficientes ajustados y las
#' variables significativas, el modelo quedaría como en la equación \ref{adjmodel}
#' 
#' \begin{equation} \label{adjmodel}
#' ln( \frac {\hat{p}} {1-\hat{p}} ) = \hat\eta = -4.9664 + 1.2490 As + 0.9235 Sb 
#' \end{equation}
#' 
#' Empezamos la inferencia haciendo los intervalos de confianza para los parámetros.
#'  Haremos los intervalos basados en las sd de las pruebas de Wald y en los cuantiles de una normal:
confint.default(ajuste)
#'
#'INTERPRETAR EN CASA
#' 
#' ## **8.** Estimación media y probabilidad condicionada
#' 
#' Haremos los intervalos de confianza y de probabilidad manteniendo las dos 
#' variables en su media:
#' 
new <- with(Oro, data.frame(As = mean(As), Sb = mean(Sb)))
#' 
#' Utilizamos predict para la predicción estimada:
p_est_proximidad <- predict(ajuste, newdata = new, 
                          type = "response")
cbind(new,p_est_proximidad)
#'
#' Para obtener los intervalos de confianza para estas predicciones, utilizaremos 
#' la siguiente función proporcionada en el Script de R Logísitica:
#' 
est.media.cond.CI <- function(ajuste, newdata, level = 0.95){
  # Predicciones de los logit 
  pred <- predict(object = ajuste, newdata = newdata, se.fit = TRUE)
  # CI para los logits 
  za <- qnorm(p = (1 - level) / 2)
  lwr <- pred$fit + za * pred$se.fit
  upr <- pred$fit - za * pred$se.fit
  # Back-transformada a probabilidades 
  fit <- 1 / (1 + exp(-pred$fit))
  lwr <- 1 / (1 + exp(-lwr))
  upr <- 1 / (1 + exp(-upr))
  # Acomodamos en una matriz la salida
  result <- cbind(fit, lwr, upr)
  colnames(result) <- c("p", "LI", "LS")
  return(result)
}
#' La aplicamos del siguiente modo:
est.media.cond.CI(ajuste, newdata = new)
#'
#'
#' ## **9.** Bondad del ajuste
#' 
#'
#'## **10.** Análisis de residuos
#'
#' El modelo de regresión logísitica tiene 3 hipótesis estructurales:
#' 1) La linealidad de los Logits.
#' 2) La independencia de las n observaciones.
#' 3) La respuesta Y debe ser binaria.
#' 
#' Tal y como sucede en regresión lineal, podemos utilizar los residuos para chequear 
#' las hipótesis estructurales. No obstante, debemos tener en cuenta que en regresión 
#' logística existen dos tipos de residuos, con fines distintos.
#' 
#' Obtención residuos de Pearson:
res.p <- residuals(ajuste, type="pearson")
#' Obtención residuos de la Deviance:
res.d <- residuals(ajuste, type="deviance")
#'
#' Los estandarizamos:
#' Residuos Pearson estandarizados:
res.p.e <- res.p/sqrt(1 - hatvalues(ajuste)) 
#' Residuos deviance estandarizados:
res.d.e <- res.d/sqrt(1 - hatvalues(ajuste)) 
#'
#' Obtenemos los gráficos de residuos:
plot(ajuste)
#' La función plot de R enfrenta los residuos estandarizados de Pearson con los
#' logits del ajuste. Este tipo de residuo es útil simplemente para chequear la 
#' normalidad que, en este caso, evidentemente no está presente, como se aprecia 
#' en el segundo gráfico de la salida.
#' 
#' Para chequear la linealidad, se utilizan los residuos del segundo tipo, es decir, 
#' los de la deviance, del siguiente modo:
car::avPlots(ajuste,terms=~.) 
#' Podemos ver que prácticamente los datos están en torno a 0.
#' El problema con las rectas es que, debido a la presencia de muchos 
#' datos entre 0 y 10, su pendiente varía mucho.
#' ACABAR INTERPRETACIÓN
#' 
#' También podemos hacer gráficos de residuos parciales, para ver si la falta de 
#' linealidad es achacable a alguna variable concreta:
library(car)
crPlots(ajuste)
#' 
#' 
#'## **11.** Análisis de influencia
#'
#' Finalmente, los residuos de Pearson también se pueden utilizar para el 
#' análisis de influencia.
#' 
#' Para ver el gráfico de la distancia de Cook, se ejecuta el siguiente comando:
plot(ajuste, which = 4)
#' Vemos 3 observaciones con una distancia de Cook mayor que el resto
#' de observaciones: {34, 39, 47}
#'
#' Tal y como haciamos en regresión lineal múltiple, podemos utilizar la siguiente
#' función de R para obtener las medidas del análisis de influencia automáticamente:
im <- influence.measures(ajuste)
summary(im)
#' Nos centramos en las columnas "cook.d", "hat" y "dffit":
#' 
#' Con respecto a los leverages de Pregibon, vemos que las observaciones
#' {9, 39, 52} parecen influyentes.
#' 
#' Con respecto a la distancia de Cook, vemos que las observaciones