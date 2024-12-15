### Inicio y enunciado ----
#' **Ejercicio 8 de los propuestos**: El archivo *Oro.rda* contiene 64 registros
#' de un estudio diseñado para predecir la probabilidad de que exista o no un
#' depósito de oro en una determinada localización geográfica. Los datos
#' proceden del análisis químico de muestras de agua subterránea sobre lugares
#' pre-establecidos mediante lineas dibujadas sobre un mapa de la localización
#' en estudio.
#' 
#' - `As`: Nivel de concentración de arsénico en la muestra de agua.
#' - `Sb`: Nivel de concentración de antimonio en la muestra de agua.
#' - `Corredor`: Variable binaria indicando si la zona muestreada está (1) o no
#'   está (0) en alguno de los corredores delimitados por las lineas sobre el
#'   mapa.
#' 
#' La variable respuesta fue la `Proximidad` a un depósito de oro, y toma los
#' valores 1 o 0 según que el depósito esté próximo o esté muy lejano al lugar.


### Carga y fijado de los datos del Oro ----
setwd("~/Escritorio/MR/")
load("Datos/Oro.rda")
attach(Oro)

### 1. Análisis descriptivo ----
head(Oro)
Oro$Corredor <- as.factor(Oro$Corredor) # Convertimos la variable Corredor a factor

str(Oro)
summary(Oro)

numericas.oro <- Oro[,1:2]    # Seleccionamos las variables numéricass
respuesta.oro <- Proximidad      # Seleccionamos la variable de respuesta

plot(numericas.oro, pch=18,
     main="Representación de la variables As y Sb")

boxplot(numericas.oro, horizontal=T, pch=5,
        main="Diagrama de cajas de las variables numéricas")

table(Proximidad); table(Proximidad)/nrow(Oro) # Distribución de la variable de Proximidad

table(Corredor)

xtabs(~Proximidad + Corredor, data=Oro) # Observamos que si los datos se
# encuentran en alguno de los corredores, suelen estar próximos a un depósito de
# oro y lejanos si no es así

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


### 2. Modelo matemático ----
#' \begin{equation} \label{logmodel}
#' \mathbb{E}[\vec{Y}|\boldsymbol{X}] = \frac{e^{\eta}}{1 + e^{\eta}} = \frac{1}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' \begin{equation} \label{loglinear}
#' \eta = \vec\beta^t\boldsymbol{X}
#' \end{equation}
#' 

### 3. Interpretación modelo ----
#' ...

### 4. Análisis de multicolinealidad ----
#' Debemos analizar si estamos ante un caso de multicolinealidad. Si así fuera, 
#' las estimaciones de los parámetros no serían correctos, y nuestro modelo 
#' solo serviría para predecir, no para explicar el comportamiento de la respuesta.
#' 
#' Utilizaremos los factores de inflacción de la varianza generalizada, para
#' ver si nos encontramos con variables correlacionadas:
library(car)
vif(ajuste_completo)
#' COMPLETAR. Los factores de inflacción de la varianza son todos menores que 10,
#' lo que nos indican que no estamos ante un caso claro de multicolinealidad.
#' 
### 5. Selección del modelo ----
#' A pesar de que no hay aparentemente multicolinealidad o un número elevado de variables,
#' decidimos hacer una selección del modelo.
#' 
#' Tal y como hicimos en el ejercicio de regresión lineal, decidimos utilizar el 
#' método de selección secuencial STEPWISE:
#' 
#' Definimos el modelo con sólo el intercept:
M0 <- glm(Proximidad~1,family=binomial,data=Oro)
#' Aplicamos selección secuencial:
step(M0, direction="forward", trace=1,
     scope = list(lower=M0,upper=ajuste_completo))
#' Efectivamente, el modelo óptimo resultante es el modelo completo. Esto era predecible
#' debido al bajo número de variables.
#' 
### 6. Posible interacción ----
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
### 7. Inferencia ----
#'
ajuste <- glm(Proximidad~., data=Oro, family="binomial")
summary(ajuste)
#' A priori son significativos únicamente el intercept y el coeficiente de `As`
summary(ajuste)$coef

confint(ajuste) #
#'
### 8. Estimación media y probabilidad condicionada ----
#' 
#'
### 9. Bondad del ajuste ----
#' 
#'
### 10. Validación modelo  ----
#' Para validar el modelo, utilizaremos el método de LOOCV (Leave One Out Cross
#' Validation) con la siguiente función de la librería boot:
library(boot)
set.seed(10203)
class(Oro) # ya es un dataframe
( ECMP.cv <- cv.glm(Oro,ajuste,K=length(Oro))$delta[1] )
#' La salida $delta[1] proporciona el error cuadrático medio de predicción promediado 
#' sobre todas las ejecuciones por validación cruzada que coincide con (FN+FP)/n.
#' 
#' Así, podemos obtener la Tasa de Clasificación Correcta:
( TCC.cv <- 1-ECMP.cv )  
#' El porcentaje resultante es muy cercano a 1, por lo que estamos ante un modelo
#' bueno a la hora de clasificar.
#'
### 11. Análisis de residuos ----
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
# Residuos deviance estandarizados:
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
#' AYUDA INTERPRETACIÓN
#' 
#' También podemos hacer gráficos de residuos parciales, para ver si la falta de 
#' linealidad es achacable a alguna variable concreta:
library(car)
crPlots(ajuste)
#' AYUDA INTERPRETACIÓN
#' 
### 12. Análisis de influencia ----
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
#' INTERPRETAR SALIDA

