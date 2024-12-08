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

### Primer análisis descriptivo ----
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


### Modelo matemático ----
#' \begin{equation} \label{logmodel}
#' \mathbb{E}[\vec{Y}|\boldsymbol{X}] = \frac{e^{\eta}}{1 + e^{\eta}} = \frac{1}{1 + e^{-\eta}}
#' \end{equation}
#' 
#' \begin{equation} \label{loglinear}
#' \eta = \vec\beta^t\boldsymbol{X}
#' \end{equation}
#' 

### Interpretación modelo ----
#' ...

### Inferencia ----
#'
ajuste <- glm(Proximidad~., data=Oro, family="binomial")
summary(ajuste)

### Bondad del ajuste ----
#'
