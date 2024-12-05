### Inicio y enunciado ----
#' **Ejercicio 8 de los propuestos**: El archivo *Oro.rda* contiene 64 registros
#' de un estudio diseñado para predecir la probabilidad de que exista o no un
#' depósito de oro en una determinada localización geográfica. Los datos
#' proceden del análisis químico de muestras de agua subterránea sobre lugares
#' pre-establecidos mediante lineas dibujadas sobre un mapa de la localización
#' en estudio.
#' 
#' - As: Nivel de concentración de arsénico en la muestra de agua.
#' - Sb: Nivel de concentración de antimonio en la muestra de agua.
#' - Corredor: Variable binaria indicando si la zona muestreada está (1) o no
#'   está (0) en alguno de los corredores delimitados por las lineas sobre el
#'   mapa.
#' 
#' La variable respuesta fue Proximidad a un depósito de oro, y toma los valores
#' 1 o 0 según que el depósito esté próximo o esté muy lejano al lugar.


### Carga y fijado de los datos del Oro ----
setwd("~/Escritorio/MR/")
load("Datos/Oro.rda")
attach(Oro)


### Primer análisis descriptivo ----
explicativas.oro <- Oro[,1:3]    # Seleccionamos las explicativas
respuesta.oro <- Proximidad # Seleccionamos la variable de respuesta

str(Oro)
summary(Oro)
plot(explicativas.oro, pch=18,
     main="Representación por parejas de las explicativas")

boxplot(explicativas.oro, horizontal=T, pch=5,
        main="Diagrama de cajas de las explicativas")

old.par <- par(mfrow=c(1,2))
hist(As, main="Histograma de la concentración de Arsénico")
hist(Sb, main="Histograma de la concentración de Antimonio")
par(old.par)

hist(Corredor, main="Histograma de Corredor")


# Modelo matemático ----
#' $\mathbb{E}(Y|\vec{X})$

