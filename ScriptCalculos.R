load("datos.RData")

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

# Mes --------------------------------------------------------------------------
summary(Mes)

cat("sd = ",sd(Mes),"\n")
cat("IQR = ",IQR(Mes))

library(moments)   # Para evaluar asimetría y kurtosis
cat("asimetría = ",skewness(Richness, na.rm = FALSE),"\n")
cat("kurtosis = ",kurtosis(Richness, na.rm = FALSE),"\n") #Al ser elevado puede tener picos

boxplot.stats(Richness)$out  # Registros atípicos (no quiere decir que sea preocupante)

par(mfrow=c(1,3)) #Dividir los paneles para las salidas 

# Histograma y ajuste normal con hist()+curve() y dnorm()

#freq=F es la frecuencia relativa y breaks son las barras que va a tener 
hist(Richness, breaks=5,freq=FALSE, main = "", xlab="Richness",
     cex.lab=1.4, ylab = "Densidad Richness", col = "lightblue") #Nos enseña que hay asimetría 

#Pintanos una normal por encima (add=T) para ver si puede venir de una normal
curve( dnorm(x,mean=mean(Richness),sd=sd(Richness)), 
       col="magenta", lwd=3, add=TRUE) #No viene de una normal (ni siquiera se pudo dibujar entera)

etiquetas <- c("Histograma","Ajuste normal")
legend("topright",etiquetas, lwd=2, col=c("lightblue","magenta"), 
       lty=c(1,1), cex=1.5, inset=0.02, box.lty=0)

# Diagrama de cajas con boxplot()
boxplot(Richness, main = "", xlab="Richness",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
#Los atipicos son con x y la caja grande es el rango intercuartilico

# Ajuste no paramétrico (kernel) con density() y polygon()
plot(density(Richness, bw=1.75),main="",lwd=3,col="blue",
     ylab="Densidad Richness estimada", cex.lab=1.4)
#density(nombre variable, ancho de banda:una muy grande hace muy pocas modas y al inverso )
#Tiene implimentado ya un ancho automatico, conviene tunearlo pero ya es aceptable

polygon(density(Richness,bw=1.75), col="lightblue") #pinta por denbaj(función estética)

par(mfrow=c(1,1))

