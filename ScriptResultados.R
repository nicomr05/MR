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
#' ## **1.** Análisis descriptivo
#' 
#' ## **2.** Modelo matemático
#' \begin{equation} \label{linearmodel}
#' \mathbb{E}(\vec{Y}|\boldsymbol{X}) = \beta_0 + \sum_{i=1}^{n}\beta_iX_{ij}
#' \end{equation}
#'
#' 
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