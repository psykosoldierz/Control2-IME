library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
library(ez)

# SE DEBEN CARGAR LOS DATOS 1.
datos1.1 <- read_excel("Escritorio/tarea 2 IME/tarea 2 IME /Tarea2.datos1.xls")
data1.1 <- datos1.1[2:5]

#Debemos determinar si alguna(s) de las versiones del algoritmo estudiado consigue clasificar mejor
#los problemas planteados. Como no hay un parametro de comparacion, (ej: media, varianza, etc.) 
#Se realiza el analisis de varianza entre las versiones del algoritmo, especificamente utilizaremos
#ANOVA(Metodo no parametrico). Para usar ANOVA para muestras independientes se debe considerar los
#siguientes supuestos:

#1)
# La variable dependiente debe medirse a nivel de intervalo.
# Se puede visualizar que los valores de esta medida varian entre 0,7 y 0,9 aproximadamente,
# por lo tanto, se cumple este supuesto.

#2)
# Las muestras se seleccionan independientemente.
#Segun lo mencionado en el enuciado, la muestras han sido seleccionadas independientemente.

#3)
# Verificar si los grupos provienen de una poblacion normal(se verificara graficamente)
Ag1 <- data1.1[[1]]
Ag2 <- data1.1[[2]]
Ag3 <- data1.1[[3]]
Ag4 <- data1.1[[4]]

              #Datos para verificar graficamente la normalidad de los grupos
media.muestra1 <- mean(Ag1)
desvest.muestra1 <- sd(Ag1)
d1 <- data.frame(Samples = Ag1)

media.muestra2 <- mean(Ag2)
desvest.muestra2 <- sd(Ag2)
d2 <- data.frame(Samples = Ag2)

media.muestra3 <- mean(Ag3)
desvest.muestra3 <- sd(Ag3)
d3 <- data.frame(Samples = Ag3)

media.muestra4 <- mean(Ag4)
desvest.muestra4 <- sd(Ag4)
d4 <- data.frame(Samples = Ag4)

            # Graficos grupo 1
p1.1 <- ggplot(d1, aes(x = Ag1))
p1.1 <- p1.1 + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p1.2 <- ggplot(d1, aes(sample = Ag1))
p1.2 <- p1.2 + stat_qq(colour = "#56B4E9")
p1.2 <- p1.2 + geom_abline(
  intercept = media.muestra1,
  slope = desvest.muestra1,
  colour = "red"
)

            # Graficos grupo 2
p2.1 <- ggplot(d2, aes(x = Ag2))
p2.1 <- p2.1 + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p2.2 <- ggplot(d2, aes(sample = Ag2))
p2.2 <- p2.2 + stat_qq(colour = "#56B4E9")
p2.2 <- p2.2 + geom_abline(
  intercept = media.muestra2,
  slope = desvest.muestra2,
  colour = "red"
)

            # Graficos grupo 3
p3.1 <- ggplot(d3, aes(x = Ag3))
p3.1 <- p3.1 + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p3.2 <- ggplot(d3, aes(sample = Ag3))
p3.2 <- p3.2 + stat_qq(colour = "#56B4E9")
p3.2 <- p3.2 + geom_abline(
  intercept = media.muestra3,
  slope = desvest.muestra3,
  colour = "red"
)

            # Graficos grupo 4
p4.1 <- ggplot(d4, aes(x = Ag4))
p4.1 <- p4.1 + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p4.2 <- ggplot(d4, aes(sample = Ag4))
p4.2 <- p4.2 + stat_qq(colour = "#56B4E9")
p4.2 <- p4.2 + geom_abline(
  intercept = media.muestra4,
  slope = desvest.muestra4,
  colour = "red"
)

p1 <- arrangeGrob(p1.1, p1.2,p2.1, p2.2,p3.1, p3.2,p4.1, p4.2, ncol = 4, nrow = 2)
grid.draw(p1)


#4.a)
# Tambien se debe verificar si los grupos tienen Varianzas aproximadamente 
#iguales iguales(Homocedasticidad).
# Una primera aproximación es comparar los grupos con una gráfico de
# cajas, y asi obtener una nocion de lo que podria arrojar la 
#verificacion de las varianzas. Especificamente si son iguales o no.
#En el test ezAnova se continua el analisis de Homocedasticidad.

versiones.algoritmo <- c("AGv1", "AGv2", "AGv3", "AGv4")

idvar <- "Instancia"
timevar <- "Version.algoritmo"
vname <- "Proporcion.correcta"
varying <- versiones.algoritmo

datos.long <- reshape(
  data = data1.1,
  idvar = idvar,
  timevar = timevar,
  varying = varying,
  v.names = vname,
  direction = "long"
)


# 4.b)
datos.long[[idvar]] <- factor(1:nrow(datos.long))
datos.long[[timevar]] <- factor(rep(versiones.algoritmo, each = 24))
rownames(datos.long) <- NULL
#View(datos.long)


p <- ggplot(datos.long, aes(x = Version.algoritmo, y = Proporcion.correcta,
                            fill = Version.algoritmo))
p <- p + geom_boxplot()
p <- p + theme(legend.position = "none")
print(p)

# En ez.aov se obtienen los resultados del test que mide la Homocedasticidad. En el caso de que no se 
# cumpla. se debe recurrir al test de Kruskal-Wallis.
# Homocedasticidad: 
#                  H0: SS1 = SS2 = SS3 = SS4 ;(las varianzas son aproximadamente iguales)
#                  Ha: existe una SS distinta a las otras.
ez.aov <- ezANOVA(
  data = datos.long, 
  dv = Proporcion.correcta,
  wid = Instancia,
  between = Version.algoritmo,
  type = 3,
  return_aov = TRUE
)
print(ez.aov)
#plot(ez.aov$aov)

#)

# Podemos el el resultado gráficamente.
ezp <- ezPlot(
  data = datos.long,
  dv = Proporcion.correcta,
  wid = Instancia,
  between = Version.algoritmo,
  type = 3,
  x = Version.algoritmo
)
print(ezp)


