library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
library(ez)
# SE DEBEN CARGAR LOS DATOS 2.
datos1.2 <- read_excel("Escritorio/tarea 2 IME/tarea 2 IME /Tarea2-datos2.xls")
data1.2 <- datos1.2[2:5]

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
Ag1.b <- data1.2[[1]]
Ag2.b <- data1.2[[2]]
Ag3.b <- data1.2[[3]]
Ag4.b <- data1.2[[4]]

#Datos para verificar graficamente la normalidad de los grupos
media.muestra1.b <- mean(Ag1.b)
desvest.muestra1.b <- sd(Ag1.b)
d1.b <- data.frame(Samples = Ag1.b)

media.muestra2.b <- mean(Ag2.b)
desvest.muestra2.b <- sd(Ag2.b)
d2.b <- data.frame(Samples = Ag2.b)

media.muestra3.b <- mean(Ag3.b)
desvest.muestra3.b <- sd(Ag3.b)
d3.b <- data.frame(Samples = Ag3.b)

media.muestra4.b <- mean(Ag4.b)
desvest.muestra4.b <- sd(Ag4.b)
d4.b <- data.frame(Samples = Ag4.b)

# Graficos grupo 1
p1.1.b <- ggplot(d1, aes(x = Ag1.b))
p1.1.b <- p1.1.b + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p1.2.b <- ggplot(d1, aes(sample = Ag1.b))
p1.2.b <- p1.2.b + stat_qq(colour = "#56B4E9")
p1.2.b <- p1.2.b + geom_abline(
  intercept = media.muestra1.b,
  slope = desvest.muestra1.b,
  colour = "red"
)

# Graficos grupo 2
p2.1.b <- ggplot(d2.b, aes(x = Ag2.b))
p2.1.b <- p2.1.b + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p2.2.b <- ggplot(d2.b, aes(sample = Ag2.b))
p2.2.b <- p2.2.b + stat_qq(colour = "#56B4E9")
p2.2.b <- p2.2.b + geom_abline(
  intercept = media.muestra2.b,
  slope = desvest.muestra2.b,
  colour = "red"
)

# Graficos grupo 3
p3.1.b <- ggplot(d3.b, aes(x = Ag3.b))
p3.1.b <- p3.1.b + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p3.2.b <- ggplot(d3.b, aes(sample = Ag3.b))
p3.2.b <- p3.2.b + stat_qq(colour = "#56B4E9")
p3.2.b <- p3.2.b+ geom_abline(
  intercept = media.muestra3.b,
  slope = desvest.muestra3.b,
  colour = "red"
)

# Graficos grupo 4
p4.1.b <- ggplot(d4.b, aes(x = Ag4.b))
p4.1.b <- p4.1.b + geom_histogram(
  binwidth = 0.01,
  colour = "red",
  fill = "#56B4E9"
)

p4.2.b <- ggplot(d4.b, aes(sample = Ag4.b))
p4.2.b <- p4.2.b + stat_qq(colour = "#56B4E9")
p4.2.b <- p4.2.b + geom_abline(
  intercept = media.muestra4.b,
  slope = desvest.muestra4.b,
  colour = "red"
)

p1 <- arrangeGrob(p1.1.b, p1.2.b,p2.1.b, p2.2.b,p3.1.b, p3.2.b,p4.1.b, p4.2.b, ncol = 4, nrow = 2)
#grid.draw(p1)


#4.a)
# Tambien se debe verificar si los grupos tienen Varianzas aproximadamente 
#iguales iguales(Homocedasticidad).
# Una primera aproximación es comparar los grupos con una gráfico de
# cajas, y asi obtener una nocion de lo que podria arrojar la 
#verificacion de las varianzas. Especificamente si son iguales o no.
#En el test ezAnova se continua el analisis de Homocedasticidad.

versiones.algoritmo <- c("AGv1", "AGv2", "AGv3", "AGv4")
# 4.b)

idvar <- "Instancia"
timevar <- "Version.algoritmo"
vname <- "Proporcion.correcta"
varying <- versiones.algoritmo

datos.long.b <- reshape(
  data = data1.2,
  idvar = idvar,
  timevar = timevar,
  varying = varying,
  v.names = vname,
  direction = "long"
)


datos.long.b[[idvar]] <- factor(1:24)
datos.long.b[[timevar]] <- factor(rep(versiones.algoritmo, each = 24))
rownames(datos.long.b) <- NULL
View(datos.long.b)



p.b <- ggplot(datos.long.b, aes(x = Version.algoritmo, y = Proporcion.correcta,
                            fill = Version.algoritmo))
p.b <- p.b + geom_boxplot()
p.b <- p.b + theme(legend.position = "none")
#print(p.b)

# En ez.aov se obtienen los resultados del test "Mauchly's Test for Sphericity", el cual corrobora la Esfericidad.
# Si se rechaza la hipotesis nula se debe recurrir al test de Freidman.
# Esfericidad: 
#                  H0: Los grupos estan correlacionados.
#                  Ha: .
ez.aov <- ezANOVA(
  data = datos.long.b, 
  dv = Proporcion.correcta,
  wid = Instancia,
  within = Version.algoritmo,
  type = 3,
  return_aov = TRUE
)
#print(ez.aov)

# Podemos el el resultado gráficamente.
ezp <- ezPlot(
  data = datos.long.b,
  dv = Proporcion.correcta,
  wid = Instancia,
  within = Version.algoritmo,
  type = 3,
  x = Version.algoritmo
)
#print(ezp)



