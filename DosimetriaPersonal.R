##########################################################
## Universidad del Valle de Guatemala                   ##
## Tema:                                                ##
##          Análisis de datos circulares de DP          ##
## Autora:                                              ##
##          Elvia Odalis Reyes Guevara                  ## 
## Fecha:                                               ##
##          Octubre 2020                                ##
##########################################################


#----------------------------------------------------------------
#  Instalación de paquetes nuevos en caso no esten descargados  
#----------------------------------------------------------------
if(!require(readxl)) {install.packages("readxl")}
if(!require(circular)) {install.packages("circular")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(ggplot2)) {install.packages("ggplot2")}
if(!require(tseries)) {install.packages("tseries")}
if(!require(dygraphs)) {install.packages("dygraphs")}

#------------------------------
# Se importan las librerías 
#------------------------------
library("readxl") # para leer archivos excel
library("circular") # para hacer estadística circular
library('tidyverse') 
library("dplyr")
library('ggplot2') # para hacer gráficas bonitas
library('tseries') # series de tiempo 
library('dygraphs') # grafica interactiva ts
library('xts') # formato xts
library('plotly')

#---------------------
# Carga de archivos
#---------------------

# Directorio en donde se encuentran los datos
setwd("/Users/odalisrg/Downloads/DPDatos/Anual")

# Se cargan los archivos del 2010-2019 y se leen los datos
DP2010 <- read_excel("Reporte dosimetrico 2010.xls", sheet = 1, range =  cell_rows(10:26), col_names = TRUE, col_types = 'list')
DP2011 <- read_excel("Reporte dosimetrico 2011.xls", sheet = 1, range =  cell_rows(10:26), col_names = TRUE, col_types = 'list')
DP2012 <- read_excel("Reporte dosimetrico 2012.xlsx", sheet = 1, range =  cell_rows(7:25), col_names = TRUE, col_types = 'list')
DP2013 <- read_excel("Reporte dosimetrico 2013.xlsx", sheet = 1, range =  'A6:P23', col_names = TRUE, col_types = 'list')
DP2014 <- read_excel("Reporte dosimetrico 2014.xlsx", sheet = 1, range =  'A6:P23', col_names = TRUE, col_types = 'list')
DP2015 <- read_excel("Reporte dosimetrico 2015.xls", sheet = 1, range =  'A4:P22', col_names = TRUE, col_types = 'list')
DP2016 <- read_excel("Reporte dosimetrico 2016.xls", sheet = 1, range =  'A4:P24', col_names = TRUE, col_types = 'list')
DP2017 <- read_excel("Reporte dosimetrico 2017.xls", sheet = 1, range =  'A4:P25', col_names = TRUE, col_types = 'list')
DP2018 <- read_excel("Reporte dosimetrico 2018.xlsx", sheet = 1, range =  'A4:P26', col_names = TRUE, col_types = 'list')
DP2019 <- read_excel("Reporte dosimetrico 2019.xlsx", sheet = 1, range =  'A5:P27', col_names = TRUE, col_types = 'list')


#---------------------------------
# Funciones para modificar el DF
#---------------------------------

# Proposito: Escoger los datos de un solo trabajador
# @param: num1, num2 (últimos dos dígitos del codigo del trabajador)
Trabajador <- function(num1,num2){
  # se establece el codigo del trabajador para sacar los datos
  codigo = paste0('238-',num1,num2)
  # se sacan esos datos, se coloca transpuesta y se convierte a un dataframe
  a <- as.data.frame(t(DP2010[which(DP2010$CODIGO==codigo),]))
  b <- as.data.frame(t(DP2011[which(DP2011$CODIGO==codigo),]))
  c <- as.data.frame(t(DP2012[which(DP2012$CODIGO==codigo),]))
  d <- as.data.frame(t(DP2013[which(DP2013$CODIGO==codigo),]))
  e <- as.data.frame(t(DP2014[which(DP2014$CODIGO==codigo),]))
  f <- as.data.frame(t(DP2015[which(DP2015$CODIGO==codigo),]))
  g <- as.data.frame(t(DP2016[which(DP2016$CODIGO==codigo),]))
  h <- as.data.frame(t(DP2017[which(DP2017$CODIGO==codigo),]))
  i <- as.data.frame(t(DP2018[which(DP2018$CODIGO==codigo),]))
  j <- as.data.frame(t(DP2019[which(DP2019$CODIGO==codigo),]))
  # se combinan los datos por columnas
  newdf <- cbind(a,b,c,d,e,f,g,h,i,j)
  newdf <- as.data.frame(newdf)
}

# Proposito: Cambiar el nombre de las columnas por el año que corresponde
# @param: x (dataframe)
NombreColumnas <- function(x){
  # se eliminan las primeras 3 filas (código, nombre, apellido)
  x <- x[c(4:16),]
  for (i in 1:length(x)){
    # se agrega el año como nombre de la columna
    names(x)[i] <- paste0('201',abs(10-i))
  }
  # convierte el nombre de las columnas a variables numéricas
  cnames <- sapply(colnames(x), as.numeric)
  # reordena el orden de las columas de menor a mayor
  colnames(x) <- sort(cnames)
  return(x)
}

# Proposito: Limpiar los datos
# @param: x (dataframe)
Limpieza <- function(x){
  x[x == "*"] <- 0
  x[x == "---"] <- 0 
  x[x == "***"] <- 0
  x[x == "NEC"] <- 0
  x[x == "M"] <- 0.2
  return(x)
}

# Proposito: Obtener los totales de los datos fila y datos columna
# @param: x (dataframe)
TotalFilaColumna <- function(x){
  # primero se elimina la fila del total de excel 
  x <- x[-c(13), ]
  x['TOTAL',] <- colSums(sapply(x, as.numeric)) # suma de columnas
  x$MesTotal <- rowSums(sapply(x, as.numeric)) # suma de filas
  return(x)
}

# Proposito: Convertir los meses en una columna de tipo factor
# @param: x (dataframe)
FilasFactor <- function(x){
  # se agrega una columna con los mismos nombres del índice
  x <- cbind(Mes = rownames(x), x)
  rownames(x) <- NULL # se elimina el índice con los meses
  x$Mes <- factor(x$Mes, levels=unique(x$Mes)) # se crea una columna aparte de los meses
  return(x)
}

# Proposito: Calcular la estadística circular mensual
# @param: x (dataframe)
CircularMensual <- function(x){
  # se extraen todos los datos mensuales
  todo <- data.frame(a=unlist(x[1:12,3:length(x)-1], use.names = FALSE))
  # se convierten los datos a datos circulares
  cx <- circular(todo, type='angles', units='degrees', template='none', modulo='asis', zero=0, rotation='counter')
  cx <- as.numeric(cx)
  cx <- circular(cx)
  
  # Estadística Circular
  media <- mean(cx)
  longresult <- rho.circular(cx)
  varianza <- (1-rho.circular(cx))
  desv <- sd.circular(cx)
  
  # se imprimen los resultados
  print(paste0('- La media circular es: ', round(media,2)))
  print(paste0('- La longitud del vector medio es: ', round(longresult,5)))
  print(paste0('- La varianza circular es: ', round(varianza,5)))
  print(paste0('- La desviación estándar circular es: ', round(desv,2)))
}

# Proposito: Calcular la estadística circular anual
# @param: x (dataframe)
CircularAnual <- function(x){
  # se convierten los datos a datos circulares
  cx <- circular(x[13,7:11], type='angles', units='degrees', template='none', modulo='asis', zero=0, rotation='counter')
  cx <- as.numeric(cx)
  cx <- circular(cx)
  
  # Estadística Circular
  suma <- sum(cx)
  longresult <- rho.circular(cx)
  varianza <- (1-rho.circular(cx))
  desv <- sd.circular(cx)
  
  # se imprimen los resultados
  print(paste0('- La dosis acumulada en los últimos años es de: ', round(suma,2)))
  print(paste0('- La longitud del vector medio es: ', round(longresult,2)))
  print(paste0('- La varianza circular es: ', round(varianza,2)))
  print(paste0('- La desviación estándar circular es: ', round(desv,2)))
}

# Proposito: Realiza una gráfica de barras circular del total de meses 
# @param: x (dataframe)
cbarras.plot <- function(df){
  cplot <- ggplot(df[1:12,], aes(x=df$Mes[1:12], y=df$MesTotal[1:12], fill = df$Mes[1:12])) +
    geom_bar(stat="identity") +
    ggtitle("Dosis acumulada del trabajador M03LU en los años 2010-2019 [mSv]")+
    ylim(0,3) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text=element_text(size=8),
      legend.title = element_blank(),
      legend.text= element_text(size=8),
      plot.caption = element_text(hjust = 0)
    ) +
    coord_polar(start = 0)+
    geom_text(aes(label=df$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
  return(cplot)
}

# Proposito: Graficar una serie de tiempo con su línea de tendencia
# @param: x (dataframe)
ts.tendencia <- function(x){
  # descompone el dataframe y lo une todo como un solo vector
  df <-data.frame(datos=unlist(x[1:12,3:length(x)-1], use.names = FALSE))
  # agrega la variable del tiempo, desde 01-2010 hasta 12-2019
  df$tiempo = seq(as.Date("2010/1/1"), as.Date("2019/12/31"), "month")
  # grafica la serie de tiempo
  df.ts <- ggplot(data = df, aes(x = tiempo, y = datos))+ geom_line(size = 0.5) + 
    stat_smooth(method = 'loess') + # agrega línea suavizada de tendencia
    xlab("Tiempo") + ylab("Dosis [mSv]")
  return(df.ts)
}

# Proposito: Graficar una serie de tiempo interactiva
# @param: x (dataframe)
ts.interactive <- function(x){
  # descompone el dataframe y lo une todo como un solo vector
  df <-data.frame(datos=unlist(x[1:12,3:length(x)-1], use.names = FALSE))
  # agrega la variable del tiempo, desde 01-2010 hasta 12-2019
  df$tiempo = seq(as.Date("2010/1/1"), as.Date("2019/12/31"), "month")
  # conversión de data frame -> formato xts
  don <- xts(x = df$datos, order.by = df$tiempo)
  
  # grafica la serie de tiempo interactiva
  p <- dygraph(don) %>%
    dySeries("V1", label = "Dosis (mSv)") %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE)%>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)
  return(p)
}

# Proposito: Graficar la parte seasonal de la serie de timepo
# @param: x (dataframe)
ts.season <- function(x){
  # descompone el dataframe y lo une todo como un solo vector
  df <-data.frame(datos=unlist(x[1:12,3:length(x)-1], use.names = FALSE))
  # convierte los datos a una serie de tiempo
  df.ts <-ts(df, start = c(2010,1), end=c(2019,12), frequency = 12)
  # descompone la serie de tiempo
  df.ts.desc <- decompose(df.ts)
  vec <- data.frame(datos=sapply(df.ts.desc$seasonal, as.numeric))
  vec$tiempo = seq(as.Date("2010/1/1"), as.Date("2019/12/31"), "month")
  # se grafica solamente la parte seasonal
  season.df <- ggplot(data = vec, aes(x = tiempo, y = datos))+ geom_line(size = 0.2)+
    xlab('Tiempo') + ylab('Dosis [mSv]')
  season.df <- ggplotly(season.df)
  return(season.df)
}



#------------------------------
# Aplicación de las funciones
#------------------------------

#------------------------------------------------------------------------------------------
# MEDICOS = 2

# Trabajador M03LU
M03LU <- Trabajador(0,3)  
M03LU <- NombreColumnas(M03LU)
M03LU <- Limpieza(M03LU)
M03LU <- TotalFilaColumna(M03LU)
M03LU <- FilasFactor(M03LU)

# Trabajador M05MO
M05MO <- Trabajador(0,5) 
M05MO <- NombreColumnas(M05MO)
M05MO <- Limpieza(M05MO)
M05MO <- TotalFilaColumna(M05MO)
M05MO <- FilasFactor(M05MO)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# FISICOS MEDICOS = 2

# Trabajador F04EH
F04EH <- Trabajador(0,4)
F04EH <- NombreColumnas(F04EH)
F04EH <- Limpieza(F04EH)
F04EH <- TotalFilaColumna(F04EH)
F04EH <- FilasFactor(F04EH)

# Trabajador F10LD
F10LD <- Trabajador(1,0) 
F10LD <- NombreColumnas(F10LD)
F10LD <- Limpieza(F10LD)
F10LD <- TotalFilaColumna(F10LD)
F10LD <- FilasFactor(F10LD)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# TECNICOS = 2

# Trabajador T01EE
T01EE <- Trabajador(0,1) 
T01EE <- NombreColumnas(T01EE)
T01EE <- Limpieza(T01EE)
T01EE <- TotalFilaColumna(T01EE)
T01EE <- FilasFactor(T01EE)

# Trabajador T26PM
T26PM <- Trabajador(2,6) 
T26PM <- NombreColumnas(T26PM)
T26PM <- Limpieza(T26PM)
T26PM <- TotalFilaColumna(T26PM)
T26PM <- FilasFactor(T26PM)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# INGENIEROS = 1

# Trabajador I22CR
I22CR <- Trabajador(2,2) 
I22CR <- NombreColumnas(I22CR)
I22CR <- Limpieza(I22CR)
I22CR <- TotalFilaColumna(I22CR)
I22CR <- FilasFactor(I22CR)
#------------------------------------------------------------------------------------------



#------------------------------
# Análisis exploratorio
#------------------------------

#-----------------------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M03LU
cbarras.plot(M03LU)
#ts.tendencia(M03LU) 
ts.interactive(M03LU)
ts.season(M03LU)

# Trabajador M05MO
cbarras.plot(M05MO)
#ts.tendencia(M05MO)
ts.interactive(M05MO)
ts.season(M05MO)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F04EH
cbarras.plot(F04EH)
ts.interactive(F04EH)
ts.season(F04EH)

# Trabajador F10LD
cbarras.plot(F10LD)
ts.tendencia(F10LD)
ts.interactive(F10LD)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# TECNICOS

# Trabajador T01EE
cbarras.plot(T01EE)
ts.tendencia(T01EE)
ts.interactive(T01EE)

# Trabajador T26PM
cbarras.plot(T26PM)
ts.tendencia(T26PM)
ts.interactive(T26PM)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# INGENIERO

# Trabajador I22CR
cbarras.plot(I22CR)
ts.tendencia(I22CR)
ts.interactive(I22CR)
#-----------------------------------------------------------------------------------------------------------



#------------------------------
# Estadística circular
#------------------------------

#-----------------------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M03LU
CircularMensual(M03LU)
CircularAnual(M03LU)

# Trabajador M05MO
CircularMensual(M05MO)
CircularAnual(M05MO)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F04EH
CircularMensual(F04EH)
CircularAnual(F04EH)

# Trabajador F10LD
CircularMensual(F10LD)
CircularAnual(F10LD)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# TECNICOS

# Trabajador T01EE
CircularMensual(T01EE)
CircularAnual(T01EE)

# Trabajador T26PM
CircularMensual(T26PM)
CircularAnual(T26PM)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# INGENIERO

# Trabajador I22CR
CircularMensual(I22CR) # media =1.61, R=0.98, V=0.02, Desv=0.2
# nueva funcion = 0.16, 1, 0, 0.06
CircularAnual(I22CR)
#-----------------------------------------------------------------------------------------------------------

prueba <-data.frame(a=unlist(I22CR[1:12,3:length(I22CR)-1], use.names = FALSE))





