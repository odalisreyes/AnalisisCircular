##########################################################
## Universidad del Valle de Guatemala                   ##
## Tema:                                                ##
##          Análisis de datos circulares de DP          ##
## Autora:                                              ##
##          Elvia Odalis Reyes Guevara                  ## 
## Fecha:                                               ##
##          Septiembre 2020                             ##
##########################################################


#----------------------------------------------------------------
#  Instalación de paquetes nuevos en caso no esten descargados  
#----------------------------------------------------------------
if(!require(readxl)) {install.packages("readxl")}
if(!require(circular)) {install.packages("circular")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(ggplot2)) {install.packages("ggplot2")}

#------------------------------
# Se importan las librerías 
#------------------------------
library("readxl")
library("circular")
library('tidyverse')
library("dplyr")
library('ggplot2')

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
# @param: a-g (dataframe), trabajador (número de columna)
Trabajador <- function(a,b,c,d,e,f,g,h,i,j,trabajador){
  # transpuesta de los archivos de excel
  a <- t(a); b <- t(b); c <- t(c); d <- t(d); e <- t(e); f <- t(f); g <- t(g); h <- t(h); i <- t(i); j <- t(j);
  # se escoge la fila columna del trabajador a usar
  a <- a[,trabajador]
  b <- b[,trabajador]
  c <- c[,trabajador]
  d <- d[,trabajador]
  e <- e[,trabajador]
  f <- f[,trabajador]
  g <- g[,trabajador]
  h <- h[,trabajador]
  i <- i[,trabajador]
  j <- j[,trabajador]
  # se combinan los datos por columnas
  x <- cbind(a,b,c,d,e,f,g,h,i,j)
  x <- as.data.frame(x)
}

# Proposito: Cambiar el nombre de las columnas por el año que corresponde
# @param: x (dataframe)
NombreColumnas <- function(x){
  # se eliminan las primeras 3 filas (código, nombre, apellido)
  x <- x[c(4:16),]
  for (i in 1:10){
    num = i-1
    # se agrega el año como nombre de la columna
    names(x)[i] <- paste0('201',num)
  }
  return(x)
}

# Proposito: Limpiar los datos
# @param: x (dataframe)
Limpieza <- function(x){
  x[x == "---"] <- NA 
  x[x == "***"] <- NA
  x[x == "M"] <- 0.20
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



#------------------------------
# Aplicación de las funciones
#------------------------------

#------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M23803LU
M23803LU <- Trabajador(DP2010, DP2011, DP2012, DP2013, DP2014, DP2015, DP2016, DP2017, DP2018, DP2019, 2) 
M23803LU <- NombreColumnas(M23803LU)
M23803LU <- Limpieza(M23803LU)
M23803LU <- TotalFilaColumna(M23803LU)
M23803LU <- FilasFactor(M23803LU)

# Trabajador M23805MO
M23805MO <- Trabajador(DP2010, DP2011, DP2012, DP2013, DP2014, DP2015, DP2016, DP2017, DP2018, DP2019, 4) 
M23805MO <- NombreColumnas(M23805MO)
M23805MO <- Limpieza(M23805MO)
M23805MO <- TotalFilaColumna(M23805MO)
M23805MO <- FilasFactor(M23805MO)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F23804EH
F23804EH <- Trabajador(DP2010, DP2011, DP2012, DP2013, DP2014, DP2015, DP2016, DP2017, DP2018, DP2019, 3) 
F23804EH <- NombreColumnas(F23804EH)
F23804EH <- Limpieza(F23804EH)
F23804EH <- TotalFilaColumna(F23804EH)
F23804EH <- FilasFactor(F23804EH)

# Trabajador F23810LD
F23810LD <- Trabajador(DP2010, DP2011, DP2012, DP2013, DP2014, DP2015, DP2016, DP2017, DP2018, DP2019, 5) 
F23810LD <- NombreColumnas(F23810LD)
F23810LD <- Limpieza(F23810LD)
F23810LD <- TotalFilaColumna(F23810LD)
F23810LD <- FilasFactor(F23810LD)
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# TECNICOS

# Trabajador T23801EE
T23801EE <- Trabajador(DP2010, DP2011, DP2012, DP2013, DP2014, DP2015, DP2016, DP2017, DP2018, DP2019, 1) 
T23801EE <- NombreColumnas(T23801EE)
T23801EE <- Limpieza(T23801EE)
T23801EE <- TotalFilaColumna(T23801EE)
T23801EE <- FilasFactor(T23801EE)
#------------------------------------------------------------------------------------------



#------------------------------
# Análisis exploratorio
#------------------------------

#-----------------------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M23803LU
ggplot(M23803LU[1:12,], aes(x=M23803LU$Mes[1:12], y=M23803LU$MesTotal[1:12], fill = M23803LU$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada [mSv] en los años 2010-2019")+
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
  geom_text(aes(label=M23803LU$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)


# Trabajador M23805MO
ggplot(M23805MO[1:12,], aes(x=M23805MO$Mes[1:12], y=M23805MO$MesTotal[1:12], fill = M23805MO$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada [mSv] en los años 2010-2019")+
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
  geom_text(aes(label=M23805MO$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F23804EH
ggplot(F23804EH[1:12,], aes(x=F23804EH$Mes[1:12], y=F23804EH$MesTotal[1:12], fill = F23804EH$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada [mSv] en los años 2010-2019")+
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
  geom_text(aes(label=F23804EH$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)


# Trabajador F23810LD
ggplot(F23810LD[1:12,], aes(x=F23810LD$Mes[1:12], y=F23810LD$MesTotal[1:12], fill = F23810LD$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada [mSv] en los años 2010-2019")+
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
  geom_text(aes(label=F23810LD$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------




                