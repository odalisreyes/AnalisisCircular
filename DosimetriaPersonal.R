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
F23804EH <- Trabajador(0,4)
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




                