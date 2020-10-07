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

#------------------------------
# Se importan las librerías 
#------------------------------
library("readxl") # para leer archivos excel
library("circular") # para hacer estadística circular
library('tidyverse') 
library("dplyr")
library('ggplot2') # para hacer gráficas bonitas

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
ggplot(M03LU[1:12,], aes(x=M03LU$Mes[1:12], y=M03LU$MesTotal[1:12], fill = M03LU$Mes[1:12])) +
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
  geom_text(aes(label=M03LU$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)


# Trabajador M05MO
ggplot(M05MO[1:12,], aes(x=M05MO$Mes[1:12], y=M05MO$MesTotal[1:12], fill = M05MO$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador M05MO en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=M05MO$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F04EH
ggplot(F04EH[1:12,], aes(x=F04EH$Mes[1:12], y=F04EH$MesTotal[1:12], fill = F04EH$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador F04EH en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=F04EH$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)


# Trabajador F10LD
ggplot(F10LD[1:12,], aes(x=F10LD$Mes[1:12], y=F10LD$MesTotal[1:12], fill = F10LD$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador F10LD en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=F10LD$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# TECNICOS

# Trabajador T01EE
ggplot(T01EE[1:12,], aes(x=T01EE$Mes[1:12], y=T01EE$MesTotal[1:12], fill = T01EE$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador T01EE en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=T01EE$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)


# Trabajador T26PM
ggplot(T26PM[1:12,], aes(x=T26PM$Mes[1:12], y=T26PM$MesTotal[1:12], fill = T26PM$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador T26PM en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=T26PM$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# INGENIERO

# Trabajador I22CR
ggplot(I22CR[1:12,], aes(x=I22CR$Mes[1:12], y=I22CR$MesTotal[1:12], fill = I22CR$Mes[1:12])) +
  geom_bar(stat="identity") +
  ggtitle("Dosis acumulada del trabajador I22CR en los años 2010-2019 [mSv]")+
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
  geom_text(aes(label=I22CR$MesTotal[1:12]), position=position_dodge(width=0.5), vjust=0)
#-----------------------------------------------------------------------------------------------------------



#------------------------------
# Estadística circular
#------------------------------

#-----------
# MENSUAL
#------------
#-----------------------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M23803LU
# se convierten a datos circulares
cM03LU <- circular(M23803LU[13,7:11], units='degrees', zero=circular(0), rotation='counter')
# se convierte a un vector columna
cM03LU <- as.data.frame(t(cM03LU)); colnames(cM03LU) <- '2015-2019'
# los elementos se convierten a variables numéricas
cM03LU <- sapply(cM03LU, as.numeric)

mean(cM03LU) # dirección media = 1.828
rho.circular(cM03LU) # media de la longitud resultante = 0.9452089
(1-rho.circular(cM03LU)) # varianza = 0.05479111
sd.circular(cM03LU) # desviación = 0.3357062

#-#-#-#-#-#
# Trabajador M23805MO
cM05MO <- circular(M23805MO[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cM05MO <- as.data.frame(t(cM05MO)); colnames(cM05MO) <- '2015-2019'
cM05MO <- sapply(cM05MO, as.numeric)

mean(cM05MO) # dirección media = 1.872
rho.circular(cM05MO) # media de la longitud resultante = 0.9900009
(1-rho.circular(cM05MO)) # varianza = 0.009999146
sd.circular(cM05MO) # desviación = 0.1417708 
#-----------------------------------------------------------------------------------------------------------




#----------
# ANUAL
#----------
#-----------------------------------------------------------------------------------------------------------
# MEDICOS

# Trabajador M03LU
# se convierten a datos circulares
cM03LU <- circular(M23803LU[13,7:11], units='degrees', zero=circular(0), rotation='counter')
# se convierte a un vector columna
cM03LU <- as.data.frame(t(cM03LU)); colnames(cM03LU) <- '2015-2019'
# los elementos se convierten a variables numéricas
cM03LU <- sapply(cM03LU, as.numeric)

mean(cM03LU) # dirección media = 1.828
rho.circular(cM03LU) # media de la longitud resultante = 0.9452089
(1-rho.circular(cM03LU)) # varianza = 0.05479111
sd.circular(cM03LU) # desviación = 0.3357062

#-#-#-#-#-#
# Trabajador M05MO
cM05MO <- circular(M23805MO[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cM05MO <- as.data.frame(t(cM05MO)); colnames(cM05MO) <- '2015-2019'
cM05MO <- sapply(cM05MO, as.numeric)

mean(cM05MO) # dirección media = 1.872
rho.circular(cM05MO) # media de la longitud resultante = 0.9900009
(1-rho.circular(cM05MO)) # varianza = 0.009999146
sd.circular(cM05MO) # desviación = 0.1417708 
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# FISICOS MEDICOS

# Trabajador F04EH
cF04EH <- circular(F04EH[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cF04EH <- as.data.frame(t(cF04EH)); colnames(cF04EH) <- '2015-2019'
cF04EH <- sapply(cF04EH, as.numeric)

mean(cF04EH) # dirección media = 1.8474
rho.circular(cF04EH) # media de la longitud resultante = 0.9749765
(1-rho.circular(cF04EH)) # varianza = 0.02502346
sd.circular(cF04EH) # desviación = 0.2251305 


# Trabajador F10LD
cF10LD <- circular(F10LD[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cF10LD <- as.data.frame(t(cF10LD)); colnames(cF10LD) <- '2015-2019'
cF10LD <- sapply(cF10LD, as.numeric)

mean(cF10LD) # dirección media = 1.938
rho.circular(cF10LD) # media de la longitud resultante = 0.9542922
(1-rho.circular(cF10LD)) # varianza = 0.04570778
sd.circular(cF10LD) # desviación = 0.3058933
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# TECNICOS

# Trabajador T01EE
cT01EE <- circular(T01EE[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cT01EE <- as.data.frame(t(cT01EE)); colnames(cT01EE) <- '2015-2019'
cT01EE <- sapply(cT01EE, as.numeric)

mean(cT01EE) # dirección media = 1.64
rho.circular(cT01EE)# media de la longitud resultante = 0.99
(1-rho.circular(cT01EE)) # varianza = 0.01
sd.circular(cT01EE) # desviación = 0.1006633


# Trabajador T26PM
cT26PM <- circular(T26PM[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cT26PM <- as.data.frame(t(cT26PM)); colnames(cT26PM) <- '2015-2019'
cT26PM <- sapply(cT26PM, as.numeric)

mean(cT26PM) # dirección media = 1.936
rho.circular(cT26PM) # media de la longitud resultante = 0.965824
(1-rho.circular(cT26PM)) # varianza = 0.03417596
sd.circular(cT26PM) # desviación = 0.2637181
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# INGENIERO

# Trabajador I22CR
cI22CR <- circular(I22CR[13,7:11], units='degrees', zero=circular(0), rotation='counter')
cI22CR <- as.data.frame(t(cI22CR)); colnames(cI22CR) <- '2015-2019'
cI22CR <- sapply(cI22CR, as.numeric)

mean(cI22CR) # dirección media =  1.662
rho.circular(cI22CR) # media de la longitud resultante = 0.9741001
(1-rho.circular(cI22CR)) # varianza = 0.02589993
sd.circular(cI22CR) # desviación = 0.2290905
#-----------------------------------------------------------------------------------------------------------



