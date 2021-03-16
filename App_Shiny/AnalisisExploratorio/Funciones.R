library(psych)
library(ggplot2)
library(likert)
library(plotrix)
library(scales)
library(tidyverse)
library(grid)
library(gridExtra)

library(dplyr, quietly = TRUE)
library(ggwordcloud)
library(psych)
library(ggplot2)
library(likert)
library(plotrix)
library(scales)
library(tidyverse)
library(grid)
library(gridExtra)

library(dplyr, quietly = TRUE)
library(ggwordcloud)

#setwd("~/UC/Analisis_IslaMujeres/App_Shiny/AnalisisExploratorio")

Ejido <- read.csv("Zona-urbana-Ejido-230030286.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
Cancun <- read.csv("Cancun-230050001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
Isla  <- read.csv("Isla-Mujeres-230030001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")


Lista <- read.csv("Lista.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
columna <- read.csv("columnas.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")

#Mapas
CorE <- Ejido[2:3]
CorC <- Cancun[2:3]
CorI <- Isla[2:3]
CorPS <- columna[3:4]
names(CorE) = c("lat", "lng")
names(CorC) = c("lat", "lng")
names(CorI) = c("lat", "lng")
names(CorPS) = c("lat", "lng")

CorPS['country.etc'] = 'MX'
CorPS['city'] = 'Zona continental'

#data <- read.csv("UC/PT/Entregables APP Isla/encuestas/encuestas_finales_limpias2.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")

#data = data[-c(10),]

#dataCruda <- data[80:112]
#dataClean = columna[133:163]

#names(dataCruda)[1] = c("RESPUESTA")


#dataCruda[1] <- dataCruda[1] %>% 
#  mutate(v101 = ifelse(v101 == "RENTADA-PAGO-MENSUAL", "RENTADA", v101)) 


#  Descripción
#  dff = dataframe
#  textX, textY, tituloGrifco: son titulos de x y y de la grafica
#  N : numero de columnas

graficarPlot <- function(dff,  textX, textY, tituloGrifco){
  x <- ggplot(dff, aes(x = RESPUESTA, y = n ,fill=RESPUESTA))  +
    geom_bar(stat="identity") +
    xlab(textX)+ 
    ylab(textY) +  
    ggtitle(tituloGrifco) +  
    ylim(0,376)+
    geom_text(aes(label=percent(n/376)), position=position_stack(vjust=0.5),color="black",size=3)
  scale_fill_hue(c=45, l=80)+ 
  return(x)
}

# Solo se le pasa de parametro el plot

graficarPie <- function(Gplot){
  pie <- Gplot + coord_polar("y", start=0)+
    theme_minimal()
  return(pie)
}

#  Descripción
#  dff = dataframe
#  textX, textY, tituloGrifco: son titulos de x y y de la grafica
#  N : numero de columnas


graficarPlotDot <- function(dff,  textX, textY, tituloGrifco){
  x <- ggplot(dff, aes(x=RESPUESTA, y=n)) +
    geom_segment( aes(x=RESPUESTA, xend=RESPUESTA, y=0, yend=n), color="grey") +
    xlab(textX)+ 
    ylab(textY) +  
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    geom_point( color="orange", size=4) +
    ggtitle(tituloGrifco) +  
    ylim(0,376)+
    geom_text(aes(label=percent(n/376)), position=position_stack(vjust=0.5),color="black",size=3)
  return(x)
}

#  Descripción
#  dff = dataframe
#  textX, textY, tituloGrifco: son titulos de x y y de la grafica
#  N : numero de columnas

graficarPlotBar <- function(dff,  textX, textY, tituloGrifco, color){
  ggplot(dff, aes(x=RESPUESTA, y=n)) +
    geom_bar(stat="identity", fill=color, alpha=.6, width=.4) +
    xlab(textX)+ 
    ylab(textY) + 
    ggtitle(tituloGrifco) +  
    coord_flip() +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) 
}


#-------------------------------------


#  Descripción
#  dff = dataframe
#  textX, textY, tituloGrifco: son titulos de x y y de la grafica
#  N : numero de columnas


graficarPolar <- function(dff,  textX, textY, tituloGrifco){

  gg1 <- ggplot(dff, aes(x = RESPUESTA, y = n, fill = RESPUESTA)) + 
  geom_bar(width = 1, stat="identity") +
  coord_polar()
  myAng <- seq(-20, -340, length.out = 8)
  gg1 +   theme(axis.text.x = element_text(size = 12, angle = myAng)) 
  return(gg1)
}

