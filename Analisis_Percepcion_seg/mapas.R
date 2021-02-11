library(sp)
library(leaflet)
library(dplyr)


Isla  <- read.csv("Isla-Mujeres-230030001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
Ejido <- read.csv("Zona-urbana-Ejido-230030286.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
Cancun <- read.csv("Cancún-230050001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")

mapp <- function(datos){
  df <- data.frame(longitude = datos$encuesta_longitude, latitude = datos$encuesta_latitude)
  coordinates(df) <- ~longitude+latitude
  leaflet(df) %>% addMarkers() %>% addTiles()
}

mapp(Isla)

filtro  <- filter(Isla, encuesta_latitude > 21.2365 & encuesta_latitude < 21.241)

#f <- filter(Isla, encuesta_latitude < 21.2365 & encuesta_latitude > 21.241)
mapp(filtro)
mapp(Ejido)





