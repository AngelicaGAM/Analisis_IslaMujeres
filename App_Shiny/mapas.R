library(shiny)



# Texto para label de mapas 
textMap <- function(M){
mytext <- paste(
  "Pais: ", "Mexico","<br/>", 
  "Area: ", M$zona , "<br/>", 

  sep="") %>%
  lapply(htmltools::HTML)
}

#Funcion para graficar mapas 
map <- function(datos){
  df <- data.frame(longitude = datos$encuesta_longitude, latitude = datos$encuesta_latitude)
  coordinates(df) <- ~longitude+latitude
  leaflet(df) %>% 
      addTiles() %>% 
      addMarkers(
        clusterId = "cluster" ,
            label = mytext,
            labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "13px", 
                        direction = "auto" ),
            clusterOptions = 
          markerClusterOptions(
              color= "red"
            )
      )
}


#mytext = textMap(Isla)

#filtro  <- filter(Isla, encuesta_latitude > 21.2365 & encuesta_latitude < 21.241)

#mapp(Isla)
#f <- filter(Isla, encuesta_latitude < 21.2365 & encuesta_latitude > 21.241)
#mapp(filtro) 
#mapp(Ejido)





