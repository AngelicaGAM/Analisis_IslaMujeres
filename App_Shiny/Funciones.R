library(shiny)

#Funciones y carga de archivos 

setwd("~/UC/Analisis_IslaMujeres/App_Shiny")

#Datos graficar 
  Ejido <- read.csv("Zona-urbana-Ejido-230030286.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")
  Cancun <- read.csv("Cancun-230050001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")
  Isla  <- read.csv("Isla-Mujeres-230030001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")
  columna <- read.csv("columnas.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding = "latin1")
df_fin <- read.csv("columnas.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")

df02 <- read.csv("df_02.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")  
#Datos mapas
  CancunM <- read.csv("Cancun.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
  EjidoM <- read.csv("Urbana.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
  IslaM <- read.csv("Isla.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
  DataMap  <- read.csv("result.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")

  filtro  <- filter(IslaM, encuesta_latitude > 21.2365 & encuesta_latitude < 21.241)




# -------------- FINAL --------------- #

Final_df <- read.csv("Final.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")

Final_df_2 <- Final_df[118:173,1:6]
Final_df_2 <- Final_df_2 %>%
  row_to_names(row_number = 1)
row.names(Final_df_2) <- NULL


Final_df <- Final_df[2:289,1:2]
Final_df <- Final_df[-c(58,59,116,117,174,175,232,233,c(118:173)),]
Final_df <- Final_df %>%
  row_to_names(row_number = 1)
row.names(Final_df) <- NULL

# -------------- AMBIENTAL --------------- #

Ambiental_df <- read.csv("Ambiental.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Ambiental_df <- Ambiental_df[2:347,1:3] 
Ambiental_df <- Ambiental_df[-c(58,59,116,117,174,175,232,233,290,291),]
Ambiental_df <- Ambiental_df %>%
  row_to_names(row_number = 1)
row.names(Ambiental_df) <- NULL

# -------------- ECONOMICO --------------- #

Economico_df <- read.csv("Economico.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Economico_df <- Economico_df[2:289,1:3] 
Economico_df <- Economico_df[-c(58,59,116,117,174,175,232,233),]
Economico_df <- Economico_df %>%
  row_to_names(row_number = 1)
row.names(Economico_df) <- NULL

# -------------- SOCIAL --------------- #

Social_df <- read.csv("Salinas1.csv", header = FALSE, sep=",", strip.white = TRUE, na.strings = "EMPTY")
Social_df_2 <- Social_df[2:57,1:5] 
Social_df_2 <- Social_df_2 %>%
  row_to_names(row_number = 1)
row.names(Social_df_2) <- NULL

Social_df <- Social_df[60:347,1:3]
Social_df <- Social_df %>%
  row_to_names(row_number = 1)
row.names(Social_df) <- NULL 
Social_df <- Social_df[-c(57,58,115,116,173,174,231,232),]




  
#--------------------------------------
#HOME 

#-------------------------------------
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

#-------------------------------------

graficarPlotFija <- function(dff,  textX, textY, tituloGrifco){
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

graficarPlot <- function(dff,  textX, textY, tituloGrifco){
  bar <- plot_ly(dff,
                 x = ~RESPUESTA, 
                 y = ~n, 
                 hoverinfo = ~n,
                 color = ~RESPUESTA, 
                 text = ~RESPUESTA,
                 type = 'bar'
  ) %>%
    layout(title = tituloGrifco, 
           xaxis = list(title = textX),
           yaxis = list(title = textY,
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           margin = list(l = 5, r = 5, t = 25, b = 45),
           showlegend = T)
  
  bar
}

graficarPlotS <- function(df,  textX, textY, tituloGrifco){
  #var = subset(Ambiental_df, Ambiental_df$FOLIO == Ambiental_df$FOLIO , select=c("FOLIO", "RESPUESTA")) 

  bar <- plot_ly(df,
                 x = ~RESPUESTA, 
                 y = ~FOLIO, 
                 hoverinfo = ~RESPUESTA,
                 color = ~RESPUESTA, 
                 text = ~RESPUESTA,
                 type = 'bar'
  ) %>%
    layout(title = tituloGrifco, 
           xaxis = list(title = textX),
           yaxis = list(title = textY,
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           margin = list(l = 5, r = 5, t = 25, b = 45),
           showlegend = T)
  
  bar
}

#-------------------------------------
#Solo se pasa el grafico del plot

graficarPie <- function(Gplot){
  pie <- Gplot + coord_polar("y", start=0)+
    theme_minimal()
  return(pie)
}

#-------------------------------------

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


#-------------------------------------

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

graficarPolar <- function(dff,  textX, textY, tituloGrifco){

  gg1 <- ggplot(dff, aes(x = RESPUESTA, y = n, fill = RESPUESTA)) + 
  geom_bar(width = 1, stat="identity") +
  coord_polar()
  myAng <- seq(-20, -340, length.out = 8)
  gg1 +   theme(axis.text.x = element_text(size = 12, angle = myAng)) 
  return(gg1)
}

#-------------------------------------
#     MAPAS

# Texto para label de mapas 
textMap <- function(M){
  mytext <- paste(
    "Pais: ", "Mexico","<br/>", 
    "Area: ", M$zona , "<br/>", 
    "Estudio: ", M$est , "<br/>", 
    sep="") %>%
    lapply(htmltools::HTML)
}

#Funcion para graficar mapas 
mapp <- function(datos, mytext){
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

GraphM <- function(datos){
  mytext =  textMap(datos)
  mapp(datos, mytext)
}

#-------------------------------------
