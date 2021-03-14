#source("UC/PT/Entregables APP Isla/DatosEjido/Graficas/Funciones.R")
source("Funciones.R")
#------------------------------------------------------------------------

#SALINAS 


# --------- AMBIENTAL ---------------
d <- Ambiental_df[1:55,]
#P1 Qué uso le dan sus vecinos a la salina?
#AP1 = graficarPlotS(Ambiental_df,1,55 ,"USO", "Numero de personas", "AMBIENTAL - P1 - USO LE DAN SUS VECINOS A LA SALINA" )

#P2  ¿Qué beneficio recibe de vivir aquí?
#AP2 = graficarPlotS(Ambiental_df,57,111 ,"VENTAJAS", "Numero de personas", "AMBIENTAL - P2 - VENTAJAS DE VIVIR CERCA DE LA SALINA" )

#P3 Qué desventajas recibe de vivir aquí cerca de la salina?
#AP3 = graficarPlotS(Ambiental_df,113,167 ,"DESVENTAJAS", "Numero de personas", "AMBIENTAL - P3 - DESVENTAJAS DE VIVIR CERCA DE LA SALINA " )

#P4 ¿En qué condiciones considera que se encuentra la salina?
#AP4 = graficarPlotS(Ambiental_df,169,223 ,"PERCEPCIÓN ", "Numero de personas", "AMBIENTAL - P4 - PERCEPCIÓN DEL ESTADO DE LA SALINA" )

#P5 Qué efectos genera la condición (sucia o contaminada) de la Salina? 
#AP5 = graficarPlotS(Ambiental_df,225,279 ,"CONSECUENCIAS", "Numero de personas", "AMBIENTAL - P5 - CONSECUENCIAS DEL ESTADO ACTUAL DE LA SALINA" )


#P6 ¿Han llevado a cabo alguna actividad de limpieza, saneamiento o conservación de la Salina?
#AP6 =  graficarPlotS(Ambiental_df,281,336 ,"RESPUESTA", "Numero de personas", "AMBIENTAL - P6 - SE HAN REALIZADO ACTIVIDADES DE LIMPIEZA" )




# --------- SOCIAL ---------------



#P1 Entre los vecinos, realizan alguna actividad en común:
#AS1 = graficarPlotS(Social_df,1,55 ,"RESPUESTA", "Numero de personas", "SOCIAL - P1 - REALIZA ACTIVIDADES VECINALES" )

#P2  Cómo es la relación con sus vecinos?
#AS2 = graficarPlotS(Social_df,57,111 ,"ESTADO", "Numero de personas", "SOCIAL - P2 - ESTADO DE RELACION VECINAL" )

#P3 Ha tenido problemas con sus vecinos: pleitos, demandas.?
#AS3 = graficarPlotS(Social_df,113,167 ,"PROBLEMAS", "Numero de personas", "SOCIAL - P3 - PROBLEMAS VECINALES " )

#P4 Con qué frecuencia se hacen favores entre vecinos?
#AS4 = graficarPlotS(Social_df,169,223 ,"RESPUESTA ", "Numero de personas", "SOCIAL - P4 - FAVORES VECINALES" )

#P5 ¿En algún problema que se le presente, sus vecinos le ayudan?
#AS5 = graficarPlotS(Social_df,225,279 ,"RESPUESTA", "Numero de personas", "SOCIAL - P5 - AYUDA ENTRE VECINOS" )




#Poblacion y migracion  VIVIENDA

# --------- VIVIENDA   V1 La vivienda es :  --------- 
vi = columna[132:148]
A = sum(vi$VI01_Propia == 1)
B = sum(vi$VI01_Renta == 1)
C = sum(vi$VI01_Prestada == 1)
n = c(A, B, C)
RESPUESTA = c("Propia", "Renta", "Prestada")
dataf <- data.frame(n,RESPUESTA)
V1P1R1 = graficarPlot(dataf,"RESPUESTA", "Numero de personas", "VI01 - Situacion de la vivienda")

#---------- VIVIENDA   Si es propia como fue adquirida V4 ---------- 

A = sum(vi$VI04_Contado == 1)
B = sum(vi$VI04_Herencia == 1)
C = sum(vi$VI04_Mensual == 1)
n = c(A, B, C)
RESPUESTA = c("Pago de contado", "Herencia", "Pagos Mensuales")
VI04 <- data.frame(n,RESPUESTA)

V1P4R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "VI04 - Tipo de pago en vivienda propia ")

#---------- VIVIENDA  Huracanes Inundaciones  V5 ----------

# huracan
Inundacion = vi[13]
Huracan = vi[14]
AcI  = vi[15]
AcH = vi[16]
VI51 = sum(Huracan$VI09PeHuracan == 1)
VI50 = sum(Huracan$VI09PeHuracan == 0)
n = c(VI51, VI50)
RESPUESTA = c("Ha enfrentado huracán", "No ha enfrentado huracán")
VI04 <- data.frame(n,RESPUESTA)

V1H1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado enfrentar un huracán en actual localidad")

# Inundacion
VI51 = sum(Inundacion$VI08NInundaciones == 1)
VI50 = sum(Inundacion$VI08NInundaciones == 0)
n = c(VI51, VI50)
RESPUESTA = c("Ha sufrido inundaciones en localidad", "No ha sufrido inundaciones en localidad")
VI04 <- data.frame(n,RESPUESTA)

V1I1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado pasar por una inundacion en actual localidad")

# --------- VIVIENDAS Huracan e inundacion  --------- 

#grid.arrange(V1H1, V1I1, nrow = 2 ,top=textGrob("Inundaciones o Huracanes"))

#grid.arrange(V1H2, V1I2, nrow = 2 ,top=textGrob("Inundaciones o Huracanes"))

#grid.arrange(V1H2, V1I3, nrow = 2 ,top=textGrob("Inundaciones o Huracanes"))

#grid.arrange(V1H2, V1I4, nrow = 2 ,top=textGrob("Inundaciones o Huracanes"))

#grid.arrange(V1H3, V1I2, nrow = 2 ,top=textGrob("Inundaciones o Huracanes"))


#accion  inundaciones

unique(AcI$VI10AccionHuracan)
unique(AcH$VI11AccionInundacion)


op1H1 = sum(AcI$VI10AccionHuracan == "QUEDAMOS")
op1H2 = sum(AcI$VI10AccionHuracan == "FAMILIAR-AMIGO")
op1H3 = sum(AcI$VI10AccionHuracan == "REFUGIO")
op1Ii = sum(AcH$VI11AccionInundacion == "ME-AGRADA-EL-LUGAR")
op1I2 = sum(AcH$VI11AccionInundacion == "OTRO")
op1I3 = sum(AcH$VI11AccionInundacion == "NO-ACCESO-A-CREDITO")
op1I4 = sum(AcH$VI11AccionInundacion == "NO-ALTERNATIVA")
op1I5 = sum(AcH$VI11AccionInundacion == "TRABAJO-ESCUELA-CERCA")



n = c(op1H1,op1H2,op1H3)
RESPUESTA = c("QUEDAMOS","FAMILIAR-AMIGO", "REFUGIO")

VI04 <- data.frame(n,RESPUESTA)


n1 = c(op1Ii,op1I2,op1I3,op1I4, op1I5)
RESPUESTA1 = c("ME-AGRADA-EL-LUGAR","OTRO", "NO-ACCESO-A-CREDITO","NO-ALTERNATIVA","TRABAJO-ESCUELA-CERCA" )

VI041 <- data.frame(n1,RESPUESTA1)


HU = VI041
#ggwordcloud2(VI04[, c("RESPUESTA", "n")], size = 2.5)

IN = VI041
#ggwordcloud2(VI041[, c("RESPUESTA1", "n1")], size = 2.5)


#----------------------------------------------------------------------
# mapas estudio socieconomico 
# 

edades1 <- read.csv("edad - Hoja 1.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")

edades18 = edades1
  
 edadZonaUrbana =   edades18   %>% ggplot( 
    aes(x = Poblacion.por.sexo, 
        y = Grupos.de.edad, fill = Sexo)
    )+
    geom_col(position = "stack", alpha = 0.7) + 
    scale_x_continuous(name="Población", limits=c(-86, 86)) +
    scale_fill_manual(values = c("#2c7fb8", "#c51b8a")) +
    # tema minimalista
    theme_minimal()+
    # leyenda para el fondo
    theme(
      legend.position = "bottom",
      plot.caption = element_text(vjust = 0.5))+
    labs(
      y = "Quinquenios de edad",
      x = "Población",
      title = "Pirámide de población total por sexo. ", 
      subtitle = "Zona continental, 2020")
 
 #----------------------------------------------------------------------
 # mapa SALINAS
 # 
 #salinasEDAD 
 salinasEDAD <- read.csv("salinasEDAD.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")
 
 sal1 = sum(salinasEDAD$Poblacion.por.sexo >= 18 & salinasEDAD$Poblacion.por.sexo <= 25)
 sal2 = sum(salinasEDAD$Poblacion.por.sexo >= 26 & salinasEDAD$Poblacion.por.sexo <= 35)
 sal3 = sum(salinasEDAD$Poblacion.por.sexo >= 36 & salinasEDAD$Poblacion.por.sexo <= 45)
 sal4 = sum(salinasEDAD$Poblacion.por.sexo >= 46 & salinasEDAD$Poblacion.por.sexo <= 55)
 sal5 = sum(salinasEDAD$Poblacion.por.sexo >= 56 & salinasEDAD$Poblacion.por.sexo <= 92)
 
 edadS = c(sal1, sal2, sal3, sal4, sal5)
 grupo = c('[18,25]', '[26,35]', '[36,45]',  '[46,55]',  '[56,92]' )
 Sex = c('none', '[26,35]', '[36,45]',  '[46,55]',  '[56,92]' )
 edadSf <- data.frame(grupo, edadS)
 
names(edadSf) = c('Grupos.de.edad' , 'Poblacion.por.sexo')

edadSalinas =   edadSf   %>% ggplot( 
          aes(x = Poblacion.por.sexo, 
              y = Grupos.de.edad))+
          geom_col(position = "stack", alpha = 0.7, fill = "purple") + 
          # tema minimalista
          theme_minimal()+
          # leyenda para el fondo
          theme(
            legend.position = "bottom",
            plot.caption = element_text(vjust = 0.5))+
          labs(
            y = "Quinquenios de edad",
            x = "Población",
            title = "Pirámide de población total por sexo.", 
            subtitle = " Salinas, 2017")
#edadSalinas

#----------------------------------------------------------------------
# mapa CANCUN
# 
#cancunEDAD 
salinasEDAD <- read.csv("salinasEDAD.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", fileEncoding  = "UTF-8")

sal1 = sum(salinasEDAD$Poblacion.por.sexo >= 18 & salinasEDAD$Poblacion.por.sexo <= 25)
sal2 = sum(salinasEDAD$Poblacion.por.sexo >= 26 & salinasEDAD$Poblacion.por.sexo <= 35)
sal3 = sum(salinasEDAD$Poblacion.por.sexo >= 36 & salinasEDAD$Poblacion.por.sexo <= 45)
sal4 = sum(salinasEDAD$Poblacion.por.sexo >= 46 & salinasEDAD$Poblacion.por.sexo <= 55)
sal5 = sum(salinasEDAD$Poblacion.por.sexo >= 56 & salinasEDAD$Poblacion.por.sexo <= 92)

edadS = c(sal1, sal2, sal3, sal4, sal5)
grupo = c('[18,25]', '[26,35]', '[36,45]',  '[46,55]',  '[56,92]' )
Sex = c('none', '[26,35]', '[36,45]',  '[46,55]',  '[56,92]' )
edadSf <- data.frame(grupo, edadS)

names(edadSf) = c('Grupos.de.edad' , 'Poblacion.por.sexo')

edadSalinas =   edadSf   %>% ggplot( 
  aes(x = Poblacion.por.sexo, 
      y = Grupos.de.edad))+
  geom_col(position = "stack", alpha = 0.7, fill = "purple") + 
  # tema minimalista
  theme_minimal()+
  # leyenda para el fondo
  theme(
    legend.position = "bottom",
    plot.caption = element_text(vjust = 0.5))+
  labs(
    y = "Quinquenios de edad",
    x = "Población",
    title = "Pirámide de población total por sexo.", 
    subtitle = " Salinas, 2017")
#edadSalinas

