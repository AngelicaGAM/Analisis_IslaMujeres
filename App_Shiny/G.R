
#------------------------------------------------------------------------
library(shiny)
# --------- AMBIENTAL ---------------
d <- Ambiental_df[1:55,]
# --------- VIVIENDA   V1 La vivienda es :  --------- 
vi = columna[132:148]
A = sum(vi$VI01_Propia == 1)
B = sum(vi$VI01_Renta == 1)
C = sum(vi$VI01_Prestada == 1)
n = c(A, B, C)
RESPUESTA = c("Propia", "Renta", "Prestada")
dataf <- data.frame(n,RESPUESTA)
#1
V1P1R1 = graficarPlot(dataf,"RESPUESTA", "Numero de personas", "1. La vivienda que habita tu familia es")
T1P1R1 = graficarTable(dataf,"RESPUESTA", "Numero de personas", "1. La vivienda que habita tu familia es")

#---------- VIVIENDA   Si es propia como fue adquirida V4 ---------- 

A = sum(vi$VI04_Contado == 1)
B = sum(vi$VI04_Herencia == 1)
C = sum(vi$VI04_Mensual == 1)
n = c(A, B, C)
RESPUESTA = c("Pago de contado", "Herencia", "Pagos Mensuales")
VI04 <- data.frame(n,RESPUESTA)
#3
V1P2R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "2. En caso  de ser  propia, ¿De qué forma fue adquirida?  ")
T1P2R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "2. En caso  de ser  propia, ¿De qué forma fue adquirida?  ")


# Inundacion 3
A = 104 #INTERNET
B = 296 #RECOLECCION-BASURA
C = 356 #ELECTRICIDAD
D = 356 #TV
E = 356 #TELEFONO-CELULAR
FF = 43 #AGUA

to = A+B+C+D+E+FF
total = c(A, B, C,D,E,FF)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)
FF = as.integer((FF/to)*100)

n =  c(A, B, C,D,E,FF)


RESPUESTA = c("INTERNET", "RECOLECCION-BASURA", "ELECTRICIDAD", "TV-PAGA", "TELEFONO-CELULAR", "AGUA")
VI04 <- data.frame(n,RESPUESTA)
#3
V1P3R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "3. ¿Con qué servicios cuenta su vivienda? ")
T1P3R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "3. ¿Con qué servicios cuenta su vivienda? ")

#CHECk

# Inundacion 4
VI51 = sum(df_fin$VI08NInundaciones == 1)
VI50 = sum(df_fin$VI08NInundaciones == 0)
n = c(VI51, VI50)
RESPUESTA = c("Ha sufrido inundaciones en localidad", "No ha sufrido inundaciones en localidad")
VI04 <- data.frame(n,RESPUESTA)

V1P4R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "4. ¿Cuántas inundaciones ha sufrido al vivir en esta localidad?")
T1P4R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "4. ¿Cuántas inundaciones ha sufrido al vivir en esta localidad?")

# 5
op1Ii = sum(df_fin$VI11AccionInundacion == "ME-AGRADA-EL-LUGAR")
op1I2 = sum(df_fin$VI11AccionInundacion == "OTRO")
op1I3 = sum(df_fin$VI11AccionInundacion == "NO-ACCESO-A-CREDITO")
op1I4 = sum(df_fin$VI11AccionInundacion == "NO-ALTERNATIVA")
op1I5 = sum(df_fin$VI11AccionInundacion == "TRABAJO-ESCUELA-CERCA")


n = c(op1Ii,op1I2,op1I3,op1I4, op1I5)
RESPUESTA1 = c("ME-AGRADA-EL-LUGAR","OTRO", "NO-ACCESO-A-CREDITO","NO-ALTERNATIVA","TRABAJO-ESCUELA-CERCA" )

VI041 <- data.frame(n,RESPUESTA1)

V1P5R1 = graficarPlot(VI041,"RESPUESTA", "Numero de personas", "5. A pesar de la inundación usted decidió quedarse a vivir aquí por: ")
T1P5R1 = graficarTable(VI041,"RESPUESTA", "Numero de personas", "5. A pesar de la inundación usted decidió quedarse a vivir aquí por: ")


#6
VI51 = 39
VI50 = 337
n = c(VI51, VI50)
RESPUESTA = c("SI", "NO")
VI04 <- data.frame(n,RESPUESTA)

V1P6R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "6. ¿Sabe si su vivienda está en zona de riesgo?")
T1P6R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "6. ¿Sabe si su vivienda está en zona de riesgo?")






#6
VI51 = 131
VI50 = 245
n = c(VI51, VI50)
RESPUESTA = c("SI", "NO")
VI04 <- data.frame(n,RESPUESTA)

V1P7R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "7. ¿Conoce los impactos o afectaciones que puede sufrir?")
T1P7R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "7. ¿Conoce los impactos o afectaciones que puede sufrir?")



#6
VI51 = 131
VI50 = 245
# Inundacion 3
A = 149 #INTERNET
B = 99 #RECOLECCION-BASURA
C = 85 #ELECTRICIDAD
D = 290 #TV
E = 120 #TELEFONO-CELULAR
FF = 24 #AGUA
H = 331

to = A+B+C+D+E+FF + H
total = c(A, B, C,D,E,FF,H)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)
FF = as.integer((FF/to)*100)
H = as.integer((H/to)*100)

n =  c(A, B, C,D,E,FF,H)

RESPUESTA = c("ÁREAS VERDES", "CALLES PAVIMENTADAS", "BANQUETAS", "LUMINARIAS PUBLICAS","TRANSPORTE PUBLICO"	, "PATRULLAS VIGILANDO" , "LOTES BALDIOS")
VI04 <- data.frame(n,RESPUESTA)

V1P8R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "8. FAVOR DE SEÑALAR LOS SIGUIENTES SERVICIOS OBSERVADOS ALREDEDOR DE LA VIVIENDA: ")
T1P8R1 = graficarTable(VI04,"RESPUESTA", "Numero de personas", "8. FAVOR DE SEÑALAR LOS SIGUIENTES SERVICIOS OBSERVADOS ALREDEDOR DE LA VIVIENDA: ")

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

