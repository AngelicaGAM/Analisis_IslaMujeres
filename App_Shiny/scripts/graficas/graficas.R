library(ggplot2)
library(plotrix)
library(scales)
source("UC/Proyecto Terminal/Analisis_IslaMujeres/limpieza.R")


#dev.off()
#--------- Funciones para graficar ------------

graficarPlot <- function(df, inicio, final, colorg,  textX, textY, tituloGrifco){
    var = subset(df, df$FOLIO == df$FOLIO , select=c("FOLIO", "RESPUESTA"))  
    x =  ggplot(data = var[inicio:final,], aes(x = RESPUESTA)) + geom_bar() +
    geom_bar(color = colorg, fill = colorg) + 
    xlab(textX) + 
    ylab(textY) + 
    ggtitle(tituloGrifco)
    return(x)
}


graficarPie <- function(Gplot){
  pie <- Gplot + coord_polar("y", start=0)+
    theme_minimal()
  return(pie)
}



# --------- AMBIENTAL ---------------

#P1 Qu� uso le dan sus vecinos a la salina?
AP1 = graficarPlot(Ambiental_df,1,55 ,'steelblue',"USO", "Numero de peronas", "AMBIENTAL - P1 - USO LE DAN SUS VECINOS A LA SALINA" )
AP1
#P2  �Qu� beneficio recibe de vivir aqu�?
AP2 = graficarPlot(Ambiental_df,57,111 ,'steelblue',"VENTAJAS", "Numero de peronas", "AMBIENTAL - P2 - VENTAJAS DE VIVIR CERCA DE LA SALINA" )
AP2
#P3 Qu� desventajas recibe de vivir aqu� cerca de la salina?
AP3 = graficarPlot(Ambiental_df,113,167 ,'steelblue',"DESVENTAJAS", "Numero de peronas", "AMBIENTAL - P3 - DESVENTAJAS DE VIVIR CERCA DE LA SALINA " )
AP3
#P4 �En qu� condiciones considera que se encuentra la salina?
AP4 = graficarPlot(Ambiental_df,169,223 ,'steelblue',"PERCEPCI�N ", "Numero de peronas", "AMBIENTAL - P4 - PERCEPCI�N DEL ESTADO DE LA SALINA" )
AP4
#P5 Qu� efectos genera la condici�n (sucia o contaminada) de la Salina? 
AP5 = graficarPlot(Ambiental_df,225,279 ,'steelblue',"CONSECUENCIAS", "Numero de peronas", "AMBIENTAL - P5 - CONSECUENCIAS DEL ESTADO ACTUAL DE LA SALINA" )
AP5

#P6 �Han llevado a cabo alguna actividad de limpieza, saneamiento o conservaci�n de la Salina?
AP6 =  graficarPlot(Ambiental_df,281,336 ,'steelblue',"RESPUESTA", "Numero de peronas", "AMBIENTAL - P6 - SE HAN REALIZADO ACTIVIDADES DE LIMPIEZA" )
AP6



graficarPie(AP1)
graficarPie(AP2)

graficarPie(AP3)
graficarPie(AP4)

graficarPie(AP5)
graficarPie(AP6)

# --------- SOCIAL ---------------



#P1 Entre los vecinos, realizan alguna actividad en com�n:
graficarPlot(Social_df_2,1,55 ,'steelblue',"RESPUESTA", "Numero de peronas", "SOCIAL - P1 - REALIZA ACTIVIDADES VECINALES" )

#P2  C�mo es la relaci�n con sus vecinos?
graficarPlot(Social_df,57,111 ,'steelblue',"ESTADO", "Numero de peronas", "SOCIAL - P2 - ESTADO DE RELACION VECINAL" )

#P3 Ha tenido problemas con sus vecinos: pleitos, demandas.?
graficarPlot(Social_df,113,167 ,'steelblue',"PROBLEMAS", "Numero de peronas", "SOCIAL - P3 - PROBLEMAS VECINALES " )

#P4 Con qu� frecuencia se hacen favores entre vecinos?
graficarPlot(Social_df,169,223 ,'steelblue',"RESPUESTA ", "Numero de peronas", "SOCIAL - P4 - FAVORES VECINALES" )

#P5 �En alg�n problema que se le presente, sus vecinos le ayudan?
graficarPlot(Social_df,225,279 ,'steelblue',"RESPUESTA", "Numero de peronas", "SOCIAL - P5 - AYUDA ENTRE VECINOS" )


#--------- Funciones para graficar ------------






