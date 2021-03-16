#source("UC/PT/Entregables APP Isla/DatosEjido/Graficas/Funciones.R")
source("Funciones.R", local=TRUE)
# --------- VIVIENDA   V1 La vivienda es :  --------- 
vi = columna[132:148]
A = sum(vi$VI01_Propia == 1)
B = sum(vi$VI01_Renta == 1)
C = sum(vi$VI01_Prestada == 1)
n = c(A, B, C)
RESPUESTA = c("Propia", "Renta", "Prestada")
dataf <- data.frame(n,RESPUESTA)


V1P1R1 = graficarPlot(dataf,"RESPUESTA", "Numero de personas", "VI01 - Situacion de la vivienda")
V1P1R2 =  graficarPie(V1P1R1)
#V1P1R3 = graficarPlotDot(dataf,"RESPUESTA", "Numero de personas", "VI01 - Situacion de la vivienda")
#V1P1R4 = graficarPlotBar(dataf,"RESPUESTA", "Numero de personas", "VI01 - Situacion de la vivienda","#008000")

#---------- VIVIENDA   Si es propia como fue adquirida V4 ---------- 

A = sum(vi$VI04_Contado == 1)
B = sum(vi$VI04_Herencia == 1)
C = sum(vi$VI04_Mensual == 1)
n = c(A, B, C)
RESPUESTA = c("Pago de contado", "Herencia", "Pagos Mensuales")
VI04 <- data.frame(n,RESPUESTA)

V1P4R1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "VI04 - Tipo de pago en vivienda propia ")
#V1P4R2 = graficarPie(V1P4R1)
#V1P4R3 = graficarPlotDot(VI04,"RESPUESTA", "Numero de personas", "VI04 - Tipo de pago en vivienda propia ")
V1P4R4 = graficarPlotBar(VI04,"RESPUESTA", "Numero de personas", "VI04 - Tipo de pago en vivienda propia ", "#00FF00")


# --------- VIVIENDAS PROPIAS mix --------- 

#grid.arrange(V1P1R2, V1P4R1, nrow = 2 ,top=textGrob("Adquisición de viviendas propias"))

#grid.arrange(V1P1R3, V1P4R2, nrow = 2 ,top=textGrob("Adquisición de viviendas propias"))

#grid.arrange(V1P1R2, V1P4R3, nrow = 2 ,top=textGrob("Adquisición de viviendas propias"))

#grid.arrange(V1P1R1, V1P4R4, nrow = 2 ,top=textGrob("Adquisición de viviendas propias"))

#grid.arrange(V1P1R2, V1P4R2, nrow = 2 ,top=textGrob("Adquisición de viviendas propias"))




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
V1H2 = graficarPie(V1H1)
V1H3 = graficarPlotDot(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado enfrentar un huracán en actual localidad")
V1H4 = graficarPlotBar(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado enfrentar un huracán en actual localidad", "#00FF00")

# Inundacion
VI51 = sum(Inundacion$VI08NInundaciones == 1)
VI50 = sum(Inundacion$VI08NInundaciones == 0)
n = c(VI51, VI50)
RESPUESTA = c("Ha sufrido inundaciones en localidad", "No ha sufrido inundaciones en localidad")
VI04 <- data.frame(n,RESPUESTA)

#V1I1 = graficarPlot(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado pasar por una inundacion en actual localidad")
#V1I2 = graficarPie(V1I1)
#V1I3 = graficarPlotDot(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado pasar por una inundacion en actual localidad")
V1I4 = graficarPlotBar(VI04,"RESPUESTA", "Numero de personas", "VI05 - Le ha tocado pasar por una inundacion en actual localidad", "#00FF00")


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



HU = ggwordcloud2(VI04[, c("RESPUESTA", "n")], size = 2.5)

IN = ggwordcloud2(VI041[, c("RESPUESTA1", "n1")], size = 2.5)






