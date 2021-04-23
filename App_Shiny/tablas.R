library(vcd)
library(shiny)

#graficas
mosaics <- function(tbl, rot, offs){
  vcd::mosaic(tbl,gp = shading_Friendly,labeling = labeling_border(rot_labels = rot, 
                                                         just_labels = c("left", 
                                                                         "left", 
                                                                         "left", 
                                                                         "left"),
                                                         offset_labels = offs,
                                                         gp_labels = gpar(fontsize = 10),
                                                         gp_varnames = gpar(fontsize = 0, fontface = 2)))
}
mosaics2 <- function(tbl, rot, offs){
  vcd::mosaic(tbl,gp = shading_Friendly,direction = TRUE,labeling = labeling_border(rot_labels = rot, 
                                                        just_labels = c("left", 
                                                                        "left", 
                                                                        "left", 
                                                                        "left"),
                                                        offset_labels = offs,
                                                        abbreviate_labs = TRUE,
                                                        gp_labels = gpar(fontsize = 10),
                                                        gp_varnames = gpar(fontsize = 0, fontface = 2)))
}
mosaics3 <- function(tbl, rot, offs){
  vcd::mosaic(tbl,gp = shading_Friendly,direction = TRUE,labeling = labeling_border(rot_labels = rot, 
                                                                                    just_labels = c("right", 
                                                                                                    "left", 
                                                                                                    "left", 
                                                                                                    "left"),
                                                                                    offset_labels = offs,
                                                                                    abbreviate_labs = TRUE,
                                                                                    gp_labels = gpar(fontsize = 10),
                                                                                    gp_varnames = gpar(fontsize = 0, fontface = 2)))
}
salinas <- read.csv("salinas_mosaicos.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")
xdf <- read.csv("x.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")
percep <- read.csv("percepcion_mosaicos.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY", encoding = "UTF-8")



tbl1 <- xtabs(~Origen + Puesto, xdf)
tbl1 <- tbl1[c(1,2,11,13,15,16),c(1,2,6,5,3,4)]

tbl2 <- xtabs(~Origen + Trabajo, xdf)
tbl2 <- tbl2[c(1,2,11,13,15,16),]

tbl3 <- xtabs(~Origen + Escolaridad, xdf)
tbl3 <- tbl3[c(1,2,11,13,15,16),c(4,5,6,1,7,2,3)]

tbl4 <- xtabs(~Origen + Ingreso_sem, xdf)
tbl4 <- tbl4[c(1,2,11,13,15,16),]

tbl5 <- xtabs(~Escolaridad + Puesto, xdf)
tbl5 <- tbl5[c(4,5,6,1,7,2,3),c(1,2,6,5,3,4)]

tbl6 <- xtabs(~Escolaridad + Trabajo, xdf)
tbl6 <- tbl6[c(4,5,6,1,7,2,3),]

#tbl7 <- xtabs(~Escolaridad + Tiempo_aqui, xdf)
#tbl7 <- tbl7[c(4,5,6,1,7,2,3),c(3,1,2,4)]

tbl8 <- xtabs(~Escolaridad + Ingreso_sem, xdf)
tbl8 <- tbl8[c(4,5,6,1,7,2,3),]

tbl9 <- xtabs(~Puesto + Trabajo, xdf)
tbl9 <- tbl9[c(1,2,6,5,3,4),]

tbl10 <- xtabs(~Puesto + Escolaridad,xdf)
tbl10 <- tbl10[c(1,2,6,5,3,4),c(4,5,6,1,7,2,3)]

tbl11 <- xtabs(~Puesto + Ingreso_sem, xdf)
tbl11 <- tbl11[c(1,2,6,5,3,4),]

tbl12 <- xtabs(~Trabajo + Puesto, xdf)
tbl12 <- tbl12[,c(1,2,6,5,3,4)]

tbl13 <- xtabs(~Trabajo + Escolaridad,xdf)
tbl13 <- tbl13[,c(4,5,6,1,7,2,3)]

tbl14 <- xtabs(~Trabajo + Ingreso_sem, xdf)

#3
tbl15 <- xtabs(~Origen + Puesto + Edad, xdf)
tbl15 <- tbl15[c(1,2,11,13,15,16),c(1,2,6,5,3,4),]
rownames(tbl15)[rownames(tbl15) == "Campeche"] = "CHIS"
rownames(tbl15)[rownames(tbl15) == "Chiapas"] = "CAM"
rownames(tbl15)[rownames(tbl15) == "Quintana Roo"] = "QROO"
rownames(tbl15)[rownames(tbl15) == "Tabasco"] = "TAB"
rownames(tbl15)[rownames(tbl15) == "Veracruz"] = "VER"
rownames(tbl15)[rownames(tbl15) == "Yucatan"] = "YUC"


tbl16 <- xtabs(~Origen + Puesto + Sexo, xdf)
tbl16 <- tbl16[c(1,2,11,13,15,16),c(1,2,6,5,3,4),]
rownames(tbl16)[rownames(tbl16) == "Campeche"] = "CHIS"
rownames(tbl16)[rownames(tbl16) == "Chiapas"] = "CAM"
rownames(tbl16)[rownames(tbl16) == "Quintana Roo"] = "QROO"
rownames(tbl16)[rownames(tbl16) == "Tabasco"] = "TAB"
rownames(tbl16)[rownames(tbl16) == "Veracruz"] = "VER"
rownames(tbl16)[rownames(tbl16) == "Yucatan"] = "YUC"

tbl17 <- xtabs(~Origen + Trabajo + Edad, xdf)
tbl17 <- tbl17[c(1,2,11,13,15,16),,]
rownames(tbl17)[rownames(tbl17) == "Campeche"] = "CHIS"
rownames(tbl17)[rownames(tbl17) == "Chiapas"] = "CAM"
rownames(tbl17)[rownames(tbl17) == "Quintana Roo"] = "QROO"
rownames(tbl17)[rownames(tbl17) == "Tabasco"] = "TAB"
rownames(tbl17)[rownames(tbl17) == "Veracruz"] = "VER"
rownames(tbl17)[rownames(tbl17) == "Yucatan"] = "YUC"

tbl18 <- xtabs(~Origen + Trabajo + Sexo, xdf)
tbl18 <- tbl18[c(1,2,11,13,15,16),,]
rownames(tbl18)[rownames(tbl18) == "Campeche"] = "CHIS"
rownames(tbl18)[rownames(tbl18) == "Chiapas"] = "CAM"
rownames(tbl18)[rownames(tbl18) == "Quintana Roo"] = "QROO"
rownames(tbl18)[rownames(tbl18) == "Tabasco"] = "TAB"
rownames(tbl18)[rownames(tbl18) == "Veracruz"] = "VER"
rownames(tbl18)[rownames(tbl18) == "Yucatan"] = "YUC"

tbl19 <- xtabs(~Origen + Escolaridad + Edad, xdf)
tbl19 <- tbl19[c(1,2,11,13,15,16),c(4,5,6,1,7,2,3),]
rownames(tbl19)[rownames(tbl19) == "Campeche"] = "CHIS"
rownames(tbl19)[rownames(tbl19) == "Chiapas"] = "CAM"
rownames(tbl19)[rownames(tbl19) == "Quintana Roo"] = "QROO"
rownames(tbl19)[rownames(tbl19) == "Tabasco"] = "TAB"
rownames(tbl19)[rownames(tbl19) == "Veracruz"] = "VER"
rownames(tbl19)[rownames(tbl19) == "Yucatan"] = "YUC"

tbl20 <- xtabs(~Origen + Escolaridad + Sexo, xdf)
tbl20 <- tbl20[c(1,2,11,13,15,16),c(4,5,6,1,7,2,3),]
rownames(tbl20)[rownames(tbl20) == "Campeche"] = "CHIS"
rownames(tbl20)[rownames(tbl20) == "Chiapas"] = "CAM"
rownames(tbl20)[rownames(tbl20) == "Quintana Roo"] = "QROO"
rownames(tbl20)[rownames(tbl20) == "Tabasco"] = "TAB"
rownames(tbl20)[rownames(tbl20) == "Veracruz"] = "VER"
rownames(tbl20)[rownames(tbl20) == "Yucatan"] = "YUC"


######################################################
tbl21 <- xtabs(~Origen + Ingreso_sem + Edad, xdf)
tbl21 <- tbl21[c(1,2,11,13,15,16),,]
rownames(tbl21)[rownames(tbl21) == "Campeche"] = "CHIS"
rownames(tbl21)[rownames(tbl21) == "Chiapas"] = "CAM"
rownames(tbl21)[rownames(tbl21) == "Quintana Roo"] = "QROO"
rownames(tbl21)[rownames(tbl21) == "Tabasco"] = "TAB"
rownames(tbl21)[rownames(tbl21) == "Veracruz"] = "VER"
rownames(tbl21)[rownames(tbl21) == "Yucatan"] = "YUC"

tbl22 <- xtabs(~Origen + Ingreso_sem + Sexo, xdf)
tbl22 <- tbl22[c(1,2,11,13,15,16),,]
rownames(tbl22)[rownames(tbl22) == "Campeche"] = "CHIS"
rownames(tbl22)[rownames(tbl22) == "Chiapas"] = "CAM"
rownames(tbl22)[rownames(tbl22) == "Quintana Roo"] = "QROO"
rownames(tbl22)[rownames(tbl22) == "Tabasco"] = "TAB"
rownames(tbl22)[rownames(tbl22) == "Veracruz"] = "VER"
rownames(tbl22)[rownames(tbl22) == "Yucatan"] = "YUC"

tbl23 <- xtabs(~Escolaridad + Puesto + Edad, xdf)
tbl23 <- tbl23[c(4,5,6,1,7,2,3),c(1,2,6,5,3,4),]

tbl24 <- xtabs(~Escolaridad + Puesto + Sexo, xdf)
tbl24 <- tbl24[c(4,5,6,1,7,2,3),c(1,2,6,5,3,4),]

tbl25 <- xtabs(~Escolaridad + Trabajo + Edad, xdf)
tbl25 <- tbl25[c(4,5,6,1,7,2,3),,]

tbl26 <- xtabs(~Escolaridad + Trabajo + Sexo, xdf)
tbl26 <- tbl26[c(4,5,6,1,7,2,3),,]

tbl27 <- xtabs(~Escolaridad + Edad, xdf)
tbl27 <- tbl27[c(4,5,6,1,7,2,3),]

tbl28 <- xtabs(~Escolaridad + Sexo, xdf)
tbl28 <- tbl28[c(4,5,6,1,7,2,3),]

tbl29 <- xtabs(~Escolaridad + Ingreso_sem + Edad, xdf)
tbl29 <- tbl29[c(4,5,6,1,7,2,3),,]

tbl30 <- xtabs(~Escolaridad + Ingreso_sem + Sexo, xdf)
tbl30 <- tbl30[c(4,5,6,1,7,2,3),,]

tbl31 <- xtabs(~Puesto + Trabajo + Edad, xdf)
tbl31 <- tbl31[c(1,2,6,5,3,4),,]

tbl32 <- xtabs(~Puesto + Trabajo + Sexo, xdf)
tbl32 <- tbl32[c(1,2,6,5,3,4),,]

tbl33 <- xtabs(~Puesto + Escolaridad + Edad,xdf)
tbl33 <- tbl33[c(1,2,6,5,3,4),c(4,5,6,1,7,2,3),]
tbl34 <- xtabs(~Puesto + Escolaridad + Sexo,xdf)
tbl34 <- tbl34[c(1,2,6,5,3,4),c(4,5,6,1,7,2,3),]

tbl35 <- xtabs(~Puesto + Ingreso_sem + Edad, xdf)
tbl35 <- tbl35[c(1,2,6,5,3,4),,]
tbl36 <- xtabs(~Puesto + Ingreso_sem + Sexo, xdf)
tbl36 <- tbl36[c(1,2,6,5,3,4),,]

tbl37 <- xtabs(~Trabajo + Puesto + Edad, xdf)
tbl37 <- tbl37[,c(1,2,6,5,3,4),]
tbl38 <- xtabs(~Trabajo + Puesto + Sexo, xdf)
tbl38 <- tbl38[,c(1,2,6,5,3,4),]

tbl39 <- xtabs(~Trabajo + Escolaridad + Edad,xdf)
tbl39 <- tbl39[,c(4,5,6,1,7,2,3),]
tbl40 <- xtabs(~Trabajo + Escolaridad + Sexo,xdf)
tbl40 <- tbl40[,c(4,5,6,1,7,2,3),]

tbl41 <- xtabs(~Trabajo + Ingreso_sem + Edad, xdf)
tbl42 <- xtabs(~Trabajo + Ingreso_sem + Sexo, xdf)

#########################################

tbl43 <- xtabs(~Puesto + Edad, xdf)
tbl43 <- tbl43[c(1,2,6,5,3,4),]

tbl44 <- xtabs(~Puesto + Sexo, xdf)
tbl44 <- tbl44[c(1,2,6,5,3,4),]

tbl45 <- xtabs(~Trabajo + Edad, xdf)

tbl46 <- xtabs(~Trabajo + Sexo, xdf)
################################
#############SALINAS############

sbl1 <- xtabs(~trabajo + beneficio_aqu.ed. , salinas) 
sbl1 <- sbl1[c(2,3,5,6,4),c(2,3,4,6,5)]
sbl2 <- xtabs(~trabajo + Desventaja_cerca, salinas)
sbl2 <- sbl2[c(2,3,5,6,4),c(2,3,4,5)]
sbl3 <- xtabs(~trabajo + condiciones_considera_salina, salinas)
sbl3 <- sbl3[c(2,3,5,6,4),c(2,3,5,4)]
sbl4 <- xtabs(~trabajo + efectos_condici.f3.n_salina, salinas)
sbl4 <- sbl4[c(2,3,5,6,4),c(2,3,4,5,6)]

sbl5 <- xtabs(~actividad_productiva + beneficio_aqu.ed. , salinas) 
sbl5 <- sbl5[c(2,3,4,6,5),c(2,3,4,6,5)]
sbl6 <- xtabs(~actividad_productiva + Desventaja_cerca, salinas)
sbl6 <- sbl6[c(2,3,4,6,5),c(2,3,4,5)]
sbl7 <- xtabs(~actividad_productiva + condiciones_considera_salina, salinas)
sbl7 <- sbl7[c(2,3,4,6,5),c(2,3,5,4)]
sbl8 <- xtabs(~actividad_productiva + efectos_condici.f3.n_salina, salinas)
sbl8 <- sbl8[c(2,3,4,6,5),c(2,3,4,5,6)]

sbl9 <- xtabs(~uso_salina + beneficio_aqu.ed. , salinas) 
sbl9 <- sbl9[c(2,3,5,4),c(2,3,4,6,5)]
sbl10 <- xtabs(~uso_salina + Desventaja_cerca, salinas)
sbl10 <- sbl10[c(2,3,5,4),c(2,3,4,5)]
sbl11 <- xtabs(~uso_salina + condiciones_considera_salina, salinas)
sbl11 <- sbl11[c(2,3,5,4),c(2,3,5,4)]
sbl12 <- xtabs(~uso_salina + efectos_condici.f3.n_salina, salinas)
sbl12 <- sbl12[c(2,3,5,4),c(2,3,4,5,6)]

#############
sbl13 <- xtabs(~trabajo + beneficio_aqu.ed. + Edad, salinas) 
sbl13 <- sbl13[c(2,3,5,6,4),c(2,3,4,6,5),]
sbl14 <- xtabs(~trabajo + Desventaja_cerca + Edad, salinas)
sbl14 <- sbl14[c(2,3,5,6,4),c(2,3,4,5),]
sbl15 <- xtabs(~trabajo + condiciones_considera_salina + Edad, salinas)
sbl15 <- sbl15[c(2,3,5,6,4),c(2,3,5,4),]
sbl16 <- xtabs(~trabajo + efectos_condici.f3.n_salina + Edad, salinas)
sbl16<- sbl16[c(2,3,5,6,4),c(2,3,4,5,6),]

sbl17 <- xtabs(~actividad_productiva + beneficio_aqu.ed. + Edad, salinas) 
sbl17 <- sbl17[c(2,3,4,6,5),c(2,3,4,6,5),]
sbl18 <- xtabs(~actividad_productiva + Desventaja_cerca + Edad, salinas)
sbl18 <- sbl18[c(2,3,4,6,5),c(2,3,4,5),]
sbl19<- xtabs(~actividad_productiva + condiciones_considera_salina + Edad, salinas)
sbl19 <- sbl19[c(2,3,4,6,5),c(2,3,5,4),]
sbl20 <- xtabs(~actividad_productiva + efectos_condici.f3.n_salina + Edad, salinas)
sbl20 <- sbl20[c(2,3,4,6,5),c(2,3,4,5,6),]

sbl21 <- xtabs(~uso_salina + beneficio_aqu.ed. + Edad, salinas) 
sbl21 <- sbl21[c(2,3,5,4),c(2,3,4,6,5),]
sbl22 <- xtabs(~uso_salina + Desventaja_cerca + Edad, salinas)
sbl22 <- sbl22[c(2,3,5,4),c(2,3,4,5),]
sbl23 <- xtabs(~uso_salina + condiciones_considera_salina + Edad, salinas)
sbl23 <- sbl23[c(2,3,5,4),c(2,3,5,4),]
sbl24 <- xtabs(~uso_salina + efectos_condici.f3.n_salina + Edad, salinas)
sbl24 <- sbl24[c(2,3,5,4),c(2,3,4,5,6),]

sbl25 <- xtabs(~actividad_productiva + trabajo, salinas) 
sbl25 <- sbl25[c(2,3,4,6,5),c(2,3,5,6,4)]
sbl26 <- xtabs(~actividad_productiva + trabajo + Edad, salinas) 
sbl26 <- sbl26[c(2,3,4,6,5),c(2,3,5,6,4),]

sbl27 <- xtabs(~uso_salina + trabajo, salinas) 
sbl27 <- sbl27[c(2,3,5,4),c(2,3,5,6,4)]
sbl28 <- xtabs(~uso_salina + trabajo + Edad, salinas) 
sbl28 <- sbl28[c(2,3,5,4),c(2,3,5,6,4),]

sbl29 <- xtabs(~trabajo + Edad, salinas)
sbl29 <- sbl29[c(2,3,5,6,4),]

sbl30 <- xtabs(~actividad_productiva + Edad, salinas)
sbl30 <- sbl30[c(2,3,4,6,5),]

sbl31 <- xtabs(~uso_salina + Edad, salinas)
sbl31 <- sbl31[c(2,3,5,4),]

##############
###### percepcion #####
pl1 <- xtabs(~tiempo_aqui + riesgo_casa, percep)
pl1 <- pl1[c(4,1,2,3),]

pl2 <- xtabs(~tiempo_aqui + riesgo_calle, percep)
pl2 <- pl2[c(4,1,2,3),]

pl3 <- xtabs(~tiempo_aqui + riesgo_zona, percep)
pl3 <- pl3[c(4,1,2,3),]

pl4 <- xtabs(~tiempo_aqui + riesgo_ciudad, percep)
pl4 <- pl4[c(4,1,2,3),]

pl5 <- xtabs(~tiempo_casa + riesgo_casa, percep)
pl5 <- pl5[c(4,1,2,3),]

pl6 <- xtabs(~tiempo_casa + riesgo_calle, percep)
pl6 <- pl6[c(4,1,2,3),]

pl7 <- xtabs(~tiempo_casa + riesgo_zona, percep)
pl7 <- pl7[c(4,1,2,3),]

pl8 <- xtabs(~tiempo_casa + riesgo_ciudad, percep)
pl8 <- pl8[c(4,1,2,3),]

pl9 <- xtabs(~victima + riesgo_casa, percep)

pl10 <- xtabs(~victima + riesgo_calle, percep)

pl11 <- xtabs(~victima + riesgo_zona, percep)

pl12 <- xtabs(~victima + riesgo_ciudad, percep)

pl13 <- xtabs(~tiempo_aqui + riesgo_casa + Sexo, percep)
pl13 <- pl13[c(4,1,2,3),,]

pl14 <- xtabs(~tiempo_aqui + riesgo_calle + Sexo, percep)
pl14 <- pl14[c(4,1,2,3),,]

pl15 <- xtabs(~tiempo_aqui + riesgo_zona + Sexo, percep)
pl15 <- pl15[c(4,1,2,3),,]

pl16 <- xtabs(~tiempo_aqui + riesgo_ciudad + Sexo, percep)
pl16 <- pl16[c(4,1,2,3),,]

pl17 <- xtabs(~tiempo_casa + riesgo_casa + Sexo, percep)
pl17 <- pl17[c(4,1,2,3),,]

pl18 <- xtabs(~tiempo_casa + riesgo_calle + Sexo, percep)
pl18 <- pl18[c(4,1,2,3),,]

pl19 <- xtabs(~tiempo_casa + riesgo_zona + Sexo, percep)
pl19 <- pl19[c(4,1,2,3),,]

pl20 <- xtabs(~tiempo_casa + riesgo_ciudad + Sexo, percep)
pl20 <- pl20[c(4,1,2,3),,]
pl20
pl21 <- xtabs(~victima + riesgo_casa + Sexo, percep)

pl22 <- xtabs(~victima + riesgo_calle + Sexo, percep)

pl23 <- xtabs(~victima + riesgo_zona+ Sexo, percep)

pl24 <- xtabs(~victima + riesgo_ciudad + Sexo, percep)

#extra
pl25 <- xtabs(~tiempo_aqui + Sexo, percep)
pl25 <- pl25[c(4,1,2,3),]

pl26 <- xtabs(~tiempo_casa + Sexo, percep)
pl26 <- pl26[c(4,1,2,3),]

pl27 <- xtabs(~victima + Sexo, percep)


################## NINGUNO ###########################
nbl1 <- xtabs(~Origen + Edad, xdf)
nbl1 <- nbl1[c(1,2,11,13,15,16),]

nbl2 <- xtabs(~Origen + Sexo, xdf)
nbl2 <- nbl2[c(1,2,11,13,15,16),]

nbl3 <- xtabs(~Escolaridad + Edad, xdf)
nbl3 <- nbl3[c(4,5,6,1,7,2,3),]

nbl4 <- xtabs(~Escolaridad + Sexo, xdf)
nbl4 <- nbl4[c(4,5,6,1,7,2,3),]

nbl5 <- xtabs(~Puesto + Edad, xdf)
nbl5 <- nbl5[c(1,2,6,5,3,4),]

nbl6 <- xtabs(~Puesto + Sexo,xdf)
nbl6 <- nbl6[c(1,2,6,5,3,4),]

nbl7 <- xtabs(~Trabajo + Edad, xdf)

nbl8 <- xtabs(~Trabajo + Sexo,xdf)
