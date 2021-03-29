library(vcd)
library(shiny)

#graficas
mosaics <- function(tbl, rot, offs){
  vcd::mosaic(tbl,gp = shading_Friendly2,labeling = labeling_border(rot_labels = rot, 
                                                         just_labels = c("left", 
                                                                         "left", 
                                                                         "left", 
                                                                         "left"),
                                                         offset_labels = offs,
                                                         gp_labels = gpar(fontsize = 10),
                                                         gp_varnames = gpar(fontsize = 0, fontface = 2)))
}
mosaics2 <- function(tbl, rot, offs){
  vcd::mosaic(tbl,gp = shading_Friendly2,direction = TRUE,labeling = labeling_border(rot_labels = rot, 
                                                        just_labels = c("left", 
                                                                        "left", 
                                                                        "left", 
                                                                        "left"),
                                                        offset_labels = offs,
                                                        abbreviate_labs = TRUE,
                                                        gp_labels = gpar(fontsize = 10),
                                                        gp_varnames = gpar(fontsize = 0, fontface = 2)))
}

xdf <- read.csv("x.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")

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



