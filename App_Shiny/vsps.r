Can = dataC1$n
Eji = dataE1$n
Isl = dataI1$n 
RESP = dataI1$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS1 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )

Can = dataC1
Eji = dataE1
Isl = dataI1 
RESP = dataI1$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS1 =   graficarTableGroup(dataT,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )

#------------------------------------------------------------------------------------
#Usted conoce a sus vecinos:
Can = dataC2$n
Eji = dataE2$n
Isl = dataI2$n 
RESP = dataI2$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS2 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )


Can = dataC2
Eji = dataE2
Isl = dataI2 
RESP = dataI2$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS2 = graficarTableGroup(dataT,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )

#------------------------------------------------------------------------------------
#3
Can = dataC3$n
Eji = dataE3$n
Isl = dataI3$n 
RESP = dataI3$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS3 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:" )

Can = dataC3
Eji = dataE3
Isl = dataI3 
RESP = dataI3$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS3 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:")

#------------------------------------------------------------------------------------
#4 dia y 5 horarios

Can = dataC4$n
Eji = dataE4$n
Isl = dataI4$n 
RESP = dataI4$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS4 = graficarPlotGroup(A1 ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")

Can = dataC4
Eji = dataE4
Isl = dataI4 
RESP = dataI4$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS4 = graficarTableGroup(dataT ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")

#---------------------------------------------------------------------
Can = dataC5$n
Eji = dataE5$n
Isl = dataI5$n 
RESP = dataE5$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS5 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad " )

Can = dataC5
Eji = dataE5
Isl = dataI5 
RESP = dataI5$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS5 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad ")

# ------------------------------------------------------------------------


Can = dataC6$n
Eji = dataE6$n 
Isl = dataI6$n 
RESP = dataI6$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS6 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:" )

Can = dataC6
Eji = dataE6 
Isl = dataI6 
RESP = dataI6$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS6 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:")
# ------------------------------------------------------------------------

Can = dataC7$n
Eji = dataE7$n 
Isl = dataI7$n 
RESP = dataI7$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS7 = graficarPlotGroup( A1, "Respuesta", "Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:" )

Can = dataC7
Eji = dataE7 
Isl = dataI7 
RESP = dataI7$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS7 = graficarTableGroup( dataT, "Respuesta","Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:")

# ------------------------------------------------------------------------
Can = dataC81$n
Eji = dataE81$n 
Isl = dataI81$n 
RESP = dataI81$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS81 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: " )

Can = dataC81
Eji = dataE81 
Isl = dataI81 
RESP = dataI81$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS81 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: ")
# ------------------------------------------------------------------------
#---------------
# 8.2 

Can = dataC82$n
Eji = dataE82$n 
Isl = dataI82$n 
RESP = dataI82$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS82 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: " )
Can = dataC82
Eji = dataE82 
Isl = dataI82 
RESP = dataI82$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS82 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: ")
# ------------------------------------------------------------------------
#---------------

Can = dataC83$n
Eji = dataE83$n 
Isl = dataI83$n 
RESP = dataI83$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS83 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: " )

Can = dataC83
Eji = dataE83 
Isl = dataI83 
RESP = dataI83$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS83 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: ")
# ------------------------------------------------------------------------
#---------------

Can = dataC84$n
Eji = dataE84$n 
Isl = dataI84$n 
RESP = dataI84$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS84 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: " )

Can = dataC84
Eji = dataE84 
Isl = dataI84 
RESP = dataI84$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS84 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: ")
# ------------------------------------------------------------------------
#---------------
#9
Can = dataC9$n
Eji = dataE9$n 
Isl = dataI9$n 
RESP = dataI9$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS9 = graficarPlotGroup( A1, "Respuesta", "Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?" )

Can = dataC9
Eji = dataE9 
Isl = dataI9 
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS9 = graficarTableGroup(dataT, "Respuesta","Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?")
# ------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#10 

Can = dataC10$n
Eji = dataE10$n 
Isl = dataI10$n 
RESP = dataI10$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS10 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "10. En caso de ser víctima del delito Usted: " )

Can = dataC10
Eji = dataE10 
Isl = dataI10 
RESP = dataI10$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS10 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "10. En caso de ser víctima del delito Usted: ")
# ------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
#11

Can = dataC11$n
Eji = dataE11$n 
Isl = dataI11$n 
RESP = dataI11$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS11 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:" )


Can = dataC11
Eji = dataE11 
Isl = dataI11 
RESP = dataI11$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS11 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:")
# ------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#12

Can = dataC12$n
Eji = dataE12$n 
Isl = dataI12$n 
RESP = dataI12$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS12 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "En esta calle o zona:" )

Can = dataC12
Eji = dataE12 
Isl = dataI12 
RESP = dataI12$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS12 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "En esta calle o zona:")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#En esta calle o zona, hay personas
#13
Can = dataC13$n
Eji = dataE13$n 
Isl = dataI13$n 
RESP = dataI13$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS13 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "13. En esta calle o zona, hay personas:" )

Can = dataC13
Eji = dataE13 
Isl = dataI13 
RESP = dataI13$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS13 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "13. En esta calle o zona, hay personas:")
#------------------------------------------------------------------------------------
# 14 . En es# ------------------------------------------------------------------------ta calle o zona hay violencia:


Can = dataC14$n
Eji = dataE14$n 
Isl = dataI14$n 
RESP = dataI14$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS14 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "14. En esta calle o zona, hay personas:" )

Can = dataC14
Eji = dataE14 
Isl = dataI14 
RESP = dataI14$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS14 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "14. En esta calle o zona, hay personas:")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 15 . En esta calle o zona hay violencia:


Can = dataC15$n
Eji = dataE15$n 
Isl = dataI15$n 
RESP = dataI15$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS15 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" )

Can = dataC15
Eji = dataE15 
Isl = dataI15 
RESP = dataI15$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS15 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:

Can = dataC16$n
Eji = dataE16$n 
Isl = dataI16$n 
RESP = dataI16$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS16 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:" )

Can = dataC16
Eji = dataE16 
Isl = dataI16 
RESP = dataI16$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS16 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:

Can = dataC17$n
Eji = dataE17$n 
Isl = dataI17$n 
RESP = dataI17$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS17 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " )

Can = dataC17
Eji = dataE17 
Isl = dataI17 
RESP = dataI17$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS17 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 18. En esta calle o zona hay jóvenes que:

Can = dataC18$n
Eji = dataE18$n 
Isl = dataI18$n 
RESP = dataI18$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS18 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "18. En esta calle o zona hay jóvenes que: " )

Can = dataC18
Eji = dataE18 
Isl = dataI18 
RESP = dataI18$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS18 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "18. En esta calle o zona hay jóvenes que: ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#18.1

# 18. En esta calle o zona hay jóvenes que:
Can = dataC181$n
Eji = dataE181$n 
Isl = dataI181$n 
RESP = dataI181$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS181 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas" )

Can = dataC181
Eji = dataE181 
Isl = dataI181 
RESP = dataI181$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
PSVS181 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 19. En esta calle o zona hay un parque


Can = dataC19$n
Eji = dataE19$n 
Isl = dataI19$n 
RESP = dataI19$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS19 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan?" )

Can = dataC19
Eji = dataE19 
Isl = dataI19 
RESP = dataI19$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS19 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan? ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 20. En esta calle o zona hay:


Can = dataC20$n
Eji = dataE20$n 
Isl = dataI20$n 
RESP = dataI20$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS20 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "20. En esta calle o zona hay: " )

Can = dataC20
Eji = dataE20 
Isl = dataI20 
RESP = dataI20$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS20 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "20. En esta calle o zona hay:  ")
# ------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# 21. En esta calle o zona hay:

Can = dataC21$n
Eji = dataE21$n 
Isl = dataI21$n 
RESP = dataI21$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS21 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "21. En esta calle o zona hay: " )

Can = dataC21
Eji = dataE21 
Isl = dataI21 
RESP = dataI21$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS21 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "21. En esta calle o zona hay:  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 22. En el último año Usted supo que algún menor de 18 años:

Can = dataC22$n
Eji = dataE22$n 
Isl = dataI22$n 
RESP = dataI22$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS22 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "22. En el último año Usted supo que algún menor de 18 años: " )

Can = dataC22
Eji = dataE22 
Isl = dataI22 
RESP = dataI22$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS22 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "22. En el último año Usted supo que algún menor de 18 años:  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 23. Para corregir a un niño o niña que se porta mal, Usted recomienda:

Can = dataC23$n
Eji = dataE23$n 
Isl = dataI23$n 
RESP = dataI23$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS23 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda: " )

Can = dataC23
Eji = dataE23 
Isl = dataI23 
RESP = dataI23$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS23 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda:  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 25. En esta casa alguien: (APLICA TARJETON)
Can = dataC25$n
Eji = dataE25$n 
Isl = dataI25$n 
RESP = dataI25$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS25 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "25. En esta casa alguien: (APLICA TARJETON) " )

Can = dataC25
Eji = dataE25 
Isl = dataI25 
RESP = dataI25$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS25 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "25. En esta casa alguien: (APLICA TARJETON)  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 26. En el último año, por cuestiones de seguridad Usted ha pensado:

Can = dataC26$n
Eji = dataE26$n 
Isl = dataI26$n 
RESP = dataI26$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS26 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado: " )

Can = dataC26
Eji = dataE26 
Isl = dataI26 
RESP = dataI26$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS26 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado:  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 27. En el último año, por cuestiones de seguridad Usted dejó de:

Can = dataC27$n
Eji = dataE27$n 
Isl = dataI27$n 
RESP = dataI27$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS27 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de: " )

Can = dataC27
Eji = dataE27 
Isl = dataI27 
RESP = dataI27$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS27 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de:  ")
# ------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# 28. En esta calle o zona la policía:

Can = dataC28$n
Eji = dataE28$n 
Isl = dataI28$n 
RESP = dataI28$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS28 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "28. En esta calle o zona la policía: " )

Can = dataC28
Eji = dataE28 
Isl = dataI28 
RESP = dataI28$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS28 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "28. En esta calle o zona la policía:  ")
# ------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC291$n
Eji = dataE291$n 
Isl = dataI291$n 
RESP = dataI291$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS291 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  " )


Can = dataC291
Eji = dataE291 
Isl = dataI291 
RESP = dataI291$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS291 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  ")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC292$n
Eji = dataE292$n 
Isl = dataI292$n 
RESP = dataI292$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS292 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  " )

Can = dataC292
Eji = dataE292 
Isl = dataI292 
RESP = dataI292$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS292 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  ")
# ------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC293$n
Eji = dataE293$n 
Isl = dataI293$n 
RESP = dataI293$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS293 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  " )


Can = dataC293
Eji = dataE293 
Isl = dataI293 
RESP = dataI293$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS293 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  ")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC294$n
Eji = dataE294$n 
Isl = dataI294$n 
RESP = dataI294$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS294 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  " )

Can = dataC294
Eji = dataE294 
Isl = dataI294 
RESP = dataI294$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS294 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  ")
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC295$n
Eji = dataE295$n 
Isl = dataI295$n 
RESP = dataI295$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS295 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  " )

Can = dataC295
Eji = dataE295 
Isl = dataI295 
RESP = dataI295$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS295 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC296$n
Eji = dataE296$n 
Isl = dataI296$n 
RESP = dataI296$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS296 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  " )

Can = dataC296
Eji = dataE296 
Isl = dataI296 
RESP = dataI296$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS296 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  ")
# ------------------------------------------------------------------------



#------------------------------------------------------------------------------------
# 30 Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC301$n
Eji = dataE301$n 
Isl = dataI301$n 
RESP = dataI301$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS301 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia " )

Can = dataC301
Eji = dataE301 
Isl = dataI301 
RESP = dataI301$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS301 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar

Can = dataC302$n
Eji = dataE302$n 
Isl = dataI302$n 
RESP = dataI302$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS302 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar" )

Can = dataC302
Eji = dataE302 
Isl = dataI302 
RESP = dataI302$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS302 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno

Can = dataC303$n
Eji = dataE303$n 
Isl = dataI303$n 
RESP = dataI303$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS303 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno" )

Can = dataC303
Eji = dataE303 
Isl = dataI303 
RESP = dataI303$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS303 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal 


Can = dataC304$n
Eji = dataE304$n 
Isl = dataI304$n 
RESP = dataI304$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS304 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal " )

Can = dataC304
Eji = dataE304 
Isl = dataI304 
RESP = dataI304$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS304 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Su.presidente.municipal
Can = dataC305$n
Eji = dataE305$n 
Isl = dataI305$n 
RESP = dataI305$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS305 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal " )

Can = dataC305
Eji = dataE305 
Isl = dataI305 
RESP = dataI305$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS305 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  
Can = dataC306$n
Eji = dataE306$n 
Isl = dataI306$n 
RESP = dataI306$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS306 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador " )

Can = dataC306
Eji = dataE306 
Isl = dataI306 
RESP = dataI306$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS306 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC311$n
Eji = dataE311$n 
Isl = dataI311$n 
RESP = dataI311$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS311 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia " )

Can = dataC311
Eji = dataE311 
Isl = dataI311 
RESP = dataI311$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS311 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia ")
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC312$n
Eji = dataE312$n 
Isl = dataI312$n 
RESP = dataI312$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS312 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar " )

Can = dataC312
Eji = dataE312 
Isl = dataI312 
RESP = dataI312$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS312 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar ")
#-----------------------------2------------------------------------------------------
# 31 Del 1 al# ------------------------------------------------------------------------ 5, califique el trato que recibe de: 
Can = dataC313$n
Eji = dataE313$n 
Isl = dataI313$n 
RESP = dataI313$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS313 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno " )

Can = dataC313
Eji = dataE313 
Isl = dataI313 
RESP = dataI313$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS313 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno ")
# ------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC314$n
Eji = dataE314$n 
Isl = dataI314$n 
RESP = dataI314$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS314 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal " )

Can = dataC314
Eji = dataE314 
Isl = dataI314 
RESP = dataI314$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS314 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal ")
#------------------------------------------------------------------------------------
# 31 Del 1 al# ------------------------------------------------------------------------ 5, califique el trato que recibe de: 

Can = dataC315$n
Eji = dataE315$n 
Isl = dataI315$n 
RESP = dataI315$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS315 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal " )

Can = dataC315
Eji = dataE315 
Isl = dataI315 
RESP = dataI315$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS315 = graficarTableGroup(dataT ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal ")
# ------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 

Can = dataC316$n
Eji = dataE316$n 
Isl = dataI316$n 
RESP = dataI316$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS316 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador " )

Can = dataC316
Eji = dataE316 
Isl = dataI316 
RESP = dataI316$RESPUESTA
Isl['loc'] = 'Isla'
Can['loc'] = 'Cancún'
Eji['loc'] = 'Ejido'
A1 <- data.frame(Can,Eji,Isl, RESP)
data = merge(Isl, Can , all = TRUE)
data1 = merge(Eji, data ,  all = TRUE)
dataT = arrange(data1, RESPUESTA)
TPSVS316 = graficarTableGroup(dataT,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador ")
# ------------------------------------------------------------------------






