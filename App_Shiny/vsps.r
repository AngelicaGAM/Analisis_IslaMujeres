Can = dataC1$n
Eji = dataE1$n 
Isl = dataI1$n 
RESP = dataI1$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)
GPSVS1 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )
##GPSVS1 = graficarTableGroup(A1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )


Can = dataC6$n
Eji = dataE6$n 
Isl = dataI6$n 
RESP = dataI6$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS6 = graficarPlotGroup(A1,"Respuesta", "Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:" )
##TPSVS6 = graficarTableGroup(A1,"Respuesta","Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:")


Can = dataC7$n
Eji = dataE7$n 
Isl = dataI7$n 
RESP = dataI7$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS7 = graficarPlotGroup( A1, "Respuesta", "Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:" )
##TPSVS7 = graficarTableGroup( A1, "Respuesta","Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:")

Can = dataC81$n
Eji = dataE81$n 
Isl = dataI81$n 
RESP = dataI81$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS81 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: " )
##TPSVS81 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: ")

#---------------
# 8.2 

Can = dataC82$n
Eji = dataE82$n 
Isl = dataI82$n 
RESP = dataI82$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS82 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: " )
##TPSVS82 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: ")

#---------------

Can = dataC83$n
Eji = dataE83$n 
Isl = dataI83$n 
RESP = dataI83$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS83 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: " )
##TPSVS83 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: ")

#---------------

Can = dataC84$n
Eji = dataE84$n 
Isl = dataI84$n 
RESP = dataI84$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS84 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: " )
##TPSVS84 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: ")

#---------------
#9
Can = dataC9$n
Eji = dataE9$n 
Isl = dataI9$n 
RESP = dataI9$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS9 = graficarPlotGroup( A1, "Respuesta", "Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?" )
##TPSVS9 = graficarTableGroup( A1, "Respuesta","Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?")

#-----------------------------------------------------------------------------------
#10 

Can = dataC10$n
Eji = dataE10$n 
Isl = dataI10$n 
RESP = dataI10$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS10 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "10. En caso de ser víctima del delito Usted: " )
##TPSVS10 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "10. En caso de ser víctima del delito Usted: ")



#-----------------------------------------------------------------------------------
#11

Can = dataC11$n
Eji = dataE11$n 
Isl = dataI11$n 
RESP = dataI11$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS11 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:" )
#TPSVS11 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:")

#-----------------------------------------------------------------------------------
#12

Can = dataC12$n
Eji = dataE12$n 
Isl = dataI12$n 
RESP = dataI12$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS12 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "En esta calle o zona:" )
#TPSVS12 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "En esta calle o zona:")


#------------------------------------------------------------------------------------
#En esta calle o zona, hay personas
#13
Can = dataC13$n
Eji = dataE13$n 
Isl = dataI13$n 
RESP = dataI13$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS13 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "13. En esta calle o zona, hay personas:" )
#TPSVS13 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "13. En esta calle o zona, hay personas:")
#------------------------------------------------------------------------------------
# 14 . En esta calle o zona hay violencia:


Can = dataC14$n
Eji = dataE14$n 
Isl = dataI14$n 
RESP = dataI14$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS14 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "14. En esta calle o zona, hay personas:" )
#TPSVS14 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "14. En esta calle o zona, hay personas:")



#------------------------------------------------------------------------------------
# 15 . En esta calle o zona hay violencia:


Can = dataC15$n
Eji = dataE15$n 
Isl = dataI15$n 
RESP = dataI15$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS15 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" )
#TPSVS15 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:")



#------------------------------------------------------------------------------------
# 16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:

Can = dataC16$n
Eji = dataE16$n 
Isl = dataI16$n 
RESP = dataI16$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS16 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:" )
#TPSVS16 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "16. En esta calle o zona hay niños o adolesVSentes que se quedan encerrados con llave:")



#------------------------------------------------------------------------------------
# 17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:

Can = dataC17$n
Eji = dataE17$n 
Isl = dataI17$n 
RESP = dataI17$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS17 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " )
#TPSVS17 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ")



#------------------------------------------------------------------------------------
# 18. En esta calle o zona hay jóvenes que:

Can = dataC18$n
Eji = dataE18$n 
Isl = dataI18$n 
RESP = dataI18$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS18 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "18. En esta calle o zona hay jóvenes que: " )
#TPSVS18 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "18. En esta calle o zona hay jóvenes que: ")


#------------------------------------------------------------------------------------
#18.1

# 18. En esta calle o zona hay jóvenes que:
Can = dataC181$n
Eji = dataE181$n 
Isl = dataI181$n 
RESP = dataI181$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS181 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas" )
#TPSVS181 = graficarTableGroup( A1,"Respuesta","Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas")



#------------------------------------------------------------------------------------
# 19. En esta calle o zona hay un parque


Can = dataC19$n
Eji = dataE19$n 
Isl = dataI19$n 
RESP = dataI19$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS19 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan?" )
#TPSVS19 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan? ")


#------------------------------------------------------------------------------------
# 20. En esta calle o zona hay:


Can = dataC20$n
Eji = dataE20$n 
Isl = dataI20$n 
RESP = dataI20$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS20 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "20. En esta calle o zona hay: " )
#TPSVS20 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "20. En esta calle o zona hay:  ")




#------------------------------------------------------------------------------------
# 21. En esta calle o zona hay:

Can = dataC21$n
Eji = dataE21$n 
Isl = dataI21$n 
RESP = dataI21$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS21 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "21. En esta calle o zona hay: " )
#TPSVS21 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "21. En esta calle o zona hay:  ")


#------------------------------------------------------------------------------------
# 22. En el último año Usted supo que algún menor de 18 años:

Can = dataC22$n
Eji = dataE22$n 
Isl = dataI22$n 
RESP = dataI22$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS22 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "22. En el último año Usted supo que algún menor de 18 años: " )
#TPSVS22 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "22. En el último año Usted supo que algún menor de 18 años:  ")


#------------------------------------------------------------------------------------
# 23. Para corregir a un niño o niña que se porta mal, Usted recomienda:

Can = dataC23$n
Eji = dataE23$n 
Isl = dataI23$n 
RESP = dataI23$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS23 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda: " )
#TPSVS23 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda:  ")


#------------------------------------------------------------------------------------
# 25. En esta casa alguien: (APLICA TARJETON)
Can = dataC25$n
Eji = dataE25$n 
Isl = dataI25$n 
RESP = dataI25$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS25 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "25. En esta casa alguien: (APLICA TARJETON) " )
#TPSVS25 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "25. En esta casa alguien: (APLICA TARJETON)  ")


#------------------------------------------------------------------------------------
# 26. En el último año, por cuestiones de seguridad Usted ha pensado:

Can = dataC26$n
Eji = dataE26$n 
Isl = dataI26$n 
RESP = dataI26$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS26 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado: " )
#TPSVS26 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado:  ")


#------------------------------------------------------------------------------------
# 27. En el último año, por cuestiones de seguridad Usted dejó de:

Can = dataC27$n
Eji = dataE27$n 
Isl = dataI27$n 
RESP = dataI27$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS27 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de: " )
#TPSVS27 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de:  ")

#------------------------------------------------------------------------------------
# 28. En esta calle o zona la policía:

Can = dataC28$n
Eji = dataE28$n 
Isl = dataI28$n 
RESP = dataI28$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS28 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "28. En esta calle o zona la policía: " )
#TPSVS28 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "28. En esta calle o zona la policía:  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC291$n
Eji = dataE291$n 
Isl = dataI291$n 
RESP = dataI291$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS291 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  " )
#TPSVS291 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC292$n
Eji = dataE292$n 
Isl = dataI292$n 
RESP = dataI292$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS292 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  " )
#TPSVS292 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC293$n
Eji = dataE293$n 
Isl = dataI293$n 
RESP = dataI293$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS293 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  " )

#TPSVS293 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC294$n
Eji = dataE294$n 
Isl = dataI294$n 
RESP = dataI294$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS294 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  " )
#TPSVS294 = graficarTableGroup( A1,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC295$n
Eji = dataE295$n 
Isl = dataI295$n 
RESP = dataI295$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS295 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  " )
#TPSVS295 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  ")


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 


Can = dataC296$n
Eji = dataE296$n 
Isl = dataI296$n 
RESP = dataI296$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS296 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  " )
#TPSVS296 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  ")




#------------------------------------------------------------------------------------
# 30 Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

Can = dataC301$n
Eji = dataE301$n 
Isl = dataI301$n 
RESP = dataI301$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS301 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia " )
#TPSVS301 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar

Can = dataC302$n
Eji = dataE302$n 
Isl = dataI302$n 
RESP = dataI302$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS302 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar" )
#TPSVS302 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno

Can = dataC303$n
Eji = dataE303$n 
Isl = dataI303$n 
RESP = dataI303$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS303 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno" )
#TPSVS303 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal 


Can = dataC304$n
Eji = dataE304$n 
Isl = dataI304$n 
RESP = dataI304$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS304 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal " )
#TPSVS304 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Su.presidente.municipal
Can = dataC305$n
Eji = dataE305$n 
Isl = dataI305$n 
RESP = dataI305$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS305 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal " )
#TPSVS305 = graficarTableGroup( A1,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  
Can = dataC306$n
Eji = dataE306$n 
Isl = dataI306$n 
RESP = dataI306$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS306 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador " )
#TPSVS306 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC311$n
Eji = dataE311$n 
Isl = dataI311$n 
RESP = dataI311$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS311 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia " )
#TPSVS311 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC312$n
Eji = dataE312$n 
Isl = dataI312$n 
RESP = dataI312$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS312 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar " )
#TPSVS312 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar ")
#-----------------------------2------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC313$n
Eji = dataE313$n 
Isl = dataI313$n 
RESP = dataI313$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS313 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno " )
#TPSVS313 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
Can = dataC314$n
Eji = dataE314$n 
Isl = dataI314$n 
RESP = dataI314$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)


GPSVS314 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal " )
#TPSVS314 = graficarTableGroup( A1,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal ")
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 

Can = dataC315$n
Eji = dataE315$n 
Isl = dataI315$n 
RESP = dataI315$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS315 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal " )
#TPSVS315 = graficarTableGroup( A1 ,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 

Can = dataC316$n
Eji = dataE316$n 
Isl = dataI316$n 
RESP = dataI316$RESPUESTA
A1 <- data.frame(Can,Eji,Isl, RESP)

GPSVS316 = graficarPlotGroup( A1 ,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador " )
#TPSVS316 = graficarTableGroup( A1,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador ")







