N = nrow(Ejido)
#------------------------------------------------------------------------------------
#1
# En esta calle o zona, Usted participa:
A = sum(Ejido$En.esta.calle.o.zona..Usted.participa..En.eventos.deportivos == 'SI')
B = sum(Ejido$En.esta.calle.o.zona..Usted.participa..En.tandas == 'SI')
C = sum(Ejido$En.esta.calle.o.zona..Usted.participa..En.fiestas == 'SI')
D = sum(Ejido$En.esta.calle.o.zona..Usted.participa..En.iglesia.o.templo == 'SI')
E = sum(Ejido$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
FF = sum(Ejido$En.esta.calle.o.zona..Usted.participa..Para.solucionar.problemas.de.la.comunidad == 'SI')

total = c(A, B, C, D, E, FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D, E,FF)
RESPUESTA = c("Eventos deportivos", "tandas", "fiestas", "iglesia o templo", "Actividades.con.hijos","Para.solucionar.problemas.de.la.comunidad")
dataE1 <- data.frame(n,RESPUESTA,   total)

GPSE1 = graficarPlot(dataE1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )
TPSE1 = graficarTable(dataE1,"Respuesta","Numero de personas", "1. En esta calle o zona, Usted participa:")

#------------------------------------------------------------------------------------
#Usted conoce a sus vecinos:
A = sum(Ejido$Usted.conoce.a.sus.vecinos. == 'SI')
B = sum(Ejido$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.a.los.niños == 'SI')
C = sum(Ejido$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.su.casa == 'SI')

total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)

n = c(A, B, C)

RESPUESTA = c("Conoce a sus vecinos", "Confiaría a los niños", "Confiaría su casa")
dataE2 <- data.frame(n,RESPUESTA,   total)
GPSE2 = graficarPlot(dataE2,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )
TPSE2 = graficarTable(dataE2,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )
#------------------------------------------------------------------------------------
#3
A = sum(Ejido$Usted.conoce.a.sus.vecinos..Participa.con.ellos.para.mejorar.la.seguridad == 'SI')
B = sum(Ejido$Usted.conoce.a.sus.vecinos..Le.interesaría.participar == 'SI')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
n = c(A, B)
RESPUESTA = c("Participa con ellos para mejorar la seguridad", "Interesaría participar con ellos para mejorar la seguridad")
dataE3 <- data.frame(n,RESPUESTA,   total)
GPSE3 = graficarPlot(dataE3,"Respuesta", "Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:" )
TPSE3 = graficarTable(dataE3,"Respuesta","Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:")

#------------------------------------------------------------------------------------
#4 dia y 5 horarios
A = 516 # DOMI
B = 260 # LUNED 
C = 180  #MARTES
D = 268 # MIERCOLES
E =  254 #JUEVES
FF = 340 #VIERNES
G =  588 

total = c(A, B, C, D, E, FF, G)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)

G = as.integer((G/N)*100)

n = c(A, B, C, D, E, FF, G)

RESPUESTA = c(  "DOMINGO", "LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO")
dataE4 <- data.frame(n,RESPUESTA,   total)
GPSE4 = graficarPlot(dataE4 ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")
TPSE4 = graficarTable(dataE4 ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")


#HORARIO 

A = 264
B = 947
C = 136
total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C)
RESPUESTA = c("MAÑANA", "TARDE", "NOCHE")
dataE5 <- data.frame(n,RESPUESTA,   total)

GPSE5 = graficarPlot(dataE5,"Respuesta", "Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad " )
TPSE5 = graficarTable(dataE5,"Respuesta","Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad ")


#-----------------------------------------------------------------------------------
#6
#Cuando hay un delito, en esta calle o zona los vecinos:
A = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.reúnen  == 'SI')
B = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.organizan.para.vigilar  == 'SI')
C = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Intercambian.números.telefónicos  == 'SI')
D = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Forman.un.chat  == 'SI')
E = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Ponen.letreros.de.advertencia  == 'SI')
FF = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Llaman.a.la.policía  == 'SI')
G = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Denuncian.ante.la.autoridad  == 'SI')
H = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Vigilan  == 'SI')
I = sum(Ejido$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Buscan.desquitarse  == 'SI')

total = c(A, B, C, D, E, FF, G, H, I)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
I = as.integer((I/N)*100)
n = c(A, B, C, D, E, FF, G, H, I)
RESPUESTA = c("Se.reúnen", "Se.organizan.para.vigilar", "Intercambian.números.telefónicos", "Forman.un.chat", "Ponen.letreros.de.advertencia", "Llaman.a.la.policía", "Denuncian.ante.la.autoridad", "Vigilan", "Buscan.desquitarse")
dataE6 <- data.frame(n,RESPUESTA,   total)

GPSE6 = graficarPlot(dataE6,"Respuesta", "Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSE6 = graficarTable(dataE6,"Respuesta","Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#7

A = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.casa == 'SI')
B = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.la.calle == 'SI')
C = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.transporte == 'SI')
D = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.negocio == 'SI')
E = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.partes.de.auto == 'SI')
FF = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.vehículo == 'SI')
G = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Balaceras == 'SI')
H = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Cobro.de.piso == 'SI')
I = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Violencia.familiar == 'SI')
J = sum(Ejido$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Peleas.de.gallos == 'SI')

total =  c(A, B, C, D, E, FF, G, H, I,J)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
I = as.integer((I/N)*100)
J = as.integer((J/N)*100)
n = c(A, B, C, D, E, FF, G, H, I,J)

RESPUESTA = c("Robo.en.casa", "Robo.en.la.calle", "Robo.en.transporte", "Robo.en.negocio", "Robo.de.partes.de.auto", "Robo.de.vehículo", "Balaceras", "Cobro.de.piso", "Violencia.familiar", "Peleas.de.gallos" )
dataE7 <- data.frame(n,RESPUESTA,   total)
GPSE7 = graficarPlot(dataE7,"Respuesta", "Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSE7 = graficarTable(dataE7,"Respuesta","Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#8 . 1

A = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 1 )
B = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 2 )
C = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 3 )
D = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 4 )
E = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataE81 <- data.frame(n,RESPUESTA,   total)

GPSE81 = graficarPlot(dataE81,"Respuesta", "Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: " )
TPSE81 = graficarTable(dataE81,"Respuesta","Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: ")

#---------------
# 8.2 
A = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 1 )
B = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 2 )
C = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 3 )
D = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 4 )
E = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataE82 <- data.frame(n,RESPUESTA,   total)

GPSE82 = graficarPlot(dataE82,"Respuesta", "Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: " )
TPSE82 = graficarTable(dataE82,"Respuesta","Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: ")

#---------------

A = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 1 )
B = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 2 )
C = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 3 )
D = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 4 )
E = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataE83 <- data.frame(n,RESPUESTA,   total)

GPSE83 = graficarPlot(dataE83,"Respuesta", "Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: " )
TPSE83 = graficarTable(dataE83,"Respuesta","Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: ")

#---------------

A = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 1 )
B = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 2 )
C = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 3 )
D = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 4 )
E = sum(Ejido$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 5 )
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataE84 <- data.frame(n,RESPUESTA,   total)

GPSE84 = graficarPlot(dataE84,"Respuesta", "Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: " )
TPSE84 = graficarTable(dataE84,"Respuesta","Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: ")

#---------------
#9

A = sum(Ejido$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'SI')
B = sum(Ejido$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'NO')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
n = c(A, B)
RESPUESTA = c("Sí", "No")
dataE9 <- data.frame(n,RESPUESTA,   total)
GPSE9 = graficarPlot(dataE9,"Respuesta", "Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?" )
TPSE9 = graficarTable(dataE9,"Respuesta","Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?")

#-----------------------------------------------------------------------------------
#10 

A = sum(Ejido$En.caso.de.ser.víctima.del.delito..Usted..Llama.a.la.policía == 'SI')
B = sum(Ejido$En.caso.de.ser.víctima.del.delito..Usted..Hace.una.denuncia == 'SI')
C = sum(Ejido$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.sus.vecinos.del.peligro == 'SI')
D = sum(Ejido$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.su.familia.del.peligro == 'SI')
E = sum(Ejido$En.caso.de.ser.víctima.del.delito..Usted..Busca.desquitarse == 'SI')

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("Llama.a.la.policía", "Hace.una.denuncia", "Advierte.a.sus.vecinos.del.peligro", "Advierte.a.su.familia.del.peligro", "Busca.desquitarse")
dataE10 <- data.frame(n,RESPUESTA,   total)

GPSE10 = graficarPlot(dataE10,"Respuesta", "Numero de personas", "10. En caso de ser víctima del delito Usted: " )
TPSE10 = graficarTable(dataE10,"Respuesta","Numero de personas", "10. En caso de ser víctima del delito Usted: ")



#-----------------------------------------------------------------------------------
#11

A = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.pruebas == 'SI')
B = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Considera.que.es.un.delito.de.poca.importancia == 'SI')
C = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Conoce.al.agresor.o.agresores == 'SI')
D = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconfía.de.las.autoridades == 'SI')
E = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Teme.a.que.lo.extorsionen == 'SI')
FF = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.tiempo == 'SI')
G = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Son.trámites.complicados == 'SI')
H = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconoce.dónde.denunciar == 'SI')
I = sum(Ejido$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Aunque.denuncie.no.va.a.pasar.nada == 'SI')

total = c(A, B, C, D,  E, FF, G, H, I )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
I = as.integer((I/N)*100)

n = c(A, B, C, D,  E, FF, G, H, I )
RESPUESTA = c("Falta.de.pruebas", "Considera.que.es.un.delito.de.poca.importancia", "Conoce.al.agresor.o.agresores", "Desconfía.de.las.autoridades", "Teme.a.que.lo.extorsionen", "Falta.de.tiempo", "Son.trámites.complicados", "Desconoce.dónde.denunciar", "Aunque.denuncie.no.va.a.pasar.nada");
dataE11 <- data.frame(n,RESPUESTA,   total)
GPSE11 = graficarPlot(dataE11,"Respuesta", "Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:" )
TPSE11 = graficarTable(dataE11,"Respuesta","Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:")

#-----------------------------------------------------------------------------------
#12

A = sum(Ejido$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
B = sum(Ejido$En.esta.calle.o.zona..Los.vecinos.se.organizan.para.prevenir.delitos == 'SI')
C = sum(Ejido$En.esta.calle.o.zona..Las.personas.son.amables == 'SI')
D = sum(Ejido$En.esta.calle.o.zona..Hay.alguna.persona.que.siempre.ayuda.a.los.demás == 'SI')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n =  c(A, B, C, D)
RESPUESTA = c("Actividades.con.hijos","Prevenir.delitos", "Las.personas.son.amables", "Hay.alguna.persona.que.siempre.ayuda.a.los.demás");
dataE12 <- data.frame(n,RESPUESTA,   total)
GPSE12 = graficarPlot(dataE12,"Respuesta", "Numero de personas", "En esta calle o zona:" )
TPSE12 = graficarTable(dataE12,"Respuesta","Numero de personas", "En esta calle o zona:")


#------------------------------------------------------------------------------------
#En esta calle o zona, hay personas
#13
A = sum(Ejido$En.esta.calle.o.zona.hay.personas..A.las.que.todos.tienen.miedo == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.personas..Que.acosan.a.menores == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.personas..Que.acosan.a.mujeres == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.personas..Que.se.emborrachan.o.se.drogan == 'SI')
E = sum(Ejido$En.esta.calle.o.zona.hay.personas..Que.han.estado.en.la.cárcel == 'SI')
F = sum(Ejido$En.esta.calle.o.zona.hay.personas..Sospechosas == 'SI')
total = c(A, B, C, D,E,  F)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
F = as.integer((F/N)*100)
n =  c(A, B, C, D,E,  F)
RESPUESTA = c("A.las.que.todos.tienen.miedo", "Que.acosan.a.menores",  "Que.acosan.a.mujeres", "Que.se.emborrachan.o.se.drogan", "Que.han.estado.en.la.cárcel", "Sospechosas");
dataE13 <- data.frame(n,RESPUESTA,   total)
GPSE13 = graficarPlot(dataE13,"Respuesta", "Numero de personas", "13. En esta calle o zona, hay personas:" )
TPSE13 = graficarTable(dataE13,"Respuesta","Numero de personas", "13. En esta calle o zona, hay personas:")
#------------------------------------------------------------------------------------
# 14 . En esta calle o zona hay violencia:

A = sum(Ejido$En.esta.calle.o.zona.hay.violencia..Entre.mujeres == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.violencia..Entre.hombres == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.violencia..Entre.familias == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.violencia..Entre.adultos.y.jóvenes == 'SI')
E = sum(Ejido$En.esta.calle.o.zona.hay.violencia..Entre.jóvenes == 'SI')

total = c(A, B, C, D,E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n =  c(A, B, C, D,E)
RESPUESTA = c("Entre mujeres", "Entre hombres" , "Entre familias" , "Entre adultos y jóvenes", "Entre jóvenes");
dataE14 <- data.frame(n,RESPUESTA,   total)
GPSE14 = graficarPlot(dataE14,"Respuesta", "Numero de personas", "14. En esta calle o zona, hay personas:" )
TPSE14 = graficarTable(dataE14,"Respuesta","Numero de personas", "14. En esta calle o zona, hay personas:")



#------------------------------------------------------------------------------------
# 15 . En esta calle o zona hay violencia:

A = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..A.gritos == 'SI')
B = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.golpes == 'SI')
C = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.cuchillos..navajas.o.machetes == 'SI')
D = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.armas.de.fuego..como.pistolas.o.rifles == 'SI')
E = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Desquitándose.del.otro == 'SI')
FF = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Conciliando.con.una.tercera.persona == 'SI')
G = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Dialogando == 'SI')
H = sum(Ejido$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..De.manera.respetuosa == 'SI')
total = c(A, B, C, D,  E, FF, G, H)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
n = c(A, B, C, D,  E, FF, G, H)

RESPUESTA = c("A gritos","Con golpes","Con cuchillos, navajas o machetes","Con armas de fuego, como pistolas o rifles","Desquitándose del otro", "Amistosamente","Dialogando","De manera respetuosa");
dataE15 <- data.frame(n,RESPUESTA,   total)
GPSE15 = graficarPlot(dataE15,"Respuesta", "Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" )
TPSE15 = graficarTable(dataE15,"Respuesta","Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:")



#------------------------------------------------------------------------------------
# 16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:

A = sum(Ejido$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Por.la.inseguridad == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Descuido.de.los.padres == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Castigo == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por la inseguridad","Descuido de los pades","Castigo","Trabajo de los padres");
dataE16 <- data.frame(n,RESPUESTA,   total)
GPSE16 = graficarPlot(dataE16,"Respuesta", "Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" )
TPSE16 = graficarTable(dataE16,"Respuesta","Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:")



#------------------------------------------------------------------------------------
# 17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:

A = sum(Ejido$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Por.descuido.de.los.padres == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Castigo == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Falta.de.dinero == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por descuido de los padres", "Castigo", "Falta de dinero", "Trabajo de los padres");
dataE17 <- data.frame(n,RESPUESTA,   total)
GPSE17 = graficarPlot(dataE17,"Respuesta", "Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " )
TPSE17 = graficarTable(dataE17,"Respuesta","Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ")



#------------------------------------------------------------------------------------
# 18. En esta calle o zona hay jóvenes que:

A = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Hacen.deporte == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Ayudan.a.los.demás == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..La.mayoria.de.los.jovenes.estudian.o.trabajan == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Andan.en.pandillas == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Hacen deporte", "Ayudan a los demás", "La mayoría de los jóvenes estudian o trabajan.","Andan en pandillas");
dataE18 <- data.frame(n,RESPUESTA,   total)
GPSE18 = graficarPlot(dataE18,"Respuesta", "Numero de personas", "18. En esta calle o zona hay jóvenes que: " )
TPSE18 = graficarTable(dataE18,"Respuesta","Numero de personas", "18. En esta calle o zona hay jóvenes que: ")


#------------------------------------------------------------------------------------
#18.1

# 18. En esta calle o zona hay jóvenes que:

A = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Andan.armados == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Destruyen.o.vandalizan.propiedad.ajena == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Son.violentos == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.jóvenes.que..Amenazan.a.los.vecinos == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)

RESPUESTA = c("Andan armados","Destruyen o vandalizan la propiedad ajena", "Son violentos", "Amenazan a los vecinos");
dataE181 <- data.frame(n,RESPUESTA,   total)
GPSE181 = graficarPlot(dataE181,"Respuesta", "Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas" )
TPSE181 = graficarTable(dataE181,"Respuesta","Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas")



#------------------------------------------------------------------------------------
# 19. En esta calle o zona hay un parque


A = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Niños.y.niñas == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Jóvenes == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Pandillas == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Familias == 'SI')
E = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Adultos == 'SI')
FF = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Personas.de.la.tercera.edad == 'SI')
G = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Usted.lo.utiliza == 'SI')
H = sum(Ejido$En.esta.calle.o.zona.hay.un.parque..Vándalos == 'SI')

total = c(A, B, C, D,  E, FF, G, H )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)


n = c(A, B, C, D,  E, FF, G, H )

RESPUESTA = c("Niños y niñas", "Jóvenes", "Pandillas","Familias","Adultos",  "Personas de la tercera edad", "Encuestado", "Vandalos");
dataE19 <- data.frame(n,RESPUESTA,   total)
GPSE19 = graficarPlot(dataE19,"Respuesta", "Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan?" )
TPSE19 = graficarTable(dataE19,"Respuesta","Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan? ")


#------------------------------------------------------------------------------------
# 20. En esta calle o zona hay:


A = sum(Ejido$En.esta.calle.hay..Banquetas == 'SI')
B = sum(Ejido$En.esta.calle.hay..Baches == 'SI')
C = sum(Ejido$En.esta.calle.hay..Letreros.con.nombres.de.las.calles == 'SI')
D = sum(Ejido$En.esta.calle.hay..Tiendita == 'SI')
E = sum(Ejido$En.esta.calle.hay..Alumbrado == 'SI')
FF = sum(Ejido$En.esta.calle.hay..Consumo.del.alcohol.en.la.calle == 'SI')


total = c(A, B, C, D,  E, FF )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)



n = c(A, B, C, D,  E, FF )

RESPUESTA = c("Banquetas","Baches","Letreros con nombres de las calles","Tiendita","Alumbrado","Consumo de alcohol en la calle");
dataE20 <- data.frame(n,RESPUESTA,   total)
GPSE20 = graficarPlot(dataE20,"Respuesta", "Numero de personas", "20. En esta calle o zona hay: " )
TPSE20 = graficarTable(dataE20,"Respuesta","Numero de personas", "20. En esta calle o zona hay:  ")


   

#------------------------------------------------------------------------------------
# 21. En esta calle o zona hay:

A = sum(Ejido$En.esta.zona.hay..Horarios.de.transporte.que.convienen == 'SI')
B = sum(Ejido$En.esta.zona.hay..Una.parada.de.camión.cerca.de.esta.casa == 'SI')
C = sum(Ejido$En.esta.zona.hay..Terrenos.baldíos == 'SI')
D = sum(Ejido$En.esta.zona.hay..Basura == 'SI')
E = sum(Ejido$En.esta.zona.hay..Autos.abandonados == 'SI')
FF = sum(Ejido$En.esta.zona.hay..Casas.abandonadas == 'SI')
G = sum(Ejido$En.esta.zona.hay..Vandalismo == 'SI')
H = sum(Ejido$En.esta.zona.hay..Grafiti == 'SI')
I = sum(Ejido$En.esta.zona.hay..Venta.de.thinner.o.pegamento.a.menores == 'SI')
J = sum(Ejido$En.esta.zona.hay..Venta.de.alcohol.o.cigarros.a.menores == 'SI')
L = sum(Ejido$En.esta.zona.hay..Venta.de.droga == 'SI')
M = sum(Ejido$En.esta.zona.hay..Venta.de.alcohol.después.de.las.11.00.de.la.noche == 'SI')


total = c(A, B, C, D,  E, FF, G, H ,I,J,L,M)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
I = as.integer((I/N)*100)
J = as.integer((J/N)*100)
L = as.integer((L/N)*100)
M = as.integer((M/N)*100)


n = c(A, B, C, D,  E, FF, G, H ,I,J,L,M)

RESPUESTA = c("Horarios de transporte que convienen","Una parada de camión cerca de esta casa","Terrenos baldíos","Basura","Autos abandonados","Casas abandonadas","Vandalismo","Grafiti","Venta de tiner o pegamento a menores","Venta de alcohol o cigarros a menores","Venta de droga","Venta de alcohol después de las 11:00 de la noche");
dataE21 <- data.frame(n,RESPUESTA,   total)
GPSE21 = graficarPlot(dataE21,"Respuesta", "Numero de personas", "21. En esta calle o zona hay: " )
TPSE21 = graficarTable(dataE21,"Respuesta","Numero de personas", "21. En esta calle o zona hay:  ")


#------------------------------------------------------------------------------------
# 22. En el último año Usted supo que algún menor de 18 años:


A = sum(Ejido$Usted.supo.que.algún.menor.de.18.años..Se.fue.de.la.casa == 'SI')
B = sum(Ejido$Usted.supo.que.algún.menor.de.18.años..Sufrió.violencia == 'SI')
C = sum(Ejido$Usted.supo.que.algún.menor.de.18.años..Abandonó.la.escuela == 'SI')
D = sum(Ejido$Usted.supo.que.algún.menor.de.18.años..Tiene.problemas.de.conducta == 'SI')
E = sum(Ejido$Usted.supo.que.algún.menor.de.18.años..Quedó.embarazada == 'SI')



total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("Se fue de la casa", "Sufrió violencia" , "Abandonó la escuela", "Tiene problemas de conducta","Quedó embarazada");
dataE22 <- data.frame(n,RESPUESTA,   total)
GPSE22 = graficarPlot(dataE22,"Respuesta", "Numero de personas", "22. En el último año Usted supo que algún menor de 18 años: " )
TPSE22 = graficarTable(dataE22,"Respuesta","Numero de personas", "22. En el último año Usted supo que algún menor de 18 años:  ")


#------------------------------------------------------------------------------------
# 23. Para corregir a un niño o niña que se porta mal, Usted recomienda:



A = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Castigarle == 'SI')
B = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Gritarle == 'SI')
C = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.nalgadas == 'SI')
D = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.una.golpiza...cueriza == 'SI')
E = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Explicarle.lo.que.está.mal == 'SI')
FF = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Aconsejarle == 'SI')
G = sum(Ejido$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Enseñar.con.el.ejemplo == 'SI')


total = c(A, B, C, D,  E ,FF,G)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
n = c(A, B, C, D,  E, FF,G)

RESPUESTA = c("Castigarle"  , "Gritarle" , "Darle nalgadas" , "Darle una golpiza / cueriza" , "Explicarle lo que está mal" , "Aconsejarle" , "Darle buen ejemplo" );
dataE23 <- data.frame(n,RESPUESTA,   total)
GPSE23 = graficarPlot(dataE23,"Respuesta", "Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda: " )
TPSE23 = graficarTable(dataE23,"Respuesta","Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda:  ")

 
 #------------------------------------------------------------------------------------
# 24. Para corregir a un niño o niña que se porta mal, Usted recomienda:



A = sum(Ejido$En.esta.casa...APLICA.TARJETON..Todos.se.conocen == 'SI')
B = sum(Ejido$En.esta.casa...APLICA.TARJETON..Platican.unos.con.otros == 'SI')
C = sum(Ejido$En.esta.casa...APLICA.TARJETON..Comen.juntos == 'SI')
D = sum(Ejido$En.esta.casa...APLICA.TARJETON..Se.ayudan.con.los.gastos == 'SI')
E = sum(Ejido$En.esta.casa...APLICA.TARJETON..Discuten == 'SI')
FF = sum(Ejido$En.esta.casa...APLICA.TARJETON..Se.gritan.entre.sí == 'SI')
G = sum(Ejido$En.esta.casa...APLICA.TARJETON..Llegan.a.los.golpes == 'SI')
H = sum(Ejido$En.esta.casa...APLICA.TARJETON..Se.ignoran == 'SI')


total = c(A, B, C, D,  E ,FF,G, H)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)

H = as.integer((H/N)*100)
n = c(A, B, C, D,  E, FF,G, H)

RESPUESTA = c("Todos se conocen" , "Platican unos con otros" , "Comen juntos" , "Se ayudan con los gastos" , "Discuten" , "Se gritan entre sí" , "Llegan a los golpes" , "Se ignoran" );
dataE24 <- data.frame(n,RESPUESTA,   total)
GPSE24 = graficarPlot(dataE24,"Respuesta", "Numero de personas", "24. En esta casa:  " )
TPSE24 = graficarTable(dataE24,"Respuesta","Numero de personas", "24. En esta casa:   ")


#------------------------------------------------------------------------------------
# 25. En esta casa alguien: (APLICA TARJETON)

A = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Tiene.discapacidad == 'SI')
B = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Sabe.manejar.armas.de.fuego..como.pistolas.o.rifles == 'SI')
C = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Habla.de.comprar.armas.de.fuego == 'SI')
D = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..No.habla.español == 'SI')
E = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.obesidad == 'SI')

FF = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.fumar == 'SI')
G = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.beber == 'SI')
H = sum(Ejido$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.drogas == 'SI')


total = c(A, B, C, D,  E, FF, G, H )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
n = c(A, B, C, D,  E, FF, G, H )

RESPUESTA = c("Por su discapacidad, ha vivido violencia" , "Sabe manejar armas de fuego, como pistolas o rifles " , "Habla de comprar armas de fuego" , "Habla lengua indigena" , "Necesita ayuda por obesidad" , "Necesita ayuda por fumar" , "Necesita ayuda por beber" , "Necesita ayuda por drogas" );
dataE25 <- data.frame(n,RESPUESTA,   total)
GPSE25 = graficarPlot(dataE25,"Respuesta", "Numero de personas", "25. En esta casa alguien: (APLICA TARJETON) " )
TPSE25 = graficarTable(dataE25,"Respuesta","Numero de personas", "25. En esta casa alguien: (APLICA TARJETON)  ")


#------------------------------------------------------------------------------------
# 26. En el último año, por cuestiones de seguridad Usted ha pensado:

A = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.casa == 'SI')
B = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.ciudad == 'SI')
C = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.estado == 'SI')
D = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.trabajo == 'SI')
E = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cerrar.su.negocio == 'SI')
FF = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiar.a.los.hijos.de.escuela == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cambiarse de casa" , "Cambiarse de ciudad" , "Cambiarse de estado" , "Cambiar de trabajo" , "Cerrar su negocio" , "Cambiar a los hijos de escuela"  );
dataE26 <- data.frame(n,RESPUESTA,   total)
GPSE26 = graficarPlot(dataE26,"Respuesta", "Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado: " )
TPSE26 = graficarTable(dataE26,"Respuesta","Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado:  ")


#------------------------------------------------------------------------------------
# 27. En el último año, por cuestiones de seguridad Usted dejó de:

A = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.de.noche == 'SI')
B = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.a.caminar.o.hacer.ejercicio == 'SI')
C = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Impidió.que.los.niños.salieran.a.la.calle == 'SI')
D = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Evitó.relacionarse.con.nuevas.personas == 'SI')
E = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.visitar.a.parientes.o.amigos == 'SI')
FF = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.transporte.público.combi == 'SI')
G = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.taxi == 'SI')
H = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.llevar.mucho.dinero.en.efectivo == 'SI')
I = sum(Ejido$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.joyas == 'SI')

total = c(A, B, C, D, E, FF, G, H, I)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
G = as.integer((G/N)*100)
H = as.integer((H/N)*100)
I = as.integer((I/N)*100)
n = c(A, B, C, D, E, FF, G, H, I)

RESPUESTA = c("Dejo de salir de noche" , "Dejo de salir a caminar o hacer ejercicio" , "Impidio que los niños salgan a la calle" , "Evito relacionarse con nuevas personas" , "Dejo de visitar a parientes o amigos" , "Dejo de usar transporte público /combi" , "Dejo de usar taxi" , "Dejo de llevar mucho dinero en efectivo" , "Dejo de usar joyas");
dataE27 <- data.frame(n,RESPUESTA,   total)
GPSE27 = graficarPlot(dataE27,"Respuesta", "Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de: " )
TPSE27 = graficarTable(dataE27,"Respuesta","Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de:  ")

#------------------------------------------------------------------------------------
# 28. En esta calle o zona la policía:

A = sum(Ejido$En.esta.calle.o.zona.la.policia.Cuida.o.vigila.bien == 'SI')
B = sum(Ejido$En.esta.calle.o.zona.la.policia.Comete.abusos == 'SI')
C = sum(Ejido$En.esta.calle.o.zona.la.policia.Acude.a.los.llamados == 'SI')
D = sum(Ejido$En.esta.calle.o.zona.la.policia.Pide.mordidas == 'SI')
E = sum(Ejido$En.esta.calle.o.zona.la.policia.Hace.rondines == 'SI')
FF = sum(Ejido$En.esta.calle.o.zona.la.policia.Comete.delitos == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cuida o vigila bien" , "Comete abusos" , "Acude a  los llamados" , "Pide mordidas" , "Hace rondines" , "Comete delitos" );
dataE28 <- data.frame(n,RESPUESTA,   total)
GPSE28 = graficarPlot(dataE28,"Respuesta", "Numero de personas", "28. En esta calle o zona la policía: " )
TPSE28 = graficarTable(dataE28,"Respuesta","Numero de personas", "28. En esta calle o zona la policía:  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE291 <- data.frame(n,RESPUESTA,   total)
GPSE291 = graficarPlot(dataE291,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  " )
TPSE291 = graficarTable(dataE291,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE292 <- data.frame(n,RESPUESTA,   total)
GPSE292 = graficarPlot(dataE292,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  " )
TPSE292 = graficarTable(dataE292,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE293 <- data.frame(n,RESPUESTA,   total)
GPSE293 = graficarPlot(dataE293,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  " )
TPSE293 = graficarTable(dataE293,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE294 <- data.frame(n,RESPUESTA,   total)
GPSE294 = graficarPlot(dataE294,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  " )
TPSE294 = graficarTable(dataE294,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE295 <- data.frame(n,RESPUESTA,   total)
GPSE295 = graficarPlot(dataE295,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  " )
TPSE295 = graficarTable(dataE295,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  ")


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 1)
B = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 2)
C = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 3)
D = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 4)
E = sum(Ejido$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE296 <- data.frame(n,RESPUESTA,   total)
GPSE296 = graficarPlot(dataE296,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  " )
TPSE296 = graficarTable(dataE296,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  ")




#------------------------------------------------------------------------------------
# 30 Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE301 <- data.frame(n,RESPUESTA,   total)
GPSE301 = graficarPlot(dataE301,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia " )
TPSE301 = graficarTable(dataE301,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE302 <- data.frame(n,RESPUESTA,   total)
GPSE302 = graficarPlot(dataE302,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar" )
TPSE302 = graficarTable(dataE302,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE303 <- data.frame(n,RESPUESTA,   total)
GPSE303 = graficarPlot(dataE303,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno" )
TPSE303 = graficarTable(dataE303,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE304 <- data.frame(n,RESPUESTA,   total)
GPSE304 = graficarPlot(dataE304,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal " )
TPSE304 = graficarTable(dataE304,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Su.presidente.municipal
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE305 <- data.frame(n,RESPUESTA,   total)
GPSE305 = graficarPlot(dataE305,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal " )
TPSE305 = graficarTable(dataE305,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

 
total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE306 <- data.frame(n,RESPUESTA,   total)
GPSE306 = graficarPlot(dataE306,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador " )
TPSE306 = graficarTable(dataE306,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE311 <- data.frame(n,RESPUESTA,   total)
GPSE311 = graficarPlot(dataE311,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia " )
TPSE311 = graficarTable(dataE311,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE312 <- data.frame(n,RESPUESTA,   total)
GPSE312 = graficarPlot(dataE312,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar " )
TPSE312 = graficarTable(dataE312,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar ")
#-----------------------------2------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE313 <- data.frame(n,RESPUESTA,   total)
GPSE313 = graficarPlot(dataE313,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno " )
TPSE313 = graficarTable(dataE313,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE314 <- data.frame(n,RESPUESTA,   total)
GPSE314 = graficarPlot(dataE314,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal " )
TPSE314 = graficarTable(dataE314,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal ")
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE315 <- data.frame(n,RESPUESTA,   total)
GPSE315 = graficarPlot(dataE315,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal " )
TPSE315 = graficarTable(dataE315,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Ejido$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataE316 <- data.frame(n,RESPUESTA,   total)
GPSE316 = graficarPlot(dataE316,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador " )
TPSE316 = graficarTable(dataE316,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador ")







