N = nrow(Isla)
#------------------------------------------------------------------------------------
#1
# En esta calle o zona, Usted participa:
A = sum(Isla$En.esta.calle.o.zona..Usted.participa..En.eventos.deportivos == 'SI')
B = sum(Isla$En.esta.calle.o.zona..Usted.participa..En.tandas == 'SI')
C = sum(Isla$En.esta.calle.o.zona..Usted.participa..En.fiestas == 'SI')
D = sum(Isla$En.esta.calle.o.zona..Usted.participa..En.iglesia.o.templo == 'SI')
E = sum(Isla$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
FF = sum(Isla$En.esta.calle.o.zona..Usted.participa..Para.solucionar.problemas.de.la.comunidad == 'SI')

total = c(A, B, C, D, E, FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D, E,FF)
RESPUESTA = c("Eventos deportivos", "tandas", "fiestas", "iglesia o templo", "Actividades.con.hijos","Para.solucionar.problemas.de.la.comunidad")
dataI1 <- data.frame(n,RESPUESTA,   total)

GPSI1 = graficarPlot(dataI1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )
TPSI1 = graficarTable(dataI1,"Respuesta","Numero de personas", "1. En esta calle o zona, Usted participa:")

#------------------------------------------------------------------------------------
#Usted conoce a sus vecinos:
A = sum(Isla$Usted.conoce.a.sus.vecinos. == 'SI')
B = sum(Isla$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.a.los.niños == 'SI')
C = sum(Isla$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.su.casa == 'SI')

total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)

n = c(A, B, C)

RESPUESTA = c("Conoce a sus vecinos", "Confiaría a los niños", "Confiaría su casa")
dataI2 <- data.frame(n,RESPUESTA,   total)
GPSI2 = graficarPlot(dataI2,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )
GPSI2 = graficarPlot(dataI2,"Respuesta", "Numero de personas", "2. Usted conoce a sus vecinos:" )
#------------------------------------------------------------------------------------
#3
A = sum(Isla$Usted.conoce.a.sus.vecinos..Participa.con.ellos.para.mejorar.la.seguridad == 'SI')
B = sum(Isla$Usted.conoce.a.sus.vecinos..Le.interesaría.participar == 'SI')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
n = c(A, B)
RESPUESTA = c("Participa con ellos para mejorar la seguridad", "Interesaría participar con ellos para mejorar la seguridad")
dataI3 <- data.frame(n,RESPUESTA,   total)
GPSI3 = graficarPlot(dataI3,"Respuesta", "Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:" )
TPSI3 = graficarTable(dataI3,"Respuesta","Numero de personas", "3. Participa con la autoridad para mejorar la seguridad:")

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
dataI4 <- data.frame(n,RESPUESTA,   total)
GPSI4 = graficarPlot(dataI4 ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")
TPSI4 = graficarTable(dataI4 ,"Respuesta", "Numero de personas",  "4. Día en que podría participar en actividades con la autoridad*")


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
dataI5 <- data.frame(n,RESPUESTA,   total)

GPSI5 = graficarPlot(dataI5,"Respuesta", "Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad " )
TPSI5 = graficarTable(dataI5,"Respuesta","Numero de personas", "5. Horarios en los que podría participar en actividades con la autoridad ")


#-----------------------------------------------------------------------------------
#6
#Cuando hay un delito, en esta calle o zona los vecinos:
A = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.reúnen  == 'SI')
B = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.organizan.para.vigilar  == 'SI')
C = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Intercambian.números.telefónicos  == 'SI')
D = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Forman.un.chat  == 'SI')
E = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Ponen.letreros.de.advertencia  == 'SI')
FF = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Llaman.a.la.policía  == 'SI')
G = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Denuncian.ante.la.autoridad  == 'SI')
H = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Vigilan  == 'SI')
I = sum(Isla$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Buscan.desquitarse  == 'SI')

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
dataI6 <- data.frame(n,RESPUESTA,   total)

GPSI6 = graficarPlot(dataI6,"Respuesta", "Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSI6 = graficarTable(dataI6,"Respuesta","Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#7

A = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.casa == 'SI')
B = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.la.calle == 'SI')
C = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.transporte == 'SI')
D = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.negocio == 'SI')
E = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.partes.de.auto == 'SI')
FF = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.vehículo == 'SI')
G = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Balaceras == 'SI')
H = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Cobro.de.piso == 'SI')
I = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Violencia.familiar == 'SI')
J = sum(Isla$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Peleas.de.gallos == 'SI')

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
dataI7 <- data.frame(n,RESPUESTA,   total)
GPSI7 = graficarPlot(dataI7,"Respuesta", "Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSI7 = graficarTable(dataI7,"Respuesta","Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#8 . 1

A = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 1 )
B = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 2 )
C = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 3 )
D = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 4 )
E = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataI81 <- data.frame(n,RESPUESTA,   total)

GPSI81 = graficarPlot(dataI81,"Respuesta", "Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: " )
TPSI81 = graficarTable(dataI81,"Respuesta","Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: ")

#---------------
# 8.2 
A = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 1 )
B = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 2 )
C = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 3 )
D = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 4 )
E = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataI82 <- data.frame(n,RESPUESTA,   total)

GPSI82 = graficarPlot(dataI82,"Respuesta", "Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: " )
TPSI82 = graficarTable(dataI82,"Respuesta","Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: ")

#---------------

A = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 1 )
B = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 2 )
C = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 3 )
D = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 4 )
E = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataI83 <- data.frame(n,RESPUESTA,   total)

GPSI83 = graficarPlot(dataI83,"Respuesta", "Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: " )
TPSI83 = graficarTable(dataI83,"Respuesta","Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: ")

#---------------

A = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 1 )
B = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 2 )
C = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 3 )
D = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 4 )
E = sum(Isla$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 5 )
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataI84 <- data.frame(n,RESPUESTA,   total)

GPSI84 = graficarPlot(dataI84,"Respuesta", "Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: " )
TPSI84 = graficarTable(dataI84,"Respuesta","Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: ")

#---------------
#9

A = sum(Isla$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'SI')
B = sum(Isla$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'NO')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
n = c(A, B)
RESPUESTA = c("Sí", "No")
dataI9 <- data.frame(n,RESPUESTA,   total)
GPSI9 = graficarPlot(dataI9,"Respuesta", "Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?" )
TPSI9 = graficarTable(dataI9,"Respuesta","Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?")

#-----------------------------------------------------------------------------------
#10 

A = sum(Isla$En.caso.de.ser.víctima.del.delito..Usted..Llama.a.la.policía == 'SI')
B = sum(Isla$En.caso.de.ser.víctima.del.delito..Usted..Hace.una.denuncia == 'SI')
C = sum(Isla$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.sus.vecinos.del.peligro == 'SI')
D = sum(Isla$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.su.familia.del.peligro == 'SI')
E = sum(Isla$En.caso.de.ser.víctima.del.delito..Usted..Busca.desquitarse == 'SI')

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("Llama.a.la.policía", "Hace.una.denuncia", "Advierte.a.sus.vecinos.del.peligro", "Advierte.a.su.familia.del.peligro", "Busca.desquitarse")
dataI10 <- data.frame(n,RESPUESTA,   total)

GPSI10 = graficarPlot(dataI10,"Respuesta", "Numero de personas", "10. En caso de ser víctima del delito Usted: " )
TPSI10 = graficarTable(dataI10,"Respuesta","Numero de personas", "10. En caso de ser víctima del delito Usted: ")



#-----------------------------------------------------------------------------------
#11

A = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.pruebas == 'SI')
B = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Considera.que.es.un.delito.de.poca.importancia == 'SI')
C = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Conoce.al.agresor.o.agresores == 'SI')
D = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconfía.de.las.autoridades == 'SI')
E = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Teme.a.que.lo.extorsionen == 'SI')
FF = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.tiempo == 'SI')
G = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Son.trámites.complicados == 'SI')
H = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconoce.dónde.denunciar == 'SI')
I = sum(Isla$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Aunque.denuncie.no.va.a.pasar.nada == 'SI')

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
dataI11 <- data.frame(n,RESPUESTA,   total)
GPSI11 = graficarPlot(dataI11,"Respuesta", "Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:" )
TPSI11 = graficarTable(dataI11,"Respuesta","Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:")

#-----------------------------------------------------------------------------------
#12

A = sum(Isla$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
B = sum(Isla$En.esta.calle.o.zona..Los.vecinos.se.organizan.para.prevenir.delitos == 'SI')
C = sum(Isla$En.esta.calle.o.zona..Las.personas.son.amables == 'SI')
D = sum(Isla$En.esta.calle.o.zona..Hay.alguna.persona.que.siempre.ayuda.a.los.demás == 'SI')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n =  c(A, B, C, D)
RESPUESTA = c("Actividades.con.hijos","Prevenir.delitos", "Las.personas.son.amables", "Hay.alguna.persona.que.siempre.ayuda.a.los.demás");
dataI12 <- data.frame(n,RESPUESTA,   total)
GPSI12 = graficarPlot(dataI12,"Respuesta", "Numero de personas", "En esta calle o zona:" )
TPSI12 = graficarTable(dataI12,"Respuesta","Numero de personas", "En esta calle o zona:")


#------------------------------------------------------------------------------------
#En esta calle o zona, hay personas
#13
A = sum(Isla$En.esta.calle.o.zona.hay.personas..A.las.que.todos.tienen.miedo == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.personas..Que.acosan.a.menores == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.personas..Que.acosan.a.mujeres == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.personas..Que.se.emborrachan.o.se.drogan == 'SI')
E = sum(Isla$En.esta.calle.o.zona.hay.personas..Que.han.estado.en.la.cárcel == 'SI')
F = sum(Isla$En.esta.calle.o.zona.hay.personas..Sospechosas == 'SI')
total = c(A, B, C, D,E,  F)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
F = as.integer((F/N)*100)
n =  c(A, B, C, D,E,  F)
RESPUESTA = c("A.las.que.todos.tienen.miedo", "Que.acosan.a.menores",  "Que.acosan.a.mujeres", "Que.se.emborrachan.o.se.drogan", "Que.han.estado.en.la.cárcel", "Sospechosas");
dataI13 <- data.frame(n,RESPUESTA,   total)
GPSI13 = graficarPlot(dataI13,"Respuesta", "Numero de personas", "13. En esta calle o zona, hay personas:" )
TPSI13 = graficarTable(dataI13,"Respuesta","Numero de personas", "13. En esta calle o zona, hay personas:")
#------------------------------------------------------------------------------------
# 14 . En esta calle o zona hay violencia:

A = sum(Isla$En.esta.calle.o.zona.hay.violencia..Entre.mujeres == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.violencia..Entre.hombres == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.violencia..Entre.familias == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.violencia..Entre.adultos.y.jóvenes == 'SI')
E = sum(Isla$En.esta.calle.o.zona.hay.violencia..Entre.jóvenes == 'SI')

total = c(A, B, C, D,E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n =  c(A, B, C, D,E)
RESPUESTA = c("Entre mujeres", "Entre hombres" , "Entre familias" , "Entre adultos y jóvenes", "Entre jóvenes");
dataI14 <- data.frame(n,RESPUESTA,   total)
GPSI14 = graficarPlot(dataI14,"Respuesta", "Numero de personas", "14. En esta calle o zona, hay personas:" )
TPSI14 = graficarTable(dataI14,"Respuesta","Numero de personas", "14. En esta calle o zona, hay personas:")



#------------------------------------------------------------------------------------
# 15 . En esta calle o zona hay violencia:

A = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..A.gritos == 'SI')
B = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.golpes == 'SI')
C = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.cuchillos..navajas.o.machetes == 'SI')
D = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.armas.de.fuego..como.pistolas.o.rifles == 'SI')
E = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Desquitándose.del.otro == 'SI')
FF = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Conciliando.con.una.tercera.persona == 'SI')
G = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Dialogando == 'SI')
H = sum(Isla$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..De.manera.respetuosa == 'SI')
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
dataI15 <- data.frame(n,RESPUESTA,   total)
GPSI15 = graficarPlot(dataI15,"Respuesta", "Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" )
TPSI15 = graficarTable(dataI15,"Respuesta","Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:")



#------------------------------------------------------------------------------------
# 16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:

A = sum(Isla$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Por.la.inseguridad == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Descuido.de.los.padres == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Castigo == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por la inseguridad","Descuido de los pades","Castigo","Trabajo de los padres");
dataI16 <- data.frame(n,RESPUESTA,   total)
GPSI16 = graficarPlot(dataI16,"Respuesta", "Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" )
TPSI16 = graficarTable(dataI16,"Respuesta","Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:")



#------------------------------------------------------------------------------------
# 17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:

A = sum(Isla$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Por.descuido.de.los.padres == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Castigo == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Falta.de.dinero == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por descuido de los padres", "Castigo", "Falta de dinero", "Trabajo de los padres");
dataI17 <- data.frame(n,RESPUESTA,   total)
GPSI17 = graficarPlot(dataI17,"Respuesta", "Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " )
TPSI17 = graficarTable(dataI17,"Respuesta","Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ")



#------------------------------------------------------------------------------------
# 18. En esta calle o zona hay jóvenes que:

A = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Hacen.deporte == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Ayudan.a.los.demás == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..La.mayoria.de.los.jovenes.estudian.o.trabajan == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Andan.en.pandillas == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Hacen deporte", "Ayudan a los demás", "La mayoría de los jóvenes estudian o trabajan.","Andan en pandillas");
dataI18 <- data.frame(n,RESPUESTA,   total)
GPSI18 = graficarPlot(dataI18,"Respuesta", "Numero de personas", "18. En esta calle o zona hay jóvenes que: " )
TPSI18 = graficarTable(dataI18,"Respuesta","Numero de personas", "18. En esta calle o zona hay jóvenes que: ")


#------------------------------------------------------------------------------------
#18.1

# 18. En esta calle o zona hay jóvenes que:

A = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Andan.armados == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Destruyen.o.vandalizan.propiedad.ajena == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Son.violentos == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.jóvenes.que..Amenazan.a.los.vecinos == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)

RESPUESTA = c("Andan armados","Destruyen o vandalizan la propiedad ajena", "Son violentos", "Amenazan a los vecinos");
dataI181 <- data.frame(n,RESPUESTA,   total)
GPSI181 = graficarPlot(dataI181,"Respuesta", "Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas" )
TPSI181 = graficarTable(dataI181,"Respuesta","Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas")



#------------------------------------------------------------------------------------
# 19. En esta calle o zona hay un parque


A = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Niños.y.niñas == 'SI')
B = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Jóvenes == 'SI')
C = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Pandillas == 'SI')
D = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Familias == 'SI')
E = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Adultos == 'SI')
FF = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Personas.de.la.tercera.edad == 'SI')
G = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Usted.lo.utiliza == 'SI')
H = sum(Isla$En.esta.calle.o.zona.hay.un.parque..Vándalos == 'SI')

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
dataI19 <- data.frame(n,RESPUESTA,   total)
GPSI19 = graficarPlot(dataI19,"Respuesta", "Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan?" )
TPSI19 = graficarTable(dataI19,"Respuesta","Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan? ")


#------------------------------------------------------------------------------------
# 20. En esta calle o zona hay:


A = sum(Isla$En.esta.calle.hay..Banquetas == 'SI')
B = sum(Isla$En.esta.calle.hay..Baches == 'SI')
C = sum(Isla$En.esta.calle.hay..Letreros.con.nombres.de.las.calles == 'SI')
D = sum(Isla$En.esta.calle.hay..Tiendita == 'SI')
E = sum(Isla$En.esta.calle.hay..Alumbrado == 'SI')
FF = sum(Isla$En.esta.calle.hay..Consumo.del.alcohol.en.la.calle == 'SI')


total = c(A, B, C, D,  E, FF )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)



n = c(A, B, C, D,  E, FF )

RESPUESTA = c("Banquetas","Baches","Letreros con nombres de las calles","Tiendita","Alumbrado","Consumo de alcohol en la calle");
dataI20 <- data.frame(n,RESPUESTA,   total)
GPSI20 = graficarPlot(dataI20,"Respuesta", "Numero de personas", "20. En esta calle o zona hay: " )
TPSI20 = graficarTable(dataI20,"Respuesta","Numero de personas", "20. En esta calle o zona hay:  ")


   

#------------------------------------------------------------------------------------
# 21. En esta calle o zona hay:

A = sum(Isla$En.esta.zona.hay..Horarios.de.transporte.que.convienen == 'SI')
B = sum(Isla$En.esta.zona.hay..Una.parada.de.camión.cerca.de.esta.casa == 'SI')
C = sum(Isla$En.esta.zona.hay..Terrenos.baldíos == 'SI')
D = sum(Isla$En.esta.zona.hay..Basura == 'SI')
E = sum(Isla$En.esta.zona.hay..Autos.abandonados == 'SI')
FF = sum(Isla$En.esta.zona.hay..Casas.abandonadas == 'SI')
G = sum(Isla$En.esta.zona.hay..Vandalismo == 'SI')
H = sum(Isla$En.esta.zona.hay..Grafiti == 'SI')
I = sum(Isla$En.esta.zona.hay..Venta.de.thinner.o.pegamento.a.menores == 'SI')
J = sum(Isla$En.esta.zona.hay..Venta.de.alcohol.o.cigarros.a.menores == 'SI')
L = sum(Isla$En.esta.zona.hay..Venta.de.droga == 'SI')
M = sum(Isla$En.esta.zona.hay..Venta.de.alcohol.después.de.las.11.00.de.la.noche == 'SI')


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
dataI21 <- data.frame(n,RESPUESTA,   total)
GPSI21 = graficarPlot(dataI21,"Respuesta", "Numero de personas", "21. En esta calle o zona hay: " )
TPSI21 = graficarTable(dataI21,"Respuesta","Numero de personas", "21. En esta calle o zona hay:  ")


#------------------------------------------------------------------------------------
# 22. En el último año Usted supo que algún menor de 18 años:


A = sum(Isla$Usted.supo.que.algún.menor.de.18.años..Se.fue.de.la.casa == 'SI')
B = sum(Isla$Usted.supo.que.algún.menor.de.18.años..Sufrió.violencia == 'SI')
C = sum(Isla$Usted.supo.que.algún.menor.de.18.años..Abandonó.la.escuela == 'SI')
D = sum(Isla$Usted.supo.que.algún.menor.de.18.años..Tiene.problemas.de.conducta == 'SI')
E = sum(Isla$Usted.supo.que.algún.menor.de.18.años..Quedó.embarazada == 'SI')



total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("Se fue de la casa", "Sufrió violencia" , "Abandonó la escuela", "Tiene problemas de conducta","Quedó embarazada");
dataI22 <- data.frame(n,RESPUESTA,   total)
GPSI22 = graficarPlot(dataI22,"Respuesta", "Numero de personas", "22. En el último año Usted supo que algún menor de 18 años: " )
TPSI22 = graficarTable(dataI22,"Respuesta","Numero de personas", "22. En el último año Usted supo que algún menor de 18 años:  ")


#------------------------------------------------------------------------------------
# 23. Para corregir a un niño o niña que se porta mal, Usted recomienda:



A = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Castigarle == 'SI')
B = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Gritarle == 'SI')
C = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.nalgadas == 'SI')
D = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.una.golpiza...cueriza == 'SI')
E = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Explicarle.lo.que.está.mal == 'SI')
FF = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Aconsejarle == 'SI')
G = sum(Isla$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Enseñar.con.el.ejemplo == 'SI')


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
dataI23 <- data.frame(n,RESPUESTA,   total)
GPSI23 = graficarPlot(dataI23,"Respuesta", "Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda: " )
TPSI23 = graficarTable(dataI23,"Respuesta","Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda:  ")

 
#------------------------------------------------------------------------------------
# 25. En esta casa alguien: (APLICA TARJETON)

A = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Tiene.discapacidad == 'SI')
B = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Sabe.manejar.armas.de.fuego..como.pistolas.o.rifles == 'SI')
C = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Habla.de.comprar.armas.de.fuego == 'SI')
D = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..No.habla.español == 'SI')
E = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.obesidad == 'SI')

FF = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.fumar == 'SI')
G = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.beber == 'SI')
H = sum(Isla$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.drogas == 'SI')


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
dataI25 <- data.frame(n,RESPUESTA,   total)
GPSI25 = graficarPlot(dataI25,"Respuesta", "Numero de personas", "25. En esta casa alguien: (APLICA TARJETON) " )
TPSI25 = graficarTable(dataI25,"Respuesta","Numero de personas", "25. En esta casa alguien: (APLICA TARJETON)  ")


#------------------------------------------------------------------------------------
# 26. En el último año, por cuestiones de seguridad Usted ha pensado:

A = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.casa == 'SI')
B = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.ciudad == 'SI')
C = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.estado == 'SI')
D = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.trabajo == 'SI')
E = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cerrar.su.negocio == 'SI')
FF = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiar.a.los.hijos.de.escuela == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cambiarse de casa" , "Cambiarse de ciudad" , "Cambiarse de estado" , "Cambiar de trabajo" , "Cerrar su negocio" , "Cambiar a los hijos de escuela"  );
dataI26 <- data.frame(n,RESPUESTA,   total)
GPSI26 = graficarPlot(dataI26,"Respuesta", "Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado: " )
TPSI26 = graficarTable(dataI26,"Respuesta","Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado:  ")


#------------------------------------------------------------------------------------
# 27. En el último año, por cuestiones de seguridad Usted dejó de:

A = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.de.noche == 'SI')
B = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.a.caminar.o.hacer.ejercicio == 'SI')
C = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Impidió.que.los.niños.salieran.a.la.calle == 'SI')
D = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Evitó.relacionarse.con.nuevas.personas == 'SI')
E = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.visitar.a.parientes.o.amigos == 'SI')
FF = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.transporte.público.combi == 'SI')
G = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.taxi == 'SI')
H = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.llevar.mucho.dinero.en.efectivo == 'SI')
I = sum(Isla$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.joyas == 'SI')

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
dataI27 <- data.frame(n,RESPUESTA,   total)
GPSI27 = graficarPlot(dataI27,"Respuesta", "Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de: " )
TPSI27 = graficarTable(dataI27,"Respuesta","Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de:  ")

#------------------------------------------------------------------------------------
# 28. En esta calle o zona la policía:

A = sum(Isla$En.esta.calle.o.zona.la.policia.Cuida.o.vigila.bien == 'SI')
B = sum(Isla$En.esta.calle.o.zona.la.policia.Comete.abusos == 'SI')
C = sum(Isla$En.esta.calle.o.zona.la.policia.Acude.a.los.llamados == 'SI')
D = sum(Isla$En.esta.calle.o.zona.la.policia.Pide.mordidas == 'SI')
E = sum(Isla$En.esta.calle.o.zona.la.policia.Hace.rondines == 'SI')
FF = sum(Isla$En.esta.calle.o.zona.la.policia.Comete.delitos == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cuida o vigila bien" , "Comete abusos" , "Acude a  los llamados" , "Pide mordidas" , "Hace rondines" , "Comete delitos" );
dataI28 <- data.frame(n,RESPUESTA,   total)
GPSI28 = graficarPlot(dataI28,"Respuesta", "Numero de personas", "28. En esta calle o zona la policía: " )
TPSI28 = graficarTable(dataI28,"Respuesta","Numero de personas", "28. En esta calle o zona la policía:  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI291 <- data.frame(n,RESPUESTA,   total)
GPSI291 = graficarPlot(dataI291,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  " )
TPSI291 = graficarTable(dataI291,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI292 <- data.frame(n,RESPUESTA,   total)
GPSI292 = graficarPlot(dataI292,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  " )
TPSI292 = graficarTable(dataI292,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI293 <- data.frame(n,RESPUESTA,   total)
GPSI293 = graficarPlot(dataI293,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  " )
TPSI293 = graficarTable(dataI293,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI294 <- data.frame(n,RESPUESTA,   total)
GPSI294 = graficarPlot(dataI294,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  " )
TPSI294 = graficarTable(dataI294,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI295 <- data.frame(n,RESPUESTA,   total)
GPSI295 = graficarPlot(dataI295,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  " )
TPSI295 = graficarTable(dataI295,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  ")


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 1)
B = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 2)
C = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 3)
D = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 4)
E = sum(Isla$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI296 <- data.frame(n,RESPUESTA,   total)
GPSI296 = graficarPlot(dataI296,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  " )
TPSI296 = graficarTable(dataI296,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  ")




#------------------------------------------------------------------------------------
# 30 Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI301 <- data.frame(n,RESPUESTA,   total)
GPSI301 = graficarPlot(dataI301,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia " )
TPSI301 = graficarTable(dataI301,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI302 <- data.frame(n,RESPUESTA,   total)
GPSI302 = graficarPlot(dataI302,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar" )
TPSI302 = graficarTable(dataI302,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI303 <- data.frame(n,RESPUESTA,   total)
GPSI303 = graficarPlot(dataI303,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno" )
TPSI303 = graficarTable(dataI303,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI304 <- data.frame(n,RESPUESTA,   total)
GPSI304 = graficarPlot(dataI304,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal " )
TPSI304 = graficarTable(dataI304,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Su.presidente.municipal
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI305 <- data.frame(n,RESPUESTA,   total)
GPSI305 = graficarPlot(dataI305,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal " )
TPSI305 = graficarTable(dataI305,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

 
total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI306 <- data.frame(n,RESPUESTA,   total)
GPSI306 = graficarPlot(dataI306,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador " )
TPSI306 = graficarTable(dataI306,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI311 <- data.frame(n,RESPUESTA,   total)
GPSI311 = graficarPlot(dataI311,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia " )
TPSI311 = graficarTable(dataI311,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI312 <- data.frame(n,RESPUESTA,   total)
GPSI312 = graficarPlot(dataI312,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar " )
TPSI312 = graficarTable(dataI312,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar ")
#-----------------------------2------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI313 <- data.frame(n,RESPUESTA,   total)
GPSI313 = graficarPlot(dataI313,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno " )
TPSI313 = graficarTable(dataI313,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI314 <- data.frame(n,RESPUESTA,   total)
GPSI314 = graficarPlot(dataI314,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal " )
TPSI314 = graficarTable(dataI314,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal ")
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI315 <- data.frame(n,RESPUESTA,   total)
GPSI315 = graficarPlot(dataI315,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal " )
TPSI315 = graficarTable(dataI315,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Isla$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataI316 <- data.frame(n,RESPUESTA,   total)
GPSI316 = graficarPlot(dataI316,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador " )
TPSI316 = graficarTable(dataI316,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador ")







