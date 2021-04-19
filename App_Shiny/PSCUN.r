N = nrow(Cancun)
#------------------------------------------------------------------------------------
#1
# En esta calle o zona, Usted participa:
A = sum(Cancun$En.esta.calle.o.zona..Usted.participa..En.eventos.deportivos == 'SI')
B = sum(Cancun$En.esta.calle.o.zona..Usted.participa..En.tandas == 'SI')
C = sum(Cancun$En.esta.calle.o.zona..Usted.participa..En.fiestas == 'SI')
D = sum(Cancun$En.esta.calle.o.zona..Usted.participa..En.iglesia.o.templo == 'SI')
E = sum(Cancun$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
FF = sum(Cancun$En.esta.calle.o.zona..Usted.participa..Para.solucionar.problemas.de.la.comunidad == 'SI')

total = c(A, B, C, D, E, FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D, E,FF)
RESPUESTA = c("Eventos deportivos", "tandas", "fiestas", "iglesia o templo", "Actividades.con.hijos","Para.solucionar.problemas.de.la.comunidad")
dataC1 <- data.frame(n,RESPUESTA,   total)

GPSC1 = graficarPlot(dataC1,"Respuesta", "Numero de personas", "1. En esta calle o zona, Usted participa:" )
TPSC1 = graficarTable(dataC1,"Respuesta","Numero de personas", "1. En esta calle o zona, Usted participa:")

#------------------------------------------------------------------------------------
#Usted conoce a sus vecinos:
# P2R1S = sum(Cancun$Usted.conoce.a.sus.vecinos. == 'SI')
# P2R2S = sum(Cancun$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.a.los.niños == 'SI')
# P2R3S = sum(Cancun$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.su.casa == 'SI')
# P2R4S = sum(Cancun$Usted.conoce.a.sus.vecinos..Participa.con.ellos.para.mejorar.la.seguridad == 'SI')
# P2R5S = sum(Cancun$Usted.conoce.a.sus.vecinos..Le.interesaría.participar == 'SI')
# total = c(P2R1S, P2R2S, P2R3S, P2R4S, P2R5S)
# P2R1S = as.integer((P2R1S/N)*100)
# P2R2S = as.integer((P2R2S/N)*100)
# P2R3S = as.integer((P2R3S/N)*100)
# P2R4S = as.integer((P2R4S/N)*100)
# P2R5S = as.integer((P2R5S/N)*100)
# n = c(P2R1S, P2R2S, P2R3S, P2R4S, P2R5S)
# RESPUESTA = c("Conoce a sus vecinos", "Confiaría a los niños", "Confiaría su casa", "Participa con ellos para mejorar la seguridad", "Interesaría participar")
# dataC <- data.frame(n,RESPUESTA,   total)
# GPSC2 = graficarPlot(dataC,"Respuesta", "Numero de personas", "Usted conoce a sus vecinos:" )
# TPSC2 = graficarTable(dataC,"Respuesta","Numero de personas", "Usted conoce a sus vecinos:")
# P3R1S = sum(Cancun$Participa.con.la.autoridad.para.mejorar.la.seguridad. == 'SI')
# total = c(P3R1S, P3R1N)
# P3R1S = as.integer((P3R1S/N)*100)
# P3R1N = as.integer((P3R1N/N)*100)
# n = c(P3R1S, P3R1N)
# RESPUESTA = c("Sí", "No")
# dataC <- data.frame(n,RESPUESTA,   total)
# GPSC3 = graficarPlot(dataC,"Respuesta", "Numero de personas", "Usted conoce a sus vecinos:" )
# TPSC3 = graficarTable(dataC,"Respuesta","Numero de personas", "Usted conoce a sus vecinos:")
#4 dia y 5 horarios
#-----------------------------------------------------------------------------------
#6
#Cuando hay un delito, en esta calle o zona los vecinos:
A = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.reúnen  == 'SI')
B = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.organizan.para.vigilar  == 'SI')
C = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Intercambian.números.telefónicos  == 'SI')
D = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Forman.un.chat  == 'SI')
E = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Ponen.letreros.de.advertencia  == 'SI')
FF = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Llaman.a.la.policía  == 'SI')
G = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Denuncian.ante.la.autoridad  == 'SI')
H = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Vigilan  == 'SI')
I = sum(Cancun$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Buscan.desquitarse  == 'SI')

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
dataC6 <- data.frame(n,RESPUESTA,   total)

GPSC6 = graficarPlot(dataC6,"Respuesta", "Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSC6 = graficarTable(dataC6,"Respuesta","Numero de personas", "6. Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#7

A = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.casa == 'SI')
B = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.la.calle == 'SI')
C = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.transporte == 'SI')
D = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.negocio == 'SI')
E = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.partes.de.auto == 'SI')
FF = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.vehículo == 'SI')
G = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Balaceras == 'SI')
H = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Cobro.de.piso == 'SI')
I = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Violencia.familiar == 'SI')
J = sum(Cancun$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Peleas.de.gallos == 'SI')

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
dataC7 <- data.frame(n,RESPUESTA,   total)
GPSC7 = graficarPlot(dataC7,"Respuesta", "Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:" )
TPSC7 = graficarTable(dataC7,"Respuesta","Numero de personas", "Cuando hay un delito, en esta calle o zona los vecinos:")

#-----------------------------------------------------------------------------------
#8 . 1

A = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 1 )
B = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 2 )
C = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 3 )
D = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 4 )
E = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataC81 <- data.frame(n,RESPUESTA,   total)

GPSC81 = graficarPlot(dataC81,"Respuesta", "Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: " )
TPSC81 = graficarTable(dataC81,"Respuesta","Numero de personas", "8.1 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa: ")

#---------------
# 8.2 
A = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 1 )
B = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 2 )
C = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 3 )
D = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 4 )
E = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataC82 <- data.frame(n,RESPUESTA,   total)

GPSC82 = graficarPlot(dataC82,"Respuesta", "Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: " )
TPSC82 = graficarTable(dataC82,"Respuesta","Numero de personas", "8.2 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle: ")

#---------------

A = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 1 )
B = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 2 )
C = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 3 )
D = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 4 )
E = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona == 5 )

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataC83 <- data.frame(n,RESPUESTA,   total)

GPSC83 = graficarPlot(dataC83,"Respuesta", "Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: " )
TPSC83 = graficarTable(dataC83,"Respuesta","Numero de personas", "8.3 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona: ")

#---------------

A = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 1 )
B = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 2 )
C = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 3 )
D = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 4 )
E = sum(Cancun$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad == 5 )
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("1", "2", "3", "4", "5")
dataC84 <- data.frame(n,RESPUESTA,   total)

GPSC84 = graficarPlot(dataC84,"Respuesta", "Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: " )
TPSC84 = graficarTable(dataC84,"Respuesta","Numero de personas", "8.4 Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad: ")

#---------------
#9

A = sum(Cancun$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'SI')
B = sum(Cancun$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año. == 'NO')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
n = c(A, B)
RESPUESTA = c("Sí", "No")
dataC9 <- data.frame(n,RESPUESTA,   total)
GPSC9 = graficarPlot(dataC9,"Respuesta", "Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?" )
TPSC9 = graficarTable(dataC9,"Respuesta","Numero de personas", "9. ¿Usted ha sido víctima de algún delito en el último año?")

#-----------------------------------------------------------------------------------
#10 

A = sum(Cancun$En.caso.de.ser.víctima.del.delito..Usted..Llama.a.la.policía == 'SI')
B = sum(Cancun$En.caso.de.ser.víctima.del.delito..Usted..Hace.una.denuncia == 'SI')
C = sum(Cancun$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.sus.vecinos.del.peligro == 'SI')
D = sum(Cancun$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.su.familia.del.peligro == 'SI')
E = sum(Cancun$En.caso.de.ser.víctima.del.delito..Usted..Busca.desquitarse == 'SI')

total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n = c(A, B, C, D, E)
RESPUESTA = c("Llama.a.la.policía", "Hace.una.denuncia", "Advierte.a.sus.vecinos.del.peligro", "Advierte.a.su.familia.del.peligro", "Busca.desquitarse")
dataC10 <- data.frame(n,RESPUESTA,   total)

GPSC10 = graficarPlot(dataC10,"Respuesta", "Numero de personas", "10. En caso de ser víctima del delito Usted: " )
TPSC10 = graficarTable(dataC10,"Respuesta","Numero de personas", "10. En caso de ser víctima del delito Usted: ")



#-----------------------------------------------------------------------------------
#11

A = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.pruebas == 'SI')
B = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Considera.que.es.un.delito.de.poca.importancia == 'SI')
C = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Conoce.al.agresor.o.agresores == 'SI')
D = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconfía.de.las.autoridades == 'SI')
E = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Teme.a.que.lo.extorsionen == 'SI')
FF = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.tiempo == 'SI')
G = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Son.trámites.complicados == 'SI')
H = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconoce.dónde.denunciar == 'SI')
I = sum(Cancun$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Aunque.denuncie.no.va.a.pasar.nada == 'SI')

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
dataC11 <- data.frame(n,RESPUESTA,   total)
GPSC11 = graficarPlot(dataC11,"Respuesta", "Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:" )
TPSC11 = graficarTable(dataC11,"Respuesta","Numero de personas", "11. Ha sido víctima de algún delito y no denuncio:")

#-----------------------------------------------------------------------------------
#12

A = sum(Cancun$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos == 'SI')
B = sum(Cancun$En.esta.calle.o.zona..Los.vecinos.se.organizan.para.prevenir.delitos == 'SI')
C = sum(Cancun$En.esta.calle.o.zona..Las.personas.son.amables == 'SI')
D = sum(Cancun$En.esta.calle.o.zona..Hay.alguna.persona.que.siempre.ayuda.a.los.demás == 'SI')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n =  c(A, B, C, D)
RESPUESTA = c("Actividades.con.hijos","Prevenir.delitos", "Las.personas.son.amables", "Hay.alguna.persona.que.siempre.ayuda.a.los.demás");
dataC12 <- data.frame(n,RESPUESTA,   total)
GPSC12 = graficarPlot(dataC12,"Respuesta", "Numero de personas", "En esta calle o zona:" )
TPSC12 = graficarTable(dataC12,"Respuesta","Numero de personas", "En esta calle o zona:")


#------------------------------------------------------------------------------------
#En esta calle o zona, hay personas
#13
A = sum(Cancun$En.esta.calle.o.zona.hay.personas..A.las.que.todos.tienen.miedo == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.personas..Que.acosan.a.menores == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.personas..Que.acosan.a.mujeres == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.personas..Que.se.emborrachan.o.se.drogan == 'SI')
E = sum(Cancun$En.esta.calle.o.zona.hay.personas..Que.han.estado.en.la.cárcel == 'SI')
F = sum(Cancun$En.esta.calle.o.zona.hay.personas..Sospechosas == 'SI')
total = c(A, B, C, D,E,  F)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
F = as.integer((F/N)*100)
n =  c(A, B, C, D,E,  F)
RESPUESTA = c("A.las.que.todos.tienen.miedo", "Que.acosan.a.menores",  "Que.acosan.a.mujeres", "Que.se.emborrachan.o.se.drogan", "Que.han.estado.en.la.cárcel", "Sospechosas");
dataC13 <- data.frame(n,RESPUESTA,   total)
GPSC13 = graficarPlot(dataC13,"Respuesta", "Numero de personas", "13. En esta calle o zona, hay personas:" )
TPSC13 = graficarTable(dataC13,"Respuesta","Numero de personas", "13. En esta calle o zona, hay personas:")
#------------------------------------------------------------------------------------
# 14 . En esta calle o zona hay violencia:

A = sum(Cancun$En.esta.calle.o.zona.hay.violencia..Entre.mujeres == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.violencia..Entre.hombres == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.violencia..Entre.familias == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.violencia..Entre.adultos.y.jóvenes == 'SI')
E = sum(Cancun$En.esta.calle.o.zona.hay.violencia..Entre.jóvenes == 'SI')

total = c(A, B, C, D,E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)

n =  c(A, B, C, D,E)
RESPUESTA = c("Entre mujeres", "Entre hombres" , "Entre familias" , "Entre adultos y jóvenes", "Entre jóvenes");
dataC14 <- data.frame(n,RESPUESTA,   total)
GPSC14 = graficarPlot(dataC14,"Respuesta", "Numero de personas", "14. En esta calle o zona, hay personas:" )
TPSC14 = graficarTable(dataC14,"Respuesta","Numero de personas", "14. En esta calle o zona, hay personas:")



#------------------------------------------------------------------------------------
# 15 . En esta calle o zona hay violencia:

A = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..A.gritos == 'SI')
B = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.golpes == 'SI')
C = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.cuchillos..navajas.o.machetes == 'SI')
D = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.armas.de.fuego..como.pistolas.o.rifles == 'SI')
E = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Desquitándose.del.otro == 'SI')
FF = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Conciliando.con.una.tercera.persona == 'SI')
G = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Dialogando == 'SI')
H = sum(Cancun$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..De.manera.respetuosa == 'SI')
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
dataC15 <- data.frame(n,RESPUESTA,   total)
GPSC15 = graficarPlot(dataC15,"Respuesta", "Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" )
TPSC15 = graficarTable(dataC15,"Respuesta","Numero de personas", "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:")



#------------------------------------------------------------------------------------
# 16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:

A = sum(Cancun$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Por.la.inseguridad == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Descuido.de.los.padres == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Castigo == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por la inseguridad","Descuido de los pades","Castigo","Trabajo de los padres");
dataC16 <- data.frame(n,RESPUESTA,   total)
GPSC16 = graficarPlot(dataC16,"Respuesta", "Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" )
TPSC16 = graficarTable(dataC16,"Respuesta","Numero de personas", "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:")



#------------------------------------------------------------------------------------
# 17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:

A = sum(Cancun$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Por.descuido.de.los.padres == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Castigo == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Falta.de.dinero == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer..Trabajo.de.los.padres == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Por descuido de los padres", "Castigo", "Falta de dinero", "Trabajo de los padres");
dataC17 <- data.frame(n,RESPUESTA,   total)
GPSC17 = graficarPlot(dataC17,"Respuesta", "Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " )
TPSC17 = graficarTable(dataC17,"Respuesta","Numero de personas", "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ")



#------------------------------------------------------------------------------------
# 18. En esta calle o zona hay jóvenes que:

A = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Hacen.deporte == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Ayudan.a.los.demás == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..La.mayoria.de.los.jovenes.estudian.o.trabajan == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Andan.en.pandillas == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)
RESPUESTA = c("Hacen deporte", "Ayudan a los demás", "La mayoría de los jóvenes estudian o trabajan.","Andan en pandillas");
dataC18 <- data.frame(n,RESPUESTA,   total)
GPSC18 = graficarPlot(dataC18,"Respuesta", "Numero de personas", "18. En esta calle o zona hay jóvenes que: " )
TPSC18 = graficarTable(dataC18,"Respuesta","Numero de personas", "18. En esta calle o zona hay jóvenes que: ")


#------------------------------------------------------------------------------------
#18.1

# 18. En esta calle o zona hay jóvenes que:

A = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Andan.armados == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Destruyen.o.vandalizan.propiedad.ajena == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Son.violentos == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.jóvenes.que..Amenazan.a.los.vecinos == 'SI')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)

n =  c(A, B, C, D)

RESPUESTA = c("Andan armados","Destruyen o vandalizan la propiedad ajena", "Son violentos", "Amenazan a los vecinos");
dataC181 <- data.frame(n,RESPUESTA,   total)
GPSC181 = graficarPlot(dataC181,"Respuesta", "Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas" )
TPSC181 = graficarTable(dataC181,"Respuesta","Numero de personas", "18.1 En esta calle o zona hay jóvenes que: Andan en pandillas")



#------------------------------------------------------------------------------------
# 19. En esta calle o zona hay un parque


A = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Niños.y.niñas == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Jóvenes == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Pandillas == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Familias == 'SI')
E = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Adultos == 'SI')
FF = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Lo.utilizan..Personas.de.la.tercera.edad == 'SI')
G = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Usted.lo.utiliza == 'SI')
H = sum(Cancun$En.esta.calle.o.zona.hay.un.parque..Vándalos == 'SI')

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
dataC19 <- data.frame(n,RESPUESTA,   total)
GPSC19 = graficarPlot(dataC19,"Respuesta", "Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan?" )
TPSC19 = graficarTable(dataC19,"Respuesta","Numero de personas", "19. En esta calle o zona hay un parque: ¿Quiénes lo utilizan? ")


#------------------------------------------------------------------------------------
# 20. En esta calle o zona hay:


A = sum(Cancun$En.esta.calle.hay..Banquetas == 'SI')
B = sum(Cancun$En.esta.calle.hay..Baches == 'SI')
C = sum(Cancun$En.esta.calle.hay..Letreros.con.nombres.de.las.calles == 'SI')
D = sum(Cancun$En.esta.calle.hay..Tiendita == 'SI')
E = sum(Cancun$En.esta.calle.hay..Alumbrado == 'SI')
FF = sum(Cancun$En.esta.calle.hay..Consumo.del.alcohol.en.la.calle == 'SI')


total = c(A, B, C, D,  E, FF )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)



n = c(A, B, C, D,  E, FF )

RESPUESTA = c("Banquetas","Baches","Letreros con nombres de las calles","Tiendita","Alumbrado","Consumo de alcohol en la calle");
dataC20 <- data.frame(n,RESPUESTA,   total)
GPSC20 = graficarPlot(dataC20,"Respuesta", "Numero de personas", "20. En esta calle o zona hay: " )
TPSC20 = graficarTable(dataC20,"Respuesta","Numero de personas", "20. En esta calle o zona hay:  ")


   

#------------------------------------------------------------------------------------
# 21. En esta calle o zona hay:

A = sum(Cancun$En.esta.zona.hay..Horarios.de.transporte.que.convienen == 'SI')
B = sum(Cancun$En.esta.zona.hay..Una.parada.de.camión.cerca.de.esta.casa == 'SI')
C = sum(Cancun$En.esta.zona.hay..Terrenos.baldíos == 'SI')
D = sum(Cancun$En.esta.zona.hay..Basura == 'SI')
E = sum(Cancun$En.esta.zona.hay..Autos.abandonados == 'SI')
FF = sum(Cancun$En.esta.zona.hay..Casas.abandonadas == 'SI')
G = sum(Cancun$En.esta.zona.hay..Vandalismo == 'SI')
H = sum(Cancun$En.esta.zona.hay..Grafiti == 'SI')
I = sum(Cancun$En.esta.zona.hay..Venta.de.thinner.o.pegamento.a.menores == 'SI')
J = sum(Cancun$En.esta.zona.hay..Venta.de.alcohol.o.cigarros.a.menores == 'SI')
L = sum(Cancun$En.esta.zona.hay..Venta.de.droga == 'SI')
M = sum(Cancun$En.esta.zona.hay..Venta.de.alcohol.después.de.las.11.00.de.la.noche == 'SI')


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
dataC21 <- data.frame(n,RESPUESTA,   total)
GPSC21 = graficarPlot(dataC21,"Respuesta", "Numero de personas", "21. En esta calle o zona hay: " )
TPSC21 = graficarTable(dataC21,"Respuesta","Numero de personas", "21. En esta calle o zona hay:  ")


#------------------------------------------------------------------------------------
# 22. En el último año Usted supo que algún menor de 18 años:


A = sum(Cancun$Usted.supo.que.algún.menor.de.18.años..Se.fue.de.la.casa == 'SI')
B = sum(Cancun$Usted.supo.que.algún.menor.de.18.años..Sufrió.violencia == 'SI')
C = sum(Cancun$Usted.supo.que.algún.menor.de.18.años..Abandonó.la.escuela == 'SI')
D = sum(Cancun$Usted.supo.que.algún.menor.de.18.años..Tiene.problemas.de.conducta == 'SI')
E = sum(Cancun$Usted.supo.que.algún.menor.de.18.años..Quedó.embarazada == 'SI')



total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("Se fue de la casa", "Sufrió violencia" , "Abandonó la escuela", "Tiene problemas de conducta","Quedó embarazada");
dataC22 <- data.frame(n,RESPUESTA,   total)
GPSC22 = graficarPlot(dataC22,"Respuesta", "Numero de personas", "22. En el último año Usted supo que algún menor de 18 años: " )
TPSC22 = graficarTable(dataC22,"Respuesta","Numero de personas", "22. En el último año Usted supo que algún menor de 18 años:  ")


#------------------------------------------------------------------------------------
# 23. Para corregir a un niño o niña que se porta mal, Usted recomienda:



A = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Castigarle == 'SI')
B = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Gritarle == 'SI')
C = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.nalgadas == 'SI')
D = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.una.golpiza...cueriza == 'SI')
E = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Explicarle.lo.que.está.mal == 'SI')
FF = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Aconsejarle == 'SI')
G = sum(Cancun$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Enseñar.con.el.ejemplo == 'SI')


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
dataC23 <- data.frame(n,RESPUESTA,   total)
GPSC23 = graficarPlot(dataC23,"Respuesta", "Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda: " )
TPSC23 = graficarTable(dataC23,"Respuesta","Numero de personas", "23. Para corregir a un niño o niña que se porta mal, Usted recomienda:  ")

 
#------------------------------------------------------------------------------------
# 25. En esta casa alguien: (APLICA TARJETON)

A = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Tiene.discapacidad == 'SI')
B = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Sabe.manejar.armas.de.fuego..como.pistolas.o.rifles == 'SI')
C = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Habla.de.comprar.armas.de.fuego == 'SI')
D = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..No.habla.español == 'SI')
E = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.obesidad == 'SI')

FF = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.fumar == 'SI')
G = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.beber == 'SI')
H = sum(Cancun$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.drogas == 'SI')


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
dataC25 <- data.frame(n,RESPUESTA,   total)
GPSC25 = graficarPlot(dataC25,"Respuesta", "Numero de personas", "25. En esta casa alguien: (APLICA TARJETON) " )
TPSC25 = graficarTable(dataC25,"Respuesta","Numero de personas", "25. En esta casa alguien: (APLICA TARJETON)  ")


#------------------------------------------------------------------------------------
# 26. En el último año, por cuestiones de seguridad Usted ha pensado:

A = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.casa == 'SI')
B = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.ciudad == 'SI')
C = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.estado == 'SI')
D = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.trabajo == 'SI')
E = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cerrar.su.negocio == 'SI')
FF = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiar.a.los.hijos.de.escuela == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cambiarse de casa" , "Cambiarse de ciudad" , "Cambiarse de estado" , "Cambiar de trabajo" , "Cerrar su negocio" , "Cambiar a los hijos de escuela"  );
dataC26 <- data.frame(n,RESPUESTA,   total)
GPSC26 = graficarPlot(dataC26,"Respuesta", "Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado: " )
TPSC26 = graficarTable(dataC26,"Respuesta","Numero de personas", "26. En el último año, por cuestiones de seguridad Usted ha pensado:  ")


#------------------------------------------------------------------------------------
# 27. En el último año, por cuestiones de seguridad Usted dejó de:

A = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.de.noche == 'SI')
B = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.a.caminar.o.hacer.ejercicio == 'SI')
C = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Impidió.que.los.niños.salieran.a.la.calle == 'SI')
D = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Evitó.relacionarse.con.nuevas.personas == 'SI')
E = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.visitar.a.parientes.o.amigos == 'SI')
FF = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.transporte.público.combi == 'SI')
G = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.taxi == 'SI')
H = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.llevar.mucho.dinero.en.efectivo == 'SI')
I = sum(Cancun$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.joyas == 'SI')

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
dataC27 <- data.frame(n,RESPUESTA,   total)
GPSC27 = graficarPlot(dataC27,"Respuesta", "Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de: " )
TPSC27 = graficarTable(dataC27,"Respuesta","Numero de personas", "27. En el último año, por cuestiones de seguridad Usted dejó de:  ")

#------------------------------------------------------------------------------------
# 28. En esta calle o zona la policía:

A = sum(Cancun$En.esta.calle.o.zona.la.policia.Cuida.o.vigila.bien == 'SI')
B = sum(Cancun$En.esta.calle.o.zona.la.policia.Comete.abusos == 'SI')
C = sum(Cancun$En.esta.calle.o.zona.la.policia.Acude.a.los.llamados == 'SI')
D = sum(Cancun$En.esta.calle.o.zona.la.policia.Pide.mordidas == 'SI')
E = sum(Cancun$En.esta.calle.o.zona.la.policia.Hace.rondines == 'SI')
FF = sum(Cancun$En.esta.calle.o.zona.la.policia.Comete.delitos == 'SI')

total = c(A, B, C, D,  E ,FF)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
FF = as.integer((FF/N)*100)
n = c(A, B, C, D,  E,FF)

RESPUESTA = c("Cuida o vigila bien" , "Comete abusos" , "Acude a  los llamados" , "Pide mordidas" , "Hace rondines" , "Comete delitos" );
dataC28 <- data.frame(n,RESPUESTA,   total)
GPSC28 = graficarPlot(dataC28,"Respuesta", "Numero de personas", "28. En esta calle o zona la policía: " )
TPSC28 = graficarTable(dataC28,"Respuesta","Numero de personas", "28. En esta calle o zona la policía:  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC291 <- data.frame(n,RESPUESTA,   total)
GPSC291 = graficarPlot(dataC291,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  " )
TPSC291 = graficarTable(dataC291,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La.policia  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC292 <- data.frame(n,RESPUESTA,   total)
GPSC292 = graficarPlot(dataC292,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  " )
TPSC292 = graficarTable(dataC292,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Ministerio.Público.para.denunciar  ")




#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC293 <- data.frame(n,RESPUESTA,   total)
GPSC293 = graficarPlot(dataC293,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  " )
TPSC293 = graficarTable(dataC293,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: ...La.institución.educativa.de.la.zona  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC294 <- data.frame(n,RESPUESTA,   total)
GPSC294 = graficarPlot(dataC294,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  " )
TPSC294 = graficarTable(dataC294,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.comisario.ejidal  ")



#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC295 <- data.frame(n,RESPUESTA,   total)
GPSC295 = graficarPlot(dataC295,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  " )
TPSC295 = graficarTable(dataC295,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su.presidente.municipal  ")


#------------------------------------------------------------------------------------
# 29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 

A = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 1)
B = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 2)
C = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 3)
D = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 4)
E = sum(Cancun$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC296 <- data.frame(n,RESPUESTA,   total)
GPSC296 = graficarPlot(dataC296,"Respuesta", "Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  " )
TPSC296 = graficarTable(dataC296,"Respuesta","Numero de personas", "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El.Gobernador  ")




#------------------------------------------------------------------------------------
# 30 Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC301 <- data.frame(n,RESPUESTA,   total)
GPSC301 = graficarPlot(dataC301,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia " )
TPSC301 = graficarTable(dataC301,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  La.policia ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar== 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC302 <- data.frame(n,RESPUESTA,   total)
GPSC302 = graficarPlot(dataC302,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar" )
TPSC302 = graficarTable(dataC302,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Ministerio.Público.para.denunciar")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC303 <- data.frame(n,RESPUESTA,   total)
GPSC303 = graficarPlot(dataC303,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno" )
TPSC303 = graficarTable(dataC303,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:   Los.empleados.de.gobierno")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC304 <- data.frame(n,RESPUESTA,   total)
GPSC304 = graficarPlot(dataC304,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal " )
TPSC304 = graficarTable(dataC304,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.comisario.ejidal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:   Su.presidente.municipal
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC305 <- data.frame(n,RESPUESTA,   total)
GPSC305 = graficarPlot(dataC305,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal " )
TPSC305 = graficarTable(dataC305,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  Su.presidente.municipal ")


#------------------------------------------------------------------------------------
# 30 Del 1 al 5, califique el trabajo de:  
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

 
total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC306 <- data.frame(n,RESPUESTA,   total)
GPSC306 = graficarPlot(dataC306,"Respuesta", "Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador " )
TPSC306 = graficarTable(dataC306,"Respuesta","Numero de personas", "30 Del 1 al 5, califique el trabajo de:  El.Gobernador ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..La.policia == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC311 <- data.frame(n,RESPUESTA,   total)
GPSC311 = graficarPlot(dataC311,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia " )
TPSC311 = graficarTable(dataC311,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: La.policia ")


#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Ministerio.Público.para.denunciar == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC312 <- data.frame(n,RESPUESTA,   total)
GPSC312 = graficarPlot(dataC312,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar " )
TPSC312 = graficarTable(dataC312,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Ministerio.Público.para.denunciar ")
#-----------------------------2------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC313 <- data.frame(n,RESPUESTA,   total)
GPSC313 = graficarPlot(dataC313,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno " )
TPSC313 = graficarTable(dataC313,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Los.empleados.de.gobierno ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC314 <- data.frame(n,RESPUESTA,   total)
GPSC314 = graficarPlot(dataC314,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal " )
TPSC314 = graficarTable(dataC314,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.comisario.ejidal ")
#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC315 <- data.frame(n,RESPUESTA,   total)
GPSC315 = graficarPlot(dataC315,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal " )
TPSC315 = graficarTable(dataC315,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: Su.presidente.municipal ")

#------------------------------------------------------------------------------------
# 31 Del 1 al 5, califique el trato que recibe de: 
A = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 1)
B = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 2)
C = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 3)
D = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 4)
E = sum(Cancun$Del.1.al.5..califique.el.trabajo.de..El.Gobernador == 5)

total = c(A, B, C, D,  E )
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,  E)

RESPUESTA = c("1" , "2" , "3" , "4" , "5" );
dataC316 <- data.frame(n,RESPUESTA,   total)
GPSC316 = graficarPlot(dataC316,"Respuesta", "Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador " )
TPSC316 = graficarTable(dataC316,"Respuesta","Numero de personas", "31 Del 1 al 5, califique el trato que recibe de: El.Gobernador ")







