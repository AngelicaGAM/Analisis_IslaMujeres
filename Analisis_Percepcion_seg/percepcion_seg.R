library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotrix)
library(scales)
library(sp)
library(leaflet)
library(ggpattern)

#--------- Funciones para graficar ------------

graficarPlot <- function(dff, colorg,  textX, textY, tituloGrifco,N){
  x <- ggplot(dff, aes(x = dff$RESPUESTA, y = dff$n ,fill=RESPUESTA))  +
    geom_bar(stat="identity") +
    xlab(textX)+ 
    ylab(textY) + 
    ggtitle(tituloGrifco) +  
    ylim(0,N)+
    geom_text(aes(label=percent(n/N)), position=position_stack(vjust=0.4),color="black",size=3)
  scale_fill_hue(c=45, l=80)
  return(x)
}


#--------- Funciones para graficar ------------


graficarPie <- function(Gplot){
  pie <- Gplot + coord_polar("y", start=0)+
    theme_minimal()
  return(pie)
}
graficar <- function(df,titulo,textX,textY){
  ggplot(data = df) + 
    geom_col_pattern(
      aes(df$RESPUESTA, df$n, pattern_fill = df$RESPUESTA), 
      pattern = 'stripe',
      fill    = 'white',
      colour  = 'black',
      position_identity()
    )+
    #geom_bar(stat = "identity",position = "identity",aes(x = df$RESPUESTA, y = df$n))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(titulo)+
    xlab(textX)+ 
    ylab(textY)
    #scale_fill_hue(c=45, l=80)  
    #scale_fill_brewer()
}
contar_reactivos <- function(indice){
  c=0 
  for (x in indice) {
    if (x == "SI"){
      c = c+1
    }
  }
  return(c)
}

contar_react <- function(indice){
  uno=0
  dos=0
  tres=0
  cuatro=0
  cinco=0
  for (x in indice) {
    if (x == 1){
      uno = uno+1
    }else{
      if (x == 2){
        dos=dos+1
      }else{
        if (x == 3){
          tres = tres +1
        }else{
          if (x == 4){
            cuatro = cuatro+1
          }else{
            if (x == 5){
              cinco = cinco +1
            }
          }
        }
      }
    }
  }
  resp = c(uno,dos,tres,cuatro,cinco)
  return(resp)
}

####### Carga de archivo ########

Isla <- read.csv("Isla-Mujeres-230030001.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
N <- nrow(Isla)

ejido <- read.csv("Zona-urbana-Ejido-230030286.csv",header=TRUE,sep=",",strip.white = TRUE,na.strings="EMPTY",encoding = "UTF-8")
N2 <- nrow(ejido)

salinas  <- filter(Isla, encuesta_latitude > 21.2365 & encuesta_latitude < 21.241)
N3 <- nrow(salinas)
View(salinas)
#######################

p1 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona..Usted.participa..En.eventos.deportivos
  c_1 = contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona..Usted.participa..En.tandas
  c_2 = contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona..Usted.participa..En.fiestas
  c_3 = contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona..Usted.participa..En.iglesia.o.templo
  c_4 = contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona..Usted.participa..Para.solucionar.problemas.de.la.comunidad
  c_5 = contar_reactivos(indice5)
  
  RESPUESTA = c("Eventos deportivos", "Tandas", "Fiestas", "Iglesia o templo", "Solucionar problemas")
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  n <- c(c_1,c_2,c_3,c_4,c_5)
  zona <- c(z,z,z,z,z)
  
  dff <- data_frame(RESPUESTA, n,zona)
  return(dff)
}
p2_1 <- function(archivo,N,z){  
  ######### Pregunta 2 ##########
  indice1 <- archivo$Usted.conoce.a.sus.vecinos.
  c_1 = contar_reactivos(indice1)
  c1_1 = N-c_1
  
  c_1 = as.integer((c_1/N)*100)
  c1_1 = as.integer((c1_1/N)*100)
  
  RESPUESTA <- c("Si", "No")
  n <- c(c_1,c1_1)
  zona <- c(z,z)
  
  dff <- data_frame(RESPUESTA,n,zona)
  return(dff)
}
p2_2 <- function(archivo,N,z){
  indice1 <- archivo$Usted.conoce.a.sus.vecinos.
  c_1 = contar_reactivos(indice1)
  c1_1 = 204-c_1 
  
  indice2 <- archivo$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.a.los.niños
  c_2 = contar_reactivos(indice2)
  
  indice3 <- archivo$Usted.conoce.a.sus.vecinos..A.alguno.le.confiaría.su.casa
  c_3 = contar_reactivos(indice3)
  
  indice4 <- archivo$Usted.conoce.a.sus.vecinos..Participa.con.ellos.para.mejorar.la.seguridad
  c_4 = contar_reactivos(indice4)
  
  indice5 <- archivo$Usted.conoce.a.sus.vecinos..Le.interesaría.participar
  c_5 = contar_reactivos(indice5)
  
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  
  RESPUESTA = c("Le confiaría a sus niños", "Le confiaría su casa", "Participa para mejorar la seguridad", "Le Interesa participar")
  n <- c(c_2,c_3,c_4,c_5)
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p3_1 <- function(archivo,N,z){
  indice1 <- archivo$Participa.con.la.autoridad.para.mejorar.la.seguridad.
  c_1 = contar_reactivos(indice1)
  c1_1 = N-c_1
  
  c_1 = as.integer((c_1/N)*100)
  c1_1 = as.integer((c1_1/N)*100)

  
  RESPUESTA <- c("Si", "No")
  n <- c(c_1,c1_1)
  zona <- c(z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
  
}
p3_2 <- function(archivo,N,z){
  indice1 <- archivo$Participa.con.la.autoridad.para.mejorar.la.seguridad.
  c_1 = contar_reactivos(indice1) 
  
  indice2 <- archivo$Participa.con.la.autoridad.para.mejorar.la.seguridad..Le.interesaría.participar.
  c_2 = contar_reactivos(indice2)
  
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((((N-c_1)-c_2)/N)*100)

  RESPUESTA <- c("Si","No")
  n <- c(c_2,c3)
  zona <- c(z,z)
  dff <- data.frame(RESPUESTA, n, zona)
  return(dff)
}
p6 <- function(archivo,N,z){
  indice1 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.reÃºnen
  c_1 = contar_reactivos(indice1)
  
  indice2 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Se.organizan.para.vigilar
  c_2 = contar_reactivos(indice2)
  
  indice3 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Intercambian.números.telefónicos
  c_3 = contar_reactivos(indice3)
  
  indice4 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Forman.un.chat
  c_4 = contar_reactivos(indice4)
  
  indice5 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Ponen.letreros.de.advertencia
  c_5 = contar_reactivos(indice5)
  
  indice6 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Llaman.a.la.policía
  c_6 = contar_reactivos(indice6)
  
  indice7 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Denuncian.ante.la.autoridad
  c_7 = contar_reactivos(indice7)
  
  indice8 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Vigilan
  c_8 = contar_reactivos(indice8)
  
  indice9 <- archivo$Cuando.hay.un.delito..en.esta.calle.o.zona.los.vecinos..Buscan.desquitarse
  c_9 = contar_reactivos(indice9)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  c_9 = as.integer((c_9/N)*100)
  
  RESPUESTA <- c("Se Reunen", "Se Organizan para vigilar", "Intercambian números", "Forman un Chat", "Ponen Letreros", "Llaman a la Policía", "Denuncian","Vigilan", "Buscan Desquitarse")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9)
  zona <- c(z,z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n, zona)
  return(dff)
}
p7 <- function(archivo,N,z){
  indice1 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.casa
  c_1 = contar_reactivos(indice1)
  
  indice2 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.la.calle
  c_2 = contar_reactivos(indice2)
  
  indice3 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.transporte
  c_3 = contar_reactivos(indice3)
  
  indice4 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.en.negocio
  c_4 = contar_reactivos(indice4)
  
  indice5 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.partes.de.auto
  c_5 = contar_reactivos(indice5)
  
  indice6 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Robo.de.vehículo
  c_6 = contar_reactivos(indice6)
  
  indice7 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Balaceras
  c_7 = contar_reactivos(indice7)
  
  indice8 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Cobro.de.piso
  c_8 = contar_reactivos(indice8)
  
  indice9 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Violencia.familiar
  c_9 = contar_reactivos(indice9)
  
  indice10 <- archivo$Durante.el.último.año..en.esta.calle.o.zona.ha.habido..Peleas.de.gallos
  c_10 = contar_reactivos(indice10)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  c_9 = as.integer((c_9/N)*100)
  c_10 = as.integer((c_10/N)*100)
  
  RESPUESTA <- c("Robo en Casa", "Robo en la Calle", "Robo en Transporte", "Robo en Negocio", "Robo de Partes auto", "Robo de Vehículo", "Balacera","Cobro de piso", "Violencia familiar", "Peleas de gallos o perros")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10)
  zona <- c(z,z,z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
  
}
p8_1 <- function(archivo,N,z){
  indice1 <- archivo$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.casa
  c_1 <- contar_react(indice1)
  
  c_1[1] = as.integer((c_1[1]/N)*100)
  c_1[2] = as.integer((c_1[2]/N)*100)
  c_1[3] = as.integer((c_1[3]/N)*100)
  c_1[4] = as.integer((c_1[4]/N)*100)
  c_1[5] = as.integer((c_1[5]/N)*100)
  
  #En su casa
  RESPUESTA <- c("1 - Muy bajo", "2 - Bajo", "3 - Medio", "4 - Alto", "5 - Muy alto")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4],c_1[5])
  zona <- c(z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p8_2 <- function(archivo,N,z){
  
  indice2 <- archivo$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.su.calle
  c_2 <- contar_react(indice2)
  
  c_2[1] = as.integer((c_2[1]/N)*100)
  c_2[2] = as.integer((c_2[2]/N)*100)
  c_2[3] = as.integer((c_2[3]/N)*100)
  c_2[4] = as.integer((c_2[4]/N)*100)
  c_2[5] = as.integer((c_2[5]/N)*100)
  
  #En esta calle
  RESPUESTA <- c("1 - Muy bajo", "2 - Bajo", "3 - Ni bajo ni alto", "4 - Alto", "5 - Muy alto")
  n <- c(c_2[1],c_2[2],c_2[3],c_2[4],c_2[5])
  zona <- c(z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p8_3 <- function(archivo,N,z){
  indice3 <- archivo$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.zona
  c_3 <- contar_react(indice3)
  
  c_3[1] = as.integer((c_3[1]/N)*100)
  c_3[2] = as.integer((c_3[2]/N)*100)
  c_3[3] = as.integer((c_3[3]/N)*100)
  c_3[4] = as.integer((c_3[4]/N)*100)
  c_3[5] = as.integer((c_3[5]/N)*100)
  
  #En esta zona
  RESPUESTA <- c("1 - Muy bajo", "2 - Bajo", "3 - Ni bajo ni alto", "4 - Alto", "5 - Muy alto")
  n <- c(c_3[1],c_3[2],c_3[3],c_3[4],c_3[5])
  zona <- c(z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)

  return(dff)
}
p8_4 <- function(archivo,N,z){
  indice4 <- archivo$Valore.del.1.al.5.el.riesgo.de.sufrir.un.delito.en.alguno.de.los.siguientes.lugares..En.esta.ciudad
  c_4 <- contar_react(indice4)
  
  c_4[1] = as.integer((c_4[1]/N)*100)
  c_4[2] = as.integer((c_4[2]/N)*100)
  c_4[3] = as.integer((c_4[3]/N)*100)
  c_4[4] = as.integer((c_4[4]/N)*100)
  c_4[5] = as.integer((c_4[5]/N)*100)
  
  #En esta ciudad
  RESPUESTA <- c("1 - Muy bajo", "2 - Bajo", "3 - Ni bajo ni alto", "4 - Alto", "5 - Muy alto")
  n <- c(c_4[1],c_4[2],c_4[3],c_4[4],c_4[5])
  zona <- c(z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p9 <- function(archivo,N,z){
  indice1 <- archivo$X.Usted.ha.sido.víctima.de.algún.delito.en.el.último.año.
  c_1 <- contar_reactivos(indice1)
  c1_1 <- N-c_1
  
  c_1 = as.integer(((c_1)/N)*100)
  c1_1 = as.integer(((c1_1)/N)*100)
  
  RESPUESTA <- c("Si", "No")
  n <- c(c_1, c1_1)
  zona <- c(z,z)
  
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p10 <- function(archivo,N,z){
  indice1 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Llama.a.la.policía
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Hace.una.denuncia
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.su.familia.del.peligro
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Advierte.a.sus.vecinos.del.peligro
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Busca.desquitarse
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.caso.de.ser.víctima.del.delito..Usted..Amenazas
  c_6 <- contar_reactivos(indice6)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  
  RESPUESTA <- c("Llama a la policía", "Denuncia", "Advierte a su familia", "Advierte a sus vecinos", "Busca Desquitarse", "Amenaza")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6)
  zona <- c(z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n, zona)
  
  return(dff)
}
p11_1 <- function(archivo,N,z){
  indice0 <- archivo$En.caso.de.haber.sido.víctima.de.algín.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció.
  c_0 = contar_reactivos(indice0)
  c_1 = N-c_0
  
  c_0 = as.integer((c_0/N)*100)
  c_1 = as.integer((c_1/N)*100)
  
  n <- c(c_0,c_1)
  RESPUESTA <- c("Si", "No")
  zona <- c(z,z)
  dff <- data.frame(RESPUESTA, n, zona)
  
  return(dff)
}
p11_2 <- function(archivo,N,z){
  indice0 <- archivo$En.caso.de.haber.sido.víctima.de.algín.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció.
  c_0 = contar_reactivos(indice0)
  
  indice1 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.pruebas
  c_1 = contar_reactivos(indice1)
  
  indice2 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Considera.que.es.un.delito.de.poca.importancia
  c_2 = contar_reactivos(indice2)
  
  indice3 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Conoce.al.agresor.o.agresores
  c_3 = contar_reactivos(indice3)
  
  indice4 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconfía.de.las.autoridades
  c_4 = contar_reactivos(indice4)
  
  indice5 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Teme.a.que.lo.extorsionen
  c_5 = contar_reactivos(indice5)
  
  indice6 <-archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Falta.de.tiempo
  c_6 = contar_reactivos(indice6)
  
  indice7 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Son.trámites.complicados
  c_7 = contar_reactivos(indice7)
  
  indice8 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Desconoce.dónde.denunciar
  c_8 = contar_reactivos(indice8)
  
  indice9 <- archivo$En.caso.de.haber.sido.víctima.de.algún.delito.y.no.haber.denunciado...Podría.señalar.las.razones.por.las.que.no.denunció..Aunque.denuncie.no.va.a.pasar.nada
  c_9 = contar_reactivos(indice9)
  
  c_0 = as.integer((c_0/N)*100)
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  c_9 = as.integer((c_9/N)*100)
  
  RESPUESTA <- c("Falta de pruebas", "Delito de Poca Importancia", "Conoce al agresor", "Desconfía de Autoridad", "Teme extorsión", "Falta Tiempo", "Son trámites complicados", "No sabe donde denunciar", "Impunidad")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9)
  zona <- c(z,z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p12 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona..Los.padres.participan.en.actividades.con.hijos
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona..Los.vecinos.se.organizan.para.prevenir.delitos
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona..Las.personas.son.amables
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona..Hay.alguna.persona.que.siempre.ayuda.a.los.demás
  c_4 <- contar_reactivos(indice4)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  
  RESPUESTA <- c("Padres participan en actividades con hijos", "Vecinos se organizan para prevenir delitos", "Personas son amables", "Una persona siempre ayuda a los demás")
  n <- c(c_1,c_2,c_3,c_4)
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p13 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.personas..A.las.que.todos.tienen.miedo
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona.hay.personas..Que.acosan.a.menores
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona.hay.personas..Que.acosan.a.mujeres
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona.hay.personas..Que.se.emborrachan.o.se.drogan
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona.hay.personas..Que.han.estado.en.la.cárcel
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.calle.o.zona.hay.personas..Sospechosas
  c_6 <- contar_reactivos(indice6)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  
  RESPUESTA <- c("A las que todos tienen miedo", "Que acosan menores", "Que acosan mujeres", "Que se emborrachan o drogan", "Han estado en la cárcel", "Sospechosas")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6)
  zona <- c(z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p14 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.violencia..Entre.mujeres
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona.hay.violencia..Entre.hombres
  c_2 <- contar_reactivos(indice2)
  
  indice3 <-archivo$En.esta.calle.o.zona.hay.violencia..Entre.familias
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona.hay.violencia..Entre.adultos.y.jóvenes
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona.hay.violencia..Entre.jóvenes
  c_5 <- contar_reactivos(indice5)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  
  RESPUESTA <- c("Entre mujeres", "Entre hombres", "Entre familias", "Entre Adultos y jóvenes", "Entre Jóvenes")
  n <- c(c_1,c_2,c_3,c_4,c_5)
  zona <- c(z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p15 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Conciliando.con.una.tercera.persona
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Dialogando
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..De.manera.respetuosa
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..A.gritos
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.golpes
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.cuchillos..navajas.o.machetes
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Con.armas.de.fuego..como.pistolas.o.rifles
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.esta.calle.o.zona..cuando.hay.conflictos.entre.vecinos.se.manejan..Desquitándose.del.otro
  c_8 <- contar_reactivos(indice8)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  
  RESPUESTA <- c("Conciliando con una tercera persona", "Dialogando", "De manera Respetuosa", "A gritos", "Con golpes", "Con Cuchillos", "Con Armas De Fuego", "Desquitándose")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8)
  zona <- c(z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p16_1 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave.
  c_1 <- contar_reactivos(indice1)
  c_2 <- N-c_1
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  
  RESPUESTA <- c("Si", "No")
  n <- c(c_1,c_2)
  zona <- c(z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p16_2 <- function(archivo, N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave.
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Por.la.inseguridad
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Descuido.de.los.padres
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Castigo
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona.hay.niños.o.adolescentes.que.se.quedan.encerrados.con.llave..Trabajo.de.los.padres
  c_5 <- contar_reactivos(indice5)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  
  RESPUESTA <- c("Por la Inseguridad", "Descuido", "Por Castigo", "Trabajo de los Padres")
  n <- c(c_2,c_3,c_4,c_5)
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  return(dff)
}
p17_1 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer.
  c_1 <- contar_reactivos(indice1)
  c_2 <- N-c_1
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  
  RESPUESTA <- c("Si", "No")
  n <- c(c_1,c_2)
  zona <- c(z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p17_2 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.niños.que.se.quedan.la.mayor.parte.del.día.sin.comer.
  c_1 <- contar_reactivos(indice1)
  
  RESPUESTA <- c("Por Descuido", "Castigo", "Falta de Dinero", "Trabajo de los Padres")
  n <- c(0,0,0,0)
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p18 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Hacen.deporte
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Ayudan.a.los.demás
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..La.mayoria.de.los.jovenes.estudian.o.trabajan
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Andan.en.pandillas
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Andan.armados
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Destruyen.o.vandalizan.propiedad.ajena
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Son.violentos
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.esta.calle.o.zona.hay.jóvenes.que..Amenazan.a.los.vecinos
  c_8 <- contar_reactivos(indice8)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  
  RESPUESTA <- c("Hacen deporte", "Ayudan a los demás", "Estudian o Trabajan", "Andan en Pandillas", "Andan Armados", "Destruyen ó Vandalizan", "Son violentos", "Amenazan a los vecinos")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8)
  zona <- c(z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p20 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.hay..Banquetas
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.hay..Baches
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.hay..Letreros.con.nombres.de.las.calles
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.calle.hay..Tiendita
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.calle.hay..Alumbrado
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.calle.hay..Consumo.del.alcohol.en.la.calle
  c_6 <- contar_reactivos(indice6)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  
  
  RESPUESTA <- c("Banquetas", "Baches", "Letreros con nombres de las calles", "Tiendita", "Alumbrado", "Consumo de alcohol en la calle")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6)
  zona <- c(z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p21 <- function(archivo,N){
  indice1 <- archivo$En.esta.zona.hay..Horarios.de.transporte.que.convienen
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.zona.hay..Una.parada.de.camión.cerca.de.esta.casa
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.zona.hay..Terrenos.baldíos
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$En.esta.zona.hay..Basura
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$En.esta.zona.hay..Autos.abandonados
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.zona.hay..Casas.abandonadas
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.zona.hay..Vandalismo
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.esta.zona.hay..Grafiti
  c_8 <- contar_reactivos(indice8)
  
  indice9 <- archivo$En.esta.zona.hay..Venta.de.thinner.o.pegamento.a.menores
  c_9 <- contar_reactivos(indice9)
  
  indice10 <- archivo$En.esta.zona.hay..Venta.de.alcohol.o.cigarros.a.menores
  c_10 <- contar_reactivos(indice10)
  
  indice11 <- archivo$En.esta.zona.hay..Venta.de.droga
  c_11 <- contar_reactivos(indice11)
  
  indice12 <- archivo$En.esta.zona.hay..Venta.de.alcohol.después.de.las.11.00.de.la.noche
  c_12 <- contar_reactivos(indice12)
  
  RESPUESTA <- c("Horarios de transporte que convienen", "Parada de camión cerca de esta casa", "Terrenos baldíos", "Basura", "Autos abandonados", "Casas abadonadas", "Bandalismo", "Grafiti", "Venta de tiner o pegamento a menores", "Venta de alcohol o cigarros a menores", "Venta de droga", "Venta de alcohol después de las 11p.m")
  n <- c(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10,c_11,c_12)
  dff <- data.frame(RESPUESTA, n)
  
  AP21 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "En esta zona hay: ",N)
  graficarPie(AP21)
}
p22 <- function(archivo,N){
  indice1 <- archivo$Usted.supo.que.algún.menor.de.18.años..Se.fue.de.la.casa
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$Usted.supo.que.algún.menor.de.18.años..Sufrió.violencia
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$Usted.supo.que.algún.menor.de.18.años..Abandonó.la.escuela
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$Usted.supo.que.algún.menor.de.18.años..Tiene.problemas.de.conducta
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$Usted.supo.que.algún.menor.de.18.años..Quedó.embarazada
  c_5 <- contar_reactivos(indice5)
  
  
  RESPUESTA <- c("Se fue de la casa", "SufriÃ³ violencia", "AbandonÃ³ la escuela", "Tiene problemas de conducta", "Quedó embarazada")
  n <- c(c_1,c_2,c_3,c_4,c_5)
  dff <- data.frame(RESPUESTA, n)
  
  AP22 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Usted supo de algún menor de 18 años que: ",N)
  graficarPie(AP22)
}
p23 <- function(archivo,N){
  indice1 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Castigarle
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Gritarle
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.nalgadas
  c_3 <- contar_reactivos(indice3)
  
  indice4 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Darle.una.golpiza...cueriza
  c_4 <- contar_reactivos(indice4)
  
  indice5 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Explicarle.lo.que.está.mal
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Aconsejarle
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$Para.corregir.a.un.niño.o.niña.que.se.porta.mal..Usted.recomienda..Enseñar.con.el.ejemplo
  c_7 <- contar_reactivos(indice7)
  
  RESPUESTA <- c("Castigarle", "Gritarle", "Darle nalgadas", "Darle una golpiza/cueriza", "Explicarle lo que estÃ¡ mal", "Aconsejarle", "EnseÃ±ar con el ejemplo")
  n <- c(c_1,c_2,c_3,c_4,c_5, c_6, c_7)
  dff <- data.frame(RESPUESTA, n)
  
  AP23 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Para corregir a un niÃ±o/niÃ±a que se porta mal, usted recomienda:  ",N)
  graficarPie(AP23)
}
p24 <- function(archivo,N){
  indice1 <- archivo$En.esta.casa...APLICA.TARJETON..Todos.se.conocen
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.casa...APLICA.TARJETON..Platican.unos.con.otros
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.casa...APLICA.TARJETON..Comen.juntos
  c_3 <- contar_reactivos(indice3)
  
  indice5 <- archivo$En.esta.casa...APLICA.TARJETON..Se.ayudan.con.los.gastos
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.casa...APLICA.TARJETON..Discuten
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.casa...APLICA.TARJETON..Se.gritan.entre.sí
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.esta.casa...APLICA.TARJETON..Llegan.a.los.golpes
  c_8 <- contar_reactivos(indice8)
  
  indice9 <- archivo$En.esta.casa...APLICA.TARJETON..Se.ignoran
  c_9 <- contar_reactivos(indice9)
  
  RESPUESTA <- c("Todos se conocen", "Platican unos con otros", "Comen juntos", "Se ayudan con los gastos", "Discuten", "Se gritan entre sí", "Llegan a los golpes", "Se ignoran")
  n <- c(c_1,c_2,c_3,c_5, c_6, c_7,c_8,c_9)
  dff <- data.frame(RESPUESTA, n)
  
  AP24 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "En esta casa:  ",N)
  graficarPie(AP24)
}
p25 <- function(archivo,N){
  indice1 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Tiene.discapacidad
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Ha.sufrido.violencia.debido.a.su.discapacidad
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Sabe.manejar.armas.de.fuego..como.pistolas.o.rifles
  c_3 <- contar_reactivos(indice3)
  
  indice5 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Habla.de.comprar.armas.de.fuego
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..No.habla.español
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.obesidad
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.fumar
  c_8 <- contar_reactivos(indice8)
  
  indice9 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.beber
  c_9 <- contar_reactivos(indice9)
  
  indice10 <- archivo$En.esta.casa.alguien..APLICA.TARJETON..Necesita.ayuda.por.drogas
  c_10 <- contar_reactivos(indice9)
  
  RESPUESTA <- c("Tiene discapacidad", "Ha sufrido violencia por discapacidad", "Sabe manejar armas de fuego", "Habla de comprar armas de fuego", "Habla lengua indígena", "Necesita ayuda por obesidad", "Necesita ayuda por fomar", "Necesita ayuda por beber", "Necesita ayuda por drogas")
  n <- c(c_1,c_2,c_3,c_5, c_6, c_7,c_8,c_9, c_10)
  dff <- data.frame(RESPUESTA, n)
  
  AP25 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "En esta casa alguien:  ",N)
  graficarPie(AP25)
}
p26 <- function(archivo,N,z){
  indice1 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.casa
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.ciudad
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.estado
  c_3 <- contar_reactivos(indice3)
  
  indice5 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiarse.de.trabajo
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.ha.pensado.en..Cerrar.su.negocio
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.elúltimo.año..por.cuestiones.de.seguridad.ha.pensado.en..Cambiar.a.los.hijos.de.escuela
  c_7 <- contar_reactivos(indice7)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  #c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  
  RESPUESTA <- c("Cambiarse de casa", "Cambiarse de ciudad", "Cambiarse de estado", "Cambiarse de trabajo", "Cerrar su negocio", "Cambiar a los hijos de escuela")
  n <- c(c_1,c_2,c_3,c_5, c_6, c_7)
  zona <- c(z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p27 <- function(archivo,N,z){
  indice1 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.de.noche
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.salir.a.caminar.o.hacer.ejercicio
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Impidió.que.los.niños.salieran.a.la.calle
  c_3 <- contar_reactivos(indice3)
  
  indice5 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Evitó.relacionarse.con.nuevas.personas
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.visitar.a.parientes.o.amigos
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.transporte.público.combi
  c_7 <- contar_reactivos(indice7)
  
  indice8 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.taxi
  c_8 <- contar_reactivos(indice8)
  
  indice9 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.llevar.mucho.dinero.en.efectivo
  c_9 <- contar_reactivos(indice9)
  
  indice10 <- archivo$En.el.último.año..por.cuestiones.de.seguridad.Usted..Dejó.de.usar.joyas
  c_10 <- contar_reactivos(indice10)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  #c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  c_8 = as.integer((c_8/N)*100)
  c_9 = as.integer((c_9/N)*100)
  c_10 = as.integer((c_10/N)*100)
  
  RESPUESTA <- c("Salir de noche", "Salir a caminar o hacer ejercicio", "Impidió que los niños salgan a la calle", "Evitó relacionarse con nuevas personas", "Dejó de visitar parientes o amigos", "Dejó de usar transporte público/combi", "Dejó de usat taxi", "Dejó de llevar mucho dinero en efectivo", "Dejó de usar joyas")
  n <- c(c_1,c_2,c_3,c_5, c_6, c_7,c_8,c_9,c_10)
  zona <- c(z,z,z,z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
  
}
p28 <- function(archivo,N,z){
  indice1 <- archivo$En.esta.calle.o.zona.la.policia.Cuida.o.vigila.bien
  c_1 <- contar_reactivos(indice1)
  
  indice2 <- archivo$En.esta.calle.o.zona.la.policia.Comete.abusos
  c_2 <- contar_reactivos(indice2)
  
  indice3 <- archivo$En.esta.calle.o.zona.la.policia.Acude.a.los.llamados
  c_3 <- contar_reactivos(indice3)
  
  indice5 <- archivo$En.esta.calle.o.zona.la.policia.Pide.mordidas
  c_5 <- contar_reactivos(indice5)
  
  indice6 <- archivo$En.esta.calle.o.zona.la.policia.Comete.delitos
  c_6 <- contar_reactivos(indice6)
  
  indice7 <- archivo$En.esta.calle.o.zona.la.policia.Hace.rondines
  c_7 <- contar_reactivos(indice7)
  
  c_1 = as.integer((c_1/N)*100)
  c_2 = as.integer((c_2/N)*100)
  c_3 = as.integer((c_3/N)*100)
  #c_4 = as.integer((c_4/N)*100)
  c_5 = as.integer((c_5/N)*100)
  c_6 = as.integer((c_6/N)*100)
  c_7 = as.integer((c_7/N)*100)
  
  RESPUESTA <- c("Cuida o vigila bien", "Comete abusos", "Acude a los llamados", "Pide mordidas", "Comete delitos", "Hace rondines")
  n <- c(c_1,c_2,c_3,c_5, c_6, c_7)
  zona <- c(z,z,z,z,z,z)
  dff <- data.frame(RESPUESTA, n,zona)
  
  return(dff)
}
p29_1 <- function(archivo,N){
  indice1 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.policia
  c_1 <- contar_react(indice1)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4],c_1[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_1 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en la policía:  ", N)
  graficarPie(AP29_1)
}
p29_2 <- function(archivo,N){
  indice2 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Ministerio.Público.para.denunciar
  c_2 <- contar_react(indice2)

  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_2[1],c_2[2],c_2[3],c_2[4],c_2[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_2 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en el ministerio público para denunciar:  ", N)
  graficarPie(AP29_2)
}
p29_3 <- function(archivo,N){
  indice3 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...La.institución.educativa.de.la.zona
  c_3 <- contar_react(indice3)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_3[1],c_3[2],c_3[3],c_3[4],c_3[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_3 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en las esculas públicas de la zona  ", N)
  graficarPie(AP29_3) 
}
p29_4 <- function(archivo,N){
  indice4 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.comisario.ejidal
  c_4 <- contar_react(indice4)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_4[1],c_4[2],c_4[3],c_4[4],c_4[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_4 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en su comisario ejidal:  ", N)
  graficarPie(AP29_4)
}
p29_5 <- function(archivo,N){
  indice5 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...Su.presidente.municipal
  c_5 <- contar_react(indice5)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_5[1],c_5[2],c_5[3],c_5[4],c_5[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_5 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en su presidente municipal:  ", N)
  graficarPie(AP29_5)
}
p29_6 <- function(archivo,N){
  indice6 <- archivo$Del.1.al.5..con.el.5.como.mejor.calificación..califique.la.confianza.que.Usted.tiene.en...El.Gobernador
  c_6 <- contar_react(indice6)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_6[1],c_6[2],c_6[3],c_6[4],c_6[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP29_6 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en el Gobernador: ", N)
  graficarPie(AP29_6)
}
p30_1 <- function(archivo,N){
  indice1 <- archivo$Del.1.al.5..califique.el.trabajo.de..La.policia
  c_1 <- contar_react(indice1)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4],c_1[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_1 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo de la policía:  ", N)
  graficarPie(AP30_1)
}
p30_2 <- function(archivo,N){
  indice2 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..El.Ministerio.Público.para.denunciar
  c_2 <- contar_react(indice2)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_2[1],c_2[2],c_2[3],c_2[4],c_2[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_2 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo del ministerio público para denunciar:  ", N)
  graficarPie(AP30_2)
}
p30_3 <- function(archivo,N){
  indice3 <- archivo$Del.1.al.5..califique.el.trabajo.de..Los.empleados.de.gobierno
  c_3 <- contar_react(indice3)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_3[1],c_3[2],c_3[3],c_3[4],c_3[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_3 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo de los empleados de gobierno: ", N)
  graficarPie(AP30_3)
}
p30_4 <- function(archivo,N){
  indice4 <- archivo$Del.1.al.5..califique.el.trabajo.de..Su.comisario.ejidal
  c_4 <- contar_react(indice4)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_4[1],c_4[2],c_4[3],c_4[4],c_4[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_4 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo de su comisario ejidal:  ", N)
  graficarPie(AP30_4)
}
p30_5 <- function(archivo,N){
  indice5 <- archivo$Del.1.al.5..califique.el.trabajo.de..Su.presidente.municipal
  c_5 <- contar_react(indice5)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_5[1],c_5[2],c_5[3],c_5[4],c_5[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_5 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo de su presidente municipal:  ", N)
  graficarPie(AP30_5)
}
p30_6 <- function(archivo,N){
  indice6 <- archivo$Del.1.al.5..califique.el.trabajo.de..El.Gobernador
  c_6 <- contar_react(indice6)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_6[1],c_6[2],c_6[3],c_6[4],c_6[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_6 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trabajo del gobernador: ", N)
  graficarPie(AP30_6)
}

p31_1 <- function(archivo,N){
  indice1 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..La.policia
  c_1 <- contar_react(indice1)
  
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4],c_1[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_1 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe de la policía:  ", N)
  graficarPie(AP30_1)
}
p31_2 <- function(archivo,N){
  indice2 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..El.Ministerio.Público.para.denunciar
  c_2 <- contar_react(indice2)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_2[1],c_2[2],c_2[3],c_2[4],c_2[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_2 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe del ministerio público para denunciar:  ", N)
  graficarPie(AP30_2)
}
p31_3 <- function(archivo,N){
  indice3 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..Los.empleados.de.gobierno
  c_3 <- contar_react(indice3)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_3[1],c_3[2],c_3[3],c_3[4],c_3[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_3 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe de los empleados de gobierno: ", N)
  graficarPie(AP30_3)
}
p31_4 <- function(archivo,N){
  indice4 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..Su.comisario.ejidal
  c_4 <- contar_react(indice4)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_4[1],c_4[2],c_4[3],c_4[4],c_4[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_4 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe de su comisario ejidal:  ", N)
  graficarPie(AP30_4)
}
p31_5 <- function(archivo,N){
  indice5 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..Su.presidente.municipal
  c_5 <- contar_react(indice5)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_5[1],c_5[2],c_5[3],c_5[4],c_5[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_5 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe de su presidente municipal:  ", N)
  graficarPie(AP30_5)
}
p31_6 <- function(archivo,N){
  indice6 <- archivo$Del.1.al.5..califique.el.trato.que.recibe.de..El.Gobernador
  c_6 <- contar_react(indice6)
  RESPUESTA <- c("1", "2", "3", "4", "5")
  n <- c(c_6[1],c_6[2],c_6[3],c_6[4],c_6[5])
  dff <- data.frame(RESPUESTA, n)
  
  AP30_6 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Del 1 al 5, califique el trato que recibe del gobernador: ", N)
  graficarPie(AP30_6)
}
contar_sexo <- function(indice){
  c=0
  h=0
  m=0
  for(x in indice){
    if(x == "HOMBRE"){
      h = h+1
    }else{
      if(x == "MUJER"){
        m = m+1
      }
    }
  }
  resp = c(h,m)
  return(resp)
}
p32 <- function(archivo,N){
  indice1 <- archivo$Sexo
  c_1 = contar_sexo(indice1)
  
  RESPUESTA <- c("Hombre","Mujer")
  n <- c(c_1[1],c_1[2])
  dff <- data.frame(RESPUESTA,n)
  
  AP32 = graficarPlot(dff ,'steelblue',"USO", "Numero de personas", "Sexo: ", N)
  graficarPie(AP32)
}
cont_tmp <- function(indice){
  a=0
  b=0
  c=0
  d=0
  for (x in indice) {
    if(x == "MENOS DE UN AÑO"){
      a = a+1
    }else{
      if(x == "ENTRE 1 AÑO Y 5 AÑOS"){
        b = b+1
      }else{
        if(x == "ENTRE 6 Y 10 AÑOS"){
          c = c+1
        }else{
          if(x == "MÁS DE 10 AÑOS"){
            d = d+1
          }
        }
      }
    }
  }
  resp = c(a,b,c,d)
  return(resp)
}
p38 <- function(archivo,N,z){
  indice1 = archivo$Tiempo.que.ha.vivido.en.Q..Roo
  c_1 = cont_tmp(indice1)
  
  c_1[1] = as.integer((c_1[1]/N)*100)
  c_1[2] = as.integer((c_1[2]/N)*100)
  c_1[3] = as.integer((c_1[3]/N)*100)
  c_1[4] = as.integer((c_1[4]/N)*100)

  RESPUESTA <- c("Menos de un año", "Entre 1 y 5 años", "Entre 6 y 10 años", "Más de 10 años")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4])
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA,n,zona)
  return(dff)
}
p39 <- function(archivo,N,z){
  indice1 = archivo$Tiempo.que.ha.vivido.en.esta.casa.
  c_1 = cont_tmp(indice1)
  
  c_1[1] = as.integer((c_1[1]/N)*100)
  c_1[2] = as.integer((c_1[2]/N)*100)
  c_1[3] = as.integer((c_1[3]/N)*100)
  c_1[4] = as.integer((c_1[4]/N)*100)
  
  RESPUESTA <- c("Menos de un año", "Entre 1 y 5 años", "Entre 6 y 10 años", "Más de 10 años")
  n <- c(c_1[1],c_1[2],c_1[3],c_1[4])
  zona <- c(z,z,z,z)
  dff <- data.frame(RESPUESTA,n,zona)
  return(dff)
}
################################
########## Pregunta 1 ##########

isla1 = p1(Isla,N,"Isla")
ejido1 = p1(ejido,N2,"Ejido")
salinas1 = p1(salinas, N3,"Salinas")

c1 = rbind.data.frame(isla1,ejido1,salinas1)

graficar(c1,"En esta calle o zona, usted participa en:","Respuesta","%")

View(salinas)

################################
########## Pregunta 2 ##########
isla1 = p2_1(Isla,N,"Isla")
ejido1 = p2_1(ejido,N2,"Ejido")
salinas1 = p2_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Usted conoce a sus vecinos: ","Respuesta","%")

isla1 = p2_2(Isla,N,"Isla")
ejido1 = p2_2(ejido,N2,"Ejido")
salinas1 = p2_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Usted conoce a sus vecinos: ","Respuesta","%")

################################
########## Pregunta 3 ##########
isla1 = p3_1(Isla,N,"Isla")
ejido1 = p3_1(ejido,N2,"Ejido")
salinas1 = p3_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Participa con la autoridad para mejorar la seguridad: ","Respuesta","%")

isla1 = p3_2(Isla,N,"Isla")
ejido1 = p3_2(ejido,N2,"Ejido")
salinas1 = p3_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Le interesa participar ","Respuesta","%")
################################
########## Pregunta 6 ##########
isla1 = p6(Isla,N,"Isla")
ejido1 = p6(ejido,N2,"Ejido")
salinas1 = p6(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Cuando hay un delito, en esta calle o zona los vecinos: ","Respuesta","%")

################################
########## Pregunta 7 ##########
isla1 = p7(Isla,N,"Isla")
ejido1 = p7(ejido,N2,"Ejido")
salinas1 = p7(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Durante el último año, en esta calle o zona ha habido: ","Respuesta","%")


################################
########## Pregunta 8 ##########
isla1 = p8_1(Isla,N,"Isla")
ejido1 = p8_1(ejido,N2,"Ejido")
salinas1 = p8_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Valore el riesgo de sufrir un delito en su casa: ","Respuesta","%")

isla1 = p8_2(Isla,N,"Isla")
ejido1 = p8_2(ejido,N2,"Ejido")
salinas1 = p8_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Valore el riesgo de sufrir un delito en la calle: ","Respuesta","%")

isla1 = p8_3(Isla,N,"Isla")
ejido1 = p8_3(ejido,N2,"Ejido")
salinas1 = p8_3(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Valore el riesgo de sufrir un delito en esta zona: ","Respuesta","%")

isla1 = p8_4(Isla,N,"Isla")
ejido1 = p8_4(ejido,N2,"Ejido")
salinas1 = p8_4(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Valore el riesgo de sufrir un delito en esta ciudad: ","Respuesta","%")

################################
########## Pregunta 9 ##########
isla1 = p9(Isla,N,"Isla")
ejido1 = p9(ejido,N2,"Ejido")
salinas1 = p9(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
ejido1

graficar(df,"¿Usted ha sido víctima de algún delito en el último año? ","Respuesta","%")

################################
########## Pregunta 10 ##########

isla1 = p10(Isla,N,"Isla")
ejido1 = p10(ejido,N2,"Ejido")
salinas1 = p10(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"¿En caso de haber sido víctima, usted:  ","Respuesta","%")

################################
########## Pregunta 11 ##########

isla1 = p11_1(Isla,N,"Isla")
ejido1 = p11_1(ejido,N2,"Ejido")
salinas1 = p11_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"¿Ha sido víctima de algún delito y no denunció?:  ","Respuesta","%")

isla1 = p11_2(Isla,N,"Isla")
ejido1 = p11_2(ejido,N2,"Ejido")
salinas1 = p11_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Señale las razones por las que no denunció: ","Respuesta","%")

################################
########## Pregunta 12 ##########

isla1 = p12(Isla,N,"Isla")
ejido1 = p12(ejido,N2,"Ejido")
salinas1 = p12(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona:  ","Respuesta","%")

################################
########## Pregunta 13 ##########

isla1 = p13(Isla,N,"Isla")
ejido1 = p13(ejido,N2,"Ejido")
salinas1 = p13(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona hay personas:  ","Respuesta","%")

################################
########## Pregunta 14 ##########

isla1 = p14(Isla,N,"Isla")
ejido1 = p14(ejido,N2,"Ejido")
salinas1 = p14(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona hay violencia:  ","Respuesta","%")

################################
########## Pregunta 15 ##########

isla1 = p15(Isla,N,"Isla")
ejido1 = p15(ejido,N2,"Ejido")
salinas1 = p15(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona, cuando hay conflictos entre vecinos se manejan:","Respuesta","%")

################################
########## Pregunta 16 ##########
isla1 = p16_1(Isla,N,"Isla")
ejido1 = p16_1(ejido,N2,"Ejido")
salinas1 = p16_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:","Respuesta","%")

isla1 = p16_2(Isla,N,"Isla")
ejido1 = p16_2(ejido,N2,"Ejido")
salinas1 = p16_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Señale los motivos: ","Respuesta","%")


################################
########## Pregunta 17 ##########
isla1 = p17_1(Isla,N,"Isla")
ejido1 = p17_1(ejido,N2,"Ejido")
salinas1 = p17_1(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: ","Respuesta","%")

isla1 = p17_2(Isla,N,"Isla")
ejido1 = p17_2(ejido,N2,"Ejido")
salinas1 = p17_2(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"Señale los motivos: ","Respuesta","%")

################################
########## Pregunta 18 ##########
isla1 = p18(Isla,N,"Isla")
ejido1 = p18(ejido,N2,"Ejido")
salinas1 = p18(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)

graficar(df,"En esta calle o zona hay jovenes que: ","Respuesta","%")

################################
########## Pregunta 26 ##########
isla1 = p26(Isla,N,"Isla")
ejido1 = p26(ejido,N2,"Ejido")
salinas1 = p26(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
graficar(df,"En el último año, por cuestiones de seguridad Usted ha pensado: ","Respuesta","%")

################################
########## Pregunta 27 ##########

isla1 = p27(Isla,N,"Isla")
ejido1 = p27(ejido,N2,"Ejido")
salinas1 = p27(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
graficar(df,"En el último año, por cuestiones de seguridad usted dejó de: ","Respuesta","%")

################################
########## Pregunta 28 ##########


isla1 = p28(Isla,N,"Isla")
ejido1 = p28(ejido,N2,"Ejido")
salinas1 = p28(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
graficar(df,"En esta calle o zona, la policía: ","Respuesta","%")

################################
########## Pregunta 38 ##########


isla1 = p38(Isla,N,"Isla")
ejido1 = p38(ejido,N2,"Ejido")
salinas1 = p38(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
graficar(df,"Iiempo que ha vivido en Q.roo ","Respuesta","%")

################################
########## Pregunta 39 ##########


isla1 = p39(Isla,N,"Isla")
ejido1 = p39(ejido,N2,"Ejido")
salinas1 = p39(salinas, N3,"Salinas")

df = rbind.data.frame(isla1,ejido1,salinas1)
graficar(df,"Iiempo que ha vivido en esta casa: ","Respuesta","%")
