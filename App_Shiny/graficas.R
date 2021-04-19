library(ggplot2)
library(reshape2)
library(plotrix)
library(scales)
library(tidyverse)
#setwd("~/UC/Analisis_IslaMujeres/App_Shiny/Barplots")

## Conteo de variables ##
contar <- function(indice){
  i = 0
  for (x in indice) {
    if (x == "1") {
      i = i+1
    }
  }
  return(i)
}
contar_tr <- function(indice){
  i= 0
  j = 0
  k = 0
  l = 0
  for (x in indice) {
    if (x == "COMERCIO") {
      i = i+1
    }else{
      if(x == "INDUSTRIA"){
        j = j+1
      }else{
        if(x == "JUBILADO"){
          k = k+1
        }else{
          if(x == "AGRICULA"){
            l = l+1
          }
        }
      }
    }
  }
  tr = c(i,j,k,l)
  return(tr)
}
contar_tr2 <- function(indice){
  i= 0
  j = 0
  k = 0
  l = 0
  m = 0
  n = 0
  o = 0
  for (x in indice) {
    if (x == "PRIMARIA"){
      i = i+1
    }else{
      if(x == "SECUNDARIA"){
        j = j+1
      }else{
        if(x == "BACHILLERATO"){
          k = k+1
        }else{
          if(x == "LICENCIATURA"){
            l = l+1
          }else{
            if(x == "TECNICO-SUPERIOR"){
              m = m+1
            }else{
              if(x == "PRIMERIA-INCOMPLETA"){
                n = n+1
              }else{
                if(x == "POSGRADO"){
                  o = o+1
                }
              }
            }
          }
        }
      }
    }
  }
  tr = c(i,j,k,l,m,n,o)
  return(tr)
}
contar_tr3 <- function(indice){
  a = 0
  b = 0
  c = 0
  d = 0
  e = 0
  f = 0
  for (x in indice) {
    if(x == "0"){
      a = a+1
    }else{
      if(x == "1"){
        b = b+1
      }else{
        if(x == "2"){
          c = c+1
        }else{
          if(x == "3"){
            d = d+1
          }else{
            if(x == "4"){
              e = e+1
            }else{
              if(x == "5")
                f = f+1
            }
          }
        }
      }
    }
  }
  tr = c(a,b,c,d,e,f)
  return(tr)
}
contar_puesto <- function(indice){
  a = 0
  b = 0
  c = 0
  d = 0
  e = 0
  f = 0
  for (x in indice) {
    if(x == "EMPLEADO-BAJO"){
      a = a+1
    }else{
      if(x == "EMPLEADO-MEDIO"){
        b = b+1
      }else{
        if(x == "JUBILADO"){
          c = c+1
        }else{
          if(x == "OTRO"){
            d = d+1
          }else{
            if(x == "PATRON"){
              e = e+1
            }else{
              if(x == "TRABAJADOR-ARRENDADOR"){
                f = f+1
              }
            }
          }
        }
      }
    }
  }
  tr = c(a,b,c,d,e,f)
  return(tr)
}
contar_salario <- function(indice){
  a = 0
  b = 0
  c = 0
  d = 0
  for (x in indice) {
    if(x == "A"){
      a = a+1
    }else{
      if(x == "B"){
        b = b+1
      }else{
        if(x == "C"){
          c = c+1
        }else{
          if(x == "D"){
            d = d+1
          }
        }
      }
    }
  }
  tr = c(a,b,c,d)
  return(tr)
}
contar_tmp <- function(indice){
  a = 0
  b = 0
  c = 0
  d = 0
  e = 0
  for (x in indice) {
    if(x == "A"){
      a = a+1
    }else{
      if(x == "B"){
        b = b+1
      }else{
        if(x == "C"){
          c = c+1
        }else{
          if(x == "D"){
            d = d+1
          }else{
            if(x == "E"){
              e = e+1
            }
          }
        }
      }
    }
  }
  tr = c(a,b,c,d)
  return(tr)
}

#Datos familiares
transporte <- function(df, N){
  a = df$DF_tr_autobus
  a_0 = contar(a)
  
  b = df$DF_tr_colectivo
  b_0 = contar(b)
  
  c = df$DF_tr_moto
  c_0 = contar(c)
  
  d = df$DF_tr_mototaxi
  d_0 = contar(d)
  
  e = df$DF_tr_taxi
  e_0 = contar(e)
  
  f = df$DF_tr_auto
  f_0 = contar(f)
  
  g = df$DF_tr_otro
  g_0 = contar(g)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  
  RESPUESTA = c("Autobús","Colectivo","Moto","Mototaxi","Taxi","Auto","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  group = c(rep('Transporte', 7))
  dff <- data.frame(RESPUESTA, n,group, total)
  return(dff)
}
viveres <- function(df, N){
  a = df$DF_v_col
  a_0 = contar(a)
  
  b = df$DF_v_abarrote
  b_0 = contar(b)
  
  c = df$DF_v_super
  c_0 = contar(c)
  
  d = df$DF_v_conv
  d_0 = contar(d)
  
  e = df$DF_v_plaza
  e_0 = contar(e)
  
  f = df$DF_v_otro
  f_0 = contar(f)
  total  <- c(a_0,b_0,c_0,d_0,e_0,f_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  
  RESPUESTA = c("Mercado de la colonia","Tienda abarrotes","Super mercado","Tienda de conveniencia","Plaza comercial","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0)
  group = c(rep("Víveres",6))
  dff <- data.frame(RESPUESTA, n, group, total)
  
  return(dff)
}
medico <- function(df, N){
  a = df$DF_urg_casa
  a_0 = contar(a)
  
  b = df$DF_urg_partcom
  b_0 = contar(b)
  
  c = df$DF_urg_partcan
  c_0 = contar(c)
  
  d = df$DF_urg_hosp
  d_0 = contar(d)
  
  e = df$DF_urg_cruz
  e_0 = contar(e)
  
  f = df$DF_urg_farmacia
  f_0 = contar(f)
  
  g = df$DF_urg_otro
  g_0 = contar(g)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  
  RESPUESTA = c("Me atiendo en casa","Médico part en comunidad","Médico particular Cancún","Hospital general Cancún","Cruz roja","Farmacia","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  group = c(rep("Acude Médico",7))
  dff <- data.frame(RESPUESTA, n,group, total)
  
  return(dff)
}
areas <- function(df, N){
  a = df$DF_a_parque
  a_0 = contar(a)
  
  b = df$DF_a_unidad
  b_0 = contar(b)
  
  c = df$DF_a_jardines
  c_0 = contar(c)
  
  d = df$DF_a_casa
  d_0 = contar(d)
  
  e = df$DF_a_biblioteca
  e_0 = contar(e)
  
  f = df$DF_a_ning
  f_0 = contar(f)
  
  g = df$DF_a_otro
  g_0 = contar(g)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  
  RESPUESTA = c("Parque","Unidad deportiva","Jardines","Casa de la cultura","Biblioteca","Otros(Ninguna)", "Otros(Varios)")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  group = c(rep("Áreas recreo",7))
  dff <- data.frame(RESPUESTA, n,group, total)
  
  dff = dff %>% arrange(n)
  for (x in dff) {
    
  }
  return(dff)
}

#Datos economicos
trabajos <- function(df,N){
  a = df$DE02_trabajo
  a_0 = contar_tr(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  
  RESPUESTA <- c("Comercio", "Industria", "Jubilado", "Agrícola")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4])
  group = c(rep("Trabajo", 4))
  dff <- data.frame(RESPUESTA, n,group, total)
  return(dff)
}
escolaridad <- function(df,N){
  a = df$DE06_esc_jefe
  a_0 = contar_tr2(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6],a_0[7])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  a_0[5] = as.integer((a_0[5]/N)*100)
  a_0[6] = as.integer((a_0[6]/N)*100)
  a_0[7] = as.integer((a_0[7]/N)*100)
  
  RESPUESTA <- c("Primaria", "Secundaria", "Bachillerato","Licenciatura", "Técnico-superior","Primaria incompleta","Posgrado")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6],a_0[7])
  group = c(rep("Escolaridad",7))
  dff <- data.frame(RESPUESTA, n,group, total)
  
  return(dff)
}
puesto <- function(df, N){
  a = df$DE03_puesto
  a_0 = contar_puesto(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  a_0[5] = as.integer((a_0[5]/N)*100)
  a_0[6] = as.integer((a_0[6]/N)*100)
  
  RESPUESTA <- c("Empleado-bajo", "Empleado-medio", "Jubilado","Otro", "Patrón","Trabajador-arrendador")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  group = c(rep("Puesto", 6))
  dff <- data.frame(RESPUESTA, n,group, total)
  return(dff)
}
salario <- function(df, N){
  a = df$DE05_ingreso_sem
  a_0 = contar_salario(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  
  RESPUESTA <- c("$0 a $1200", "$1201 - $2500", "$2501 - $5000","Más de $5000")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4])
  group = c(rep("Ingr x Sem.", 4))
  dff <- data.frame(RESPUESTA, n,group, total)
  return(dff)
}
trabajan <- function(df, N){
  a = df$DE01_hogar_trabajan
  a_0 = contar_tr3(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  a_0[5] = as.integer((a_0[5]/N)*100)
  a_0[6] = as.integer((a_0[6]/N)*100)
  
  RESPUESTA <- c("Ninguno", "1", "2","3", "4","5")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  #group = c(rep("Puesto", 6))
  dff <- data.frame(RESPUESTA, n, total)
  return(dff)
}
sIngreso <- function(df, N){
  a = df$DE07_sIngreso
  a_0 = contar_tr3(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  a_0[5] = as.integer((a_0[5]/N)*100)
  a_0[6] = as.integer((a_0[6]/N)*100)
  
  RESPUESTA <- c("Ninguno", "1", "2","3", "4","5")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5],a_0[6])
  #group = c(rep("Puesto", 6))
  dff <- data.frame(RESPUESTA, n, total)
  return(dff)
}
tiempo <- function(df,N){
  a = df$DE08_tmp
  a_0 = contar_tmp(a)
  total <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5])
  a_0[1] = as.integer((a_0[1]/N)*100)
  a_0[2] = as.integer((a_0[2]/N)*100)
  a_0[3] = as.integer((a_0[3]/N)*100)
  a_0[4] = as.integer((a_0[4]/N)*100)
  a_0[5] = as.integer((a_0[5]/N)*100)
  
  RESPUESTA <- c("Trabaja en casa", "Menos de 20 minutos", "Menos de una hora","Más de una hora","Más de 2 horas")
  n <- c(a_0[1],a_0[2],a_0[3],a_0[4],a_0[5])
  dff <- data.frame(RESPUESTA, n, total)
  return(dff)
}

#Datos identidad y comunidad#
origen <- function(df, N){
  a = df$IyC02_Estado_Origen_CAMPECHE
  a_0 = 38
  
  b = df$IyC02_Estado_Origen_CHIAPAS
  b_0 =51
  
  c = df$IyC02_Estado_Origen_CHIHUAHUA
  c_0 = 1
  
  d = df$IyC02_Estado_Origen_DISTRITO.FEDERAL
  d_0 = 6
  
  e = df$IyC02_Estado_Origen_GUANAJUATO
  e_0 = 1
  
  f = df$IyC02_Estado_Origen_GUERRERO
  f_0 = 8
  
  g = df$IyC02_Estado_Origen_HIDALGO
  g_0 = 1
  
  h = df$IyC02_Estado_Origen_OAXACA
  h_0 = 4
  
  i = df$IyC02_Estado_Origen_PUEBLA
  i_0 = 2
  
  j = df$IyC02_Estado_Origen_QUINTANA.ROO
  j_0 = 52
  
  k = df$IyC02_Estado_Origen_SONORA
  k_0 = 2
  
  l = df$IyC02_Estado_Origen_TABASCO
  l_0 = 80
  
  m = df$IyC02_Estado_Origen_TAMAULIPAS
  m_0 = 5
  
  n = df$IyC02_Estado_Origen_VERACRUZ
  n_0 = 48
  
  o = df$IyC02_Estado_Origen_YUCAT.c1.N
  o_0 = 71
  total <- c(a_0,b_0,c_0, d_0, e_0,f_0,g_0, h_0, i_0, j_0, k_0 ,l_0, m_0, n_0,o_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  h_0 = as.integer((h_0/N)*100)
  i_0 = as.integer((i_0/N)*100)
  j_0 = as.integer((j_0/N)*100)
  k_0 = as.integer((k_0/N)*100)
  l_0 = as.integer((l_0/N)*100)
  m_0 = as.integer((m_0/N)*100)
  n_0 = as.integer((n_0/N)*100)
  o_0 = as.integer((o_0/N)*100)
  
  
  RESPUESTA <- c("Campeche","Chiapas","Chihuahua","DF", "Guanajuato", "Guerrero","Hidalgo","Oaxaca","Puebla","Q.Roo","Sonora", "Tabasco","Tamaulipas" ,"Veracruz","Yucatán")
  n <- c(a_0,b_0,c_0, d_0, e_0,f_0,g_0, h_0, i_0, j_0, k_0 ,l_0, m_0, n_0,o_0)
  group <- c(rep("Origen",5))
  dff <- data.frame(RESPUESTA,n,group, total)
  
  dff = dff %>% arrange(n)
  return(dff)
}
tmp_viv <- function(df,N){
  a = df$IyC03_Tiempo_Viviendo_Lugar_MENOS.DE.UN.A.d1.O
  a_0 = 36
  
  b = df$IyC03_Tiempo_Viviendo_Lugar_ENTRE.UN.A.d1.O.Y.CINCO
  b_0 = 97
  
  c = df$IyC03_Tiempo_Viviendo_Lugar_MAS.DE.5.ANOS
  c_0 = 211
  
  d = df$IyC03_Tiempo_Viviendo_Lugar_TODA
  d_0 = 36
  total <- c(a_0,b_0,c_0,d_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  
  RESPUESTA <- c("Menos de un año", "Entre un año y cinco", "Más de cinco años", "Toda mi vida")
  n <- c(a_0,b_0,c_0,d_0)
  group = c(rep("Tiempo aquí", 4))
  dff <- data.frame(RESPUESTA, n, group, total)
  return(dff)
}
motivo <- function(df, N){
  a = df$IyC04_Motivo_Localidad_Parientes
  a_0 = contar(a)
  
  b = df$IyC04_Motivo_Localidad_Amigos
  b_0 = contar(b)
  
  c = df$IyC04_Motivo_Localidad_Trabajo
  c_0 = contar(c)
  
  d = df$IyC04_Motivo_Localidad_Negocio
  d_0 = contar(d)
  
  e = df$IyC04_Motivo_Localidad_Oportunidad
  e_0 = contar(e)
  
  f = df$IyC04_Motivo_Localidad_Otro
  f_0 = contar(f)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  
  RESPUESTA <- c("Parientes","Amigos","Trabajo","Negocio","Oportunidad","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0)
  group <- c(rep("Motivo venir",6))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
ayuda <- function(df,N){
  a = df$IyC05_Acudo_Vecinos
  a_0 = contar(a)
  
  b = df$IyC05_Acudo_Familia
  b_0 = contar(b)
  
  c = df$IyC05_Acudo_Autoridad
  c_0 = contar(c)
  
  d = df$IyC05_Acudo_Iglesia
  d_0 = contar(d)
  
  e = df$IyC05_Acudo_Otro
  e_0 = contar(e)
  total <- c(a_0,b_0,c_0,d_0,e_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  
  RESPUESTA <- c("Vecinos", "Familia","Autoridad","Iglesia","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0)
  group <- c(rep("Acudo ayuda",5))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
religion <- function(df,N){
  a = df$IyC06_Religion_ADVENTISTA
  a_0 = contar(a)
  
  b = df$IyC06_Religion_CATOLICA
  b_0 = contar(b)
  
  c = df$IyC06_Religion_EVANGELICA
  c_0 = contar(c)
  
  d = df$IyC06_Religion_MORMON
  d_0 = contar(d)
  
  e = df$IyC06_Religion_NINGUNA
  e_0 = contar(e)
  
  f = df$IyC06_Religion_TESTIGOS
  f_0 = contar(f)
  
  g = df$IyC06_Religion_OTRO
  g_0 = contar(g)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  
  RESPUESTA <- c("Adventista", "Católica","Evangélica","Mormón","Ninguna","Testigos","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  group <- c(rep("Religión",7))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
ventajas <- function(df,N){
  a = df$IyC08_Ventajas_Casa
  a_0 = contar(a)
  
  b = df$IyC08_Ventajas_Trabajo
  b_0 = contar(b)
  
  c = df$IyC08_Ventajas_Familia
  c_0 = contar(c)
  
  d = df$IyC08_Ventajas_Tiempo
  d_0 = contar(d)
  
  e = df$IyC08_Ventajas_Tranquilo
  e_0 = contar(e)
  
  f = df$IyC08_Ventajas_Seguro
  f_0 = contar(f)
  
  g = df$IyC08_Ventajas_Otro
  g_0 = contar(g)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  g_0 = as.integer((g_0/N)*100)
  
  RESPUESTA <- c("Tengo casa", "Tengo trabajo","Tengo familia","Tengo más tiempo","Es tranquilo","Es seguro","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0,g_0)
  group <- c(rep("Causa residencia",7))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
irse <- function(df, N){
  a = df$IyC09_Emigrar_LUGAR.DE.ORIGEN
  a_0 = contar(a)
  
  b = df$IyC09_Emigrar_MISMO.ESTADO
  b_0 = contar(b)
  
  c = df$IyC09_Emigrar_NO
  c_0 = contar(c)
  
  d = df$IyC09_Emigrar_QUIZAS
  d_0 = contar(d)
  total <- c(a_0,b_0,c_0,d_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  
  RESPUESTA <- c("Me regreso a mi lugar de origen", "En el mismo estado","No","Quizás")
  n <- c(a_0,b_0,c_0,d_0)
  group <- c(rep("Irse",4))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
pertenencia <- function(df,N){
  a = df$IyC10_Pertenencia_BENITO.JUAREZ
  a_0 = contar(a)
  
  b = df$IyC10_Pertenencia_ISLA.MUJERES
  b_0 = contar(b)
  
  c = df$IyC10_Pertenencia_NINGUNO
  c_0 = contar(c)
  total <- c(a_0,b_0,c_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  
  RESPUESTA <- c("Benito Juárez", "Isla Mujeres","Ninguno")
  n <- c(a_0,b_0,c_0)
  group <- c(rep("Pertenencia",3))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
frecuencia <- function(df, N){
  a = df$IyC11_Frecuencia_Cabecera_Isla_SIEMPRE
  a_0 = contar(a)
  
  b =  df$IyC11_Frecuencia_Cabecera_Isla_CASI.SIEMPRE
  b_0 = contar(b)
  
  c =  df$IyC11_Frecuencia_Cabecera_Isla_FRECUENTEMENTE
  c_0 = contar(c)
  
  d = df$IyC11_Frecuencia_Cabecera_Isla_A.VECES
  d_0 = contar(d)
  
  e = df$IyC11_Frecuencia_Cabecera_Isla_NUNCA
  e_0 = contar(e)
  total <- c(a_0,b_0,c_0,d_0,e_0)
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  
  RESPUESTA <- c("Siempre", "Casi siempre","Frecuentemente","A veces", "Nunca")
  n <- c(a_0,b_0,c_0,d_0,e_0)
  group <- c(rep("Va a Isla",5))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}
motivos <- function(df,N){
  a = df$IyC12_Motivo_Viaja_Asuntos_Admin
  a_0 = contar(a)
  
  b = df$IyC12_Motivo_Viaja_Pago_Servicios
  b_0 = contar(b)
  
  c = df$IyC12_Motivo_Viaja_Trabajo
  c_0 = contar(c)
  
  d = df$IyC12_Motivio_Viaja_Recreacion
  d_0 = contar(d)
  
  e = df$IyC12_Motivo_Viaja_Familia
  e_0 = contar(e)
  
  f = df$IyC12_Motivo_Viaja_Otro
  f_0 = contar(f)
  total <- c(a_0,b_0,c_0,d_0,e_0,f_0) 
  a_0 = as.integer((a_0/N)*100)
  b_0 = as.integer((b_0/N)*100)
  c_0 = as.integer((c_0/N)*100)
  d_0 = as.integer((d_0/N)*100)
  e_0 = as.integer((e_0/N)*100)
  f_0 = as.integer((f_0/N)*100)
  
  RESPUESTA <- c("Asuntos administrativos", "Pago de servicios","Trabajo","Recreación","Familia","Otro")
  n <- c(a_0,b_0,c_0,d_0,e_0,f_0)
  group <- c(rep("Motivos para Isla",6))
  dff <- data.frame(RESPUESTA,n,group, total)
  return(dff)
}

N = nrow(df_fin)
N2 = nrow(df02)
####################################
######## Datos familiares ##########
tr = transporte(df_fin,N)
vivs = viveres(df_fin,N)
med = medico(df_fin,N)
ar = areas(df02,N2)

######## Datos económicos ##########
tr2 = trabajos(df_fin,N)
esc = escolaridad(df_fin,N)
ps = puesto(df_fin,N)
sal = salario(df_fin,N)
trab = trabajan(df_fin,N)
ing = sIngreso(df_fin,N)
tmp = tiempo(df_fin,N)

######## Datos identidad ###########
or = origen(df_fin,N)
viv = tmp_viv(df_fin,N)
mot = motivo(df_fin,N)
ay = ayuda(df_fin,N)
rel = religion(df_fin,N)
ven = ventajas(df_fin,N)
ir = irse(df_fin,N)
pert = pertenencia(df_fin,N)
frec = frecuencia(df_fin,N)
mots = motivos(df_fin,N)

#Graficas de barplots normales#
#Familiares
#Circular bar plots#
#Familiares
#dev.off()

data = rbind(tr,vivs,med,ar)
data = data %>% arrange(group, n)
#¿Cuántas personas viven en esta casa?

#Grafias
PFAM1 = graficarPlot(tr,"Respuesta","%",  "3. ¿Qué medio de transporte utilizan?")
PFAM3 = graficarPlot(med,"Respuesta","%", "5. ¿A dónde acude en caso de urgencia médica?")
PFAM4 = graficarPlot(ar,"Respuesta","%",  "6.¿Con qué áreas de recreo cuenta en su colonia?")

#tablas
TFAM1 = graficarTable(tr,"Respuesta","%", "3. ¿Qué medio de transporte utilizan?")
TFAM3 = graficarTable(med,"Respuesta","%", "4. ¿A dónde acude en caso de urgencia médica?  ")
TFAM4 = graficarTable(ar,"Respuesta","%", "5. ¿Con qué áreas de recreo cuenta en su colonia?")

#Económicos
#Económicos
#Grafias
PECO1 = graficarPlot(tr2,"Respuesta","%","1. ¿Cuál es el principal trabajo pagado del jefe o jefa de familia?")
PECO3 = graficarPlot(ps,"Respuesta","%","2. Puesto o posición en el trabajo del jefe de familia")
PECO4 = graficarPlot(sal,"Respuesta","%","3. ¿A  cuánto asciende el ingreso total semanal del jefe de familia?")
PECO2 = graficarPlot(esc,"Respuesta","%","4. Máximo nivel de estudios completo del jefe o jefa de familia")
PECO6 = graficarPlot(ing,"Respuesta","%","5. ¿Número de personas que dependen del Jefe de Familia?")
#PECO7 = graficarPlot(tmp,"Respuesta","%","6. ¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?")
#PECO5 = graficarPlot(trab,"Respuesta","%","7. Además del jefe de familia ¿Cuántas personas trabajan en el hogar  con salario remunerado?")
#tables
TECO1 = graficarTable(tr2,"Respuesta","%","1. ¿Cuál es el principal trabajo pagado del jefe o jefa de familia?")
TECO3 = graficarTable(ps,"Respuesta","%","2. Puesto o posición en el trabajo del jefe de familia")
TECO4 = graficarTable(sal,"Respuesta","%","3. ¿A  cuánto asciende el ingreso total semanal del jefe de familia?")
TECO2 = graficarTable(esc,"Respuesta","%","4. Máximo nivel de estudios completo del jefe o jefa de familia")
TECO6 = graficarTable(ing,"Respuesta","%","5. ¿Número de personas que dependen del Jefe de Familia?")
#PECO7 = graficarTable(tmp,"Respuesta","%","6. ¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?")
#PECO5 = graficarTable(trab,"Respuesta","%","7. Además del jefe de familia ¿Cuántas personas trabajan en el hogar  con salario remunerado?")


#Identidad y comunidad
#Identidad
PIyC1 = graficarPlot(or,"Respuesta","%","1. ¿Cuál es su lugar de origen")
PIyC2 = graficarPlot(viv,"Respuesta","%","2. ¿Cuánto tiempo lleva viviendo en este lugar?")
PIyC3 = graficarPlot(mot,"Respuesta","%","3. ¿Qué lo motivó a venir a vivir en esta localidad?")
PIyC4 = graficarPlot(ay,"Respuesta","%","4 En caso de requerir ayuda, apoyo legal o económico ante algún problema acudo a:")
PIyC5 = graficarPlot(rel,"Respuesta","%","5. ¿Qué religión practica?")
PIyC6 = graficarPlot(ven,"Respuesta","%","6. ¿Cuáles son las ventajas de vivir en este lugar?")

TIyC1 = graficarTable(or,"Respuesta","%","1. ¿Cuál es su lugar de origen")
TIyC2 = graficarTable(viv,"Respuesta","%","2. ¿Cuánto tiempo lleva viviendo en este lugar?")
TIyC3 = graficarTable(mot,"Respuesta","%","3. ¿Qué lo motivó a venir a vivir en esta localidad?")
TIyC4 = graficarTable(ay,"Respuesta","%","4 En caso de requerir ayuda, apoyo legal o económico ante algún problema acudo a:")
TIyC5 = graficarTable(rel,"Respuesta","%","5. ¿Qué religión practica?")
TIyC6 = graficarTable(ven,"Respuesta","%","6. ¿Cuáles son las ventajas de vivir en este lugar?")





#Comunidad
PCC1 = graficarPlot(ir,"Respuesta","%","¿Piensa irse a vivir a otra localidad?")
PCC2 =graficarPlot(pert,"Respuesta","%","¿Usted a qué municipio siente que pertenece?")
PCC3 =graficarPlot(frec,"Respuesta","%","¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?")
PCC4 =graficarPlot(mots,"Respuesta","%","¿Cuáles son los motivos por los que viaja a la Isla?")

TCC1 = graficarTable(ir,"Respuesta","%","¿Piensa irse a vivir a otra localidad?")
TCC2 =graficarTable(pert,"Respuesta","%","¿Usted a qué municipio siente que pertenece?")
TCC3 =graficarTable(frec,"Respuesta","%","¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?")
TCC4 =graficarTable(mots,"Respuesta","%","¿Cuáles son los motivos por los que viaja a la Isla?")

# DF 1 y 2

A = 1045
B = 589
total = c(A, B)
A = as.integer((A/376)*100)
B = as.integer((B/376)*100)
n = c(A, B)
RESPUESTA = c("ADULTOS", "NIÑOS")
VI04 <- data.frame(n,RESPUESTA,total)
PFAM01 = graficarPlot(VI04 ,"Respuesta ", "%", "1. ¿Cuántas personas viven en esta casa?" )
TFAM01 = graficarTable(VI04 ,"Respuesta ", "%", "1. ¿Cuántas personas viven en esta casa?" )


A = sum(df_fin$DF_kinder == 1)
B = sum(df_fin$DF_primaria == 1)
C = sum(df_fin$DF_secundaria == 1)
D = sum(df_fin$DF_bachillerato == 1)
E = sum(df_fin$DF_licenciatura == 1)
FF = sum(df_fin$DF_posgrado == 1)
to = A+B+C+D+E+FF
total = c(A, B, C, D,E,FF)

A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)
FF = as.integer((FF/to)*100)
n = c(A, B, C, D,E,FF)

RESPUESTA = c("Jardín de niños", "Primaria" , "Secundaria" , "Bachillerato" ,"Licenciatura" , "Posgrado" )
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?
PFAM02 = graficarPlot(VI04,"Respuesta ", "%", "2. ¿ A qué niveles de escolaridad asisten los integrantes de esta familia?")
TFAM02 = graficarTable(VI04,"Respuesta ", "%", "2. ¿ A qué niveles de escolaridad asisten los integrantes de esta familia?")


A = sum(df_fin$DF_v_col == 1)
B = sum(df_fin$DF_v_abarrote == 1)
C = sum(df_fin$DF_v_super == 1)
D = sum(df_fin$DF_v_conv == 1)
E = sum(df_fin$DF_v_plaza == 1)
FF = sum(df_fin$DF_v_otro == 1)
to = A+B+C+D+E+FF
total = c(A, B, C, D,E,FF)

A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)
FF = as.integer((FF/to)*100)
n = c(A, B, C, D,E,FF)

RESPUESTA = c("Mercado de la colonia ", "Tienda de abarrotes, carnicerías","súper mercado","Tienda de conveniencia ", "Plaza comercial", "Otro" )
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PFAM2 = graficarPlot(VI04,"Respuesta","%", "4. ¿Dónde adquiere sus víveres?")
TFAM2 = graficarTable(VI04,"Respuesta","%", "4. ¿Dónde adquiere sus víveres?")



#DE

A = 37
B = 123
C = 161
D = 39

to = A+B+C+D
total = c(A, B, C, D)

A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)

n = c(A, B, C, D)

RESPUESTA = c("menos de 20 minutos", "más de 20 minutos y menos de una hora", "más de una hora", "trabaja en casa" )
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PPECO7 = graficarPlot(VI04,"Respuesta","%", "7. ¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?")
TECO7 = graficarTable(VI04,"Respuesta","%", "7. ¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?")


df_fin
A = 190
B = 110
C = 42
D = 28
E = 5



to = A+B+C+D+E
total = c(A, B, C, D,E)

A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)


n = c(A, B, C, D,E)

RESPUESTA = c("Ninguna" , "Uno", "Dos ", "Tres ", "Cuatro")
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PPECO8 = graficarPlot(VI04,"Respuesta","%", "8. Además del jefe de familia ¿Cuántas personas trabajan en el hogar  con salario remunerado? ")
TECO8 = graficarTable(VI04,"Respuesta","%", "8. Además del jefe de familia ¿Cuántas personas trabajan en el hogar  con salario remunerado? ")



#Identidad y comunidad

#7 

A =  335
B = 5
C = 27
D = 9

to = A+B+C+D
total = c(A, B, C, D)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)



n = c(A, B, C, D)

RESPUESTA = c("NO" , "Mismo estado", "Quizas ", "Lugar de origen ")
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PIyC7 = graficarPlot(VI04,"Respuesta","%", " 7. ¿Piensa irse a vivir a otra localidad?")
TIyC7 = graficarTable(VI04,"Respuesta","%", " 7. ¿Piensa irse a vivir a otra localidad?")


# 8

A =  347
B = 20
C = 9
D = 8
to = A+B+C+D
total = c(A, B, C,D)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)

n =  c(A, B, C,D)

RESPUESTA = c("ISLA-MUJERES" , "BENITO-JUAREZ", "NINGUNO", "DESCONOCE")
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PIyC8 = graficarPlot(VI04,"Respuesta","%", " 8. ¿Usted a qué municipio siente que pertenece?")
TIyC8 = graficarTable(VI04,"Respuesta","%", " 8. ¿Usted a qué municipio siente que pertenece?")



# 9

A =  264
B = 97
C = 3
D = 8
to = A+B+C+D
total = c(A, B, C,D)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)

n =  c(A, B, C,D)

RESPUESTA = c("NUNCA" , "A-VECES", "CASI-SIEMPRE", "FRECUENTEMENTE")
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?


PIyC9 = graficarPlot(VI04,"Respuesta","%", " 9. ¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?")
TIyC9 = graficarTable(VI04,"Respuesta","%", " 9. ¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?")




# 10
A =  16
B = 13
C = 26
D = 63
E = 31
FF = 3

to = A+B+C+D+E+FF
total = c(A, B, C,D,E,FF)
A = as.integer((A/to)*100)
B = as.integer((B/to)*100)
C = as.integer((C/to)*100)
D = as.integer((D/to)*100)
E = as.integer((E/to)*100)
FF = as.integer((FF/to)*100)

n =  c(A, B, C,D,E,FF)

RESPUESTA = c("Asuntos administrativos" , "Pagos de servicios", "Trabajo", "Recreación", "Visita de la familia", "Otro")
VI04 <- data.frame(n ,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?
PIyC10 = graficarPlot(VI04,"Respuesta","%", " 10. ¿Cuáles son los motivos por los que viaja a la Isla?")
TIyC10 = graficarTable(VI04,"Respuesta","%", " 10. ¿Cuáles son los motivos por los que viaja a la Isla?")

