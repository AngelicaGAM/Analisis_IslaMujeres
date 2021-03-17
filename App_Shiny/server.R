# server library(shinyjs)
library(shinyjs)

library(shiny)
library(rsconnect)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(arules)
library(arulesViz)
library(ggplot2)
library(plyr)
library(janitor)
library(shinythemes)
require(visNetwork)


server <- function(input, output, session) {
  source("librerias.R")
  source("Funciones.R")
  source("G.R")
  source("Tablas.R")
  xdf  <- read.csv("./x.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")



  # ---------------------------------------------------------------------
  # HOME
  #cuestionarios
  observeEvent(input$popPyM, {
    showModal(modalDialog(
      title = HTML("Población y migración<br>",
        "I.	DATOS FAMILIARES <br><br>1. ¿Cuántas personas viven en esta casa?<br>Adultos (¿Cuantas personas viven en esta casa? Adultos) <br>Niños (¿Cuantas personas viven en esta casa? Niños)<br><br>  1. A qué niveles de escolaridad asisten los integrantes de esta familia, puede marcar más de una opción<br><br>Jardín de niños (A qué nivel de escolaridad asisten los integrantes de esta familia Jardin de niños)<br>Primaria (A qué nivel de escolaridad asisten los integrantes de esta familia Primaria)<br>Secundaria (A qué nivel de escolaridad asisten los integrantes de esta familia Secundaria)<br>Bachillerato (A qué nivel de escolaridad asisten los integrantes de esta familia Bachillerato)<br>Licenciatura (A qué nivel de escolaridad asisten los integrantes de esta familia Licenciatura)<br>Posgrado (A qué nivel de escolaridad asisten los integrantes de esta familia Posgrado)<br>2.¿Qué medio de transporte utiliza para ir a la escuela, al trabajo, etc? <br><br>Autobús (¿Qué medios de transporte utilizan para ir a la escuela, al trabajo, etc? Transportes Posibles)<br>Combi, colectivo<br>Taxi<br> Mototaxi<br>Motocicleta<br> Automóvil propio<br>Otro3. ¿Dónde adquiere sus víveres?<br><br>Mercado de la colonia (¿Dónde compra sus víveres? Posibles)<br>Tienda de abarrotes, carnicerías<br>súper mercado<br>Tienda de conveniencia (Oxxo,7eleven, …)<br>Plaza comercial<br> Otro (¿Dónde compra sus víveres? Otro)   4.¿A dónde acude en caso de urgencia médica? (¿A dónde acude en caso de urgencia médica? Posibles) <br><br>Me atiendo en casa<br>Médico particular en la comunidad<br>Médico particular Cancún<br>Hospital General Cancún<br>Cruza roja<br>Otro (¿A dónde acude en caso de urgencia médica? Otro)<br>5. ¿Con qué áreas de recreo cuenta su colonia? <br><br>Parque (¿Con qué áreas de recreo cuenta su colonia? Posibles)<br>Unidad deportiva<br>Jardines<br>Casa de la cultura<br>Biblioteca<br>Otro<br>II.	IDENTIDAD Y COMUNIDAD<br><br>6.¿Cuál es su lugar de origen?  (¿Cuál es su lugar de origen? Estados del país, ¿Cuál es su lugar de origen? Especifique)<br><br>1 Tabasco<br>2 Chiapas<br>3 Yucatán<br>4 Quintana Roo<br>5 Veracruz<br>6 CDMX<br>7 Otro______________<br><br>7.¿Cuánto tiempo lleva viviendo en este lugar?  (¿Cuánto tiempo lleva viviendo en este lugar? Opciones)<br>1 Recién llegué<br>2 Un año<br>3 Dos años<br>4 Más de dos años<br>5 Toda mi vida<br>8.Razones por las que llegó a vivir a la localidad)<br><br>1 parientes (¿Qué lo motivó a venir a vivir esta la localidad Tengo Parientes,)<br>2 amigos  (¿Qué lo motivó a venir a vivir esta la localidad Tengo amigos)<br>3 trabajo (¿Qué lo motivó a venir a vivir esta la localidad Por trabajo)<br>4 negocio  (¿Qué lo motivó a venir a vivir esta la localidad Por poner negocio,)<br>5 vivir mejor (¿Qué lo motivó a venir a vivir esta la localidad Por oportunidad de vivir mejor)<br>5 otro Especifique (¿Qué lo motivó a venir a vivir esta la localidad Otro)<br>9.En caso de requerir ayuda, apoyo legal, económico o familiar ante algún problema acudo a: <br><br>1 Grupo de vecinos (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Grupo de vecinos)<br>2 Familiares  (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Familiares)<br>3 A la autoridad (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a A la autoridad)<br>      4 La iglesia o templo (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a La iglesia o templo)<br>5 otro Especifique (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Otro)<br>10.¿Qué religión practica? (¿Qué religión practica? Opciones)<br><br>1 Ninguna<br>2 Católica<br>3 Evangélica<br>4 Testigos de Jehová<br>5 Adventista<br>6 Mormón<br>7 Otra (¿Qué religión practica? Otro)<br>11.¿Cuáles son sus principales costumbres o tradiciones? (¿Cuáles son sus principales costumbres o tradiciones? Opciones)<br><br>1 Fiestas en honor a la Virgen de Fátima<br>2 Fiestas patronales<br>3 Festival de la cultura del Caribe<br>4 Carnaval <br>5 Fin de año<br>6 Día de muertos<br>7 Otro (¿Cuáles son sus principales costumbres o tradiciones? Otro<br>12.¿Cuáles son las ventajas de vivir en esta comunidad?<br><br>1 Tengo casa (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo casa)<br>2 Trabajo (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo trabajo)<br>3 Familia (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo familia)<br>4 Tengo más tiempo (¿Cuáles son las ventajas de viv en esta comunidad? Tengo mas tiempo)<br>5 Es tranquilo (¿Cuáles son las ventajas de vivir ir  en esta comunidad? Es tranquilo)<br>6 Es seguro (¿Cuáles son las ventajas de vivir en esta comunidad? Es seguro)<br>7 Otro (¿Cuáles son las ventajas de vivir en esta comunidad? Otro)<br>13. Piensa irse a vivir a otra localidad (Piensa irse a vivir a otra localidad Opciones)<br><br>1 No <br>2 Quizás <br>3 Sí, en el mismo estado <br>4 Sí, me regreso a mi lugar de origen <br>5 Sí, al extranjero<br>14.¿Con qué municipio de identifica más? (¿Usted a qué municipio siente que pertenece? Opciones)<br><br>1 Benito Juárez<br>2 Isla Mujeres<br><br>3 Ninguno<br>15.¿Qué tan frecuente va a la isla, la cabecera municipal de Isla Mujeres? (¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres? Opciones)<br><br>1 Nunca <br>2 A veces [varias veces al año] <br>3 Frecuentemente [al menos una vez al mes]<br>4 casi Siempre [más de una vez al mes]<br>5 Siempre [una vez por semana]<br><br>16.¿Cuales son los motivos por los que viaja a isla? <br><br>1 Asuntos administrativos (¿Cuales son los motivos por los que viaja a isla? Asuntos administrativos)<br>2 Pagos de servicios (¿Cuales son los motivos por los que viaja a isla? Pagos de servicios)<br>3 Trabajo (¿Cuales son los motivos por los que viaja a isla? Trabajo)<br> 4 Recreación (¿Cuales son los motivos por los que viaja a isla? Recreación)<br> 5 Visita de la familia (¿Cuales son los motivos por los que viaja a isla? Visita de la familia)<br>6 Otros (¿Cuales son los motivos por los que viaja a isla? Otro)<br>IV.	VIVIENDA<br><br>La vivienda que habita tu familia es (La vivienda que habita tu familia es Opciones)<br>1 Propia.   <br>2 Prestada por (La vivienda que habita tu familia es Prestado por)<br>3 Rentada y se  paga mensual (La vivienda que habita tu familia es Renta mensual)<br> 4 Asentamiento irregular o informal <br>5 Otro (La vivienda que habita tu familia es Otro)<br>¿Ha recibido la visita del municipio para ofrecerle reubicarlo en otro lugar?  (¿Ha recibido la visita del municipio para ofrecerle reubicarlo en otro lugar? Si o no)<br>  •	Sólo si responde Asentamiento irregular o informal<br><br> ¿Le indicaron a qué sitio los reubicarían? (¿Le indicaron a qué sitio los reubicarían? Si o no)<br>•	Sólo si responde Asentamiento irregular o informal<br><br>En caso  de ser  propia, ¿De qué forma fue adquirida? (En caso de ser vivienda propia ¿De que forma fue adquirida? Opciones)<br>1 Herencia   <br>2 Pago de contado<br>3  Pagándola:    <br><br>Monto mensual (En caso de ser vivienda propia ¿De que forma fue adquirida? Monto mensual)<br>¿A quién? (En caso de ser vivienda propia ¿De que forma fue adquirida? A quien)<br>4  No es propia<br><br>¿Con qué servicios cuenta su vivienda? <br>1 televisión de paga (cable o satelital) <br>2 internet <br>3 servicio telefónico celular <br>4  energía eléctrica <br>5 agua <br><br>¿Con qué servicios cuenta su vivienda? (¿Con qué servicios cuenta su vivienda?)<br>1 televisión de paga (cable o satelital) <br>2 internet <br>3 servicio telefónico celular <br>4 energía eléctrica <br>5 agua <br><br>¿Cuántas inundaciones ha sufrido al vivir en esta localidad? (¿Cuántas inundaciones ha sufrido al vivir en esta localidad?)<br><br>¿En caso de haber sufrido una inundación qué hizo al respecto? (En caso de haber sufrido una inundación qué hizo al respecto Opciones) <br>•	Sólo si ha sufrido inundaciones<br>1 Dejé mi casa y fui al refugio<br>2 Dejé mi casa y fui a la casa de un familiar o amigo<br>3 Aquí nos quedamos<br>4 Vinieron las autoridades a desalojar pero decidimos quedarnos<br>5 Otro (En caso de haber sufrido una inundación qué hizo al respecto Otro)<br><br>A pesar de la inundación usted decidió quedarse a vivir aquí por (A pesar de la inundación usted decidió quedarse a vivir aquí por  Opciones)<br>•	Sólo si ha sufrido inundaciones<br>1 No tengo alternativa de vivienda    <br>2 Me agrada el lugar<br>3 Estoy esperando un crédito y mientras aquí me quedo<br>4 La ubicación del trabajo y escuela  nos queda más cerca<br>5 No dispongo de un trabajo que me permita acceder a un crédito para vivienda<br>6 Otro (A pesar de la inundación usted decidió quedarse a vivir aquí por  Opciones)<br><br>¿Sabe si su vivienda está en zona de riesgo? (¿Sabe si su vivienda está en zona de riesgo?)<br><br>¿Sabe si su vivienda está en zona de riesgo? ¿Cuales? (¿Sabe si su vivienda está en zona de riesgo? ¿Cuales?) <br><br>•	Sólo si responde sí a la pregunta: ¿Sabe si su vivienda está en zona de riesgo?<br>¿Conoce los impactos o afectaciones que puede sufrir? (¿Conoce los impactos o afectaciones que puede sufrir?) <br><br>¿Conoce los impactos o afectaciones que puede sufrir? (¿Conoce los impactos o afectaciones que puede sufrir? ¿Cuales?) <br><br>•	Sólo si responde Sí a la pregunta ¿Conoce los impactos o afectaciones que puede sufrir?<br>V.	APRECIACIONES DEL ENCUESTADOR<br><br>Favor de señalar los siguientes servicios observados alrededor de la vivienda <br><br>1 Áreas verdes (favor de señalar los siguientes servicios observados alrededor de la vivienda áreas verdes)<br>2 Calles pavimentadas (favor de señalar los siguientes servicios observados alrededor de la vivienda calles)<br>3 Banquetas (favor de señalar los siguientes servicios observados alrededor de la vivienda banquetas)<br>4 Luminarias públicas (favor de señalar los siguientes servicios observados alrededor de la vivienda luminarias)<br>5 Transporte público (favor de señalar los siguientes servicios observados alrededor de la vivienda transporte)<br>6 Patrullas vigilando (favor de señalar los siguientes servicios observados alrededor de la vivienda patrullas)<br>7 Lotes baldíos (favor de señalar los siguientes servicios observados alrededor de la vivienda lotes baldios)<br>¿Que percepción tuvo de la vivienda y la calle?<br><br>1 Seguridad  (¿Que percepción tuvo de la vivienda y la calle? Percepcion seguridad)<br>2 Comodidad (¿Que percepción tuvo de la vivienda y la calle? Percepcion comodidad)<br>3 Riesgo personal o natural (¿Que percepción tuvo de la vivienda y la calle? Percepcion riesgo personal o natural)  <br>"),
      easyClose = TRUE
    ))
  })

  observeEvent( input$poSyA, {
      showModal(modalDialog(
        title = HTML("Diagnóstico socio económico y ambiental<br>",
          "Social <br><br>1. Entre los vecinos, realizan alguna actividad en común:  fiestas, reuniones vecinales, levantar quejas etc <br>2. ¿Cómo es la relación con sus vecinos? <br>3. ¿Ha tenido problemas con sus vecinos: pleitos, demandas…? <br>4. ¿Con qué frecuencia se hacen favores entre vecinos? <br>5. ¿En algún problema que se le presente, sus vecinos le ayudan? <br>6. ¿Pertenecen a alguna organización? <br><br>Economico <br><br>1.  ¿Cuántas personas de esta familia trabajan? <br>2.  ¿En qué trabajan? <br>3. ¿Realizan alguna actividad productiva por su cuenta: manualidad, artesanía u oficio, cultivo de hortalizas?<br>4. ¿Intercambian productos con sus vecinos? <br>5. ¿Se ayudan entre vecinos para algún trabajo que beneficie la economía familiar dentro de sus casas?<br><br>Ambiental <br><br>1. ¿Qué uso le dan sus vecinos a la salina? <br>2.¿Qué beneficio recibe de vivir aquí?<br>3.¿Qué desventajas recibe de vivir aquí cerca de la salina? <br>4.¿En qué condiciones considera que se encuentra la salina? <br>5.¿Qué efectos genera la condición (sucia o contaminada) de la Salina? <br>6.¿Han llevado a cabo alguna actividad de limpieza, saneamiento o conservación de la Salina?<br>7.¿Están conectados al drenaje?<br>8.¿Por qué decidieron vivir aquí?<br>"),
        easyClose = TRUE
      ))
    })

    observeEvent(input$popC, {
      showModal(modalDialog(
        title = HTML("Percepcion de seguridad<br>",
          "  <br>"),
        easyClose = TRUE
      ))
    })

  # ---------------------------------------------------------------------
  # GRAFICAS 
  
  # Visualizacion principal 
  output$plot1=renderLeaflet({
     #datoG = Vgrafica()
     #datoG
     if(input$pregunta=="V1P1R1"){V1P1R1}
     else if(input$pregunta=="V1P4R1"){V1P4R1}
     else if(input$pregunta=="V1H1"){V1H1}
     else if(input$pregunta=="V1H1"){V1H1}
  })
  
  # Visualizacion secundaria 
  ##-- + Dados do candidato e eleição selecionada ----
  observe({

    x <- input$enfoque

    if (is.null(x))
      x <- character(0)
    # Poblacion y migracion
    # DG , DF, DE, ID, VI, AE
    if (x== "DG"){
            updateSelectInput(session, "pregunta",                
                choices = c("d" = "DF21" , "Adquisigfcion Vivienda" = "DF212"," HurgacaneDs " = "DF23", " IngundDciones " = "DF24") 
            )
    }else  if (x== "DF"){
            updateSelectInput(session, "pregunta",  
            choices = c("d" = "DF1" , "Adquisicion Vivienda" = "DF12"," HuracaneDs " = "DF3", " InundDciones " = "DF4")
            
    )} else  if (x== "DE"){
            updateSelectInput(session, "pregunta",  
            choices = c("d" = "DF1" , "Adquisicion Vivienda" = "DF12"," HuracaneDs " = "DF3", " InundDciones " = "DF4")
            
    )} else  if (x== "ID"){
            updateSelectInput(session, "pregunta",  
            choices = c("d" = "DF1" , "Adquisicion Vivienda" = "DF12"," HuracaneDs " = "DF3", " InundDciones " = "DF4")
            
    )} else  if (x== "VI"){
            updateSelectInput(session, "pregunta",  
            choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1", " Inundaciones " = "V1I1")
            
    )} else  if (x== "AE"){
            updateSelectInput(session, "pregunta",  
            choices = c("d" = "DF1" , "Adquisicion Vivienda" = "DF12"," HuracaneDs " = "DF3", " InundDciones " = "DF4")
            
    )} 
  })
  
  output$localiz <- renderText ({
    if(input$tipomapa=="ALL"){ "Cancún - Isla Mujeres " }
    else if(input$tipomapa=="PS"){ "Cancún, QRoo." }
    else if(input$tipomapa=="IS"){ "Salinas, Isla Mujeres." }
    else if(input$tipomapa=="EJ"){ " Zona Urbana Isla Mujeres "}
  })

    output$TipoestudioG <- renderText ({
    if(input$tipomapa=="PS"){ "Percepción sobre seguridad" }
    else if(input$tipomapa=="IS"){ "Estudio Socioeconómico y ambiental" }
    else if(input$tipomapa=="EJ"){ "Características sobre población y migración"}
  })

  # ---------------------------------------------------------------------
  # MAPAS
  
  #Pintado de mapas
  output$mymap <- renderLeaflet({
    if(input$showmapa=="ALL"){ GraphM(DataMap) }
    else if(input$showmapa=="PS"){ GraphM(CancunM) }
    else if(input$showmapa=="IS"){ GraphM(filtro) }
    else if(input$showmapa=="EJ"){ GraphM(EjidoM) }
  })
  
  # Texto tipo de estudio
  output$Tipoestudio <- renderText ({
    if(input$showmapa=="ALL"){ "Estudios socioeconomicos, Percepción de seguridad y Características sobre población y migración. " }
    else if(input$showmapa=="PS"){ "Percepción sobre seguridad" }
    else if(input$showmapa=="IS"){ "Estudio Socioeconómico y ambiental" }
    else if(input$showmapa=="EJ"){ "Características sobre población y migración"}
  })

  # Grafica de edades en Mapa 
  output$plot3=renderPlot({
    if(input$showmapa=="IS"){edadSalinas }
    else if(input$showmapa=="PS"){ edadZonaUrbana  }
  })
  

  output$zona <- renderText ({
    if(input$showmapa=="ALL"){ "Cancún - Isla Mujeres " }
    else if(input$showmapa=="PS"){ "Cancún - Isla Mujeres" }
    else if(input$showmapa=="IS"){ "Salinas, Isla Mujeres." }
    else if(input$showmapa=="EJ"){ " Zona Urbana Isla Mujeres "}
  })


  #------------------------------------------
  # graficas de mosaico
  output$mosaico1 <- renderPlot({
    if(input$var1 == "Origen" & input$var2 == "Puesto" & input$var3 == "Edad"){
      mosaics2(tbl15, c(0,0,0,0), c(0,0,0,2))
    }else{
      if(input$var1 == "Origen" & input$var2 == "Puesto" & input$var3 == "Sexo"){
        mosaics2(tbl16, c(0,0,0,0), c(0,0,0,2))
      }else{
        if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Edad"){
          mosaics2(tbl17, c(0,0,0,0), c(0,0,0,2))
        }else{
          if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
            mosaics2(tbl18, c(0,0,0,0), c(0,0,0,2))
          }else{
            if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
              mosaics2(tbl19, c(0,0,0,0), c(0,0,0,2))
            }else{
              if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
                mosaics2(tbl20, c(0,0,0,0), c(0,0,0,2))
              }else{
                if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
                  mosaics2(tbl21, c(0,0,0,0), c(0,0,0,2))
                }else{
                  if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
                    mosaics2(tbl22, c(0,0,0,0), c(0,0,0,2))
                  }else{
                    if(input$var1 == "Escolaridad" & input$var2 == "Puesto" & input$var3 == "Edad"){
                      mosaics2(tbl23, c(0,0,0,0), c(0,0,0,2))
                    }else{
                      if(input$var1 == "Escolaridad" & input$var2 == "Puesto" & input$var3 == "Sexo"){
                        mosaics2(tbl24, c(0,0,0,0), c(0,0,0,2))
                      }else{
                        if(input$var1 == "Escolaridad" & input$var2 == "Trabajo" & input$var3 == "Edad"){
                          mosaics2(tbl25, c(0,0,0,0), c(0,0,0,2))
                        }else{
                          if(input$var1 == "Escolaridad" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
                            mosaics2(tbl26, c(0,0,0,0), c(0,0,0,2))
                          }else{
                            if(input$var1 == "Escolaridad" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
                              mosaics2(tbl27, c(0,0,0,0), c(0,0,0,2))
                            }else{
                              if(input$var1 == "Escolaridad" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
                                mosaics2(tbl28, c(0,0,0,0), c(0,0,0,2))
                              }else{
                                if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
                                  mosaics2(tbl29, c(0,0,0,0), c(0,0,0,2))
                                }else{
                                  if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
                                    mosaics2(tbl30, c(0,0,0,0), c(0,0,0,2))
                                  }else{
                                    if(input$var1 == "Puesto" & input$var2 == "Trabajo" & input$var3 == "Edad"){
                                      mosaics2(tbl31, c(0,0,0,0), c(0,0,0,2))
                                    }else{
                                      if(input$var1 == "Puesto" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
                                        mosaics2(tbl32, c(0,0,0,0), c(0,0,0,2))
                                      }else{
                                        if(input$var1 == "Puesto" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
                                          mosaics2(tbl33, c(0,0,0,0), c(0,0,0,2))
                                        }else{
                                          if(input$var1 == "Puesto" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
                                            mosaics2(tbl34, c(0,0,0,0), c(0,0,0,2))
                                          }else{
                                            if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
                                              mosaics2(tbl35, c(0,0,0,0), c(0,0,0,2))
                                            }else{
                                              if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
                                                mosaics2(tbl36, c(0,0,0,0), c(0,0,0,2))
                                              }else{
                                                if(input$var1 == "Trabajo" & input$var2 == "Puesto" & input$var3 == "Edad"){
                                                  mosaics2(tbl37, c(0,0,0,0), c(0,0,0,2))
                                                }else{
                                                  if(input$var1 == "Trabajo" & input$var2 == "Puesto" & input$var3 == "Sexo"){
                                                    mosaics2(tbl38, c(0,0,0,0), c(0,0,0,2))
                                                  }else{
                                                    if(input$var1 == "Trabajo" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
                                                      mosaics2(tbl39, c(0,0,0,0), c(0,0,0,2))
                                                    }else{
                                                      if(input$var1 == "Trabajo" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
                                                        mosaics2(tbl40, c(0,0,0,0), c(0,0,0,2))
                                                      }else{
                                                        if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
                                                          mosaics2(tbl41, c(0,0,0,0), c(0,0,0,2))
                                                        }else{
                                                          if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
                                                            mosaics2(tbl42, c(0,0,0,0), c(0,0,0,2))
                                                          }else{
                                                            if(input$var1 == "Origen" & input$var2 == "Puesto"){
                                                              mosaics(tbl1, c(90,0,0,0), c(0,0,0,6.5))
                                                            }else{
                                                              if(input$var1 == "Origen" & input$var2 == "Trabajo"){
                                                                mosaics(tbl2, c(90,0,0,0), c(0,0,0,6.5))
                                                              }else{
                                                                if(input$var1 == "Origen" & input$var2 == "Escolaridad"){
                                                                  mosaics(tbl3, c(90,0,0,0), c(0,0,0,6.5))
                                                                }else{
                                                                  if(input$var1 == "Origen" & input$var2 == "Ingreso_sem"){
                                                                    mosaics(tbl4, c(0,0,0,0), c(0,0,0,6.5))
                                                                  }else{
                                                                    if(input$var1 == "Escolaridad" & input$var2 == "Puesto"){
                                                                      mosaics(tbl5, c(90,0,0,0), c(0,0,0,9.5))
                                                                    }else{
                                                                      if(input$var1 == "Escolaridad" & input$var2 == "Trabajo"){
                                                                        mosaics(tbl6, c(90,0,0,0), c(0,0,0,9.5))
                                                                      }else{
                                                                        if(input$var1 == "Trabajo" & input$var2 == "Trabajo" & input$var3 == "Edad"){
                                                                          mosaics(tbl45, c(0,0,0,0), c(0,0,0,5))
                                                                        }else{
                                                                          if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem"){
                                                                            mosaics(tbl8, c(0,0,0,0), c(0,0,0,9.5))
                                                                          }else{
                                                                            if(input$var1 == "Puesto" & input$var2 == "Trabajo"){
                                                                              mosaics(tbl9, c(90,0,0,0), c(0,0,0,11.5))
                                                                            }else{
                                                                              if(input$var1 == "Puesto" & input$var2 == "Escolaridad"){
                                                                                mosaics(tbl10, c(90,0,0,0), c(0,0,0,11.5))
                                                                              }else{
                                                                                if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem"){
                                                                                  mosaics(tbl11, c(0,0,0,0), c(0,0,0,11.5))
                                                                                }else{
                                                                                  if(input$var1 == "Trabajo" & input$var2 == "Puesto"){
                                                                                    mosaics(tbl12, c(90,0,0,0), c(0,0,0,5))
                                                                                  }else{
                                                                                    if(input$var1 == "Trabajo" & input$var2 == "Escolaridad"){
                                                                                      mosaics(tbl13, c(90,0,0,0), c(0,0,0,5))
                                                                                    }else{
                                                                                      if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem"){
                                                                                        mosaics(tbl14, c(0,0,0,0), c(0,0,0,5))
                                                                                      }else{
                                                                                        if(input$var1 == "Puesto" & input$var2 == "Puesto" & input$var3 == "Edad"){
                                                                                          mosaics(tbl43, c(0,0,0,0), c(0,0,0,11.5))
                                                                                        }else{
                                                                                          if(input$var1 == "Puesto" & input$var2 == "Puesto" & input$var3 == "Sexo"){
                                                                                            mosaics(tbl44, c(0,0,0,0), c(0,0,0,11.5))
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })

  

  
  
  output$data <- DT::renderDataTable({
    DT::datatable(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[, input$show_vars, drop = FALSE])
  })
  
  
  output$sum <- renderTable({
    summary(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[, input$show_vars, drop = FALSE])
  })
  
  
  
  
  rules <- reactive({
    
    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
    
    
    df_ide$Info.Edad <- ordered(cut(df_ide$Info.Edad,
                                    c(0, 18, 30, 60, 100), 
                                    labels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                                    include.lowest = TRUE))
    
    
    df_ide$V01_Casa_Adultos    <- factor(df_ide$V01_Casa_Adultos,
                                         levels = c("1", "2", "3", "4", "5", "6", "7", "13"),
                                         ordered = TRUE)
    
    df_ide$V02_Casa_Ninos    <- factor(df_ide$V02_Casa_Ninos,
                                       levels = c("0", "1", "2", "3", "4", "5", "6", "7"),
                                       ordered = TRUE)
    
    df_ide$DF_kinder    <- factor(df_ide$DF_kinder,
                                  levels = c(0,1,2),
                                  ordered = TRUE)
    
    
    df_ide$DF_primaria    <- factor(df_ide$DF_primaria,
                                    levels = c(0,1,2,3,5),
                                    ordered = TRUE)
    
    df_ide$DF_secundaria    <- factor(df_ide$DF_secundaria,
                                      levels = c(0,1,2,3,4),
                                      ordered = TRUE)
    
    
    df_ide$DF_bachillerato   <- factor(df_ide$DF_bachillerato,
                                       levels = c(0,1,2,3,4),
                                       ordered = TRUE)
    
    
    df_ide$DF_licenciatura    <- factor(df_ide$DF_licenciatura,
                                        levels = c(0,1,2,3),
                                        ordered = TRUE)
    
    df_ide$DF_tr_autobus   <- factor(df_ide$DF_tr_autobus,
                                     levels = c(0,1),
                                     ordered = TRUE)
    
    
    df_ide$DF_tr_colectivo    <- factor(df_ide$DF_tr_colectivo,
                                        levels = c(0,1),
                                        ordered = TRUE)
    
    df_ide$DF_tr_taxi   <- factor(df_ide$DF_tr_taxi,
                                  levels = c(0,1),
                                  ordered = TRUE)
    
    df_ide$DF_tr_mototaxi    <- factor(df_ide$DF_tr_mototaxi,
                                       levels = c(0,1),
                                       ordered = TRUE)
    
    df_ide$DF_tr_moto    <- factor(df_ide$DF_tr_moto,
                                   levels = c(0,1),
                                   ordered = TRUE)
    
    df_ide$DF_tr_auto    <- factor(df_ide$DF_tr_auto,
                                   levels = c(0,1),
                                   ordered = TRUE)
    
    df_ide$DF_v_col    <- factor(df_ide$DF_v_col,
                                 levels = c(0,1),
                                 ordered = TRUE)
    
    df_ide$DF_v_abarrote    <- factor(df_ide$DF_v_abarrote,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    df_ide$DF_v_super   <- factor(df_ide$DF_v_super,
                                  levels = c(0,1),
                                  ordered = TRUE)
    
    df_ide$DF_v_conv    <- factor(df_ide$DF_v_conv,
                                  levels = c(0,1),
                                  ordered = TRUE)
    
    df_ide$DF_v_plaza    <- factor(df_ide$DF_v_plaza,
                                   levels = c(0,1),
                                   ordered = TRUE)
    
    
    df_ide$DF_urg_casa    <- factor(df_ide$DF_urg_casa,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    df_ide$DF_urg_partcom    <- factor(df_ide$DF_urg_partcom,
                                       levels = c(0,1),
                                       ordered = TRUE)
    
    df_ide$DF_urg_partcan    <- factor(df_ide$DF_urg_partcan,
                                       levels = c(0,1),
                                       ordered = TRUE)
    
    df_ide$DF_urg_hosp    <- factor(df_ide$DF_urg_hosp,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    df_ide$DF_urg_cruz   <- factor(df_ide$DF_urg_cruz,
                                   levels = c(0,1),
                                   ordered = TRUE)
    
    df_ide$DF_urg_farmacia    <- factor(df_ide$DF_urg_farmacia,
                                        levels = c(0,1),
                                        ordered = TRUE)
    
    df_ide$DF_urg_otro    <- factor(df_ide$DF_urg_otro,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    
    
    df_ide$DF_a_parque    <- factor(df_ide$DF_a_parque,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    df_ide$DF_a_unidad    <- factor(df_ide$DF_a_unidad,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    
    df_ide$DF_a_jardines    <- factor(df_ide$DF_a_jardines,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    df_ide$DF_a_casa   <- factor(df_ide$DF_a_casa,
                                 levels = c(0,1),
                                 ordered = TRUE)
    
    df_ide$DF_a_biblioteca    <- factor(df_ide$DF_a_biblioteca,
                                        levels = c(0,1),
                                        ordered = TRUE)
    
    df_ide$DF_a_otro    <- factor(df_ide$DF_a_otro,
                                  levels = c(0,1),
                                  ordered = TRUE)
    
    df_ide$DE01_hogar_trabajan    <- factor(df_ide$DE01_hogar_trabajan,
                                            levels = c(0,1,2,3,4,5),
                                            ordered = TRUE)
    
    #DE02_trabajo
    #DE03_puesto
    
    df_ide$DE05_ingreso_sem   <- factor(df_ide$DE05_ingreso_sem,
                                        levels = c("A","B","C","D"),
                                        ordered = TRUE)
    
    #DE06_esc_jefe
    
    df_ide$DE07_sIngreso   <- factor(df_ide$DE07_sIngreso,
                                     levels = c(0,1,2,3,4,5),
                                     ordered = TRUE)
    
    
    df_ide$DE08_tmp   <- factor(df_ide$DE08_tmp,
                                levels = c("A","B","C","D","E"),
                                ordered = TRUE)
    
    
    ##DE09_col
    #DE10_mun
    #IyC02_Estado_Origen
    #IyC03_Tiempo
    #IyC06_Religion
    
    ##IyC07_Costumbres
    
    
    df_ide$IyC08_Ventajas_Casa    <- factor(df_ide$IyC08_Ventajas_Casa,
                                            levels = c(0,1),
                                            ordered = TRUE)
    
    df_ide$IyC08_Ventajas_Trabajo    <- factor(df_ide$IyC08_Ventajas_Trabajo,
                                               levels = c(0,1),
                                               ordered = TRUE)
    
    
    df_ide$IyC08_Ventajas_Familia    <- factor(df_ide$IyC08_Ventajas_Familia,
                                               levels = c(0,1),
                                               ordered = TRUE)
    
    df_ide$IyC08_Ventajas_Tiempo   <- factor(df_ide$IyC08_Ventajas_Tiempo,
                                             levels = c(0,1),
                                             ordered = TRUE)
    
    df_ide$IyC08_Ventajas_Tranquilo    <- factor(df_ide$IyC08_Ventajas_Tranquilo,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
    
    df_ide$IyC08_Ventajas_Seguro    <- factor(df_ide$IyC08_Ventajas_Seguro,
                                              levels = c(0,1),
                                              ordered = TRUE)
    
    
    df_ide$IyC08_Ventajas_Otro    <- factor(df_ide$IyC08_Ventajas_Otro,
                                            levels = c(0,1),
                                            ordered = TRUE)
    
    #IyC09_Emigrar
    #IyC10_Pertenencia
    #IyC11_Frecuencia_Cabecera_Isla
    
    df_ide$IyC12_Motivo_Viaja_Asuntos_Admin    <- factor(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin,
                                                         levels = c(0,1),
                                                         ordered = TRUE)
    
    
    df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS    <- factor(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS,
                                                                   levels = c(0,1),
                                                                   ordered = TRUE)
    
    df_ide$IyC12_Motivo_Viaja_Trabajo   <- factor(df_ide$IyC12_Motivo_Viaja_Trabajo,
                                                  levels = c(0,1),
                                                  ordered = TRUE)
    
    df_ide$IyC12_Motivio_Viaja_Recreacion    <- factor(df_ide$IyC12_Motivio_Viaja_Recreacion,
                                                       levels = c(0,1),
                                                       ordered = TRUE)
    
    df_ide$IyC12_Motivo_Viaja_Familia    <- factor(df_ide$IyC12_Motivo_Viaja_Familia,
                                                   levels = c(0,1),
                                                   ordered = TRUE)
    
    df_ide$IyC12_Motivo_Viaja_Otro    <- factor(df_ide$IyC12_Motivo_Viaja_Otro,
                                                levels = c(0,1),
                                                ordered = TRUE)
    
    
    #VI01_La_vivienda_es
    
    
    df_ide$VI04_Contado   <- factor(df_ide$VI04_Contado,
                                    levels = c(0,1),
                                    ordered = TRUE)
    
    df_ide$VI04_Herencia    <- factor(df_ide$VI04_Herencia,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    df_ide$VI04_Mensual    <- factor(df_ide$VI04_Mensual,
                                     levels = c(0,1),
                                     ordered = TRUE)
    
    ##CPropiaMensual
    ##CPropiaAdquirida
    
    ##VI07_FALTA_DE_SERVICIOS
    
    df_ide$VI08NINUNDACIONES    <- factor(df_ide$VI08NINUNDACIONES,
                                          levels = c(0,1),
                                          ordered = TRUE)
    
    df_ide$VI09PeHuracan    <- factor(df_ide$VI09PeHuracan,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    
    
    df_ide$VI14SabeZo0Riesgo    <- factor(df_ide$VI14SabeZo0Riesgo,
                                          levels = c(0,1),
                                          ordered = TRUE)
    
    
    df_ide$VI15SabeAfectaciones    <- factor(df_ide$VI15SabeAfectaciones,
                                             levels = c(0,1),
                                             ordered = TRUE)
    
    df_ide$AE1_AreasVerdes   <- factor(df_ide$AE1_AreasVerdes,
                                       levels = c(0,1),
                                       ordered = TRUE)
    
    df_ide$AE3_Banquetas    <- factor(df_ide$AE3_Banquetas,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    df_ide$AE4_Lumi0rias    <- factor(df_ide$AE4_Lumi0rias,
                                      levels = c(0,1),
                                      ordered = TRUE)
    
    df_ide$AE5_Transporte    <- factor(df_ide$AE5_Transporte,
                                       levels = c(0,1),
                                       ordered = TRUE)
    
    df_ide$AE6_Patrullas   <- factor(df_ide$AE6_Patrullas,
                                     levels = c(0,1),
                                     ordered = TRUE)
    
    df_ide$AE7_Lotes    <- factor(df_ide$AE7_Lotes,
                                  levels = c(0,1),
                                  ordered = TRUE)
    
    df_ide$AE9_PeSeguridad<- factor(df_ide$AE9_PeSeguridad,
                                    levels = c(0,1,2,3,4,5),
                                    ordered = TRUE)
    
    df_ide$AE10_PeComodidad<- factor(df_ide$AE10_PeComodidad,
                                     levels = c(0,1,2,3,4,5),
                                     ordered = TRUE)
    
    df_ide$AE11_PeRiesgo<- factor(df_ide$AE11_PeRiesgo,
                                  levels = c(0,1,2,3,4,5),
                                  ordered = TRUE)
    
    
    for(i in 1:170){
      df_ide[,i]<-factor(df_ide[,i])
    }
    
    
    if(length(input$Info.Sexo)!=0 || length(input$Info.Edad)!=0 || #length(input$DF_tr_autobus)!=0 || length(input$DF_tr_colectivo)!=0 || 
       length(input$Direccion.Calle)!=0 || length(input$Direccion.Numero.Exterior)!=0 || length(input$Direccion.Numero.Interior)!=0 || length(input$Direccion.Manza0)!=0 ||
       length(input$Direccion.Super.Manza0)!=0 || length(input$Direccion.Colonia)!=0 || length(input$V01_Casa_Adultos)!=0 || length(input$V02_Casa_Ninos)!=0 ||
       length(input$Direccion.Codigo.Postal)!=0 || length(input$DF_kinder)!=0 || length(input$DF_primaria)!=0 || length(input$DF_secundaria)!=0 ||
       length(input$DF_bachillerato)!=0 || length(input$DF_licenciatura)!=0 || length(input$DF_tr_autobus)!=0 || length(input$DF_tr_colectivo)!=0 ||
       length(input$DF_tr_taxi)!=0 || length(input$DF_tr_mototaxi)!=0 || length(input$DF_tr_moto)!=0 || length(input$DF_tr_moto)!=0 || 
       length(input$DF_tr_auto)!=0 || length(input$DF_v_col)!=0 || length(input$DF_v_abarrote)!=0 || length(input$DF_v_super)!=0 ||
       length(input$DF_v_conv)!=0 || length(input$DF_v_plaza)!=0 || length(input$DF_urg_casa)!=0 || length(input$DF_urg_partcom)!=0 ||
       length(input$DF_urg_partcan)!=0 || length(input$DF_urg_hosp)!=0 || length(input$DF_urg_cruz)!=0 || length(input$DF_urg_farmacia)!=0 ||
       length(input$DF_urg_otro)!=0 || length(input$DF_a_parque)!=0 || length(input$DF_a_unidad)!=0 || length(input$DF_a_jardines)!=0 ||
       length(input$DF_a_casa)!=0 || length(input$DF_a_biblioteca)!=0 || length(input$DF_a_otro)!=0 || length(input$DE01_hogar_trabajan)!=0 ||
       length(input$DE02_trabajo)!=0 || length(input$DE03_puesto)!=0 || length(input$DE05_ingreso_sem)!=0 || length(input$DE06_esc_jefe)!=0 ||
       length(input$DE07_sIngreso)!=0 || length(input$DE08_tmp)!=0 || length(input$DE09_col)!=0 || length(input$DE10_mun)!=0 ||
       length(input$IyC02_Estado_Origen)!=0 || length(input$IyC03_Tiempo)!=0 || length(input$IyC04_Motivo_Localidad_Parientes)!=0 || length(input$IyC04_Motivo_Localidad_Amigos)!=0 ||
       length(input$IyC04_Motivo_Localidad_Trabajo)!=0 || length(input$IyC04_Motivo_Localidad_Negocio)!=0 || length(input$IyC04_Motivo_Localidad_Oportunidad)!=0 ||
       length(input$IyC04_Motivo_Localidad_Otro)!=0 || length(input$IyC05_Acudo_Vecinos)!=0 || length(input$IyC05_Acudo_Familia)!=0  || length(input$IyC05_Acudo_Autoridad)!=0 ||
       length(input$IyC05_Acudo_Iglesia)!=0 || length(input$IyC05_Acudo_Otro)!=0 || length(input$IyC06_Religion)!=0  || #length(input$IyC07_Costumbres)!=0 ||
       length(input$IyC08_Ventajas_Casa)!=0 || length(input$IyC08_Ventajas_Trabajo)!=0  || length(input$IyC08_Ventajas_Familia)!=0  || length(input$IyC08_Ventajas_Tiempo)!=0 ||
       length(input$IyC08_Ventajas_Tranquilo)!=0 || length(input$IyC08_Ventajas_Seguro)!=0 || length(input$IyC08_Ventajas_Otro)!=0  || length(input$IyC09_Emigrar)!=0 ||
       length(input$IyC10_Pertenencia)!=0 || length(input$IyC11_Frecuencia_Cabecera_Isla)!=0  || length(input$IyC12_Motivo_Viaja_Asuntos_Admin)!=0  || length(input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS)!=0 ||
       length(input$IyC12_Motivo_Viaja_Trabajo)!=0 || length(input$IyC12_Motivio_Viaja_Recreacion)!=0  || length(input$IyC12_Motivo_Viaja_Familia)!=0  || length(input$IyC12_Motivo_Viaja_Otro)!=0 ||
       length(input$VI01_La_vivienda_es)!=0 || length(input$VI04_Contado)!=0 || length(input$VI04_Herencia)!=0  || length(input$VI04_Mensual)!=0 ||
       length(input$VI05_CPropiaMensual)!=0 || length(input$VI06_CPropiaAdquirida)!=0  || length(input$VI07_FALTA_DE_SERVICIOS)!=0  || length(input$VI08NINUNDACIONES)!=0 ||
       length(input$VI09PeHuracan)!=0 || length(input$VI14SabeZo0Riesgo)!=0 || length(input$VI15SabeAfectaciones)!=0  || length(input$AE1_AreasVerdes)!=0 ||
       length(input$AE3_Banquetas)!=0 || length(input$AE4_Lumi0rias)!=0  || length(input$AE5_Transporte)!=0  || length(input$AE6_Patrullas)!=0 ||
       length(input$AE7_Lotes)!=0 || length(input$AE9_PeSeguridad)!=0 || length(input$AE10_PeComodidad)!=0  || length(input$AE11_PeRiesgo)!=0){
      df_ide <-  dft()
      
      
    }else{
      df_ide <- df_ide[, input$show_vars, drop = FALSE]
    }
    
    
    
    
    
    rules<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),
                                         maxtime=as.numeric(input$time), target = "rules"))
    
    rules <- sort(rules, by = "confidence")
    #quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
    
    ## non-redundant rules
    rulesPruned_ide<- (rules[!is.redundant(rules)])
    
    
    subset(rulesPruned_ide, subset = confidence == 1) %>% inspect
    
    # Se eliminan esas reglas
    reglas_seleccionadas_ide <- subset(rulesPruned_ide, subset = confidence < 1)
    
    
    reglas_seleccionadas_ide <- subset(reglas_seleccionadas_ide, subset = lift > 1)
    
    reglas_altura_ide <- subset(reglas_seleccionadas_ide, subset = lhs %pin% "hei")
    
    mInteres_ide <- interestMeasure(reglas_seleccionadas_ide,
                                    measure = c("gini", "chiSquared"),
                                    transactions=df_ide)
    quality(reglas_seleccionadas_ide) <- cbind(quality(reglas_seleccionadas_ide), mInteres_ide)
    
    
    
    return(reglas_seleccionadas_ide)
  })
  
  output$rules <- renderPrint({
    # This is a little bit of a hack to prevent the output of calculating the
    # rules from being displayed in the "rules" verbatimTextOutput output.
    capture.output(rules())
    
    if (is.null(rules()))
      return(invisible())
    inspect(rules())
    #rules()
  })
  
  
  output$plot <- renderVisNetwork({
    
    income_rules <-rules()
    #"htmlwidget"
    p<-plot(income_rules,method = "graph", engine="htmlwidget", shading="confidence",height = "100", width = "100")
    
    print (p)
  })
  
  
  output$txt1 <- renderText({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('Info.Edad'==input$show_vars)){
      str1 <- ("o La variable Edad esta dividida en intervalos para incorporarla en la regla de asociaciones: 18 - Joven, 19 a 30 - Adulto Joven, 31 a 60 - Adulto, 61 mas - Tercera edad")
    }else{
      str1 <-""
    }
  })
  
  output$txt2 <- renderText({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('DE05_ingreso_sem'==input$show_vars)){
      str2 <- ("o El Ingreso semanal esta categorizado de la siguiente manera: A - $0 a $1200, B - $1201 a $2500, C - $2501 a $5000, D - $Mas de $5000")
    }else{
      str2 <-""
    }
  })
  
  output$txt3 <- renderText({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('DE08_tmp'==input$show_vars)){
      str2 <- ("o El tiempo que tarda el jefe de familia en llegar a su trabajo esta categorizado de la siguiente manera: A - Trabaja en casa, B - Menos de 20 min, C - Mas de 20 min y menos de 1 hora, D - Entre 1 y 2 horas, E - Mas de 2 horas")
    }else{
      str2 <-""
    }
  })
  
  output$txt4 <- renderText({
    paste("o Descripcion del grafo: Los nodos del grafo indican las reglas de asociacion que se obtienen con las columnas seleccionadas, estas se conectan por medio de flechas que van del antecedente al consecuente. En el medio de estas flechas de conexiones, podemos encontra un circulo cuyo tam. representa el valor de soporte o frecuencia de la regla mientras que el color representa la confianza. Podemos ver que la confianza se representa desde rosa palido hasta el rojo, siendo este ultimo el valor mas alto.")
  })
  
  
  dat<-reactive({
    #unique(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[, input$show_vars, drop = FALSE])
    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
    
    df_ide$Info.Sexo    <- factor(df_ide$Info.Sexo,
                                  levels = c("H", "M"),
                                  ordered = TRUE)
    
    df_ide$Info.Edad <- ordered(cut(df_ide$Info.Edad,
                                    c(0, 18, 30, 60, 100), 
                                    labels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                                    include.lowest = TRUE))
    
    for(i in 1:170){
      df_ide[,i]<-factor(df_ide[,i])
    }
    
    
    df_ide <- df_ide[, input$show_vars, drop = FALSE]
    
    
    
    lapply(df_ide, unique)
  })
  
  variants2 <- reactive({
    selectInput('filtro', 
                'Elige Atributo(s) a Filtrar', 
                dat(), 
                selected="", 
                multiple = TRUE)
  })  
  
  output$variants <- renderUI({
    variants2()
  })
  
  
  dft <- reactive( {
    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
    df_ide <- df_ide[, input$show_vars, drop = FALSE]
    
    
    
    
    
    
    if("Info.Sexo" %in% input$show_vars){
      if(length(input$Info.Sexo)!=0){
        df_ide <- dplyr::filter(df_ide, Info.Sexo %in% input$Info.Sexo)
      }
    }
    if("Info.Edad" %in% input$show_vars){
      if(length(input$Info.Edad)!=0){
        df_ide$Info.Edad <- ordered(cut(df_ide$Info.Edad,
                                        c(0, 18, 30, 60, 100), 
                                        labels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                                        include.lowest = TRUE))
        df_ide <- dplyr::filter(df_ide, Info.Edad %in% input$Info.Edad)
      }
    }
    if("Direccion.Calle" %in% input$show_vars){
      if(length(input$Direccion.Calle)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Calle %in% input$Direccion.Calle)
      }
    }
    if("Direccion.Numero.Exterior" %in% input$show_vars){
      if(length(input$Direccion.Numero.Exterior)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Numero.Exterior %in% input$Direccion.Numero.Exterior)
      }
    }
    
    if("Direccion.Numero.Interior" %in% input$show_vars){
      if(length(input$Direccion.Numero.Interior)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Numero.Interior %in% input$Direccion.Numero.Interior)
      }
    }
    if("Direccion.Manza0" %in% input$show_vars){
      if(length(input$Direccion.Manza0)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Manza0 %in% input$Direccion.Manza0)
      }
    }
    
    
    if("Direccion.Super.Manza0" %in% input$show_vars){
      if(length(input$Direccion.Super.Manza0)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Super.Manza0 %in% input$Direccion.Super.Manza0)
      }
    }
    if("Direccion.Colonia" %in% input$show_vars){
      if(length(input$Direccion.Colonia)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Colonia %in% input$Direccion.Colonia)
      }
    }
    
    if("Direccion.Codigo.Postal" %in% input$show_vars){
      if(length(input$Direccion.Codigo.Postal)!=0){
        df_ide <- dplyr::filter(df_ide, Direccion.Codigo.Postal %in% input$Direccion.Codigo.Postal)
      }
    }
    if("V01_Casa_Adultos" %in% input$show_vars){
      if(length(input$V01_Casa_Adultos)!=0){
        df_ide$V01_Casa_Adultos    <- factor(df_ide$V01_Casa_Adultos,
                                             levels = c("1", "2", "3", "4", "5", "6", "7", "13"),
                                             ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, V01_Casa_Adultos %in% input$V01_Casa_Adultos)
      }
    }
    
    
    if("V02_Casa_Ninos" %in% input$show_vars){
      if(length(input$V02_Casa_Ninos)!=0){
        df_ide$V02_Casa_Ninos    <- factor(df_ide$V02_Casa_Ninos,
                                           levels = c("0", "1", "2", "3", "4", "5", "6", "7"),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, V02_Casa_Ninos %in% input$V02_Casa_Ninos)
      }
    }
    if("DF_kinder" %in% input$show_vars){
      if(length(input$DF_kinder)!=0){
        df_ide$DF_kinder    <- factor(df_ide$DF_kinder,
                                      levels = c(0,1,2),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_kinder %in% input$DF_kinder)
      }
    }
    
    if("DF_primaria" %in% input$show_vars){
      if(length(input$DF_primaria)!=0){
        df_ide$DF_primaria    <- factor(df_ide$DF_primaria,
                                        levels = c(0,1,2,3,5),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_primaria %in% input$DF_primaria)
      }
    }
    if("DF_secundaria" %in% input$show_vars){
      if(length(input$DF_secundaria)!=0){
        df_ide$DF_secundaria    <- factor(df_ide$DF_secundaria,
                                          levels = c(0,1,2,3,4),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_secundaria %in% input$DF_secundaria)
      }
    }
    if("DF_bachillerato" %in% input$show_vars){
      if(length(input$DF_bachillerato)!=0){
        df_ide$DF_bachillerato   <- factor(df_ide$DF_bachillerato,
                                           levels = c(0,1,2,3,4),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_bachillerato %in% input$DF_bachillerato)
      }
    }
    if("DF_licenciatura" %in% input$show_vars){
      if(length(input$DF_licenciatura)!=0){
        df_ide$DF_licenciatura    <- factor(df_ide$DF_licenciatura,
                                            levels = c(0,1,2,3),
                                            ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_licenciatura %in% input$DF_licenciatura)
      }
    }
    
    
    if("DF_tr_autobus" %in% input$show_vars){
      if(length(input$DF_tr_autobus)!=0){
        df_ide$DF_tr_autobus   <- factor(df_ide$DF_tr_autobus,
                                         levels = c(0,1),
                                         ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_autobus %in% input$DF_tr_autobus)
      }
    }
    if("DF_tr_colectivo" %in% input$show_vars){
      if(length(input$DF_tr_colectivo)!=0){
        df_ide$DF_tr_colectivo    <- factor(df_ide$DF_tr_colectivo,
                                            levels = c(0,1),
                                            ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_colectivo %in% input$DF_tr_colectivo)
      }
    }
    
    if("DF_tr_taxi" %in% input$show_vars){
      if(length(input$DF_tr_taxi)!=0){
        df_ide$DF_tr_taxi   <- factor(df_ide$DF_tr_taxi,
                                      levels = c(0,1),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_taxi %in% input$DF_tr_taxi)
      }
    }
    if("DF_tr_mototaxi" %in% input$show_vars){
      if(length(input$DF_tr_mototaxi)!=0){
        df_ide$DF_tr_mototaxi   <- factor(df_ide$DF_tr_mototaxi,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_mototaxi %in% input$DF_tr_mototaxi)
      }
    }
    
    
    if("DF_tr_moto" %in% input$show_vars){
      if(length(input$DF_tr_moto)!=0){
        df_ide$DF_tr_moto    <- factor(df_ide$DF_tr_moto,
                                       levels = c(0,1),
                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_moto %in% input$DF_tr_moto)
      }
    }
    if("DF_tr_auto" %in% input$show_vars){
      if(length(input$DF_tr_auto)!=0){
        df_ide$DF_tr_auto    <- factor(df_ide$DF_tr_auto,
                                       levels = c(0,1),
                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_tr_auto %in% input$DF_tr_auto)
      }
    }
    if("DF_v_col" %in% input$show_vars){
      if(length(input$DF_v_col)!=0){
        
        df_ide$DF_v_col    <- factor(df_ide$DF_v_col,
                                     levels = c(0,1),
                                     ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_v_col %in% input$DF_v_col)
      }
    }
    if("DF_v_abarrote" %in% input$show_vars){
      if(length(input$DF_v_abarrote)!=0){
        df_ide$DF_v_abarrote    <- factor(df_ide$DF_v_abarrote,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_v_abarrote %in% input$DF_v_abarrote)
      }
    }
    
    if("DF_v_super" %in% input$show_vars){
      if(length(input$DF_v_super)!=0){
        df_ide$DF_v_super   <- factor(df_ide$DF_v_super,
                                      levels = c(0,1),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_v_super %in% input$DF_v_super)
      }
    }
    if("DF_v_conv" %in% input$show_vars){
      if(length(input$DF_v_conv)!=0){
        df_ide$DF_v_conv    <- factor(df_ide$DF_v_conv,
                                      levels = c(0,1),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_v_conv %in% input$DF_v_conv)
      }
    }
    if("DF_v_plaza" %in% input$show_vars){
      if(length(input$DF_v_plaza)!=0){
        df_ide$DF_v_plaza    <- factor(df_ide$DF_v_plaza,
                                       levels = c(0,1),
                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_v_plaza %in% input$DF_v_plaza)
      }
    }
    if("DF_urg_casa" %in% input$show_vars){
      if(length(input$DF_urg_casa)!=0){
        df_ide$DF_urg_casa    <- factor(df_ide$DF_urg_casa,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_casa %in% input$DF_urg_casa)
      }
    }
    
    if("DF_urg_partcom" %in% input$show_vars){
      if(length(input$DF_urg_partcom)!=0){
        df_ide$DF_urg_partcom    <- factor(df_ide$DF_urg_partcom,
                                           levels = c(0,1),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_partcom %in% input$DF_urg_partcom)
      }
    }
    if("DF_urg_partcan" %in% input$show_vars){
      if(length(input$DF_urg_partcan)!=0){
        df_ide$DF_urg_partcan    <- factor(df_ide$DF_urg_partcan,
                                           levels = c(0,1),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_partcan %in% input$DF_urg_partcan)
      }
    }
    if("DF_urg_hosp" %in% input$show_vars){
      if(length(input$DF_urg_hosp)!=0){
        df_ide$DF_urg_hosp    <- factor(df_ide$DF_urg_hosp,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_hosp %in% input$DF_urg_hosp)
      }
    }
    if("DF_urg_cruz" %in% input$show_vars){
      if(length(input$DF_urg_cruz)!=0){
        df_ide$DF_urg_cruz   <- factor(df_ide$DF_urg_cruz,
                                       levels = c(0,1),
                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_cruz %in% input$DF_urg_cruz)
      }
    }
    
    
    if("DF_urg_farmacia" %in% input$show_vars){
      if(length(input$DF_urg_farmacia)!=0){
        df_ide$DF_urg_farmacia    <- factor(df_ide$DF_urg_farmacia,
                                            levels = c(0,1),
                                            ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_farmacia %in% input$DF_urg_farmacia)
      }
    }
    if("DF_urg_otro" %in% input$show_vars){
      if(length(input$DF_urg_otro)!=0){
        df_ide$DF_urg_otro    <- factor(df_ide$DF_urg_otro,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_urg_otro %in% input$DF_urg_otro)
      }
    }
    if("DF_a_parque" %in% input$show_vars){
      if(length(input$DF_a_parque)!=0){
        df_ide$DF_a_parque    <- factor(df_ide$DF_a_parque,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_parque %in% input$DF_a_parque)
      }
    }
    if("DF_a_unidad" %in% input$show_vars){
      if(length(input$DF_a_unidad)!=0){
        df_ide$DF_a_unidad    <- factor(df_ide$DF_a_unidad,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_unidad %in% input$DF_a_unidad)
      }
    }
    
    if("DF_a_jardines" %in% input$show_vars){
      if(length(DF_a_jardines)!=0){
        df_ide$DF_a_jardines    <- factor(df_ide$DF_a_jardines,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_jardines %in% input$DF_a_jardines)
      }
    }
    if("DF_a_casa" %in% input$show_vars){
      if(length(input$DF_a_casa)!=0){
        df_ide$DF_a_casa   <- factor(df_ide$DF_a_casa,
                                     levels = c(0,1),
                                     ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_casa %in% input$DF_a_casa)
      }
    }
    if("DF_a_biblioteca" %in% input$show_vars){
      if(length(input$DF_a_biblioteca)!=0){
        df_ide$DF_a_biblioteca    <- factor(df_ide$DF_a_biblioteca,
                                            levels = c(0,1),
                                            ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_biblioteca %in% input$DF_a_biblioteca)
      }
    }
    if("DF_a_otro" %in% input$show_vars){
      if(length(input$DF_a_otro)!=0){
        df_ide$DF_a_otro    <- factor(df_ide$DF_a_otro,
                                      levels = c(0,1),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DF_a_otro %in% input$DF_a_otro)
      }
    }
    
    if("DE01_hogar_trabajan" %in% input$show_vars){
      if(length(input$DE01_hogar_trabajan)!=0){
        df_ide$DE01_hogar_trabajan    <- factor(df_ide$DE01_hogar_trabajan,
                                                levels = c(0,1,2,3,4,5),
                                                ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DE01_hogar_trabajan %in% input$DE01_hogar_trabajan)
      }
    }
    if("DE02_trabajo" %in% input$show_vars){
      if(length(input$DE02_trabajo)!=0){
        df_ide <- dplyr::filter(df_ide, DE02_trabajo %in% input$DE02_trabajo)
      }
    }
    if("DE03_puesto" %in% input$show_vars){
      if(length(input$DE03_puesto)!=0){
        df_ide <- dplyr::filter(df_ide, DE03_puesto %in% input$DE03_puesto)
      }
    }
    if("DE05_ingreso_sem" %in% input$show_vars){
      if(length(input$DE05_ingreso_sem)!=0){
        df_ide$DE05_ingreso_sem   <- factor(df_ide$DE05_ingreso_sem,
                                            levels = c("A","B","C","D"),
                                            ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DE05_ingreso_sem %in% input$DE05_ingreso_sem)
      }
    }
    
    if("DE06_esc_jefe" %in% input$show_vars){
      if(length(input$DE06_esc_jefe)!=0){
        df_ide <- dplyr::filter(df_ide, DE06_esc_jefe %in% input$DE06_esc_jefe)
      }
    }
    if("DE07_sIngreso" %in% input$show_vars){
      if(length(input$DE07_sIngreso)!=0){
        df_ide$DE07_sIngreso   <- factor(df_ide$DE07_sIngreso,
                                         levels = c(0,1,2,3,4,5),
                                         ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DE07_sIngreso %in% input$DE07_sIngreso)
      }
    }
    if("DE08_tmp" %in% input$show_vars){
      if(length(input$DE08_tmp)!=0){
        df_ide$DE08_tmp   <- factor(df_ide$DE08_tmp,
                                    levels = c("A","B","C","D","E"),
                                    ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, DE08_tmp %in% input$DE08_tmp)
      }
    }
    if("DE09_col" %in% input$show_vars){
      if(length(input$DE09_col)!=0){
        df_ide <- dplyr::filter(df_ide, DE09_col %in% input$DE09_col)
      }
    }
    
    if("DE10_mun" %in% input$show_vars){
      if(length(input$DE10_mun)!=0){
        df_ide <- dplyr::filter(df_ide, DE10_mun %in% input$DE10_mun)
      }
    }
    if("IyC02_Estado_Origen" %in% input$show_vars){
      if(length(input$IyC02_Estado_Origen)!=0){
        df_ide <- dplyr::filter(df_ide, IyC02_Estado_Origen %in% input$IyC02_Estado_Origen)
      }
    }
    if("IyC03_Tiempo" %in% input$show_vars){
      if(length(input$IyC03_Tiempo)!=0){
        df_ide <- dplyr::filter(df_ide, IyC03_Tiempo %in% input$IyC03_Tiempo)
      }
    }
    if("IyC04_Motivo_Localidad_Parientes" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Parientes)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Parientes %in% input$IyC04_Motivo_Localidad_Parientes)
      }
    }
    
    if("IyC04_Motivo_Localidad_Amigos" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Amigos)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Amigos %in% input$IyC04_Motivo_Localidad_Amigos)
      }
    }
    if("IyC04_Motivo_Localidad_Trabajo" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Trabajo)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Trabajo %in% input$IyC04_Motivo_Localidad_Trabajo)
      }
    }
    if("IyC04_Motivo_Localidad_Negocio" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Negocio)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Negocio %in% input$IyC04_Motivo_Localidad_Negocio)
      }
    }
    if("IyC04_Motivo_Localidad_Oportunidad" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Oportunidad)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Oportunidad %in% input$IyC04_Motivo_Localidad_Oportunidad)
      }
    }
    
    if("IyC04_Motivo_Localidad_Otro" %in% input$show_vars){
      if(length(input$IyC04_Motivo_Localidad_Otro)!=0){
        df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Otro %in% input$IyC04_Motivo_Localidad_Otro)
      }
    }
    if("IyC05_Acudo_Vecinos" %in% input$show_vars){
      if(length(input$IyC05_Acudo_Vecinos)!=0){
        df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Vecinos %in% input$IyC05_Acudo_Vecinos)
      }
    }
    if("IyC05_Acudo_Familia" %in% input$show_vars){
      if(length(input$IyC05_Acudo_Familia)!=0){
        df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Familia %in% input$IyC05_Acudo_Familia)
      }
    }
    if("IyC05_Acudo_Autoridad" %in% input$show_vars){
      if(length(input$IyC05_Acudo_Autoridad)!=0){
        df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Autoridad %in% input$IyC05_Acudo_Autoridad)
      }
    }
    
    
    if("IyC05_Acudo_Iglesia" %in% input$show_vars){
      if(length(input$IyC05_Acudo_Iglesia)!=0){
        df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Iglesia %in% input$IyC05_Acudo_Iglesia)
      }
    }
    if("IyC05_Acudo_Otro" %in% input$show_vars){
      if(length(input$IyC05_Acudo_Otro)!=0){
        df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Otro %in% input$IyC05_Acudo_Otro)
      }
    }
    if("IyC06_Religion" %in% input$show_vars){
      if(length(input$IyC06_Religion)!=0){
        df_ide <- dplyr::filter(df_ide, IyC06_Religion %in% input$IyC06_Religion)
      }
    }
    #if("IyC07_Costumbres" %in% input$show_vars){
    # if(length(input$IyC07_Costumbres)!=0){
    #  df_ide <- dplyr::filter(df_ide, IyC07_Costumbres %in% input$IyC07_Costumbres)
    #}
    #}
    
    if("IyC08_Ventajas_Casa" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Casa)!=0){
        df_ide$IyC08_Ventajas_Casa    <- factor(df_ide$IyC08_Ventajas_Casa,
                                                levels = c(0,1),
                                                ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Casa %in% input$IyC08_Ventajas_Casa)
      }
    }
    if("IyC08_Ventajas_Trabajo" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Trabajo)!=0){
        df_ide$IyC08_Ventajas_Trabajo    <- factor(df_ide$IyC08_Ventajas_Trabajo,
                                                   levels = c(0,1),
                                                   ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Trabajo %in% input$IyC08_Ventajas_Trabajo)
      }
    }
    if("IyC08_Ventajas_Familia" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Familia)!=0){
        df_ide$IyC08_Ventajas_Familia    <- factor(df_ide$IyC08_Ventajas_Familia,
                                                   levels = c(0,1),
                                                   ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Familia %in% input$IyC08_Ventajas_Familia)
      }
    }
    if("IyC08_Ventajas_Tiempo" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Tiempo)!=0){
        df_ide$IyC08_Ventajas_Tiempo   <- factor(df_ide$IyC08_Ventajas_Tiempo,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tiempo %in% input$IyC08_Ventajas_Tiempo)
      }
    }
    
    if("IyC08_Ventajas_Tranquilo" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Tranquilo)!=0){
        df_ide$IyC08_Ventajas_Tranquilo    <- factor(df_ide$IyC08_Ventajas_Tranquilo,
                                                     levels = c(0,1),
                                                     ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tranquilo %in% input$IyC08_Ventajas_Tranquilo)
      }
    }
    if("IyC08_Ventajas_Seguro" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Seguro)!=0){
        df_ide$IyC08_Ventajas_Seguro    <- factor(df_ide$IyC08_Ventajas_Seguro,
                                                  levels = c(0,1),
                                                  ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Seguro %in% input$IyC08_Ventajas_Seguro)
      }
    }
    if("IyC08_Ventajas_Otro" %in% input$show_vars){
      if(length(input$IyC08_Ventajas_Otro)!=0){
        df_ide$IyC08_Ventajas_Otro    <- factor(df_ide$IyC08_Ventajas_Otro,
                                                levels = c(0,1),
                                                ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Otro %in% input$IyC08_Ventajas_Otro)
      }
    }
    
    if("IyC09_Emigrar" %in% input$show_vars){
      if(length(input$IyC09_Emigrar)!=0){
        df_ide <- dplyr::filter(df_ide, IyC09_Emigrar %in% input$IyC09_Emigrar)
      }
    }
    if("IyC10_Pertenencia" %in% input$show_vars){
      if(length(input$IyC10_Pertenencia)!=0){
        df_ide <- dplyr::filter(df_ide, IyC10_Pertenencia %in% input$IyC10_Pertenencia)
      }
    }
    if("IyC11_Frecuencia_Cabecera_Isla" %in% input$show_vars){
      if(length(input$IyC11_Frecuencia_Cabecera_Isla)!=0){
        df_ide <- dplyr::filter(df_ide, IyC11_Frecuencia_Cabecera_Isla %in% input$IyC11_Frecuencia_Cabecera_Isla)
      }
    }
    if("IyC12_Motivo_Viaja_Asuntos_Admin" %in% input$show_vars){
      if(length(input$IyC12_Motivo_Viaja_Asuntos_Admin)!=0){
        df_ide$IyC12_Motivo_Viaja_Asuntos_Admin    <- factor(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin,
                                                             levels = c(0,1),
                                                             ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Asuntos_Admin %in% input$IyC12_Motivo_Viaja_Asuntos_Admin)
      }
    }
    
    if("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS" %in% input$show_vars){
      if(length(input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS)!=0){
        df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS    <- factor(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS,
                                                                       levels = c(0,1),
                                                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS %in% input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS)
      }
    }
    if("IyC12_Motivo_Viaja_Trabajo" %in% input$show_vars){
      if(length(input$IyC12_Motivo_Viaja_Trabajo)!=0){
        df_ide$IyC12_Motivo_Viaja_Trabajo   <- factor(df_ide$IyC12_Motivo_Viaja_Trabajo,
                                                      levels = c(0,1),
                                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Trabajo %in% input$IyC12_Motivo_Viaja_Trabajo)
      }
    }
    if("IyC12_Motivio_Viaja_Recreacion" %in% input$show_vars){
      if(length(input$IyC12_Motivio_Viaja_Recreacion)!=0){
        df_ide$IyC12_Motivio_Viaja_Recreacion    <- factor(df_ide$IyC12_Motivio_Viaja_Recreacion,
                                                           levels = c(0,1),
                                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivio_Viaja_Recreacion %in% input$IyC12_Motivio_Viaja_Recreacion)
      }
    }
    if("IyC12_Motivo_Viaja_Familia" %in% input$show_vars){
      if(length(input$IyC12_Motivo_Viaja_Familia)!=0){
        
        df_ide$IyC12_Motivo_Viaja_Familia    <- factor(df_ide$IyC12_Motivo_Viaja_Familia,
                                                       levels = c(0,1),
                                                       ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Familia %in% input$IyC12_Motivo_Viaja_Familia)
      }
    }
    
    if("IyC12_Motivo_Viaja_Otro" %in% input$show_vars){
      if(length(input$IyC12_Motivo_Viaja_Otro)!=0){
        df_ide$IyC12_Motivo_Viaja_Otro    <- factor(df_ide$IyC12_Motivo_Viaja_Otro,
                                                    levels = c(0,1),
                                                    ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Otro %in% input$IyC12_Motivo_Viaja_Otro)
      }
    }
    if("VI01_La_vivienda_es" %in% input$show_vars){
      if(length(input$VI01_La_vivienda_es)!=0){
        df_ide <- dplyr::filter(df_ide, VI01_La_vivienda_es %in% input$VI01_La_vivienda_es)
      }
    }
    if("VI04_Contado" %in% input$show_vars){
      if(length(input$VI04_Contado)!=0){
        df_ide$VI04_Contado   <- factor(df_ide$VI04_Contado,
                                        levels = c(0,1),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI04_Contado %in% input$VI04_Contado)
      }
    }
    if("VI04_Herencia" %in% input$show_vars){
      if(length(input$VI04_Herencia)!=0){
        df_ide$VI04_Herencia    <- factor(df_ide$VI04_Herencia,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI04_Herencia %in% input$VI04_Herencia)
      }
    }
    
    if("VI04_Mensual" %in% input$show_vars){
      if(length(input$VI04_Mensual)!=0){
        df_ide$VI04_Mensual    <- factor(df_ide$VI04_Mensual,
                                         levels = c(0,1),
                                         ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI04_Mensual %in% input$VI04_Mensual)
      }
    }
    if("VI05_CPropiaMensual" %in% input$show_vars){
      if(length(input$VI05_CPropiaMensual)!=0){
        df_ide <- dplyr::filter(df_ide, VI05_CPropiaMensual %in% input$VI05_CPropiaMensual)
      }
    }
    if("VI06_CPropiaAdquirida" %in% input$show_vars){
      if(length(input$VI06_CPropiaAdquirida)!=0){
        df_ide <- dplyr::filter(df_ide, VI06_CPropiaAdquirida %in% input$VI06_CPropiaAdquirida)
      }
    }
    
    if("VI07_FALTA_DE_SERVICIOS" %in% input$show_vars){
      if(length(input$VI07_FALTA_DE_SERVICIOS)!=0){
        df_ide <- dplyr::filter(df_ide, VI07_FALTA_DE_SERVICIOS %in% input$VI07_FALTA_DE_SERVICIOS)
      }
    }
    if("VI08NINUNDACIONES" %in% input$show_vars){
      if(length(input$VI08NINUNDACIONES)!=0){
        df_ide$VI08NINUNDACIONES    <- factor(df_ide$VI08NINUNDACIONES,
                                              levels = c(0,1),
                                              ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI08NINUNDACIONES %in% input$VI08NINUNDACIONES)
      }
    }
    if("VI09PeHuracan" %in% input$show_vars){
      if(length(input$VI09PeHuracan)!=0){
        df_ide$VI09PeHuracan    <- factor(df_ide$VI09PeHuracan,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI09PeHuracan %in% input$VI09PeHuracan)
      }
    }
    if("VI14SabeZo0Riesgo" %in% input$show_vars){
      if(length(input$VI14SabeZo0Riesgo)!=0){
        df_ide$VI14SabeZo0Riesgo    <- factor(df_ide$VI14SabeZo0Riesgo,
                                              levels = c(0,1),
                                              ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI14SabeZo0Riesgo %in% input$VI14SabeZo0Riesgo)
      }
    }
    
    if("VI15SabeAfectaciones" %in% input$show_vars){
      if(length(input$VI15SabeAfectaciones)!=0){
        df_ide$VI15SabeAfectaciones    <- factor(df_ide$VI15SabeAfectaciones,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, VI15SabeAfectaciones %in% input$VI15SabeAfectaciones)
      }
    }
    if("AE1_AreasVerdes" %in% input$show_vars){
      if(length(input$AE1_AreasVerdes)!=0){
        df_ide$AE1_AreasVerdes   <- factor(df_ide$AE1_AreasVerdes,
                                           levels = c(0,1),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE1_AreasVerdes %in% input$AE1_AreasVerdes)
      }
    }
    if("AE3_Banquetas" %in% input$show_vars){
      if(length(input$AE3_Banquetas)!=0){
        df_ide$AE3_Banquetas    <- factor(df_ide$AE3_Banquetas,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE3_Banquetas %in% input$AE3_Banquetas)
      }
    }
    if("AE4_Lumi0rias" %in% input$show_vars){
      if(length(input$AE4_Lumi0rias)!=0){
        df_ide$AE4_Lumi0rias    <- factor(df_ide$AE4_Lumi0rias,
                                          levels = c(0,1),
                                          ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE4_Lumi0rias %in% input$AE4_Lumi0rias)
      }
    }
    
    if("AE5_Transporte" %in% input$show_vars){
      if(length(input$AE5_Transporte)!=0){
        df_ide$AE5_Transporte    <- factor(df_ide$AE5_Transporte,
                                           levels = c(0,1),
                                           ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE5_Transporte %in% input$AE5_Transporte)
      }
    }
    if("AE6_Patrullas" %in% input$show_vars){
      if(length(input$AE6_Patrullas)!=0){
        df_ide$AE6_Patrullas   <- factor(df_ide$AE6_Patrullas,
                                         levels = c(0,1),
                                         ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE6_Patrullas %in% input$AE6_Patrullas)
      }
    }
    if("AE7_Lotes" %in% input$show_vars){
      if(length(input$AE7_Lotes)!=0){
        df_ide$AE7_Lotes    <- factor(df_ide$AE7_Lotes,
                                      levels = c(0,1),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE7_Lotes %in% input$AE7_Lotes)
      }
    }
    if("AE9_PeSeguridad" %in% input$show_vars){
      if(length(input$AE9_PeSeguridad)!=0){
        df_ide$AE9_PeSeguridad<- factor(df_ide$AE9_PeSeguridad,
                                        levels = c(0,1,2,3,4,5),
                                        ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE9_PeSeguridad %in% input$AE9_PeSeguridad)
      }
    }
    if("AE10_PeComodidad" %in% input$show_vars){
      if(length(input$AE10_PeComodidad)!=0){
        df_ide$AE10_PeComodidad<- factor(df_ide$AE10_PeComodidad,
                                         levels = c(0,1,2,3,4,5),
                                         ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE10_PeComodidad %in% input$AE10_PeComodidad)
      }
    }
    if("AE11_PeRiesgo" %in% input$show_vars){
      if(length(input$AE11_PeRiesgo)!=0){
        df_ide$AE11_PeRiesgo<- factor(df_ide$AE11_PeRiesgo,
                                      levels = c(0,1,2,3,4,5),
                                      ordered = TRUE)
        df_ide <- dplyr::filter(df_ide, AE11_PeRiesgo %in% input$AE11_PeRiesgo)
      }
    }
    
    return(df_ide)
  })
  
  
  output$rows1 <- renderUI({
    if("Info.Sexo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$Info.Sexo    <- factor(df_ide$Info.Sexo,levels = c("H", "M"), ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i]) }
      updateSelectInput(getDefaultReactiveDomain(),"Info.Sexo","Info.Sexo", choices = unique(df_ide$Info.Sexo))
      selectInput("Info.Sexo", "Info.Sexo",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows2 <- renderUI({
    if("Info.Edad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      
      df_ide$Info.Edad <- ordered(cut(df_ide$Info.Edad,
                                      c(0, 18, 30, 60, 100), 
                                      labels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                                      include.lowest = TRUE))
      
      
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Info.Edad", choices = unique(df_ide$Info.Edad))
      selectInput("Info.Edad", "Info.Edad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows3 <- renderUI({
    if("Direccion.Calle" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Calle","Direccion.Calle", choices = unique(df_ide$Direccion.Calle))
      selectInput("Direccion.Calle", "Direccion.Calle", choices = NULL,multiple = TRUE)}
  })
  
  output$rows4 <- renderUI({
    if("Direccion.Numero.Exterior" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Numero.Exterior","Direccion.Numero.Exterior", choices = unique(df_ide$Direccion.Numero.Exterior))
      selectInput("Direccion.Numero.Exterior", "Direccion.Numero.Exterior", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows5 <- renderUI({
    if("Direccion.Numero.Interior" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Numero.Interior","Direccion.Numero.Interior", choices = unique(df_ide$Direccion.Numero.Interior))
      selectInput("Direccion.Numero.Interior", "Direccion.Numero.Interior",choices = NULL, multiple = TRUE)}
  })
  
  output$rows6 <- renderUI({
    if("Direccion.Manza0" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Manza0","Direccion.Manza0", choices = unique(df_ide$Direccion.Manza0))
      selectInput("Direccion.Manza0", "Direccion.Manza0", choices = NULL,multiple = TRUE)}
  })
  
  output$rows7 <- renderUI({
    if("Direccion.Super.Manza0" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Super.Manza0","Direccion.Super.Manza0", choices = unique(df_ide$Direccion.Super.Manza0))
      selectInput("Direccion.Super.Manza0", "Direccion.Super.Manza0", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows8 <- renderUI({
    if("Direccion.Colonia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Colonia","Direccion.Colonia", choices = unique(df_ide$Direccion.Colonia))
      selectInput("Direccion.Colonia", "Direccion.Colonia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows9 <- renderUI({
    if("Direccion.Codigo.Postal" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Codigo.Postal","Direccion.Codigo.Postal", choices = unique(df_ide$Direccion.Codigo.Postal))
      selectInput("Direccion.Codigo.Postal", "Direccion.Codigo.Postal", choices = NULL,multiple = TRUE)}
  })
  
  output$rows10 <- renderUI({
    if("V01_Casa_Adultos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$V01_Casa_Adultos    <- factor(df_ide$V01_Casa_Adultos,
                                           levels = c("1", "2", "3", "4", "5", "6", "7", "13"),
                                           ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"V01_Casa_Adultos","V01_Casa_Adultos", choices = unique(df_ide$V01_Casa_Adultos))
      selectInput("V01_Casa_Adultos", "V01_Casa_Adultos", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows11 <- renderUI({
    if("V02_Casa_Ninos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$V02_Casa_Ninos    <- factor(df_ide$V02_Casa_Ninos,
                                         levels = c("0", "1", "2", "3", "4", "5", "6", "7"),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"V02_Casa_Ninos","V02_Casa_Ninos", choices = unique(df_ide$V02_Casa_Ninos))
      selectInput("V02_Casa_Ninos", "V02_Casa_Ninos",choices = NULL, multiple = TRUE)}
  })
  
  output$rows12 <- renderUI({
    if("DF_kinder" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_kinder    <- factor(df_ide$DF_kinder,
                                    levels = c(0,1,2),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_kinder","DF_kinder", choices = unique(df_ide$DF_kinder))
      selectInput("DF_kinder", "DF_kinder", choices = NULL,multiple = TRUE)}
  })
  
  output$rows13 <- renderUI({
    if("DF_primaria" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_primaria    <- factor(df_ide$DF_primaria,
                                      levels = c(0,1,2,3,5),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_primaria","DF_primaria", choices = unique(df_ide$DF_primaria))
      selectInput("DF_primaria", "DF_primaria", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows14 <- renderUI({
    if("DF_secundaria" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_secundaria    <- factor(df_ide$DF_secundaria,
                                        levels = c(0,1,2,3,4),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_secundaria","DF_secundaria", choices = unique(df_ide$DF_secundaria))
      selectInput("DF_secundaria", "DF_secundaria",choices = NULL, multiple = TRUE)}
  })
  
  output$rows15 <- renderUI({
    if("DF_bachillerato" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_bachillerato   <- factor(df_ide$DF_bachillerato,
                                         levels = c(0,1,2,3,4),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_bachillerato","DF_bachillerato", choices = unique(df_ide$DF_bachillerato))
      selectInput("DF_bachillerato", "DF_bachillerato", choices = NULL,multiple = TRUE)}
  })
  
  output$rows16 <- renderUI({
    if("DF_licenciatura" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_licenciatura    <- factor(df_ide$DF_licenciatura,
                                          levels = c(0,1,2,3),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_licenciatura","DF_licenciatura", choices = unique(df_ide$DF_licenciatura))
      selectInput("DF_licenciatura", "DF_licenciatura", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows17 <- renderUI({
    if("DF_tr_autobus" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_autobus   <- factor(df_ide$DF_tr_autobus,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_autobus","DF_tr_autobus", choices = unique(df_ide$DF_tr_autobus))
      selectInput("DF_tr_autobus", "DF_tr_autobus",choices = NULL, multiple = TRUE)}
  })
  
  output$rows18 <- renderUI({
    if("DF_tr_colectivo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_colectivo    <- factor(df_ide$DF_tr_colectivo,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_colectivo","DF_tr_colectivo", choices = unique(df_ide$DF_tr_colectivo))
      selectInput("DF_tr_colectivo", "DF_tr_colectivo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows19 <- renderUI({
    if("DF_tr_taxi" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_taxi   <- factor(df_ide$DF_tr_taxi,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_taxi","DF_tr_taxi", choices = unique(df_ide$DF_tr_taxi))
      selectInput("DF_tr_taxi", "DF_tr_taxi", choices = NULL,multiple = TRUE)}
  })
  
  output$rows20 <- renderUI({
    if("DF_tr_mototaxi" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_mototaxi   <- factor(df_ide$DF_tr_mototaxi,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_mototaxi","DF_tr_mototaxi", choices = unique(df_ide$DF_tr_mototaxi))
      selectInput("DF_tr_mototaxi", "DF_tr_mototaxi", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows21 <- renderUI({
    if("DF_tr_moto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_moto    <- factor(df_ide$DF_tr_moto,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_moto","DF_tr_moto", choices = unique(df_ide$DF_tr_moto))
      selectInput("DF_tr_moto", "DF_tr_moto",choices = NULL, multiple = TRUE)}
  })
  
  output$rows22 <- renderUI({
    if("DF_tr_auto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_tr_auto    <- factor(df_ide$DF_tr_auto,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_auto","DF_tr_auto", choices = unique(df_ide$DF_tr_auto))
      selectInput("DF_tr_auto", "DF_tr_auto", choices = NULL,multiple = TRUE)}
  })
  
  output$rows23 <- renderUI({
    if("DF_v_col" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_v_col    <- factor(df_ide$DF_v_col,
                                   levels = c(0,1),
                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_col","DF_v_col", choices = unique(df_ide$DF_v_col))
      selectInput("DF_v_col", "DF_v_col", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows24 <- renderUI({
    if("DF_v_abarrote" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_v_abarrote    <- factor(df_ide$DF_v_abarrote,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_abarrote","DF_v_abarrote", choices = unique(df_ide$DF_v_abarrote))
      selectInput("DF_v_abarrote", "DF_v_abarrote", choices = NULL,multiple = TRUE)}
  })
  
  output$rows25 <- renderUI({
    if("DF_v_super" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_v_super   <- factor(df_ide$DF_v_super,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_super","DF_v_super", choices = unique(df_ide$DF_v_super))
      selectInput("DF_v_super", "DF_v_super",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows26 <- renderUI({
    if("DF_v_conv" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_v_conv    <- factor(df_ide$DF_v_conv,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_conv","DF_v_conv", choices = unique(df_ide$DF_v_conv))
      selectInput("DF_v_conv", "DF_v_conv", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows27 <- renderUI({
    if("DF_v_plaza" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_v_plaza    <- factor(df_ide$DF_v_plaza,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_plaza","DF_v_plaza", choices = unique(df_ide$DF_v_plaza))
      selectInput("DF_v_plaza", "DF_v_plaza",choices = NULL, multiple = TRUE)}
  })
  
  output$rows28 <- renderUI({
    if("DF_urg_casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_casa    <- factor(df_ide$DF_urg_casa,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_casa","DF_urg_casa", choices = unique(df_ide$DF_urg_casa))
      selectInput("DF_urg_casa", "DF_urg_casa",choices = NULL, multiple = TRUE)}
  })
  
  output$rows29 <- renderUI({
    if("DF_urg_partcom" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_partcom    <- factor(df_ide$DF_urg_partcom,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_partcom","DF_urg_partcom", choices = unique(df_ide$DF_urg_partcom))
      selectInput("DF_urg_partcom", "DF_urg_partcom", choices = NULL,multiple = TRUE)}
  })
  
  output$rows30 <- renderUI({
    if("DF_urg_partcan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_partcan   <- factor(df_ide$DF_urg_partcan,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_partcan","DF_urg_partcan", choices = unique(df_ide$DF_urg_partcan))
      selectInput("DF_urg_partcan", "DF_urg_partcan", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows31 <- renderUI({
    if("DF_urg_hosp" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_hosp    <- factor(df_ide$DF_urg_hosp,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_hosp","DF_urg_hosp", choices = unique(df_ide$DF_urg_hosp))
      selectInput("DF_urg_hosp", "DF_urg_hosp",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows32 <- renderUI({
    if("DF_urg_cruz" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_cruz   <- factor(df_ide$DF_urg_cruz,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_cruz","DF_urg_cruz", choices = unique(df_ide$DF_urg_cruz))
      selectInput("DF_urg_cruz", "DF_urg_cruz",choices = NULL, multiple = TRUE)}
  })
  
  output$rows33 <- renderUI({
    if("DF_urg_farmacia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_farmacia    <- factor(df_ide$DF_urg_farmacia,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_farmacia","DF_urg_farmacia", choices = unique(df_ide$DF_urg_farmacia))
      selectInput("DF_urg_farmacia", "DF_urg_farmacia", choices = NULL,multiple = TRUE)}
  })
  
  output$rows34 <- renderUI({
    if("DF_urg_otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_urg_otro    <- factor(df_ide$DF_urg_otro,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_otro","DF_urg_otro", choices = unique(df_ide$DF_urg_otro))
      selectInput("DF_urg_otro", "DF_urg_otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows35 <- renderUI({
    if("DF_a_parque" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_parque    <- factor(df_ide$DF_a_parque,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_parque","DF_a_parque", choices = unique(df_ide$DF_a_parque))
      selectInput("DF_a_parque", "DF_a_parque",choices = NULL, multiple = TRUE)}
  })
  
  output$rows36 <- renderUI({
    if("DF_a_unidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_unidad    <- factor(df_ide$DF_a_unidad,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_unidad","DF_a_unidad", choices = unique(df_ide$DF_a_unidad))
      selectInput("DF_a_unidad", "DF_a_unidad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows37 <- renderUI({
    if("DF_a_jardines" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_jardines    <- factor(df_ide$DF_a_jardines,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_jardines","DF_a_jardines", choices = unique(df_ide$DF_a_jardines))
      selectInput("DF_a_jardines", "DF_a_jardines", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows38 <- renderUI({
    if("DF_a_casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_casa   <- factor(df_ide$DF_a_casa,
                                   levels = c(0,1),
                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_casa","DF_a_casa", choices = unique(df_ide$DF_a_casa))
      selectInput("DF_a_casa", "DF_a_casa",choices = NULL, multiple = TRUE)}
  })
  
  output$rows39 <- renderUI({
    if("DF_a_biblioteca" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_biblioteca    <- factor(df_ide$DF_a_biblioteca,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_biblioteca","DF_a_biblioteca", choices = unique(df_ide$DF_a_biblioteca))
      selectInput("DF_a_biblioteca", "DF_a_biblioteca", choices = NULL,multiple = TRUE)}
  })
  
  output$rows40 <- renderUI({
    if("DF_a_otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DF_a_otro    <- factor(df_ide$DF_a_otro,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_otro","DF_a_otro", choices = unique(df_ide$DF_a_otro))
      selectInput("DF_a_otro", "DF_a_otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows41 <- renderUI({
    if("DE01_hogar_trabajan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DE01_hogar_trabajan    <- factor(df_ide$DE01_hogar_trabajan,
                                              levels = c(0,1,2,3,4,5),
                                              ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE01_hogar_trabajan","DE01_hogar_trabajan", choices = unique(df_ide$DE01_hogar_trabajan))
      selectInput("DE01_hogar_trabajan", "DE01_hogar_trabajan",choices = NULL, multiple = TRUE)}
  })
  
  output$rows42 <- renderUI({
    if("DE02_trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE02_trabajo","DE02_trabajo", choices = unique(df_ide$DE02_trabajo))
      selectInput("DE02_trabajo", "DE02_trabajo", choices = NULL,multiple = TRUE)}
  })
  
  output$rows43 <- renderUI({
    if("DE03_puesto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE03_puesto","DE03_puesto", choices = unique(df_ide$DE03_puesto))
      selectInput("DE03_puesto", "DE03_puesto", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows44 <- renderUI({
    if("DE05_ingreso_sem" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DE05_ingreso_sem   <- factor(df_ide$DE05_ingreso_sem,
                                          levels = c("A","B","C","D"),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE05_ingreso_sem","DE05_ingreso_sem", choices = unique(df_ide$DE05_ingreso_sem))
      selectInput("DE05_ingreso_sem", "DE05_ingreso_sem",choices = NULL, multiple = TRUE)}
  })
  
  output$rows45 <- renderUI({
    if("DE06_esc_jefe" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE06_esc_jefe","DE06_esc_jefe", choices = unique(df_ide$DE06_esc_jefe))
      selectInput("DE06_esc_jefe", "DE06_esc_jefe", choices = NULL,multiple = TRUE)}
  })
  
  output$rows46 <- renderUI({
    if("DE07_sIngreso" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DE07_sIngreso   <- factor(df_ide$DE07_sIngreso,
                                       levels = c(0,1,2,3,4,5),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE07_sIngreso","DE07_sIngreso", choices = unique(df_ide$DE07_sIngreso))
      selectInput("DE07_sIngreso", "DE07_sIngreso", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows47 <- renderUI({
    if("DE08_tmp" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$DE08_tmp   <- factor(df_ide$DE08_tmp,
                                  levels = c("A","B","C","D","E"),
                                  ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE08_tmp","DE08_tmp", choices = unique(df_ide$DE08_tmp))
      selectInput("DE08_tmp", "DE08_tmp",choices = NULL, multiple = TRUE)}
  })
  
  output$rows48 <- renderUI({
    if("DE09_col" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE09_col","DE09_col", choices = unique(df_ide$DE09_col))
      selectInput("DE09_col", "DE09_col",choices = NULL, multiple = TRUE)}
  })
  
  output$rows49 <- renderUI({
    if("DE10_mun" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE10_mun","DE10_mun", choices = unique(df_ide$DE10_mun))
      selectInput("DE10_mun", "DE10_mun", choices = NULL,multiple = TRUE)}
  })
  
  output$rows50 <- renderUI({
    if("IyC02_Estado_Origen" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC02_Estado_Origen","IyC02_Estado_Origen", choices = unique(df_ide$IyC02_Estado_Origen))
      selectInput("IyC02_Estado_Origen", "IyC02_Estado_Origen", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows51 <- renderUI({
    if("IyC03_Tiempo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC03_Tiempo","IyC03_Tiempo", choices = unique(df_ide$IyC03_Tiempo))
      selectInput("IyC03_Tiempo", "IyC03_Tiempo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows52 <- renderUI({
    if("IyC04_Motivo_Localidad_Parientes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Parientes","IyC04_Motivo_Localidad_Parientes", choices = unique(df_ide$IyC04_Motivo_Localidad_Parientes))
      selectInput("IyC04_Motivo_Localidad_Parientes", "IyC04_Motivo_Localidad_Parientes", choices = NULL,multiple = TRUE)}
  })
  
  output$rows53 <- renderUI({
    if("IyC04_Motivo_Localidad_Amigos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Amigos","IyC04_Motivo_Localidad_Amigos", choices = unique(df_ide$IyC04_Motivo_Localidad_Amigos))
      selectInput("IyC04_Motivo_Localidad_Amigos", "IyC04_Motivo_Localidad_Amigos", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows54 <- renderUI({
    if("IyC04_Motivo_Localidad_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Trabajo","IyC04_Motivo_Localidad_Trabajo", choices = unique(df_ide$IyC04_Motivo_Localidad_Trabajo))
      selectInput("IyC04_Motivo_Localidad_Trabajo", "IyC04_Motivo_Localidad_Trabajo", choices = NULL,multiple = TRUE)}
  })
  
  output$rows55 <- renderUI({
    if("IyC04_Motivo_Localidad_Negocio" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Negocio","IyC04_Motivo_Localidad_Negocio", choices = unique(df_ide$IyC04_Motivo_Localidad_Negocio))
      selectInput("IyC04_Motivo_Localidad_Negocio", "IyC04_Motivo_Localidad_Negocio",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows56 <- renderUI({
    if("IyC04_Motivo_Localidad_Oportunidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Oportunidad","IyC04_Motivo_Localidad_Oportunidad", choices = unique(df_ide$IyC04_Motivo_Localidad_Oportunidad))
      selectInput("IyC04_Motivo_Localidad_Oportunidad", "IyC04_Motivo_Localidad_Oportunidad",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows57 <- renderUI({
    if("IyC04_Motivo_Localidad_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Otro","IyC04_Motivo_Localidad_Otro", choices = unique(df_ide$IyC04_Motivo_Localidad_Otro))
      selectInput("IyC04_Motivo_Localidad_Otro", "IyC04_Motivo_Localidad_Otro",choices = NULL, multiple = TRUE)}
  })
  
  output$rows58 <- renderUI({
    if("IyC05_Acudo_Vecinos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Vecinos","IyC05_Acudo_Vecinos", choices = unique(df_ide$IyC05_Acudo_Vecinos))
      selectInput("IyC05_Acudo_Vecinos", "IyC05_Acudo_Vecinos", choices = NULL,multiple = TRUE)}
  })
  
  output$rows59 <- renderUI({
    if("IyC05_Acudo_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Familia","IyC05_Acudo_Familia", choices = unique(df_ide$IyC05_Acudo_Familia))
      selectInput("IyC05_Acudo_Familia", "IyC05_Acudo_Familia", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows60 <- renderUI({
    if("IyC05_Acudo_Autoridad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Autoridad","IyC05_Acudo_Autoridad", choices = unique(df_ide$IyC05_Acudo_Autoridad))
      selectInput("IyC05_Acudo_Autoridad", "IyC05_Acudo_Autoridad",choices = NULL, multiple = TRUE)}
  })
  
  output$rows61 <- renderUI({
    if("IyC05_Acudo_Iglesia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Iglesia","IyC05_Acudo_Iglesia", choices = unique(df_ide$IyC05_Acudo_Iglesia))
      selectInput("IyC05_Acudo_Iglesia", "IyC05_Acudo_Iglesia",choices = NULL, multiple = TRUE)}
  })
  
  
  
  
  
  
  output$rows62 <- renderUI({
    if("IyC05_Acudo_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Otro","IyC05_Acudo_Otro", choices = unique(df_ide$IyC05_Acudo_Otro))
      selectInput("IyC05_Acudo_Otro", "IyC05_Acudo_Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows63 <- renderUI({
    if("IyC06_Religion" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC06_Religion","IyC06_Religion", choices = unique(df_ide$IyC06_Religion))
      selectInput("IyC06_Religion", "IyC06_Religion",choices = NULL, multiple = TRUE)}
  })
  
  
  
  #output$rows64 <- renderUI({
  # if("IyC07_Costumbres" %in% input$show_vars){
  #    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
  #    for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
  #    updateSelectInput(getDefaultReactiveDomain(),"IyC07_Costumbres","IyC07_Costumbres", choices = unique(df_ide$IyC07_Costumbres))
  #    selectInput("IyC07_Costumbres", "IyC07_Costumbres",choices = NULL, multiple = TRUE)}
  #})
  
  output$rows65 <- renderUI({
    if("IyC08_Ventajas_Casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Casa","IyC08_Ventajas_Casa", choices = unique(df_ide$IyC08_Ventajas_Casa))
      df_ide$IyC08_Ventajas_Casa    <- factor(df_ide$IyC08_Ventajas_Casa,
                                              levels = c(0,1),
                                              ordered = TRUE)
      selectInput("IyC08_Ventajas_Casa", "IyC08_Ventajas_Casa", choices = NULL,multiple = TRUE)}
  })
  
  output$rows66 <- renderUI({
    if("IyC08_Ventajas_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Trabajo    <- factor(df_ide$IyC08_Ventajas_Trabajo,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Trabajo","IyC08_Ventajas_Trabajo", choices = unique(df_ide$IyC08_Ventajas_Trabajo))
      selectInput("IyC08_Ventajas_Trabajo", "IyC08_Ventajas_Trabajo", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows67 <- renderUI({
    if("IyC08_Ventajas_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Familia    <- factor(df_ide$IyC08_Ventajas_Familia,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Familia","IyC08_Ventajas_Familia", choices = unique(df_ide$IyC08_Ventajas_Familia))
      selectInput("IyC08_Ventajas_Familia", "IyC08_Ventajas_Familia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows68 <- renderUI({
    if("IyC08_Ventajas_Tiempo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Tiempo   <- factor(df_ide$IyC08_Ventajas_Tiempo,
                                               levels = c(0,1),
                                               ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Tiempo","IyC08_Ventajas_Tiempo", choices = unique(df_ide$IyC08_Ventajas_Tiempo))
      selectInput("IyC08_Ventajas_Tiempo", "IyC08_Ventajas_Tiempo",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows69 <- renderUI({
    if("IyC08_Ventajas_Tranquilo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Tranquilo    <- factor(df_ide$IyC08_Ventajas_Tranquilo,
                                                   levels = c(0,1),
                                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC03_Tiempo","IyC08_Ventajas_Tranquilo", choices = unique(df_ide$IyC08_Ventajas_Tranquilo))
      selectInput("IyC08_Ventajas_Tranquilo", "IyC08_Ventajas_Tranquilo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows70 <- renderUI({
    if("IyC08_Ventajas_Seguro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Seguro    <- factor(df_ide$IyC08_Ventajas_Seguro,
                                                levels = c(0,1),
                                                ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Seguro","IyC08_Ventajas_Seguro", choices = unique(df_ide$IyC08_Ventajas_Seguro))
      selectInput("IyC08_Ventajas_Seguro", "IyC08_Ventajas_Seguro", choices = NULL,multiple = TRUE)}
  })
  
  output$rows71 <- renderUI({
    if("IyC08_Ventajas_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC08_Ventajas_Otro    <- factor(df_ide$IyC08_Ventajas_Otro,
                                              levels = c(0,1),
                                              ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Otro","IyC08_Ventajas_Otro", choices = unique(df_ide$IyC08_Ventajas_Otro))
      selectInput("IyC08_Ventajas_Otro", "IyC08_Ventajas_Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows72 <- renderUI({
    if("IyC09_Emigrar" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC09_Emigrar","IyC09_Emigrar", choices = unique(df_ide$IyC09_Emigrar))
      selectInput("IyC09_Emigrar", "IyC09_Emigrar", choices = NULL,multiple = TRUE)}
  })
  
  
  output$rows73 <- renderUI({
    if("IyC10_Pertenencia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC10_Pertenencia","IyC10_Pertenencia", choices = unique(df_ide$IyC10_Pertenencia))
      selectInput("IyC10_Pertenencia", "IyC10_Pertenencia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows74 <- renderUI({
    if("IyC11_Frecuencia_Cabecera_Isla" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC11_Frecuencia_Cabecera_Isla","IyC11_Frecuencia_Cabecera_Isla", choices = unique(df_ide$IyC11_Frecuencia_Cabecera_Isla))
      selectInput("IyC11_Frecuencia_Cabecera_Isla", "IyC11_Frecuencia_Cabecera_Isla",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows75 <- renderUI({
    if("IyC12_Motivo_Viaja_Asuntos_Admin" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivo_Viaja_Asuntos_Admin    <- factor(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin,
                                                           levels = c(0,1),
                                                           ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Asuntos_Admin","IyC12_Motivo_Viaja_Asuntos_Admin", choices = unique(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin))
      selectInput("IyC12_Motivo_Viaja_Asuntos_Admin", "IyC12_Motivo_Viaja_Asuntos_Admin", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows76 <- renderUI({
    if("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS    <- factor(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS,
                                                                     levels = c(0,1),
                                                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS","IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS", choices = unique(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS))
      selectInput("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS", "IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows77 <- renderUI({
    if("IyC12_Motivo_Viaja_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivo_Viaja_Trabajo   <- factor(df_ide$IyC12_Motivo_Viaja_Trabajo,
                                                    levels = c(0,1),
                                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Trabajo","IyC12_Motivo_Viaja_Trabajo", choices = unique(df_ide$IyC12_Motivo_Viaja_Trabajo))
      selectInput("IyC12_Motivo_Viaja_Trabajo", "IyC12_Motivo_Viaja_Trabajo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows78 <- renderUI({
    if("IyC12_Motivio_Viaja_Recreacion" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivio_Viaja_Recreacion    <- factor(df_ide$IyC12_Motivio_Viaja_Recreacion,
                                                         levels = c(0,1),
                                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivio_Viaja_Recreacion","IyC12_Motivio_Viaja_Recreacion", choices = unique(df_ide$IyC12_Motivio_Viaja_Recreacion))
      selectInput("IyC12_Motivio_Viaja_Recreacion", "IyC12_Motivio_Viaja_Recreacion", choices = NULL,multiple = TRUE)}
  })
  
  output$rows79 <- renderUI({
    if("IyC12_Motivo_Viaja_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivo_Viaja_Familia    <- factor(df_ide$IyC12_Motivo_Viaja_Familia,
                                                     levels = c(0,1),
                                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Familia","IyC12_Motivo_Viaja_Familia", choices = unique(df_ide$IyC12_Motivo_Viaja_Familia))
      selectInput("IyC12_Motivo_Viaja_Familia", "IyC12_Motivo_Viaja_Familia", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows80 <- renderUI({
    if("IyC12_Motivo_Viaja_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$IyC12_Motivo_Viaja_Otro    <- factor(df_ide$IyC12_Motivo_Viaja_Otro,
                                                  levels = c(0,1),
                                                  ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Otro","IyC12_Motivo_Viaja_Otro", choices = unique(df_ide$IyC12_Motivo_Viaja_Otro))
      selectInput("IyC12_Motivo_Viaja_Otro", "IyC12_Motivo_Viaja_Otro",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows81 <- renderUI({
    if("VI01_La_vivienda_es" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI01_La_vivienda_es","VI01_La_vivienda_es", choices = unique(df_ide$VI01_La_vivienda_es))
      selectInput("VI01_La_vivienda_es", "VI01_La_vivienda_es",choices = NULL, multiple = TRUE)}
  })
  
  output$rows82 <- renderUI({
    if("VI04_Contado" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI04_Contado   <- factor(df_ide$VI04_Contado,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Contado","VI04_Contado", choices = unique(df_ide$VI04_Contado))
      selectInput("VI04_Contado", "VI04_Contado",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows83 <- renderUI({
    if("VI04_Herencia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI04_Herencia    <- factor(df_ide$VI04_Herencia,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Herencia","VI04_Herencia", choices = unique(df_ide$VI04_Herencia))
      selectInput("VI04_Herencia", "VI04_Herencia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows84 <- renderUI({
    if("VI01_Prestada" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI01_Prestada","VI01_Prestada", choices = unique(df_ide$VI01_Prestada))
      selectInput("VI01_Prestada", "VI01_Prestada", choices = NULL,multiple = TRUE)}
  })
  
  output$rows85 <- renderUI({
    if("VI04_Mensual" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI04_Mensual    <- factor(df_ide$VI04_Mensual,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Mensual","VI04_Mensual", choices = unique(df_ide$VI04_Mensual))
      selectInput("VI04_Mensual", "VI04_Mensual", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows86 <- renderUI({
    if("VI05_CPropiaMensual" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI05_CPropiaMensual","VI05_CPropiaMensual", choices = unique(df_ide$VI05_CPropiaMensual))
      selectInput("VI05_CPropiaMensual", "VI05_CPropiaMensual", choices = NULL,multiple = TRUE)}
  })
  
  
  output$rows87 <- renderUI({
    if("VI06_CPropiaAdquirida" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI06_CPropiaAdquirida","VI06_CPropiaAdquirida", choices = unique(df_ide$VI06_CPropiaAdquirida))
      selectInput("VI06_CPropiaAdquirida", "VI06_CPropiaAdquirida",choices = NULL, multiple = TRUE)}
  })
  
  output$rows88 <- renderUI({
    if("VI07_FALTA_DE_SERVICIOS" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI07_FALTA_DE_SERVICIOS","VI07_FALTA_DE_SERVICIOS", choices = unique(df_ide$VI07_FALTA_DE_SERVICIOS))
      selectInput("VI07_FALTA_DE_SERVICIOS", "VI07_FALTA_DE_SERVICIOS",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows89 <- renderUI({
    if("VI08NINUNDACIONES" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI08NINUNDACIONES    <- factor(df_ide$VI08NINUNDACIONES,
                                            levels = c(0,1),
                                            ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI08NINUNDACIONES","VI08NINUNDACIONES", choices = unique(df_ide$VI08NINUNDACIONES))
      selectInput("VI08NINUNDACIONES", "VI08NINUNDACIONES", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows90 <- renderUI({
    if("VI09PeHuracan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI09PeHuracan    <- factor(df_ide$VI09PeHuracan,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI09PeHuracan","VI09PeHuracan", choices = unique(df_ide$VI09PeHuracan))
      selectInput("VI09PeHuracan", "VI09PeHuracan",choices = NULL, multiple = TRUE)}
  })
  
  output$rows91 <- renderUI({
    if("VI14SabeZo0Riesgo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI14SabeZo0Riesgo    <- factor(df_ide$VI14SabeZo0Riesgo,
                                            levels = c(0,1),
                                            ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI14SabeZo0Riesgo","VI14SabeZo0Riesgo", choices = unique(df_ide$VI14SabeZo0Riesgo))
      selectInput("VI14SabeZo0Riesgo", "VI14SabeZo0Riesgo",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows92 <- renderUI({
    if("VI15SabeAfectaciones" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$VI15SabeAfectaciones    <- factor(df_ide$VI15SabeAfectaciones,
                                               levels = c(0,1),
                                               ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI15SabeAfectaciones","VI15SabeAfectaciones", choices = unique(df_ide$VI15SabeAfectaciones))
      selectInput("VI15SabeAfectaciones", "VI15SabeAfectaciones",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows93 <- renderUI({
    if("AE1_AreasVerdes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE1_AreasVerdes   <- factor(df_ide$AE1_AreasVerdes,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE1_AreasVerdes","AE1_AreasVerdes", choices = unique(df_ide$AE1_AreasVerdes))
      selectInput("AE1_AreasVerdes", "AE1_AreasVerdes",choices = NULL, multiple = TRUE)}
  })
  
  output$rows94 <- renderUI({
    if("AE3_Banquetas" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE3_Banquetas    <- factor(df_ide$AE3_Banquetas,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE3_Banquetas","AE3_Banquetas", choices = unique(df_ide$AE3_Banquetas))
      selectInput("AE3_Banquetas", "AE3_Banquetas", choices = NULL,multiple = TRUE)}
  })
  
  output$rows95 <- renderUI({
    if("AE4_Lumi0rias" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE4_Lumi0rias    <- factor(df_ide$AE4_Lumi0rias,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE4_Lumi0rias","AE4_Lumi0rias", choices = unique(df_ide$AE4_Lumi0rias))
      selectInput("AE4_Lumi0rias", "AE4_Lumi0rias", choices = NULL, multiple = TRUE)}
  })
  
  output$rows96 <- renderUI({
    if("AE5_Transporte" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE5_Transporte    <- factor(df_ide$AE5_Transporte,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE5_Transporte","AE5_Transporte", choices = unique(df_ide$AE5_Transporte))
      selectInput("AE5_Transporte", "AE5_Transporte",choices = NULL, multiple = TRUE)}
  })
  
  output$rows97 <- renderUI({
    if("AE6_Patrullas" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE6_Patrullas   <- factor(df_ide$AE6_Patrullas,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE6_Patrullas","AE6_Patrullas", choices = unique(df_ide$AE6_Patrullas))
      selectInput("AE6_Patrullas", "AE6_Patrullas",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows98 <- renderUI({
    if("AE7_Lotes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE7_Lotes    <- factor(df_ide$AE7_Lotes,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE7_Lotes","AE7_Lotes", choices = unique(df_ide$AE7_Lotes))
      selectInput("AE7_Lotes", "AE7_Lotes",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows99 <- renderUI({
    if("AE9_PeSeguridad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE9_PeSeguridad<- factor(df_ide$AE9_PeSeguridad,
                                      levels = c(0,1,2,3,4,5),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE9_PeSeguridad","AE9_PeSeguridad", choices = unique(df_ide$AE9_PeSeguridad))
      selectInput("AE9_PeSeguridad", "AE9_PeSeguridad",choices = NULL, multiple = TRUE)}
  })
  
  output$rows100 <- renderUI({
    if("AE10_PeComodidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE10_PeComodidad<- factor(df_ide$AE10_PeComodidad,
                                       levels = c(0,1,2,3,4,5),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE10_PeComodidad","AE10_PeComodidad", choices = unique(df_ide$AE10_PeComodidad))
      selectInput("AE10_PeComodidad", "AE10_PeComodidad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows101 <- renderUI({
    if("AE11_PeRiesgo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      df_ide$AE11_PeRiesgo<- factor(df_ide$AE11_PeRiesgo,
                                    levels = c(0,1,2,3,4,5),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE11_PeRiesgo","AE11_PeRiesgo", choices = unique(df_ide$AE11_PeRiesgo))
      selectInput("AE11_PeRiesgo", "AE11_PeRiesgo", choices = NULL, multiple = TRUE)}
  }) 

  

}