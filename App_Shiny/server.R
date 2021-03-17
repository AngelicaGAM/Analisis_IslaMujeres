# server library(shinyjs)
library(shinyjs)
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

  

   

  

}