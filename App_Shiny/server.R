# server library(shinyjs)



server <- function(input, output, session) {
  source("librerias.R")
  source("Funciones.R")
  source("graficas.R")
  source("graficarSE.R")
  source("G.R")
  source("PSCUN.r")
  source("PSEjido.r")
  source("PSIsla.r")
  source("vsps.r")
  source("tablas.R")
  xdf  <- read.csv("x.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
  
  
  
  # ---------------------------------------------------------------------
  # HOME
  #cuestionarios
  
  observeEvent(input$popPyM, {
    showModal(modalDialog(
      title = HTML("Población y migración<br>",
                   "I.	DATOS FAMILIARES <br><br>1. ¿Cuántas personas viven en esta casa?<br>Adultos (¿Cuantas personas viven en esta casa? Adultos) <br>Niños (¿Cuantas personas viven en esta casa? Niños)<br><br>  1. A qué niveles de escolaridad asisten los integrantes de esta familia, puede marcar más de una opción<br><br>Jard??n de niños (A qué nivel de escolaridad asisten los integrantes de esta familia Jardin de niños)<br>Primaria (A qué nivel de escolaridad asisten los integrantes de esta familia Primaria)<br>Secundaria (A qué nivel de escolaridad asisten los integrantes de esta familia Secundaria)<br>Bachillerato (A qué nivel de escolaridad asisten los integrantes de esta familia Bachillerato)<br>Licenciatura (A qué nivel de escolaridad asisten los integrantes de esta familia Licenciatura)<br>Posgrado (A qué nivel de escolaridad asisten los integrantes de esta familia Posgrado)<br>2.¿Qué medio de transporte utiliza para ir a la escuela, al trabajo, etc? <br><br>Autobús (¿Qué medios de transporte utilizan para ir a la escuela, al trabajo, etc? Transportes Posibles)<br>Combi, colectivo<br>Taxi<br> Mototaxi<br>Motocicleta<br> Automóvil propio<br>Otro3. ¿Dónde adquiere sus v??veres?<br><br>Mercado de la colonia (¿Dónde compra sus v??veres? Posibles)<br>Tienda de abarrotes, carnicer??as<br>súper mercado<br>Tienda de conveniencia (Oxxo,7eleven, …)<br>Plaza comercial<br> Otro (¿Dónde compra sus v??veres? Otro)   4.¿A dónde acude en caso de urgencia médica? (¿A dónde acude en caso de urgencia médica? Posibles) <br><br>Me atiendo en casa<br>Médico particular en la comunidad<br>Médico particular Cancún<br>Hospital General Cancún<br>Cruza roja<br>Otro (¿A dónde acude en caso de urgencia médica? Otro)<br>5. ¿Con qué áreas de recreo cuenta su colonia? <br><br>Parque (¿Con qué áreas de recreo cuenta su colonia? Posibles)<br>Unidad deportiva<br>Jardines<br>Casa de la cultura<br>Biblioteca<br>Otro<br>II.	IDENTIDAD Y COMUNIDAD<br><br>6.¿Cuál es su lugar de origen?  (¿Cuál es su lugar de origen? Estados del pa??s, ¿Cuál es su lugar de origen? Especifique)<br><br>1 Tabasco<br>2 Chiapas<br>3 Yucatán<br>4 Quintana Roo<br>5 Veracruz<br>6 CDMX<br>7 Otro______________<br><br>7.¿Cuánto tiempo lleva viviendo en este lugar?  (¿Cuánto tiempo lleva viviendo en este lugar? Opciones)<br>1 Recién llegué<br>2 Un año<br>3 Dos años<br>4 Más de dos años<br>5 Toda mi vida<br>8.Razones por las que llegó a vivir a la localidad)<br><br>1 parientes (¿Qué lo motivó a venir a vivir esta la localidad Tengo Parientes,)<br>2 amigos  (¿Qué lo motivó a venir a vivir esta la localidad Tengo amigos)<br>3 trabajo (¿Qué lo motivó a venir a vivir esta la localidad Por trabajo)<br>4 negocio  (¿Qué lo motivó a venir a vivir esta la localidad Por poner negocio,)<br>5 vivir mejor (¿Qué lo motivó a venir a vivir esta la localidad Por oportunidad de vivir mejor)<br>5 otro Especifique (¿Qué lo motivó a venir a vivir esta la localidad Otro)<br>9.En caso de requerir ayuda, apoyo legal, económico o familiar ante algún problema acudo a: <br><br>1 Grupo de vecinos (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Grupo de vecinos)<br>2 Familiares  (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Familiares)<br>3 A la autoridad (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a A la autoridad)<br>      4 La iglesia o templo (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a La iglesia o templo)<br>5 otro Especifique (En caso de requerir ayuda, apoyo legal, económico o familiar ante algún, problema acudo a Otro)<br>10.¿Qué religión practica? (¿Qué religión practica? Opciones)<br><br>1 Ninguna<br>2 Católica<br>3 Evangélica<br>4 Testigos de Jehová<br>5 Adventista<br>6 Mormón<br>7 Otra (¿Qué religión practica? Otro)<br>11.¿Cuáles son sus principales costumbres o tradiciones? (¿Cuáles son sus principales costumbres o tradiciones? Opciones)<br><br>1 Fiestas en honor a la Virgen de Fátima<br>2 Fiestas patronales<br>3 Festival de la cultura del Caribe<br>4 Carnaval <br>5 Fin de año<br>6 D??a de muertos<br>7 Otro (¿Cuáles son sus principales costumbres o tradiciones? Otro<br>12.¿Cuáles son las ventajas de vivir en esta comunidad?<br><br>1 Tengo casa (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo casa)<br>2 Trabajo (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo trabajo)<br>3 Familia (¿Cuáles son las ventajas de vivir en esta comunidad? Tengo familia)<br>4 Tengo más tiempo (¿Cuáles son las ventajas de viv en esta comunidad? Tengo mas tiempo)<br>5 Es tranquilo (¿Cuáles son las ventajas de vivir ir  en esta comunidad? Es tranquilo)<br>6 Es seguro (¿Cuáles son las ventajas de vivir en esta comunidad? Es seguro)<br>7 Otro (¿Cuáles son las ventajas de vivir en esta comunidad? Otro)<br>13. Piensa irse a vivir a otra localidad (Piensa irse a vivir a otra localidad Opciones)<br><br>1 No <br>2 Quizás <br>3 S??, en el mismo estado <br>4 S??, me regreso a mi lugar de origen <br>5 S??, al extranjero<br>14.¿Con qué municipio de identifica más? (¿Usted a qué municipio siente que pertenece? Opciones)<br><br>1 Benito Juárez<br>2 Isla Mujeres<br><br>3 Ninguno<br>15.¿Qué tan frecuente va a la isla, la cabecera municipal de Isla Mujeres? (¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres? Opciones)<br><br>1 Nunca <br>2 A veces [varias veces al año] <br>3 Frecuentemente [al menos una vez al mes]<br>4 casi Siempre [más de una vez al mes]<br>5 Siempre [una vez por semana]<br><br>16.¿Cuales son los motivos por los que viaja a isla? <br><br>1 Asuntos administrativos (¿Cuales son los motivos por los que viaja a isla? Asuntos administrativos)<br>2 Pagos de servicios (¿Cuales son los motivos por los que viaja a isla? Pagos de servicios)<br>3 Trabajo (¿Cuales son los motivos por los que viaja a isla? Trabajo)<br> 4 Recreación (¿Cuales son los motivos por los que viaja a isla? Recreación)<br> 5 Visita de la familia (¿Cuales son los motivos por los que viaja a isla? Visita de la familia)<br>6 Otros (¿Cuales son los motivos por los que viaja a isla? Otro)<br>IV.	VIVIENDA<br><br>La vivienda que habita tu familia es (La vivienda que habita tu familia es Opciones)<br>1 Propia.   <br>2 Prestada por (La vivienda que habita tu familia es Prestado por)<br>3 Rentada y se  paga mensual (La vivienda que habita tu familia es Renta mensual)<br> 4 Asentamiento irregular o informal <br>5 Otro (La vivienda que habita tu familia es Otro)<br>¿Ha recibido la visita del municipio para ofrecerle reubicarlo en otro lugar?  (¿Ha recibido la visita del municipio para ofrecerle reubicarlo en otro lugar? Si o no)<br>  •	Sólo si responde Asentamiento irregular o informal<br><br> ¿Le indicaron a qué sitio los reubicar??an? (¿Le indicaron a qué sitio los reubicar??an? Si o no)<br>•	Sólo si responde Asentamiento irregular o informal<br><br>En caso  de ser  propia, ¿De qué forma fue adquirida? (En caso de ser vivienda propia ¿De que forma fue adquirida? Opciones)<br>1 Herencia   <br>2 Pago de contado<br>3  Pagándola:    <br><br>Monto mensual (En caso de ser vivienda propia ¿De que forma fue adquirida? Monto mensual)<br>¿A quién? (En caso de ser vivienda propia ¿De que forma fue adquirida? A quien)<br>4  No es propia<br><br>¿Con qué servicios cuenta su vivienda? <br>1 televisión de paga (cable o satelital) <br>2 internet <br>3 servicio telefónico celular <br>4  energ??a eléctrica <br>5 agua <br><br>¿Con qué servicios cuenta su vivienda? (¿Con qué servicios cuenta su vivienda?)<br>1 televisión de paga (cable o satelital) <br>2 internet <br>3 servicio telefónico celular <br>4 energ??a eléctrica <br>5 agua <br><br>¿Cuántas inundaciones ha sufrido al vivir en esta localidad? (¿Cuántas inundaciones ha sufrido al vivir en esta localidad?)<br><br>¿En caso de haber sufrido una inundación qué hizo al respecto? (En caso de haber sufrido una inundación qué hizo al respecto Opciones) <br>•	Sólo si ha sufrido inundaciones<br>1 Dejé mi casa y fui al refugio<br>2 Dejé mi casa y fui a la casa de un familiar o amigo<br>3 Aqu?? nos quedamos<br>4 Vinieron las autoridades a desalojar pero decidimos quedarnos<br>5 Otro (En caso de haber sufrido una inundación qué hizo al respecto Otro)<br><br>A pesar de la inundación usted decidió quedarse a vivir aqu?? por (A pesar de la inundación usted decidió quedarse a vivir aqu?? por  Opciones)<br>•	Sólo si ha sufrido inundaciones<br>1 No tengo alternativa de vivienda    <br>2 Me agrada el lugar<br>3 Estoy esperando un crédito y mientras aqu?? me quedo<br>4 La ubicación del trabajo y escuela  nos queda más cerca<br>5 No dispongo de un trabajo que me permita acceder a un crédito para vivienda<br>6 Otro (A pesar de la inundación usted decidió quedarse a vivir aqu?? por  Opciones)<br><br>¿Sabe si su vivienda está en zona de riesgo? (¿Sabe si su vivienda está en zona de riesgo?)<br><br>¿Sabe si su vivienda está en zona de riesgo? ¿Cuales? (¿Sabe si su vivienda está en zona de riesgo? ¿Cuales?) <br><br>•	Sólo si responde s?? a la pregunta: ¿Sabe si su vivienda está en zona de riesgo?<br>¿Conoce los impactos o afectaciones que puede sufrir? (¿Conoce los impactos o afectaciones que puede sufrir?) <br><br>¿Conoce los impactos o afectaciones que puede sufrir? (¿Conoce los impactos o afectaciones que puede sufrir? ¿Cuales?) <br><br>•	Sólo si responde S?? a la pregunta ¿Conoce los impactos o afectaciones que puede sufrir?<br>V.	APRECIACIONES DEL ENCUESTADOR<br><br>Favor de señalar los siguientes servicios observados alrededor de la vivienda <br><br>1 Áreas verdes (favor de señalar los siguientes servicios observados alrededor de la vivienda áreas verdes)<br>2 Calles pavimentadas (favor de señalar los siguientes servicios observados alrededor de la vivienda calles)<br>3 Banquetas (favor de señalar los siguientes servicios observados alrededor de la vivienda banquetas)<br>4 Luminarias públicas (favor de señalar los siguientes servicios observados alrededor de la vivienda luminarias)<br>5 Transporte público (favor de señalar los siguientes servicios observados alrededor de la vivienda transporte)<br>6 Patrullas vigilando (favor de señalar los siguientes servicios observados alrededor de la vivienda patrullas)<br>7 Lotes bald??os (favor de señalar los siguientes servicios observados alrededor de la vivienda lotes baldios)<br>¿Que percepción tuvo de la vivienda y la calle?<br><br>1 Seguridad  (¿Que percepción tuvo de la vivienda y la calle? Percepcion seguridad)<br>2 Comodidad (¿Que percepción tuvo de la vivienda y la calle? Percepcion comodidad)<br>3 Riesgo personal o natural (¿Que percepción tuvo de la vivienda y la calle? Percepcion riesgo personal o natural)  <br>"),
      easyClose = TRUE
    ))
  })
  
  observeEvent( input$poSyA, {
    showModal(modalDialog(
      title = HTML("Diagnóstico socio económico y ambiental<br>",
                   "Social <br><br>1. Entre los vecinos, realizan alguna actividad en común:  fiestas, reuniones vecinales, levantar quejas etc <br>2. ¿Cómo es la relación con sus vecinos? <br>3. ¿Ha tenido problemas con sus vecinos: pleitos, demandas…? <br>4. ¿Con qué frecuencia se hacen favores entre vecinos? <br>5. ¿En algún problema que se le presente, sus vecinos le ayudan? <br>6. ¿Pertenecen a alguna organización? <br><br>Economico <br><br>1.  ¿Cuántas personas de esta familia trabajan? <br>2.  ¿En qué trabajan? <br>3. ¿Realizan alguna actividad productiva por su cuenta: manualidad, artesan??a u oficio, cultivo de hortalizas?<br>4. ¿Intercambian productos con sus vecinos? <br>5. ¿Se ayudan entre vecinos para algún trabajo que beneficie la econom??a familiar dentro de sus casas?<br><br>Ambiental <br><br>1. ¿Qué uso le dan sus vecinos a la salina? <br>2.¿Qué beneficio recibe de vivir aqu???<br>3.¿Qué desventajas recibe de vivir aqu?? cerca de la salina? <br>4.¿En qué condiciones considera que se encuentra la salina? <br>5.¿Qué efectos genera la condición (sucia o contaminada) de la Salina? <br>6.¿Han llevado a cabo alguna actividad de limpieza, saneamiento o conservación de la Salina?<br>7.¿Están conectados al drenaje?<br>8.¿Por qué decidieron vivir aqu???<br>"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$popC, {
    showModal(modalDialog(
      title = HTML("Percepcion de seguridad<br>",
                   "1. En esta calle o zona, Usted participa: <br><br>En eventos deportivos<br>En tandas<br>En fiestas<br>En iglesia o templo<br>En otras actividades<br>Para solucionar problemas de la comunidad<br>2. Usted conoce a sus vecinos: <br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Les confiar??a a los niños<br>Les confiar??a su casa<br>Participa con ellos para mejorar la seguridad<br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesar??a participar<br>D??a en que podr??a participar en actividades con su comunidad. <br>Horario en que podr??a participar: Mañana Tarde Noche<br>3. Participa con la autoridad para mejorar la seguridad:<br><br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesar??a participar<br>4. D??a en que podr??a participar en actividades con la autoridad<br><br>5. Horario en el que podr??a participar Mañana Tarde Noche<br><br>6. Cuando hay un delito, en esta calle o zona los vecinos:<br><br>Se reúnen<br>Se organizan para vigilar<br>Intercambian números telefónicos<br>Forman un chat<br>Ponen letreros de advertencia<br>Llaman a la polic??a<br>Denuncian ante la autoridad<br>Vigilan<br>Buscan desquitarse<br>No hacen nada<br>7. Durante el último año, en esta calle o zona ha habido:<br><br>Robo en casa<br>Robo en la calle<br>Robo en transporte<br>Robo en negocio<br>Robo de partes de auto<br>Robo de veh??culo<br>Balaceras<br>Cobro de piso<br>Violencia familiar<br>Peleas de gallos o perros<br>8. Valore el riesgo de sufrir un delito en alguno de los siguientes<br><br>lugares:<br>En su casa<br>En esta calle<br>En esta zona<br>En esta ciudad<br>9. ¿Usted ha sido v??ctima de algún delito en el último año? si No<br><br>10. En caso de ser v??ctima del delito Usted:<br><br>Llama a la polic??a<br>Hace una denuncia<br>Advierte a sus vecinos del peligro<br>Advierte a su familia del peligro<br>Busca desquitarse<br>Amenazas<br>11. Ha sido v??ctima de algún delito y no denuncio:<br><br>¿Podr??a señalar las razones por las que no denunció?<br>Respuesta “Si”, desplegar el siguiente reactivo:<br>Falta de pruebas<br>Considera que es un delito de poca importancia<br>Conoce al agresor o agresores<br>Desconf??a de las autoridades<br>Teme a que lo extorsionen<br>Falta de tiempo<br>Son trámites complicados<br>No sabe dónde denunciar<br>Aunque denuncie no va a pasar nada<br>12. En esta calle o zona:<br><br>Los padres participan en actividades con hijos<br>Los vecinos se organizan para prevenir delitos<br>Las personas son amables<br>Hay alguna persona que siempre ayuda a los demás<br>13.En esta calle o zona hay personas:<br><br>A las que todos tienen miedo<br>Que acosan a menores<br>Que acosan a mujeres<br>Que se emborrachan o se drogan<br>Que han estado en la cárcel<br>Sospechosas<br>14. En esta calle o zona hay violencia:<br><br>Entre mujeres<br>Entre hombres<br>Entre familias<br>Entre adultos y jóvenes<br>Entre jóvenes<br>15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:<br><br>Amistosamente<br>Dialogando<br>De manera respetuosa<br>A gritos<br>Con golpes<br>Con cuchillos, navajas o machetes<br>Con armas de fuego, como pistolas o rifles<br>Desquitándose del otro<br>16. En esta calle o zona hay niños o adolescentes que se quedan<br><br>encerrados con llave:<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Por la inseguridad<br>Descuido de los padres<br>Castigo<br>Trabajo de los padres<br>17. En esta calle o zona hay niños que se quedan la mayor parte del d??a sin comer: Respuesta “Si”, desplegar los siguientes reactivos:<br><br>Por descuido de los padres<br>Castigo<br>Falta de dinero<br>Trabajo de los padres<br>18. En esta calle o zona hay jóvenes que:<br><br>Hacen deporte<br>Ayudan a los demás<br>La mayor??a de los jóvenes estudian o trabajan.<br>Andan en pandillas<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Andan armados<br>Destruyen o vandalizan la propiedad ajena<br>Son violentos<br>Amenazan a los vecinos<br>19. En esta calle o zona hay un parque<br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Está en buen estado<br>¿Quiénes lo utilizan?: Mostrar menú abajo<br>Niños y niñas<br>Jóvenes<br>Pandillas<br>Familias<br>Adultos<br>Personas de la tercera edad<br>Hay actividades supervisadas por adultos<br>Vándalos<br>Usted lo utiliza<br>20. En esta calle o zona hay:<br><br>Banquetas<br>Baches<br>Letreros con nombres de las calles<br>Tiendita<br>Alumbrado<br>Consumo de alcohol en la calle<br>21. En esta zona hay:<br><br>Horarios de transporte que convienen<br>Una parada de camión cerca de esta casa<br>Terrenos bald??os<br>Basura<br>Autos abandonados<br>Casas abandonadas<br>Vandalismo<br>Grafiti<br>Venta de tiner o pegamento a menores<br>Venta de alcohol o cigarros a menores<br>Venta de droga<br>Venta de alcohol después de las 11:00 de la noche<br>22. En el último año Usted supo que algún menor de 18 años:<br><br>Se fue de la casa<br>Sufrió violencia<br>Abandonó la escuela<br>Tiene problemas de conducta<br>Quedó embarazada<br>23. Para corregir a un niño o niña que se porta mal, Usted recomienda:<br><br>Castigarle<br>Gritarle<br>Darle nalgadas<br>Darle una golpiza / cueriza <br>Explicarle lo que está mal<br>Aconsejarle<br>Darle buen ejemplo24. En esta casa: (APLICA TARJETON)<br>Todos se conocen<br>Platican unos con otros<br>Comen juntos<br>Se ayudan con los gastos<br>Discuten<br>Se gritan entre s??<br>Llegan a los golpes<br>Se ignoran<br>En esta casa alguien: (APLICA TARJETON)<br>Tiene discapacidad SI LA RESPUESTA ES SI, ENTONCES:<br>Por su discapacidad, ha vivido violencia<br>Sabe manejar armas de fuego, como pistolas o rifles<br>Habla de comprar armas de fuego<br>Habla lengua indigena<br>Necesita ayuda por obesidad<br>Necesita ayuda por fumar<br>Necesita ayuda por beber<br>Necesita ayuda por drogas<br>26. En el último año, por cuestiones de seguridad Usted ha<br>pensado:<br>Cambiarse de casa<br>Cambiarse de ciudad<br>Cambiarse de estado<br>Cambiar de trabajo N/A<br>Cerrar su negocio N/A<br>Cambiar a los hijos de escuela N/ASeguir como hasta ahora<br>27. En el último año, por cuestiones de seguridad Usted dejó de:Dejo de salir de noche<br><br>Dejo de salir a caminar o hacer ejercicio<br>Impidio que los niños salgan a la calle N/A<br>Evito relacionarse con nuevas personas<br>Dejo de visitar a parientes o amigos<br>Dejo de usar transporte público /combi N/A<br>Dejo de usar taxi<br>Dejo de llevar mucho dinero en efectivo<br>Dejo de usar joyas<br>28. En esta calle o zona la polic??a:<br>Cuida o vigila bien<br>Comete abusos<br>Acude a los llamados<br>Pide mordidas<br>Hace rondines<br>Comete delitos<br>"),
      easyClose = TRUE
    ))
  })
  
  
  # ---------------------------------------------------------------------
  # GRAFICAS 
  
  output$tableGraficas=renderLeaflet({
          #  Caracteristicas de poblacion y migracion


          #           Familiares

                  if(input$pregunta=="PFAM01"){TFAM01}
            else if(input$pregunta=="PFAM02"){TFAM02}
            else if(input$pregunta=="PFAM1"){TFAM1}
            else if(input$pregunta=="PFAM2"){TFAM2}
            else if(input$pregunta=="PFAM3"){TFAM3}
            else if(input$pregunta=="PFAM4"){TFAM4}
          #          Economicos
            else if(input$pregunta=="PPECO1"){TECO1}
            else if(input$pregunta=="PPECO2"){TECO2}
            else if(input$pregunta=="PPECO3"){TECO3}
            else if(input$pregunta=="PPECO4"){TECO4}
            else if(input$pregunta=="PPECO5"){TECO5}
            else if(input$pregunta=="PPECO6"){TECO6}
            else if(input$pregunta=="PPECO7"){TECO7}
            else if(input$pregunta=="PPECO8"){TECO8}

          #       Identidad  y comunidad
            else if(input$pregunta=="PPIyC1"){TIyC1}
            else if(input$pregunta=="PPIyC2"){TIyC2}
            else if(input$pregunta=="PPIyC3"){TIyC3}
            else if(input$pregunta=="PPIyC4"){TIyC4}
            else if(input$pregunta=="PPIyC5"){TIyC5}
            else if(input$pregunta=="PPIyC6"){TIyC6}
            else if(input$pregunta=="PPIyC7"){TIyC7}
            else if(input$pregunta=="PPIyC8"){TIyC8}
            else if(input$pregunta=="PPIyC9"){TIyC9}
            else if(input$pregunta=="PPIyC10"){TIyC10}


            #       vivienda
            else if(input$pregunta=="V1P1R1"){T1P1R1}
            else if(input$pregunta=="V1P2R1"){T1P2R1}
            else if(input$pregunta=="V1P3R1"){T1P3R1}
            else if(input$pregunta=="V1P4R1"){T1P4R1}
            else if(input$pregunta=="V1P5R1"){T1P5R1}
            else if(input$pregunta=="V1P6R1"){T1P6R1}
            else if(input$pregunta=="V1P7R1"){T1P7R1}
            else if(input$pregunta=="V1P8R1"){T1P8R1}

            
            else if(input$pregunta=="PPCC1"){TCC1}
            else if(input$pregunta=="PPCC2"){TCC2}
            else if(input$pregunta=="PPCC3"){TCC3}
            else if(input$pregunta=="PPCC4"){TCC4}
            
            
            
            #            Salinas
            else if(input$pregunta=="PAMB1"){TAMB1}
            else if(input$pregunta=="PAMB2"){TAMB2}
            else if(input$pregunta=="PAMB3"){TAMB3}
            else if(input$pregunta=="PAMB4"){TAMB4}
            else if(input$pregunta=="PAMB5"){TAMB5}
            else if(input$pregunta=="PAMB6"){TAMB6}
            else if(input$pregunta=="PAMB7"){TAMB7}

            else if(input$pregunta=="SOC1"){TSOC1}
            else if(input$pregunta=="SOC2"){TSOC2}
            else if(input$pregunta=="SOC3"){TSOC3}
            else if(input$pregunta=="SOC4"){TSOC4}
            else if(input$pregunta=="SOC5"){TSOC5}
            else if(input$pregunta=="SOC6"){TSOC6}

            else if(input$pregunta=="SOC6"){TSOC6}
            else if(input$pregunta=="ECO1"){TEC1}
            else if(input$pregunta=="ECO2"){TEC2}
            else if(input$pregunta=="ECO3"){TEC3}
            else if(input$pregunta=="ECO4"){TEC4}
            else if(input$pregunta=="ECO5"){TEC5}
            
   #          percepcion de seguridad  Isla
             else if(input$pregunta=="GPSI1"){TPSI1}
             else if(input$pregunta=="GPSI2"){TPSI2}
             else if(input$pregunta=="GPSI3"){TPSI3}
             else if(input$pregunta=="GPSI4"){TPSI4}
             else if(input$pregunta=="GPSI5"){TPSI5}
             else if(input$pregunta=="GPSI6"){TPSI6}
             else if(input$pregunta=="GPSI7"){TPSI7}
             else if(input$pregunta=="GPSI81"){TPSI81}
             else if(input$pregunta=="GPSI82"){TPSI82}
             else if(input$pregunta=="GPSI83"){TPSI83}
             else if(input$pregunta=="GPSI84"){TPSI84}
             else if(input$pregunta=="GPSI9"){TPSI9}
             else if(input$pregunta=="GPSI10"){TPSI10}
             else if(input$pregunta=="GPSI11"){TPSI11}
             else if(input$pregunta=="GPSI12"){TPSI12}
             else if(input$pregunta=="GPSI13"){TPSI13}
             else if(input$pregunta=="GPSI14"){TPSI14}
             else if(input$pregunta=="GPSI15"){TPSI15}
             else if(input$pregunta=="GPSI16"){TPSI16}
             else if(input$pregunta=="GPSI17"){TPSI17}
             else if(input$pregunta=="GPSI18"){TPSI18}
             else if(input$pregunta=="GPSI19"){TPSI19}
             else if(input$pregunta=="GPSI20"){TPSI20}
             else if(input$pregunta=="GPSI21"){TPSI21}
             else if(input$pregunta=="GPSI22"){TPSI22}
             else if(input$pregunta=="GPSI23"){TPSI23}

             else if(input$pregunta=="GPSI25"){TPSI25}
             else if(input$pregunta=="GPSI26"){TPSI26}
             else if(input$pregunta=="GPSI27"){TPSI27}
             else if(input$pregunta=="GPSI28"){TPSI28}
             else if(input$pregunta=="GPSI291"){TPSI291}
             else if(input$pregunta=="GPSI292"){TPSI292}
             else if(input$pregunta=="GPSI293"){TPSI293}
             else if(input$pregunta=="GPSI294"){TPSI294}
             else if(input$pregunta=="GPSI295"){TPSI295}
             else if(input$pregunta=="GPSI296"){TPSI296}
             else if(input$pregunta=="GPSI301"){TPSI301}
             else if(input$pregunta=="GPSI302"){TPSI302}
             else if(input$pregunta=="GPSI303"){TPSI303}
             else if(input$pregunta=="GPSI304"){TPSI304}
             else if(input$pregunta=="GPSI305"){TPSI305}
             else if(input$pregunta=="GPSI306"){TPSI306}
             else if(input$pregunta=="GPSI311"){TPSI311}
             else if(input$pregunta=="GPSI312"){TPSI312}
             else if(input$pregunta=="GPSI313"){TPSI313}
             else if(input$pregunta=="GPSI314"){TPSI314}
             else if(input$pregunta=="GPSI315"){TPSI315}
             else if(input$pregunta=="GPSI316"){TPSI316}

               #          percepcion de seguridad  Cancun
             else if(input$pregunta=="GPSC1"){TPSC1}
             else if(input$pregunta=="GPSC2"){TPSC2}
             else if(input$pregunta=="GPSC3"){TPSC3}
             else if(input$pregunta=="GPSC4"){TPSC4}
             else if(input$pregunta=="GPSC5"){TPSC5}
             else if(input$pregunta=="GPSC6"){TPSC6}
             else if(input$pregunta=="GPSC7"){TPSC7}
             else if(input$pregunta=="GPSC81"){TPSC81}
             else if(input$pregunta=="GPSC82"){TPSC82}
             else if(input$pregunta=="GPSC83"){TPSC83}
             else if(input$pregunta=="GPSC84"){TPSC84}
             else if(input$pregunta=="GPSC9"){TPSC9}
             else if(input$pregunta=="GPSC10"){TPSC10}
             else if(input$pregunta=="GPSC11"){TPSC11}
             else if(input$pregunta=="GPSC12"){TPSC12}
             else if(input$pregunta=="GPSC13"){TPSC13}
             else if(input$pregunta=="GPSC14"){TPSC14}
             else if(input$pregunta=="GPSC15"){TPSC15}
             else if(input$pregunta=="GPSC16"){TPSC16}
             else if(input$pregunta=="GPSC17"){TPSC17}
             else if(input$pregunta=="GPSC18"){TPSC18}
             else if(input$pregunta=="GPSC19"){TPSC19}
             else if(input$pregunta=="GPSC20"){TPSC20}
             else if(input$pregunta=="GPSC21"){TPSC21}
             else if(input$pregunta=="GPSC22"){TPSC22}
             else if(input$pregunta=="GPSC23"){TPSC23}

             else if(input$pregunta=="GPSC25"){TPSC25}
             else if(input$pregunta=="GPSC26"){TPSC26}
             else if(input$pregunta=="GPSC27"){TPSC27}
             else if(input$pregunta=="GPSC28"){TPSC28}
             else if(input$pregunta=="GPSC291"){TPSC291}
             else if(input$pregunta=="GPSC292"){TPSC292}
             else if(input$pregunta=="GPSC293"){TPSC293}
             else if(input$pregunta=="GPSC294"){TPSC294}
             else if(input$pregunta=="GPSC295"){TPSC295}
             else if(input$pregunta=="GPSC296"){TPSC296}
             else if(input$pregunta=="GPSC301"){TPSC301}
             else if(input$pregunta=="GPSC302"){TPSC302}
             else if(input$pregunta=="GPSC303"){TPSC303}
             else if(input$pregunta=="GPSC304"){TPSC304}
             else if(input$pregunta=="GPSC305"){TPSC305}
             else if(input$pregunta=="GPSC306"){TPSC306}
             else if(input$pregunta=="GPSC311"){TPSC311}
             else if(input$pregunta=="GPSC312"){TPSC312}
             else if(input$pregunta=="GPSC313"){TPSC313}
             else if(input$pregunta=="GPSC314"){TPSC314}
             else if(input$pregunta=="GPSC315"){TPSC315}
             else if(input$pregunta=="GPSC316"){TPSC316}


               #          percepcion de seguridad  Cancun
             else if(input$pregunta=="GPSE1"){TPSE1}
             else if(input$pregunta=="GPSE2"){TPSE2}
             else if(input$pregunta=="GPSE3"){TPSE3}
             else if(input$pregunta=="GPSE4"){TPSE4}
             else if(input$pregunta=="GPSE5"){TPSE5}
             else if(input$pregunta=="GPSE6"){TPSE6}
             else if(input$pregunta=="GPSE7"){TPSE7}
             else if(input$pregunta=="GPSE81"){TPSE81}
             else if(input$pregunta=="GPSE82"){TPSE82}
             else if(input$pregunta=="GPSE83"){TPSE83}
             else if(input$pregunta=="GPSE84"){TPSE84}
             else if(input$pregunta=="GPSE9"){TPSE9}
             else if(input$pregunta=="GPSE10"){TPSE10}
             else if(input$pregunta=="GPSE11"){TPSE11}
             else if(input$pregunta=="GPSE12"){TPSE12}
             else if(input$pregunta=="GPSE13"){TPSE13}
             else if(input$pregunta=="GPSE14"){TPSE14}
             else if(input$pregunta=="GPSE15"){TPSE15}
             else if(input$pregunta=="GPSE16"){TPSE16}
             else if(input$pregunta=="GPSE17"){TPSE17}
             else if(input$pregunta=="GPSE18"){TPSE18}
             else if(input$pregunta=="GPSE19"){TPSE19}
             else if(input$pregunta=="GPSE20"){TPSE20}
             else if(input$pregunta=="GPSE21"){TPSE21}
             else if(input$pregunta=="GPSE22"){TPSE22}
             else if(input$pregunta=="GPSE23"){TPSE23}

             else if(input$pregunta=="GPSE25"){TPSE25}
             else if(input$pregunta=="GPSE26"){TPSE26}
             else if(input$pregunta=="GPSE27"){TPSE27}
             else if(input$pregunta=="GPSE28"){TPSE28}
             else if(input$pregunta=="GPSE291"){TPSE291}
             else if(input$pregunta=="GPSE292"){TPSE292}
             else if(input$pregunta=="GPSE293"){TPSE293}
             else if(input$pregunta=="GPSE294"){TPSE294}
             else if(input$pregunta=="GPSE295"){TPSE295}
             else if(input$pregunta=="GPSE296"){TPSE296}
             else if(input$pregunta=="GPSE301"){TPSE301}
             else if(input$pregunta=="GPSE302"){TPSE302}
             else if(input$pregunta=="GPSE303"){TPSE303}
             else if(input$pregunta=="GPSE304"){TPSE304}
             else if(input$pregunta=="GPSE305"){TPSE305}
             else if(input$pregunta=="GPSE306"){TPSE306}
             else if(input$pregunta=="GPSE311"){TPSE311}
             else if(input$pregunta=="GPSE312"){TPSE312}
             else if(input$pregunta=="GPSE313"){TPSE313}
             else if(input$pregunta=="GPSE314"){TPSE314}
             else if(input$pregunta=="GPSE315"){TPSE315}
             else if(input$pregunta=="GPSE316"){TPSE316}
 
    
  })
  
  # Visualizacion principal 
  output$plot1=renderLeaflet({

        # Caracteristicas de poblacion y migracion
        #             Familiares
            if(input$pregunta=="PFAM01"){PFAM01}
        else if(input$pregunta=="PFAM02"){PFAM02}
        else if(input$pregunta=="PFAM1"){PFAM1}
        else if(input$pregunta=="PFAM2"){PFAM2}
        else if(input$pregunta=="PFAM3"){PFAM3}
        else if(input$pregunta=="PFAM4"){PFAM4}
        # Economicos
        else if(input$pregunta=="PPECO1"){PECO1}
        else if(input$pregunta=="PPECO2"){PECO2}
        else if(input$pregunta=="PPECO3"){PECO3}
        else if(input$pregunta=="PPECO4"){PECO4}
        else if(input$pregunta=="PPECO5"){PECO5}
        else if(input$pregunta=="PPECO6"){PECO6}
        else if(input$pregunta=="PPECO7"){PPECO7}
        else if(input$pregunta=="PPECO8"){PPECO8}
        #Identidad  y comunidad
        else if(input$pregunta=="PPIyC1"){PIyC1}
        else if(input$pregunta=="PPIyC2"){PIyC2}
        else if(input$pregunta=="PPIyC3"){PIyC3}
        else if(input$pregunta=="PPIyC4"){PIyC4}
        else if(input$pregunta=="PPIyC5"){PIyC5}
        else if(input$pregunta=="PPIyC6"){PIyC6}
        else if(input$pregunta=="PPIyC7"){PIyC7}
        else if(input$pregunta=="PPIyC8"){PIyC8}
        else if(input$pregunta=="PPIyC9"){PIyC9}
        else if(input$pregunta=="PPIyC10"){PIyC10}
         
        #       vivienda
        else if(input$pregunta=="V1P1R1"){V1P1R1}
        else if(input$pregunta=="V1P2R1"){V1P2R1}
        else if(input$pregunta=="V1P3R1"){V1P3R1}
        else if(input$pregunta=="V1P4R1"){V1P4R1}
        else if(input$pregunta=="V1P5R1"){V1P5R1}
        else if(input$pregunta=="V1P6R1"){V1P6R1}
        else if(input$pregunta=="V1P7R1"){V1P7R1}
        else if(input$pregunta=="V1P8R1"){V1P8R1}
        
        else if(input$pregunta=="PPCC1"){PCC1}
        else if(input$pregunta=="PPCC2"){PCC2}
        else if(input$pregunta=="PPCC3"){PCC3}
        else if(input$pregunta=="PPCC4"){PCC4}
        
        
        
        #------------- SALINAS --------------
        #             Ambiental 
        else if(input$pregunta=="PAMB1"){AMB1}
        else if(input$pregunta=="PAMB2"){AMB2}
        else if(input$pregunta=="PAMB3"){AMB3}
        else if(input$pregunta=="PAMB4"){AMB4}
        else if(input$pregunta=="PAMB5"){AMB5}
        else if(input$pregunta=="PAMB6"){AMB6}
        else if(input$pregunta=="PAMB7"){AMB7}
    #             Social 
        else if(input$pregunta=="SOC1"){SOC1}
        else if(input$pregunta=="SOC2"){SOC2}
        else if(input$pregunta=="SOC3"){SOC3}
        else if(input$pregunta=="SOC4"){SOC4}
        else if(input$pregunta=="SOC5"){SOC5}
        else if(input$pregunta=="SOC6"){SOC6}
    #             Economico
        else if(input$pregunta=="ECO1"){EC1}
        else if(input$pregunta=="ECO2"){EC2}
        else if(input$pregunta=="ECO3"){EC3}
        else if(input$pregunta=="ECO4"){EC4}
        else if(input$pregunta=="ECO5"){EC5}

  

  #percepcion de seguridad 
             else if(input$pregunta=="GPSI1"){GPSI1}
             else if(input$pregunta=="GPSI2"){GPSI2}
             else if(input$pregunta=="GPSI3"){GPSI3}
             else if(input$pregunta=="GPSI4"){GPSI4}
             else if(input$pregunta=="GPSI5"){GPSI5}
             else if(input$pregunta=="GPSI6"){GPSI6}
             else if(input$pregunta=="GPSI7"){GPSI7}
             else if(input$pregunta=="GPSI81"){GPSI81}
             else if(input$pregunta=="GPSI82"){GPSI82}
             else if(input$pregunta=="GPSI83"){GPSI83}
             else if(input$pregunta=="GPSI84"){GPSI84}
             else if(input$pregunta=="GPSI9"){GPSI9}
             else if(input$pregunta=="GPSI10"){GPSI10}
             else if(input$pregunta=="GPSI11"){GPSI11}
             else if(input$pregunta=="GPSI12"){GPSI12}
             else if(input$pregunta=="GPSI13"){GPSI13}
             else if(input$pregunta=="GPSI14"){GPSI14}
             else if(input$pregunta=="GPSI15"){GPSI15}
             else if(input$pregunta=="GPSI16"){GPSI16}
             else if(input$pregunta=="GPSI17"){GPSI17}
             else if(input$pregunta=="GPSI18"){GPSI18}
             else if(input$pregunta=="GPSI19"){GPSI19}
             else if(input$pregunta=="GPSI20"){GPSI20}
             else if(input$pregunta=="GPSI21"){GPSI21}
             else if(input$pregunta=="GPSI22"){GPSI22}
             else if(input$pregunta=="GPSI23"){GPSI23}

             else if(input$pregunta=="GPSI25"){GPSI25}
             else if(input$pregunta=="GPSI26"){GPSI26}
             else if(input$pregunta=="GPSI27"){GPSI27}
             else if(input$pregunta=="GPSI28"){GPSI28}
             else if(input$pregunta=="GPSI291"){GPSI291}
             else if(input$pregunta=="GPSI292"){GPSI292}
             else if(input$pregunta=="GPSI293"){GPSI293}
             else if(input$pregunta=="GPSI294"){GPSI294}
             else if(input$pregunta=="GPSI295"){GPSI295}
             else if(input$pregunta=="GPSI296"){GPSI296}
             else if(input$pregunta=="GPSI301"){GPSI301}
             else if(input$pregunta=="GPSI302"){GPSI302}
             else if(input$pregunta=="GPSI303"){GPSI303}
             else if(input$pregunta=="GPSI304"){GPSI304}
             else if(input$pregunta=="GPSI305"){GPSI305}
             else if(input$pregunta=="GPSI306"){GPSI306}
             else if(input$pregunta=="GPSI311"){GPSI311}
             else if(input$pregunta=="GPSI312"){GPSI312}
             else if(input$pregunta=="GPSI313"){GPSI313}
             else if(input$pregunta=="GPSI314"){GPSI314}
             else if(input$pregunta=="GPSI315"){GPSI315}
             else if(input$pregunta=="GPSI316"){GPSI316}


              #percepcion de seguridad 
             else if(input$pregunta=="GPSC1"){GPSC1}
             else if(input$pregunta=="GPSC2"){GPSC2}
             else if(input$pregunta=="GPSC3"){GPSC3}
             else if(input$pregunta=="GPSC4"){GPSC4}
             else if(input$pregunta=="GPSC5"){GPSC5}
             else if(input$pregunta=="GPSC6"){GPSC6}
             else if(input$pregunta=="GPSC7"){GPSC7}
             else if(input$pregunta=="GPSC81"){GPSC81}
             else if(input$pregunta=="GPSC82"){GPSC82}
             else if(input$pregunta=="GPSC83"){GPSC83}
             else if(input$pregunta=="GPSC84"){GPSC84}
             else if(input$pregunta=="GPSC9"){GPSC9}
             else if(input$pregunta=="GPSC10"){GPSC10}
             else if(input$pregunta=="GPSC11"){GPSC11}
             else if(input$pregunta=="GPSC12"){GPSC12}
             else if(input$pregunta=="GPSC13"){GPSC13}
             else if(input$pregunta=="GPSC14"){GPSC14}
             else if(input$pregunta=="GPSC15"){GPSC15}
             else if(input$pregunta=="GPSC16"){GPSC16}
             else if(input$pregunta=="GPSC17"){GPSC17}
             else if(input$pregunta=="GPSC18"){GPSC18}
             else if(input$pregunta=="GPSC19"){GPSC19}
             else if(input$pregunta=="GPSC20"){GPSC20}
             else if(input$pregunta=="GPSC21"){GPSC21}
             else if(input$pregunta=="GPSC22"){GPSC22}
             else if(input$pregunta=="GPSC23"){GPSC23}

             else if(input$pregunta=="GPSC25"){GPSC25}
             else if(input$pregunta=="GPSC26"){GPSC26}
             else if(input$pregunta=="GPSC27"){GPSC27}
             else if(input$pregunta=="GPSC28"){GPSC28}
             else if(input$pregunta=="GPSC291"){GPSC291}
             else if(input$pregunta=="GPSC292"){GPSC292}
             else if(input$pregunta=="GPSC293"){GPSC293}
             else if(input$pregunta=="GPSC294"){GPSC294}
             else if(input$pregunta=="GPSC295"){GPSC295}
             else if(input$pregunta=="GPSC296"){GPSC296}
             else if(input$pregunta=="GPSC301"){GPSC301}
             else if(input$pregunta=="GPSC302"){GPSC302}
             else if(input$pregunta=="GPSC303"){GPSC303}
             else if(input$pregunta=="GPSC304"){GPSC304}
             else if(input$pregunta=="GPSC305"){GPSC305}
             else if(input$pregunta=="GPSC306"){GPSC306}
             else if(input$pregunta=="GPSC311"){GPSC311}
             else if(input$pregunta=="GPSC312"){GPSC312}
             else if(input$pregunta=="GPSC313"){GPSC313}
             else if(input$pregunta=="GPSC314"){GPSC314}
             else if(input$pregunta=="GPSC315"){GPSC315}
             else if(input$pregunta=="GPSC316"){GPSC316}



              #percepcion de seguridad 
             else if(input$pregunta=="GPSE1"){GPSE1}
             else if(input$pregunta=="GPSE2"){GPSE2}
             else if(input$pregunta=="GPSE3"){GPSE3}
             else if(input$pregunta=="GPSE4"){GPSE4}
             else if(input$pregunta=="GPSE5"){GPSE5}
             else if(input$pregunta=="GPSE6"){GPSE6}
             else if(input$pregunta=="GPSE7"){GPSE7}
             else if(input$pregunta=="GPSE81"){GPSE81}
             else if(input$pregunta=="GPSE82"){GPSE82}
             else if(input$pregunta=="GPSE83"){GPSE83}
             else if(input$pregunta=="GPSE84"){GPSE84}
             else if(input$pregunta=="GPSE9"){GPSE9}
             else if(input$pregunta=="GPSE10"){GPSE10}
             else if(input$pregunta=="GPSE11"){GPSE11}
             else if(input$pregunta=="GPSE12"){GPSE12}
             else if(input$pregunta=="GPSE13"){GPSE13}
             else if(input$pregunta=="GPSE14"){GPSE14}
             else if(input$pregunta=="GPSE15"){GPSE15}
             else if(input$pregunta=="GPSE16"){GPSE16}
             else if(input$pregunta=="GPSE17"){GPSE17}
             else if(input$pregunta=="GPSE18"){GPSE18}
             else if(input$pregunta=="GPSE19"){GPSE19}
             else if(input$pregunta=="GPSE20"){GPSE20}
             else if(input$pregunta=="GPSE21"){GPSE21}
             else if(input$pregunta=="GPSE22"){GPSE22}
             else if(input$pregunta=="GPSE23"){GPSE23}

             else if(input$pregunta=="GPSE25"){GPSE25}
             else if(input$pregunta=="GPSE26"){GPSE26}
             else if(input$pregunta=="GPSE27"){GPSE27}
             else if(input$pregunta=="GPSE28"){GPSE28}
             else if(input$pregunta=="GPSE291"){GPSE291}
             else if(input$pregunta=="GPSE292"){GPSE292}
             else if(input$pregunta=="GPSE293"){GPSE293}
             else if(input$pregunta=="GPSE294"){GPSE294}
             else if(input$pregunta=="GPSE295"){GPSE295}
             else if(input$pregunta=="GPSE296"){GPSE296}
             else if(input$pregunta=="GPSE301"){GPSE301}
             else if(input$pregunta=="GPSE302"){GPSE302}
             else if(input$pregunta=="GPSE303"){GPSE303}
             else if(input$pregunta=="GPSE304"){GPSE304}
             else if(input$pregunta=="GPSE305"){GPSE305}
             else if(input$pregunta=="GPSE306"){GPSE306}
             else if(input$pregunta=="GPSE311"){GPSE311}
             else if(input$pregunta=="GPSE312"){GPSE312}
             else if(input$pregunta=="GPSE313"){GPSE313}
             else if(input$pregunta=="GPSE314"){GPSE314}
             else if(input$pregunta=="GPSE315"){GPSE315}
             else if(input$pregunta=="GPSE316"){GPSE316}
               
           

              #percepcion de seguridad 
             else if(input$pregunta=="GPSVS1"){GPSVS1}
             else if(input$pregunta=="GPSVS2"){GPSVS2}
             else if(input$pregunta=="GPSVS3"){GPSVS3}
             else if(input$pregunta=="GPSVS4"){GPSVS4}
             else if(input$pregunta=="GPSVS5"){GPSVS5}
             else if(input$pregunta=="GPSVS6"){GPSVS6}
             else if(input$pregunta=="GPSVS7"){GPSVS7}
             else if(input$pregunta=="GPSVS81"){GPSVS81}
             else if(input$pregunta=="GPSVS82"){GPSVS82}
             else if(input$pregunta=="GPSVS83"){GPSVS83}
             else if(input$pregunta=="GPSVS84"){GPSVS84}
             else if(input$pregunta=="GPSVS9"){GPSVS9}
             else if(input$pregunta=="GPSVS10"){GPSVS10}
             else if(input$pregunta=="GPSVS11"){GPSVS11}
             else if(input$pregunta=="GPSVS12"){GPSVS12}
             else if(input$pregunta=="GPSVS13"){GPSVS13}
             else if(input$pregunta=="GPSVS14"){GPSVS14}
             else if(input$pregunta=="GPSVS15"){GPSVS15}
             else if(input$pregunta=="GPSVS16"){GPSVS16}
             else if(input$pregunta=="GPSVS17"){GPSVS17}
             else if(input$pregunta=="GPSVS18"){GPSVS18}
             else if(input$pregunta=="GPSVS19"){GPSVS19}
             else if(input$pregunta=="GPSVS20"){GPSVS20}
             else if(input$pregunta=="GPSVS21"){GPSVS21}
             else if(input$pregunta=="GPSVS22"){GPSVS22}
             else if(input$pregunta=="GPSVS23"){GPSVS23}

             else if(input$pregunta=="GPSVS25"){GPSVS25}
             else if(input$pregunta=="GPSVS26"){GPSVS26}
             else if(input$pregunta=="GPSVS27"){GPSVS27}
             else if(input$pregunta=="GPSVS28"){GPSVS28}
             else if(input$pregunta=="GPSVS291"){GPSVS291}
             else if(input$pregunta=="GPSVS292"){GPSVS292}
             else if(input$pregunta=="GPSVS293"){GPSVS293}
             else if(input$pregunta=="GPSVS294"){GPSVS294}
             else if(input$pregunta=="GPSVS295"){GPSVS295}
             else if(input$pregunta=="GPSVS296"){GPSVS296}
             else if(input$pregunta=="GPSVS301"){GPSVS301}
             else if(input$pregunta=="GPSVS302"){GPSVS302}
             else if(input$pregunta=="GPSVS303"){GPSVS303}
             else if(input$pregunta=="GPSVS304"){GPSVS304}
             else if(input$pregunta=="GPSVS305"){GPSVS305}
             else if(input$pregunta=="GPSVS306"){GPSVS306}
             else if(input$pregunta=="GPSVS311"){GPSVS311}
             else if(input$pregunta=="GPSVS312"){GPSVS312}
             else if(input$pregunta=="GPSVS313"){GPSVS313}
             else if(input$pregunta=="GPSVS314"){GPSVS314}
             else if(input$pregunta=="GPSVS315"){GPSVS315}
             else if(input$pregunta=="GPSVS316"){GPSVS316}
                       
    
  })
  
  observeEvent(input$tipomapa, {  
       if (input$tipomapa== "PSQ"){
        updateSelectInput(session, "enfoque",                        
                           choices = c("Isla Mujeres"= "PSI", "Cancun"="PSC", "Ejido"="PSE", "Comparativa" = "PSVS")
         )
       }else
       if (input$tipomapa== "IS"){
        updateSelectInput(session, "enfoque",                
                          choices = c("Ambiental" = "ambi1" , "Economico" = "eco1","Social"="soc1") 
        )
      }else if (input$tipomapa== "EJ"){
        updateSelectInput(session, "enfoque",                
                          choices= c( "Datos familiares" = "DF","Datos económicos" = "DE", "Identidad y Comunidad" = "ID" , "Vivienda" = "VI", "Apreciación del encuestador" = "AE")
        )
      }
  })
  observeEvent(input$estudio,{
    if(input$estudio == "Salinas"){
      updateSelectInput(session, "var1",choices = c("Trabajo" = "tro","Actividad productiva","Uso salinas"))
      updateSelectInput(session, "var2", choices = c("Ninguno" = "ning","Trabajo","Beneficios de estar aquí"="benef","Desventajas de estar cerca de las salinas"="desv","Condiciones en que considera están las salinas"="cond","Efectos de la condición de las salinas"="efe"),selected = "benef")
      updateSelectInput(session, "var3", choices = c("Ninguno","Edad"))
    }else if(input$estudio == "Población y migración"){
      updateSelectInput(session, "var1", choices = c("Origen","Escolaridad","Puesto","Trabajo"))
      updateSelectInput(session,"var2",choices = c("Ninguno","Puesto","Trabajo","Ingreso_sem","Escolaridad"))
      updateSelectInput(session,"var3",choices = c("Ninguno","Edad","Sexo"))
    }
    else if(input$estudio == "Percepción de seguridad"){
      updateSelectInput(session, "var1", choices = c("Tiempo viviendo aquí" = "tmpa", "Tiempo en esta casa" = "tmpc","Víctima de algún delito"="vic"))
      updateSelectInput(session, "var2", choices = c("Ninguno" = "ning2","Riesgo de sufrir un delito en esta casa" = "R1","Riesgo de sufir un delito en esa calle" = "R2", "Riesgo de sufrir un delito en esta zona" = "R3", "Riesgo de sufrir un delito en esta ciudad" = "R4"),selected = "R1")
      updateSelectInput(session, "var3", choices = c("Ninguno", "Sexo"))
    }
  })
  observeEvent(input$var1,{
    #if(input$estudio == "PoblaciÃ³n y migraciÃ³n"){
    if(input$var1 == "Puesto"){
      updateSelectInput(session, "var2", choices = c("Ninguno","Trabajo","Ingreso_sem","Escolaridad"),selected = "Trabajo")
    }
    else if(input$var1 == "Trabajo"){
      updateSelectInput(session, "var2",choices = c("Ninguno","Puesto","Ingreso_sem","Escolaridad"),selected = "Puesto")
    }
    else if(input$var1 == "Escolaridad"){
      updateSelectInput(session,"var2",choices = c("Ninguno","Puesto","Trabajo","Ingreso_sem"),selected = "Puesto")
    }
    else if(input$var1 == "Origen"){
      updateSelectInput(session,"var2",choices = c("Ninguno","Puesto","Trabajo","Ingreso_sem","Escolaridad"),selected = "Puesto")
    }
    # }
  })
  observeEvent(input$var1,{
    if(input$var1 == "tro"){
      updateSelectInput(session, "var2", choices = c("Ninguno" = "ning","Beneficios de estar aquí"="benef","Desventajas de estar cerca de las salinas"="desv","Condiciones en que considera están las salinas"="cond","Efectos de la condición de las salinas"="efe"),selected = "benef")
    }
    else if(input$var1 == "Actividad productiva"){
      updateSelectInput(session, "var2", choices = c("Ninguno" = "ning","Trabajo","Beneficios de estar aquí"="benef","Desventajas de estar cerca de las salinas"="desv","Condiciones en que considera están las salinas"="cond","Efectos de la condición de las salinas"="efe"),selected = "Trabajo")
    }
    else if(input$var1 == "Uso salinas"){
      updateSelectInput(session, "var2", choices = c("Ninguno" = "ning","Trabajo","Beneficios de estar aquí"="benef","Desventajas de estar cerca de las salinas"="desv","Condiciones en que considera están las salinas"="cond","Efectos de la condición de las salinas"="efe"),selected = "Trabajo")
    }
  })
  observeEvent(input$var2,{
    if(input$var2 == "Ninguno"){
      updateSelectInput(session, "var3",choices = c("Ninguno","Edad","Sexo"),selected = "Edad")
    }
  })
  observeEvent(input$var2,{
    if(input$var2 == "ning"){
      updateSelectInput(session, "var3", choices = c("Ninguno","Edad"),selected = "Edad")
    }
  })
  observeEvent(input$var2,{
    if(input$var2 == "ning2"){
      updateSelectInput(session, "var3", choices = c("Ninguno","Sexo"),selected = "Sexo")
    }
  })
 


  
  
  #Panel: Graficas
  ## Muestra la info de Selectores
  observe({
    x <- input$enfoque
    if (is.null(x))
      x <- character(0)
    # Poblacion y migracion
          if (x== "DF"){
              updateSelectInput(session, "pregunta",  
                      choices = c(
                        "1. ¿Cuántas personas viven en esta casa?" = "PFAM01" , 
                        "2. ¿ A qué niveles de escolaridad asisten los integrantes de esta familia?" = "PFAM02",
                        "3. ¿Qué medio de transporte utilizan?" = "PFAM1" , 
                        "4. ¿Dónde adquiere sus v??veres?" = "PFAM2"," 
                        ¿5. ¿A dónde acude en caso de urgencia médica? " = "PFAM3", 
                        "6. ¿Con qué áreas de recreo cuenta en su colonia?" = "PFAM4")
                        
          )} else  if (x== "DE"){
              updateSelectInput(session, "pregunta",  
                        choices = c(
                        "1. ¿Cuál es el principal trabajo pagado del jefe o jefa de familia?" = "PPECO1" ,
                        "2. Puesto o posición en el trabajo del jefe de familia  " = "PPECO3",                    
                        "3. ¿A  cuánto asciende el ingreso total semanal del jefe de familia?" = "PPECO4",
                        "4. Máximo nivel de estudios completo del jefe o jefa de familia" = "PPECO2",
                        "5. ¿Número de personas que dependen del Jefe de Familia? "= "PPECO6",
                        "6. ¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?"="PPECO7" ,
                        "7. Además del jefe de familia ¿Cuántas personas trabajan en el hogar  con salario remunerado?"  ="PPECO8"  )
                      
          )} else  if (x== "ID"){
              updateSelectInput(session, "pregunta",  
                          choices = c(
                        "1. ¿Cuál es su lugar de origen? " = "PPIyC1" , 
                        "2. ¿Cuánto tiempo lleva viviendo en este lugar?" = "PPIyC2",
                        "3. ¿Qué lo motivó a venir a vivir en esta localidad?" = "PPIyC3", 
                        "4. En caso de requerir ayuda, apoyo legal o económico ante algún problema acudo a: " = "PPIyC4", 
                        "5. ¿Qué religión practica? " = "PPIyC5", 
                        "6. ¿Cuáles son las ventajas de vivir en este lugar?"="PPIyC6",
                        "7. ¿Piensa irse a vivir a otra localidad?"= "PPIyC7", 
                        "8. ¿Usted a qué municipio siente que pertenece?" = "PPIyC8",
                        "9. ¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?" = "PPIyC9",
                        "10. ¿Cuáles son los motivos por los que viaja a la Isla?"= "PPIyC10" )
                    
          )} else  if (x== "VI"){
              updateSelectInput(session, "pregunta",  
                          choices = c(
                          "1. La vivienda que habita tu familia es " = "V1P1R1" , 
                          "2. En caso  de ser  propia, ¿De qué forma fue adquirida?" = "V1P2R1",
                          "3. ¿Con qué servicios cuenta su vivienda? " = "V1P3R1", 
                          "4. ¿Cuántas inundaciones ha sufrido al vivir en esta localidad?" = "V1P4R1", 
                          "5. A pesar de la inundación usted decidió quedarse a vivir aqu?? por: " = "V1P5R1", 
                          "6. ¿Cuántas inundaciones ha sufrido al vivir en esta localidad?" = "V1P6R1", 
                          "7. ¿Conoce los impactos o afectaciones que puede sufrir?" = "V1P7R1", 
                          "8. FAVOR DE SEÑALAR LOS SIGUIENTES SERVICIOS OBSERVADOS ALREDEDOR DE LA VIVIENDA:" = "V1P8R1" )
                      
                      
          )} else  if (x== "AE"){
                updateSelectInput(session, "pregunta",  
                              choices = c("Sin informacion" = "DF1")      
          )} else  if (x== "ambi1"){  #SALINAS
                updateSelectInput(session, "pregunta",  
                        choices = c(
                          "1.-¿QUÉ USO LE DAN SUS VECINOS A LA SALINA?" = "PAMB1" , 
                          "2.-¿QUÉ BENEFICIO RECIBE DE VIVIR AQUÍ?" = "PAMB2",
                          "3.-¿QUÉ DESVENTAJA RECIBE DE VIVIR AQUÍ CERCA DE LA SALINA? " = "PAMB3", 
                          "4.-¿EN QUÉ CONDICIONES CONSIDERA QUE SE ENCUENTRA LA SALINA? " = "PAMB4", 
                          "5.-¿QUÉ EFECTOS GENERA LA CONDICIÓN (SUCIA O CONTAMINADA) DE LA SALINA?" = "PAMB5",
                          "6.-¿HAN LLEVADO A CABO ALGUNA ACTIVIDAD DE LMPIEZA, SANEAMIENTO O CONSERVACIÓN DE LA SALINA?" = "PAMB6",
                          "7.-¿ESTÁN CONECTADOS AL DRENAJE?" = "PAMB7")
                              
          )} else  if (x== "soc1"){
            updateSelectInput(session, "pregunta",  
                        choices = c(
                          "1.-¿ENTRE LOS VECINOS, REALIZAN  ALGUNA ACTIVIDAD EN COMUN?" = "SOC1" ,
                          "2.-¿CÓMO ES LA RELACIÓN CON SUS VECINOS?" = "SOC2",
                          "3.-¿HA TENIDO PROBLEMAS CON SUS VECINOS: PLEITOS, DEMANDAS…? " = "SOC3", 
                          "4.-¿CON QUE FRECUENCIA SE HACEN FAVORES ENTRE VECINOS? " = "SOC4", 
                          "5.-¿EN ALGÚN PROBLEMA QUE SE LE PRESENTE SUS VECINOS LE AYUDAN?"= "SOC5",
                          "6.-¿PERTENECE A ALGUNA ORGANIZACIÓN?"= "SOC6")
                                
      )} else  if (x== "eco1"){
            updateSelectInput(session, "pregunta",  
                        choices = c(
                          "1.-¿CUÁNTAS PERSONAS DE ESTA FAMILIA TRABAJAN?" = "ECO1" , 
                          "2.-¿EN QUÉ TRABAJAN?" = "ECO2",
                          "3.-¿REALIZAN ALGUNA ACTIVIDAD PRODUCTIVA POR SU CUENTA? " = "ECO3", 
                          "4.-¿INTERCAMBIAN PRODUCTOS CON SUS VECINOS? " = "ECO4",
                          "5.-¿ SE AYUDAN ENTRE VECINOS PARA ALGÚN TRABAJO QUE BENEFICIE LA ECONOMÍA FAMILIAR DENTRO DE SUS CASAS?" = "ECO4" )      
      )} else  if (x== "PSI"){ #percepcion
             updateSelectInput(session, "pregunta",  
                          choices = c(
                            "1. En esta calle o zona, Usted participa: - IM " = "GPSI1", 
                            "2. Usted conoce a sus vecinos: - IM " = "GPSI2" ,
                            "3. Participa con la autoridad para mejorar la seguridad: - IM " = "GPSI3" ,
                            "4. Día en que podría participar en actividades con la autoridad*" =  "GPSI4",
                            "5. Horarios en los que podría participar en actividades con la autoridad " = "GPSI5",
                            "6. Cuando hay un delito, en esta calle o zona los vecinos: - IM " = "GPSI6", 
                            "7. Durante el último año, en esta calle o zona ha habido: - IM " = "GPSI7" ,
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - IM " = "GPSI81",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - IM " = "GPSI82",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - IM " = "GPSI83",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - IM " = "GPSI84",
                            "9. ¿Usted ha sido v??ctima de algún delito en el último año? - IM " = "GPSI9" ,
                            "10. En caso de ser v??ctima del delito Usted: - IM " = "GPSI10", 
                            "11. Ha sido v??ctima de algún delito y no denuncio: - IM " = "GPSI11" ,
                            "12. En esta calle o zona: - IM " = "GPSI12" ,
                            "13. En esta calle o zona, hay personas - IM " = "GPSI13", 
                            "14. En esta calle o zona hay violencia: - IM " = "GPSI14" ,
                            "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - IM " = "GPSI15" ,
                            "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - IM " = "GPSI16" ,
                            "17. En esta calle o zona hay niños que se quedan la mayor parte del d??a sin comer:  - IM " = "GPSI17" ,
                            "18. En esta calle o zona hay jóvenes que: - IM " = "GPSI18" ,
                            "19. En esta calle o zona hay un parquI - IM " = "GPSI19" ,
                            "20. En esta calle o zona hay: - IM " = "GPSI20" ,
                            "21. En esta zona hay - IM " = "GPSI21" ,
                            "22. En el último año Usted supo que algún menor de 18 años: - IM " = "GPSI22" ,
                            "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - IM " = "GPSI23" ,
                            "24. En esta casa: (APLICA TARJETON) - IM " = "GPSI24" ,
                            "25. En esta casa alguien: (APLICA TARJETON) - IM " = "GPSI25" ,
                            "26. En el último año, por cuestiones de seguridad Usted ha pensado: - IM " = "GPSI26" ,
                            "27. En el último año, por cuestiones de seguridad Usted dejó de: - IM " = "GPSI27" ,
                            "28. En esta calle o zona la polic??a: - IM " = "GPSI28" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La polic??a  - IM " = "GPSI291" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - IM " = "GPSI292" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - IM " = "GPSI293" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - IM " = "GPSI294" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - IM " = "GPSI295" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - IM " = "GPSI296" ,
                            "30. Del 1 al 5, califique el trabajo de: La polic??a - IM " = "GPSI301" ,
                            "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - IM " = "GPSI302" ,
                            "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - IM " = "GPSI303" ,
                            "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - IM " = "GPSI304" ,
                            "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - IM " = "GPSI305" ,
                            "30. Del 1 al 5, califique el trabajo de: El Gobernador - IM " = "GPSI306" ,                          
                            "31. Del 1 al 5, califique el trato que recibe de: La polic??a - IM " = "GPSI311" ,
                            "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar - IM " = "GPSI312" ,
                            "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno - IM " = "GPSI313" ,
                            "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal - IM " = "GPSI314" ,
                            "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa - IM " = "GPSI315" ,
                            "31. Del 1 al 5, califique el trato que recibe de: El Gobernador - IM " = "GPSI316"                          
                            
                            )
              
        )} else if(x == "PSE"){
                  updateSelectInput(session, "pregunta",  
                      choices = c(
                                  "1. En esta calle o zona, Usted participa: - EJ " = "GPSE1", 
                                  "2. Usted conoce a sus vecinos: - IM " = "GPSE2" ,
                                  "3. Participa con la autoridad para mejorar la seguridad: - IM " = "GPSE3" ,
                                  "4. Día en que podría participar en actividades con la autoridad*" =  "GPSE4",
                                  "5. Horarios en los que podría participar en actividades con la autoridad " = "GPSE5",
                                  "6. Cuando hay un delito, en esta calle o zona los vecinos: - EJ " = "GPSE6", 
                                  "7. Durante el último año, en esta calle o zona ha habido: - EJ " = "GPSE7" ,
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - EJ " = "GPSE81",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - EJ " = "GPSE82",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - EJ " = "GPSE83",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - EJ " = "GPSE84",
                                  "9. ¿Usted ha sido v??ctima de algún delito en el último año? - EJ " = "GPSE9I" ,
                                  "10. En caso de ser v??ctima del delito Usted: - EJ " = "GPSE10", 
                                  "11. Ha sido v??ctima de algún delito y no denuncio: - EJ " = "GPSE11" ,
                                  "12. En esta calle o zona: - EJ " = "GPSE12" ,
                                  "13. En esta calle o zona, hay personas - EJ " = "GPSE13", 
                                  "14. En esta calle o zona hay violencia: - EJ " = "GPSE14" ,
                                  "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - EJ " = "GPSE15" ,
                                  "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - EJ " = "GPSE16" ,
                                  "17. En esta calle o zona hay niños que se quedan la mayor parte del d??a sin comer:  - EJ " = "GPSE17" ,
                                  "18. En esta calle o zona hay jóvenes que: - EJ " = "GPSE18" ,
                                  "19. En esta calle o zona hay un parquI - EJ " = "GPSE19" ,
                                  "20. En esta calle o zona hay: - EJ " = "GPSE20" ,
                                  "21. En esta zona hay - EJ " = "GPSE21" ,
                                  "22. En el último año Usted supo que algún menor de 18 años: - EJ " = "GPSE22" ,
                                  "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - EJ " = "GPSE23" ,
                                  "24. En esta casa: (APLICA TARJETON) - EJ " = "GPSE24" ,
                                  "25. En esta casa alguien: (APLICA TARJETON) - EJ " = "GPSE25" ,
                                  "26. En el último año, por cuestiones de seguridad Usted ha pensado: - EJ " = "GPSE26" ,
                                  "27. En el último año, por cuestiones de seguridad Usted dejó de: - EJ " = "GPSE27" ,
                                  "28. En esta calle o zona la polic??a: - EJ " = "GPSE28" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La polic??a  - EJ " = "GPSE291" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - EJ " = "GPSE292" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - EJ " = "GPSE293" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - EJ " = "GPSE294" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - EJ " = "GPSE295" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - EJ " = "GPSE296" ,
                                  "30. Del 1 al 5, califique el trabajo de: La polic??a - EJ " = "GPSE301" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - EJ " = "GPSE302" ,
                                  "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - EJ " = "GPSE303" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - EJ " = "GPSE304" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - EJ " = "GPSE305" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Gobernador - EJ " = "GPSE306" ,                          
                                  "31. Del 1 al 5, califique el trato que recibe de: La polic??a - EJ " = "GPSE311" ,
                                  "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar - EJ " = "GPSE312" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno - EJ " = "GPSE313" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal - EJ " = "GPSE314" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa - EJ " = "GPSE315" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: El Gobernador - EJ " = "GPSE316"                          
                                  
                      )
        
        )} else  if (x== "PSC"){ #percepcion
             updateSelectInput(session, "pregunta",  
                      choices = c(
                              "1. En esta calle o zona, Usted participa: - CUN" = "GPSC1", 
                              "2. Usted conoce a sus vecinos: - IM " = "GPSC2" ,
                              "3. Participa con la autoridad para mejorar la seguridad: - IM " = "GPSC3" ,
                              "4. Día en que podría participar en actividades con la autoridad*" =  "GPSC4",
                              "5. Horarios en los que podría participar en actividades con la autoridad " = "GPSC5",
                              "6. Cuando hay un delito, en esta calle o zona los vecinos: - CUN" = "GPSC6", 
                              "7. Durante el último año, en esta calle o zona ha habido: - CUN" = "GPSC7" ,
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - CUN" = "GPSC81",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - CUN" = "GPSC82",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - CUN" = "GPSC83",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - CUN" = "GPSC84",
                              "9. ¿Usted ha sido v??ctima de algún delito en el último año? - CUN" = "GPSC9I" ,
                              "10. En caso de ser v??ctima del delito Usted: - CUN" = "GPSC10", 
                              "11. Ha sido v??ctima de algún delito y no denuncio: - CUN" = "GPSC11" ,
                              "12. En esta calle o zona: - CUN" = "GPSC12" ,
                              "13. En esta calle o zona, hay personas - CUN" = "GPSC13", 
                              "14. En esta calle o zona hay violencia: - CUN" = "GPSC14" ,
                              "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - CUN" = "GPSC15" ,
                              "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - CUN" = "GPSC16" ,
                              "17. En esta calle o zona hay niños que se quedan la mayor parte del d??a sin comer:  - CUN" = "GPSC17" ,
                              "18. En esta calle o zona hay jóvenes que: - CUN" = "GPSC18" ,
                              "19. En esta calle o zona hay un parquI - CUN" = "GPSC19" ,
                              "20. En esta calle o zona hay: - CUN" = "GPSC20" ,
                              "21. En esta zona hay - CUN" = "GPSC21" ,
                              "22. En el último año Usted supo que algún menor de 18 años: - CUN" = "GPSC22" ,
                              "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - CUN" = "GPSC23" ,
                              "24. En esta casa: (APLICA TARJETON) - CUN" = "GPSC24" ,
                              "25. En esta casa alguien: (APLICA TARJETON) - CUN" = "GPSC25" ,
                              "26. En el último año, por cuestiones de seguridad Usted ha pensado: - CUN" = "GPSC26" ,
                              "27. En el último año, por cuestiones de seguridad Usted dejó de: - CUN" = "GPSC27" ,
                              "28. En esta calle o zona la polic??a: - CUN" = "GPSC28" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La polic??a  - CUN" = "GPSC291" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - CUN" = "GPSC292" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - CUN" = "GPSC293" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - CUN" = "GPSC294" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - CUN" = "GPSC295" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - CUN" = "GPSC296" ,
                              "30. Del 1 al 5, califique el trabajo de: La polic??a - CUN" = "GPSC301" ,
                              "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - CUN" = "GPSC302" ,
                              "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - CUN" = "GPSC303" ,
                              "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - CUN" = "GPSC304" ,
                              "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - CUN" = "GPSC305" ,
                              "30. Del 1 al 5, califique el trabajo de: El Gobernador - CUN" = "GPSC306" ,                          
                              "31. Del 1 al 5, califique el trato que recibe de: La polic??a - CUN" = "GPSC311" ,
                              "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar - CUN" = "GPSC312" ,
                              "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno - CUN" = "GPSC313" ,
                              "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal - CUN" = "GPSC314" ,
                              "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa - CUN" = "GPSC315" ,
                              "31. Del 1 al 5, califique el trato que recibe de: El Gobernador - CUN" = "GPSC316"                          
                              
                      )
        )}    else if(x == "PSVS"){
                  updateSelectInput(session, "pregunta",  
                      choices = c(
                                  "1. En esta calle o zona, Usted participa: - VS " = "GPSVS1", 
                                  "2. Usted conoce a sus vecinos: - IM " = "GPSVS2" ,
                                  "3. Participa con la autoridad para mejorar la seguridad: - IM " = "GPSVS3" ,
                                  "4. Día en que podría participar en actividades con la autoridad*" =  "GPSVS4",
                                  "5. Horarios en los que podría participar en actividades con la autoridad " = "GPSVS5",
                                  "6. Cuando hay un delito, en esta calle o zona los vecinos: - VS " = "GPSVS6", 
                                  "7. Durante el último año, en esta calle o zona ha habido: - VS " = "GPSVS7" ,
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - VS " = "GPSVS81",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - VS " = "GPSVS82",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - VS " = "GPSVS83",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - VS " = "GPSVS84",
                                  "9. ¿Usted ha sido víctima de algún delito en el último año? - VS " = "GPSVS9I" ,
                                  "10. En caso de ser víctima del delito Usted: - VS " = "GPSVS10", 
                                  "11. Ha sido víctima de algún delito y no denuncio: - VS " = "GPSVS11" ,
                                  "12. En esta calle o zona: - VS " = "GPSVS12" ,
                                  "13. En esta calle o zona, hay personas - VS " = "GPSVS13", 
                                  "14. En esta calle o zona hay violencia: - VS " = "GPSVS14" ,
                                  "15. En esta calle o zona, cuando hay conflictos entre vecinos se manVSan: - VS " = "GPSVS15" ,
                                  "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - VS " = "GPSVS16" ,
                                  "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:  - VS " = "GPSVS17" ,
                                  "18. En esta calle o zona hay jóvenes que: - VS " = "GPSVS18" ,
                                  "19. En esta calle o zona hay un parquI - VS " = "GPSVS19" ,
                                  "20. En esta calle o zona hay: - VS " = "GPSVS20" ,
                                  "21. En esta zona hay - VS " = "GPSVS21" ,
                                  "22. En el último año Usted supo que algún menor de 18 años: - VS " = "GPSVS22" ,
                                  "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - VS " = "GPSVS23" ,
                                  "24. En esta casa: (APLICA TARJETON) - VS " = "GPSVS24" ,
                                  "25. En esta casa alguien: (APLICA TARJETON) - VS " = "GPSVS25" ,
                                  "26. En el último año, por cuestiones de seguridad Usted ha pensado: - VS " = "GPSVS26" ,
                                  "27. En el último año, por cuestiones de seguridad Usted dVSó de: - VS " = "GPSVS27" ,
                                  "28. En esta calle o zona la policía: - VS " = "GPSVS28" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: La policía  - VS " = "GPSVS291" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - VS " = "GPSVS292" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - VS " = "GPSVS293" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: Su Comisario VSidal  - VS " = "GPSVS294" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - VS " = "GPSVS295" ,
                                  "29. Del 1 al 5, con el 5 como mVSor calificación, califique la confianza que Usted tiene en: El Gobernador  - VS " = "GPSVS296" ,
                                  "30. Del 1 al 5, califique el trabajo de: La policía - VS " = "GPSVS301" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - VS " = "GPSVS302" ,
                                  "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - VS " = "GPSVS303" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su Comisario VSida - VS " = "GPSVS304" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - VS " = "GPSVS305" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Gobernador - VS " = "GPSVS306" ,                          
                                  "31. Del 1 al 5, califique el trato que recibe de: La policía - VS " = "GPSVS311" ,
                                  "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar - VS " = "GPSVS312" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno - VS " = "GPSVS313" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Su Comisario VSidal - VS " = "GPSVS314" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa - VS " = "GPSVS315" ,
                                  "31. Del 1 al 5, califique el trato que recibe de: El Gobernador - VS " = "GPSVS316"                          
                                  
                      )
        
        )} 

  })
  

  #Textos descriptivos
  output$localiz <- renderText ({
      if(input$tipomapa=="ALL"){ "Cancún - Isla Mujeres " }
      else if(input$tipomapa=="PS"){ "Cancún, QRoo." }
      else if(input$tipomapa=="IS"){ "Salinas, Isla Mujeres." }
      else if(input$tipomapa=="EJ"){ " Zona Urbana Isla Mujeres "}
  })
  
  output$TipoestudioG <- renderText ({
      if(input$tipomapa=="PS"){ "Percepción sobre seguridad" }
      else if(input$tipomapa=="IS"){ "Estudio Socioeconómico y ambiental" }
      else if(input$tipomapa=="EJ"){ "Caracter??sticas sobre población y migración"}
  })
  output$tipoestudio <- renderText({
    if(input$estudio == "Salinas"){
      "Diagnóstico socioeconómico y ambiental de las Salinas de Isla Mujeres"
    }
    else if(input$estudio == "Población y migración"){
      "Características sobre población y migración"
    }
    else if(input$estudio == "Percepción de seguridad"){
      "Consulta ciudadana sobre percepción de seguridad"
    }
  })
  
  
  # ---------------------------------------------------------------------
  # MAPAS
  
  #Pintado de mapas
  output$mymap <- renderLeaflet({
        if(input$showmapa=="PS"){ GraphM(percepcion) }
        else if(input$showmapa=="IS"){ GraphM(filtro) }
        else if(input$showmapa=="EJ"){ GraphM(df_fin) }
  })
  
  # Texto tipo de estudio
  output$Tipoestudio <- renderText ({
    if(input$showmapa=="ALL"){ "Estudios socioeconomicos, Percepción de seguridad y Caracter??sticas sobre población y migración. " }
    else if(input$showmapa=="PS"){ "Percepción sobre seguridad" }
    else if(input$showmapa=="IS"){ "Estudio Socioeconómico y ambiental" }
    else if(input$showmapa=="EJ"){ "Caracter??sticas sobre población y migración"}
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
    }
    else if(input$var1 == "Origen" & input$var2 == "Puesto" & input$var3 == "Sexo"){
      mosaics2(tbl16, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics(tbl17, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
      mosaics(tbl18, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
      mosaics(tbl19, c(90,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
      mosaics(tbl20, c(90,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
      mosaics(tbl21, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
      mosaics(tbl22, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Puesto" & input$var3 == "Edad"){
      mosaics2(tbl23, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Puesto" & input$var3 == "Sexo"){
      mosaics2(tbl24, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics2(tbl25, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
      mosaics2(tbl26, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
      mosaics2(tbl27, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
      mosaics2(tbl28, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
      mosaics2(tbl29, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
      mosaics2(tbl30, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics2(tbl31, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
      mosaics2(tbl32, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
      mosaics2(tbl33, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
      mosaics2(tbl34, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
      mosaics2(tbl35, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
      mosaics2(tbl36, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Puesto" & input$var3 == "Edad"){
      mosaics2(tbl37, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Puesto" & input$var3 == "Sexo"){
      mosaics2(tbl38, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
      mosaics2(tbl39, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
      mosaics2(tbl40, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
      mosaics2(tbl41, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
      mosaics2(tbl42, c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "Origen" & input$var2 == "Puesto"){
      mosaics(tbl1, c(90,0,0,0), c(-1,0,0,6.5))
    }
    else if(input$var1 == "Origen" & input$var2 == "Trabajo"){
      mosaics(tbl2, c(0,0,0,0), c(0,0,0,6.5))
    }
    else if(input$var1 == "Origen" & input$var2 == "Escolaridad"){
      mosaics(tbl3, c(90,0,0,0), c(0,0,0,6.5))
    }
    else if(input$var1 == "Origen" & input$var2 == "Ingreso_sem"){
      mosaics(tbl4, c(0,0,0,0), c(0,0,0,6.5))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Puesto"){
      mosaics(tbl5, c(90,0,0,0), c(-1,0,0,9.5))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Trabajo"){
      mosaics(tbl6, c(90,0,0,0), c(0,0,0,9.5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics(tbl45, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Ingreso_sem"){
      mosaics(tbl8, c(0,0,0,0), c(0,0,0,9.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Trabajo"){
      mosaics(tbl9, c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Escolaridad"){
      mosaics(tbl10, c(90,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Ingreso_sem"){
      mosaics(tbl11, c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Puesto"){
      mosaics(tbl12, c(90,0,0,0), c(-1,0,0,5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Escolaridad"){
      mosaics(tbl13, c(90,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Ingreso_sem"){
      mosaics(tbl14, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Puesto" & input$var3 == "Edad"){
      mosaics(tbl43, c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Puesto" & input$var3 == "Sexo"){
      mosaics(tbl44, c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
      mosaics(tbl46, c(0,0,0,0), c(0,0,0,11.5))
    }
    #salinas
    else if(input$var1 == "tro" & input$var2 == "benef" & input$var3 == "Ninguno"){
      mosaics(sbl1 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "tro" & input$var2 == "desv" & input$var3 == "Ninguno"){
      mosaics(sbl2 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "tro" & input$var2 == "cond" & input$var3 == "Ninguno"){
      mosaics(sbl3 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "tro" & input$var2 == "efe" & input$var3 == "Ninguno"){
      mosaics(sbl4 , c(90,0,0,0), c(0,0,0,11.5))
    }
    
    else if(input$var1 == "Actividad productiva" & input$var2 == "benef" & input$var3 == "Ninguno"){
      mosaics(sbl5 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "desv" & input$var3 == "Ninguno"){
      mosaics(sbl6 , c(90,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "cond" & input$var3 == "Ninguno"){
      mosaics(sbl7 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "efe" & input$var3 == "Ninguno"){
      mosaics(sbl8 , c(90,0,0,0), c(0,0,0,11.5))
    }
    
    else if(input$var1 == "Uso salinas" & input$var2 == "benef" & input$var3 == "Ninguno"){
      mosaics(sbl9 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "desv" & input$var3 == "Ninguno"){
      mosaics(sbl10 , c(0,0,0,0), c(0,0,0,11.5))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "cond" & input$var3 == "Ninguno"){
      mosaics(sbl11 , c(0,0,0,0), c(0,0,0,11))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "efe" & input$var3 == "Ninguno"){
      mosaics(sbl12 , c(90,0,0,0), c(0,0,0,11.5))
    }
    ### Edad ###
    #salinas
    else if(input$var1 == "tro" & input$var2 == "benef" & input$var3 == "Edad"){
      mosaics3(sbl13 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "tro" & input$var2 == "desv" & input$var3 == "Edad"){
      mosaics3(sbl14 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "tro" & input$var2 == "cond" & input$var3 == "Edad"){
      mosaics3(sbl15 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "tro" & input$var2 == "efe" & input$var3 == "Edad"){
      mosaics3(sbl16 , c(0,0,0,0), c(0,0,0,1))
    }
    
    else if(input$var1 == "Actividad productiva" & input$var2 == "benef" & input$var3 == "Edad"){
      mosaics3(sbl17 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "desv" & input$var3 == "Edad"){
      mosaics3(sbl18 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "cond" & input$var3 == "Edad"){
      mosaics3(sbl19 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "efe" & input$var3 == "Edad"){
      mosaics3(sbl20 , c(0,0,0,0), c(0,0,0,1))
    }
    
    else if(input$var1 == "Uso salinas" & input$var2 == "benef" & input$var3 == "Edad"){
      mosaics3(sbl21 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "desv" & input$var3 == "Edad"){
      mosaics3(sbl22 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "cond" & input$var3 == "Edad"){
      mosaics3(sbl23 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "efe" & input$var3 == "Edad"){
      mosaics3(sbl24 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "Trabajo" & input$var3 == "Ninguno"){
      mosaics(sbl25 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics2(sbl26 , c(0,0,0,0), c(0,0,0,1))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "Trabajo" & input$var3 == "Ninguno"){
      mosaics(sbl27 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "Trabajo" & input$var3 == "Edad"){
      mosaics2(sbl28 , c(0,0,0,0), c(0,0,0,1))
    }
    ##### percepción #####
    else if(input$var1 == "tmpa" & input$var2 == "R1" & input$var3 == "Ninguno"){
      mosaics(pl1 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R2" & input$var3 == "Ninguno"){
      mosaics(pl2 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R3" & input$var3 == "Ninguno"){
      mosaics(pl3 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R4" & input$var3 == "Ninguno"){
      mosaics(pl4 , c(0,0,0,0), c(0,0,0,6))
    }
    ##
    else if(input$var1 == "tmpc" & input$var2 == "R1" & input$var3 == "Ninguno"){
      mosaics(pl5 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R2" & input$var3 == "Ninguno"){
      mosaics(pl6 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R3" & input$var3 == "Ninguno"){
      mosaics(pl7 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R4" & input$var3 == "Ninguno"){
      mosaics(pl8 , c(0,0,0,0), c(0,0,0,6))
    }
    ##
    else if(input$var1 == "vic" & input$var2 == "R1" & input$var3 == "Ninguno"){
      mosaics(pl9 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "vic" & input$var2 == "R2" & input$var3 == "Ninguno"){
      mosaics(pl10 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "vic" & input$var2 == "R3" & input$var3 == "Ninguno"){
      mosaics(pl11 , c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "vic" & input$var2 == "R4" & input$var3 == "Ninguno"){
      mosaics(pl12 , c(0,0,0,0), c(0,0,0,6))
    }
    ######
    else if(input$var1 == "tmpa" & input$var2 == "R1" & input$var3 == "Sexo"){
      mosaics2(pl13 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R2" & input$var3 == "Sexo"){
      mosaics2(pl14 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R3" & input$var3 == "Sexo"){
      mosaics2(pl15 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpa" & input$var2 == "R4" & input$var3 == "Sexo"){
      mosaics2(pl16 , c(0,0,0,0), c(0,0,0,2))
    }
    ##
    else if(input$var1 == "tmpc" & input$var2 == "R1" & input$var3 == "Sexo"){
      mosaics2(pl17 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R2" & input$var3 == "Sexo"){
      mosaics2(pl18 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R3" & input$var3 == "Sexo"){
      mosaics2(pl19 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "tmpc" & input$var2 == "R4" & input$var3 == "Sexo"){
      mosaics2(pl20 , c(0,0,0,0), c(0,0,0,2))
    }
    ##
    else if(input$var1 == "vic" & input$var2 == "R1" & input$var3 == "Sexo"){
      mosaics2(pl21 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "vic" & input$var2 == "R2" & input$var3 == "Sexo"){
      mosaics2(pl22 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "vic" & input$var2 == "R3" & input$var3 == "Sexo"){
      mosaics2(pl23 , c(0,0,0,0), c(0,0,0,2))
    }
    else if(input$var1 == "vic" & input$var2 == "R4" & input$var3 == "Sexo"){
      mosaics2(pl24 , c(0,0,0,0), c(0,0,0,2))
    }
    #Ninguno
    else if(input$var1 == "Origen" & input$var2 == "Ninguno" & input$var3 == "Edad"){
      mosaics(nbl1, c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "Origen" & input$var2 == "Ninguno" & input$var3 == "Sexo"){
      mosaics(nbl2, c(0,0,0,0), c(0,0,0,6))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Ninguno" & input$var3 == "Edad"){
      mosaics(nbl3, c(0,0,0,0), c(0,0,0,7.5))
    }
    else if(input$var1 == "Escolaridad" & input$var2 == "Ninguno" & input$var3 == "Sexo"){
      mosaics(nbl4, c(0,0,0,0), c(0,0,0,7.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Ninguno" & input$var3 == "Edad"){
      mosaics(nbl5, c(0,0,0,0), c(0,0,0,8.5))
    }
    else if(input$var1 == "Puesto" & input$var2 == "Ninguno" & input$var3 == "Sexo"){
      mosaics(nbl6, c(0,0,0,0), c(0,0,0,8.5))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Ninguno" & input$var3 == "Edad"){
      mosaics(nbl7, c(0,0,0,0), c(0,0,0,4))
    }
    else if(input$var1 == "Trabajo" & input$var2 == "Ninguno" & input$var3 == "Sexo"){
      mosaics(nbl8, c(0,0,0,0), c(0,0,0,4))
    }
    #extrasalinas
    else if(input$var1 == "tro" & input$var2 == "ning" & input$var3 == "Edad"){
      mosaics(sbl29, c(0,0,0,0), c(0,0,0,4))
    }
    else if(input$var1 == "Actividad productiva" & input$var2 == "ning" & input$var3 == "Edad"){
      mosaics(sbl30, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "Uso salinas" & input$var2 == "ning" & input$var3 == "Edad"){
      mosaics(sbl31, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "tmpa" & input$var2 == "ning2" & input$var3 == "Sexo"){
      mosaics(pl25, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "tmpc" & input$var2 == "ning2" & input$var3 == "Sexo"){
      mosaics(pl26, c(0,0,0,0), c(0,0,0,5))
    }
    else if(input$var1 == "vic" & input$var2 == "ning2" & input$var3 == "Sexo"){
      mosaics(pl27, c(0,0,0,0), c(0,0,0,5))
    }
  })
  output$Expl1 <- renderUI({
    if(input$var3 == "Edad"){
      word <- paste("<b>",tags$i("o Edad:"),"</b>",tags$i("Grupo A: Encuestados entre 18 y 40 años,
                                                          Grupo B: Encuestados mayores de 40 años"))
      HTML(paste(word))
    }
  })
  output$Expl2 <- renderUI({
    if(input$var2 == "Ingreso_sem"){
      word <- paste("<b>",tags$i("o Ingreso semanal:"),"</b>",tags$i("Grupo A: $0 a $1200,
                                                          Grupo B: $1201 a $2500,
                                                          Grupo C: $2501 a $5000,
                                                          Grupo D: Más de $5000"))
      HTML(paste(word))
    }
  })
  output$Expl3 <- renderUI({
    if(input$var2 == "R1" || input$var2 == "R2" || input$var2 == "R3" || input$var2 == "R4"){
      word <- paste("<b>",tags$i("o Riesgos de sufrir un delito:"),"</b>",tags$i("A: Riesgo Muy Bajo,
                                                          B: Riesgo Bajo,
                                                          C: Riesgo Medio,
                                                          D: Riesgo Alto,
                                                          E: Riesgo Muy Alto"))
      HTML(paste(word))
    }
  })
 
  output$data <- DT::renderDataTable({
    DT::datatable(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")[, input$show_vars, drop = FALSE])
  })
  
  
  output$sum <- renderPrint({
    summary(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")[, input$show_vars, drop = FALSE])
  })
  
  
  
  
  rules <- reactive({
    
    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
    
    
    
    
    df_ide$Info.Sexo <- factor(df_ide$Info.Sexo, 
                               levels = c("M", "H"),
                               ordered = TRUE)
    
    df_ide$Info.Edad <- factor(df_ide$Info.Edad,
                               levels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                               ordered = TRUE)
    
    
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
    
    df_ide$DE03_puesto    <- factor(df_ide$DE03_puest,
                                    levels = c("EMPLEADO-BAJO","EMPLEADO-ARRENDADOR","PATRON","EMPLEADO-MEDIO","JUBILADO","OTRO"),
                                    ordered = TRUE)
    
    df_ide$DE06_esc_jefe   <- factor(df_ide$DE06_esc_jefe,
                                     levels = c("SECUNDARIA","LICENCIATURA","PRIMARIA","BACHILLERATO","PRIMARIA-INCOMPLETA","TECNICO-SUPERIOS"),
                                     ordered = TRUE)
    
    
    
    df_ide$DE02_trabajo   <- factor(df_ide$DE02_trabajo,
                                    levels = c("COMERCIO","INDUSTRIA","AGRICULA","JUBILADO"),
                                    ordered = TRUE)
    
    
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
    
    
    
    
    
    
    
    
    df_ide$DE10_mun   <- factor(df_ide$DE10_mun,
                                levels = c("BENITO","ISLA-MUJERES","OTHON","SOLIDARIDAD","TULUM","OTRO","PUERTO"),
                                ordered = TRUE)
    
    
    df_ide$IyC02_Estado_Origen   <- factor(df_ide$IyC02_Estado_Origen,
                                           levels = c("QUINTANA ROO","YUCATAN","CHIAPAS","VERACRUZ","GUERRERO","SONORA","PUEBLA","ESTAO DE MEXICO","OAXACA","TABASCO","DISTRITO FEDERAL","CAMPECHE","CHIHUAHUA","GUANAJUATO","HIDALGO","TAMAULIPAS"),
                                           ordered = TRUE)
    
    
    
    df_ide$IyC03_Tiempo   <- factor(df_ide$IyC03_Tiempo,
                                    levels = c("TODA","ENTRE.UN.AñO.Y.CINCO","MAS.DE.5.ANOS","MENOS.DE.UN.AñO"),
                                    ordered = TRUE)
    
    
    df_ide$IyC04_Motivo_Localidad_Parientes    <- factor(df_ide$IyC04_Motivo_Localidad_Parientes,
                                                         levels = c(0,1),
                                                         ordered = TRUE)
    
    
    
    
    
    
    df_ide$IyC04_Motivo_Localidad_Amigos    <- factor(df_ide$IyC04_Motivo_Localidad_Amigos,
                                                      levels = c(0,1),
                                                      ordered = TRUE)
    
    
    df_ide$IyC04_Motivo_Localidad_Trabajo    <- factor(df_ide$IyC04_Motivo_Localidad_Trabajo,
                                                       levels = c(0,1),
                                                       ordered = TRUE)
    
    df_ide$IyC04_Motivo_Localidad_Negocio   <- factor(df_ide$IyC04_Motivo_Localidad_Negocio,
                                                      levels = c(0,1),
                                                      ordered = TRUE)
    
    df_ide$IyC04_Motivo_Localidad_Oportunidad    <- factor(df_ide$IyC04_Motivo_Localidad_Oportunidad,
                                                           levels = c(0,1),
                                                           ordered = TRUE)
    
    df_ide$IyC04_Motivo_Localidad_Otro    <- factor(df_ide$IyC04_Motivo_Localidad_Otro,
                                                    levels = c(0,1),
                                                    ordered = TRUE)
    
    
    df_ide$IyC05_Acudo_Vecinos    <- factor(df_ide$IyC05_Acudo_Vecinos,
                                            levels = c(0,1),
                                            ordered = TRUE)
    
    df_ide$IyC05_Acudo_Familia    <- factor(df_ide$IyC05_Acudo_Familia,
                                            levels = c(0,1),
                                            ordered = TRUE)
    
    df_ide$IyC05_Acudo_Autoridad    <- factor(df_ide$IyC05_Acudo_Autoridad,
                                              levels = c(0,1),
                                              ordered = TRUE)
    
    
    df_ide$IyC05_Acudo_Iglesia    <- factor(df_ide$IyC05_Acudo_Iglesia,
                                            levels = c(0,1),
                                            ordered = TRUE)
    
    
    
    df_ide$IyC05_Acudo_Otro   <- factor(df_ide$IyC05_Acudo_Otro,
                                        levels = c(0,1),
                                        ordered = TRUE)
    
    df_ide$IyC06_Religion   <- factor(df_ide$IyC06_Religion,
                                      levels = c("CATOLICA","TESTIGOS","NINGUNO","OTRO","EVANGELICA","MORMON","ADVENTISTA"),
                                      ordered = TRUE)
    
    
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
    
    df_ide$IyC09_Emigrar    <- factor(df_ide$IyC09_Emigrar,
                                      levels = c("NO","LUGAR.DE.ORIGEN","QUIZAS","MISMO.ESTADO"),
                                      ordered = TRUE)
    
    df_ide$IyC10_Pertenencia    <- factor(df_ide$IyC10_Pertenencia,
                                          levels = c("ISLA.MUJERES","BENITO.JUAREZ","NINGUNO"),
                                          ordered = TRUE)
    
    
    df_ide$IyC11_Frecuencia_Cabecera_Isla    <- factor(df_ide$IyC11_Frecuencia_Cabecera_Isla,
                                                       levels = c("NUNCA","A.VECES","CASI.SIEMPRE","FRECUENTEMENTE","SIEMPRE"),
                                                       ordered = TRUE)
    
    
    
    
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
    
    df_ide$VI01_La_vivienda_es    <- factor(df_ide$VI01_La_vivienda_es,
                                            levels = c("IRREGULAR","RENTADA-PAGO-MENSUAL","PROPIA","PRESTADA"),
                                            ordered = TRUE)
    
    
    
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
      
      
      
      
      df_ide <- df_ide[, input$show_vars, drop = FALSE]
      
      if("Info.Sexo" %in% input$show_vars){
        if(length(input$Info.Sexo)!=0){
          df_ide <- dplyr::filter(df_ide, Info.Sexo %in% input$Info.Sexo)
        }
      }
      
      
      if("Info.Edad" %in% input$show_vars){
        if(length(input$Info.Edad)!=0){
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
          df_ide <- dplyr::filter(df_ide, V01_Casa_Adultos %in% input$V01_Casa_Adultos)
        }
      }
      
      
      if("V02_Casa_Ninos" %in% input$show_vars){
        if(length(input$V02_Casa_Ninos)!=0){
          df_ide <- dplyr::filter(df_ide, V02_Casa_Ninos %in% input$V02_Casa_Ninos)
        }
      }
      if("DF_kinder" %in% input$show_vars){
        if(length(input$DF_kinder)!=0){
          df_ide <- dplyr::filter(df_ide, DF_kinder %in% input$DF_kinder)
        }
      }
      
      if("DF_primaria" %in% input$show_vars){
        if(length(input$DF_primaria)!=0){
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
          df_ide <- dplyr::filter(df_ide, DF_bachillerato %in% input$DF_bachillerato)
        }
      }
      if("DF_licenciatura" %in% input$show_vars){
        if(length(input$DF_licenciatura)!=0){
          df_ide <- dplyr::filter(df_ide, DF_licenciatura %in% input$DF_licenciatura)
        }
      }
      
      
      if("DF_tr_autobus" %in% input$show_vars){
        if(length(input$DF_tr_autobus)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_autobus %in% input$DF_tr_autobus)
        }
      }
      if("DF_tr_colectivo" %in% input$show_vars){
        if(length(input$DF_tr_colectivo)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_colectivo %in% input$DF_tr_colectivo)
        }
      }
      
      if("DF_tr_taxi" %in% input$show_vars){
        if(length(input$DF_tr_taxi)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_taxi %in% input$DF_tr_taxi)
        }
      }
      if("DF_tr_mototaxi" %in% input$show_vars){
        if(length(input$DF_tr_mototaxi)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_mototaxi %in% input$DF_tr_mototaxi)
        }
      }
      
      
      if("DF_tr_moto" %in% input$show_vars){
        if(length(input$DF_tr_moto)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_moto %in% input$DF_tr_moto)
        }
      }
      if("DF_tr_auto" %in% input$show_vars){
        if(length(input$DF_tr_auto)!=0){
          df_ide <- dplyr::filter(df_ide, DF_tr_auto %in% input$DF_tr_auto)
        }
      }
      if("DF_v_col" %in% input$show_vars){
        if(length(input$DF_v_col)!=0){
          df_ide <- dplyr::filter(df_ide, DF_v_col %in% input$DF_v_col)
        }
      }
      if("DF_v_abarrote" %in% input$show_vars){
        if(length(input$DF_v_abarrote)!=0){
          df_ide <- dplyr::filter(df_ide, DF_v_abarrote %in% input$DF_v_abarrote)
        }
      }
      
      if("DF_v_super" %in% input$show_vars){
        if(length(input$DF_v_super)!=0){
          df_ide <- dplyr::filter(df_ide, DF_v_super %in% input$DF_v_super)
        }
      }
      if("DF_v_conv" %in% input$show_vars){
        if(length(input$DF_v_conv)!=0){
          df_ide <- dplyr::filter(df_ide, DF_v_conv %in% input$DF_v_conv)
        }
      }
      if("DF_v_plaza" %in% input$show_vars){
        if(length(input$DF_v_plaza)!=0){
          df_ide <- dplyr::filter(df_ide, DF_v_plaza %in% input$DF_v_plaza)
        }
      }
      if("DF_urg_casa" %in% input$show_vars){
        if(length(input$DF_urg_casa)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_casa %in% input$DF_urg_casa)
        }
      }
      
      if("DF_urg_partcom" %in% input$show_vars){
        if(length(input$DF_urg_partcom)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_partcom %in% input$DF_urg_partcom)
        }
      }
      if("DF_urg_partcan" %in% input$show_vars){
        if(length(input$DF_urg_partcan)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_partcan %in% input$DF_urg_partcan)
        }
      }
      if("DF_urg_hosp" %in% input$show_vars){
        if(length(input$DF_urg_hosp)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_hosp %in% input$DF_urg_hosp)
        }
      }
      if("DF_urg_cruz" %in% input$show_vars){
        if(length(input$DF_urg_cruz)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_cruz %in% input$DF_urg_cruz)
        }
      }
      
      
      if("DF_urg_farmacia" %in% input$show_vars){
        if(length(input$DF_urg_farmacia)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_farmacia %in% input$DF_urg_farmacia)
        }
      }
      if("DF_urg_otro" %in% input$show_vars){
        if(length(input$DF_urg_otro)!=0){
          df_ide <- dplyr::filter(df_ide, DF_urg_otro %in% input$DF_urg_otro)
        }
      }
      if("DF_a_parque" %in% input$show_vars){
        if(length(input$DF_a_parque)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_parque %in% input$DF_a_parque)
        }
      }
      if("DF_a_unidad" %in% input$show_vars){
        if(length(input$DF_a_unidad)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_unidad %in% input$DF_a_unidad)
        }
      }
      
      if("DF_a_jardines" %in% input$show_vars){
        if(length(DF_a_jardines)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_jardines %in% input$DF_a_jardines)
        }
      }
      if("DF_a_casa" %in% input$show_vars){
        if(length(input$DF_a_casa)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_casa %in% input$DF_a_casa)
        }
      }
      if("DF_a_biblioteca" %in% input$show_vars){
        if(length(input$DF_a_biblioteca)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_biblioteca %in% input$DF_a_biblioteca)
        }
      }
      if("DF_a_otro" %in% input$show_vars){
        if(length(input$DF_a_otro)!=0){
          df_ide <- dplyr::filter(df_ide, DF_a_otro %in% input$DF_a_otro)
        }
      }
      
      if("DE01_hogar_trabajan" %in% input$show_vars){
        if(length(input$DE01_hogar_trabajan)!=0){
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
          df_ide <- dplyr::filter(df_ide, DE07_sIngreso %in% input$DE07_sIngreso)
        }
      }
      if("DE08_tmp" %in% input$show_vars){
        if(length(input$DE08_tmp)!=0){
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
      
      if("IyC08_Ventajas_Casa" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Casa)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Casa %in% input$IyC08_Ventajas_Casa)
        }
      }
      if("IyC08_Ventajas_Trabajo" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Trabajo)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Trabajo %in% input$IyC08_Ventajas_Trabajo)
        }
      }
      if("IyC08_Ventajas_Familia" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Familia)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Familia %in% input$IyC08_Ventajas_Familia)
        }
      }
      if("IyC08_Ventajas_Tiempo" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Tiempo)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tiempo %in% input$IyC08_Ventajas_Tiempo)
        }
      }
      
      if("IyC08_Ventajas_Tranquilo" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Tranquilo)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tranquilo %in% input$IyC08_Ventajas_Tranquilo)
        }
      }
      if("IyC08_Ventajas_Seguro" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Seguro)!=0){
          df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Seguro %in% input$IyC08_Ventajas_Seguro)
        }
      }
      if("IyC08_Ventajas_Otro" %in% input$show_vars){
        if(length(input$IyC08_Ventajas_Otro)!=0){
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
          df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Asuntos_Admin %in% input$IyC12_Motivo_Viaja_Asuntos_Admin)
        }
      }
      
      if("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS" %in% input$show_vars){
        if(length(input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS)!=0){
          df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS %in% input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS)
        }
      }
      if("IyC12_Motivo_Viaja_Trabajo" %in% input$show_vars){
        if(length(input$IyC12_Motivo_Viaja_Trabajo)!=0){
          df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Trabajo %in% input$IyC12_Motivo_Viaja_Trabajo)
        }
      }
      if("IyC12_Motivio_Viaja_Recreacion" %in% input$show_vars){
        if(length(input$IyC12_Motivio_Viaja_Recreacion)!=0){
          df_ide <- dplyr::filter(df_ide, IyC12_Motivio_Viaja_Recreacion %in% input$IyC12_Motivio_Viaja_Recreacion)
        }
      }
      if("IyC12_Motivo_Viaja_Familia" %in% input$show_vars){
        if(length(input$IyC12_Motivo_Viaja_Familia)!=0){
          df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Familia %in% input$IyC12_Motivo_Viaja_Familia)
        }
      }
      
      if("IyC12_Motivo_Viaja_Otro" %in% input$show_vars){
        if(length(input$IyC12_Motivo_Viaja_Otro)!=0){
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
          df_ide <- dplyr::filter(df_ide, VI04_Contado %in% input$VI04_Contado)
        }
      }
      if("VI04_Herencia" %in% input$show_vars){
        if(length(input$VI04_Herencia)!=0){
          df_ide <- dplyr::filter(df_ide, VI04_Herencia %in% input$VI04_Herencia)
        }
      }
      
      if("VI04_Mensual" %in% input$show_vars){
        if(length(input$VI04_Mensual)!=0){
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
          df_ide <- dplyr::filter(df_ide, VI08NINUNDACIONES %in% input$VI08NINUNDACIONES)
        }
      }
      if("VI09PeHuracan" %in% input$show_vars){
        if(length(input$VI09PeHuracan)!=0){
          df_ide <- dplyr::filter(df_ide, VI09PeHuracan %in% input$VI09PeHuracan)
        }
      }
      if("VI14SabeZo0Riesgo" %in% input$show_vars){
        if(length(input$VI14SabeZo0Riesgo)!=0){
          df_ide <- dplyr::filter(df_ide, VI14SabeZo0Riesgo %in% input$VI14SabeZo0Riesgo)
        }
      }
      
      if("VI15SabeAfectaciones" %in% input$show_vars){
        if(length(input$VI15SabeAfectaciones)!=0){
          df_ide <- dplyr::filter(df_ide, VI15SabeAfectaciones %in% input$VI15SabeAfectaciones)
        }
      }
      if("AE1_AreasVerdes" %in% input$show_vars){
        if(length(input$AE1_AreasVerdes)!=0){
          df_ide <- dplyr::filter(df_ide, AE1_AreasVerdes %in% input$AE1_AreasVerdes)
        }
      }
      if("AE3_Banquetas" %in% input$show_vars){
        if(length(input$AE3_Banquetas)!=0){
          df_ide <- dplyr::filter(df_ide, AE3_Banquetas %in% input$AE3_Banquetas)
        }
      }
      if("AE4_Lumi0rias" %in% input$show_vars){
        if(length(input$AE4_Lumi0rias)!=0){
          df_ide <- dplyr::filter(df_ide, AE4_Lumi0rias %in% input$AE4_Lumi0rias)
        }
      }
      
      if("AE5_Transporte" %in% input$show_vars){
        if(length(input$AE5_Transporte)!=0){
          df_ide <- dplyr::filter(df_ide, AE5_Transporte %in% input$AE5_Transporte)
        }
      }
      if("AE6_Patrullas" %in% input$show_vars){
        if(length(input$AE6_Patrullas)!=0){
          df_ide <- dplyr::filter(df_ide, AE6_Patrullas %in% input$AE6_Patrullas)
        }
      }
      if("AE7_Lotes" %in% input$show_vars){
        if(length(input$AE7_Lotes)!=0){
          df_ide <- dplyr::filter(df_ide, AE7_Lotes %in% input$AE7_Lotes)
        }
      }
      if("AE9_PeSeguridad" %in% input$show_vars){
        if(length(input$AE9_PeSeguridad)!=0){
          df_ide <- dplyr::filter(df_ide, AE9_PeSeguridad %in% input$AE9_PeSeguridad)
        }
      }
      if("AE10_PeComodidad" %in% input$show_vars){
        if(length(input$AE10_PeComodidad)!=0){
          df_ide <- dplyr::filter(df_ide, AE10_PeComodidad %in% input$AE10_PeComodidad)
        }
      }
      if("AE11_PeRiesgo" %in% input$show_vars){
        if(length(input$AE11_PeRiesgo)!=0){
          df_ide <- dplyr::filter(df_ide, AE11_PeRiesgo %in% input$AE11_PeRiesgo)
        }
      }
      
      
      df_ide2 <- as(df_ide,"transactions")
      
      rules_s<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),
                                             maxtime=as.numeric(input$time), target = "rules"))
      
      rulesSorted_ide <- sort(rules_s, by = "confidence")
      gi <- generatingItemsets(rulesSorted_ide)
      d <- which(duplicated(gi))
      reglas_seleccionadas_ide <- rulesSorted_ide[-d]
      
    } else{
       df_ide <- df_ide[, input$show_vars, drop = FALSE]
      # 
      # 
      # rules<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),
      #                                      maxtime=as.numeric(input$time), target = "rules"))
      # 
      # rules <- sort(rules, by = "confidence")
      # #quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
      # 
      # ## non-redundant rules
      # rulesPruned_ide<- (rules[!is.redundant(rules)])
      # 
      # 
      # subset(rulesPruned_ide, subset = confidence == 1) %>% inspect
      # 
      # # Se eliminan esas reglas
      # reglas_seleccionadas_ide <- subset(rulesPruned_ide, subset = confidence < 1)
      # 
      # 
      # reglas_seleccionadas_ide <- subset(reglas_seleccionadas_ide, subset = lift > 1)
      # 
      # reglas_altura_ide <- subset(reglas_seleccionadas_ide, subset = lhs %pin% "hei")
      # 
      # mInteres_ide <- interestMeasure(reglas_seleccionadas_ide,
      #                                 measure = c("gini", "chiSquared"),
      #                                 transactions=df_ide)
      # quality(reglas_seleccionadas_ide) <- cbind(quality(reglas_seleccionadas_ide), mInteres_ide)
      # 
      # gi <- generatingItemsets(reglas_seleccionadas_ide)
      # d <- which(duplicated(gi))
      # reglas_seleccionadas_ide <- reglas_seleccionadas_ide[-d]
      # 
       
       df_ide2 <- as(df_ide,"transactions")
       
       rules_s<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),
                                              maxtime=as.numeric(input$time), target = "rules"))
       
       rulesSorted_ide <- sort(rules_s, by = "confidence")
       gi <- generatingItemsets(rulesSorted_ide)
       d <- which(duplicated(gi))
       reglas_seleccionadas_ide <- rulesSorted_ide[-d]
      
    }
    
    
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
  
  
  
  
  
  output$txt0 <- renderUI({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any(length(input$show_vars)>0)){
      word <- paste("<b>","FILTROS ","</b>")
    }else{
      word <-paste("")
    }
    HTML(paste(word))
  })
  
  output$txt1 <- renderUI({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('Info.Edad'==input$show_vars)){
      word <- paste("<b>","o La variable Edad esta dividida en intervalos para incorporarla en la regla de asociaciones: ","</b>",tags$ul(
        tags$li("18 - Joven"),
        tags$li("19 a 30 - Adulto Joven"),
        tags$li("31 a 60 - Adulto"),
        tags$li("61 o más - Tercera edad")))
    }else{
      word <-paste("")
    }
    HTML(paste(word))
  })
  
  output$txt2 <- renderText({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('DE05_ingreso_sem'==input$show_vars)){
      word <- paste("<b>","o El Ingreso semanal esta categorizado de la siguiente manera: ","</b>",tags$ul(
        tags$li("A - $0 a $1200"), 
        tags$li("B - $1201 a $2500"), 
        tags$li("C - $2501 a $5000"), 
        tags$li("D - $Más de $5000")))
    }else{
      word <- paste("")
    }
    HTML(paste(word))
  })
  
  output$txt3 <- renderText({
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('DE08_tmp'==input$show_vars)){
      word <- paste("<b>","o El tiempo que tarda el jefe de familia en llegar a su trabajo estÃ¡ categorizado de la siguiente manera: ","</b>",tags$ul(
        tags$li("A - Trabaja en casa"), 
        tags$li("B - Menos de 20 min"), 
        tags$li("C - Más de 20 min y menos de 1 hora"), 
        tags$li("D - Entre 1 y 2 horas"), 
        tags$li("E - Más de 2 horas")))
    }else{
      word <-paste("")
    }
    HTML(paste(word))
  })
  
  output$txt4 <- renderUI({
    word <- paste("<b>",tags$i("o Descripción del grafo:"),"</b>",tags$i("Los nodos del grafo indican las reglas de asociación que se obtienen con las columnas seleccionadas, 
                 estas se conectan por medio de flechas que van del antecedente al consecuente. En el medio de estas flechas de conexiones, podemos encontrar un c??rculo cuyo tam. 
                                                                         representa el valor de soporte o frecuencia de la regla mientras que el color representa la confianza. 
                                                                         Podemos ver que la confianza se representa desde rosa pálido hasta el rojo, siendo este último el valor más alto."))
    HTML(paste(word))
    
  })
  
  output$txt5 <- renderUI({
    income_rules <-rules()
    
    if(any(length(input$show_vars)<2)){ 
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else if(length(income_rules)==0 ){
      word <- paste(tags$i("Error: Es necesario reducir ya sea la frecuencia o la confianza en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
    HTML(paste(word))
  })
  
  output$txt6 <- renderUI({
    income_rules <-rules()
    
    if(any(length(input$show_vars)<2)){ 
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else if(length(income_rules)==0 ){
      word <- paste(tags$i("Error: Es necesario reducir ya sea la frecuencia o la confianza en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
    HTML(paste(word))
  })
  
  
  
  output$rows1 <- renderUI({
    
    if("Info.Sexo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$Info.Sexo    <- factor(df_ide$Info.Sexo,levels = c("H", "M"), ordered = TRUE)
      
      
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i]) }
      
      
      updateSelectInput(getDefaultReactiveDomain(),"Info.Sexo","Sexo", choices = unique(df_ide$Info.Sexo))
      selectInput("Info.Sexo", "Sexo",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows2 <- renderUI({
    if("Info.Edad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$Info.Edad <- factor(df_ide$Info.Edad, 
                                 levels = c("Joven", "Adulto-Joven", "Adulto", "Tercera-Edad"),
                                 ordered = TRUE)
      
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide$Info.Edad))
      selectInput("Info.Edad", "Edad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows3 <- renderUI({
    if("Direccion.Calle" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Calle","Calle", choices = unique(df_ide$Direccion.Calle))
      selectInput("Direccion.Calle", "Calle", choices = NULL,multiple = TRUE)}
  })
  
  output$rows4 <- renderUI({
    if("Direccion.Numero.Exterior" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Numero.Exterior","N.Exterior", choices = unique(df_ide$Direccion.Numero.Exterior))
      selectInput("Direccion.Numero.Exterior", "N.Exterior", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows5 <- renderUI({
    if("Direccion.Numero.Interior" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Numero.Interior","N.Interior", choices = unique(df_ide$Direccion.Numero.Interior))
      selectInput("Direccion.Numero.Interior", "N.Interior",choices = NULL, multiple = TRUE)}
  })
  
  output$rows6 <- renderUI({
    if("Direccion.Manza0" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Manza0","Mz", choices = unique(df_ide$Direccion.Manza0))
      selectInput("Direccion.Manza0", "Mz", choices = NULL,multiple = TRUE)}
  })
  
  output$rows7 <- renderUI({
    if("Direccion.Super.Manza0" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Super.Manza0","Smz", choices = unique(df_ide$Direccion.Super.Manza0))
      selectInput("Direccion.Super.Manza0", "Smz", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows8 <- renderUI({
    if("Direccion.Colonia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Colonia","Colonia", choices = unique(df_ide$Direccion.Colonia))
      selectInput("Direccion.Colonia", "Colonia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows9 <- renderUI({
    if("Direccion.Codigo.Postal" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"Direccion.Codigo.Postal","Codigo.P", choices = unique(df_ide$Direccion.Codigo.Postal))
      selectInput("Direccion.Codigo.Postal", "Codigo.P", choices = NULL,multiple = TRUE)}
  })
  
  output$rows10 <- renderUI({
    if("V01_Casa_Adultos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$V01_Casa_Adultos    <- factor(df_ide$V01_Casa_Adultos,
                                           levels = c("1", "2", "3", "4", "5", "6", "7", "13"),
                                           ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"V01_Casa_Adultos","Num.Adultos", choices = unique(df_ide$V01_Casa_Adultos))
      selectInput("V01_Casa_Adultos", "Num.Adultos", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows11 <- renderUI({
    if("V02_Casa_Ninos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$V02_Casa_Ninos    <- factor(df_ide$V02_Casa_Ninos,
                                         levels = c("0", "1", "2", "3", "4", "5", "6", "7"),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"V02_Casa_Ninos","Num.Ninos", choices = unique(df_ide$V02_Casa_Ninos))
      selectInput("V02_Casa_Ninos", "Num.Ninos",choices = NULL, multiple = TRUE)}
  })
  
  output$rows12 <- renderUI({
    if("DF_kinder" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_kinder    <- factor(df_ide$DF_kinder,
                                    levels = c(0,1,2),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_kinder","Cuantos.Kinder", choices = unique(df_ide$DF_kinder))
      selectInput("DF_kinder", "Cuantos.Kinder", choices = NULL,multiple = TRUE)}
  })
  
  output$rows13 <- renderUI({
    if("DF_primaria" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_primaria    <- factor(df_ide$DF_primaria,
                                      levels = c(0,1,2,3,5),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_primaria","Cuantos.Primaria", choices = unique(df_ide$DF_primaria))
      selectInput("DF_primaria", "Cuantos.Primaria", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows14 <- renderUI({
    if("DF_secundaria" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_secundaria    <- factor(df_ide$DF_secundaria,
                                        levels = c(0,1,2,3,4),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_secundaria","Cuantos.Secundaria", choices = unique(df_ide$DF_secundaria))
      selectInput("DF_secundaria", "Cuantos.Secundaria",choices = NULL, multiple = TRUE)}
  })
  
  output$rows15 <- renderUI({
    if("DF_bachillerato" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_bachillerato   <- factor(df_ide$DF_bachillerato,
                                         levels = c(0,1,2,3,4),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_bachillerato","Cuantos.Bachillerato", choices = unique(df_ide$DF_bachillerato))
      selectInput("DF_bachillerato", "Cuantos.Bachillerato", choices = NULL,multiple = TRUE)}
  })
  
  output$rows16 <- renderUI({
    if("DF_licenciatura" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_licenciatura    <- factor(df_ide$DF_licenciatura,
                                          levels = c(0,1,2,3),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_licenciatura","Cuantos.Licenciatura", choices = unique(df_ide$DF_licenciatura))
      selectInput("DF_licenciatura", "Cuantos.Licenciatura", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows17 <- renderUI({
    if("DF_tr_autobus" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_autobus   <- factor(df_ide$DF_tr_autobus,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_autobus","Autobus", choices = unique(df_ide$DF_tr_autobus))
      selectInput("DF_tr_autobus", "Autobus",choices = NULL, multiple = TRUE)}
  })
  
  output$rows18 <- renderUI({
    if("DF_tr_colectivo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_colectivo    <- factor(df_ide$DF_tr_colectivo,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_colectivo","Colectivo", choices = unique(df_ide$DF_tr_colectivo))
      selectInput("DF_tr_colectivo", "Colectivo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows19 <- renderUI({
    if("DF_tr_taxi" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_taxi   <- factor(df_ide$DF_tr_taxi,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_taxi","Taxi", choices = unique(df_ide$DF_tr_taxi))
      selectInput("DF_tr_taxi", "Taxi", choices = NULL,multiple = TRUE)}
  })
  
  output$rows20 <- renderUI({
    if("DF_tr_mototaxi" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_mototaxi   <- factor(df_ide$DF_tr_mototaxi,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_mototaxi","Mototaxi", choices = unique(df_ide$DF_tr_mototaxi))
      selectInput("DF_tr_mototaxi", "Mototaxi", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows21 <- renderUI({
    if("DF_tr_moto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_moto    <- factor(df_ide$DF_tr_moto,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_moto","Moto", choices = unique(df_ide$DF_tr_moto))
      selectInput("DF_tr_moto", "Moto",choices = NULL, multiple = TRUE)}
  })
  
  output$rows22 <- renderUI({
    if("DF_tr_auto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_tr_auto    <- factor(df_ide$DF_tr_auto,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_tr_auto","Auto", choices = unique(df_ide$DF_tr_auto))
      selectInput("DF_tr_auto", "Auto", choices = NULL,multiple = TRUE)}
  })
  
  output$rows23 <- renderUI({
    if("DF_v_col" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_v_col    <- factor(df_ide$DF_v_col,
                                   levels = c(0,1),
                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_col","Mercado.Colonia", choices = unique(df_ide$DF_v_col))
      selectInput("DF_v_col", "Mercado.Colonia", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows24 <- renderUI({
    if("DF_v_abarrote" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_v_abarrote    <- factor(df_ide$DF_v_abarrote,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_abarrote","Abarrote", choices = unique(df_ide$DF_v_abarrote))
      selectInput("DF_v_abarrote", "Abarrote", choices = NULL,multiple = TRUE)}
  })
  
  output$rows25 <- renderUI({
    if("DF_v_super" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_v_super   <- factor(df_ide$DF_v_super,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_super","Super", choices = unique(df_ide$DF_v_super))
      selectInput("DF_v_super", "Super",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows26 <- renderUI({
    if("DF_v_conv" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_v_conv    <- factor(df_ide$DF_v_conv,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_conv","Tienda.conveniencia", choices = unique(df_ide$DF_v_conv))
      selectInput("DF_v_conv", "Tienda.conveniencia", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows27 <- renderUI({
    if("DF_v_plaza" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_v_plaza    <- factor(df_ide$DF_v_plaza,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_v_plaza","Plaza", choices = unique(df_ide$DF_v_plaza))
      selectInput("DF_v_plaza", "Plaza",choices = NULL, multiple = TRUE)}
  })
  
  output$rows28 <- renderUI({
    if("DF_urg_casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_casa    <- factor(df_ide$DF_urg_casa,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_casa","Emergencia.En.Casa", choices = unique(df_ide$DF_urg_casa))
      selectInput("DF_urg_casa", "Emergencia.En.Casa",choices = NULL, multiple = TRUE)}
  })
  
  output$rows29 <- renderUI({
    if("DF_urg_partcom" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_partcom    <- factor(df_ide$DF_urg_partcom,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_partcom","Emergencia.Medico.Part.Comunidad", choices = unique(df_ide$DF_urg_partcom))
      selectInput("DF_urg_partcom", "Emergencia.Medico.Part.Comunidad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows30 <- renderUI({
    if("DF_urg_partcan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_partcan   <- factor(df_ide$DF_urg_partcan,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_partcan","Emergencia.Medico.Part.Cancun", choices = unique(df_ide$DF_urg_partcan))
      selectInput("DF_urg_partcan", "Emergencia.Medico.Part.Cancun", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows31 <- renderUI({
    if("DF_urg_hosp" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_hosp    <- factor(df_ide$DF_urg_hosp,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_hosp","Emergencia.Hospital", choices = unique(df_ide$DF_urg_hosp))
      selectInput("DF_urg_hosp", "Emergencia.Hospital",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows32 <- renderUI({
    if("DF_urg_cruz" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_cruz   <- factor(df_ide$DF_urg_cruz,
                                     levels = c(0,1),
                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_cruz","Emergencia.CruzR", choices = unique(df_ide$DF_urg_cruz))
      selectInput("DF_urg_cruz", "Emergencia.CruzR",choices = NULL, multiple = TRUE)}
  })
  
  output$rows33 <- renderUI({
    if("DF_urg_farmacia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_farmacia    <- factor(df_ide$DF_urg_farmacia,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_farmacia","Emergencia.Farmacia", choices = unique(df_ide$DF_urg_farmacia))
      selectInput("DF_urg_farmacia", "Emergencia.Farmacia", choices = NULL,multiple = TRUE)}
  })
  
  output$rows34 <- renderUI({
    if("DF_urg_otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_urg_otro    <- factor(df_ide$DF_urg_otro,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_urg_otro","Emergencia.Otro", choices = unique(df_ide$DF_urg_otro))
      selectInput("DF_urg_otro", "Emergencia.Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows35 <- renderUI({
    if("DF_a_parque" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_parque    <- factor(df_ide$DF_a_parque,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_parque","Colonia.Con.Parque", choices = unique(df_ide$DF_a_parque))
      selectInput("DF_a_parque", "Colonia.Con.Parque",choices = NULL, multiple = TRUE)}
  })
  
  output$rows36 <- renderUI({
    if("DF_a_unidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_unidad    <- factor(df_ide$DF_a_unidad,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_unidad","Colonia.Con.Uni.Dep", choices = unique(df_ide$DF_a_unidad))
      selectInput("DF_a_unidad", "Colonia.Con.Uni.Dep", choices = NULL,multiple = TRUE)}
  })
  
  output$rows37 <- renderUI({
    if("DF_a_jardines" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_jardines    <- factor(df_ide$DF_a_jardines,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_jardines","Colonia.Con.Jardin", choices = unique(df_ide$DF_a_jardines))
      selectInput("DF_a_jardines", "Colonia.Con.Jardin", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows38 <- renderUI({
    if("DF_a_casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_casa   <- factor(df_ide$DF_a_casa,
                                   levels = c(0,1),
                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_casa","Colonia.Con.Centro.Comunitario", choices = unique(df_ide$DF_a_casa))
      selectInput("DF_a_casa", "Colonia.Con.Centro.Comunitario",choices = NULL, multiple = TRUE)}
  })
  
  output$rows39 <- renderUI({
    if("DF_a_biblioteca" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_biblioteca    <- factor(df_ide$DF_a_biblioteca,
                                          levels = c(0,1),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_biblioteca","Colonia.Con.Biblio", choices = unique(df_ide$DF_a_biblioteca))
      selectInput("DF_a_biblioteca", "Colonia.Con.Biblio", choices = NULL,multiple = TRUE)}
  })
  
  output$rows40 <- renderUI({
    if("DF_a_otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DF_a_otro    <- factor(df_ide$DF_a_otro,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DF_a_otro","Colonia.Con.Otro", choices = unique(df_ide$DF_a_otro))
      selectInput("DF_a_otro", "Colonia.Con.Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows41 <- renderUI({
    if("DE01_hogar_trabajan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DE01_hogar_trabajan    <- factor(df_ide$DE01_hogar_trabajan,
                                              levels = c(0,1,2,3,4,5),
                                              ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE01_hogar_trabajan","Cuantas.Personas.Trabajan", choices = unique(df_ide$DE01_hogar_trabajan))
      selectInput("DE01_hogar_trabajan", "Cuantas.Personas.Trabajan",choices = NULL, multiple = TRUE)}
  })
  
  output$rows42 <- renderUI({
    if("DE02_trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE02_trabajo","Jefe.Trabajo", choices = unique(df_ide$DE02_trabajo))
      selectInput("DE02_trabajo", "Jefe.Trabajo", choices = NULL,multiple = TRUE)}
  })
  
  output$rows43 <- renderUI({
    if("DE03_puesto" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE03_puesto","Jefe.Puesto", choices = unique(df_ide$DE03_puesto))
      selectInput("DE03_puesto", "Jefe.Puesto", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows44 <- renderUI({
    if("DE05_ingreso_sem" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DE05_ingreso_sem   <- factor(df_ide$DE05_ingreso_sem,
                                          levels = c("A","B","C","D"),
                                          ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE05_ingreso_sem","Ingreso.Sem", choices = unique(df_ide$DE05_ingreso_sem))
      selectInput("DE05_ingreso_sem", "Ingreso.Sem",choices = NULL, multiple = TRUE)}
  })
  
  output$rows45 <- renderUI({
    if("DE06_esc_jefe" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE06_esc_jefe","Jefe.Escolaridad", choices = unique(df_ide$DE06_esc_jefe))
      selectInput("DE06_esc_jefe", "Jefe.Escolaridad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows46 <- renderUI({
    if("DE07_sIngreso" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DE07_sIngreso   <- factor(df_ide$DE07_sIngreso,
                                       levels = c(0,1,2,3,4,5),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE07_sIngreso","Cuantos.Sin.Ingreso", choices = unique(df_ide$DE07_sIngreso))
      selectInput("DE07_sIngreso", "Cuantos.Sin.Ingreso", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows47 <- renderUI({
    if("DE08_tmp" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$DE08_tmp   <- factor(df_ide$DE08_tmp,
                                  levels = c("A","B","C","D","E"),
                                  ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE08_tmp","Tiempo.A.Trabajo", choices = unique(df_ide$DE08_tmp))
      selectInput("DE08_tmp", "Tiempo.A.Trabajo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows48 <- renderUI({
    if("DE09_col" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE09_col","Colonia.Trabajo", choices = unique(df_ide$DE09_col))
      selectInput("DE09_col", "Colonia.Trabajo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows49 <- renderUI({
    if("DE10_mun" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"DE10_mun","Municipio.Trabajo", choices = unique(df_ide$DE10_mun))
      selectInput("DE10_mun", "Municipio.Trabajo", choices = NULL,multiple = TRUE)}
  })
  
  output$rows50 <- renderUI({
    if("IyC02_Estado_Origen" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC02_Estado_Origen","Estado.Origen", choices = unique(df_ide$IyC02_Estado_Origen))
      selectInput("IyC02_Estado_Origen", "Estado.Origen", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows51 <- renderUI({
    if("IyC03_Tiempo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC03_Tiempo","Tiempo.En.Isla", choices = unique(df_ide$IyC03_Tiempo))
      selectInput("IyC03_Tiempo", "Tiempo.En.Isla",choices = NULL, multiple = TRUE)}
  })
  
  output$rows52 <- renderUI({
    if("IyC04_Motivo_Localidad_Parientes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Parientes","Motivo.Localidad.Parientes", choices = unique(df_ide$IyC04_Motivo_Localidad_Parientes))
      selectInput("IyC04_Motivo_Localidad_Parientes", "Motivo.Localidad.Parientes", choices = NULL,multiple = TRUE)}
  })
  
  output$rows53 <- renderUI({
    if("IyC04_Motivo_Localidad_Amigos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Amigos","Motivo.Localidad.Amigos", choices = unique(df_ide$IyC04_Motivo_Localidad_Amigos))
      selectInput("IyC04_Motivo_Localidad_Amigos", "Motivo.Localidad.Amigos", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows54 <- renderUI({
    if("IyC04_Motivo_Localidad_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Trabajo","Motivo.Localidad.Trabajo", choices = unique(df_ide$IyC04_Motivo_Localidad_Trabajo))
      selectInput("IyC04_Motivo_Localidad_Trabajo", "Motivo.Localidad.Trabajo", choices = NULL,multiple = TRUE)}
  })
  
  output$rows55 <- renderUI({
    if("IyC04_Motivo_Localidad_Negocio" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Negocio","Motivo.Localidad.Negocio", choices = unique(df_ide$IyC04_Motivo_Localidad_Negocio))
      selectInput("IyC04_Motivo_Localidad_Negocio", "Motivo.Localidad.Negocio",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows56 <- renderUI({
    if("IyC04_Motivo_Localidad_Oportunidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Oportunidad","Motivo.Localidad.Oportunidad", choices = unique(df_ide$IyC04_Motivo_Localidad_Oportunidad))
      selectInput("IyC04_Motivo_Localidad_Oportunidad", "Motivo.Localidad.Oportunidad",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows57 <- renderUI({
    if("IyC04_Motivo_Localidad_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC04_Motivo_Localidad_Otro","Motivo.Localidad.Otro", choices = unique(df_ide$IyC04_Motivo_Localidad_Otro))
      selectInput("IyC04_Motivo_Localidad_Otro", "Motivo.Localidad.Otro",choices = NULL, multiple = TRUE)}
  })
  
  output$rows58 <- renderUI({
    if("IyC05_Acudo_Vecinos" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Vecinos","Acudo.Vecinos", choices = unique(df_ide$IyC05_Acudo_Vecinos))
      selectInput("IyC05_Acudo_Vecinos", "Acudo.Vecinos", choices = NULL,multiple = TRUE)}
  })
  
  output$rows59 <- renderUI({
    if("IyC05_Acudo_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Familia","Acudo.Familia", choices = unique(df_ide$IyC05_Acudo_Familia))
      selectInput("IyC05_Acudo_Familia", "Acudo.Familia", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows60 <- renderUI({
    if("IyC05_Acudo_Autoridad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Autoridad","Acudo.Autoridad", choices = unique(df_ide$IyC05_Acudo_Autoridad))
      selectInput("IyC05_Acudo_Autoridad", "Acudo.Autoridad",choices = NULL, multiple = TRUE)}
  })
  
  output$rows61 <- renderUI({
    if("IyC05_Acudo_Iglesia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Iglesia","Acudo.Iglesia", choices = unique(df_ide$IyC05_Acudo_Iglesia))
      selectInput("IyC05_Acudo_Iglesia", "Acudo.Iglesia",choices = NULL, multiple = TRUE)}
  })
  
  
  
  
  
  
  output$rows62 <- renderUI({
    if("IyC05_Acudo_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC05_Acudo_Otro","Acudo.Otro", choices = unique(df_ide$IyC05_Acudo_Otro))
      selectInput("IyC05_Acudo_Otro", "Acudo.Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows63 <- renderUI({
    if("IyC06_Religion" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC06_Religion","Religion", choices = unique(df_ide$IyC06_Religion))
      selectInput("IyC06_Religion", "Religion",choices = NULL, multiple = TRUE)}
  })
  
  
  
  #output$rows64 <- renderUI({
  # if("IyC07_Costumbres" %in% input$show_vars){
  #    df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
  #    for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
  #    updateSelectInput(getDefaultReactiveDomain(),"IyC07_Costumbres","IyC07_Costumbres", choices = unique(df_ide$IyC07_Costumbres))
  #    selectInput("IyC07_Costumbres", "IyC07_Costumbres",choices = NULL, multiple = TRUE)}
  #})
  
  output$rows65 <- renderUI({
    if("IyC08_Ventajas_Casa" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Casa","Ventajas.Casa", choices = unique(df_ide$IyC08_Ventajas_Casa))
      df_ide$IyC08_Ventajas_Casa    <- factor(df_ide$IyC08_Ventajas_Casa,
                                              levels = c(0,1),
                                              ordered = TRUE)
      selectInput("IyC08_Ventajas_Casa", "Ventajas.Casa", choices = NULL,multiple = TRUE)}
  })
  
  output$rows66 <- renderUI({
    if("IyC08_Ventajas_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Trabajo    <- factor(df_ide$IyC08_Ventajas_Trabajo,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Trabajo","Ventajas.Trabajo", choices = unique(df_ide$IyC08_Ventajas_Trabajo))
      selectInput("IyC08_Ventajas_Trabajo", "Ventajas.Trabajo", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows67 <- renderUI({
    if("IyC08_Ventajas_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Familia    <- factor(df_ide$IyC08_Ventajas_Familia,
                                                 levels = c(0,1),
                                                 ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Familia","Ventajas.Familia", choices = unique(df_ide$IyC08_Ventajas_Familia))
      selectInput("IyC08_Ventajas_Familia", "Ventajas.Familia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows68 <- renderUI({
    if("IyC08_Ventajas_Tiempo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Tiempo   <- factor(df_ide$IyC08_Ventajas_Tiempo,
                                               levels = c(0,1),
                                               ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Tiempo","Ventajas.Tiempo", choices = unique(df_ide$IyC08_Ventajas_Tiempo))
      selectInput("IyC08_Ventajas_Tiempo", "Ventajas.Tiempo",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows69 <- renderUI({
    if("IyC08_Ventajas_Tranquilo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Tranquilo    <- factor(df_ide$IyC08_Ventajas_Tranquilo,
                                                   levels = c(0,1),
                                                   ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Tranquilo","Ventajas.Tranquilo", choices = unique(df_ide$IyC08_Ventajas_Tranquilo))
      selectInput("IyC08_Ventajas_Tranquilo", "Ventajas.Tranquilo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows70 <- renderUI({
    if("IyC08_Ventajas_Seguro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Seguro    <- factor(df_ide$IyC08_Ventajas_Seguro,
                                                levels = c(0,1),
                                                ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Seguro","Ventajas.Seguro", choices = unique(df_ide$IyC08_Ventajas_Seguro))
      selectInput("IyC08_Ventajas_Seguro", "Ventajas.Seguro", choices = NULL,multiple = TRUE)}
  })
  
  output$rows71 <- renderUI({
    if("IyC08_Ventajas_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC08_Ventajas_Otro    <- factor(df_ide$IyC08_Ventajas_Otro,
                                              levels = c(0,1),
                                              ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC08_Ventajas_Otro","Ventajas.Otro", choices = unique(df_ide$IyC08_Ventajas_Otro))
      selectInput("IyC08_Ventajas_Otro", "Ventajas.Otro", choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows72 <- renderUI({
    if("IyC09_Emigrar" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC09_Emigrar","Emigrar", choices = unique(df_ide$IyC09_Emigrar))
      selectInput("IyC09_Emigrar", "Emigrar", choices = NULL,multiple = TRUE)}
  })
  
  
  output$rows73 <- renderUI({
    if("IyC10_Pertenencia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC10_Pertenencia","Pertenencia", choices = unique(df_ide$IyC10_Pertenencia))
      selectInput("IyC10_Pertenencia", "Pertenencia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows74 <- renderUI({
    if("IyC11_Frecuencia_Cabecera_Isla" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC11_Frecuencia_Cabecera_Isla","Frec.Visita.Cabecera_Isla", choices = unique(df_ide$IyC11_Frecuencia_Cabecera_Isla))
      selectInput("IyC11_Frecuencia_Cabecera_Isla", "Frec.Visita.Cabecera_Isla",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows75 <- renderUI({
    if("IyC12_Motivo_Viaja_Asuntos_Admin" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivo_Viaja_Asuntos_Admin    <- factor(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin,
                                                           levels = c(0,1),
                                                           ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Asuntos_Admin","Motivo.Viaja.Asuntos.Admin", choices = unique(df_ide$IyC12_Motivo_Viaja_Asuntos_Admin))
      selectInput("IyC12_Motivo_Viaja_Asuntos_Admin", "Motivo.Viaja.Asuntos.Admin", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows76 <- renderUI({
    if("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS    <- factor(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS,
                                                                     levels = c(0,1),
                                                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS","Motivo.Viaja.Pago.Servicio", choices = unique(df_ide$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS))
      selectInput("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS", "Motivo.Viaja.Pago.Servicio",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows77 <- renderUI({
    if("IyC12_Motivo_Viaja_Trabajo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivo_Viaja_Trabajo   <- factor(df_ide$IyC12_Motivo_Viaja_Trabajo,
                                                    levels = c(0,1),
                                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Trabajo","Motivo.Viaja.Trabajo", choices = unique(df_ide$IyC12_Motivo_Viaja_Trabajo))
      selectInput("IyC12_Motivo_Viaja_Trabajo", "Motivo.Viaja.Trabajo",choices = NULL, multiple = TRUE)}
  })
  
  output$rows78 <- renderUI({
    if("IyC12_Motivio_Viaja_Recreacion" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivio_Viaja_Recreacion    <- factor(df_ide$IyC12_Motivio_Viaja_Recreacion,
                                                         levels = c(0,1),
                                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivio_Viaja_Recreacion","Motivio.Viaja.Recreacion", choices = unique(df_ide$IyC12_Motivio_Viaja_Recreacion))
      selectInput("IyC12_Motivio_Viaja_Recreacion", "Motivio.Viaja.Recreacion", choices = NULL,multiple = TRUE)}
  })
  
  output$rows79 <- renderUI({
    if("IyC12_Motivo_Viaja_Familia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivo_Viaja_Familia    <- factor(df_ide$IyC12_Motivo_Viaja_Familia,
                                                     levels = c(0,1),
                                                     ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Familia","Motivo.Viaja.Familia", choices = unique(df_ide$IyC12_Motivo_Viaja_Familia))
      selectInput("IyC12_Motivo_Viaja_Familia", "Motivo.Viaja.Familia", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows80 <- renderUI({
    if("IyC12_Motivo_Viaja_Otro" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$IyC12_Motivo_Viaja_Otro    <- factor(df_ide$IyC12_Motivo_Viaja_Otro,
                                                  levels = c(0,1),
                                                  ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"IyC12_Motivo_Viaja_Otro","Motivo.Viaja.Otro", choices = unique(df_ide$IyC12_Motivo_Viaja_Otro))
      selectInput("IyC12_Motivo_Viaja_Otro", "Motivo.Viaja.Otro",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows81 <- renderUI({
    if("VI01_La_vivienda_es" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI01_La_vivienda_es","La.Vivienda.Es", choices = unique(df_ide$VI01_La_vivienda_es))
      selectInput("VI01_La_vivienda_es", "La.Vivienda.Es",choices = NULL, multiple = TRUE)}
  })
  
  output$rows82 <- renderUI({
    if("VI04_Contado" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI04_Contado   <- factor(df_ide$VI04_Contado,
                                      levels = c(0,1),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Contado","Vivienda.Contado", choices = unique(df_ide$VI04_Contado))
      selectInput("VI04_Contado", "Vivienda.Contado",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows83 <- renderUI({
    if("VI04_Herencia" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI04_Herencia    <- factor(df_ide$VI04_Herencia,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Herencia","Vivienda.Herencia", choices = unique(df_ide$VI04_Herencia))
      selectInput("VI04_Herencia", "Vivienda.Herencia",choices = NULL, multiple = TRUE)}
  })
  
  output$rows84 <- renderUI({
    if("VI01_Prestada" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI01_Prestada","Vivienda.Prestada", choices = unique(df_ide$VI01_Prestada))
      selectInput("VI01_Prestada", "Vivienda.Prestada", choices = NULL,multiple = TRUE)}
  })
  
  output$rows85 <- renderUI({
    if("VI04_Mensual" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI04_Mensual    <- factor(df_ide$VI04_Mensual,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI04_Mensual","Vivienda.Mensual", choices = unique(df_ide$VI04_Mensual))
      selectInput("VI04_Mensual", "Vivienda.Mensual", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows86 <- renderUI({
    if("VI05_CPropiaMensual" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI05_CPropiaMensual","C.Propia.Mensual", choices = unique(df_ide$VI05_CPropiaMensual))
      selectInput("VI05_CPropiaMensual", "C.Propia.Mensual", choices = NULL,multiple = TRUE)}
  })
  
  
  output$rows87 <- renderUI({
    if("VI06_CPropiaAdquirida" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI06_CPropiaAdquirida","C.Propia.Adquirida", choices = unique(df_ide$VI06_CPropiaAdquirida))
      selectInput("VI06_CPropiaAdquirida", "C.Propia.Adquirida",choices = NULL, multiple = TRUE)}
  })
  
  # output$rows88 <- renderUI({
  #   if("VI07_FALTA_DE_SERVICIOS" %in% input$show_vars){
  #     df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
  #     for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
  #     updateSelectInput(getDefaultReactiveDomain(),"VI07_FALTA_DE_SERVICIOS","VI07_FALTA_DE_SERVICIOS", choices = unique(df_ide$VI07_FALTA_DE_SERVICIOS))
  #     selectInput("VI07_FALTA_DE_SERVICIOS", "VI07_FALTA_DE_SERVICIOS",choices = NULL, multiple = TRUE)}
  # })
  
  
  output$rows89 <- renderUI({
    if("VI08NINUNDACIONES" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI08NINUNDACIONES    <- factor(df_ide$VI08NINUNDACIONES,
                                            levels = c(0,1),
                                            ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI08NINUNDACIONES","Pasado.Inundaciones", choices = unique(df_ide$VI08NINUNDACIONES))
      selectInput("VI08NINUNDACIONES", "Pasado.Inundaciones", choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows90 <- renderUI({
    if("VI09PeHuracan" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI09PeHuracan    <- factor(df_ide$VI09PeHuracan,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI09PeHuracan","Pasado.Huracan", choices = unique(df_ide$VI09PeHuracan))
      selectInput("VI09PeHuracan", "Pasado.Huracan",choices = NULL, multiple = TRUE)}
  })
  
  output$rows91 <- renderUI({
    if("VI14SabeZo0Riesgo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI14SabeZo0Riesgo    <- factor(df_ide$VI14SabeZo0Riesgo,
                                            levels = c(0,1),
                                            ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI14SabeZo0Riesgo","Sabe.Zo.Riesgo", choices = unique(df_ide$VI14SabeZo0Riesgo))
      selectInput("VI14SabeZo0Riesgo", "Sabe.Zo.Riesgo",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows92 <- renderUI({
    if("VI15SabeAfectaciones" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$VI15SabeAfectaciones    <- factor(df_ide$VI15SabeAfectaciones,
                                               levels = c(0,1),
                                               ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"VI15SabeAfectaciones","Sabe.Afectaciones", choices = unique(df_ide$VI15SabeAfectaciones))
      selectInput("VI15SabeAfectaciones", "Sabe.Afectaciones",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows93 <- renderUI({
    if("AE1_AreasVerdes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE1_AreasVerdes   <- factor(df_ide$AE1_AreasVerdes,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE1_AreasVerdes","Obs.AreasVerdes", choices = unique(df_ide$AE1_AreasVerdes))
      selectInput("AE1_AreasVerdes", "Obs.AreasVerdes",choices = NULL, multiple = TRUE)}
  })
  
  output$rows94 <- renderUI({
    if("AE3_Banquetas" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE3_Banquetas    <- factor(df_ide$AE3_Banquetas,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE3_Banquetas","Obs.Banquetas", choices = unique(df_ide$AE3_Banquetas))
      selectInput("AE3_Banquetas", "Obs.Banquetas", choices = NULL,multiple = TRUE)}
  })
  
  output$rows95 <- renderUI({
    if("AE4_Lumi0rias" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE4_Lumi0rias    <- factor(df_ide$AE4_Lumi0rias,
                                        levels = c(0,1),
                                        ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE4_Lumi0rias","Obs.Luminarias", choices = unique(df_ide$AE4_Lumi0rias))
      selectInput("AE4_Lumi0rias", "Obs.Luminarias", choices = NULL, multiple = TRUE)}
  })
  
  output$rows96 <- renderUI({
    if("AE5_Transporte" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE5_Transporte    <- factor(df_ide$AE5_Transporte,
                                         levels = c(0,1),
                                         ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE5_Transporte","Obs.Transporte", choices = unique(df_ide$AE5_Transporte))
      selectInput("AE5_Transporte", "Obs.Transporte",choices = NULL, multiple = TRUE)}
  })
  
  output$rows97 <- renderUI({
    if("AE6_Patrullas" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE6_Patrullas   <- factor(df_ide$AE6_Patrullas,
                                       levels = c(0,1),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE6_Patrullas","Obs.Patrullas", choices = unique(df_ide$AE6_Patrullas))
      selectInput("AE6_Patrullas", "Obs.Patrullas",choices = NULL, multiple = TRUE)}
  })
  
  
  output$rows98 <- renderUI({
    if("AE7_Lotes" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE7_Lotes    <- factor(df_ide$AE7_Lotes,
                                    levels = c(0,1),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE7_Lotes","Obs.Lotes", choices = unique(df_ide$AE7_Lotes))
      selectInput("AE7_Lotes", "Obs.Lotes",choices = NULL, multiple = TRUE)}
  })
  
  
  
  output$rows99 <- renderUI({
    if("AE9_PeSeguridad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE9_PeSeguridad<- factor(df_ide$AE9_PeSeguridad,
                                      levels = c(0,1,2,3,4,5),
                                      ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE9_PeSeguridad","Obs.PeSeguridad", choices = unique(df_ide$AE9_PeSeguridad))
      selectInput("AE9_PeSeguridad", "Obs.PeSeguridad",choices = NULL, multiple = TRUE)}
  })
  
  output$rows100 <- renderUI({
    if("AE10_PeComodidad" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE10_PeComodidad<- factor(df_ide$AE10_PeComodidad,
                                       levels = c(0,1,2,3,4,5),
                                       ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE10_PeComodidad","Obs.PeComodidad", choices = unique(df_ide$AE10_PeComodidad))
      selectInput("AE10_PeComodidad", "Obs.PeComodidad", choices = NULL,multiple = TRUE)}
  })
  
  output$rows101 <- renderUI({
    if("AE11_PeRiesgo" %in% input$show_vars){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      df_ide$AE11_PeRiesgo<- factor(df_ide$AE11_PeRiesgo,
                                    levels = c(0,1,2,3,4,5),
                                    ordered = TRUE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),"AE11_PeRiesgo","Obs.PeRiesgo", choices = unique(df_ide$AE11_PeRiesgo))
      selectInput("AE11_PeRiesgo", "Obs.PeRiesgo", choices = NULL, multiple = TRUE)}
  })
}



