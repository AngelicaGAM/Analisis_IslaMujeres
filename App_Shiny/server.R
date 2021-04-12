# server library(shinyjs)



server <- function(input, output, session) {
  source("librerias.R")
  source("Funciones.R")
  source("G.R")
  source("graficas.R")
  source("graficarSE.R")
  source("tablas.R")
  xdf  <- read.csv("x.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
  
  
  
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
                   "1. En esta calle o zona, Usted participa: <br><br>En eventos deportivos<br>En tandas<br>En fiestas<br>En iglesia o templo<br>En otras actividades<br>Para solucionar problemas de la comunidad<br>2. Usted conoce a sus vecinos: <br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Les confiaría a los niños<br>Les confiaría su casa<br>Participa con ellos para mejorar la seguridad<br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesaría participar<br>Día en que podría participar en actividades con su comunidad. <br>Horario en que podría participar: Mañana Tarde Noche<br>3. Participa con la autoridad para mejorar la seguridad:<br><br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesaría participar<br>4. Día en que podría participar en actividades con la autoridad<br><br>5. Horario en el que podría participar Mañana Tarde Noche<br><br>6. Cuando hay un delito, en esta calle o zona los vecinos:<br><br>Se reúnen<br>Se organizan para vigilar<br>Intercambian números telefónicos<br>Forman un chat<br>Ponen letreros de advertencia<br>Llaman a la policía<br>Denuncian ante la autoridad<br>Vigilan<br>Buscan desquitarse<br>No hacen nada<br>7. Durante el último año, en esta calle o zona ha habido:<br><br>Robo en casa<br>Robo en la calle<br>Robo en transporte<br>Robo en negocio<br>Robo de partes de auto<br>Robo de vehículo<br>Balaceras<br>Cobro de piso<br>Violencia familiar<br>Peleas de gallos o perros<br>8. Valore el riesgo de sufrir un delito en alguno de los siguientes<br><br>lugares:<br>En su casa<br>En esta calle<br>En esta zona<br>En esta ciudad<br>9. ¿Usted ha sido víctima de algún delito en el último año? si No<br><br>10. En caso de ser víctima del delito Usted:<br><br>Llama a la policía<br>Hace una denuncia<br>Advierte a sus vecinos del peligro<br>Advierte a su familia del peligro<br>Busca desquitarse<br>Amenazas<br>11. Ha sido víctima de algún delito y no denuncio:<br><br>¿Podría señalar las razones por las que no denunció?<br>Respuesta “Si”, desplegar el siguiente reactivo:<br>Falta de pruebas<br>Considera que es un delito de poca importancia<br>Conoce al agresor o agresores<br>Desconfía de las autoridades<br>Teme a que lo extorsionen<br>Falta de tiempo<br>Son trámites complicados<br>No sabe dónde denunciar<br>Aunque denuncie no va a pasar nada<br>12. En esta calle o zona:<br><br>Los padres participan en actividades con hijos<br>Los vecinos se organizan para prevenir delitos<br>Las personas son amables<br>Hay alguna persona que siempre ayuda a los demás<br>13.En esta calle o zona hay personas:<br><br>A las que todos tienen miedo<br>Que acosan a menores<br>Que acosan a mujeres<br>Que se emborrachan o se drogan<br>Que han estado en la cárcel<br>Sospechosas<br>14. En esta calle o zona hay violencia:<br><br>Entre mujeres<br>Entre hombres<br>Entre familias<br>Entre adultos y jóvenes<br>Entre jóvenes<br>15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:<br><br>Amistosamente<br>Dialogando<br>De manera respetuosa<br>A gritos<br>Con golpes<br>Con cuchillos, navajas o machetes<br>Con armas de fuego, como pistolas o rifles<br>Desquitándose del otro<br>16. En esta calle o zona hay niños o adolescentes que se quedan<br><br>encerrados con llave:<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Por la inseguridad<br>Descuido de los padres<br>Castigo<br>Trabajo de los padres<br>17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: Respuesta “Si”, desplegar los siguientes reactivos:<br><br>Por descuido de los padres<br>Castigo<br>Falta de dinero<br>Trabajo de los padres<br>18. En esta calle o zona hay jóvenes que:<br><br>Hacen deporte<br>Ayudan a los demás<br>La mayoría de los jóvenes estudian o trabajan.<br>Andan en pandillas<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Andan armados<br>Destruyen o vandalizan la propiedad ajena<br>Son violentos<br>Amenazan a los vecinos<br>19. En esta calle o zona hay un parque<br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Está en buen estado<br>¿Quiénes lo utilizan?: Mostrar menú abajo<br>Niños y niñas<br>Jóvenes<br>Pandillas<br>Familias<br>Adultos<br>Personas de la tercera edad<br>Hay actividades supervisadas por adultos<br>Vándalos<br>Usted lo utiliza<br>20. En esta calle o zona hay:<br><br>Banquetas<br>Baches<br>Letreros con nombres de las calles<br>Tiendita<br>Alumbrado<br>Consumo de alcohol en la calle<br>21. En esta zona hay:<br><br>Horarios de transporte que convienen<br>Una parada de camión cerca de esta casa<br>Terrenos baldíos<br>Basura<br>Autos abandonados<br>Casas abandonadas<br>Vandalismo<br>Grafiti<br>Venta de tiner o pegamento a menores<br>Venta de alcohol o cigarros a menores<br>Venta de droga<br>Venta de alcohol después de las 11:00 de la noche<br>22. En el último año Usted supo que algún menor de 18 años:<br><br>Se fue de la casa<br>Sufrió violencia<br>Abandonó la escuela<br>Tiene problemas de conducta<br>Quedó embarazada<br>23. Para corregir a un niño o niña que se porta mal, Usted recomienda:<br><br>Castigarle<br>Gritarle<br>Darle nalgadas<br>Darle una golpiza / cueriza <br>Explicarle lo que está mal<br>Aconsejarle<br>Darle buen ejemplo24. En esta casa: (APLICA TARJETON)<br>Todos se conocen<br>Platican unos con otros<br>Comen juntos<br>Se ayudan con los gastos<br>Discuten<br>Se gritan entre sí<br>Llegan a los golpes<br>Se ignoran<br>En esta casa alguien: (APLICA TARJETON)<br>Tiene discapacidad SI LA RESPUESTA ES SI, ENTONCES:<br>Por su discapacidad, ha vivido violencia<br>Sabe manejar armas de fuego, como pistolas o rifles<br>Habla de comprar armas de fuego<br>Habla lengua indigena<br>Necesita ayuda por obesidad<br>Necesita ayuda por fumar<br>Necesita ayuda por beber<br>Necesita ayuda por drogas<br>26. En el último año, por cuestiones de seguridad Usted ha<br>pensado:<br>Cambiarse de casa<br>Cambiarse de ciudad<br>Cambiarse de estado<br>Cambiar de trabajo N/A<br>Cerrar su negocio N/A<br>Cambiar a los hijos de escuela N/ASeguir como hasta ahora<br>27. En el último año, por cuestiones de seguridad Usted dejó de:Dejo de salir de noche<br><br>Dejo de salir a caminar o hacer ejercicio<br>Impidio que los niños salgan a la calle N/A<br>Evito relacionarse con nuevas personas<br>Dejo de visitar a parientes o amigos<br>Dejo de usar transporte público /combi N/A<br>Dejo de usar taxi<br>Dejo de llevar mucho dinero en efectivo<br>Dejo de usar joyas<br>28. En esta calle o zona la policía:<br>Cuida o vigila bien<br>Comete abusos<br>Acude a los llamados<br>Pide mordidas<br>Hace rondines<br>Comete delitos<br>"),
      easyClose = TRUE
    ))
  })
  
  
  # ---------------------------------------------------------------------
  # GRAFICAS 
  
  output$tableGraficas=renderLeaflet({
    #Caracteristicas de poblacion y migracion
    #vivienda
    #  if(input$pregunta=="V1P1R1"){V1P1R1}
    #  else if(input$pregunta=="V1P4R1"){V1P4R1}
    #  else if(input$pregunta=="V1H1"){V1H1}
    #  else if(input$pregunta=="V1H1"){V1H1}
    #Familiares
    if(input$pregunta=="PFAM1"){TFAM1}
    else if(input$pregunta=="PFAM2"){TFAM2}
    else if(input$pregunta=="PFAM3"){TFAM3}
    else if(input$pregunta=="PFAM4"){TFAM4}
    # Economicos
    else if(input$pregunta=="PPECO1"){TECO1}
    else if(input$pregunta=="PPECO2"){TECO2}
    else if(input$pregunta=="PPECO3"){TECO3}
    else if(input$pregunta=="PPECO4"){TECO4}
    else if(input$pregunta=="PPECO5"){TECO5}
    else if(input$pregunta=="PPECO6"){TECO6}
    else if(input$pregunta=="PPECO7"){TECO7}
    #Identidad  y comunidad
    else if(input$pregunta=="PPIyC1"){TIyC1}
    else if(input$pregunta=="PPIyC2"){TIyC2}
    else if(input$pregunta=="PPIyC3"){TIyC3}
    else if(input$pregunta=="PPIyC4"){TIyC4}
    else if(input$pregunta=="PPIyC5"){TIyC5}
    else if(input$pregunta=="PPIyC6"){TIyC6}
    
    
    else if(input$pregunta=="PPCC1"){TCC1}
    else if(input$pregunta=="PPCC2"){TCC2}
    else if(input$pregunta=="PPCC3"){TCC3}
    else if(input$pregunta=="PPCC4"){TCC4}
    
    
    
    #Salinas
    else if(input$pregunta=="PAMB1"){TAMB1}
    else if(input$pregunta=="PAMB2"){TAMB2}
    else if(input$pregunta=="PAMB3"){TAMB3}
    else if(input$pregunta=="PAMB4"){TAMB4}
    else if(input$pregunta=="PAMB5"){TAMB5}
    else if(input$pregunta=="SOC1"){TSOC1}
    else if(input$pregunta=="SOC2"){TSOC2}
    else if(input$pregunta=="SOC3"){TSOC3}
    else if(input$pregunta=="SOC4"){TSOC4}
    else if(input$pregunta=="SOC5"){TSOC5}
    else if(input$pregunta=="SOC6"){TSOC6}
    else if(input$pregunta=="ECO1"){TEC1}
    else if(input$pregunta=="ECO2"){TEC2}
    else if(input$pregunta=="ECO3"){TEC3}
    else if(input$pregunta=="ECO4"){TEC4}
    else if(input$pregunta=="ECO5"){TEC5}
    
    #percepcion de seguridad 
    else if(input$pregunta=="GPSP1"){TPSP1}
    else if(input$pregunta=="GPSP2"){TPSP2}
    else if(input$pregunta=="GPSP3"){TPSP3}
    else if(input$pregunta=="GPSP6"){TPSP6}
    else if(input$pregunta=="GPSP7"){TPSP7}
    else if(input$pregunta=="GPSP9"){TPSP9}
    else if(input$pregunta=="GPSP10"){TPSP10}
    else if(input$pregunta=="GPSP11"){TPSP11}
    else if(input$pregunta=="GPSP12"){TPSP12}
    else if(input$pregunta=="GPSP13"){TPSP13}
    else if(input$pregunta=="GPSP14"){TPSP14}
    else if(input$pregunta=="GPSP15"){TPSP15}


      #percepcion de seguridad 
    # else if(input$localizPS=="GPSP1"){TPSP1}
    # else if(input$localizPS=="GPSP2"){TPSP2}
    # else if(input$localizPS=="GPSP3"){TPSP3}
    # else if(input$localizPS=="GPSP6"){TPSP6}
    # else if(input$localizPS=="GPSP7"){TPSP7}
    # else if(input$localizPS=="GPSP9"){TPSP9}
    # else if(input$localizPS=="GPSP10"){TPSP10}
    # else if(input$localizPS=="GPSP11"){TPSP11}
    # else if(input$localizPS=="GPSP12"){TPSP12}
    # else if(input$localizPS=="GPSP13"){TPSP13}
    # else if(input$localizPS=="GPSP14"){TPSP14}
    # else if(input$localizPS=="GPSP15"){TPSP15}

                     

    #percepcion de seguridad  isla 
    else if(input$pregunta=="GPSP1I"){TPSP1I}
    else if(input$pregunta=="GPSP2I"){TPSP2I}
    else if(input$pregunta=="GPSP3I"){TPSP3I}
    else if(input$pregunta=="GPSP6I"){TPSP6I}
    else if(input$pregunta=="GPSP7I"){TPSP7I}

    else if(input$pregunta=="GPSP81I"){TPSP81I}
    else if(input$pregunta=="GPSP82I"){TPSP82I}
    else if(input$pregunta=="GPSP83I"){TPSP83I}
    else if(input$pregunta=="GPSP84I"){TPSP84I}
    
    else if(input$pregunta=="GPSP9I"){TPSP9I}
    else if(input$pregunta=="GPSP10I"){TPSP10I}
    else if(input$pregunta=="GPSP11I"){TPSP11I}
    else if(input$pregunta=="GPSP12I"){TPSP12I}
    else if(input$pregunta=="GPSP13I"){TPSP13I}
    else if(input$pregunta=="GPSP14I"){TPSP14I}
    else if(input$pregunta=="GPSP15I"){TPSP15I}
    else if(input$pregunta=="GPSP16I"){TPSP16I}
    else if(input$pregunta=="GPSP17I"){TPSP17I}
    else if(input$pregunta=="GPSP18I"){TPSP18I}
    else if(input$pregunta=="GPSP19I"){TPSP19I}
    else if(input$pregunta=="GPSP20I"){TPSP20I}

    else if(input$pregunta=="GPSP21I"){TPSP21I}
    else if(input$pregunta=="GPSP22I"){TPSP22I}
    else if(input$pregunta=="GPSP23I"){TPSP23I}
    else if(input$pregunta=="GPSP24I"){TPSP24I}
    else if(input$pregunta=="GPSP25I"){TPSP25I}
    else if(input$pregunta=="GPSP26I"){TPSP26I}
    else if(input$pregunta=="GPSP27I"){TPSP27I}
    else if(input$pregunta=="GPSP28I"){TPSP28I}

    else if(input$pregunta=="GPSP291I"){TPSP291I}
    else if(input$pregunta=="GPSP292I"){TPSP292I}
    else if(input$pregunta=="GPSP293I"){TPSP293I}
    else if(input$pregunta=="GPSP294I"){TPSP294I}
    else if(input$pregunta=="GPSP295I"){TPSP295I}
    else if(input$pregunta=="GPSP296I"){TPSP296I}

    else if(input$pregunta=="GPSP301I"){TPSP301I}
    else if(input$pregunta=="GPSP302I"){TPSP302I}
    else if(input$pregunta=="GPSP303I"){TPSP303I}
    else if(input$pregunta=="GPSP304I"){TPSP304I}
    else if(input$pregunta=="GPSP305I"){TPSP305I}
    else if(input$pregunta=="GPSP306I"){TPSP306I}

    else if(input$pregunta=="GPSP31I"){TPSP31I}
    else if(input$pregunta=="GPSP311I"){TPSP311I}
    else if(input$pregunta=="GPSP312I"){TPSP312I}
    else if(input$pregunta=="GPSP313I"){TPSP313I}
    else if(input$pregunta=="GPSP314I"){TPSP314I}
    else if(input$pregunta=="GPSP315I"){TPSP315I}



    #percepcion de seguridad  cancun 
    else if(input$pregunta=="GPSP1C"){TPSP1C}
    else if(input$pregunta=="GPSP2C"){TPSP2C}
    else if(input$pregunta=="GPSP3C"){TPSP3C}
    else if(input$pregunta=="GPSP6C"){TPSP6C}
    else if(input$pregunta=="GPSP7C"){TPSP7C}

    else if(input$pregunta=="GPSP81C"){TPSP81C}
    else if(input$pregunta=="GPSP82C"){TPSP82C}
    else if(input$pregunta=="GPSP83C"){TPSP83C}
    else if(input$pregunta=="GPSP84C"){TPSP84C}
    
    else if(input$pregunta=="GPSP9C"){TPSP9C}
    else if(input$pregunta=="GPSP10C"){TPSP10C}
    else if(input$pregunta=="GPSP11C"){TPSP11C}
    else if(input$pregunta=="GPSP12C"){TPSP12C}
    else if(input$pregunta=="GPSP13C"){TPSP13C}
    else if(input$pregunta=="GPSP14C"){TPSP14C}
    else if(input$pregunta=="GPSP15C"){TPSP15C}
    else if(input$pregunta=="GPSP16C"){TPSP16C}
    else if(input$pregunta=="GPSP17C"){TPSP17C}
    else if(input$pregunta=="GPSP18C"){TPSP18C}
    else if(input$pregunta=="GPSP19C"){TPSP19C}
    else if(input$pregunta=="GPSP20C"){TPSP20C}

    else if(input$pregunta=="GPSP21C"){TPSP21C}
    else if(input$pregunta=="GPSP22C"){TPSP22C}
    else if(input$pregunta=="GPSP23C"){TPSP23C}
    else if(input$pregunta=="GPSP24C"){TPSP24C}
    else if(input$pregunta=="GPSP25C"){TPSP25C}
    else if(input$pregunta=="GPSP26C"){TPSP26C}
    else if(input$pregunta=="GPSP27C"){TPSP27C}
    else if(input$pregunta=="GPSP28C"){TPSP28C}

    else if(input$pregunta=="GPSP291C"){TPSP291C}
    else if(input$pregunta=="GPSP292C"){TPSP292C}
    else if(input$pregunta=="GPSP293C"){TPSP293C}
    else if(input$pregunta=="GPSP294C"){TPSP294C}
    else if(input$pregunta=="GPSP295C"){TPSP295C}
    else if(input$pregunta=="GPSP296C"){TPSP296C}

    else if(input$pregunta=="GPSP301C"){TPSP301C}
    else if(input$pregunta=="GPSP302C"){TPSP302C}
    else if(input$pregunta=="GPSP303C"){TPSP303C}
    else if(input$pregunta=="GPSP304C"){TPSP304C}
    else if(input$pregunta=="GPSP305C"){TPSP305C}
    else if(input$pregunta=="GPSP306C"){TPSP306C}

    else if(input$pregunta=="GPSP31C"){TPSP31C}
    else if(input$pregunta=="GPSP311C"){TPSP311C}
    else if(input$pregunta=="GPSP312C"){TPSP312C}
    else if(input$pregunta=="GPSP313C"){TPSP313C}
    else if(input$pregunta=="GPSP314C"){TPSP314C}
    else if(input$pregunta=="GPSP315C"){TPSP315C}


#percepcion de seguridad  EJIDO 
    else if(input$pregunta=="GPSP1E"){TPSP1E}
    else if(input$pregunta=="GPSP2E"){TPSP2E}
    else if(input$pregunta=="GPSP3E"){TPSP3E}
    else if(input$pregunta=="GPSP6E"){TPSP6E}
    else if(input$pregunta=="GPSP7E"){TPSP7E}

    else if(input$pregunta=="GPSP81E"){TPSP81E}
    else if(input$pregunta=="GPSP82E"){TPSP82E}
    else if(input$pregunta=="GPSP83E"){TPSP83E}
    else if(input$pregunta=="GPSP84E"){TPSP84E}
    
    else if(input$pregunta=="GPSP9E"){TPSP9E}
    else if(input$pregunta=="GPSP10E"){TPSP10E}
    else if(input$pregunta=="GPSP11E"){TPSP11E}
    else if(input$pregunta=="GPSP12E"){TPSP12E}
    else if(input$pregunta=="GPSP13E"){TPSP13E}
    else if(input$pregunta=="GPSP14E"){TPSP14E}
    else if(input$pregunta=="GPSP15E"){TPSP15E}
    else if(input$pregunta=="GPSP16E"){TPSP16E}
    else if(input$pregunta=="GPSP17E"){TPSP17E}
    else if(input$pregunta=="GPSP18E"){TPSP18E}
    else if(input$pregunta=="GPSP19E"){TPSP19E}
    else if(input$pregunta=="GPSP20E"){TPSP20E}

    else if(input$pregunta=="GPSP21E"){TPSP21E}
    else if(input$pregunta=="GPSP22E"){TPSP22E}
    else if(input$pregunta=="GPSP23E"){TPSP23E}
    else if(input$pregunta=="GPSP24E"){TPSP24E}
    else if(input$pregunta=="GPSP25E"){TPSP25E}
    else if(input$pregunta=="GPSP26E"){TPSP26E}
    else if(input$pregunta=="GPSP27E"){TPSP27E}
    else if(input$pregunta=="GPSP28E"){TPSP28E}

    else if(input$pregunta=="GPSP291E"){TPSP291E}
    else if(input$pregunta=="GPSP292E"){TPSP292E}
    else if(input$pregunta=="GPSP293E"){TPSP293E}
    else if(input$pregunta=="GPSP294E"){TPSP294E}
    else if(input$pregunta=="GPSP295E"){TPSP295E}
    else if(input$pregunta=="GPSP296E"){TPSP296E}

    else if(input$pregunta=="GPSP301E"){TPSP301E}
    else if(input$pregunta=="GPSP302E"){TPSP302E}
    else if(input$pregunta=="GPSP303E"){TPSP303E}
    else if(input$pregunta=="GPSP304E"){TPSP304E}
    else if(input$pregunta=="GPSP305E"){TPSP305E}
    else if(input$pregunta=="GPSP306E"){TPSP306E}

    else if(input$pregunta=="GPSP31E"){TPSP31E}
    else if(input$pregunta=="GPSP311E"){TPSP311E}
    else if(input$pregunta=="GPSP312E"){TPSP312E}
    else if(input$pregunta=="GPSP313E"){TPSP313E}
    else if(input$pregunta=="GPSP314E"){TPSP314E}
    else if(input$pregunta=="GPSP315E"){TPSP315E}


    
  })
  
  # Visualizacion principal 
  output$plot1=renderLeaflet({
    #tableGraficas
    #datoG = Vgrafica()
    #Caracteristicas de poblacion y migracion
    #vivienda
    if(input$pregunta=="V1P1R1"){V1P1R1}
    else if(input$pregunta=="V1P4R1"){V1P4R1}
    else if(input$pregunta=="V1H1"){V1H1}
    else if(input$pregunta=="V1H1"){V1H1}
    #Familiares
    else if(input$pregunta=="PFAM1"){PFAM01}
    else if(input$pregunta=="PFAM2"){PFAM02}
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
    else if(input$pregunta=="PPECO7"){PECO7}
    #Identidad  y comunidad
    else if(input$pregunta=="PPIyC1"){PIyC1}
    else if(input$pregunta=="PPIyC2"){PIyC2}
    else if(input$pregunta=="PPIyC3"){PIyC3}
    else if(input$pregunta=="PPIyC4"){PIyC4}
    else if(input$pregunta=="PPIyC5"){PIyC5}
    else if(input$pregunta=="PPIyC6"){PIyC6}
    
    
    else if(input$pregunta=="PPCC1"){PCC1}
    else if(input$pregunta=="PPCC2"){PCC2}
    else if(input$pregunta=="PPCC3"){PCC3}
    else if(input$pregunta=="PPCC4"){PCC4}
    
    
    
    #Salinas
    else if(input$pregunta=="PAMB1"){AMB1}
    else if(input$pregunta=="PAMB2"){AMB2}
    else if(input$pregunta=="PAMB3"){AMB3}
    else if(input$pregunta=="PAMB4"){AMB4}
    else if(input$pregunta=="PAMB5"){AMB5}
    else if(input$pregunta=="SOC1"){SOC1}
    else if(input$pregunta=="SOC2"){SOC2}
    else if(input$pregunta=="SOC3"){SOC3}
    else if(input$pregunta=="SOC4"){SOC4}
    else if(input$pregunta=="SOC5"){SOC5}
    else if(input$pregunta=="SOC6"){SOC6}
    else if(input$pregunta=="ECO1"){EC1}
    else if(input$pregunta=="ECO2"){EC2}
    else if(input$pregunta=="ECO3"){EC3}
    else if(input$pregunta=="ECO4"){EC4}
    else if(input$pregunta=="ECO5"){EC5}


                               #"En esta calle o zona, hay violencia:" = "GPSP14" ,
                               #"En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" = "GPSP15" 
#PERCEPCION COMPARATIVA
       else if(input$pregunta=="PSG1"){PSG1}
    else if(input$pregunta=="PSG2"){PSG2}
    else if(input$pregunta=="PSG3"){PSG3}
    else if(input$pregunta=="PSG4"){PSG4}
    else if(input$pregunta=="PSG5"){PSG5}

    else if(input$pregunta=="PSG5"){PSG5}
    else if(input$pregunta=="PSG7"){PSG7}

    else if(input$pregunta=="PSG8"){PSG8}
    else if(input$pregunta=="PSG82"){PSG82}
    else if(input$pregunta=="PSG83"){PSG83}
    else if(input$pregunta=="PSG84"){PSG84}


    else if(input$pregunta=="PSG9"){PSG9}
    
    else if(input$pregunta=="PSG10"){PSG10}
    else if(input$pregunta=="PSG11"){PSG11}
    else if(input$pregunta=="PSG12"){PSG12}
    else if(input$pregunta=="PSG13"){PSG13}
    else if(input$pregunta=="PSG14"){PSG14}
    else if(input$pregunta=="PSG15"){PSG15}
    else if(input$pregunta=="PSG16"){PSG16}
    else if(input$pregunta=="PSG17"){PSG17}
    else if(input$pregunta=="PSG18"){PSG18}

    else if(input$pregunta=="PSG14"){PSG14}
    else if(input$pregunta=="PSG15"){PSG15}
    else if(input$pregunta=="PSG16"){PSG16}
    else if(input$pregunta=="PSG17"){PSG17}
    else if(input$pregunta=="PSG18"){PSG18}


    else if(input$pregunta=="PSG182"){PSG182}
    else if(input$pregunta=="PSG20"){PSG20}
    else if(input$pregunta=="PSG19"){PSG19}

    else if(input$pregunta=="PSG20"){PSG20}
    else if(input$pregunta=="PSG21"){PSG21}

    else if(input$pregunta=="PSG22"){PSG22}
    else if(input$pregunta=="PSG23"){PSG23}
    else if(input$pregunta=="PSG24"){PSG24}
    else if(input$pregunta=="PSG25"){PSG25}
    else if(input$pregunta=="PSG26"){PSG26}
    else if(input$pregunta=="PSG27"){PSG27}
    else if(input$pregunta=="PSG28"){PSG28}
    else if(input$pregunta=="PSG29"){PSG29}

    else if(input$pregunta=="GPSP291I"){GPSP291I}
    else if(input$pregunta=="GPSP292I"){GPSP292I}
    else if(input$pregunta=="GPSP293I"){GPSP293I}
    else if(input$pregunta=="GPSP294I"){GPSP294I}
    else if(input$pregunta=="GPSP295I"){GPSP295I}
    else if(input$pregunta=="GPSP296I"){GPSP296I}

    else if(input$pregunta=="GPSP301I"){GPSP301I}
    else if(input$pregunta=="GPSP302I"){GPSP302I}
    else if(input$pregunta=="GPSP303I"){GPSP303I}
    else if(input$pregunta=="GPSP304I"){GPSP304I}
    else if(input$pregunta=="GPSP305I"){GPSP305I}
    else if(input$pregunta=="GPSP306I"){GPSP306I}

    else if(input$pregunta=="GPSP31I"){GPSP31I}
    else if(input$pregunta=="GPSP311I"){GPSP311I}
    else if(input$pregunta=="GPSP312I"){GPSP312I}
    else if(input$pregunta=="GPSP313I"){GPSP313I}
    else if(input$pregunta=="GPSP314I"){GPSP314I}
    else if(input$pregunta=="GPSP315I"){GPSP315I}


                         

    #percepcion de seguridad  isla 
    else if(input$pregunta=="GPSP1I"){GPSP1I}
    else if(input$pregunta=="GPSP2I"){GPSP2I}
    else if(input$pregunta=="GPSP3I"){GPSP3I}
    else if(input$pregunta=="GPSP6I"){GPSP6I}
    else if(input$pregunta=="GPSP7I"){GPSP7I}

    else if(input$pregunta=="GPSP81I"){GPSP81I}
    else if(input$pregunta=="GPSP82I"){GPSP82I}
    else if(input$pregunta=="GPSP83I"){GPSP83I}
    else if(input$pregunta=="GPSP84I"){GPSP84I}
    
    else if(input$pregunta=="GPSP9I"){GPSP9I}
    else if(input$pregunta=="GPSP10I"){GPSP10I}
    else if(input$pregunta=="GPSP11I"){GPSP11I}
    else if(input$pregunta=="GPSP12I"){GPSP12I}
    else if(input$pregunta=="GPSP13I"){GPSP13I}
    else if(input$pregunta=="GPSP14I"){GPSP14I}
    else if(input$pregunta=="GPSP15I"){GPSP15I}
    else if(input$pregunta=="GPSP16I"){GPSP16I}
    else if(input$pregunta=="GPSP17I"){GPSP17I}
    else if(input$pregunta=="GPSP18I"){GPSP18I}
    else if(input$pregunta=="GPSP19I"){GPSP19I}
    else if(input$pregunta=="GPSP20I"){GPSP20I}

    else if(input$pregunta=="GPSP21I"){GPSP21I}
    else if(input$pregunta=="GPSP22I"){GPSP22I}
    else if(input$pregunta=="GPSP23I"){GPSP23I}
    else if(input$pregunta=="GPSP24I"){GPSP24I}
    else if(input$pregunta=="GPSP25I"){GPSP25I}
    else if(input$pregunta=="GPSP26I"){GPSP26I}
    else if(input$pregunta=="GPSP27I"){GPSP27I}
    else if(input$pregunta=="GPSP28I"){GPSP28I}

    else if(input$pregunta=="GPSP291I"){GPSP291I}
    else if(input$pregunta=="GPSP292I"){GPSP292I}
    else if(input$pregunta=="GPSP293I"){GPSP293I}
    else if(input$pregunta=="GPSP294I"){GPSP294I}
    else if(input$pregunta=="GPSP295I"){GPSP295I}
    else if(input$pregunta=="GPSP296I"){GPSP296I}

    else if(input$pregunta=="GPSP301I"){GPSP301I}
    else if(input$pregunta=="GPSP302I"){GPSP302I}
    else if(input$pregunta=="GPSP303I"){GPSP303I}
    else if(input$pregunta=="GPSP304I"){GPSP304I}
    else if(input$pregunta=="GPSP305I"){GPSP305I}
    else if(input$pregunta=="GPSP306I"){GPSP306I}

    else if(input$pregunta=="GPSP31I"){GPSP31I}
    else if(input$pregunta=="GPSP311I"){GPSP311I}
    else if(input$pregunta=="GPSP312I"){GPSP312I}
    else if(input$pregunta=="GPSP313I"){GPSP313I}
    else if(input$pregunta=="GPSP314I"){GPSP314I}
    else if(input$pregunta=="GPSP315I"){GPSP315I}



    #percepcion de seguridad  cancun 
    else if(input$pregunta=="GPSP1C"){GPSP1C}
    else if(input$pregunta=="GPSP2C"){GPSP2C}
    else if(input$pregunta=="GPSP3C"){GPSP3C}
    else if(input$pregunta=="GPSP6C"){GPSP6C}
    else if(input$pregunta=="GPSP7C"){GPSP7C}

    else if(input$pregunta=="GPSP81C"){GPSP81C}
    else if(input$pregunta=="GPSP82C"){GPSP82C}
    else if(input$pregunta=="GPSP83C"){GPSP83C}
    else if(input$pregunta=="GPSP84C"){GPSP84C}
    
    else if(input$pregunta=="GPSP9C"){GPSP9C}
    else if(input$pregunta=="GPSP10C"){GPSP10C}
    else if(input$pregunta=="GPSP11C"){GPSP11C}
    else if(input$pregunta=="GPSP12C"){GPSP12C}
    else if(input$pregunta=="GPSP13C"){GPSP13C}
    else if(input$pregunta=="GPSP14C"){GPSP14C}
    else if(input$pregunta=="GPSP15C"){GPSP15C}
    else if(input$pregunta=="GPSP16C"){GPSP16C}
    else if(input$pregunta=="GPSP17C"){GPSP17C}
    else if(input$pregunta=="GPSP18C"){GPSP18C}
    else if(input$pregunta=="GPSP19C"){GPSP19C}
    else if(input$pregunta=="GPSP20C"){GPSP20C}

    else if(input$pregunta=="GPSP21C"){GPSP21C}
    else if(input$pregunta=="GPSP22C"){GPSP22C}
    else if(input$pregunta=="GPSP23C"){GPSP23C}
    else if(input$pregunta=="GPSP24C"){GPSP24C}
    else if(input$pregunta=="GPSP25C"){GPSP25C}
    else if(input$pregunta=="GPSP26C"){GPSP26C}
    else if(input$pregunta=="GPSP27C"){GPSP27C}
    else if(input$pregunta=="GPSP28C"){GPSP28C}

    else if(input$pregunta=="GPSP291C"){GPSP291C}
    else if(input$pregunta=="GPSP292C"){GPSP292C}
    else if(input$pregunta=="GPSP293C"){GPSP293C}
    else if(input$pregunta=="GPSP294C"){GPSP294C}
    else if(input$pregunta=="GPSP295C"){GPSP295C}
    else if(input$pregunta=="GPSP296C"){GPSP296C}

    else if(input$pregunta=="GPSP301C"){GPSP301C}
    else if(input$pregunta=="GPSP302C"){GPSP302C}
    else if(input$pregunta=="GPSP303C"){GPSP303C}
    else if(input$pregunta=="GPSP304C"){GPSP304C}
    else if(input$pregunta=="GPSP305C"){GPSP305C}
    else if(input$pregunta=="GPSP306C"){GPSP306C}

    else if(input$pregunta=="GPSP31C"){GPSP31C}
    else if(input$pregunta=="GPSP311C"){GPSP311C}
    else if(input$pregunta=="GPSP312C"){GPSP312C}
    else if(input$pregunta=="GPSP313C"){GPSP313C}
    else if(input$pregunta=="GPSP314C"){GPSP314C}
    else if(input$pregunta=="GPSP315C"){GPSP315C}


#percepcion de seguridad  EJIDO 
    else if(input$pregunta=="GPSP1E"){GPSP1E}
    else if(input$pregunta=="GPSP2E"){GPSP2E}
    else if(input$pregunta=="GPSP3E"){GPSP3E}
    else if(input$pregunta=="GPSP6E"){GPSP6E}
    else if(input$pregunta=="GPSP7E"){GPSP7E}

    else if(input$pregunta=="GPSP81E"){GPSP81E}
    else if(input$pregunta=="GPSP82E"){GPSP82E}
    else if(input$pregunta=="GPSP83E"){GPSP83E}
    else if(input$pregunta=="GPSP84E"){GPSP84E}
    
    else if(input$pregunta=="GPSP9E"){GPSP9E}
    else if(input$pregunta=="GPSP10E"){GPSP10E}
    else if(input$pregunta=="GPSP11E"){GPSP11E}
    else if(input$pregunta=="GPSP12E"){GPSP12E}
    else if(input$pregunta=="GPSP13E"){GPSP13E}
    else if(input$pregunta=="GPSP14E"){GPSP14E}
    else if(input$pregunta=="GPSP15E"){GPSP15E}
    else if(input$pregunta=="GPSP16E"){GPSP16E}
    else if(input$pregunta=="GPSP17E"){GPSP17E}
    else if(input$pregunta=="GPSP18E"){GPSP18E}
    else if(input$pregunta=="GPSP19E"){GPSP19E}
    else if(input$pregunta=="GPSP20E"){GPSP20E}

    else if(input$pregunta=="GPSP21E"){GPSP21E}
    else if(input$pregunta=="GPSP22E"){GPSP22E}
    else if(input$pregunta=="GPSP23E"){GPSP23E}
    else if(input$pregunta=="GPSP24E"){GPSP24E}
    else if(input$pregunta=="GPSP25E"){GPSP25E}
    else if(input$pregunta=="GPSP26E"){GPSP26E}
    else if(input$pregunta=="GPSP27E"){GPSP27E}
    else if(input$pregunta=="GPSP28E"){GPSP28E}

    else if(input$pregunta=="GPSP291E"){GPSP291E}
    else if(input$pregunta=="GPSP292E"){GPSP292E}
    else if(input$pregunta=="GPSP293E"){GPSP293E}
    else if(input$pregunta=="GPSP294E"){GPSP294E}
    else if(input$pregunta=="GPSP295E"){GPSP295E}
    else if(input$pregunta=="GPSP296E"){GPSP296E}

    else if(input$pregunta=="GPSP301E"){GPSP301E}
    else if(input$pregunta=="GPSP302E"){GPSP302E}
    else if(input$pregunta=="GPSP303E"){GPSP303E}
    else if(input$pregunta=="GPSP304E"){GPSP304E}
    else if(input$pregunta=="GPSP305E"){GPSP305E}
    else if(input$pregunta=="GPSP306E"){GPSP306E}

    else if(input$pregunta=="GPSP31E"){GPSP31E}
    else if(input$pregunta=="GPSP311E"){GPSP311E}
    else if(input$pregunta=="GPSP312E"){GPSP312E}
    else if(input$pregunta=="GPSP313E"){GPSP313E}
    else if(input$pregunta=="GPSP314E"){GPSP314E}
    else if(input$pregunta=="GPSP315E"){GPSP315E}

    
    
  })
  





  observeEvent(input$tipomapa, {
    
    if (input$tipomapa== "PS"){
      updateSelectInput(session, "enfoque",                
                        choices = c( "Eficacia colectiva" = "PSEC" ,"Impunidad autoridad" = "PSIA", "Social" = "PSS", "Situacional" = "PSSIA") 
      )


    }else if (input$tipomapa== "IS"){
      updateSelectInput(session, "enfoque",                
                        choices = c("Ambiental" = "ambi1" , "Economico" = "eco1","Social"="soc1") 
      )
    }else if (input$tipomapa== "EJ"){
      updateSelectInput(session, "enfoque",                
                        choices= c( "Datos familiares" = "DF","Datos económicos" = "DE", "Identidad y Comunidad" = "ID" , "Vivienda" = "VI", "Apreciación del encuestador" = "AE")
      )
    }
  })

    observeEvent(input$localizPS, {
 
       if (input$localizPS == "PSE"){
                 updateSelectInput(session, "pregunta",  
                      choices = c(
                        "1. En esta calle o zona, Usted participa:" = "GPSP1E", 
                        "2. Usted conoce a sus vecinos:" = "GPSP2E" ,
                        "3. Participa con la autoridad para mejorar la seguridad:" = "GPSP3E" ,
                        "6. Cuando hay un delito, en esta calle o zona los vecinos:" = "GPSP6E", 
                        "7. Durante el último año, en esta calle o zona ha habido:" = "GPSP7E" ,
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa" = "GPSP81E",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle" = "GPSP82E",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona" = "GPSP83E",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad" = "GPSP84E",
                        "9. ¿Usted ha sido víctima de algún delito en el último año?" = "GPSP9E" ,
                        "10. En caso de ser víctima del delito Usted:" = "GPSP10E", 
                        "11. Ha sido víctima de algún delito y no denuncio:" = "GPSP11E" ,
                        "12. En esta calle o zona:" = "GPSP12E" ,
                        "13. En esta calle o zona, hay personas" = "GPSP13E", 
                        "14. En esta calle o zona hay violencia:" = "GPSP14E" ,
                        "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" = "GPSP15E" ,
                        "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" = "GPSP16E" ,
                        "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " = "GPSP17E" ,
                        "18. En esta calle o zona hay jóvenes que:" = "GPSP18E" ,
                        "19. En esta calle o zona hay un parque" = "GPSP19E" ,
                        "20. En esta calle o zona hay:" = "GPSP20E" ,
                        "21. En esta zona hay" = "GPSP21E" ,
                        "22. En el último año Usted supo que algún menor de 18 años:" = "GPSP22E" ,
                        "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda:" = "GPSP23E" ,
                        "24. En esta casa: (APLICA TARJETON)" = "GPSP24E" ,
                        "25. En esta casa alguien: (APLICA TARJETON)" = "GPSP25E" ,
                        "26. En el último año, por cuestiones de seguridad Usted ha pensado:" = "GPSP26E" ,
                        "27. En el último año, por cuestiones de seguridad Usted dejó de:" = "GPSP27E" ,
                        "28. En esta calle o zona la policía:" = "GPSP28E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía " = "GPSP291E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar " = "GPSP292E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona " = "GPSP293E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal " = "GPSP294E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal " = "GPSP295E" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador " = "GPSP296E" ,
                        "30. Del 1 al 5, califique el trabajo de: La policía" = "GPSP301E" ,
                        "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar" = "GPSP302E" ,
                        "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno" = "GPSP303E" ,
                        "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida" = "GPSP304E" ,
                        "30. Del 1 al 5, califique el trabajo de: Su presidente municipal" = "GPSP305E" ,
                        "30. Del 1 al 5, califique el trabajo de: El Gobernador" = "GPSP306E" ,                          
                        "31. Del 1 al 5, califique el trato que recibe de: La policía" = "GPSP30E" ,
                        "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar" = "GPSP311E" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno" = "GPSP312E" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal" = "GPSP313E" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa" = "GPSP314E" ,
                        "31. Del 1 al 5, califique el trato que recibe de: El Gobernador" = "GPSP315E"   )      
    )}  else  if (input$localizPS == "PSC"){
             updateSelectInput(session, "pregunta",  
                      choices = c(
                        "1. En esta calle o zona, Usted participa:" = "GPSP1C", 
                        "2. Usted conoce a sus vecinos:" = "GPSP2C" ,
                        "3. Participa con la autoridad para mejorar la seguridad:" = "GPSP3C" ,
                        "6. Cuando hay un delito, en esta calle o zona los vecinos:" = "GPSP6C", 
                        "7. Durante el último año, en esta calle o zona ha habido:" = "GPSP7C" ,
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa" = "GPSP81C",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle" = "GPSP82C",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona" = "GPSP83C",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad" = "GPSP84C",
                        "9. ¿Usted ha sido víctima de algún delito en el último año?" = "GPSP9C" ,
                        "10. En caso de ser víctima del delito Usted:" = "GPSP10C", 
                        "11. Ha sido víctima de algún delito y no denuncio:" = "GPSP11C" ,
                        "12. En esta calle o zona:" = "GPSP12C" ,
                        "13. En esta calle o zona, hay personas" = "GPSP13C", 
                        "14. En esta calle o zona hay violencia:" = "GPSP14C" ,
                        "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" = "GPSP15C" ,
                        "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" = "GPSP16C" ,
                        "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " = "GPSP17C" ,
                        "18. En esta calle o zona hay jóvenes que:" = "GPSP18C" ,
                        "19. En esta calle o zona hay un parquC" = "GPSP19C" ,
                        "20. En esta calle o zona hay:" = "GPSP20C" ,
                        "21. En esta zona hay" = "GPSP21C" ,
                        "22. En el último año Usted supo que algún menor de 18 años:" = "GPSP22C" ,
                        "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda:" = "GPSP23C" ,
                        "24. En esta casa: (APLICA TARJETON)" = "GPSP24C" ,
                        "25. En esta casa alguien: (APLICA TARJETON)" = "GPSP25C" ,
                        "26. En el último año, por cuestiones de seguridad Usted ha pensado:" = "GPSP26C" ,
                        "27. En el último año, por cuestiones de seguridad Usted dejó de:" = "GPSP27C" ,
                        "28. En esta calle o zona la policía:" = "GPSP28C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía " = "GPSP291C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar " = "GPSP292C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona " = "GPSP293C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal " = "GPSP294C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal " = "GPSP295C" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador " = "GPSP296C" ,
                        "30. Del 1 al 5, califique el trabajo de: La policía" = "GPSP301C" ,
                        "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar" = "GPSP302C" ,
                        "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno" = "GPSP303C" ,
                        "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida" = "GPSP304C" ,
                        "30. Del 1 al 5, califique el trabajo de: Su presidente municipal" = "GPSP305C" ,
                        "30. Del 1 al 5, califique el trabajo de: El Gobernador" = "GPSP306C" ,                          
                        "31. Del 1 al 5, califique el trato que recibe de: La policía" = "GPSP30C" ,
                        "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar" = "GPSP311C" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno" = "GPSP312C" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal" = "GPSP313C" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa" = "GPSP314C" ,
                        "31. Del 1 al 5, califique el trato que recibe de: El Gobernador" = "GPSP315C"                          
                        
                        )      
    )}  else  if (input$localizPS == "PSI"){
              updateSelectInput(session, "pregunta",  
                      choices = c(
                        "1. En esta calle o zona, Usted participa:" = "GPSP1I", 
                        "2. Usted conoce a sus vecinos:" = "GPSP2I" ,
                        "3. Participa con la autoridad para mejorar la seguridad:" = "GPSP3I" ,
                        "6. Cuando hay un delito, en esta calle o zona los vecinos:" = "GPSP6I", 
                        "7. Durante el último año, en esta calle o zona ha habido:" = "GPSP7I" ,
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa" = "GPSP81I",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle" = "GPSP82I",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona" = "GPSP83I",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad" = "GPSP84I",
                        "9. ¿Usted ha sido víctima de algún delito en el último año?" = "GPSP9I" ,
                        "10. En caso de ser víctima del delito Usted:" = "GPSP10I", 
                        "11. Ha sido víctima de algún delito y no denuncio:" = "GPSP11I" ,
                        "12. En esta calle o zona:" = "GPSP12I" ,
                        "13. En esta calle o zona, hay personas" = "GPSP13I", 
                        "14. En esta calle o zona hay violencia:" = "GPSP14I" ,
                        "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" = "GPSP15I" ,
                        "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" = "GPSP16I" ,
                        "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " = "GPSP17I" ,
                        "18. En esta calle o zona hay jóvenes que:" = "GPSP18I" ,
                        "19. En esta calle o zona hay un parquI" = "GPSP19I" ,
                        "20. En esta calle o zona hay:" = "GPSP20I" ,
                        "21. En esta zona hay" = "GPSP21I" ,
                        "22. En el último año Usted supo que algún menor de 18 años:" = "GPSP22I" ,
                        "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda:" = "GPSP23I" ,
                        "24. En esta casa: (APLICA TARJETON)" = "GPSP24I" ,
                        "25. En esta casa alguien: (APLICA TARJETON)" = "GPSP25I" ,
                        "26. En el último año, por cuestiones de seguridad Usted ha pensado:" = "GPSP26I" ,
                        "27. En el último año, por cuestiones de seguridad Usted dejó de:" = "GPSP27I" ,
                        "28. En esta calle o zona la policía:" = "GPSP28I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía " = "GPSP291I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar " = "GPSP292I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona " = "GPSP293I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal " = "GPSP294I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal " = "GPSP295I" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador " = "GPSP296I" ,
                        "30. Del 1 al 5, califique el trabajo de: La policía" = "GPSP301I" ,
                        "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar" = "GPSP302I" ,
                        "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno" = "GPSP303I" ,
                        "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida" = "GPSP304I" ,
                        "30. Del 1 al 5, califique el trabajo de: Su presidente municipal" = "GPSP305I" ,
                        "30. Del 1 al 5, califique el trabajo de: El Gobernador" = "GPSP306I" ,                          
                        "31. Del 1 al 5, califique el trato que recibe de: La policía" = "GPSP30I" ,
                        "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar" = "GPSP311I" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno" = "GPSP312I" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal" = "GPSP313I" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa" = "GPSP314I" ,
                        "31. Del 1 al 5, califique el trato que recibe de: El Gobernador" = "GPSP315I"                          
                        
                        )      
    )}  else  if (input$localizPS == "PSVS"){
              updateSelectInput(session, "pregunta",  
                      choices = c(
                        "1. En esta calle o zona, Usted participa:" = "GPSP1T", 
                        "2. Usted conoce a sus vecinos:" = "GPSP2T" ,
                        "3. Participa con la autoridad para mejorar la seguridad:" = "GPSP3T" ,
                        "6. Cuando hay un delito, en esta calle o zona los vecinos:" = "GPSP6T", 
                        "7. Durante el último año, en esta calle o zona ha habido:" = "GPSP7T" ,
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa" = "GPSP81T",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: CallT" = "GPSP82T",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona" = "GPSP83T",
                        "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad" = "GPSP84T",
                        "9. ¿Usted ha sido víctima de algún delito en el último año?" = "GPSP9T" ,
                        "10. En caso de ser víctima del delito Usted:" = "GPSP10T", 
                        "11. Ha sido víctima de algún delito y no denuncio:" = "GPSP11T" ,
                        "12. En esta calle o zona:" = "GPSP12T" ,
                        "13. En esta calle o zona, hay personas" = "GPSP13T", 
                        "14. En esta calle o zona hay violencia:" = "GPSP14T" ,
                        "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:" = "GPSP15T" ,
                        "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave:" = "GPSP16T" ,
                        "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: " = "GPSP17T" ,
                        "18. En esta calle o zona hay jóvenes que:" = "GPSP18T" ,
                        "19. En esta calle o zona hay un parquT" = "GPSP19T" ,
                        "20. En esta calle o zona hay:" = "GPSP20T" ,
                        "21. En esta zona hay" = "GPSP21T" ,
                        "22. En el último año Usted supo que algún menor de 18 años:" = "GPSP22T" ,
                        "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda:" = "GPSP23T" ,
                        "24. En esta casa: (APLICA TARJETON)" = "GPSP24T" ,
                        "25. En esta casa alguien: (APLICA TARJETON)" = "" ,
                        "26. En el último año, por cuestiones de seguridad Usted ha pensado:" = "GPSP25T" ,
                        "27. En el último año, por cuestiones de seguridad Usted dejó de:" = "GPSP26T" ,
                        "28. En esta calle o zona la policía:" = "GPSP28T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía " = "GPSP291T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar " = "GPSP292T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona " = "GPSP293T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal " = "GPSP294T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal " = "GPSP295T" ,
                        "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador " = "GPSP296T" ,
                        "30. Del 1 al 5, califique el trabajo de: La policía" = "GPSP301T" ,
                        "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar" = "GPSP302T" ,
                        "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno" = "GPSP303T" ,
                        "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida" = "GPSP304T" ,
                        "30. Del 1 al 5, califique el trabajo de: Su presidente municipal" = "GPSP305T" ,
                        "30. Del 1 al 5, califique el trabajo de: El Gobernador" = "GPSP306T" ,                          
                        "31. Del 1 al 5, califique el trato que recibe de: La policía" = "GPSP30T" ,
                        "31. Del 1 al 5, califique el trato que recibe de:El Ministerio Público para denunciar" = "GPSP311T" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Los empleados de gobierno" = "GPSP312T" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su Comisario ejidal" = "GPSP313T" ,
                        "31. Del 1 al 5, califique el trato que recibe de: Su presidente municipa" = "GPSP314T" ,
                        "31. Del 1 al 5, califique el trato que recibe de: El Gobernador" = "GPSP315T"                          
                        
                        )      
    )}









  })
  

  
  
  # Visualizacion secundaria 
  ##-- + Dados do candidato e eleição selecionada ----
  observe({
    
    x <- input$enfoque
    
    if (is.null(x))
      x <- character(0)
    # Poblacion y migracion
    # DG , DF, DE, ID, VI, AE
    if (x== "DF"){
      updateSelectInput(session, "pregunta",  
                        choices = c("¿Qué medio de transporte utilizan?" = "PFAM1" , "¿Dónde adquiere sus víveres?" = "PFAM2"," ¿A dónde acude en caso de urgencia médica? " = "PFAM3", "¿Con qué áreas de recreo cuenta en su colonia?" = "PFAM4")
                        
      )} else  if (x== "DE"){
    updateSelectInput(session, "pregunta",  
                      choices = c("¿Cuál es el principal trabajo pagado del jefe o jefa de familia?" = "PPECO1" , "Máximo nivel de estudios completo del jefe p jefa de familia" = "PPECO2","Puesto o posición de trabajo del jefe o jefa de familia " = "PPECO3", "¿A cuánto asciende el salario total semanal del jefe o jefa de familia?" = "PPECO4", "Además del jefe de familia ¿Cuántas personas trabajan en el hogar con salario remunerado?"="PPECO5" , "Número de personas que no perciben ingreso económico"= "PPECO6", "¿Cuánto tiempo tarda el jefe de familia en llegar a su lugar de trabajo?"  ="PPECO7" )
                      
    )} else  if (x== "ID"){
      updateSelectInput(session, "pregunta",  
                        choices = c("¿Cuál es su lugar de origen? " = "PPIyC1" , "¿Cuánto tiempo lleva viviendo en este lugar?" = "PPIyC2","¿Qué lo motivó a venir a vivir en esta localidad?" = "PPIyC3", " ¿Qué religión practica? " = "PPIyC4", "¿Cuáles son las ventajas de vivir en este lugar?"="PPIyC5",  "¿Piensa irse a vivir a otra localidad?"= "PPIyC6", "¿Usted a qué municipio siente que pertenece?" = "PPIyC7","¿Qué tan frecuente va a la Isla, la cabecera municipal de Isla Mujeres?" = "PPIyC8","¿Cuáles son los motivos por los que viaja a la Isla?"= "PPIyC9" )
                    
  )} else  if (x== "VI"){
    updateSelectInput(session, "pregunta",  
                      choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1", " Inundaciones " = "V1I1")
                      
                      
  )} else  if (x== "AE"){
    updateSelectInput(session, "pregunta",  
                      choices = c("Sin informacion" = "DF1")      
    )} 
    #SALINAS
    else  if (x== "ambi1"){
      updateSelectInput(session, "pregunta",  
                        choices = c("Qué uso le dan sus vecinos a la salina?" = "PAMB1" , "¿Qué beneficio recibe de vivir aquí?" = "PAMB2","¿Qué desventajas recibe de vivir aquí cerca de la salina? " = "PAMB3", "¿En qué condiciones considera que se encuentra la salina? " = "PAMB4", "¿Qué efectos genera la condición de la Salina?" = "PAMB5")
                        
    )} else  if (x== "soc1"){
      updateSelectInput(session, "pregunta",  
                        choices = c("¿Entre los vecinos, realizan alguna actividad en común?" = "SOC1" , "¿Cómo es la relación con sus vecinos?" = "SOC2"," ¿Ha tenido problemas con sus vecinos? " = "SOC3", " ¿Con qué frecuencia se hacen favores entre vecinos? " = "SOC4", "¿En algún problema que se le presente, sus vecinos le ayudan?"= "SOC5")
                          
    )} else  if (x== "eco1"){
      updateSelectInput(session, "pregunta",  
    choices = c("¿Cuántas personas de esta familia trabajan? " = "ECO1" , "¿En qué trabajan?" = "ECO2"," ¿Realizan alguna actividad productiva por su cuenta? " = "ECO3", " ¿Intercambian productos con sus vecinos? " = "ECO4")      
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
    # if(input$showmapa=="ALL"){ GraphM(percepcion) }
    #  else
    if(input$showmapa=="PS"){ GraphM(percepcion) }
    else if(input$showmapa=="IS"){ GraphM(filtro) }
    else if(input$showmapa=="EJ"){ GraphM(df_fin) }
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
      mosaics(tbl15, c(90,0,0,0), c(0,0,0,2))
    }else{
      if(input$var1 == "Origen" & input$var2 == "Puesto" & input$var3 == "Sexo"){
        mosaics(tbl16, c(0,0,0,0), c(0,0,0,2))
      }else{
        if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Edad"){
          mosaics(tbl17, c(0,0,0,0), c(0,0,0,2))
        }else{
          if(input$var1 == "Origen" & input$var2 == "Trabajo" & input$var3 == "Sexo"){
            mosaics(tbl18, c(0,0,0,0), c(0,0,0,2))
          }else{
            if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Edad"){
              mosaics(tbl19, c(90,0,0,0), c(0,0,0,2))
            }else{
              if(input$var1 == "Origen" & input$var2 == "Escolaridad" & input$var3 == "Sexo"){
                mosaics(tbl20, c(90,0,0,0), c(0,0,0,2))
              }else{
                if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Edad"){
                  mosaics(tbl21, c(0,0,0,0), c(0,0,0,2))
                }else{
                  if(input$var1 == "Origen" & input$var2 == "Ingreso_sem" & input$var3 == "Sexo"){
                    mosaics(tbl22, c(0,0,0,0), c(0,0,0,2))
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
                                                                mosaics(tbl2, c(0,0,0,0), c(0,0,0,6.5))
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
  output$Expl1 <- renderUI({
    if(input$var3 == "Edad"){
      word <- paste("<b>",tags$i("o Edad:"),"</b>",tags$i("Grupo A: Encuestados entre 18 y 40 aÃ±os,
                                                          Grupo B: Encuestados mayores de 40 aÃ±os"))
      HTML(paste(word))
    }
  })
  output$Expl2 <- renderUI({
    if(input$var2 == "Ingreso_sem"){
      word <- paste("<b>",tags$i("o Ingreso semanal:"),"</b>",tags$i("Grupo A: $0 a $1200,
                                                          Grupo B: $1201 a $2500,
                                                          Grupo C: $2501 a $5000,
                                                          Grupo D: MÃ¡s de $5000"))
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
                 estas se conectan por medio de flechas que van del antecedente al consecuente. En el medio de estas flechas de conexiones, podemos encontrar un círculo cuyo tam. 
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



