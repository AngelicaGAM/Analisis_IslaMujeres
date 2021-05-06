
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
require(devtools)


library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(DT)
library(shiny)
library(dipsaus)
library(scales)

library(plotly)

library(arulesViz)
library(likert)
library(plotrix)
library(scales)
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(shinydashboard)
library(RColorBrewer)
library(shinycssloaders)
library(psych)
library(sp)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(plyr)
library(janitor)

library(visNetwork)


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
                   "1. En esta calle o zona, Usted participa: <br><br>En eventos deportivos<br>En tandas<br>En fiestas<br>En iglesia o templo<br>En otras actividades<br>Para solucionar problemas de la comunidad<br>2. Usted conoce a sus vecinos: <br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Les confiaría a los niños<br>Les confiaría su casa<br>Participa con ellos para mejorar la seguridad<br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesaría participar<br>Día en que podría participar en actividades con su comunidad. <br>Horario en que podría participar: Mañana Tarde Noche<br>3. Participa con la autoridad para mejorar la seguridad:<br><br>Respuesta “No”, desplegar el siguiente reactivo:<br>Le interesaría participar<br>4. Día en que podría participar en actividades con la autoridad<br><br>5. Horario en el que podría participar Mañana Tarde Noche<br><br>6. Cuando hay un delito, en esta calle o zona los vecinos:<br><br>Se reúnen<br>Se organizan para vigilar<br>Intercambian números telefónicos<br>Forman un chat<br>Ponen letreros de advertencia<br>Llaman a la policía<br>Denuncian ante la autoridad<br>Vigilan<br>Buscan desquitarse<br>No hacen nada<br>7. Durante el último año, en esta calle o zona ha habido:<br><br>Robo en casa<br>Robo en la calle<br>Robo en transporte<br>Robo en negocio<br>Robo de partes de auto<br>Robo de vehículo<br>Balaceras<br>Cobro de piso<br>Violencia familiar<br>Peleas de gallos o perros<br>8. Valore el riesgo de sufrir un delito en alguno de los siguientes<br><br>lugares:<br>En su casa<br>En esta calle<br>En esta zona<br>En esta ciudad<br>9. ¿Usted ha sido víctima de algún delito en el último año? si No<br><br>10. En caso de ser víctima del delito Usted:<br><br>Llama a la policía<br>Hace una denuncia<br>Advierte a sus vecinos del peligro<br>Advierte a su familia del peligro<br>Busca desquitarse<br>Amenazas<br>11. Ha sido víctima de algún delito y no denuncio:<br><br>¿Podría señalar las razones por las que no denunció?<br>Respuesta “Si”, desplegar el siguiente reactivo:<br>Falta de pruebas<br>Considera que es un delito de poca importancia<br>Conoce al agresor o agresores<br>Desconfía de las autoridades<br>Teme a que lo extorsionen<br>Falta de tiempo<br>Son trámites complicados<br>No sabe dónde denunciar<br>Aunque denuncie no va a pasar nada<br>12. En esta calle o zona:<br><br>Los padres participan en actividades con hijos<br>Los vecinos se organizan para prevenir delitos<br>Las personas son amables<br>Hay alguna persona que siempre ayuda a los demás<br>13.En esta calle o zona hay personas:<br><br>A las que todos tienen miedo<br>Que acosan a menores<br>Que acosan a mujeres<br>Que se emborrachan o se drogan<br>Que han estado en la cárcel<br>Sospechosas<br>14. En esta calle o zona hay violencia:<br><br>Entre mujeres<br>Entre hombres<br>Entre familias<br>Entre adultos y jóvenes<br>Entre jóvenes<br>15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan:<br><br>Amistosamente<br>Dialogando<br>De manera respetuosa<br>A gritos<br>Con golpes<br>Con cuchillos, navajas o machetes<br>Con armas de fuego, como pistolas o rifles<br>Desquitándose del otro<br>16. En esta calle o zona hay niños o adolescentes que se quedan<br><br>encerrados con llave:<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Por la inseguridad<br>Descuido de los padres<br>Castigo<br>Trabajo de los padres<br>17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer: Respuesta “Si”, desplegar los siguientes reactivos:<br><br>Por descuido de los padres<br>Castigo<br>Falta de dinero<br>Trabajo de los padres<br>18. En esta calle o zona hay jóvenes que:<br><br>Hacen deporte<br>Ayudan a los demás<br>La mayoría de los jóvenes estudian o trabajan.<br>Andan en pandillas<br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Andan armados<br>Destruyen o vandalizan la propiedad ajena<br>Son violentos<br>Amenazan a los vecinos<br>19. En esta calle o zona hay un parque<br><br>Respuesta “Si”, desplegar los siguientes reactivos:<br>Está en buen estado<br>¿Quiénes lo utilizan?: Mostrar menú abajo<br>Niños y niñas<br>Jóvenes<br>Pandillas<br>Familias<br>Adultos<br>Personas de la tercera edad<br>Hay actividades supervisadas por adultos<br>Vándalos<br>Usted lo utiliza<br>20. En esta calle o zona hay:<br><br>Banquetas<br>Baches<br>Letreros con nombres de las calles<br>Tiendita<br>Alumbrado<br>Consumo de alcohol en la calle<br>21. En esta zona hay:<br><br>Horarios de transporte que convienen<br>Una parada de camión cerca de esta casa<br>Terrenos baldíos<br>Basura<br>Autos abandonados<br>Casas abandonadas<br>Vandalismo<br>Grafiti<br>Venta de tiner o pegamento a menores<br>Venta de alcohol o cigarros a menores<br>Venta de droga<br>Venta de alcohol después de las 11:00 de la noche<br>22. En el último año Usted supo que algún menor de 18 años:<br><br>Se fue de la casa<br>Sufrió violencia<br>Abandonó la escuela<br>Tiene problemas de conducta<br>Quedó embarazada<br>23. Para corregir a un niño o niña que se porta mal, Usted recomienda:<br><br>Castigarle<br>Gritarle<br>Darle nalgadas<br>Darle una golpiza / cueriza <br>Explicarle lo que está mal<br>Aconsejarle<br>Darle buen ejemplo24. En esta casa: <br>Todos se conocen<br>Platican unos con otros<br>Comen juntos<br>Se ayudan con los gastos<br>Discuten<br>Se gritan entre sí<br>Llegan a los golpes<br>Se ignoran<br>En esta casa alguien: <br>Tiene discapacidad SI LA RESPUESTA ES SI, ENTONCES:<br>Por su discapacidad, ha vivido violencia<br>Sabe manejar armas de fuego, como pistolas o rifles<br>Habla de comprar armas de fuego<br>Habla lengua indigena<br>Necesita ayuda por obesidad<br>Necesita ayuda por fumar<br>Necesita ayuda por beber<br>Necesita ayuda por drogas<br>26. En el último año, por cuestiones de seguridad Usted ha<br>pensado:<br>Cambiarse de casa<br>Cambiarse de ciudad<br>Cambiarse de estado<br>Cambiar de trabajo N/A<br>Cerrar su negocio N/A<br>Cambiar a los hijos de escuela N/ASeguir como hasta ahora<br>27. En el último año, por cuestiones de seguridad Usted dejó de:Dejo de salir de noche<br><br>Dejo de salir a caminar o hacer ejercicio<br>Impidio que los niños salgan a la calle N/A<br>Evito relacionarse con nuevas personas<br>Dejo de visitar a parientes o amigos<br>Dejo de usar transporte público /combi N/A<br>Dejo de usar taxi<br>Dejo de llevar mucho dinero en efectivo<br>Dejo de usar joyas<br>28. En esta calle o zona la policía:<br>Cuida o vigila bien<br>Comete abusos<br>Acude a los llamados<br>Pide mordidas<br>Hace rondines<br>Comete delitos<br>"),
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
            else if(input$pregunta=="GPSI24"){TPSI24}
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
             else if(input$pregunta=="GPSC24"){TPSC24}
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
            else if(input$pregunta=="GPSE24"){TPSE24}
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
            

                #percepcion de seguridad 
             else if(input$pregunta=="GPSVS1"){TPSVS1}
             else if(input$pregunta=="GPSVS2"){TPSVS2}
             else if(input$pregunta=="GPSVS3"){TPSVS3}
             else if(input$pregunta=="GPSVS4"){TPSVS4}
             else if(input$pregunta=="GPSVS5"){TPSVS5}
             else if(input$pregunta=="GPSVS6"){TPSVS6}
             else if(input$pregunta=="GPSVS7"){TPSVS7}
             else if(input$pregunta=="GPSVS81"){TPSVS81}
             else if(input$pregunta=="GPSVS82"){TPSVS82}
             else if(input$pregunta=="GPSVS83"){TPSVS83}
             else if(input$pregunta=="GPSVS84"){TPSVS84}
             else if(input$pregunta=="GPSVS9"){TPSVS9}
             else if(input$pregunta=="GPSVS10"){TPSVS10}
             else if(input$pregunta=="GPSVS11"){TPSVS11}
             else if(input$pregunta=="GPSVS12"){TPSVS12}
             else if(input$pregunta=="GPSVS13"){TPSVS13}
             else if(input$pregunta=="GPSVS14"){TPSVS14}
             else if(input$pregunta=="GPSVS15"){TPSVS15}
             else if(input$pregunta=="GPSVS16"){TPSVS16}
             else if(input$pregunta=="GPSVS17"){TPSVS17}
             else if(input$pregunta=="GPSVS18"){TPSVS18}
             else if(input$pregunta=="GPSVS19"){TPSVS19}
             else if(input$pregunta=="GPSVS20"){TPSVS20}
             else if(input$pregunta=="GPSVS21"){TPSVS21}
             else if(input$pregunta=="GPSVS22"){TPSVS22}
             else if(input$pregunta=="GPSVS23"){TPSVS23}
             else if(input$pregunta=="GPSVS24"){TPSVS24}
             else if(input$pregunta=="GPSVS25"){TPSVS25}
             else if(input$pregunta=="GPSVS26"){TPSVS26}
             else if(input$pregunta=="GPSVS27"){TPSVS27}
             else if(input$pregunta=="GPSVS28"){TPSVS28}
             else if(input$pregunta=="GPSVS291"){TPSVS291}
             else if(input$pregunta=="GPSVS292"){TPSVS292}
             else if(input$pregunta=="GPSVS293"){TPSVS293}
             else if(input$pregunta=="GPSVS294"){TPSVS294}
             else if(input$pregunta=="GPSVS295"){TPSVS295}
             else if(input$pregunta=="GPSVS296"){TPSVS296}
             else if(input$pregunta=="GPSVS301"){TPSVS301}
             else if(input$pregunta=="GPSVS302"){TPSVS302}
             else if(input$pregunta=="GPSVS303"){TPSVS303}
             else if(input$pregunta=="GPSVS304"){TPSVS304}
             else if(input$pregunta=="GPSVS305"){TPSVS305}
             else if(input$pregunta=="GPSVS306"){TPSVS306}
             else if(input$pregunta=="GPSVS311"){TPSVS311}
             else if(input$pregunta=="GPSVS312"){TPSVS312}
             else if(input$pregunta=="GPSVS313"){TPSVS313}
             else if(input$pregunta=="GPSVS314"){TPSVS314}
             else if(input$pregunta=="GPSVS315"){TPSVS315}
             else if(input$pregunta=="GPSVS316"){TPSVS316}

 
    
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
            else if(input$pregunta=="GPSI24"){GPSI24}
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
            else if(input$pregunta=="GPSC24"){GPSC24}
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
             else if(input$pregunta=="GPSE24"){GPSE24}
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
             else if(input$pregunta=="GPSVS24"){GPSVS24}
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

  output$noteGra <- renderText ({
     if(input$pregunta=="PFAM01"
     || input$pregunta== "GPSI1"
     || input$pregunta== "GPSC1"
     || input$pregunta== "GPSE1"
     || input$pregunta== "GPSVS1"
     
     || input$pregunta== "GPSI2"
     || input$pregunta== "GPSC2"
     || input$pregunta== "GPSE2"
     || input$pregunta== "GPSVS2"

     || input$pregunta== "GPSI4"
     || input$pregunta== "GPSC4"
     || input$pregunta== "GPSE4"
     || input$pregunta== "GPSVS4"

     || input$pregunta== "GPSI5"
     || input$pregunta== "GPSC5"
     || input$pregunta== "GPSE5"
     || input$pregunta== "GPSVS5"

     || input$pregunta== "GPSI6"
     || input$pregunta== "GPSC6"
     || input$pregunta== "GPSE6"
     || input$pregunta== "GPSVS6"

     || input$pregunta== "GPSI10"
     || input$pregunta== "GPSC10"
     || input$pregunta== "GPSE10"
     || input$pregunta== "GPSVS10"

     || input$pregunta== "GPSI15"
     || input$pregunta== "GPSC15"
     || input$pregunta== "GPSE15"
     || input$pregunta== "GPSVS15"
    
    || input$pregunta== "GPSI18"
    || input$pregunta== "GPSC18"
    || input$pregunta== "GPSE18"
    || input$pregunta== "GPSVS18"

    || input$pregunta== "GPSI19"
    || input$pregunta== "GPSC19"
    || input$pregunta== "GPSE19"
    || input$pregunta== "GPSVS19"

    || input$pregunta== "GPSI20"
    || input$pregunta== "GPSC20"
    || input$pregunta== "GPSE20"
    || input$pregunta== "GPSVS20"

     || input$pregunta== "GPSI21"
     || input$pregunta== "GPSC21"
     || input$pregunta== "GPSE21"
     || input$pregunta== "GPSVS21"

    || input$pregunta== "GPSI27"
    || input$pregunta== "GPSC27"
    || input$pregunta== "GPSE27"
    || input$pregunta== "GPSVS27"

     || input$pregunta== "GPSI28"
     || input$pregunta== "GPSC28"
     || input$pregunta== "GPSE28"
     || input$pregunta== "GPSVS28"

    || input$pregunta== "PPIyC1"
    || input$pregunta== "PPIyC3"
    || input$pregunta== "PPIyC6"

     ){"Nota: Los valores porcentuales en está pregunta pueden variarar, dado que es una pregunta de opción multiple. "} 


     else if (  input$pregunta== "GPSE11"   
            ||  input$pregunta== "GPSI17" 
              
     
     ){"Nota: Sin información "}
            
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
  
  # Mosaicos
  observeEvent(input$var1,{
  
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
                        "4. ¿Dónde adquiere sus víveres?" = "PFAM2"," 
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
                          "5. A pesar de la inundación usted decidió quedarse a vivir aquí por: " = "V1P5R1", 
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
                            "4. Día en que podría participar en actividades con la autoridad  - IM " =  "GPSI4",
                            "5. Horarios en los que podría participar en actividades con la autoridad  - IM " = "GPSI5",
                            "6. Cuando hay un delito, en esta calle o zona los vecinos: - IM " = "GPSI6", 
                            "7. Durante el último año, en esta calle o zona ha habido: - IM " = "GPSI7" ,
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - IM " = "GPSI81",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - IM " = "GPSI82",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - IM " = "GPSI83",
                            "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - IM " = "GPSI84",
                            "9. ¿Usted ha sido víctima de algún delito en el último año? - IM " = "GPSI9" ,
                            "10. En caso de ser víctima del delito Usted: - IM " = "GPSI10", 
                            "11. Ha sido víctima de algún delito y no denuncio: - IM " = "GPSI11" ,
                            "12. En esta calle o zona: - IM " = "GPSI12" ,
                            "13. En esta calle o zona, hay personas - IM " = "GPSI13", 
                            "14. En esta calle o zona hay violencia: - IM " = "GPSI14" ,
                            "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - IM " = "GPSI15" ,
                            "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - IM " = "GPSI16" ,
                            "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:  - IM " = "GPSI17" ,
                            "18. En esta calle o zona hay jóvenes que: - IM " = "GPSI18" ,
                            "19. En esta calle o zona hay un parquI - IM " = "GPSI19" ,
                            "20. En esta calle o zona hay: - IM " = "GPSI20" ,
                            "21. En esta zona hay - IM " = "GPSI21" ,
                            "22. En el último año Usted supo que algún menor de 18 años: - IM " = "GPSI22" ,
                            "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - IM " = "GPSI23" ,
                            "24. En esta casa:  - IM " = "GPSI24" ,
                            "25. En esta casa alguien:  - IM " = "GPSI25" ,
                            "26. En el último año, por cuestiones de seguridad Usted ha pensado: - IM " = "GPSI26" ,
                            "27. En el último año, por cuestiones de seguridad Usted dejó de: - IM " = "GPSI27" ,
                            "28. En esta calle o zona la policía: - IM " = "GPSI28" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía  - IM " = "GPSI291" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - IM " = "GPSI292" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - IM " = "GPSI293" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - IM " = "GPSI294" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - IM " = "GPSI295" ,
                            "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - IM " = "GPSI296" ,
                            "30. Del 1 al 5, califique el trabajo de: La policía - IM " = "GPSI301" ,
                            "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - IM " = "GPSI302" ,
                            "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - IM " = "GPSI303" ,
                            "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - IM " = "GPSI304" ,
                            "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - IM " = "GPSI305" ,
                            "30. Del 1 al 5, califique el trabajo de: El Gobernador - IM " = "GPSI306" ,                          
                            "31. Del 1 al 5, califique el trato que recibe de: La policía - IM " = "GPSI311" ,
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
                                  "2. Usted conoce a sus vecinos:  - EJ " = "GPSE2" ,
                                  "3. Participa con la autoridad para mejorar la seguridad:  - EJ " = "GPSE3" ,
                                  "4. Día en que podría participar en actividades con la autoridad  - EJ" =  "GPSE4",
                                  "5. Horarios en los que podría participar en actividades con la autoridad  - EJ" = "GPSE5",
                                  "6. Cuando hay un delito, en esta calle o zona los vecinos: - EJ " = "GPSE6", 
                                  "7. Durante el último año, en esta calle o zona ha habido: - EJ " = "GPSE7" ,
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - EJ " = "GPSE81",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - EJ " = "GPSE82",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - EJ " = "GPSE83",
                                  "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - EJ " = "GPSE84",
                                  "9. ¿Usted ha sido víctima de algún delito en el último año? - EJ " = "GPSE9I" ,
                                  "10. En caso de ser víctima del delito Usted: - EJ " = "GPSE10", 
                                  "11. Ha sido víctima de algún delito y no denuncio: - EJ " = "GPSE11" ,
                                  "12. En esta calle o zona: - EJ " = "GPSE12" ,
                                  "13. En esta calle o zona, hay personas - EJ " = "GPSE13", 
                                  "14. En esta calle o zona hay violencia: - EJ " = "GPSE14" ,
                                  "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - EJ " = "GPSE15" ,
                                  "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - EJ " = "GPSE16" ,
                                  "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:  - EJ " = "GPSE17" ,
                                  "18. En esta calle o zona hay jóvenes que: - EJ " = "GPSE18" ,
                                  "19. En esta calle o zona hay un parquI - EJ " = "GPSE19" ,
                                  "20. En esta calle o zona hay: - EJ " = "GPSE20" ,
                                  "21. En esta zona hay - EJ " = "GPSE21" ,
                                  "22. En el último año Usted supo que algún menor de 18 años: - EJ " = "GPSE22" ,
                                  "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - EJ " = "GPSE23" ,
                                  "24. En esta casa:  - EJ " = "GPSE24" ,
                                  "25. En esta casa alguien:  - EJ " = "GPSE25" ,
                                  "26. En el último año, por cuestiones de seguridad Usted ha pensado: - EJ " = "GPSE26" ,
                                  "27. En el último año, por cuestiones de seguridad Usted dejó de: - EJ " = "GPSE27" ,
                                  "28. En esta calle o zona la policía: - EJ " = "GPSE28" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía  - EJ " = "GPSE291" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - EJ " = "GPSE292" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - EJ " = "GPSE293" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - EJ " = "GPSE294" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - EJ " = "GPSE295" ,
                                  "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - EJ " = "GPSE296" ,
                                  "30. Del 1 al 5, califique el trabajo de: La policía - EJ " = "GPSE301" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - EJ " = "GPSE302" ,
                                  "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - EJ " = "GPSE303" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - EJ " = "GPSE304" ,
                                  "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - EJ " = "GPSE305" ,
                                  "30. Del 1 al 5, califique el trabajo de: El Gobernador - EJ " = "GPSE306" ,                          
                                  "31. Del 1 al 5, califique el trato que recibe de: La policía - EJ " = "GPSE311" ,
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
                              "2. Usted conoce a sus vecinos: - CUN " = "GPSC2" ,
                              "3. Participa con la autoridad para mejorar la seguridad: - CUN " = "GPSC3" ,
                              "4. Día en que podría participar en actividades con la autoridad - CUN" =  "GPSC4",
                              "5. Horarios en los que podría participar en actividades con la autoridad - CUN" = "GPSC5",
                              "6. Cuando hay un delito, en esta calle o zona los vecinos: - CUN" = "GPSC6", 
                              "7. Durante el último año, en esta calle o zona ha habido: - CUN" = "GPSC7" ,
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Casa - CUN" = "GPSC81",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Calle - CUN" = "GPSC82",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Zona - CUN" = "GPSC83",
                              "8. Valore el riesgo de sufrir un delito en alguno de los siguientes lugares: Ciudad - CUN" = "GPSC84",
                              "9. ¿Usted ha sido víctima de algún delito en el último año? - CUN" = "GPSC9I" ,
                              "10. En caso de ser víctima del delito Usted: - CUN" = "GPSC10", 
                              "11. Ha sido víctima de algún delito y no denuncio: - CUN" = "GPSC11" ,
                              "12. En esta calle o zona: - CUN" = "GPSC12" ,
                              "13. En esta calle o zona, hay personas - CUN" = "GPSC13", 
                              "14. En esta calle o zona hay violencia: - CUN" = "GPSC14" ,
                              "15. En esta calle o zona, cuando hay conflictos entre vecinos se manejan: - CUN" = "GPSC15" ,
                              "16. En esta calle o zona hay niños o adolescentes que se quedan encerrados con llave: - CUN" = "GPSC16" ,
                              "17. En esta calle o zona hay niños que se quedan la mayor parte del día sin comer:  - CUN" = "GPSC17" ,
                              "18. En esta calle o zona hay jóvenes que: - CUN" = "GPSC18" ,
                              "19. En esta calle o zona hay un parquI - CUN" = "GPSC19" ,
                              "20. En esta calle o zona hay: - CUN" = "GPSC20" ,
                              "21. En esta zona hay - CUN" = "GPSC21" ,
                              "22. En el último año Usted supo que algún menor de 18 años: - CUN" = "GPSC22" ,
                              "23 .Para corregir a un niño o niña que se porta mal, Usted recomienda: - CUN" = "GPSC23" ,
                              "24. En esta casa:  - CUN" = "GPSC24" ,
                              "25. En esta casa alguien:  - CUN" = "GPSC25" ,
                              "26. En el último año, por cuestiones de seguridad Usted ha pensado: - CUN" = "GPSC26" ,
                              "27. En el último año, por cuestiones de seguridad Usted dejó de: - CUN" = "GPSC27" ,
                              "28. En esta calle o zona la policía: - CUN" = "GPSC28" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: La policía  - CUN" = "GPSC291" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Ministerio Público para denunciar  - CUN" = "GPSC292" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Las escuelas públicas de la zona  - CUN" = "GPSC293" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su Comisario ejidal  - CUN" = "GPSC294" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: Su presidente municipal  - CUN" = "GPSC295" ,
                              "29. Del 1 al 5, con el 5 como mejor calificación, califique la confianza que Usted tiene en: El Gobernador  - CUN" = "GPSC296" ,
                              "30. Del 1 al 5, califique el trabajo de: La policía - CUN" = "GPSC301" ,
                              "30. Del 1 al 5, califique el trabajo de: El Ministerio Público para denunciar - CUN" = "GPSC302" ,
                              "30. Del 1 al 5, califique el trabajo de: Los empleados de gobierno - CUN" = "GPSC303" ,
                              "30. Del 1 al 5, califique el trabajo de: Su Comisario Ejida - CUN" = "GPSC304" ,
                              "30. Del 1 al 5, califique el trabajo de: Su presidente municipal - CUN" = "GPSC305" ,
                              "30. Del 1 al 5, califique el trabajo de: El Gobernador - CUN" = "GPSC306" ,                          
                              "31. Del 1 al 5, califique el trato que recibe de: La policía - CUN" = "GPSC311" ,
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
                                  "2. Usted conoce a sus vecinos: - VS " = "GPSVS2" ,
                                  "3. Participa con la autoridad para mejorar la seguridad: - VS " = "GPSVS3" ,
                                  "4. Día en que podría participar en actividades con la autoridad  - VS " =  "GPSVS4",
                                  "5. Horarios en los que podría participar en actividades con la autoridad  - VS " = "GPSVS5",
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
                                  "24. En esta casa:  - VS " = "GPSVS24" ,
                                  "25. En esta casa alguien:  - VS " = "GPSVS25" ,
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
      else if(input$tipomapa=="IS"){ "Estudio Socioeconómico y Ambiental" }
      else if(input$tipomapa=="EJ"){ "Características sobre población y migración"}
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
    if(input$showmapa=="ALL"){ "Estudios socioeconomicos, Percepción de seguridad y Características sobre población y migración. " }
    else if(input$showmapa=="PS"){ "Percepción sobre seguridad" }
    else if(input$showmapa=="IS"){ "Estudio Socioeconómico y Ambiental" }
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
 

 # asociacion

output$txt81 <- renderUI({
    #income_rules <-rules()
    if(input$selec == "Percepción de Seguridad" ){
      if(input$len>input$mlen){
        word <- paste("El mínimo de combinaciones de reglas no puede superar al máximo, por lo que es necesario reducir o aumentar, el número de combinaciones para la regla de asociación. Modificar en la barra lateral izquierda, debajo de 'Mín. Combinación reglas:' o de 'Máx. Combinación reglas:'")
       }else{
        word = " " 
      }
    }else{
      if(input$len>input$mlen){
        word <- paste("El mínimo de combinaciones de reglas no puede superar al máximo, por lo que es necesario reducir o aumentar, el número de combinaciones para la regla de asociación. Modificar en la barra lateral izquierda, debajo de 'Mín. Combinación reglas:' o de 'Máx. Combinación reglas:'")
      }else{
        word = " "
      }
    }
    HTML(paste(word))
  })


  
  
  

  
  
   

output$data <- DT::renderDataTable({
  if(input$selec == "Percepción de Seguridad"){
    DT::datatable(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")[, input$show_vars, drop = FALSE])
  }else{
    DT::datatable(read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[, input$show_vards, drop = FALSE])
  }
} )


output$sum <- renderPrint({
  if(input$selec == "Percepción de Seguridad"){
    summary(read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")[, input$show_vars, drop = FALSE])
  }else{
    summary(read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[, input$show_vards, drop = FALSE])
  }
})


Func_A <- function(df_ide, showvar, in_sex, in_edad, D.Calle, N.Ext, N.int, Manza0, Mz, Col,CP,Cas_a,Cas_N,Kin,prim,sec,bach,lic,autobu,colec,taxi,motax,moto,auto,v_col,abarro,super,conv,plaza,urg_c,urgpatc,urgpart,urgh,urgc,urgf,urgo,parque,unid,jardin,unidaddep,biblio,aotro,trbajan,trabajo,puesto,ingreso,escola,sining,tiempo,colon,muni,origen,tiempoi,mpar,mamig,mtrab,mnego,mopor,motro,acudvec,acudfam,acudaut,acudig,acudotro,relig,venc,ventr, ventfam,venttiem,venttranq,ventseg,ventotro,emigrar,perten,frecc,mvasun,mvfds,mvtrab,mvrec,mvfam,mvotro,vve,conta,heren,mens,cpmen,cpadq,inun,hura,ssr,safec,aver,abanq,alum,trans,pat,lot,pseg,pcom, pries){
  df_ide <- df_ide[, showvar, drop = FALSE]
  
  if("Info.Sexo" %in% showvar){
    if(length(in_sex)!=0){
      df_ide <- dplyr::filter(df_ide, Info.Sexo %in% in_sex)
    }
  }
  
  if("Info.Edad" %in% showvar){
    if(length(in_edad)!=0){
      df_ide <- dplyr::filter(df_ide, Info.Edad %in% in_edad)
      
    }
  }
  
  
  if("Direccion.Colonia" %in% showvar){
    if(length(Col)!=0){
      df_ide <- dplyr::filter(df_ide, Direccion.Colonia %in% Col)
    }
  }
  
  
  if("V01_Casa_Adultos" %in% showvar){
    if(length(Cas_a)!=0){
      df_ide <- dplyr::filter(df_ide, V01_Casa_Adultos %in% Cas_a)
      
    }
  }
  
  
  if("V02_Casa_Ninos" %in% showvar){
    if(length(Cas_N)!=0){
      df_ide <- dplyr::filter(df_ide, V02_Casa_Ninos %in% Cas_N)
      ##updateSelectInput(getDefaultReactiveDomain(),"Info.Sexo","Sexo", choices = unique(df_ide[,"Info.Sexo"]))
      
    }
  }
  
  
  if("DF_kinder" %in% showvar){
    if(length(Kin)!=0){
      df_ide <- dplyr::filter(df_ide, DF_kinder %in% Kin)
      
    }
  }
  
  if("DF_primaria" %in% showvar){
    if(length(prim)!=0){
      df_ide <- dplyr::filter(df_ide, DF_primaria %in% prim)
      
    }
  }
  
  
  if("DF_secundaria" %in% showvar){
    if(length(sec)!=0){
      df_ide$DF_secundaria    <- factor(df_ide$DF_secundaria,
                                        levels = c(0,1,2,3,4),
                                        ordered = TRUE)
      df_ide <- dplyr::filter(df_ide, DF_secundaria %in% sec)
      
    }
  }
  if("DF_bachillerato" %in% showvar){
    if(length(bach)!=0){
      df_ide <- dplyr::filter(df_ide, DF_bachillerato %in% bach)
      
      
    }
  }
  if("DF_licenciatura" %in% showvar){
    if(length(lic)!=0){
      df_ide <- dplyr::filter(df_ide, DF_licenciatura %in% lic)
      
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
      
    }
  }
  
  
  if("DF_tr_autobus" %in% showvar){
    if(length(autobu)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_autobus %in% autobu)
      
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_tr_colectivo" %in% showvar){
    if(length(colec)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_colectivo %in% colec)
      
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
      
    }
  }
  
  if("DF_tr_taxi" %in% showvar){
    if(length(taxi)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_taxi %in% taxi)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_tr_mototaxi" %in% showvar){
    if(length(motax)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_mototaxi %in% motax)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  
  if("DF_tr_moto" %in% showvar){
    if(length(moto)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_moto %in% moto)
    }
  }
  if("DF_tr_auto" %in% showvar){
    if(length(auto)!=0){
      df_ide <- dplyr::filter(df_ide, DF_tr_auto %in% auto)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_v_col" %in% showvar){
    if(length(v_col)!=0){
      df_ide <- dplyr::filter(df_ide, DF_v_col %in% v_col)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DF_v_abarrote" %in% showvar){
    if(length(abarro)!=0){
      df_ide <- dplyr::filter(df_ide, DF_v_abarrote %in% abarro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DF_v_super" %in% showvar){
    if(length(super)!=0){
      df_ide <- dplyr::filter(df_ide, DF_v_super %in% super)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_v_conv" %in% showvar){
    if(length(conv)!=0){
      df_ide <- dplyr::filter(df_ide, DF_v_conv %in% conv)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_v_plaza" %in% showvar){
    if(length(plaza)!=0){
      df_ide <- dplyr::filter(df_ide, DF_v_plaza %in% plaza)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_urg_casa" %in% showvar){
    if(length(urg_c)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_casa %in% urg_c)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DF_urg_partcom" %in% showvar){
    if(length(urgpatc)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_partcom %in% urgpatc)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_urg_partcan" %in% showvar){
    if(length(urgpart)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_partcan %in% urgpart)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_urg_hosp" %in% showvar){
    if(length(urgh)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_hosp %in% urgh)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_urg_cruz" %in% showvar){
    if(length(urgc)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_cruz %in% urgc)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  
  if("DF_urg_farmacia" %in% showvar){
    if(length(urgf)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_farmacia %in% urgf)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_urg_otro" %in% showvar){
    if(length(urgo)!=0){
      df_ide <- dplyr::filter(df_ide, DF_urg_otro %in% urgo)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_a_parque" %in% showvar){
    if(length(parque)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_parque %in% parque)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_a_unidad" %in% showvar){
    if(length(unid)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_unidad %in% unid)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DF_a_jardines" %in% showvar){
    if(length(jardin)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_jardines %in% jardin)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_a_casa" %in% showvar){
    if(length(unidaddep)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_casa %in% unidaddep)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_a_biblioteca" %in% showvar){
    if(length(biblio)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_biblioteca %in% biblio)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DF_a_otro" %in% showvar){
    if(length(aotro)!=0){
      df_ide <- dplyr::filter(df_ide, DF_a_otro %in% aotro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DE01_hogar_trabajan" %in% showvar){
    if(length(trbajan)!=0){
      df_ide <- dplyr::filter(df_ide, DE01_hogar_trabajan %in% trbajan)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE02_trabajo" %in% showvar){
    if(length(trabajo)!=0){
      df_ide <- dplyr::filter(df_ide, DE02_trabajo %in% trabajo)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE03_puesto" %in% showvar){
    if(length(puesto)!=0){
      df_ide <- dplyr::filter(df_ide, DE03_puesto %in% puesto)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE05_ingreso_sem" %in% showvar){
    if(length(ingreso)!=0){
      df_ide <- dplyr::filter(df_ide, DE05_ingreso_sem %in% ingreso)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DE06_esc_jefe" %in% showvar){
    if(length(escola)!=0){
      df_ide <- dplyr::filter(df_ide, DE06_esc_jefe %in% escola)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE07_sIngreso" %in% showvar){
    if(length(sining)!=0){
      df_ide <- dplyr::filter(df_ide, DE07_sIngreso %in% sining)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE08_tmp" %in% showvar){
    if(length(tiempo)!=0){
      df_ide <- dplyr::filter(df_ide, DE08_tmp %in% tiempo)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("DE09_col" %in% showvar){
    if(length(colon)!=0){
      df_ide <- dplyr::filter(df_ide, DE09_col %in% colon)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("DE10_mun" %in% showvar){
    if(length(muni)!=0){
      df_ide <- dplyr::filter(df_ide, DE10_mun %in% muni)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC02_Estado_Origen" %in% showvar){
    if(length(origen)!=0){
      df_ide <- dplyr::filter(df_ide, IyC02_Estado_Origen %in% origen)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC03_Tiempo" %in% showvar){
    if(length(tiempoi)!=0){
      df_ide <- dplyr::filter(df_ide, IyC03_Tiempo %in% tiempoi)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC04_Motivo_Localidad_Parientes" %in% showvar){
    if(length(mpar)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Parientes %in% mpar)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC04_Motivo_Localidad_Amigos" %in% showvar){
    if(length(mamig)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Amigos %in% mamig)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC04_Motivo_Localidad_Trabajo" %in% showvar){
    if(length(mtrab)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Trabajo %in% mtrab)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC04_Motivo_Localidad_Negocio" %in% showvar){
    if(length(mnego)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Negocio %in% mnego)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC04_Motivo_Localidad_Oportunidad" %in% showvar){
    if(length(mopor)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Oportunidad %in% mopor)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC04_Motivo_Localidad_Otro" %in% showvar){
    if(length(motro)!=0){
      df_ide <- dplyr::filter(df_ide, IyC04_Motivo_Localidad_Otro %in% motro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC05_Acudo_Vecinos" %in% showvar){
    if(length(acudvec)!=0){
      df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Vecinos %in% acudvec)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC05_Acudo_Familia" %in% showvar){
    if(length(acudfam)!=0){
      df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Familia %in% acudfam)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC05_Acudo_Autoridad" %in% showvar){
    if(length(acudaut)!=0){
      df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Autoridad %in% acudaut)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  
  if("IyC05_Acudo_Iglesia" %in% showvar){
    if(length(acudig)!=0){
      df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Iglesia %in% acudig)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC05_Acudo_Otro" %in% showvar){
    if(length(acudotro)!=0){
      df_ide <- dplyr::filter(df_ide, IyC05_Acudo_Otro %in% acudotro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC06_Religion" %in% showvar){
    if(length(relig)!=0){
      df_ide <- dplyr::filter(df_ide, IyC06_Religion %in% relig)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC08_Ventajas_Casa" %in% showvar){
    if(length(venc)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Casa %in% venc)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC08_Ventajas_Trabajo" %in% showvar){
    if(length(ventr)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Trabajo %in% ventr )
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC08_Ventajas_Familia" %in% showvar){
    if(length(ventfam)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Familia %in% ventfam )
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC08_Ventajas_Tiempo" %in% showvar){
    if(length(venttiem)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tiempo %in% venttiem)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC08_Ventajas_Tranquilo" %in% showvar){
    if(length(venttranq)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Tranquilo %in% venttranq)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC08_Ventajas_Seguro" %in% showvar){
    if(length(ventseg)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Seguro %in% ventseg)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC08_Ventajas_Otro" %in% showvar){
    if(length(ventotro)!=0){
      df_ide <- dplyr::filter(df_ide, IyC08_Ventajas_Otro %in% ventotro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC09_Emigrar" %in% showvar){
    if(length(emigrar)!=0){
      df_ide <- dplyr::filter(df_ide, IyC09_Emigrar %in% emigrar)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC10_Pertenencia" %in% showvar){
    if(length(perten)!=0){
      df_ide <- dplyr::filter(df_ide, IyC10_Pertenencia %in% perten)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC11_Frecuencia_Cabecera_Isla" %in% showvar){
    if(length(frecc)!=0){
      df_ide <- dplyr::filter(df_ide, IyC11_Frecuencia_Cabecera_Isla %in% frecc)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC12_Motivo_Viaja_Asuntos_Admin" %in% showvar){
    if(length(mvasun)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Asuntos_Admin %in% mvasun)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS" %in% showvar){
    if(length(mvfds)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS %in% mvfds)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC12_Motivo_Viaja_Trabajo" %in% showvar){
    if(length(mvtrab)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Trabajo %in% mvtrab)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC12_Motivio_Viaja_Recreacion" %in% showvar){
    if(length(mvrec)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivio_Viaja_Recreacion %in% mvrec)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("IyC12_Motivo_Viaja_Familia" %in% showvar){
    if(length(mvfam)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Familia %in% mvfam)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("IyC12_Motivo_Viaja_Otro" %in% showvar){
    if(length(mvotro)!=0){
      df_ide <- dplyr::filter(df_ide, IyC12_Motivo_Viaja_Otro %in% mvotro)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI01_La_vivienda_es" %in% showvar){
    if(length(vve)!=0){
      df_ide <- dplyr::filter(df_ide, VI01_La_vivienda_es %in% vve)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI04_Contado" %in% showvar){
    if(length(conta)!=0){
      df_ide <- dplyr::filter(df_ide, VI04_Contado %in% conta)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI04_Herencia" %in% showvar){
    if(length(heren)!=0){
      df_ide <- dplyr::filter(df_ide, VI04_Herencia %in% heren)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("VI04_Mensual" %in% showvar){
    if(length(mens)!=0){
      df_ide <- dplyr::filter(df_ide, VI04_Mensual %in% mens)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI05_CPropiaMensual" %in% showvar){
    if(length(cpmen)!=0){
      df_ide <- dplyr::filter(df_ide, VI05_CPropiaMensual %in% cpmen)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  #######################################################################################################################
  
  if("VI06_CPropiaAdquirida" %in% showvar){
    if(length(cpadq)!=0){
      df_ide <- dplyr::filter(df_ide, VI06_CPropiaAdquirida %in% cpadq)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("VI08NINUNDACIONES" %in% showvar){
    if(length(inun)!=0){
      df_ide <- dplyr::filter(df_ide, VI08NINUNDACIONES %in% inun)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI09PeHuracan" %in% showvar){
    if(length(hura)!=0){
      df_ide <- dplyr::filter(df_ide, VI09PeHuracan %in% hura)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("VI14SabeZo0Riesgo" %in% showvar){
    if(length(ssr)!=0){
      df_ide <- dplyr::filter(df_ide, VI14SabeZo0Riesgo %in% ssr)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("VI15SabeAfectaciones" %in% showvar){
    if(length(safec)!=0){
      df_ide <- dplyr::filter(df_ide, VI15SabeAfectaciones %in% safec)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE1_AreasVerdes" %in% showvar){
    if(length(aver)!=0){
      df_ide <- dplyr::filter(df_ide, AE1_AreasVerdes %in% aver)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE3_Banquetas" %in% showvar){
    if(length(abanq)!=0){
      df_ide <- dplyr::filter(df_ide, AE3_Banquetas %in% abanq)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE4_Lumi0rias" %in% showvar){
    if(length(alum)!=0){
      df_ide <- dplyr::filter(df_ide, AE4_Lumi0rias %in% alum)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  if("AE5_Transporte" %in% showvar){
    if(length(trans)!=0){
      df_ide <- dplyr::filter(df_ide, AE5_Transporte %in% trans)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE6_Patrullas" %in% showvar){
    if(length(pat)!=0){
      df_ide <- dplyr::filter(df_ide, AE6_Patrullas %in% pat)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE7_Lotes" %in% showvar){
    if(length(lot)!=0){
      df_ide <- dplyr::filter(df_ide, AE7_Lotes %in% lot)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE9_PeSeguridad" %in% showvar){
    if(length(pseg)!=0){
      df_ide <- dplyr::filter(df_ide, AE9_PeSeguridad %in% pseg)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE10_PeComodidad" %in% showvar){
    if(length(pcom)!=0){
      df_ide <- dplyr::filter(df_ide, AE10_PeComodidad %in% pcom)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  if("AE11_PeRiesgo" %in% showvar){
    if(length(pries)!=0){
      df_ide <- dplyr::filter(df_ide, AE11_PeRiesgo %in% pries)
      #updateSelectInput(getDefaultReactiveDomain(),"Info.Edad","Edad", choices = unique(df_ide[,"Info.Edad"]))
    }
  }
  
  # df_pl(showvar,df_ide,in_sex,in_edad,Col,Cas_a,Cas_N,Kin,prim,sec,bach,lic,autobu,colec,taxi,motax,moto,auto,v_col,abarro,super,conv,plaza,urg_c,urgpatc,urgpart,urgh,urgc,urgf,urgo,parque,unid,jardin,unidaddep,biblio,aotro,trbajan,trabajo,puesto,ingreso,escola,sining,tiempo,colon,muni,origen,tiempoi,mpar,mamig,mtrab,mnego,mopor,motro,acudvec,acudfam,acudaut,acudig,acudotro,relig,venc,ventr, ventfam,venttiem,venttranq,ventseg,ventotro,emigrar,perten,frecc,mvasun,mvfds,mvtrab,mvrec,mvfam,mvotro,vve,conta,heren,mens,cpmen,cpadq,inun,hura,ssr,safec,aver,abanq,alum,trans,pat,lot,pseg,pcom, pries)
  return(df_ide)
}





lect <- function(){
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
                                  levels = c("TODA","ENTRE.UN.AÑO.Y.CINCO","MAS.DE.5.ANOS","MENOS.DE.UN.AÑO"),
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
  
  return(df_ide)
  
}

pru2 <- reactive({
  
  df_ide <- lect2()
  
  df_ide <- df_ide[, input$show_vards, drop = FALSE]
  showvards <- input$show_vards
  Sexos <- input$Sexo
  Participa.Eventos.Deportivo <- input$Participa.Eventos.Deportivos
  Participa.En.Tanda <- input$Participa.En.Tandas
  word<- input$Participa.En.Fiestas
  Participa.En.Iglesia.Templ<- input$Participa.En.Iglesia.Templo
  Participa.Solucion.Problemas.Comunida<- input$Participa.Solucion.Problemas.Comunidad
  Conoce.Vecino<- input$Conoce.Vecinos
  Conoce.Vecinos.Confiaria.Nino<- input$Conoce.Vecinos.Confiaria.Ninos
  Conoce.Vecinos.Confiaria.Cas<- input$Conoce.Vecinos.Confiaria.Casa
  Conoce.Vecinos.Participa.Mejorar.Segurida<- input$Conoce.Vecinos.Participa.Mejorar.Seguridad
  Participa.Con.Autoridad.Mejorar.Segurida<- input$Participa.Con.Autoridad.Mejorar.Seguridad
  Cuando.Delito.Vecinos.Se.Reune<- input$Cuando.Delito.Vecinos.Se.Reunen
  Cuando.Delito.Vecinos.Organizan.Para.Vigila<- input$Cuando.Delito.Vecinos.Organizan.Para.Vigilar
  Cuando.Delito.Vecinos.Intercambian.Num.Te<- input$Cuando.Delito.Vecinos.Intercambian.Num.Tel
  Cuando.Delito.Vecinos.Forman.Cha<- input$Cuando.Delito.Vecinos.Forman.Chat
  Cuando.Delito.Vecinos.Ponen.Letreros.Advertenci<- input$Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia
  Cuando.Delito.Vecinos.Llaman.Polici<- input$Cuando.Delito.Vecinos.Llaman.Policia
  Cuando.Delito.Vecinos.Denuncian.Con.Autorida<- input$Cuando.Delito.Vecinos.Denuncian.Con.Autoridad
  Durante.Ultimo.Ano.Hubo.Robo.cas<- input$Durante.Ultimo.Ano.Hubo.Robo.casa
  Durante.Ultimo.Ano.Hubo.Robo.Call<- input$Durante.Ultimo.Ano.Hubo.Robo.Calle
  Durante.Ultimo.Ano.Hubo.Robo.Transport<- input$Durante.Ultimo.Ano.Hubo.Robo.Transporte
  Durante.Ultimo.Ano.Hubo.Robo.Negoci<- input$Durante.Ultimo.Ano.Hubo.Robo.Negocio
  Durante.Ultimo.Ano.Hubo.Robo.De.Vehicul<- input$Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo
  Durante.Ultimo.Ano.Hubo.Balacera<- input$Durante.Ultimo.Ano.Hubo.Balaceras
  Durante.Ultimo.Ano.Hubo.Violencia.Familia<- input$Durante.Ultimo.Ano.Hubo.Violencia.Familiar
  Riesgo.Sufrir.Delito.En.Cas<- input$Riesgo.Sufrir.Delito.En.Casa
  Riesgo.Sufrir.Delito.En.Call<- input$Riesgo.Sufrir.Delito.En.Calle
  Riesgo.Sufrir.Delito.En.Esta.Zon<- input$Riesgo.Sufrir.Delito.En.Esta.Zona
  Riesgo.Sufrir.Delito.En.Esta.Ciuda<- input$Riesgo.Sufrir.Delito.En.Esta.Ciudad
  Ha.Sido.Victima.Delito.Ultimo.An<- input$Ha.Sido.Victima.Delito.Ultimo.Ano
  Caso.Ser.Victima.Delito.Llama.Polici<- input$Caso.Ser.Victima.Delito.Llama.Policia
  Caso.Ser.Victima.Delito.Hace.Denunci<- input$Caso.Ser.Victima.Delito.Hace.Denuncia
  Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligr<- input$Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro
  Caso.Ser.Victima.Delito.Advierte.Familia.Peligr<- input$Caso.Ser.Victima.Delito.Advierte.Familia.Peligro
  Padres.Participan.Actividades.Con.Hijo<- input$Padres.Participan.Actividades.Con.Hijos
  Vecinos.Organizan.Prevenir.Delito<- input$Vecinos.Organizan.Prevenir.Delitos
  Hay.Personas.Amable<- input$Hay.Personas.Amables
  Hay.Personas.Que.Siempre.Ayudan.A.Otro<- input$Hay.Personas.Que.Siempre.Ayudan.A.Otros
  Hay.Personas.A.Las.Que.Todos.Tienen.Mied<- input$Hay.Personas.A.Las.Que.Todos.Tienen.Miedo
  Hay.Personas.Que.Se.Emborrachan.O.Droga<- input$Hay.Personas.Que.Se.Emborrachan.O.Drogan
  Hay.Personas.Que.Han.Estado.Carce<- input$Hay.Personas.Que.Han.Estado.Carcel
  Hay.Personas.Sospechosa<- input$Hay.Personas.Sospechosas
  Hay.Violencia.Entre.Hombre<- input$Hay.Violencia.Entre.Hombres
  Hay.Violencia.Entre.Familia<- input$Hay.Violencia.Entre.Familias
  Hay.Violencia.Entre.Jovene<- input$Hay.Violencia.Entre.Jovenes
  Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Person<- input$Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona
  si.Conflictos.Entre.Vecinos.Dialogand<- input$si.Conflictos.Entre.Vecinos.Dialogando
  si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosament<- input$si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente
  si.Conflictos.Entre.Vecinos.Se.Manejan.A.Grito	<- input$si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos
  si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpe<- input$si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes
  Jovenes.Hacen.Deport<- input$Jovenes.Hacen.Deporte
  Jovenes.Ayudan.Otro<- input$Jovenes.Ayudan.Otros
  Mayoria.De.Jovenes.Estudian.Trabaja<- input$Mayoria.De.Jovenes.Estudian.Trabajan
  Jovenes.Andan.Pandilla<- input$Jovenes.Andan.Pandillas
  Jovenes.Son.Violento<- input$Jovenes.Son.Violentos
  Hay.Parqu<- input$Hay.Parque
  Hay.Parque.En.Buen.Estad<- input$Hay.Parque.En.Buen.Estado
  Hay.Parque.Utilizado.Por.Nino<- input$Hay.Parque.Utilizado.Por.Ninos
  Hay.Parque.Utilizado.Por.Jovene<- input$Hay.Parque.Utilizado.Por.Jovenes
  Hay.Parque.Utilizado.Por.Pandilla<- input$Hay.Parque.Utilizado.Por.Pandillas
  Hay.Parque.Utilizado.Por.Familia<- input$Hay.Parque.Utilizado.Por.Familias
  Hay.Parque.Utilizado.Por.Personas.Tercera.Eda<- input$Hay.Parque.Utilizado.Por.Personas.Tercera.Edad
  Hay.Parque.Utilizado.Por.Adulto<- input$Hay.Parque.Utilizado.Por.Adultos
  Hay.Parque.Actividades.Supervisadas.Por.Adulto<- input$Hay.Parque.Actividades.Supervisadas.Por.Adultos
  Hay.Parque.Utilizado.Por.Vandalo<- input$Hay.Parque.Utilizado.Por.Vandalos
  Hay.Parque.Utilizado.Por.Personas.Otras.Zona<- input$Hay.Parque.Utilizado.Por.Personas.Otras.Zonas
  Hay.Parque.Utilizado.Por.Uste<- input$Hay.Parque.Utilizado.Por.Usted
  Hay.Banqueta<- input$Hay.Banquetas
  Hay.Bache<- input$Hay.Baches
  Hay.Letreros.De.Calle<- input$Hay.Letreros.De.Calles
  Hay.Tiendit<- input$Hay.Tiendita
  Hay.Alumbrad<- input$Hay.Alumbrado
  Hay.Consumo.Alcohol.En.Call<- input$Hay.Consumo.Alcohol.En.Calle
  Hay.Horarios.Transporte.Conveniente<- input$Hay.Horarios.Transporte.Convenientes
  Hay.Terrenos.Baldio<- input$Hay.Terrenos.Baldios
  Hay.Basur<- input$Hay.Basura
  Hay.Autos.Abandonado<- input$Hay.Autos.Abandonados
  Hay.Casas.Abandonada<- input$Hay.Casas.Abandonadas
  Hay.Vandalism<- input$Hay.Vandalismo
  Hay.Grafit<- input$Hay.Grafiti
  Hay.Venta.Alcohol.Cigarros.A.Menore<- input$Hay.Venta.Alcohol.Cigarros.A.Menores
  Hay.Venta.Drog<- input$Hay.Venta.Droga
  Hay.Venta.Alcohol.Despues.De.Once.Noch<- input$Hay.Venta.Alcohol.Despues.De.Once.Noche
  Algun.Menor.Abandono.Escuel<- input$Algun.Menor.Abandono.Escuela
  Algun.Menor.Tiene.Problemas.Conduct<- input$Algun.Menor.Tiene.Problemas.Conducta
  Algun.Menor.Quedo.Embarazad<- input$Algun.Menor.Quedo.Embarazada
  Corregir.Ninos.Recomienda.Castigarl<- input$Corregir.Ninos.Recomienda.Castigarle
  Corregir.Ninos.Recomienda.Gritarl<- input$Corregir.Ninos.Recomienda.Gritarle
  Corregir.Ninos.Recomienda.Darle.Nalgada<- input$Corregir.Ninos.Recomienda.Darle.Nalgadas
  Corregir.Ninos.Recomienda.Explicarle.Esta.Ma<- input$Corregir.Ninos.Recomienda.Explicarle.Esta.Mal
  Corregir.Ninos.Recomienda.Aconsejarl<- input$Corregir.Ninos.Recomienda.Aconsejarle
  Corregir.Ninos.Recomienda.Ensenar.Ejempl<- input$Corregir.Ninos.Recomienda.Ensenar.Ejemplo
  En.Casa.Platican.Unos.Con.Otro<- input$En.Casa.Platican.Unos.Con.Otros
  En.Casa.Comen.Junto<- input$En.Casa.Comen.Juntos
  En.Casa.Se.Ayudan.Gasto<- input$En.Casa.Se.Ayudan.Gastos
  En.Casa.Discute<- input$En.Casa.Discuten
  En.Casa.Se.Grita<- input$En.Casa.Se.Gritan
  En.Casa.Se.Ignora<- input$En.Casa.Se.Ignoran
  En.Casa.Alguien.Tiene.Discapacida<- input$En.Casa.Alguien.Tiene.Discapacidad
  En.Casa.Alguien.No.Habla.Espano<- input$En.Casa.Alguien.No.Habla.Espanol
  En.Casa.Alguien.Necesita.Ayuda.Obesida<- input$En.Casa.Alguien.Necesita.Ayuda.Obesidad
  En.Casa.Alguien.Necesita.Ayuda.Fuma<- input$En.Casa.Alguien.Necesita.Ayuda.Fumar
  En.Casa.Alguien.Necesita.Ayuda.Bebe<- input$En.Casa.Alguien.Necesita.Ayuda.Beber
  Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Cas<- input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa
  Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciuda<- input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad
  Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estad<- input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado
  Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noch<- input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche
  Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Camina<- input$Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar
  Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Call<- input$Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle
  Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Persona<- input$Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas
  Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigo<- input$Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos
  Ultimo.Ano.Por.Seguridad.Dejo.Usar.Tax<- input$Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi
  Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectiv<- input$Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo
  Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joya<- input$Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas
  Policia.Cuida.Vigila.Bie<- input$Policia.Cuida.Vigila.Bien
  Policia.Comete.Abuso<- input$Policia.Comete.Abusos
  Policia.Acude.Llamado<- input$Policia.Acude.Llamados
  Policia.Pide.Mordida<- input$Policia.Pide.Mordidas
  Policia.Hace.Rondine<- input$Policia.Hace.Rondines
  Policia.Comete.Delito<- input$Policia.Comete.Delitos
  Confianza.Que.Tiene.En.La.Polici<- input$Confianza.Que.Tiene.En.La.Policia
  Confianza.Que.Tiene.En.Ministerio.Públic<- input$Confianza.Que.Tiene.En.Ministerio.Publico
  Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zon<- input$Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona
  Confianza.Que.Tiene.En.Su.Presidente.Municipa<- input$Confianza.Que.Tiene.En.Su.Presidente.Municipal
  Confianza.Que.Tiene.En.El.Gobernado<- input$Confianza.Que.Tiene.En.El.Gobernador
  Calificacion.Trabajo.De.La.Polici<- input$Calificacion.Trabajo.De.La.Policia
  Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denuncia<- input$Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar
  Calificacion.Trabajo.De.Empleados.De.Gobiern<- input$Calificacion.Trabajo.De.Empleados.De.Gobierno
  Calificacion.Trabajo.Presidente.Municipa<- input$Calificacion.Trabajo.Presidente.Municipal
  Calificacion.Trabajo.Del.Gobernado<- input$Calificacion.Trabajo.Del.Gobernador
  Calificacion.Trato.Que.Da.La.Polici<- input$Calificacion.Trato.Que.Da.La.Policia
  Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denuncia<- input$Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar
  Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobiern<- input$Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno
  Calificacion.Trato.Recibe.De.Su.Presidente.Municipa<- input$Calificacion.Trato.Recibe.De.Su.Presidente.Municipal
  Calificacion.Trato.Recibe.Del.Gobernado<- input$Calificacion.Trato.Recibe.Del.Gobernador
  Num.Personas.Viven.Esta.Cas<- input$Num.Personas.Viven.Esta.Casa
  Cuantos.Hombre<- input$Cuantos.Hombres
  Cuantas.Mujere<- input$Cuantas.Mujeres
  Cuantos.Menore<- input$Cuantos.Menores
  La.Madre.De.Los.Menores.Vive.En.Cas<- input$La.Madre.De.Los.Menores.Vive.En.Casa
  El.Padre.De.Los.Menores.Vive.En.Cas<- input$El.Padre.De.Los.Menores.Vive.En.Casa
  Esta.Casa.Es.Propi<- input$Esta.Casa.Es.Propia
  Tiempo.Viviendo.En.Q.Ro<- input$Tiempo.Viviendo.En.Q.Roo
  Tiempo.Viviendo.En.Esta.Cas<- input$Tiempo.Viviendo.En.Esta.Casa
  
  
  
  df_ide <- Func_A2(df_ide, showvards, Sexos, Participa.Eventos.Deportivo, Participa.En.Tanda, word, Participa.En.Iglesia.Templ,Participa.Solucion.Problemas.Comunida,Conoce.Vecino,Conoce.Vecinos.Confiaria.Nino	,Conoce.Vecinos.Confiaria.Cas	,Conoce.Vecinos.Participa.Mejorar.Segurida,Participa.Con.Autoridad.Mejorar.Segurida	, Cuando.Delito.Vecinos.Se.Reune,Cuando.Delito.Vecinos.Organizan.Para.Vigila	,Cuando.Delito.Vecinos.Intercambian.Num.Te	,Cuando.Delito.Vecinos.Forman.Cha	,Cuando.Delito.Vecinos.Ponen.Letreros.Advertenci	,Cuando.Delito.Vecinos.Llaman.Polici	, Cuando.Delito.Vecinos.Denuncian.Con.Autorida	,Durante.Ultimo.Ano.Hubo.Robo.cas	,Durante.Ultimo.Ano.Hubo.Robo.Call,Durante.Ultimo.Ano.Hubo.Robo.Transport	,Durante.Ultimo.Ano.Hubo.Robo.Negoci,Durante.Ultimo.Ano.Hubo.Robo.De.Vehicul,Durante.Ultimo.Ano.Hubo.Balacera	,Durante.Ultimo.Ano.Hubo.Violencia.Familia	, Riesgo.Sufrir.Delito.En.Cas	,Riesgo.Sufrir.Delito.En.Call,Riesgo.Sufrir.Delito.En.Esta.Zon	, Riesgo.Sufrir.Delito.En.Esta.Ciuda	,Ha.Sido.Victima.Delito.Ultimo.An	,Caso.Ser.Victima.Delito.Llama.Polici	,Caso.Ser.Victima.Delito.Hace.Denunci	,Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligr, Caso.Ser.Victima.Delito.Advierte.Familia.Peligr	, Padres.Participan.Actividades.Con.Hijo, Vecinos.Organizan.Prevenir.Delito,Hay.Personas.Amable,Hay.Personas.Que.Siempre.Ayudan.A.Otro,Hay.Personas.A.Las.Que.Todos.Tienen.Mied,Hay.Personas.Que.Se.Emborrachan.O.Droga	, Hay.Personas.Que.Han.Estado.Carce,Hay.Personas.Sospechosa	,Hay.Violencia.Entre.Hombre	,Hay.Violencia.Entre.Familia,Hay.Violencia.Entre.Jovene	,Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Person,si.Conflictos.Entre.Vecinos.Dialogand,si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosament,si.Conflictos.Entre.Vecinos.Se.Manejan.A.Grito	, si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpe	,Jovenes.Hacen.Deport	,Jovenes.Ayudan.Otro, Mayoria.De.Jovenes.Estudian.Trabaja,Jovenes.Andan.Pandilla,Jovenes.Son.Violento	,Hay.Parqu,Hay.Parque.En.Buen.Estad	,Hay.Parque.Utilizado.Por.Nino,Hay.Parque.Utilizado.Por.Jovene,Hay.Parque.Utilizado.Por.Pandilla,Hay.Parque.Utilizado.Por.Familia,Hay.Parque.Utilizado.Por.Personas.Tercera.Eda,Hay.Parque.Utilizado.Por.Adulto,Hay.Parque.Actividades.Supervisadas.Por.Adulto	,Hay.Parque.Utilizado.Por.Vandalo	,Hay.Parque.Utilizado.Por.Personas.Otras.Zona	,Hay.Parque.Utilizado.Por.Uste	,Hay.Banqueta,Hay.Bache,
                    Hay.Letreros.De.Calle,Hay.Tiendit	,Hay.Alumbrad,Hay.Consumo.Alcohol.En.Call,Hay.Horarios.Transporte.Conveniente,	Hay.Terrenos.Baldio,Hay.Basur	,Hay.Autos.Abandonado	,Hay.Casas.Abandonada	,Hay.Vandalism,Hay.Grafit, Hay.Venta.Alcohol.Cigarros.A.Menore	,Hay.Venta.Drog	,
                    Hay.Venta.Alcohol.Despues.De.Once.Noch	,
                    Algun.Menor.Abandono.Escuel	,Algun.Menor.Tiene.Problemas.Conduct	,
                    Algun.Menor.Quedo.Embarazad,Corregir.Ninos.Recomienda.Castigarl,Corregir.Ninos.Recomienda.Gritarl	,Corregir.Ninos.Recomienda.Darle.Nalgada,
                    Corregir.Ninos.Recomienda.Explicarle.Esta.Ma	,
                    Corregir.Ninos.Recomienda.Aconsejarl	,Corregir.Ninos.Recomienda.Ensenar.Ejempl,En.Casa.Platican.Unos.Con.Otro	, En.Casa.Comen.Junto,En.Casa.Se.Ayudan.Gasto,En.Casa.Discute, En.Casa.Se.Grita	, En.Casa.Se.Ignora,En.Casa.Alguien.Tiene.Discapacida,En.Casa.Alguien.No.Habla.Espano	,En.Casa.Alguien.Necesita.Ayuda.Obesida	,En.Casa.Alguien.Necesita.Ayuda.Fuma	, En.Casa.Alguien.Necesita.Ayuda.Bebe	,
                    Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Cas	,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciuda	,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estad,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noch,Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Camina	,Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Call,Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Persona,Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigo	,Ultimo.Ano.Por.Seguridad.Dejo.Usar.Tax	, Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectiv	,Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joya	,Policia.Cuida.Vigila.Bie	,Policia.Comete.Abuso	, Policia.Acude.Llamado,Policia.Pide.Mordida,Policia.Hace.Rondine	,Policia.Comete.Delito	,Confianza.Que.Tiene.En.La.Polici,Confianza.Que.Tiene.En.Ministerio.Públic,Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zon	, Confianza.Que.Tiene.En.Su.Presidente.Municipa,Confianza.Que.Tiene.En.El.Gobernado,Calificacion.Trabajo.De.La.Polici	, Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denuncia	, Calificacion.Trabajo.De.Empleados.De.Gobiern, Calificacion.Trabajo.Presidente.Municipa	, Calificacion.Trabajo.Del.Gobernado	, Calificacion.Trato.Que.Da.La.Policia	,Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denuncia	, Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobiern,Calificacion.Trato.Recibe.De.Su.Presidente.Municipa	,Calificacion.Trato.Recibe.Del.Gobernado,Num.Personas.Viven.Esta.Cas,Cuantos.Hombre	,Cuantas.Mujere	,Cuantos.Menore,La.Madre.De.Los.Menores.Vive.En.Cas,El.Padre.De.Los.Menores.Vive.En.Cas	,Esta.Casa.Es.Propi	, Tiempo.Viviendo.En.Q.Ro,Tiempo.Viviendo.En.Esta.Cas
  )
  
  return(df_ide)
})


lect2 <- function(){
  df_ide <- read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
  
  df_ide$Sexo <- factor(df_ide$Sexo, 
                        levels = c("M", "H"),
                        ordered = TRUE)
  
  df_ide$Participa.Eventos.Deportivos <- factor(df_ide$Participa.Eventos.Deportivos,
                                                levels = c(1, 0),
                                                ordered = TRUE)
  
  df_ide$Participa.En.Tandas <- factor(df_ide$Participa.En.Tandas,
                                       levels = c(1, 0),
                                       ordered = TRUE)
  
  
  df_ide$Participa.En.Fiestas <- factor(df_ide$Participa.En.Fiestas,
                                        levels = c(1, 0),
                                        ordered = TRUE)
  
  df_ide$Participa.En.Iglesia.Templo <- factor(df_ide$Participa.En.Iglesia.Templo,
                                               levels = c(1, 0),
                                               ordered = TRUE)
  
  df_ide$Participa.Solucion.Problemas.Comunidad <- factor(df_ide$Participa.Solucion.Problemas.Comunidad,
                                                          levels = c(1, 0),
                                                          ordered = TRUE)
  
  df_ide$Conoce.Vecinos <- factor(df_ide$Conoce.Vecinos, 
                                  levels = c(1, 0),
                                  ordered = TRUE)
  
  df_ide$Conoce.Vecinos.Confiaria.Ninos <- factor(df_ide$Conoce.Vecinos.Confiaria.Ninos,
                                                  levels = c(1, 0, "-1"),
                                                  ordered = TRUE)
  
  df_ide$Conoce.Vecinos.Confiaria.Casa <- factor(df_ide$Conoce.Vecinos.Confiaria.Casa,
                                                 levels = c(1, 0, "-1"),
                                                 ordered = TRUE)
  
  df_ide$Conoce.Vecinos.Participa.Mejorar.Seguridad <- factor(df_ide$Conoce.Vecinos.Participa.Mejorar.Seguridad,
                                                              levels = c(1, 0, "-1"),
                                                              ordered = TRUE)
  
  df_ide$Participa.Con.Autoridad.Mejorar.Seguridad <- factor(df_ide$Participa.Con.Autoridad.Mejorar.Seguridad,
                                                             levels = c(1, 0),
                                                             ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Se.Reunen <- factor(df_ide$Cuando.Delito.Vecinos.Se.Reunen,
                                                   levels = c(1, 0),
                                                   ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Organizan.Para.Vigilar <- factor(df_ide$Cuando.Delito.Vecinos.Organizan.Para.Vigilar,
                                                                levels = c(1, 0),
                                                                ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Intercambian.Num.Tel <- factor(df_ide$Cuando.Delito.Vecinos.Intercambian.Num.Tel,
                                                              levels = c(1, 0),
                                                              ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Forman.Chat <- factor(df_ide$Cuando.Delito.Vecinos.Forman.Chat,
                                                     levels = c(1, 0),
                                                     ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia <- factor(df_ide$Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia,
                                                                    levels = c(1, 0),
                                                                    ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Llaman.Policia <- factor(df_ide$Cuando.Delito.Vecinos.Llaman.Policia,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Cuando.Delito.Vecinos.Denuncian.Con.Autoridad <- factor(df_ide$Cuando.Delito.Vecinos.Denuncian.Con.Autoridad,
                                                                 levels = c(1, 0),
                                                                 ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Robo.casa <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Robo.casa,
                                                     levels = c(1, 0),
                                                     ordered = TRUE)
  
  
  df_ide$Durante.Ultimo.Ano.Hubo.Robo.Calle <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Robo.Calle,
                                                      levels = c(1, 0),
                                                      ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Robo.Transporte <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Robo.Transporte,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Robo.Negocio <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Robo.Negocio,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo,
                                                            levels = c(1, 0),
                                                            ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Balaceras <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Balaceras,
                                                     levels = c(1, 0),
                                                     ordered = TRUE)
  
  df_ide$Durante.Ultimo.Ano.Hubo.Violencia.Familiar <- factor(df_ide$Durante.Ultimo.Ano.Hubo.Violencia.Familiar,
                                                              levels = c(1, 0),
                                                              ordered = TRUE)
  
  df_ide$Riesgo.Sufrir.Delito.En.Casa <- factor(df_ide$Riesgo.Sufrir.Delito.En.Casa,
                                                levels = c(1, 2,3,4,5),
                                                ordered = TRUE)
  
  df_ide$Riesgo.Sufrir.Delito.En.Calle <- factor(df_ide$Riesgo.Sufrir.Delito.En.Calle,
                                                 levels = c(1, 2,3,4,5),
                                                 ordered = TRUE)
  
  df_ide$Riesgo.Sufrir.Delito.En.Esta.Zona <- factor(df_ide$Riesgo.Sufrir.Delito.En.Esta.Zona,
                                                     levels = c(1, 2,3,4,5),
                                                     ordered = TRUE)
  
  df_ide$Riesgo.Sufrir.Delito.En.Esta.Ciudad <- factor(df_ide$Riesgo.Sufrir.Delito.En.Esta.Ciudad,
                                                       levels = c(1, 2,3,4,5),
                                                       ordered = TRUE)
  
  
  df_ide$Ha.Sido.Victima.Delito.Ultimo.Ano <- factor(df_ide$Ha.Sido.Victima.Delito.Ultimo.Ano,
                                                     levels = c(1, 0),
                                                     ordered = TRUE)
  
  df_ide$Caso.Ser.Victima.Delito.Llama.Policia <- factor(df_ide$Caso.Ser.Victima.Delito.Llama.Policia,
                                                         levels = c(1, 0),
                                                         ordered = TRUE)
  
  df_ide$Caso.Ser.Victima.Delito.Hace.Denuncia <- factor(df_ide$Caso.Ser.Victima.Delito.Hace.Denuncia,
                                                         levels = c(1, 0),
                                                         ordered = TRUE)
  
  df_ide$Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro <- factor(df_ide$Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro,
                                                                    levels = c(1, 0),
                                                                    ordered = TRUE)
  
  df_ide$Caso.Ser.Victima.Delito.Advierte.Familia.Peligro <- factor(df_ide$Caso.Ser.Victima.Delito.Advierte.Familia.Peligro,
                                                                    levels = c(1, 0),
                                                                    ordered = TRUE)
  
  df_ide$Padres.Participan.Actividades.Con.Hijos <- factor(df_ide$Padres.Participan.Actividades.Con.Hijos,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$Vecinos.Organizan.Prevenir.Delitos <- factor(df_ide$Vecinos.Organizan.Prevenir.Delitos,
                                                      levels = c(1, 0),
                                                      ordered = TRUE)
  
  df_ide$Hay.Personas.Amables <- factor(df_ide$Hay.Personas.Amables,
                                        levels = c(1, 0),
                                        ordered = TRUE)
  
  df_ide$Hay.Personas.Que.Siempre.Ayudan.A.Otros <- factor(df_ide$Hay.Personas.Que.Siempre.Ayudan.A.Otros,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$Hay.Personas.A.Las.Que.Todos.Tienen.Miedo <- factor(df_ide$Hay.Personas.A.Las.Que.Todos.Tienen.Miedo,
                                                             levels = c(1, 0),
                                                             ordered = TRUE)
  
  df_ide$Hay.Personas.Que.Se.Emborrachan.O.Drogan <- factor(df_ide$Hay.Personas.Que.Se.Emborrachan.O.Drogan,
                                                            levels = c(1, 0),
                                                            ordered = TRUE)
  
  df_ide$Hay.Personas.Que.Han.Estado.Carcel <- factor(df_ide$Hay.Personas.Que.Han.Estado.Carcel,
                                                      levels = c(1, 0),
                                                      ordered = TRUE)
  
  df_ide$Hay.Personas.Sospechosas <- factor(df_ide$Hay.Personas.Sospechosas,
                                            levels = c(1, 0),
                                            ordered = TRUE)
  
  df_ide$Hay.Violencia.Entre.Hombres <- factor(df_ide$Hay.Violencia.Entre.Hombres,
                                               levels = c(1, 0),
                                               ordered = TRUE)
  
  df_ide$Hay.Violencia.Entre.Familias <- factor(df_ide$Hay.Violencia.Entre.Familias,
                                                levels = c(1, 0),
                                                ordered = TRUE)
  
  df_ide$Hay.Violencia.Entre.Jovenes <- factor(df_ide$Hay.Violencia.Entre.Jovenes,
                                               levels = c(1, 0),
                                               ordered = TRUE)
  
  df_ide$Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona <- factor(df_ide$Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona,
                                                                            levels = c(1, 0),
                                                                            ordered = TRUE)
  
  df_ide$si.Conflictos.Entre.Vecinos.Dialogando <- factor(df_ide$si.Conflictos.Entre.Vecinos.Dialogando,
                                                          levels = c(1, 0),
                                                          ordered = TRUE)
  
  df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente <- factor(df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente,
                                                                          levels = c(1, 0),
                                                                          ordered = TRUE)
  
  df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos <- factor(df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos,
                                                                   levels = c(1, 0),
                                                                   ordered = TRUE)
  
  df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes <- factor(df_ide$si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes,
                                                                     levels = c(1, 0),
                                                                     ordered = TRUE)
  
  df_ide$Jovenes.Hacen.Deporte <- factor(df_ide$Jovenes.Hacen.Deporte,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Jovenes.Ayudan.Otros <- factor(df_ide$Jovenes.Ayudan.Otros,
                                        levels = c(1, 0),
                                        ordered = TRUE)
  
  
  df_ide$Mayoria.De.Jovenes.Estudian.Trabajan <- factor(df_ide$Mayoria.De.Jovenes.Estudian.Trabajan,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Jovenes.Andan.Pandillas <- factor(df_ide$Jovenes.Andan.Pandillas,
                                           levels = c(1, 0),
                                           ordered = TRUE)
  
  df_ide$Jovenes.Son.Violentos <- factor(df_ide$Jovenes.Son.Violentos,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Hay.Parque <- factor(df_ide$Hay.Parque, levels = c(1, 0), ordered = TRUE)
  
  df_ide$Hay.Parque.En.Buen.Estado <- factor(df_ide$Hay.Parque.En.Buen.Estado,
                                             levels = c(1, 0, "-1"),
                                             ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Ninos <- factor(df_ide$Hay.Parque.Utilizado.Por.Ninos,
                                                  levels = c(1, 0, "-1"),
                                                  ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Jovenes <- factor(df_ide$Hay.Parque.Utilizado.Por.Jovenes,
                                                    levels = c(1, 0, "-1"),
                                                    ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Pandillas <- factor(df_ide$Hay.Parque.Utilizado.Por.Pandillas,
                                                      levels =c(1, 0, "-1"),
                                                      ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Familias <- factor(df_ide$Hay.Parque.Utilizado.Por.Familias,
                                                     levels = c(1, 0, "-1"),
                                                     ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Personas.Tercera.Edad <- factor(df_ide$Hay.Parque.Utilizado.Por.Personas.Tercera.Edad,
                                                                  levels = c(1, 0, "-1"),
                                                                  ordered = TRUE)
  
  
  df_ide$Hay.Parque.Utilizado.Por.Adultos <- factor(df_ide$Hay.Parque.Utilizado.Por.Adultos,
                                                    levels = c(1, 0, "-1"),
                                                    ordered = TRUE)
  
  df_ide$Hay.Parque.Actividades.Supervisadas.Por.Adultos <- factor(df_ide$Hay.Parque.Actividades.Supervisadas.Por.Adultos,
                                                                   levels = c(1, 0, "-1"),
                                                                   ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Vandalos <- factor(df_ide$Hay.Parque.Utilizado.Por.Vandalos,
                                                     levels = c(1, 0, "-1"),
                                                     ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Personas.Otras.Zonas <- factor(df_ide$Hay.Parque.Utilizado.Por.Personas.Otras.Zonas,
                                                                 levels = c(1, 0, "-1"),
                                                                 ordered = TRUE)
  
  df_ide$Hay.Parque.Utilizado.Por.Usted <- factor(df_ide$Hay.Parque.Utilizado.Por.Usted,
                                                  levels = c(1, 0, "-1"),
                                                  ordered = TRUE)
  
  df_ide$Hay.Banquetas <- factor(df_ide$Hay.Banquetas,
                                 levels = c(1, 0),
                                 ordered = TRUE)
  
  df_ide$Hay.Baches <- factor(df_ide$Hay.Baches, levels = c(1, 0),
                              ordered = TRUE)
  
  
  df_ide$Hay.Letreros.De.Calles <- factor(df_ide$Hay.Letreros.De.Calles,
                                          levels = c(1, 0),
                                          ordered = TRUE)
  
  df_ide$Hay.Tiendita <- factor(df_ide$Hay.Tiendita,
                                levels = c(1, 0),
                                ordered = TRUE)
  
  
  df_ide$Hay.Alumbrado <- factor(df_ide$Hay.Alumbrado,
                                 levels = c(1, 0),
                                 ordered = TRUE)
  
  df_ide$Hay.Consumo.Alcohol.En.Calle <- factor(df_ide$Hay.Consumo.Alcohol.En.Calle,
                                                levels = c(1, 0),
                                                ordered = TRUE)
  
  df_ide$Hay.Horarios.Transporte.Convenientes <- factor(df_ide$Hay.Horarios.Transporte.Convenientes,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Hay.Terrenos.Baldios <- factor(df_ide$Hay.Terrenos.Baldios,
                                        levels = c(1, 0),
                                        ordered = TRUE)
  
  df_ide$Hay.Basura <- factor(df_ide$Hay.Basura,
                              levels = c(1, 0),
                              ordered = TRUE)
  
  df_ide$Hay.Autos.Abandonados <- factor(df_ide$Hay.Autos.Abandonados,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Hay.Casas.Abandonadas <- factor(df_ide$Hay.Casas.Abandonadas,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Hay.Vandalismo <- factor(df_ide$Hay.Vandalismo,
                                  levels = c(1, 0),
                                  ordered = TRUE)
  
  df_ide$Hay.Grafiti <- factor(df_ide$Hay.Grafiti,
                               levels = c(1, 0),
                               ordered = TRUE)
  
  df_ide$Hay.Venta.Alcohol.Cigarros.A.Menores <- factor(df_ide$Hay.Venta.Alcohol.Cigarros.A.Menores,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Hay.Venta.Droga <- factor(df_ide$Hay.Venta.Droga,
                                   levels = c(1, 0),
                                   ordered = TRUE)
  
  df_ide$Hay.Venta.Alcohol.Despues.De.Once.Noche <- factor(df_ide$Hay.Venta.Alcohol.Despues.De.Once.Noche,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$Algun.Menor.Abandono.Escuela <- factor(df_ide$Algun.Menor.Abandono.Escuela,
                                                levels = c(1, 0),
                                                ordered = TRUE)
  
  df_ide$Algun.Menor.Tiene.Problemas.Conducta <- factor(df_ide$Algun.Menor.Tiene.Problemas.Conducta,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$Algun.Menor.Quedo.Embarazada <- factor(df_ide$Algun.Menor.Quedo.Embarazada,
                                                levels = c(1, 0),
                                                ordered = TRUE)
  
  df_ide$Corregir.Ninos.Recomienda.Castigarle <- factor(df_ide$Corregir.Ninos.Recomienda.Castigarle,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  
  df_ide$Corregir.Ninos.Recomienda.Gritarle <- factor(df_ide$Corregir.Ninos.Recomienda.Gritarle,
                                                      levels = c(1, 0),
                                                      ordered = TRUE)
  
  df_ide$Corregir.Ninos.Recomienda.Darle.Nalgadas <- factor(df_ide$Corregir.Ninos.Recomienda.Darle.Nalgadas,
                                                            levels = c(1, 0),
                                                            ordered = TRUE)
  
  df_ide$Corregir.Ninos.Recomienda.Explicarle.Esta.Mal <- factor(df_ide$Corregir.Ninos.Recomienda.Explicarle.Esta.Mal,
                                                                 levels = c(1, 0),
                                                                 ordered = TRUE)
  
  df_ide$Corregir.Ninos.Recomienda.Aconsejarle <- factor(df_ide$Corregir.Ninos.Recomienda.Aconsejarle,
                                                         levels = c(1, 0),
                                                         ordered = TRUE)
  
  df_ide$Corregir.Ninos.Recomienda.Ensenar.Ejemplo <- factor(df_ide$Corregir.Ninos.Recomienda.Ensenar.Ejemplo,
                                                             levels = c(1, 0),
                                                             ordered = TRUE)
  
  df_ide$En.Casa.Todos.Se.Conocen <- factor(df_ide$En.Casa.Todos.Se.Conocen,
                                            levels = c(1, 0),
                                            ordered = TRUE)
  
  df_ide$En.Casa.Platican.Unos.Con.Otros <- factor(df_ide$En.Casa.Platican.Unos.Con.Otros,
                                                   levels = c(1, 0),
                                                   ordered = TRUE)
  
  df_ide$En.Casa.Comen.Juntos <- factor(df_ide$En.Casa.Comen.Juntos,
                                        levels = c(1, 0),
                                        ordered = TRUE)
  
  df_ide$En.Casa.Se.Ayudan.Gastos <- factor(df_ide$En.Casa.Se.Ayudan.Gastos,
                                            levels = c(1, 0),
                                            ordered = TRUE)
  
  df_ide$En.Casa.Discuten <- factor(df_ide$En.Casa.Discuten,
                                    levels = c(1, 0),
                                    ordered = TRUE)
  
  
  df_ide$En.Casa.Se.Gritan <- factor(df_ide$En.Casa.Se.Gritan,
                                     levels = c(1, 0),
                                     ordered = TRUE)
  
  df_ide$En.Casa.Se.Ignoran <- factor(df_ide$En.Casa.Se.Ignoran,
                                      levels = c(1, 0),
                                      ordered = TRUE)
  
  df_ide$En.Casa.Alguien.Tiene.Discapacidad <- factor(df_ide$En.Casa.Alguien.Tiene.Discapacidad,
                                                      levels = c(1, 0),
                                                      ordered = TRUE)
  
  df_ide$En.Casa.Alguien.No.Habla.Espanol <- factor(df_ide$En.Casa.Alguien.No.Habla.Espanol,
                                                    levels = c(1, 0),
                                                    ordered = TRUE)
  
  df_ide$En.Casa.Alguien.Necesita.Ayuda.Obesidad <- factor(df_ide$En.Casa.Alguien.Necesita.Ayuda.Obesidad,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$En.Casa.Alguien.Necesita.Ayuda.Fumar <- factor(df_ide$En.Casa.Alguien.Necesita.Ayuda.Fumar,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  df_ide$En.Casa.Alguien.Necesita.Ayuda.Beber <- factor(df_ide$En.Casa.Alguien.Necesita.Ayuda.Beber,
                                                        levels = c(1, 0),
                                                        ordered = TRUE)
  
  
  df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa,
                                                                      levels = c(1, 0),
                                                                      ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad,
                                                                        levels = c(1, 0),
                                                                        ordered = TRUE)
  
  
  df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado,
                                                                        levels = c(1, 0),
                                                                        ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche,
                                                                        levels = c(1, 0),
                                                                        ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar,
                                                                  levels = c(1, 0),
                                                                  ordered = TRUE)
  
  
  df_ide$Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle,
                                                                         levels = c(1, 0, "-1"),
                                                                         ordered = TRUE)
  
  df_ide$Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas <- factor(df_ide$Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas,
                                                                            levels = c(1, 0),
                                                                            ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos,
                                                                             levels = c(1, 0),
                                                                             ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi,
                                                           levels = c(1, 0),
                                                           ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo,
                                                                          levels = c(1, 0),
                                                                          ordered = TRUE)
  
  df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas <- factor(df_ide$Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas,
                                                               levels = c(1, 0),
                                                               ordered = TRUE)
  
  df_ide$Policia.Cuida.Vigila.Bien <- factor(df_ide$Policia.Cuida.Vigila.Bien,
                                             levels = c(1, 0),
                                             ordered = TRUE)
  
  df_ide$Policia.Comete.Abusos <- factor(df_ide$Policia.Comete.Abusos,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Policia.Acude.Llamados <- factor(df_ide$Policia.Acude.Llamados,
                                          levels = c(1, 0),
                                          ordered = TRUE)
  
  df_ide$Policia.Pide.Mordidas <- factor(df_ide$Policia.Pide.Mordidas,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Policia.Hace.Rondines <- factor(df_ide$Policia.Hace.Rondines,
                                         levels = c(1, 0),
                                         ordered = TRUE)
  
  df_ide$Policia.Comete.Delitos <- factor(df_ide$Policia.Comete.Delitos,
                                          levels = c(1, 0),
                                          ordered = TRUE)
  
  df_ide$Confianza.Que.Tiene.En.La.Policia <- factor(df_ide$Confianza.Que.Tiene.En.La.Policia,
                                                     levels = c(1, 2,3,4,5),
                                                     ordered = TRUE)
  
  df_ide$Confianza.Que.Tiene.En.Ministerio.Publico <- factor(df_ide$Confianza.Que.Tiene.En.Ministerio.Publico,
                                                             levels = c(1, 2,3,4,5),
                                                             ordered = TRUE)
  
  
  df_ide$Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona <- factor(df_ide$Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona,
                                                                           levels = c("-1",1, 2,3,4,5),
                                                                           ordered = TRUE)
  
  df_ide$Confianza.Que.Tiene.En.Su.Presidente.Municipal <- factor(df_ide$Confianza.Que.Tiene.En.Su.Presidente.Municipal,
                                                                  levels = c(1, 2,3,4,5),
                                                                  ordered = TRUE)
  
  df_ide$Confianza.Que.Tiene.En.El.Gobernador <- factor(df_ide$Confianza.Que.Tiene.En.El.Gobernador,
                                                        levels = c(1, 2,3,4,5),
                                                        ordered = TRUE)
  
  df_ide$Calificacion.Trabajo.De.La.Policia <- factor(df_ide$Calificacion.Trabajo.De.La.Policia,
                                                      levels = c(1,2,3,4,5),
                                                      ordered = TRUE)
  
  df_ide$Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar <- factor(df_ide$Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar,
                                                                              levels = c(1, 2,3,4,5),
                                                                              ordered = TRUE)
  
  df_ide$Calificacion.Trabajo.De.Empleados.De.Gobierno <- factor(df_ide$Calificacion.Trabajo.De.Empleados.De.Gobierno,
                                                                 levels = c(1, 2,3,4,5),
                                                                 ordered = TRUE)
  
  df_ide$Calificacion.Trabajo.Presidente.Municipal <- factor(df_ide$Calificacion.Trabajo.Presidente.Municipal,
                                                             levels = c(1, 2,3,4,5),
                                                             ordered = TRUE)
  
  df_ide$Calificacion.Trabajo.Del.Gobernador <- factor(df_ide$Calificacion.Trabajo.Del.Gobernador,
                                                       levels = c(1, 2,3,4,5),
                                                       ordered = TRUE)
  
  df_ide$Calificacion.Trato.Que.Da.La.Policia <- factor(df_ide$Calificacion.Trato.Que.Da.La.Policia,
                                                        levels = c(1, 2,3,4,5),
                                                        ordered = TRUE)
  
  df_ide$Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar <- factor(df_ide$Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar,
                                                                               levels = c(1, 2,3,4,5),
                                                                               ordered = TRUE)
  
  
  df_ide$Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno <- factor(df_ide$Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno,
                                                                          levels = c(1, 2,3,4,5),
                                                                          ordered = TRUE)
  
  df_ide$Calificacion.Trato.Recibe.De.Su.Presidente.Municipal <- factor(df_ide$Calificacion.Trato.Recibe.De.Su.Presidente.Municipal,
                                                                        levels = c("-1",1, 2,3,4,5),
                                                                        ordered = TRUE)
  
  df_ide$Calificacion.Trato.Recibe.Del.Gobernador <- factor(df_ide$Calificacion.Trato.Recibe.Del.Gobernador,
                                                            levels = c("-1",1, 2,3,4,5),
                                                            ordered = TRUE)
  
  df_ide$Num.Personas.Viven.Esta.Casa <- factor(df_ide$Num.Personas.Viven.Esta.Casa,
                                                levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,20),
                                                ordered = TRUE)
  
  df_ide$Cuantos.Hombres <- factor(df_ide$Cuantos.Hombres,
                                   levels = c(0, 1, 2,3,4,5,6,7,9,10),
                                   ordered = TRUE)
  
  df_ide$Cuantas.Mujeres <- factor(df_ide$Cuantas.Mujeres,
                                   levels = c(1, 0 ,2,3,4,5,6,7,8,10,11,15),
                                   ordered = TRUE)
  
  df_ide$Cuantos.Menores <- factor(df_ide$Cuantos.Menores,
                                   levels = c(1, 0,2,3,4,5,7),
                                   ordered = TRUE)
  
  df_ide$La.Madre.De.Los.Menores.Vive.En.Casa <- factor(df_ide$La.Madre.De.Los.Menores.Vive.En.Casa,
                                                        levels = c(1,0,"-1"),
                                                        ordered = TRUE)
  
  df_ide$El.Padre.De.Los.Menores.Vive.En.Casa <- factor(df_ide$El.Padre.De.Los.Menores.Vive.En.Casa,
                                                        levels = c(1,0,"-1"),
                                                        ordered = TRUE)
  
  df_ide$Esta.Casa.Es.Propia <- factor(df_ide$Esta.Casa.Es.Propia,
                                       levels = c(0, 1 ),
                                       ordered = TRUE)
  
  df_ide$Tiempo.Viviendo.En.Q.Roo <- factor(df_ide$Tiempo.Viviendo.En.Q.Roo,
                                            levels = c("MÁS DE 10 AÑOS", "ENTRE 1 Y 5 AÑOS", "ENTRE 6 Y 10 AÑOS", "MENOS DE UN AÑO" ),
                                            ordered = TRUE)
  
  df_ide$Tiempo.Viviendo.En.Esta.Casa <- factor(df_ide$Tiempo.Viviendo.En.Esta.Casa,
                                                levels = c("MÁS DE 10 AÑOS", "ENTRE 1 Y 5 AÑOS", "ENTRE 6 Y 10 AÑOS", "MENOS DE UN AÑO" ),
                                                ordered = TRUE)
  
  
  return(df_ide)
}

Func_A2 <- function(df_ide, showvards, Sexos, Participa.Eventos.Deportivo, Participa.En.Tanda, word, Participa.En.Iglesia.Templ,Participa.Solucion.Problemas.Comunida,Conoce.Vecino,Conoce.Vecinos.Confiaria.Nino	,Conoce.Vecinos.Confiaria.Cas	,Conoce.Vecinos.Participa.Mejorar.Segurida,Participa.Con.Autoridad.Mejorar.Segurida	, Cuando.Delito.Vecinos.Se.Reune,Cuando.Delito.Vecinos.Organizan.Para.Vigila	,Cuando.Delito.Vecinos.Intercambian.Num.Te	,Cuando.Delito.Vecinos.Forman.Cha	,Cuando.Delito.Vecinos.Ponen.Letreros.Advertenci	,Cuando.Delito.Vecinos.Llaman.Polici	, Cuando.Delito.Vecinos.Denuncian.Con.Autorida	,Durante.Ultimo.Ano.Hubo.Robo.cas	,Durante.Ultimo.Ano.Hubo.Robo.Call,Durante.Ultimo.Ano.Hubo.Robo.Transport	,Durante.Ultimo.Ano.Hubo.Robo.Negoci,Durante.Ultimo.Ano.Hubo.Robo.De.Vehicul,Durante.Ultimo.Ano.Hubo.Balacera	,Durante.Ultimo.Ano.Hubo.Violencia.Familia	, Riesgo.Sufrir.Delito.En.Cas	,Riesgo.Sufrir.Delito.En.Call,Riesgo.Sufrir.Delito.En.Esta.Zon	, Riesgo.Sufrir.Delito.En.Esta.Ciuda	,Ha.Sido.Victima.Delito.Ultimo.An	,Caso.Ser.Victima.Delito.Llama.Polici	,Caso.Ser.Victima.Delito.Hace.Denunci	,Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligr, Caso.Ser.Victima.Delito.Advierte.Familia.Peligr	, Padres.Participan.Actividades.Con.Hijo,
                    Vecinos.Organizan.Prevenir.Delito,Hay.Personas.Amable,Hay.Personas.Que.Siempre.Ayudan.A.Otro,Hay.Personas.A.Las.Que.Todos.Tienen.Mied,Hay.Personas.Que.Se.Emborrachan.O.Droga	, Hay.Personas.Que.Han.Estado.Carce,Hay.Personas.Sospechosa	,Hay.Violencia.Entre.Hombre	,Hay.Violencia.Entre.Familia,Hay.Violencia.Entre.Jovene	,Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Person,si.Conflictos.Entre.Vecinos.Dialogand,si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosament,si.Conflictos.Entre.Vecinos.Se.Manejan.A.Grito	, si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpe	,Jovenes.Hacen.Deport	,Jovenes.Ayudan.Otro, Mayoria.De.Jovenes.Estudian.Trabaja,Jovenes.Andan.Pandilla,Jovenes.Son.Violento	,Hay.Parqu,Hay.Parque.En.Buen.Estad	,Hay.Parque.Utilizado.Por.Nino,Hay.Parque.Utilizado.Por.Jovene,Hay.Parque.Utilizado.Por.Pandilla,Hay.Parque.Utilizado.Por.Familia,Hay.Parque.Utilizado.Por.Personas.Tercera.Eda,Hay.Parque.Utilizado.Por.Adulto,Hay.Parque.Actividades.Supervisadas.Por.Adulto	,Hay.Parque.Utilizado.Por.Vandalo	,Hay.Parque.Utilizado.Por.Personas.Otras.Zona	,Hay.Parque.Utilizado.Por.Uste	,Hay.Banqueta,Hay.Bache, 
                    Hay.Letreros.De.Calle,Hay.Tiendit	,Hay.Alumbrad,Hay.Consumo.Alcohol.En.Call,Hay.Horarios.Transporte.Conveniente	,Hay.Terrenos.Baldio,Hay.Basur	,Hay.Autos.Abandonado	,Hay.Casas.Abandonada	,Hay.Vandalism,Hay.Grafit, Hay.Venta.Alcohol.Cigarros.A.Menore	,Hay.Venta.Drog	
                    ,Hay.Venta.Alcohol.Despues.De.Once.Noch	,
                    Algun.Menor.Abandono.Escuel	,Algun.Menor.Tiene.Problemas.Conduct, Algun.Menor.Quedo.Embarazad,Corregir.Ninos.Recomienda.Castigarl,Corregir.Ninos.Recomienda.Gritarl	,Corregir.Ninos.Recomienda.Darle.Nalgada,
                    Corregir.Ninos.Recomienda.Explicarle.Esta.Ma	,
                    Corregir.Ninos.Recomienda.Aconsejarl	,Corregir.Ninos.Recomienda.Ensenar.Ejempl,En.Casa.Platican.Unos.Con.Otro	, En.Casa.Comen.Junto,En.Casa.Se.Ayudan.Gasto,En.Casa.Discute, En.Casa.Se.Grita	, En.Casa.Se.Ignora,En.Casa.Alguien.Tiene.Discapacida,En.Casa.Alguien.No.Habla.Espano	,En.Casa.Alguien.Necesita.Ayuda.Obesida	,En.Casa.Alguien.Necesita.Ayuda.Fuma	, En.Casa.Alguien.Necesita.Ayuda.Bebe	,
                    Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Cas	,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciuda	,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estad,Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noch,Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Camina	,Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Call,Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Persona,Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigo	,Ultimo.Ano.Por.Seguridad.Dejo.Usar.Tax	, Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectiv	,Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joya	,Policia.Cuida.Vigila.Bie	,Policia.Comete.Abuso	, Policia.Acude.Llamado,Policia.Pide.Mordida,Policia.Hace.Rondine	,Policia.Comete.Delito	,Confianza.Que.Tiene.En.La.Polici,Confianza.Que.Tiene.En.Ministerio.Públic,Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zon	, Confianza.Que.Tiene.En.Su.Presidente.Municipa,Confianza.Que.Tiene.En.El.Gobernado,Calificacion.Trabajo.De.La.Polici	, Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denuncia	, Calificacion.Trabajo.De.Empleados.De.Gobiern, Calificacion.Trabajo.Presidente.Municipa	, Calificacion.Trabajo.Del.Gobernado	, Calificacion.Trato.Que.Da.La.Polici	,Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denuncia	, Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobiern,Calificacion.Trato.Recibe.De.Su.Presidente.Municipa	,Calificacion.Trato.Recibe.Del.Gobernado,Num.Personas.Viven.Esta.Cas,Cuantos.Hombre	,Cuantas.Mujere	,Cuantos.Menore,La.Madre.De.Los.Menores.Vive.En.Cas,El.Padre.De.Los.Menores.Vive.En.Cas	,Esta.Casa.Es.Propi	, Tiempo.Viviendo.En.Q.Ro,Tiempo.Viviendo.En.Esta.Cas
                    
){
  df_ide <- df_ide[, showvards, drop = FALSE]
  
  if("Sexo" %in% showvards){
    if(length(Sexos)!=0){
      df_ide <- dplyr::filter(df_ide, Sexo %in% Sexos)
    }
  }
  
  if("Participa.Eventos.Deportivos" %in% showvards){
    if(length(Participa.Eventos.Deportivo)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.Eventos.Deportivos %in% Participa.Eventos.Deportivo)
    }
  }
  
  if("Participa.En.Tandas" %in% showvards){
    if(length(Participa.En.Tanda)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.En.Tandas %in% Participa.En.Tanda)
    }
  }
  
  if("Participa.En.Fiestas" %in% showvards){
    if(length(word)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.En.Fiestas %in% word)
    }
  }
  
  
  if("Participa.En.Iglesia.Templo" %in% showvards){
    if(length(Participa.En.Iglesia.Templ)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.En.Iglesia.Templo %in% Participa.En.Iglesia.Templ)
    }
  }
  
  if("Participa.Solucion.Problemas.Comunidad" %in% showvards){
    if(length(Participa.Solucion.Problemas.Comunida)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.Solucion.Problemas.Comunidad %in% Participa.Solucion.Problemas.Comunida)
    }
  }
  
  if("Conoce.Vecinos" %in% showvards){
    if(length(Conoce.Vecino)!=0){
      df_ide <- dplyr::filter(df_ide, Conoce.Vecinos %in% Conoce.Vecino)
    }
  }
  
  
  if("Conoce.Vecinos.Confiaria.Ninos" %in% showvards){
    if(length(Conoce.Vecinos.Confiaria.Nino)!=0){
      df_ide <- dplyr::filter(df_ide, Conoce.Vecinos.Confiaria.Ninos %in% Conoce.Vecinos.Confiaria.Nino)
    }
  }
  
  if("Conoce.Vecinos.Confiaria.Casa" %in% showvards){
    if(length(Conoce.Vecinos.Confiaria.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, Conoce.Vecinos.Confiaria.Casa %in% Conoce.Vecinos.Confiaria.Cas)
    }
  }
  
  if("Conoce.Vecinos.Participa.Mejorar.Seguridad" %in% showvards){
    if(length(Conoce.Vecinos.Participa.Mejorar.Segurida)!=0){
      df_ide <- dplyr::filter(df_ide, Conoce.Vecinos.Participa.Mejorar.Seguridad %in% Conoce.Vecinos.Participa.Mejorar.Segurida)
    }
  }
  
  if("Participa.Con.Autoridad.Mejorar.Seguridad" %in% showvards){
    if(length(Participa.Con.Autoridad.Mejorar.Segurida)!=0){
      df_ide <- dplyr::filter(df_ide, Participa.Con.Autoridad.Mejorar.Seguridad %in% Participa.Con.Autoridad.Mejorar.Segurida)
    }
  }
  
  
  if("Cuando.Delito.Vecinos.Se.Reunen" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Se.Reune)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Se.Reunen %in% Cuando.Delito.Vecinos.Se.Reune)
    }
  }
  
  
  
  if("Cuando.Delito.Vecinos.Organizan.Para.Vigilar" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Organizan.Para.Vigila)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Organizan.Para.Vigilar %in% Cuando.Delito.Vecinos.Organizan.Para.Vigila)
    }
  }
  
  
  if("Cuando.Delito.Vecinos.Intercambian.Num.Tel" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Intercambian.Num.Te)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Intercambian.Num.Tel %in% Cuando.Delito.Vecinos.Intercambian.Num.Te)
    }
  }
  
  if("Cuando.Delito.Vecinos.Forman.Chat" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Forman.Cha)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Forman.Chat %in% Cuando.Delito.Vecinos.Forman.Cha)
    }
  }
  
  if("Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Ponen.Letreros.Advertenci)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia %in% Cuando.Delito.Vecinos.Ponen.Letreros.Advertenci)
    }
  }
  
  if("Cuando.Delito.Vecinos.Llaman.Policia" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Llaman.Polici)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Llaman.Policia %in% Cuando.Delito.Vecinos.Llaman.Polici)
    }
  }
  
  
  if("Cuando.Delito.Vecinos.Denuncian.Con.Autoridad" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Denuncian.Con.Autorida)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Denuncian.Con.Autoridad %in% Cuando.Delito.Vecinos.Denuncian.Con.Autorida)
    }
  }
  
  if("Durante.Ultimo.Ano.Hubo.Robo.casa" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Robo.cas)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Robo.casa %in% Durante.Ultimo.Ano.Hubo.Robo.cas)
    }
  }
  
  if("Durante.Ultimo.Ano.Hubo.Robo.Calle" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Robo.Call)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Robo.Calle %in% Durante.Ultimo.Ano.Hubo.Robo.Call)
    }
  }
  
  
  if("Durante.Ultimo.Ano.Hubo.Robo.Transporte" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Robo.Transport)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Robo.Transporte %in% Durante.Ultimo.Ano.Hubo.Robo.Transport)
    }
  }
  
  if("Durante.Ultimo.Ano.Hubo.Robo.Negocio" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Robo.Negoci)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Robo.Negocio %in% Durante.Ultimo.Ano.Hubo.Robo.Negoci)
    }
  }
  
  if("Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Robo.De.Vehicul)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo %in% Durante.Ultimo.Ano.Hubo.Robo.De.Vehicul)
    }
  }
  
  if("Durante.Ultimo.Ano.Hubo.Balaceras" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Balacera)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Balaceras %in% Durante.Ultimo.Ano.Hubo.Balacera)
    }
  }
  
  
  if("Durante.Ultimo.Ano.Hubo.Violencia.Familiar" %in% showvards){
    if(length(Durante.Ultimo.Ano.Hubo.Violencia.Familia)!=0){
      df_ide <- dplyr::filter(df_ide, Durante.Ultimo.Ano.Hubo.Violencia.Familiar %in% Durante.Ultimo.Ano.Hubo.Violencia.Familia)
    }
  }
  
  if("Riesgo.Sufrir.Delito.En.Casa" %in% showvards){
    if(length(Riesgo.Sufrir.Delito.En.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, Riesgo.Sufrir.Delito.En.Casa %in% Riesgo.Sufrir.Delito.En.Cas)
    }
  }
  
  if("Riesgo.Sufrir.Delito.En.Calle" %in% showvards){
    if(length(Riesgo.Sufrir.Delito.En.Call)!=0){
      df_ide <- dplyr::filter(df_ide, Riesgo.Sufrir.Delito.En.Calle %in% Riesgo.Sufrir.Delito.En.Call)
    }
  }
  
  if("Riesgo.Sufrir.Delito.En.Esta.Zona" %in% showvards){
    if(length(Riesgo.Sufrir.Delito.En.Esta.Zon)!=0){
      df_ide <- dplyr::filter(df_ide, Riesgo.Sufrir.Delito.En.Esta.Zona %in% Riesgo.Sufrir.Delito.En.Esta.Zon)
    }
  }
  
  if("Riesgo.Sufrir.Delito.En.Esta.Ciudad" %in% showvards){
    if(length(Riesgo.Sufrir.Delito.En.Esta.Ciuda)!=0){
      df_ide <- dplyr::filter(df_ide, Riesgo.Sufrir.Delito.En.Esta.Ciudad %in% Riesgo.Sufrir.Delito.En.Esta.Ciuda)
    }
  }
  
  if("Ha.Sido.Victima.Delito.Ultimo.Ano" %in% showvards){
    if(length(Ha.Sido.Victima.Delito.Ultimo.An)!=0){
      df_ide <- dplyr::filter(df_ide, Ha.Sido.Victima.Delito.Ultimo.Ano %in% Ha.Sido.Victima.Delito.Ultimo.An)
    }
  }
  
  if("Caso.Ser.Victima.Delito.Llama.Policia" %in% showvards){
    if(length(Caso.Ser.Victima.Delito.Llama.Polici)!=0){
      df_ide <- dplyr::filter(df_ide, Caso.Ser.Victima.Delito.Llama.Policia %in% Caso.Ser.Victima.Delito.Llama.Polici)
    }
  }
  
  
  if("Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro" %in% showvards){
    if(length(Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligr)!=0){
      df_ide <- dplyr::filter(df_ide, Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro %in% Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligr)
    }
  }
  
  if("Caso.Ser.Victima.Delito.Advierte.Familia.Peligro" %in% showvards){
    if(length(Caso.Ser.Victima.Delito.Advierte.Familia.Peligr)!=0){
      df_ide <- dplyr::filter(df_ide, Caso.Ser.Victima.Delito.Advierte.Familia.Peligro %in% Caso.Ser.Victima.Delito.Advierte.Familia.Peligr)
    }
  }
  
  if("Padres.Participan.Actividades.Con.Hijos" %in% showvards){
    if(length(Padres.Participan.Actividades.Con.Hijo)!=0){
      df_ide <- dplyr::filter(df_ide, Padres.Participan.Actividades.Con.Hijos %in% Padres.Participan.Actividades.Con.Hijo)
    }
  }
  
  if("Vecinos.Organizan.Prevenir.Delitos" %in% showvards){
    if(length(Vecinos.Organizan.Prevenir.Delito)!=0){
      df_ide <- dplyr::filter(df_ide, Vecinos.Organizan.Prevenir.Delitos %in% Vecinos.Organizan.Prevenir.Delito)
    }
  }
  
  if("Hay.Personas.Amables" %in% showvards){
    if(length(Hay.Personas.Amable)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.Amables %in% Hay.Personas.Amable)
    }
  }
  
  if("Hay.Personas.Que.Siempre.Ayudan.A.Otros" %in% showvards){
    if(length(Hay.Personas.Que.Siempre.Ayudan.A.Otro)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.Que.Siempre.Ayudan.A.Otros %in% Hay.Personas.Que.Siempre.Ayudan.A.Otro)
    }
  }
  
  if("Hay.Personas.A.Las.Que.Todos.Tienen.Miedo" %in% showvards){
    if(length(Hay.Personas.A.Las.Que.Todos.Tienen.Mied)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.A.Las.Que.Todos.Tienen.Miedo %in% Hay.Personas.A.Las.Que.Todos.Tienen.Mied)
    }
  }
  
  
  if("Hay.Personas.Que.Se.Emborrachan.O.Drogan" %in% showvards){
    if(length(Hay.Personas.Que.Se.Emborrachan.O.Droga)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.Que.Se.Emborrachan.O.Drogan %in% Hay.Personas.Que.Se.Emborrachan.O.Droga)
    }
  }
  
  if("Hay.Personas.Que.Han.Estado.Carcel" %in% showvards){
    if(length(Hay.Personas.Que.Han.Estado.Carce)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.Que.Han.Estado.Carcel %in% Hay.Personas.Que.Han.Estado.Carce)
    }
  }
  
  if("Hay.Personas.Sospechosas" %in% showvards){
    if(length(Hay.Personas.Sospechosa)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Personas.Sospechosas %in% Hay.Personas.Sospechosa)
    }
  }
  
  
  if("Hay.Violencia.Entre.Hombres" %in% showvards){
    if(length(Hay.Violencia.Entre.Hombre)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Violencia.Entre.Hombres %in% Hay.Violencia.Entre.Hombre)
    }
  }
  
  if("Hay.Violencia.Entre.Familias" %in% showvards){
    if(length(Hay.Violencia.Entre.Familia)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Violencia.Entre.Familias %in% Hay.Violencia.Entre.Familia)
    }
  }
  
  if("Hay.Violencia.Entre.Jovenes" %in% showvards){
    if(length(Hay.Violencia.Entre.Jovene)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Violencia.Entre.Jovenes %in% Hay.Violencia.Entre.Jovene)
    }
  }
  
  if("Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona" %in% showvards){
    if(length(Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Person)!=0){
      df_ide <- dplyr::filter(df_ide, Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona %in% Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Person)
    }
  }
  
  
  if("si.Conflictos.Entre.Vecinos.Dialogando" %in% showvards){
    if(length(si.Conflictos.Entre.Vecinos.Dialogand)!=0){
      df_ide <- dplyr::filter(df_ide, si.Conflictos.Entre.Vecinos.Dialogando %in% si.Conflictos.Entre.Vecinos.Dialogand)
    }
  }
  
  if("si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente" %in% showvards){
    if(length(si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosament)!=0){
      df_ide <- dplyr::filter(df_ide, si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente %in% si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosament)
    }
  }
  
  if("si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos" %in% showvards){
    if(length(si.Conflictos.Entre.Vecinos.Se.Manejan.A.Grito)!=0){
      df_ide <- dplyr::filter(df_ide, si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos %in% si.Conflictos.Entre.Vecinos.Se.Manejan.A.Grito)
    }
  }
  
  
  if("Cuando.Delito.Vecinos.Intercambian.Num.Tel" %in% showvards){
    if(length(Cuando.Delito.Vecinos.Intercambian.Num.Te)!=0){
      df_ide <- dplyr::filter(df_ide, Cuando.Delito.Vecinos.Intercambian.Num.Tel %in% Cuando.Delito.Vecinos.Intercambian.Num.Te)
    }
  }
  
  if("si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes" %in% showvards){
    if(length(si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpe)!=0){
      df_ide <- dplyr::filter(df_ide, si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes %in% si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpe)
    }
  }
  
  if("Jovenes.Hacen.Deporte" %in% showvards){
    if(length(Jovenes.Hacen.Deport)!=0){
      df_ide <- dplyr::filter(df_ide, Jovenes.Hacen.Deporte %in% Jovenes.Hacen.Deport)
    }
  }
  
  if("Jovenes.Ayudan.Otros" %in% showvards){
    if(length(Jovenes.Ayudan.Otro)!=0){
      df_ide <- dplyr::filter(df_ide, Jovenes.Ayudan.Otros %in% Jovenes.Ayudan.Otro)
    }
  }
  
  
  if("Mayoria.De.Jovenes.Estudian.Trabajan" %in% showvards){
    if(length(Mayoria.De.Jovenes.Estudian.Trabaja)!=0){
      df_ide <- dplyr::filter(df_ide, Mayoria.De.Jovenes.Estudian.Trabajan %in% Mayoria.De.Jovenes.Estudian.Trabaja)
    }
  }
  
  if("Jovenes.Andan.Pandillas" %in% showvards){
    if(length(Jovenes.Andan.Pandilla)!=0){
      df_ide <- dplyr::filter(df_ide, Jovenes.Andan.Pandillas %in% Jovenes.Andan.Pandilla)
    }
  }
  
  if("Jovenes.Son.Violentos" %in% showvards){
    if(length(Jovenes.Son.Violento)!=0){
      df_ide <- dplyr::filter(df_ide, Jovenes.Son.Violentos %in% Jovenes.Son.Violento)
    }
  }
  
  
  if("Hay.Parque" %in% showvards){
    if(length(Hay.Parqu)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque %in% Hay.Parqu)
    }
  }
  
  if("Hay.Parque.En.Buen.Estado" %in% showvards){
    if(length(Hay.Parque.En.Buen.Estad)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.En.Buen.Estado %in% Hay.Parque.En.Buen.Estad)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Ninos" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Nino)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Ninos %in% Hay.Parque.Utilizado.Por.Nino)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Jovenes" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Jovene)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Jovenes %in% Hay.Parque.Utilizado.Por.Jovene)
    }
  }
  
  
  if("Hay.Parque.Utilizado.Por.Pandillas" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Pandilla)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Pandillas %in% Hay.Parque.Utilizado.Por.Pandilla)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Familias" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Familia)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Familias %in% Hay.Parque.Utilizado.Por.Familia)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Personas.Tercera.Edad" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Personas.Tercera.Eda)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Personas.Tercera.Edad %in% Hay.Parque.Utilizado.Por.Personas.Tercera.Eda)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Adultos" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Adulto)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Adultos %in% Hay.Parque.Utilizado.Por.Adulto)
    }
  }
  
  if("Hay.Parque.Actividades.Supervisadas.Por.Adultos" %in% showvards){
    if(length(Hay.Parque.Actividades.Supervisadas.Por.Adulto)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Actividades.Supervisadas.Por.Adultos %in% Hay.Parque.Actividades.Supervisadas.Por.Adulto)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Vandalos" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Vandalo)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Vandalos %in% Hay.Parque.Utilizado.Por.Vandalo)
    }
  }
  
  if("Hay.Parque.Utilizado.Por.Personas.Otras.Zonas" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Personas.Otras.Zona)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Personas.Otras.Zonas %in% Hay.Parque.Utilizado.Por.Personas.Otras.Zona)
    }
  }
  
  
  if("Hay.Parque.Utilizado.Por.Usted" %in% showvards){
    if(length(Hay.Parque.Utilizado.Por.Uste)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Parque.Utilizado.Por.Usted %in% Hay.Parque.Utilizado.Por.Uste)
    }
  }
  
  if("Hay.Banquetas" %in% showvards){
    if(length(Hay.Banqueta)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Banquetas %in% Hay.Banqueta)
    }
  }
  
  if("Hay.Baches" %in% showvards){
    if(length(Hay.Bache)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Baches %in% Hay.Bache)
    }
  }
  
  if("Hay.Letreros.De.Calles" %in% showvards){
    if(length(Hay.Letreros.De.Calle)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Letreros.De.Calles %in% Hay.Letreros.De.Calle)
    }
  }
  
  if("Hay.Tiendita" %in% showvards){
    if(length(Hay.Tiendit)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Tiendita %in% Hay.Tiendit)
    }
  }
  
  if("Hay.Alumbrado" %in% showvards){
    if(length(Hay.Alumbrad)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Alumbrado %in% Hay.Alumbrad)
    }
  }
  
  if("Hay.Consumo.Alcohol.En.Calle" %in% showvards){
    if(length(Hay.Consumo.Alcohol.En.Call)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Consumo.Alcohol.En.Calle %in% Hay.Consumo.Alcohol.En.Call)
    }
  }
  
  
  if("Hay.Horarios.Transporte.Convenientes" %in% showvards){
    if(length(Hay.Horarios.Transporte.Conveniente)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Horarios.Transporte.Convenientes %in% Hay.Horarios.Transporte.Conveniente)
    }
  }
  
  if("Hay.Terrenos.Baldios" %in% showvards){
    if(length(Hay.Terrenos.Baldio)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Terrenos.Baldios %in% Hay.Terrenos.Baldio)
    }
  }
  
  if("Hay.Basura" %in% showvards){
    if(length(Hay.Basur)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Basura %in% Hay.Basur)
    }
  }
  
  
  if("Hay.Autos.Abandonados" %in% showvards){
    if(length(Hay.Autos.Abandonado)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Autos.Abandonados %in% Hay.Autos.Abandonado)
    }
  }
  
  if("Hay.Casas.Abandonadas" %in% showvards){
    if(length(Hay.Casas.Abandonada)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Casas.Abandonadas %in% Hay.Casas.Abandonada)
    }
  }
  
  if("Hay.Vandalismo" %in% showvards){
    if(length(Hay.Vandalism)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Vandalismo %in% Hay.Vandalism)
    }
  }
  
  if("Hay.Grafiti" %in% showvards){
    if(length(Hay.Grafit)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Grafiti %in% Hay.Grafit)
    }
  }
  
  
  if("Hay.Venta.Alcohol.Cigarros.A.Menores" %in% showvards){
    if(length(Hay.Venta.Alcohol.Cigarros.A.Menore)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Venta.Alcohol.Cigarros.A.Menores %in% Hay.Venta.Alcohol.Cigarros.A.Menore)
    }
  }
  
  if("Hay.Venta.Droga" %in% showvards){
    if(length(Hay.Venta.Drog)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Venta.Droga %in% Hay.Venta.Drog)
    }
  }
  
  if("Hay.Venta.Alcohol.Despues.De.Once.Noche" %in% showvards){
    if(length(Hay.Venta.Alcohol.Despues.De.Once.Noch)!=0){
      df_ide <- dplyr::filter(df_ide, Hay.Venta.Alcohol.Despues.De.Once.Noche %in% Hay.Venta.Alcohol.Despues.De.Once.Noch)
    }
  }
  
  
  if("Algun.Menor.Abandono.Escuela" %in% showvards){
    if(length(Algun.Menor.Abandono.Escuel)!=0){
      df_ide <- dplyr::filter(df_ide, Algun.Menor.Abandono.Escuela %in% Algun.Menor.Abandono.Escuel)
    }
  }
  
  if("Algun.Menor.Tiene.Problemas.Conducta" %in% showvards){
    if(length(Algun.Menor.Tiene.Problemas.Conduct)!=0){
      df_ide <- dplyr::filter(df_ide, Algun.Menor.Tiene.Problemas.Conducta %in% Algun.Menor.Tiene.Problemas.Conduct)
    }
  }
  
  if("Algun.Menor.Quedo.Embarazada" %in% showvards){
    if(length(Algun.Menor.Quedo.Embarazad)!=0){
      df_ide <- dplyr::filter(df_ide, Algun.Menor.Quedo.Embarazada %in% Algun.Menor.Quedo.Embarazad)
    }
  }
  
  if("Corregir.Ninos.Recomienda.Castigarle" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Castigarl)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Castigarle %in% Corregir.Ninos.Recomienda.Castigarl)
    }
  }
  
  
  if("Corregir.Ninos.Recomienda.Gritarle" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Gritarl)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Gritarle %in% Corregir.Ninos.Recomienda.Gritarl)
    }
  }
  
  if("Corregir.Ninos.Recomienda.Darle.Nalgadas" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Darle.Nalgada)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Darle.Nalgadas %in% Corregir.Ninos.Recomienda.Darle.Nalgada)
    }
  }
  
  if("Corregir.Ninos.Recomienda.Explicarle.Esta.Mal" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Explicarle.Esta.Ma)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Explicarle.Esta.Mal %in% Corregir.Ninos.Recomienda.Explicarle.Esta.Ma)
    }
  }
  
  
  if("Corregir.Ninos.Recomienda.Aconsejarle" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Aconsejarl)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Aconsejarle %in% Corregir.Ninos.Recomienda.Aconsejarl)
    }
  }
  
  if("Corregir.Ninos.Recomienda.Ensenar.Ejemplo" %in% showvards){
    if(length(Corregir.Ninos.Recomienda.Ensenar.Ejempl)!=0){
      df_ide <- dplyr::filter(df_ide, Corregir.Ninos.Recomienda.Ensenar.Ejemplo %in% Corregir.Ninos.Recomienda.Ensenar.Ejempl)
    }
  }
  
  if("En.Casa.Todos.Se.Conocen" %in% showvards){
    if(length(En.Casa.Todos.Se.Conoce)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Todos.Se.Conocen %in% En.Casa.Todos.Se.Conoce)
    }
  }
  
  if("En.Casa.Platican.Unos.Con.Otros" %in% showvards){
    if(length(En.Casa.Platican.Unos.Con.Otro)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Platican.Unos.Con.Otros %in% En.Casa.Platican.Unos.Con.Otro)
    }
  }
  
  
  if("En.Casa.Comen.Juntos" %in% showvards){
    if(length(En.Casa.Comen.Junto)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Comen.Juntos %in% En.Casa.Comen.Junto)
    }
  }
  
  if("En.Casa.Se.Ayudan.Gastos" %in% showvards){
    if(length(En.Casa.Se.Ayudan.Gasto)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Se.Ayudan.Gastos %in% En.Casa.Se.Ayudan.Gasto)
    }
  }
  
  if("En.Casa.Discuten" %in% showvards){
    if(length(En.Casa.Discute)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Discuten %in% En.Casa.Discute)
    }
  }
  
  if("En.Casa.Se.Gritan" %in% showvards){
    if(length(En.Casa.Se.Grita)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Se.Gritan %in% En.Casa.Se.Grita)
    }
  }
  
  if("En.Casa.Se.Ignoran" %in% showvards){
    if(length(En.Casa.Se.Ignora)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Se.Ignoran %in% En.Casa.Se.Ignora)
    }
  }
  
  if("En.Casa.Alguien.Tiene.Discapacidad" %in% showvards){
    if(length(En.Casa.Alguien.Tiene.Discapacida)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Alguien.Tiene.Discapacidad %in% En.Casa.Alguien.Tiene.Discapacida)
    }
  }
  
  if("En.Casa.Alguien.No.Habla.Espanol" %in% showvards){
    if(length(En.Casa.Alguien.No.Habla.Espano)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Alguien.No.Habla.Espanol %in% En.Casa.Alguien.No.Habla.Espano)
    }
  }
  
  
  if("En.Casa.Alguien.Necesita.Ayuda.Obesidad" %in% showvards){
    if(length(En.Casa.Alguien.Necesita.Ayuda.Obesida)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Alguien.Necesita.Ayuda.Obesidad %in% En.Casa.Alguien.Necesita.Ayuda.Obesida)
    }
  }
  
  if("En.Casa.Alguien.Necesita.Ayuda.Fumar" %in% showvards){
    if(length(En.Casa.Alguien.Necesita.Ayuda.Fuma)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Alguien.Necesita.Ayuda.Fumar %in% En.Casa.Alguien.Necesita.Ayuda.Fuma)
    }
  }
  
  if("En.Casa.Alguien.Necesita.Ayuda.Beber" %in% showvards){
    if(length(En.Casa.Alguien.Necesita.Ayuda.Bebe)!=0){
      df_ide <- dplyr::filter(df_ide, En.Casa.Alguien.Necesita.Ayuda.Beber %in% En.Casa.Alguien.Necesita.Ayuda.Bebe)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa %in% Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Cas)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciuda)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad %in% Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciuda)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estad)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado %in% Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estad)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noch)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche %in% Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noch)
    }
  }
  
  
  if("Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Camina)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar %in% Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Camina)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Call)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle %in% Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Call)
    }
  }
  
  if("Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas" %in% showvards){
    if(length(Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Persona)!=0){
      df_ide <- dplyr::filter(df_ide, Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas %in% Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Persona)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigo)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos %in% Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigo)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Dejo.Usar.Tax)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi %in% Ultimo.Ano.Por.Seguridad.Dejo.Usar.Tax)
    }
  }
  
  if("Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectiv)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo %in% Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectiv)
    }
  }
  
  
  if("Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas" %in% showvards){
    if(length(Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joya)!=0){
      df_ide <- dplyr::filter(df_ide, Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas %in% Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joya)
    }
  }
  
  if("Policia.Cuida.Vigila.Bien" %in% showvards){
    if(length(Policia.Cuida.Vigila.Bie)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Cuida.Vigila.Bien %in% Policia.Cuida.Vigila.Bie)
    }
  }
  
  if("Policia.Comete.Abusos" %in% showvards){
    if(length(Policia.Comete.Abuso)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Comete.Abusos %in% Policia.Comete.Abuso)
    }
  }
  
  
  if("Policia.Acude.Llamados" %in% showvards){
    if(length(Policia.Acude.Llamado)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Acude.Llamados %in% Policia.Acude.Llamado)
    }
  }
  
  if("Policia.Pide.Mordidas" %in% showvards){
    if(length(Policia.Pide.Mordida)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Pide.Mordidas %in% Policia.Pide.Mordida)
    }
  }
  
  if("Policia.Hace.Rondines" %in% showvards){
    if(length(Policia.Hace.Rondine)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Hace.Rondines %in% Policia.Hace.Rondine)
    }
  }
  
  if("Policia.Comete.Delitos" %in% showvards){
    if(length(Policia.Comete.Delito)!=0){
      df_ide <- dplyr::filter(df_ide, Policia.Comete.Delitos %in% Policia.Comete.Delito)
    }
  }
  
  
  if("Confianza.Que.Tiene.En.La.Policia" %in% showvards){
    if(length(Confianza.Que.Tiene.En.La.Polici)!=0){
      df_ide <- dplyr::filter(df_ide, Confianza.Que.Tiene.En.La.Policia %in% Confianza.Que.Tiene.En.La.Polici)
    }
  }
  
  if("Confianza.Que.Tiene.En.Ministerio.Publico" %in% showvards){
    if(length(Confianza.Que.Tiene.En.Ministerio.Públic)!=0){
      df_ide <- dplyr::filter(df_ide, Confianza.Que.Tiene.En.Ministerio.Publico %in% Confianza.Que.Tiene.En.Ministerio.Públic)
    }
  }
  
  if("Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona" %in% showvards){
    if(length(Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zon)!=0){
      df_ide <- dplyr::filter(df_ide, Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona %in% Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zon)
    }
  }
  
  
  if("Confianza.Que.Tiene.En.Su.Presidente.Municipal" %in% showvards){
    if(length(Confianza.Que.Tiene.En.Su.Presidente.Municipa)!=0){
      df_ide <- dplyr::filter(df_ide, Confianza.Que.Tiene.En.Su.Presidente.Municipal %in% Confianza.Que.Tiene.En.Su.Presidente.Municipa)
    }
  }
  
  if("Confianza.Que.Tiene.En.El.Gobernador" %in% showvards){
    if(length(Confianza.Que.Tiene.En.El.Gobernado)!=0){
      df_ide <- dplyr::filter(df_ide, Confianza.Que.Tiene.En.El.Gobernador %in% Confianza.Que.Tiene.En.El.Gobernado)
    }
  }
  
  if("Calificacion.Trabajo.De.La.Policia" %in% showvards){
    if(length(Calificacion.Trabajo.De.La.Polici)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trabajo.De.La.Policia %in% Calificacion.Trabajo.De.La.Polici)
    }
  }
  
  if("Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar" %in% showvards){
    if(length(Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denuncia)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar %in% Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denuncia)
    }
  }
  
  
  if("Calificacion.Trabajo.De.Empleados.De.Gobierno" %in% showvards){
    if(length(Calificacion.Trabajo.De.Empleados.De.Gobiern)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trabajo.De.Empleados.De.Gobierno %in% Calificacion.Trabajo.De.Empleados.De.Gobiern)
    }
  }
  
  if("Calificacion.Trabajo.Presidente.Municipal" %in% showvards){
    if(length(Calificacion.Trabajo.Presidente.Municipa)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trabajo.Presidente.Municipal %in% Calificacion.Trabajo.Presidente.Municipa)
    }
  }
  
  if("Calificacion.Trabajo.Del.Gobernador" %in% showvards){
    if(length(Calificacion.Trabajo.Del.Gobernado)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trabajo.Del.Gobernador %in% Calificacion.Trabajo.Del.Gobernado)
    }
  }
  
  
  if("Calificacion.Trato.Que.Da.La.Policia" %in% showvards){
    if(length(Calificacion.Trato.Que.Da.La.Polici)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trato.Que.Da.La.Policia %in% Calificacion.Trato.Que.Da.La.Polici)
    }
  }
  
  if("Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar" %in% showvards){
    if(length(Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denuncia)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar %in% Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denuncia)
    }
  }
  
  if("Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno" %in% showvards){
    if(length(Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobiern)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno %in% Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobiern)
    }
  }
  
  if("Calificacion.Trato.Recibe.De.Su.Presidente.Municipal" %in% showvards){
    if(length(Calificacion.Trato.Recibe.De.Su.Presidente.Municipa)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trato.Recibe.De.Su.Presidente.Municipal %in% Calificacion.Trato.Recibe.De.Su.Presidente.Municipa)
    }
  }
  
  
  if("Calificacion.Trato.Recibe.Del.Gobernador" %in% showvards){
    if(length(Calificacion.Trato.Recibe.Del.Gobernado)!=0){
      df_ide <- dplyr::filter(df_ide, Calificacion.Trato.Recibe.Del.Gobernador %in% Calificacion.Trato.Recibe.Del.Gobernado)
    }
  }
  
  if("Num.Personas.Viven.Esta.Casa" %in% showvards){
    if(length(Num.Personas.Viven.Esta.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, Num.Personas.Viven.Esta.Casa %in% Num.Personas.Viven.Esta.Cas)
    }
  }
  
  if("Cuantos.Hombres" %in% showvards){
    if(length(Cuantos.Hombre)!=0){
      df_ide <- dplyr::filter(df_ide, Cuantos.Hombres %in% Cuantos.Hombre)
    }
  }
  
  if("Cuantas.Mujeres" %in% showvards){
    if(length(Cuantas.Mujere)!=0){
      df_ide <- dplyr::filter(df_ide, Cuantas.Mujeres %in% Cuantas.Mujere)
    }
  }
  
  if("Cuantos.Menores" %in% showvards){
    if(length(Cuantos.Menore)!=0){
      df_ide <- dplyr::filter(df_ide, Cuantos.Menores %in% Cuantos.Menore)
    }
  }
  
  if("La.Madre.De.Los.Menores.Vive.En.Casa" %in% showvards){
    if(length(La.Madre.De.Los.Menores.Vive.En.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, La.Madre.De.Los.Menores.Vive.En.Casa %in% La.Madre.De.Los.Menores.Vive.En.Cas)
    }
  }
  
  if("El.Padre.De.Los.Menores.Vive.En.Casa" %in% showvards){
    if(length(El.Padre.De.Los.Menores.Vive.En.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, El.Padre.De.Los.Menores.Vive.En.Casa %in% El.Padre.De.Los.Menores.Vive.En.Cas)
    }
  }
  
  
  if("Esta.Casa.Es.Propia" %in% showvards){
    if(length(Esta.Casa.Es.Propi)!=0){
      df_ide <- dplyr::filter(df_ide, Esta.Casa.Es.Propia %in% Esta.Casa.Es.Propi)
    }
  }
  
  if("Tiempo.Viviendo.En.Q.Roo" %in% showvards){
    if(length(Tiempo.Viviendo.En.Q.Ro)!=0){
      df_ide <- dplyr::filter(df_ide, Tiempo.Viviendo.En.Q.Roo %in% Tiempo.Viviendo.En.Q.Ro)
    }
  }
  
  if("Tiempo.Viviendo.En.Esta.Casa" %in% showvards){
    if(length(Tiempo.Viviendo.En.Esta.Cas)!=0){
      df_ide <- dplyr::filter(df_ide, Tiempo.Viviendo.En.Esta.Casa %in% Tiempo.Viviendo.En.Esta.Cas)
    }
  }
  
  
  return(df_ide)
}


datasetComplete <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")[c(5:6,12, 15:21,23:28,30:34,36, 37:51, 53:58,62,79,84:94,96,107:113,115,120,126,133:138,140,146:148,152,153,157,159,161,162,164:167,169:171)]
datasetCompletes <- read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[,c(6,8:14,18,22:28,31:34,36,37,39,41:49,62:66,69:71,73,74,76:81,95:98,101,103:121,123:128,130:132,135:140,142:150,152,153,157:160,162:164,168:172,174:185,187:191,193:197,199:210)]



buttons <- lapply(1:ncol(datasetComplete), function(i){
  actionButtonStyled(
    paste0("this_id_is_not_used",i),
    "Frecuencia",
    type="primary",
    onclick = sprintf(
      "Shiny.setInputValue('button', %d, {priority:'event'});
          $('#modal%d').modal('show');", i, i)
  )
})

## DataTable #####
output$tableInfo <- renderDT({
  sketch <- tags$table(
    class = "row-border stripe hover compact",
    tableHeader(c("", c("Info.Sexo"="Sexo",
                        "Info.Edad"="Edad",
                        "Direccion.Colonia"="Colonia",
                        "V01_Casa_Adultos"="Núm.Adultos","V02_Casa_Ninos"="Núm.Niños","DF_kinder"="Cuántos.Kinder",
                        "DF_primaria"="Cuántos.Primaria","DF_secundaria"="Cuántos.Secundaria","DF_bachillerato"="Cuántos.Bachillerato","DF_licenciatura"="Cuántos.Licenciatura",
                        "DF_tr_autobus"="Autobus","DF_tr_colectivo"="Colectivo","DF_tr_taxi"="Taxi","DF_tr_mototaxi"="Mototaxi",
                        "DF_tr_moto"="Moto","DF_tr_auto"="Auto",
                        "DF_v_col"="Mercado.Col","DF_v_abarrote"="Abarrote","DF_v_super"="Super","DF_v_conv"="Tienda.conveniencia","DF_v_plaza"="Plaza",
                        "DF_urg_casa"="Emergencia.En.Casa",
                        "DF_urg_partcom"="Emergencia.Medico.Part.Comunidad", "DF_urg_partcan"="Emergencia.Medico.Part.Cancun","DF_urg_hosp"="Emergencia.Hospital",
                        "DF_urg_cruz"="Emergencia.CruzR","DF_urg_farmacia"="Emergencia.Farmacia","DF_urg_otro"="Emergencia.Otro","DF_a_parque"="Colonia.Con.Parque","DF_a_unidad"="Colonia.Con.Uni.Dep",
                        "DF_a_jardines"="Colonia.Con.Jardin","DF_a_casa"="Colonia.Con.Centro.Comunitario","DF_a_biblioteca"="Colonia.Con.Biblio","DF_a_otro"="Colonia.Con.Otro","DE01_hogar_trabajan"="Cuantas.Personas.Trabajan",
                        "DE02_trabajo"="Jefe.Trabajo","DE03_puesto"="Jefe.Puesto","DE05_ingreso_sem"="Ingreso.Sem","DE06_esc_jefe"="Jefe.Escolaridad","DE07_sIngreso"="Cuántos.Sin.Ingreso",
                        "DE08_tmp"="Tiempo.A.Trabajo","DE09_col"="Colonia.Trabajo","DE10_mun"="Municipio.Trabajo","IyC02_Estado_Origen"= "Estado.Origen",
                        "IyC03_Tiempo"="Tiempo.En.Isla","IyC04_Motivo_Localidad_Parientes"="Motivo.Localidad.Parientes","IyC04_Motivo_Localidad_Amigos"="Motivo.Localidad.Amigos","IyC04_Motivo_Localidad_Trabajo"="Motivo.Localidad.Trabajo","IyC04_Motivo_Localidad_Negocio"="Motivo.Localidad.Negocio",
                        "IyC04_Motivo_Localidad_Oportunidad"="Motivo.Localidad.Oportunidad","IyC04_Motivo_Localidad_Otro"="Motivo.Localidad.Otro","IyC05_Acudo_Vecinos"="Acudo.Vecinos","IyC05_Acudo_Familia"="Acudo.Familia","IyC05_Acudo_Autoridad"="Acudo.Autoridad","IyC05_Acudo_Iglesia"="Acudo.Iglesia",
                        "IyC05_Acudo_Otro"="Acudo.Otro","IyC06_Religion"="Religion",
                        "IyC08_Ventajas_Casa"="Ventajas.Casa","IyC08_Ventajas_Trabajo"="Ventajas.Trabajo","IyC08_Ventajas_Familia"="Ventajas.Familia","IyC08_Ventajas_Tiempo"="Ventajas.Tiempo",
                        
                        "IyC08_Ventajas_Tranquilo"="Ventajas.Tranquilo","IyC08_Ventajas_Seguro"="Ventajas.Seguro","IyC08_Ventajas_Otro"="Ventajas.Otro","IyC09_Emigrar"="Emigrar","IyC10_Pertenencia"="Pertenencia","IyC11_Frecuencia_Cabecera_Isla"="Frec.Visita.Cabecera_Isla","IyC12_Motivo_Viaja_Asuntos_Admin"="Motivo.Viaja.Asuntos.Admin",
                        "IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS"="Motivo.Viaja.Pago.Servicio","IyC12_Motivo_Viaja_Trabajo"="Motivo.Viaja.Trabajo","IyC12_Motivio_Viaja_Recreacion"="Motivio.Viaja.Recreación","IyC12_Motivo_Viaja_Familia"="Motivo.Viaja.Familia","IyC12_Motivo_Viaja_Otro"="Motivo.Viaja.Otro",
                        "VI01_La_vivienda_es"="La.Vivienda.Es","VI04_Contado"="Vivienda.Contado","VI04_Herencia"="Vivienda.Herencia","VI04_Mensual"="Vivienda.Mensual",
                        "VI08NINUNDACIONES"="Pasado.Inundaciones",
                        "VI09PeHuracan"="Pasado.Huracan","VI14SabeZo0Riesgo"="Sabe.Zo.Riesgo","VI15SabeAfectaciones"="Sabe.Afectaciones","AE1_AreasVerdes"="Obs.AreasVerdes","AE3_Banquetas"="Obs.Banquetas","AE4_Lumi0rias"="Obs.Luminarias","AE5_Transporte"="Obs.Transporte",
                        "AE6_Patrullas"="Obs.Patrullas","AE7_Lotes"="Obs.Lotes","AE9_PeSeguridad"="Obs.PeSeguridad","AE10_PeComodidad"="Obs.PeComodidad","AE11_PeRiesgo"="Obs.PeRiesgo"
                        
    ) )),   #names(datasetComplete)
    tableFooter(c("", buttons))
  )
  datatable(
    datasetComplete, container = sketch,
    options =
      list(
        scrollX = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
          
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 10
      )
  )
})

# modals ####
output$modals <- renderUI({
  lapply(1:ncol(datasetComplete), function(i){
    bsModal(
      id = paste0("modal",i),
      title = c("Info.Sexo"="Sexo",
                "Info.Edad"="Edad",
                "Direccion.Colonia"="Colonia",
                "V01_Casa_Adultos"="Núm.Adultos","V02_Casa_Ninos"="Núm.Niños","DF_kinder"="Cuántos.Kinder",
                "DF_primaria"="Cuántos.Primaria","DF_secundaria"="Cuántos.Secundaria","DF_bachillerato"="Cuántos.Bachillerato","DF_licenciatura"="Cuántos.Licenciatura",
                "DF_tr_autobus"="Autobus","DF_tr_colectivo"="Colectivo","DF_tr_taxi"="Taxi","DF_tr_mototaxi"="Mototaxi",
                "DF_tr_moto"="Moto","DF_tr_auto"="Auto",
                "DF_v_col"="Mercado.Col","DF_v_abarrote"="Abarrote","DF_v_super"="Super","DF_v_conv"="Tienda.conveniencia","DF_v_plaza"="Plaza",
                "DF_urg_casa"="Emergencia.En.Casa",
                "DF_urg_partcom"="Emergencia.Medico.Part.Comunidad", "DF_urg_partcan"="Emergencia.Medico.Part.Cancun","DF_urg_hosp"="Emergencia.Hospital",
                "DF_urg_cruz"="Emergencia.CruzR","DF_urg_farmacia"="Emergencia.Farmacia","DF_urg_otro"="Emergencia.Otro","DF_a_parque"="Colonia.Con.Parque","DF_a_unidad"="Colonia.Con.Uni.Dep",
                "DF_a_jardines"="Colonia.Con.Jardin","DF_a_casa"="Colonia.Con.Centro.Comunitario","DF_a_biblioteca"="Colonia.Con.Biblio","DF_a_otro"="Colonia.Con.Otro","DE01_hogar_trabajan"="Cuantas.Personas.Trabajan",
                "DE02_trabajo"="Jefe.Trabajo","DE03_puesto"="Jefe.Puesto","DE05_ingreso_sem"="Ingreso.Sem","DE06_esc_jefe"="Jefe.Escolaridad","DE07_sIngreso"="Cuántos.Sin.Ingreso",
                "DE08_tmp"="Tiempo.A.Trabajo","DE09_col"="Colonia.Trabajo","DE10_mun"="Municipio.Trabajo","IyC02_Estado_Origen"= "Estado.Origen",
                "IyC03_Tiempo"="Tiempo.En.Isla","IyC04_Motivo_Localidad_Parientes"="Motivo.Localidad.Parientes","IyC04_Motivo_Localidad_Amigos"="Motivo.Localidad.Amigos","IyC04_Motivo_Localidad_Trabajo"="Motivo.Localidad.Trabajo","IyC04_Motivo_Localidad_Negocio"="Motivo.Localidad.Negocio",
                "IyC04_Motivo_Localidad_Oportunidad"="Motivo.Localidad.Oportunidad","IyC04_Motivo_Localidad_Otro"="Motivo.Localidad.Otro","IyC05_Acudo_Vecinos"="Acudo.Vecinos","IyC05_Acudo_Familia"="Acudo.Familia","IyC05_Acudo_Autoridad"="Acudo.Autoridad","IyC05_Acudo_Iglesia"="Acudo.Iglesia",
                "IyC05_Acudo_Otro"="Acudo.Otro","IyC06_Religion"="Religion",
                "IyC08_Ventajas_Casa"="Ventajas.Casa","IyC08_Ventajas_Trabajo"="Ventajas.Trabajo","IyC08_Ventajas_Familia"="Ventajas.Familia","IyC08_Ventajas_Tiempo"="Ventajas.Tiempo",
                "IyC08_Ventajas_Tranquilo"="Ventajas.Tranquilo","IyC08_Ventajas_Seguro"="Ventajas.Seguro","IyC08_Ventajas_Otro"="Ventajas.Otro","IyC09_Emigrar"="Emigrar","IyC10_Pertenencia"="Pertenencia","IyC11_Frecuencia_Cabecera_Isla"="Frec.Visita.Cabecera_Isla","IyC12_Motivo_Viaja_Asuntos_Admin"="Motivo.Viaja.Asuntos.Admin",
                "IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS"="Motivo.Viaja.Pago.Servicio","IyC12_Motivo_Viaja_Trabajo"="Motivo.Viaja.Trabajo","IyC12_Motivio_Viaja_Recreacion"="Motivio.Viaja.Recreación","IyC12_Motivo_Viaja_Familia"="Motivo.Viaja.Familia","IyC12_Motivo_Viaja_Otro"="Motivo.Viaja.Otro",
                "VI01_La_vivienda_es"="La.Vivienda.Es","VI04_Contado"="Vivienda.Contado","VI04_Herencia"="Vivienda.Herencia","VI04_Mensual"="Vivienda.Mensual",
                "VI08NINUNDACIONES"="Pasado.Inundaciones",
                "VI09PeHuracan"="Pasado.Huracan","VI14SabeZo0Riesgo"="Sabe.Zo.Riesgo","VI15SabeAfectaciones"="Sabe.Afectaciones","AE1_AreasVerdes"="Obs.AreasVerdes","AE3_Banquetas"="Obs.Banquetas","AE4_Lumi0rias"="Obs.Luminarias","AE5_Transporte"="Obs.Transporte",
                "AE6_Patrullas"="Obs.Patrullas","AE7_Lotes"="Obs.Lotes","AE9_PeSeguridad"="Obs.PeSeguridad","AE10_PeComodidad"="Obs.PeComodidad","AE11_PeRiesgo"="Obs.PeRiesgo"
                
      )[i],
      trigger = paste0("this_is_not_used",i),
      plotOutput(paste0("plotse",i))
    )
  })
})

# plots in modals ####
for(i in 1:ncol(datasetComplete)){
  local({
    ii <- i
    output[[paste0("plotse",ii)]] <- renderPlot({
      if(is.numeric(datasetComplete[[ii]])){
        if( length(unique(datasetComplete[[ii]]))==2 ){
          ggplot( data=datasetComplete, aes_string(names(datasetComplete)[ii]) ) +
            geom_bar( binwidth=input[[paste0("slider",ii)]], colour="white", fill="lightblue")+
            scale_x_continuous(name = " ", breaks = c(0, 1))+
            theme(axis.title = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust=1))+
            geom_text(aes(label = scales::percent(..prop..), group = 1), stat = "count", vjust = 1.5)
        }else{
          #flevels <- unique((datasetComplete)[ii])
          ggplot( data=datasetComplete, aes_string(names(datasetComplete)[ii]) ) +
            geom_bar( binwidth=input[[paste0("slider",ii)]], colour="white", fill="lightblue")+
            theme(axis.title = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust=1))+
            geom_text(aes(label = scales::percent(..prop..), group = 1), stat = "count", vjust = 1.5)
        }
      }else{
        ggplot( data=datasetComplete, aes_string(names(datasetComplete)[ii]) ) +
          geom_bar( binwidth=input[[paste0("slider",ii)]], colour="white", fill="lightblue") +
          theme(axis.title = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust=1))+
          geom_text(aes(label = scales::percent(..prop..), group = 1), stat = "count", vjust = 1.5)
      }
      
    })
  })
}

datasetCompletes <- read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)[,c(6,8:14,18,22:28,31:34,36,37,39,41:49,62:66,69:71,73,74,76:81,95:98,101,103:121,123:128,130:132,135:140,142:150,152,153,157:160,162:164,168:172,174:185,187:191,193:197,199:210)]

 
buttonss <- lapply(1:ncol(datasetCompletes), function(i){
  actionButtonStyled(
    paste0("this_id_is_not_usedd",i),
    "Frecuencia",
    type="primary",
    onclick = sprintf(
      "Shiny.setInputValue('button', %d, {priority:'event'});
            $('#modals%d').modal('show');", i, i)
  )
})

## DataTable #####
output$tableInfos <- renderDT({
  sketch <- tags$table(
    class = "row-border stripe hover compact",
    tableHeader(c("", names(datasetCompletes))),
    tableFooter(c("", buttonss))
  )
  datatable(
    datasetCompletes, container = sketch,
    options =
      list(
        scrollX = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 10
      )
  )
})

# modals ####
output$modalss <- renderUI({
  lapply(1:ncol(datasetCompletes), function(i){
    bsModal(
      id = paste0("modals",i),
      title = names(datasetCompletes)[i],
      trigger = paste0("this_is_not_usedd",i),
      plotOutput(paste0("plotset",i))
    )
  })
})

# plots in modals ####
for(i in 1:ncol(datasetCompletes)){
  local({
    ii <- i
    output[[paste0("plotset",ii)]] <- renderPlot({
      if( length(unique(datasetCompletes[[ii]]))==2 ){
        ggplot( data=datasetCompletes, aes_string(names(datasetCompletes)[ii]) ) +
          geom_bar( binwidth=input[[paste0("slider",ii)]], colour="white", fill="lightblue") +
          scale_x_continuous(name = " ", breaks = c(0, 1))+
          theme(axis.title = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust=1))+
          geom_text(aes(label = scales::percent(..prop..), group = 1), stat = "count", vjust = 1.5)
      }else{
        ggplot( data=datasetCompletes, aes_string(names(datasetCompletes)[ii]) ) +
          geom_bar( binwidth=input[[paste0("slider",ii)]], colour="white", fill="lightblue") +
          theme(axis.title = element_text(size = 16),axis.text.x = element_text(angle = 45, hjust=1))+
          geom_text(aes(label = scales::percent(..prop..), group = 1), stat = "count", vjust = 1.5)
        
      }
      
      
      
      
    })
  })
}



pru <- reactive({
  
  df_ide <- lect()
  
  df_ide <- df_ide[, input$show_vars, drop = FALSE]
  showvar <- input$show_vars
  in_edad <- input$Info.Edad
  in_sex <- input$Info.Sexo
  D.Calle <- input$Direccion.Calle
  N.Ext <- input$Direccion.Numero.Exterior
  N.int <- input$Direccion.Numero.Interior
  Manza0 <- input$Direccion.Manza0
  Mz <- input$Direccion.Super.Manza0
  Col <- input$Direccion.Colonia
  CP <- input$Direccion.Codigo.Postal
  Cas_a <- input$V01_Casa_Adultos
  Cas_N <- input$V02_Casa_Ninos
  Kin <- input$DF_kinder
  prim <- input$DF_primaria
  sec <- input$DF_secundaria
  bach <- input$DF_bachillerato
  lic <- input$DF_licenciatura
  autobu <- input$DF_tr_autobus
  colec <- input$DF_tr_colectivo
  taxi <- input$DF_tr_taxi
  motax <- input$DF_tr_mototaxi
  moto <- input$DF_tr_moto
  auto <- input$DF_tr_auto
  v_col <- input$DF_v_col
  abarro <- input$DF_v_abarrote
  super <- input$DF_v_super
  conv <- input$DF_v_conv
  plaza <- input$DF_v_plaza
  urg_c <- input$DF_urg_casa
  urgpatc <- input$DF_urg_partcom
  urgpart <- input$DF_urg_partcan
  urgh <- input$DF_urg_hosp
  urgc <- input$DF_urg_cruz
  urgf <- input$DF_urg_farmacia
  urgo <- input$DF_urg_otro
  parque <- input$DF_a_parque
  unid <- input$DF_a_unidad
  jardin <- input$DF_a_jardines
  unidaddep <- input$DF_a_casa
  biblio <- input$DF_a_biblioteca
  aotro <- input$DF_a_otro
  trbajan <- input$DE01_hogar_trabajan
  trabajo <- input$DE02_trabajo
  puesto <- input$DE03_puesto
  ingreso <- input$DE05_ingreso_sem
  escola <- input$DE06_esc_jefe
  sining <- input$DE07_sIngreso
  tiempo <- input$DE08_tmp
  colon <- input$DE09_col
  muni <- input$DE10_mun
  origen <- input$IyC02_Estado_Origen
  tiempoi <- input$IyC03_Tiempo
  mpar <-input$IyC04_Motivo_Localidad_Parientes
  mamig <- input$IyC04_Motivo_Localidad_Amigos
  mtrab <- input$IyC04_Motivo_Localidad_Trabajo
  mnego <- input$IyC04_Motivo_Localidad_Negocio
  mopor <- input$IyC04_Motivo_Localidad_Oportunidad
  motro <- input$IyC04_Motivo_Localidad_Otro
  acudvec <- input$IyC05_Acudo_Vecinos
  acudfam <- input$IyC05_Acudo_Familia
  acudaut <- input$IyC05_Acudo_Autoridad
  acudig <- input$IyC05_Acudo_Iglesia
  acudotro <- input$IyC05_Acudo_Otro
  relig <- input$IyC06_Religion
  venc <- input$IyC08_Ventajas_Casa
  ventr <- input$IyC08_Ventajas_Trabajo
  ventfam <- input$IyC08_Ventajas_Familia
  venttiem <- input$IyC08_Ventajas_Tiempo
  venttranq <- input$IyC08_Ventajas_Tranquilo
  ventseg <- input$IyC08_Ventajas_Seguro
  ventotro <- input$IyC08_Ventajas_Otro
  emigrar <- input$IyC09_Emigrar
  perten <- input$IyC10_Pertenencia
  frecc <- input$IyC11_Frecuencia_Cabecera_Isla
  mvasun <- input$IyC12_Motivo_Viaja_Asuntos_Admin
  mvfds <- input$IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS
  mvtrab <- input$IyC12_Motivo_Viaja_Trabajo
  mvrec <- input$IyC12_Motivio_Viaja_Recreacion
  mvfam <- input$IyC12_Motivo_Viaja_Familia
  mvotro <- input$IyC12_Motivo_Viaja_Otro
  vve <- input$VI01_La_vivienda_es
  conta <- input$VI04_Contado
  heren <- input$VI04_Herencia
  mens <- input$VI04_Mensual
  #prest <- input$VI01_Prestada
  cpmen <- input$VI05_CPropiaMensual
  cpadq <- input$VI06_CPropiaAdquirida
  inun <- input$VI08NINUNDACIONES
  hura <- input$VI09PeHuracan
  ssr <- input$VI14SabeZo0Riesgo
  safec <- input$VI15SabeAfectaciones
  aver <- input$AE1_AreasVerdes
  abanq <- input$AE3_Banquetas
  alum <-input$AE4_Lumi0rias
  trans <- input$AE5_Transporte
  pat <- input$AE6_Patrullas
  lot <- input$AE7_Lotes
  pseg <- input$AE9_PeSeguridad
  pcom <- input$AE10_PeComodidad
  pries <- input$AE11_PeRiesgo
  
  df_ide <- Func_A(df_ide, showvar, in_sex, in_edad, D.Calle, N.Ext, N.int, Manza0, Mz, Col,CP,Cas_a,Cas_N,Kin,prim,sec,bach,lic,autobu,colec,taxi,motax,moto,auto,v_col,abarro,super,conv,plaza,urg_c,urgpatc,urgpart,urgh,urgc,urgf,urgo,parque,unid,jardin,unidaddep,biblio,aotro,trbajan,trabajo,puesto,ingreso,escola,sining,tiempo,colon,muni,origen,tiempoi,mpar,mamig,mtrab,mnego,mopor,motro,acudvec,acudfam,acudaut,acudig,acudotro,relig,venc,ventr, ventfam,venttiem,venttranq,ventseg,ventotro,emigrar,perten,frecc,mvasun,mvfds,mvtrab,mvrec,mvfam,mvotro,vve,conta,heren,mens,cpmen,cpadq,inun,hura,ssr,safec,aver,abanq,alum,trans,pat,lot,pseg,pcom, pries)
  
  return(df_ide)
  
})


rules <- reactive({
  
  
  if(input$selec == "Percepción de Seguridad"){
    
    df_ide <- lect()
  }else{
    df_ide <- lect2()
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
     length(input$AE7_Lotes)!=0 || length(input$AE9_PeSeguridad)!=0 || length(input$AE10_PeComodidad)!=0  || length(input$AE11_PeRiesgo)!=0 
     ||
     
     length(input$Participa.Eventos.Deportivos)!=0 || length(input$Participa.En.Tandas)!=0  || length(input$Participa.En.Fiestas)!=0  || length(input$Participa.En.Iglesia.Templo)!=0  || length(input$Participa.Solucion.Problemas.Comunidad)!=0  || length(input$Conoce.Vecinos)!=0  || length(input$Conoce.Vecinos.Confiaria.Ninos	)!=0  || length(input$Conoce.Vecinos.Confiaria.Casa	)!=0  || length(input$Conoce.Vecinos.Participa.Mejorar.Seguridad)!=0  || length(input$Participa.Con.Autoridad.Mejorar.Seguridad	)!=0  || length(input$Cuando.Delito.Vecinos.Se.Reunen)!=0  || length(input$Cuando.Delito.Vecinos.Organizan.Para.Vigilar	)!=0  || length(input$Cuando.Delito.Vecinos.Intercambian.Num.Tel	)!=0  || length(input$Cuando.Delito.Vecinos.Forman.Chat	)!=0  || length(input$Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia	)!=0  || length(input$Cuando.Delito.Vecinos.Llaman.Policia	)!=0  || length(input$Cuando.Delito.Vecinos.Denuncian.Con.Autoridad	)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Robo.casa	)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Robo.Calle)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Robo.Transporte	)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Robo.Negocio)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Balaceras	)!=0  || length(input$Durante.Ultimo.Ano.Hubo.Violencia.Familiar	)!=0  || length(input$Riesgo.Sufrir.Delito.En.Casa	)!=0  || length(input$Riesgo.Sufrir.Delito.En.Calle)!=0  || length(input$Riesgo.Sufrir.Delito.En.Esta.Zona	)!=0  || length(input$Riesgo.Sufrir.Delito.En.Esta.Ciudad	)!=0  || length(input$Ha.Sido.Victima.Delito.Ultimo.Ano	)!=0  || length(input$Caso.Ser.Victima.Delito.Llama.Policia	)!=0  || length(input$Caso.Ser.Victima.Delito.Hace.Denuncia	)!=0  || length(input$Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro)!=0  || length(input$Caso.Ser.Victima.Delito.Advierte.Familia.Peligro	)!=0  || length(input$Padres.Participan.Actividades.Con.Hijos) ||
     length(input$Vecinos.Organizan.Prevenir.Delitos)!=0 || length(input$Hay.Personas.Amables)!=0 || length(input$Hay.Personas.Que.Siempre.Ayudan.A.Otros)!=0 || length(input$Hay.Personas.A.Las.Que.Todos.Tienen.Miedo)!=0 || length(input$Hay.Personas.Que.Se.Emborrachan.O.Drogan	)!=0 || length(input$Hay.Personas.Que.Han.Estado.Carcel)!=0 || length(input$Hay.Personas.Sospechosas	)!=0 || length(input$Hay.Violencia.Entre.Hombres	)!=0 || length(input$Hay.Violencia.Entre.Familias)!=0 || length(input$Hay.Violencia.Entre.Jovenes	)!=0 || length(input$Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona)!=0 || length(input$si.Conflictos.Entre.Vecinos.Dialogando)!=0 || length(input$si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente)!=0 || length(input$si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos	)!=0 || length(input$si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes	)!=0 || length(input$Jovenes.Hacen.Deporte	)!=0 || length(input$Jovenes.Ayudan.Otros)!=0 || length(input$Mayoria.De.Jovenes.Estudian.Trabajan)!=0 || length(input$Jovenes.Andan.Pandillas)!=0 || length(input$Jovenes.Son.Violentos	)!=0 || length(input$Hay.Parque)!=0 || length(input$Hay.Parque.En.Buen.Estado	)!=0 || length(input$Hay.Parque.Utilizado.Por.Ninos)!=0 || length(input$Hay.Parque.Utilizado.Por.Jovenes)!=0 || length(input$Hay.Parque.Utilizado.Por.Pandillas)!=0 || length(input$Hay.Parque.Utilizado.Por.Familias)!=0 || length(input$Hay.Parque.Utilizado.Por.Personas.Tercera.Edad)!=0 || length(input$Hay.Parque.Utilizado.Por.Adultos)!=0 || length(input$Hay.Parque.Actividades.Supervisadas.Por.Adultos	)!=0 || length(input$Hay.Parque.Utilizado.Por.Vandalos	)!=0 || length(input$Hay.Parque.Utilizado.Por.Personas.Otras.Zonas	)!=0 || length(input$Hay.Parque.Utilizado.Por.Usted	)!=0 || length(input$Hay.Banquetas)!=0 || length(input$Hay.Baches)!=0 ||
     length(input$Hay.Letreros.De.Calles)!=0 || length(input$Hay.Tiendita	)!=0 || length(input$Hay.Alumbrado)!=0 || length(input$Hay.Consumo.Alcohol.En.Calle)!=0 || length(input$Hay.Horarios.Transporte.Convenientes	)!=0 || length(input$Hay.Terrenos.Baldios)!=0 || length(input$Hay.Basura	)!=0 || length(input$Hay.Autos.Abandonados	)!=0 || length(input$Hay.Casas.Abandonadas	)!=0 || length(input$Hay.Vandalismo)!=0 || length(input$Hay.Grafiti)!=0 || length(input$Hay.Venta.Alcohol.Cigarros.A.Menores	)!=0 || length(input$Hay.Venta.Droga	)!=0 || 
     length(input$Hay.Venta.Alcohol.Despues.De.Once.Noche	)!=0 || 
     length(input$Algun.Menor.Abandonó.Escuela	)!=0 || length(input$Algun.Menor.Tiene.Problemas.Conducta	)!=0 || length(input$Algun.Menor.Quedó.Embarazada)!=0 || length(input$Corregir.Ninos.Recomienda.Castigarle)!=0 || length(input$Corregir.Ninos.Recomienda.Gritarle	)!=0 || length(input$Corregir.Ninos.Recomienda.Darle.Nalgadas)!=0 || 
     length(input$Corregir.Ninos.Recomienda.Explicarle.Esta.Mal	)!=0 || 
     length(input$Corregir.Ninos.Recomienda.Aconsejarle	)!=0 || length(input$Corregir.Ninos.Recomienda.Ensenar.Ejemplo)!=0 ||  length(input$En.Casa.Platican.Unos.Con.Otros	)!=0 || length(input$En.Casa.Comen.Juntos)!=0 || length(input$En.Casa.Se.Ayudan.Gastos)!=0 || length(input$En.Casa.Discuten)!=0 || length(input$En.Casa.Se.Gritan	)!=0 || length(input$En.Casa.Se.Ignoran)!=0 || length(input$En.Casa.Alguien.Tiene.Discapacidad)!=0 || length(input$En.Casa.Alguien.No.Habla.Espanol	)!=0 || length(input$En.Casa.Alguien.Necesita.Ayuda.Obesidad	)!=0 || length(input$En.Casa.Alguien.Necesita.Ayuda.Fumar	)!=0 || length(input$En.Casa.Alguien.Necesita.Ayuda.Beber	)!=0 ||
     length(input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejó.Salir.Noche)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Dejó.De.Salir.Caminar	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Impidió.Ninos.Salieran.Calle)!=0 || length(input$Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Dejó.De.Visitar.Parientes.Amigos	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Dejó.Usar.Taxi	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Dejó.De.Llevar.Mucho.Efectivo	)!=0 || length(input$Ultimo.Ano.Por.Seguridad.Dejó.De.Usar.Joyas	)!=0 || length(input$Policia.Cuida.Vigila.Bien	)!=0 || length(input$Policia.Comete.Abusos	)!=0 || length(input$Policia.Acude.Llamados)!=0 || length(input$Policia.Pide.Mordidas)!=0 || length(input$Policia.Hace.Rondines	)!=0 || length(input$Policia.Comete.Delitos	)!=0 || length(input$Confianza.Que.Tiene.En.La.Policia)!=0 || length(input$Confianza.Que.Tiene.En.Ministerio.Público)!=0 || length(input$Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona	)!=0 || length(input$Confianza.Que.Tiene.En.Su.Presidente.Municipal)!=0 || length(input$Confianza.Que.Tiene.En.El.Gobernador)!=0 || length(input$Calificación.Trabajo.De.La.Policia	)!=0 || length(input$Calificación.Trabajo.Del.Ministerio.Público.Para.Denunciar	)!=0 || length(input$Calificación.Trabajo.De.Empleados.De.Gobierno)!=0 || length(input$Calificación.Trabajo.Presidente.Municipal	)!=0 || length(input$Calificación.Trabajo.Del.Gobernador	)!=0 || length(input$Calificación.Trato.Que.Da.La.Policia	)!=0 || length(input$Calificación.Trato.Que.Da.Ministerio.Público.Para.Denunciar	)!=0 || length(input$Calificación.Trato.Recibe.De.Los.Empleados.De.Gobierno)!=0 || length(input$Calificación.Trato.Recibe.De.Su.Presidente.Municipal	)!=0 || length(input$Calificación.Trato.Recibe.Del.Gobernador)!=0 || length(input$Núm.Personas.Viven.Esta.Casa)!=0 || length(input$Cuantos.Hombres	)!=0 || length(input$Cuantas.Mujeres	)!=0 || length(input$Cuantos.Menores)!=0 || length(input$La.Madre.De.Los.Menores.Vive.En.Casa)!=0 || length(input$El.Padre.De.Los.Menores.Vive.En.Casa	)!=0 || length(input$Esta.Casa.Es.Propia	)!=0 || length(input$Tiempo.Viviendo.En.Q.Roo)!=0 || length(input$Tiempo.Viviendo.En.Esta.Casa)!=0 |
     length(input$Sexo)!=0
  )
  {
    
    
    if(input$selec == "Percepción de Seguridad"){
      
      df_ide <- pru()
      
    }else{
      df_ide <- pru2()    
    }
    
    
    df_ide2 <- as(df_ide,"transactions")
    
    rules_s<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),
                                           maxtime=as.numeric(input$time), target = "rules"))
    
    rulesSorted_ide <- sort(rules_s, by = "confidence")
    gi <- generatingItemsets(rulesSorted_ide)
    d <- which(duplicated(gi))
    reglas_seleccionadas_ide <- rulesSorted_ide[-d]
    
  }else{
    
    
    if(input$selec == "Percepción de Seguridad"){
      df_ide <- df_ide[, input$show_vars, drop = FALSE]
    }else{
      df_ide <- df_ide[, input$show_vards, drop = FALSE]
    }
    
    df_ide2 <- as(df_ide,"transactions")
    rules_s<-apriori(df_ide,parameter=list(support=as.numeric(input$sup),confidence=as.numeric(input$conf) ,minlen=as.numeric(input$len),maxlen = as.numeric(input$mlen),maxtime=as.numeric(input$time), target = "rules"))
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
  p<-plot(income_rules,method = "graph", engine="htmlwidget", shading="confidence")
  
  print (p)
})

output$txt0 <- renderUI({
  if(input$selec == "Percepción de Seguridad"){
    icons <- paste(input$show_vars, collapse = ", ")
    if(any(length(input$show_vars)>0)){
      word <- paste("<b>","FILTROS ","</b>")
    }else{
      word <-paste("")
    }
  }else{
    icons <- paste(input$show_vards, collapse = ", ")
    if(any(length(input$show_vards)>0)){
      word <- paste("<b>","FILTROS ","</b>")
    }else{
      word <-paste("")
    }
  }
  HTML(paste(word))
})

output$txt1 <- renderUI({
  if(input$selec == "Percepción de Seguridad"){
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('Info.Edad'==input$show_vars)){
      word <- paste("<b>","o La variable Edad está dividida en intervalos para incorporarla en la regla de asociaciones: ","</b>",tags$ul(
        tags$li("18 - Joven"),
        tags$li("19 a 30 - Adulto Joven"),
        tags$li("31 a 60 - Adulto"),
        tags$li("61 o Más - Tercera edad")))
    }else{
      word <-paste("")
    }
    HTML(paste(word))
  }
})


output$txt2 <- renderText({
  if(input$selec == "Percepción de Seguridad"){
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
  }
})

output$txt3 <- renderText({
  if(input$selec == "Percepción de Seguridad"){
    icons <- paste(input$show_vars, collapse = ", ")
    if(any('DE08_tmp'==input$show_vars)){
      word <- paste("<b>","o El tiempo que tarda el jefe de familia en llegar a su trabajo esta categorizado de la siguiente manera: ","</b>",tags$ul(
        tags$li("A - Trabaja en casa"),
        tags$li("B - Menos de 20 min"),
        tags$li("C - Más de 20 min y menos de 1 hora"),
        tags$li("D - Entre 1 y 2 horas"),
        tags$li("E - Más de 2 horas")))
    }else{
      word <-paste("")
    }
    HTML(paste(word))
  }
})

output$txt4 <- renderUI({
  word <- paste("<b>",tags$i("o Descripción del grafo:"),"</b>",tags$i("Los nodos del grafo indican las reglas de asociación que se obtienen con las columnas seleccionadas, estas se conectan por medio de flechas que van del antecedente al consecuente. En el medio de estas flechas de conexiones, podemos encontrar un círculo cuyo tam. representa el valor de soporte o frecuencia de la regla mientras que el color representa la confianza. Podemos ver que la confianza se representa desde rosa pálido hasta el rojo, siendo este último el valor más alto."))
  HTML(paste(word))
  
})



output$txt5 <- renderUI({
  #income_rules <-rules()
  #income_rules =c(0,1,0)
  
  if(input$selec == "Percepción de Seguridad"){
    df <- pru()
    if(dim(df)[1]==0){
      word <- paste(tags$i("Error: Los filtros elegidos de columnas, no tienen coincidencias. Elegir otros filtros o quitar el último seleccionado."))
      
    }else if(any(length(input$show_vars)<2) ){
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else{
      word = paste("")
    }
    HTML(paste(word))
    
    
  }else{
    df <- pru2()
    if(dim(df)[1]==0){
      word <- paste(tags$i("Error: Los filtros elegidos de columnas, no tienen coincidencias. Elegir otros filtros o quitar el último seleccionado."))
    }else if(any(length(input$show_vards)<2) ){
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else{
      word = ""
    }
    HTML(paste(word))
  }
})


output$txt9 <- renderUI({
  income_rules <- rules()
  if(input$selec == "Percepción de Seguridad" ){
    if(length(income_rules)==0 && any(length(input$show_vars)>=2)){
      word <- paste(("Error: Es necesario reducir ya sea el soporte, la confianza o modificar el mín y máx de cambinaciones en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
  }else{
    if(length(income_rules)==0 && any(length(input$show_vards)>=2)){
      word <- paste(("Error: Es necesario reducir ya sea el soporte, la confianza o modificar el mín y máx de cambinaciones en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
  }
  
})


output$txt8 <- renderUI({
  #income_rules <-rules()
  if(input$selec == "Características de población y migración"){
    word <- paste("<b>","o Las preguntas con respuesta de si y no tienen el siguiente formato: ","</b>",tags$ul(
      tags$li("0 - No"),
      tags$li("1 - Si"),
      tags$li("En ocasiones se mostrará un valor -1 indicando que el/la entrevistad@ no contesto la pregunta, lo cual puede significar que no cuenta con el objetivo de la pregunta. Ejemplo: ¿El Padre de los menores vive en casa? a lo cual la persona anteriormente respondió que no cuenta con niños por lo que se omitió la respuesta a esa pregunta.")
    ))
  }else{
    word = ""
  }
  HTML(paste(word))
})



output$txt10 <- renderUI({
  word = paste("<b>","TABLA DE DATOS","</b>")
  
  HTML(paste(word))
})

output$txt20 <- renderUI({
  word = paste("<b>","REGLAS DE ASOCIACIÓN","</b>")
  
  HTML(paste(word))
})


output$txt6 <- renderUI({
  income_rules <-rules()
  if(input$selec == "Percepción de Seguridad"){
    if(any(length(input$show_vars)<2)){
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else if(length(income_rules)==0 ){
      word <- paste(tags$i("Error: Es necesario reducir ya sea el soporte o la confianza en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
    HTML(paste(word))
  }else{
    if(any(length(input$show_vards)<2)){
      word <- paste(tags$i("Error: Es necesario seleccionar 2 o más variables de datos en la barra lateral izquierda debajo de 'Campos posibles a seleccionar'."))
    }else if(length(income_rules)==0 ){
      word <- paste(tags$i("Error: Es necesario reducir ya sea el soporte o la confianza en la barra lateral izquierda, de lo contrario la asociación no encontró reglas significativas."))
    }else{
      word = ""
    }
    HTML(paste(word))
  }
})


secc2 <- function(Text, showvar, selec){
  if(selec == "Características de población y migración"){
    if(Text %in% showvar){
      df_ide <- read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),Text, Text, choices = unique(df_ide[,Text]))
      selectInput(Text, Text, choices = NULL,multiple = TRUE)
    }
  }
}

secc3 <- function(Text, showvar, selec, tex){
  if(selec == "Características de población y migración"){
    if(Text %in% showvar){
      df_ide <- read.table(file='./CSV/ps/Isla-Mujeres-230030001-2.csv', sep=',', header = TRUE, stringsAsFactors = FALSE)
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),Text, tex, choices = unique(df_ide[,Text]))
      selectInput(Text, tex, choices = NULL,multiple = TRUE)
    }
  }
}





secc <- function(Text, showvar, selec, tex){
  if(selec == "Percepción de Seguridad"){
    if(Text %in% showvar){
      df_ide <- read.table(file='./CSV/ps/columnas1.csv', sep=',', header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
      for(i in 1:170){df_ide[,i]<-factor(df_ide[,i])}
      updateSelectInput(getDefaultReactiveDomain(),Text,tex, choices = unique(df_ide[,Text]))
      selectInput(Text, tex, choices = NULL,multiple = TRUE)
    }
  }
}


output$rows1 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Info.Sexo"
tex <- "Sexo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)
}else{ Text <- "Sexo"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec)
}
})

output$rows2 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Info.Edad"
tex <- "Edad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Participa.Eventos.Deportivos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec)}  })

output$rows3 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Calle"
tex <- "Calle"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Participa.En.Tandas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec)}  })

output$rows4 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Numero.Exterior"
tex <- "N.Exterior"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{ Text <- "Participa.En.Fiestas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows5 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Numero.Interior"
tex <- "N.Interior"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Participa.En.Iglesia.Templo"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows6 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Manza0"
tex <- "Mz"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Participa.Solucion.Problemas.Comunidad"
text <- "Participa.Solución.Problemas.Comunidad"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows7 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Super.Manza0"
tex <- "Smz"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Conoce.Vecinos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows8 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Colonia"
tex <- "Colonia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Conoce.Vecinos.Confiaria.Ninos"
text <- "Conoce.Vecinos.Confiaria.Niños"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows9 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "Direccion.Codigo.Postal"
tex <- "Codigo.P"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Conoce.Vecinos.Confiaria.Casa"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows10 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "V01_Casa_Adultos"
tex <- "Núm.Adultos"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)  }else{ Text <- "Conoce.Vecinos.Participa.Mejorar.Seguridad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows11 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "V02_Casa_Ninos"
tex <- "Núm.Niños"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Participa.Con.Autoridad.Mejorar.Seguridad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows12 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_kinder"
tex <- "Cuántos.Kinder"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex) }else{ Text <- "Cuando.Delito.Vecinos.Se.Reunen"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows13 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_primaria"
tex <- "Cuántos.Primaria"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{ Text <- "Cuando.Delito.Vecinos.Organizan.Para.Vigilar"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows14 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_secundaria"
tex <- "Cuántos.Secundaria"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Cuando.Delito.Vecinos.Intercambian.Num.Tel"
text <- "Cuando.Delito.Vecinos.Intercambian.Núm.Tel"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows15 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_bachillerato"
tex <- "Cuántos.Bachillerato"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Cuando.Delito.Vecinos.Forman.Chat"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows16 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_licenciatura"
tex <- "Cuántos.Licenciatura"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Cuando.Delito.Vecinos.Ponen.Letreros.Advertencia"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows17 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_autobus"
tex <- "Autobus"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Cuando.Delito.Vecinos.Llaman.Policia"
text <- "Cuando.Delito.Vecinos.Llaman.Policía"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows18 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_colectivo"
tex <- "Colectivo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Cuando.Delito.Vecinos.Denuncian.Con.Autoridad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows19 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_taxi"
tex <- "Taxi"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Robo.casa"
text <- "Durante.Último.Año.Hubo.Robo.casa"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows20 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_mototaxi"
tex <- "Mototaxi"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Robo.Calle"
text <- "Durante.Último.Año.Hubo.Robo.Calle"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows21 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_moto"
tex <- "Moto"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Robo.Transporte"
text <- "Durante.Último.Año.Hubo.Robo.Transporte"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows22 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_tr_auto"
tex <- "Auto"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Robo.Negocio"
text <- "Durante.Último.Año.Hubo.Robo.Negocio"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows23 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_v_col"
tex <- "Mercado.Colonia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Robo.De.Vehiculo"
text <- "Durante.Último.Año.Hubo.Robo.De.Vehículo"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows24 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_v_abarrote"
tex <- "Abarrote"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Balaceras"
text <- "Durante.Último.Año.Hubo.Balaceras"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows25 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_v_super"
tex <- "Super"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Durante.Ultimo.Ano.Hubo.Violencia.Familiar"
text <- "Durante.Último.Año.Hubo.Violencia.Familiar"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows26 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_v_conv"
tex <- "Tienda.conveniencia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Riesgo.Sufrir.Delito.En.Casa"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows27 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_v_plaza"
tex <- "Plaza"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Riesgo.Sufrir.Delito.En.Calle"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows28 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_casa"
tex <- "Emergencia.En.Casa"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Riesgo.Sufrir.Delito.En.Esta.Zona"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows29 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_partcom"
tex <- "Emergencia.Medico.Part.Comunidad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Riesgo.Sufrir.Delito.En.Esta.Ciudad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows30 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_partcan"
tex <- "Emergencia.Medico.Part.Cancun"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Ha.Sido.Victima.Delito.Ultimo.Ano"
text <- "Ha.Sido.Victima.Delito.Último.Año"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows31 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_hosp"
tex <- "Emergencia.Hospital"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Caso.Ser.Victima.Delito.Llama.Policia"
text <- "Caso.Ser.Victima.Delito.Llama.Policía"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })


output$rows32 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_cruz"
tex <- "Emergencia.CruzR"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Caso.Ser.Victima.Delito.Hace.Denuncia"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows33 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_farmacia"
tex <- "Emergencia.Farmacia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Caso.Ser.Victima.Delito.Advierte.Vecinos.Peligro"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows34 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_urg_otro"
tex <- "Emergencia.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Caso.Ser.Victima.Delito.Advierte.Familia.Peligro"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows35 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_parque"
tex <- "Colonia.Con.Parque"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Padres.Participan.Actividades.Con.Hijos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows36 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_unidad"
tex <- "Colonia.Con.Uni.Dep"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Vecinos.Organizan.Prevenir.Delitos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows37 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_jardines"
tex <- "Colonia.Con.Jardin"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.Amables"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows38 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_casa"
tex <- "Colonia.Con.Centro.Comunitario"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.Que.Siempre.Ayudan.A.Otros"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows39 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_biblioteca"
tex <- "Colonia.Con.Biblio"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.A.Las.Que.Todos.Tienen.Miedo"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows40 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DF_a_otro"
tex <- "Colonia.Con.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.Que.Se.Emborrachan.O.Drogan"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows41 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE01_hogar_trabajan"
tex <- "Cuantas.Personas.Trabajan"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.Que.Han.Estado.Carcel"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows42 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE02_trabajo"
tex <- "Jefe.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Personas.Sospechosas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows43 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE03_puesto"
tex <- "Jefe.Puesto"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Violencia.Entre.Hombres"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows44 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE05_ingreso_sem"
tex <- "Ingreso.Sem"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Violencia.Entre.Familias"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows45 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE06_esc_jefe"
tex <- "Jefe.Escolaridad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Violencia.Entre.Jovenes"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows46 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE07_sIngreso"
tex <- "Cuantos.Sin.Ingreso"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Conflictos.Entre.Vecinos.Conciliando.Con.Tercera.Persona"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows47 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE08_tmp"
tex <- "Tiempo.A.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "si.Conflictos.Entre.Vecinos.Dialogando"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows48 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE09_col"
tex <- "Colonia.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "si.Conflictos.Entre.Vecinos.Se.Manejan.Respetuosamente"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows49 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "DE10_mun"
tex <- "Municipio.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "si.Conflictos.Entre.Vecinos.Se.Manejan.A.Gritos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows50 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC02_Estado_Origen"
tex <- "Estado.Origen"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "si.Conflictos.Entre.Vecinos.Se.Manejan.Con.Golpes"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows51 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC03_Tiempo"
tex <- "Tiempo.En.Isla"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Jovenes.Hacen.Deporte"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows52 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Parientes"
tex <- "Motivo.Localidad.Parientes"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Jovenes.Ayudan.Otros"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows53 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Amigos"
tex <- "Motivo.Localidad.Amigos"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Mayoria.De.Jovenes.Estudian.Trabajan"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows54 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Trabajo"
tex <- "Motivo.Localidad.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Jovenes.Andan.Pandillas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows55 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Negocio"
tex <- "Motivo.Localidad.Negocio"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Jovenes.Son.Violentos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows56 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Oportunidad"
tex <- "Motivo.Localidad.Oportunidad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows57 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC04_Motivo_Localidad_Otro"
tex <- "Motivo.Localidad.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.En.Buen.Estado"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows58 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC05_Acudo_Vecinos"
tex <- "Acudo.Vecinos"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Ninos"
text <- "Hay.Parque.Utilizado.Por.Niños"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows59 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC05_Acudo_Familia"
tex <- "Acudo.Familia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Jovenes"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows60 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC05_Acudo_Autoridad"
tex <- "Acudo.Autoridad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Pandillas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows61 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC05_Acudo_Iglesia"
tex <- "Acudo.Iglesia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Familias"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows62 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC05_Acudo_Otro"
tex <- "Acudo.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Personas.Tercera.Edad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows63 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC06_Religion"
tex <- "Religion"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Adultos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows65 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Casa"
tex <- "Ventajas.Casa"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Actividades.Supervisadas.Por.Adultos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows66 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Trabajo"
tex <- "Ventajas.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Vandalos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows67 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Familia"
tex <- "Ventajas.Familia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Personas.Otras.Zonas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows68 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Tiempo"
tex <- "Ventajas.Tiempo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Parque.Utilizado.Por.Usted"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows69 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Tranquilo"
tex <- "Ventajas.Tranquilo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Banquetas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows70 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Seguro"
tex <- "Ventajas.Seguro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Baches"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows71 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC08_Ventajas_Otro"
tex <- "Ventajas.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Letreros.De.Calles"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows72 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC09_Emigrar"
tex <- "Emigrar"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Tiendita"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows73 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC10_Pertenencia"
tex <- "Pertenencia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Alumbrado"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows74 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC11_Frecuencia_Cabecera_Isla"
tex <- "Frec.Visita.Cabecera_Isla"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Consumo.Alcohol.En.Calle"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows75 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivo_Viaja_Asuntos_Admin"
tex <- "Motivo.Viaja.Asuntos.Admin"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Horarios.Transporte.Convenientes"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows76 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS"
tex <- "Motivo.Viaja.Pago.Servicio"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Terrenos.Baldios"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows77 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivo_Viaja_Trabajo"
tex <- "Motivo.Viaja.Trabajo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Basura"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows78 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivio_Viaja_Recreacion"
tex <- "Motivio.Viaja.Recreacion"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Autos.Abandonados"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows79 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivo_Viaja_Familia"
tex <- "Motivo.Viaja.Familia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Casas.Abandonadas"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows80 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "IyC12_Motivo_Viaja_Otro"
tex <- "Motivo.Viaja.Otro"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Vandalismo"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows81 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI01_La_vivienda_es"
tex <- "La.Vivienda.Es"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Grafiti"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows82 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI04_Contado"
tex <- "Vivienda.Contado"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Venta.Alcohol.Cigarros.A.Menores"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows83 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI04_Herencia"
tex <- "Vivienda.Herencia"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Venta.Droga"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows84 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI01_Prestada"
tex <- "Vivienda.Prestada"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Hay.Venta.Alcohol.Despues.De.Once.Noche"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows85 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI04_Mensual"
tex <- "Vivienda.Mensual"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Algun.Menor.Abandono.Escuela"
text <- "Algún.Menor.Abandono.Escuela"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows86 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI05_CPropiaMensual"
tex <- "C.Propia.Mensual"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Algun.Menor.Tiene.Problemas.Conducta"
text <- "Algún.Menor.Tiene.Problemas.Conducta"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows87 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI06_CPropiaAdquirida"
tex <- "C.Propia.Adquirida"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Algun.Menor.Quedo.Embarazada"
text <- "Algún.Menor.Quedó.Embarazada"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) }  })

output$rows89 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "VI08NINUNDACIONES"
tex <- "Pasado.Inundaciones"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Castigarle"
text <- "Corregir.Niños.Recomienda.Castigarle"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows90 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI09PeHuracan"
tex <- "Pasado.Huracan"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Gritarle"
text <- "Corregir.Niños.Recomienda.Gritarle"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) }  })

output$rows91 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI14SabeZo0Riesgo"
tex <- "Sabe.Zo.Riesgo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Darle.Nalgadas"
text <- "Corregir.Niños.Recomienda.Darle.Nalgadas"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) }  })

output$rows92 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "VI15SabeAfectaciones"
tex <- "Sabe.Afectaciones"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Explicarle.Esta.Mal"
text <- "Corregir.Niños.Recomienda.Explicarle.Esta.Mal"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) }  })

output$rows93 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "AE1_AreasVerdes"
tex <- "Obs.AreasVerdes"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Aconsejarle"
text <- "Corregir.Niños.Recomienda.Aconsejarle"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) }  })

output$rows94 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "AE3_Banquetas"
tex <- "Obs.Banquetas"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "Corregir.Ninos.Recomienda.Ensenar.Ejemplo"
text <- "Corregir.Niños.Recomienda.Enseñar.Ejemplo"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) }  })

output$rows95 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "AE4_Lumi0rias"
tex <- "Obs.Luminarias"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Todos.Se.Conocen"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows96 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "AE5_Transporte"
tex <- "Obs.Transporte"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Platican.Unos.Con.Otros"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows97 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "AE6_Patrullas"
tex <- "Obs.Patrullas"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Comen.Juntos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows98 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "AE7_Lotes"
tex <- "Obs.Lotes"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Se.Ayudan.Gastos"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows99 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "AE9_PeSeguridad"
tex <- "Obs.PeSeguridad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Discuten"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) }  })

output$rows100 <- renderUI({ if(input$selec == "Percepción de Seguridad"){ Text <- "AE10_PeComodidad"
tex <- "Obs.PeComodidad"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Se.Gritan"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows101 <- renderUI({  if(input$selec == "Percepción de Seguridad"){ Text <- "AE11_PeRiesgo"
tex <- "Obs.PeRiesgo"
showvar <- input$show_vars
selec <- input$selec
secc(Text, showvar, selec, tex)}else{Text <- "En.Casa.Se.Ignoran"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows102 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "En.Casa.Alguien.Tiene.Discapacidad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows103 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "En.Casa.Alguien.No.Habla.Espanol"
text <- "En.Casa.Alguien.No.Habla.Español"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows104 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "En.Casa.Alguien.Necesita.Ayuda.Obesidad"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows105 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "En.Casa.Alguien.Necesita.Ayuda.Fumar"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows106 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "En.Casa.Alguien.Necesita.Ayuda.Beber"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows107 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Casa"
text <- "Último.Año.Por.Seguridad.Ha.Pensado.Cambiarse.Casa"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows108 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad"
text <- "Último.Año.Por.Seguridad.Ha.Pensado.Cambiarse.Ciudad"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows109 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Ha.Pensado.Cambiarse.Estado"
text <- "Último.Año.Por.Seguridad.Ha.Pensado.Cambiarse.Estado"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows110 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Ha.Pensado.Dejo.Salir.Noche"
text <- "Último.Año.Por.Seguridad.Ha.Pensado.Dejó.Salir.Noche"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows111 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Dejo.De.Salir.Caminar"
text <- "Último.Año.Por.Seguridad.Dejó.De.Salir.Caminar"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows112 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ultimo.Ano.Por.Seguridad.Impidio.Ninos.Salieran.Calle"
text <- "Último.Año.Por.Seguridad.Impidió.Niños.Salieran.Calle"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows113 <- renderUI({ if(input$selec != "Percepción de Seguridad"){ Text <- "Ult.Ano.Por.Seguridad.Evito.Relacionarse.Nuevas.Personas"
text <- "Último.Año.Por.Seguridad.Evitó.Relacionarse.Nuevas.Personas"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows114 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Ultimo.Ano.Por.Seguridad.Dejo.De.Visitar.Parientes.Amigos"
text <- "Último.Año.Por.Seguridad.Dejó.De.Visitar.Parientes.Amigos"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows115 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Ultimo.Ano.Por.Seguridad.Dejo.Usar.Taxi"
text <- "Último.Año.Por.Seguridad.Dejó.Usar.Taxi"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows116 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Ultimo.Ano.Por.Seguridad.Dejo.De.Llevar.Mucho.Efectivo"
text <- "Último.Año.Por.Seguridad.Dejó.De.Llevar.Mucho.Efectivo"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows117 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Ultimo.Ano.Por.Seguridad.Dejo.De.Usar.Joyas"
text <- "Último.Año.Por.Seguridad.Dejó.De.Usar.Joyas"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows118 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Cuida.Vigila.Bien"
text <- "Policía.Cuida.Vigila.Bien"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows119 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Comete.Abusos"
text <- "Policía.Comete.Abusos"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows120 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Acude.Llamados"
text <- "Policía.Acude.Llamados"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows121 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Pide.Mordidas"
text <- "Policía.Pide.Mordidas"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows122 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Hace.Rondines"
text <- "Policía.Hace.Rondines"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows123 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Policia.Comete.Delitos"
text <- "Policía.Comete.Delitos"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows124 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Confianza.Que.Tiene.En.La.Policia"
text <- "Confianza.Que.Tiene.En.La.Policía"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows125 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Confianza.Que.Tiene.En.Ministerio.Publico"
text <- "Confianza.Que.Tiene.En.Ministerio.Público"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec,text) } })

output$rows126 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Confianza.Que.Tiene.En.La.Institucion.Educativa.De.Zona"
text <- "Confianza.Que.Tiene.En.La.Institución.Educativa.De.Zona"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows127 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Confianza.Que.Tiene.En.Su.Presidente.Municipal"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows128 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Confianza.Que.Tiene.En.El.Gobernador"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows129 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trabajo.De.La.Policia"
text <- "Calificación.Trabajo.De.La.Policia"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows130 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trabajo.Del.Ministerio.Publico.Para.Denunciar"
text <- "Calificación.Trabajo.Del.Ministerio.Público.Para.Denunciar"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows131 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trabajo.De.Empleados.De.Gobierno"
text <- "Calificación.Trabajo.De.Empleados.De.Gobierno"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })


output$rows132 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trabajo.Presidente.Municipal"
text <- "Calificación.Trabajo.Presidente.Municipal"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows133 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trabajo.Del.Gobernador"
text <- "Calificación.Trabajo.Del.Gobernador"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows134 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trato.Que.Da.La.Policia"
text <- "Calificación.Trato.Que.Da.La.Policia"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows135 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trato.Que.Da.Ministerio.Publico.Para.Denunciar"
text <- "Calificación.Trato.Que.Da.Ministerio.Público.Para.Denunciar"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows136 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trato.Recibe.De.Los.Empleados.De.Gobierno"
text <- "Calificación.Trato.Recibe.De.Los.Empleados.De.Gobierno"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows137 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trato.Recibe.De.Su.Presidente.Municipal"
text <- "Calificación.Trato.Recibe.De.Su.Presidente.Municipal"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows138 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Calificacion.Trato.Recibe.Del.Gobernador"
text <- "Calificación.Trato.Recibe.Del.Gobernador"  
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows139 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Num.Personas.Viven.Esta.Casa"
text <- "Núm.Personas.Viven.Esta.Casa"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows140 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Cuantos.Hombres"
text <- "Cuantos.Hombres"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows141 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Cuantas.Mujeres"
text <- "Cuantas.Mujeres"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows142 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Cuantos.Menores"
text <- "Cuántos.Menores"
showvar <- input$show_vards
selec <- input$selec
secc3(Text, showvar, selec, text) } })

output$rows143 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "La.Madre.De.Los.Menores.Vive.En.Casa"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows144 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "El.Padre.De.Los.Menores.Vive.En.Casa"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows145 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Esta.Casa.Es.Propia"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })


output$rows147 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Tiempo.Viviendo.En.Q.Roo"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })

output$rows148 <- renderUI({ if(input$selec != "Percepción de Seguridad"){Text <- "Tiempo.Viviendo.En.Esta.Casa"
showvar <- input$show_vards
selec <- input$selec
secc2(Text, showvar, selec) } })







  
  
  
  
  
  
  
  
  
  
  
}



 