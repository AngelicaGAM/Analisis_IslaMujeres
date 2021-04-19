library(shinydashboard)
library(plotly)
library(shiny)
library(ggplot2)
library(leaflet)
library(RColorBrewer)



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



# header
header <- dashboardHeader( title="Analisis Exploratorio")
varsx <- c("Origen", "Escolaridad", "Puesto","Trabajo")
varsy <- c("Puesto","Trabajo","Ingreso_sem","Escolaridad")
varsz <- c("Ninguno", "Edad", "Sexo")

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem(text = "Inicio",  tabName = "inicio", icon = icon("home")),
              menuItem(text = "Graficas", tabName = "graficas", icon = icon("chart-bar")), 
              menuItem(text = "Mosaicos",  tabName = "mosaics", icon = icon("th")),
              conditionalPanel(
                condition = "input.tabs == 'mosaics'",
                selectInput("var1","Primera variable de cruce",varsx,selected = "Origen"),
                selectInput("var2","Segunda variable de cruce",varsy,selected = "Puesto"), 
                selectInput("var3","Tercera variable de cruce",varsz)
              ),
              menuItem(text = "Mapas",  tabName = "mapas", icon = icon("map-marker-alt")),
              menuItem(text = "Asociación",  tabName = "asociation", icon = icon("diagnoses")),
              conditionalPanel(
                
                br(),
                selectizeInput("show_vars", "Campos posibles a elegir:",
                               c("Sexo"="Info.Sexo",
                                 "Edad"="Info.Edad","Calle"="Direccion.Calle","N.Exterior"="Direccion.Numero.Exterior","N.Interior"="Direccion.Numero.Interior","Mz"="Direccion.Manza0","Smz"="Direccion.Super.Manza0","Colonia"="Direccion.Colonia","Codigo.P"="Direccion.Codigo.Postal","Num.Adultos"="V01_Casa_Adultos","Num.Ninos"="V02_Casa_Ninos","Cuantos.Kinder"="DF_kinder",
                                 "Cuantos.Primaria"="DF_primaria","Cuantos.Secundaria"="DF_secundaria","Cuantos.Bachillerato"="DF_bachillerato","Cuantos.Licenciatura"="DF_licenciatura",#"Cuantos.Posgrado"="DF_posgrado",
                                 "Autobus"="DF_tr_autobus","Colectivo"="DF_tr_colectivo","Taxi"="DF_tr_taxi","Mototaxi"="DF_tr_mototaxi",
                                 "Moto"="DF_tr_moto","Auto"="DF_tr_auto","Mercado.Col"="DF_v_col","Abarrote"="DF_v_abarrote","Super"="DF_v_super","Tienda.conveniencia"="DF_v_conv","Plaza"="DF_v_plaza","Emergencia.En.Casa"="DF_urg_casa","Emergencia.Medico.Part.Comunidad"="DF_urg_partcom",
                                 "Emergencia.Medico.Part.Cancun"="DF_urg_partcan","Emergencia.Hospital"="DF_urg_hosp","Emergencia.CruzR"="DF_urg_cruz","Emergencia.Farmacia"="DF_urg_farmacia","Emergencia.Otro"="DF_urg_otro","Colonia.Con.Parque"="DF_a_parque","Colonia.Con.Uni.Dep"="DF_a_unidad",	
                                 "Colonia.Con.Jardin"="DF_a_jardines","Colonia.Con.Centro.Comunitario"="DF_a_casa","Colonia.Con.Biblio"="DF_a_biblioteca","Colonia.Con.Otro"="DF_a_otro","Cuantas.Personas.Trabajan"="DE01_hogar_trabajan","Jefe.Trabajo"="DE02_trabajo","Jefe.Puesto"="DE03_puesto",
                                 "Ingreso.Sem"="DE05_ingreso_sem","Jefe.Escolaridad"="DE06_esc_jefe","Cuantos.Sin.Ingreso"="DE07_sIngreso","Tiempo.A.Trabajo"="DE08_tmp","Colonia.Trabajo"="DE09_col","Municipio.Trabajo"="DE10_mun", "Estado.Origen"="IyC02_Estado_Origen",
                                 "Tiempo.En.Isla"="IyC03_Tiempo","Motivo.Localidad.Parientes"="IyC04_Motivo_Localidad_Parientes","Motivo.Localidad.Amigos"="IyC04_Motivo_Localidad_Amigos","Motivo.Localidad.Trabajo"="IyC04_Motivo_Localidad_Trabajo","Motivo.Localidad.Negocio"="IyC04_Motivo_Localidad_Negocio",
                                 "Motivo.Localidad.Oportunidad"="IyC04_Motivo_Localidad_Oportunidad","Motivo.Localidad.Otro"="IyC04_Motivo_Localidad_Otro","Acudo.Vecinos"="IyC05_Acudo_Vecinos","Acudo.Familia"="IyC05_Acudo_Familia","Acudo.Autoridad"="IyC05_Acudo_Autoridad","Acudo.Iglesia"="IyC05_Acudo_Iglesia",	
                                 "Acudo.Otro"="IyC05_Acudo_Otro","Religion"="IyC06_Religion",
                                 #"Costumbres"="IyC07_Costumbres",
                                 "Ventajas.Casa"="IyC08_Ventajas_Casa","Ventajas.Trabajo"="IyC08_Ventajas_Trabajo","Ventajas.Familia"="IyC08_Ventajas_Familia","Ventajas.Tiempo"="IyC08_Ventajas_Tiempo",
                                 
                                 "Ventajas.Tranquilo"="IyC08_Ventajas_Tranquilo","Ventajas.Seguro"="IyC08_Ventajas_Seguro","Ventajas.Otro"="IyC08_Ventajas_Otro","Emigrar"="IyC09_Emigrar","Pertenencia"="IyC10_Pertenencia","Frec.Visita.Cabecera_Isla"="IyC11_Frecuencia_Cabecera_Isla","Motivo.Viaja.Asuntos.Admin"="IyC12_Motivo_Viaja_Asuntos_Admin",
                                 "Motivo.Viaja.Pago.Servicio"="IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS","Motivo.Viaja.Trabajo"="IyC12_Motivo_Viaja_Trabajo","Motivio.Viaja.Recreacion"="IyC12_Motivio_Viaja_Recreacion","Motivo.Viaja.Familia"="IyC12_Motivo_Viaja_Familia","Motivo.Viaja.Otro"="IyC12_Motivo_Viaja_Otro",
                                 "La.Vivienda.Es"="VI01_La_vivienda_es","Vivienda.Contado"="VI04_Contado","Vivienda.Herencia"="VI04_Herencia","Vivienda.Mensual"="VI04_Mensual","C.Propia.Mensual"="VI05_CPropiaMensual","C.Propia.Adquirida"="VI06_CPropiaAdquirida",
                                 #"Falta.De.Servicos"="VI07_FALTA_DE_SERVICIOS",
                                 "Pasado.Inundaciones"="VI08NINUNDACIONES",
                                 "Pasado.Huracan"="VI09PeHuracan","Sabe.Zo.Riesgo"="VI14SabeZo0Riesgo","Sabe.Afectaciones"="VI15SabeAfectaciones","Obs.AreasVerdes"="AE1_AreasVerdes","Obs.Banquetas"="AE3_Banquetas","Obs.Luminarias"="AE4_Lumi0rias","Obs.Transporte"="AE5_Transporte",
                                 "Obs.Patrullas"="AE6_Patrullas","Obs.Lotes"="AE7_Lotes","Obs.PeSeguridad"="AE9_PeSeguridad","Obs.PeComodidad"="AE10_PeComodidad","Obs.PeRiesgo"="AE11_PeRiesgo")
                               , selected = "Info.Sexo" , multiple = TRUE),
                
                condition = "input.tabs == 'asociation'",
                sliderInput('sup', "Frecuencia:", min = 0.001, max = 1, value = 0.25, step = 0.005),
                
                sliderInput('conf', 'Confianza:', min = 0.01, max =1, value = 0.25, step = 0.005),
                
                sliderInput('len', 'Mín. Combinación  reglas:', min = 1, max =15, value = 2, step = 1),
                
                sliderInput('mlen', 'Máx. Combinación  reglas:', min = 1, max =15, value = 7, step = 1),
                
                sliderInput('time', 'Máximo  de tiempo:', min = 1, max =25, value = 3, step = 1), 
              )
              
  )
)

# body
body <- dashboardBody(
  tabItems(
    #INICIO
    tabItem(tabName = "inicio",
            fluidRow(
              column(12,
                     wellPanel(
                       HTML(" <h2><b>Estudios Socio-Económicos, Percepción de Seguridad y Características sobre población y migración. </b></h2>")     
                     )),    
              column(12,
                      br(),   
                    infoBox( "Población y migración",377 , icon=icon("user-alt"), color = "light-blue", fill = TRUE ),
                    infoBox("Socieconimico y Ambiental",55, icon=icon("seedling"), color = "olive", fill = TRUE),
                    infoBox("Percepcion de seguridad",8701, icon=icon("eye"),color = "orange", fill = TRUE), br(), br(), br(), br(),br(), br(), br(),       
                ),

              column(4, wellPanel(
                HTML(" <h2><b>Características sobre población y migración.</b></h2><h3>  Zona Urbana Isla Mujeres</h3><h4>Enfoque exclusivo a la percepción de seguridad en la Zona Continental de Isla Mujeres tomando los resultados de ambos conjuntos de datos realizados por diferentes instituciones.<br> <br>Enfoque:<br> <ul><li>Economico</li><li>Social</li><li>Vivienda</li><li>Apreciación de encuestador </li> <br><br></h4>"),
                actionBttn(inputId = "popPyM", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm")
              )),
              column(4,wellPanel(             
                HTML(" <h2><b>Diagnóstico socio económico y ambiental.</b></h2><h3>  Salinas, Isla Mujeres.</h3><h4>Estudio enfocado en las colonias que colindan con las Salinas localizadas en el municipio de Isla Mujeres. <br> <br>Enfoque:<br> <ul><li>Economico</li><li>Social</li> <li>Ambiental</li> <br><br></h4>"),
                actionBttn(inputId = "poSyA", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm") 
              )),
              column(4, wellPanel(
                HTML(" <h2><b>Estudio de percepción de seguridad.</b></h2><h3>  Estado de Quintana Roo.</h3><h4> Estudio donde 16,671 encuestas válidas aplicadas en el Estado de Quintana Roo, donde 9,233 son en Benito Juárez/Puerto Morelos y 208 en Isla Mujeres(Isla y Zona Urbana Ejido)<br> <br>Enfoque:<br> <ul><li>Percepción de seguridad </li><br><br></h4>") ,
                actionBttn(inputId = "popC", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm") 
                
              )),     
            )
    ),
    
    # GRAFICAR BARRAS
    tabItem(tabName = "graficas",
            fluidRow(
              wellPanel(h1(textOutput("TipoestudioG"), align = "center")),    
              column(3, wellPanel(
                selectInput(inputId='tipomapa', label = h3('Estudio:'),  choices = c("Percepcion de seguridad" = "PSQ", "Socioeconómico y ambiental" = "IS" ,"Población y migración" = "EJ"), selected = "IS"),
               # selectInput(inputId='tipomapa', label = h3('Estudio:'),  choices = c( "Socioeconómico y ambiental" = "IS" , "Socioeconómico y ambiental" = "PS" ,"Población y migración" = "EJ"), selected = "EJ"),
                  # conditionalPanel(
                  #          condition = "input.tipomapa == 'PSQ'",
                  #          selectInput(inputId='localizPS', label = h3('Ubicación:'),choices = c("Comparativa"= "PSVS", "Isla Mujeres"= "PSI", "Cancun"="PSC", "Ejido"="PSE"), selected = "PSI")
                  # ),
                selectInput(inputId='enfoque', label = h3('Enfoque:'), choices= c("Datos generales del encuestado" = "DG", "Datos familiares" = "DF","Datos económicos" = "DE", "Identidad y Comunidad" = "ID" , "Vivienda" = "VI", "Apreciación del encuestador" = "AE"), selected = "VI"), 
                selectInput(inputId='pregunta', label = h3('Graficas:'),choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1", " Inundaciones " = "V1I1"), selected = "V1P1R1") 
                  
              )), 
              
              column(width = 9,
                     withSpinner(plotlyOutput("plot1"), type = 6)
                     
                     
              ),
              column(width = 12,br(),
                     plotlyOutput("tableGraficas", height = 'auto', width = 'auto')
              )
              
            )
    ),
    
    
    #MAPAS
    tabItem(tabName = "mapas",
            div(class="outer",
                tags$head(includeCSS("style.css")),
                #Mapa
                leafletOutput("mymap", height=600), 
                #Panel movil
                absolutePanel(id = "controls", class = "panel panel-default", 
                              top = 75, right = 300, width = 400, fixed=TRUE, 
                              draggable = TRUE, height = "auto",
                              h1("Analisis Exploratorio", style="color:#045a8d"),
                              h3(textOutput("Tipoestudio"), align = "right"),
                              h3(textOutput("zona"), align = "right"),
                              selectInput(inputId='showmapa', label = h3('Estudio:'), choices = c("Percepcion de seguridad" = "PS", "Estudio Socioeconómico y ambiental" = "IS" ,"Población y migración" = "EJ")),
                              plotOutput("plot3", height = 190, width = 330)                    
                )),
            
    ),
    # MOSAICOS
    tabItem(tabName = "mosaics",
            column(12, wellPanel(
              h2("Características sobre población y migración"),
              h3("Zona Urbana Isla Mujeres"),
            )), 
            plotOutput("mosaico1", height = 700, width = "100%"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            htmlOutput("Expl1"),tags$head(tags$style("#Expl1{font-size: 20px;font-style: italic;}")),
            htmlOutput("Expl2"),tags$head(tags$style("#Expl2{font-size: 20px;font-style: italic;}")),
    ),
    
    
    tabItem(tabName = "asociation",
            tabsetPanel(
              tabPanel("Data", DT::dataTableOutput("data")),
              tabPanel("Summary", verbatimTextOutput("sum")),
              tabPanel('Rules',
                       htmlOutput("txt6"),tags$head(tags$style("#txt6{color:red;font-size: 20px;font-style: italic;}")),
                       verbatimTextOutput("rules")),
              tabPanel('Plot',
                       htmlOutput("txt5"),tags$head(tags$style("#txt5{color:red;font-size: 20px;font-style: italic;}")),
                       
                       fluidRow(align="center", visNetworkOutput('plot',height = 450, width = 800)),
                       br(),
                       br(),
                       htmlOutput("txt0"),tags$head(tags$style("#txt0{font-size: 20px;font-style: italic;}")),
                       br(),
                       uiOutput("rows1"),
                       uiOutput("rows2"),
                       uiOutput("rows3"),
                       uiOutput("rows4"),
                       uiOutput("rows5"),
                       uiOutput("rows6"),
                       uiOutput("rows7"),
                       uiOutput("rows8"),
                       uiOutput("rows9"),
                       uiOutput("rows10"),
                       uiOutput("rows11"),
                       uiOutput("rows12"),
                       uiOutput("rows13"),
                       uiOutput("rows14"),
                       uiOutput("rows15"),
                       uiOutput("rows16"),
                       uiOutput("rows17"),
                       uiOutput("rows18"),
                       uiOutput("rows19"),
                       uiOutput("rows20"),
                       uiOutput("rows21"),
                       uiOutput("rows22"),
                       uiOutput("rows23"),
                       uiOutput("rows24"),
                       uiOutput("rows25"),
                       uiOutput("rows26"),
                       uiOutput("rows27"),
                       uiOutput("rows28"),
                       uiOutput("rows29"),
                       uiOutput("rows30"),
                       uiOutput("rows31"),
                       uiOutput("rows32"),
                       uiOutput("rows33"),
                       uiOutput("rows34"),
                       uiOutput("rows35"),
                       uiOutput("rows36"),
                       uiOutput("rows37"),
                       uiOutput("rows38"),
                       uiOutput("rows39"),
                       uiOutput("rows40"),
                       uiOutput("rows41"),
                       uiOutput("rows42"),
                       uiOutput("rows43"),
                       uiOutput("rows44"),
                       uiOutput("rows45"),
                       uiOutput("rows46"),
                       uiOutput("rows47"),
                       uiOutput("rows48"),
                       uiOutput("rows49"),
                       uiOutput("rows50"),
                       uiOutput("rows51"),
                       uiOutput("rows52"),
                       uiOutput("rows53"),
                       uiOutput("rows54"),
                       uiOutput("rows55"),
                       uiOutput("rows56"),
                       uiOutput("rows57"),
                       uiOutput("rows58"),
                       uiOutput("rows59"),
                       uiOutput("rows60"),
                       uiOutput("rows61"),
                       uiOutput("rows62"),
                       uiOutput("rows63"),
                       # uiOutput("rows64"),
                       uiOutput("rows65"),
                       uiOutput("rows66"),
                       uiOutput("rows67"),
                       uiOutput("rows68"),
                       uiOutput("rows69"),
                       uiOutput("rows70"),
                       uiOutput("rows71"),
                       uiOutput("rows72"),
                       uiOutput("rows73"),
                       uiOutput("rows74"),
                       uiOutput("rows75"),
                       uiOutput("rows76"),
                       uiOutput("rows77"),
                       uiOutput("rows78"),
                       uiOutput("rows79"),
                       uiOutput("rows80"),
                       uiOutput("rows81"),
                       uiOutput("rows82"),
                       uiOutput("rows83"),
                       uiOutput("rows84"),
                       uiOutput("rows85"),
                       uiOutput("rows86"),
                       uiOutput("rows87"),
                       uiOutput("rows88"),
                       uiOutput("rows89"),
                       uiOutput("rows90"),
                       uiOutput("rows91"),
                       uiOutput("rows92"),
                       uiOutput("rows93"),
                       uiOutput("rows94"),
                       uiOutput("rows95"),
                       uiOutput("rows96"),
                       uiOutput("rows97"),
                       uiOutput("rows98"),
                       uiOutput("rows99"),
                       uiOutput("rows100"),
                       uiOutput("rows101"),
                       br(),
                       htmlOutput("txt1"),tags$head(tags$style("#txt1{font-size: 20px;font-style: italic;}")),
                       br(),
                       htmlOutput("txt2"),tags$head(tags$style("#txt2{font-size: 20px;font-style: italic;}")),
                       br(),
                       htmlOutput("txt3"),tags$head(tags$style("#txt3{font-size: 20px;font-style: italic;}")),
                       br(),
                       br(),
                       htmlOutput("txt7"),tags$head(tags$style("#txt7{font-size: 20px;font-style: italic;}")),
                       br(),
                       br(),
                       htmlOutput("txt4"),tags$head(tags$style("#txt4{font-size: 20px;font-style: italic;}")),
                       br(),
                       br()
              )
            )
    )
    
    
  ))#FIN




# Create the UI using the header, sidebar, and body
ui <- dashboardPage(skin ="blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)
