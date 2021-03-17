library(shinydashboard)
library(plotly)
library(shiny)
library(ggplot2)
library(leaflet)
library(RColorBrewer)

varsx <- c("Origen", "Escolaridad", "Puesto","Trabajo")
varsy <- c("Puesto","Trabajo","Ingreso_sem","Escolaridad")
varsz <- c("Ninguno", "Edad", "Sexo")

# header
header <- dashboardHeader(
  title="Analisis Exploratorio de Estudios Socio-Economicos y de Percepcion de Seguridad en el Municipio de Isla Mujeres.",
  titleWidth = 1100
)


# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    # Create two `menuItem()`s, "Dashboard" and "Inputs"
    menuItem(text = "Graficas", tabName = "graficas", icon = icon("chart-bar")), 
    conditionalPanel(
      condition = "input.tabs == 'graficas'", 
      
      selectInput(inputId='tipomapa', label = h3('Tipo de visualizacion grafica:'),
                  choices = c("Barras" = "PO", "Polar" = "BA"  )),
      
      selectInput(inputId='seguridad1', label = h3('Graficas:'),
                  choices = c("Situacion Vivienda" = "V1P1R1" ,
                              "Adquisicion Vivienda" = "V1P4R1" ,
                              " Huracanes " = "V1H1" 
                  )
      ), 
      
      
      selectInput(inputId='seguridad2', label = h3('Nube de palabras:'),
      choices = c("Accion Huracanes" = "HU" , "Accion Inundaciones" = "IN" )) 
      
      
      
    ),
    menuItem(text = "Mapas",  tabName = "mapas", icon = icon("map-marker-alt")),
    menuItem(text = "Analisis",  tabName = "analisis", icon = icon("search")),
    menuItem(text = "Mosaicos",  tabName = "mosaics", icon = icon("chart-bar")),
    menuItem(text = "Asociacion",  tabName = "asociation", icon = icon("diagnoses")),
    
    conditionalPanel(
      condition = "input.tabs == 'mosaics'",
      
      selectInput("var1","Primera variable de cruce",varsx,selected = "Origen"),
      
      selectInput("var2","Segunda variable de cruce",varsy,selected = "Puesto"
      ), 
      selectInput("var3","Tercera variable de cruce",varsz
      )
      
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'asociation'",
      
      
        sliderInput('sup', "Support", min = 0.001, max = 1, value = 0.25, step = 0.005),
        
        sliderInput('conf', 'Confidence', min = 0.01, max =1, value = 0.25, step = 0.005),
        
        sliderInput('len', 'Minimum Rule Length', min = 1, max =15, value = 2, step = 1),
        
        sliderInput('mlen', 'Maximum Rule Length', min = 1, max =15, value = 7, step = 1),
        
        sliderInput('time', 'Maximum Time Taken', min = 1, max =25, value = 3, step = 1),
        
        br(),
        checkboxGroupInput("show_vars", "Campos posibles a elegir:",
                           c("Sexo"="Info.Sexo",
                             "Edad"="Info.Edad","Calle"="Direccion.Calle","N.Exterior"="Direccion.Numero.Exterior","N.Interior"="Direccion.Numero.Interior","Mz"="Direccion.Manza0","Smz"="Direccion.Super.Manza0","Colonia"="Direccion.Colonia","Codigo.P"="Direccion.Codigo.Postal","Num.Adultos"="V01_Casa_Adultos","Num.Ninos"="V02_Casa_Ninos","Cuantos.Kinder"="DF_kinder",
                             "Cuantos.Primaria"="DF_primaria","Cuantos.Secundaria"="DF_secundaria","Cuantos.Bachillerato"="DF_bachillerato","Cuantos.Licenciatura"="DF_licenciatura","Cuantos.Posgrado"="DF_posgrado","Autobus"="DF_tr_autobus","Colectivo"="DF_tr_colectivo","Taxi"="DF_tr_taxi","Mototaxi"="DF_tr_mototaxi",
                             "Moto"="DF_tr_moto","Auto"="DF_tr_auto","Mercado.Col"="DF_v_col","Abarrote"="DF_v_abarrote","Super"="DF_v_super","T.conveniencia"="DF_v_conv","Plaza"="DF_v_plaza","Emer.En.Casa"="DF_urg_casa","Emer.Medico.Part.Comunidad"="DF_urg_partcom",
                             "Emer.Medico.Part.Cancun"="DF_urg_partcan","Emer.Hospital"="DF_urg_hosp","Emer.CruzR"="DF_urg_cruz","Emer.Farmacia"="DF_urg_farmacia","Emer.Otro"="DF_urg_otro","Col.Con.Parque"="DF_a_parque","Col.Con.Uni.Dep"="DF_a_unidad",	
                             "Col.Con.Jardin"="DF_a_jardines","Col.Con.Centro.Comunitario"="DF_a_casa","Col.Con.Biblio"="DF_a_biblioteca","Col.Con.Otro"="DF_a_otro","Cuantas.Personas.Trabajan"="DE01_hogar_trabajan","Jefe.Trabajo"="DE02_trabajo","Jefe.Puesto"="DE03_puesto",
                             "Ingreso.Sem"="DE05_ingreso_sem","Jefe.Escolaridad"="DE06_esc_jefe","Cuantos.Sin.Ingreso"="DE07_sIngreso","Tiempo.A.Trabajo"="DE08_tmp","Colonia.Trabajo"="DE09_col","Municipio.Trabajo"="DE10_mun", "Estado.Origen"="IyC02_Estado_Origen",
                             "Tiempo.En.Isla"="IyC03_Tiempo","Motivo.Localidad.Parientes"="IyC04_Motivo_Localidad_Parientes","Motivo.Localidad.Amigos"="IyC04_Motivo_Localidad_Amigos","Motivo.Localidad.Trabajo"="IyC04_Motivo_Localidad_Trabajo","Motivo.Localidad.Negocio"="IyC04_Motivo_Localidad_Negocio",
                             "Motivo.Localidad.Oportunidad"="IyC04_Motivo_Localidad_Oportunidad","Motivo.Localidad.Otro"="IyC04_Motivo_Localidad_Otro","Acudo.Vecinos"="IyC05_Acudo_Vecinos","Acudo.Familia"="IyC05_Acudo_Familia","Acudo.Autoridad"="IyC05_Acudo_Autoridad","Acudo.Iglesia"="IyC05_Acudo_Iglesia",	
                             "Acudo.Otro"="IyC05_Acudo_Otro","Religion"="IyC06_Religion","Costumbres"="IyC07_Costumbres","Ventajas.Casa"="IyC08_Ventajas_Casa","Ventajas.Trabajo"="IyC08_Ventajas_Trabajo","Ventajas.Familia"="IyC08_Ventajas_Familia","Ventajas.Tiempo"="IyC08_Ventajas_Tiempo",
                             
                             "Ventajas.Tranquilo"="IyC08_Ventajas_Tranquilo","Ventajas.Seguro"="IyC08_Ventajas_Seguro","Ventajas.Otro"="IyC08_Ventajas_Otro","Emigrar"="IyC09_Emigrar","Pertenencia"="IyC10_Pertenencia","Frec.Visita.Cabecera_Isla"="IyC11_Frecuencia_Cabecera_Isla","Motivo.Viaja.Asuntos.Admin"="IyC12_Motivo_Viaja_Asuntos_Admin",
                             "Motivo.Viaja.Pago.Servicio"="IyC12_Motivo_Viaja_Pago_FALTA_DE_SERVICIOS","Motivo.Viaja.Trabajo"="IyC12_Motivo_Viaja_Trabajo","Motivio.Viaja.Recreacion"="IyC12_Motivio_Viaja_Recreacion","Motivo.Viaja.Familia"="IyC12_Motivo_Viaja_Familia","Motivo.Viaja.Otro"="IyC12_Motivo_Viaja_Otro",
                             "La.Vivienda.Es"="VI01_La_vivienda_es","Vivienda.Contado"="VI04_Contado","Vivienda.Herencia"="VI04_Herencia","Vivienda.Mensual"="VI04_Mensual","C.Propia.Mensual"="VI05_CPropiaMensual","C.Propia.Adquirida"="VI06_CPropiaAdquirida","Falta.De.Servicos"="VI07_FALTA_DE_SERVICIOS","Pasado.Inundaciones"="VI08NINUNDACIONES",
                             "Pasado.Huracan"="VI09PeHuracan","Sabe.Zo.Riesgo"="VI14SabeZo0Riesgo","Sabe.Afectaciones"="VI15SabeAfectaciones","Obs.AreasVerdes"="AE1_AreasVerdes","Obs.Banquetas"="AE3_Banquetas","Obs.Luminarias"="AE4_Lumi0rias","Obs.Transporte"="AE5_Transporte",
                             "Obs.Patrullas"="AE6_Patrullas","Obs.Lotes"="AE7_Lotes","Obs.PeSeguridad"="AE9_PeSeguridad","Obs.PeComodidad"="AE10_PeComodidad","Obs.PeRiesgo"="AE11_PeRiesgo")
                           , selected = "Info.Sexo")

      
      
      
    )
    
  )
)

# body
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "graficas",
            fluidRow(
              column(12,
                     #infoBox("Estudio socieconimico",55, icon=icon("leaf"), color = "olive", fill = TRUE, width = 12)
                     infoBox("Percepcion de seguridad",376, icon=icon("lock"),color = "olive", fill = TRUE, width = 12)
              ),
              column(1,),
              column(5,
                     h1("Graficas")
              ),
              column(6,
                     h1("Nube de palabras")
              ),
              
              column(12,
                     box(plotOutput("plot1", height = 500, width = 700)),
                     box(plotOutput("plot2", height = 500, width = 800))
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "mapas",
            column(12,
                leafletOutput("mymap"), 
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 300, bottom = "auto",
                              width = 330, height = "auto",
                              column(12,
                              h2("Analisis Exploratorio"),
                              selectInput(inputId='showmapa', label = h3('Estudio:'),
                              choices = c("Socieconimico" = "SO" ,"Percepcion de seguridad" = "PS")
                              ),
                              selectInput(inputId='showmapaTip', label = h3('Estudio:'),
                                          choices = c("Socieconimico" = "SO" ,"Percepcion de seguridad" = "PS")
                              ),
                              box(plotOutput("plot3", height = 190, width = 250))
                              )
                                  
                                  
                    )),
            column(12,
                   h1("Grafica")
            ),
            column(12,
                   box(plotOutput("plot4", height = 400, width = 700)),
              )
              
            
     ),
    # Second tab content
    tabItem(tabName = "analisis",
            fluidRow(
              column(10,
                     h2("Datos generales"),  
                     box(plotOutput("ana1", height = 550))       ),
              column(10,
                     h2("Datos economicos"),  
                     box(plotOutput("anaeco1", height = 550))
                            )
              
              
              
             
            )
    ),
    
    tabItem(tabName = "mosaics",
            plotOutput("mosaico1", height = 500, width = "100%")
            ),
    
    tabItem(tabName = "asociation",
            tabsetPanel(
              id = 'dataset',
              tabPanel("Data", DT::dataTableOutput("data")),
              tabPanel("Summary", tableOutput("sum")),
              tabPanel('Rules',verbatimTextOutput("rules")),
              tabPanel('Plot', visNetworkOutput('plot',height = 650),
                       br(),
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
                       br(),
                       textOutput("txt1"),tags$head(tags$style("#txt1{font-size: 20px;font-style: italic;}")),
                       textOutput("txt2"),tags$head(tags$style("#txt2{font-size: 20px;font-style: italic;}")),
                       textOutput("txt3"),tags$head(tags$style("#txt3{font-size: 20px;font-style: italic;}")),
                       br(),
                       br(),
                       textOutput("txt4"),tags$head(tags$style("#txt4{font-size: 20px;font-style: italic;}"))
              )
            )
    )
    
  )
)




# Create the UI using the header, sidebar, and body
ui <- dashboardPage(skin ="blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)
