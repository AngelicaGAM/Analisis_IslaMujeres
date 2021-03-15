library(shinycssloaders)
library(shinyWidgets)
# header
        header <- dashboardHeader( title="Analisis Exploratorio")
        varsx <- c("Origen", "Escolaridad", "Puesto","Trabajo")
        varsy <- c("Puesto","Trabajo","Ingreso_sem","Escolaridad")
        varsz <- c("Ninguno", "Edad", "Sexo")

# Menu lateral 
        sidebar <- dashboardSidebar(
                sidebarMenu(id="tabs",
                        tags$head(includeCSS("style.css")),
                # Dashboard items
                        menuItem(text = "Inicio",  tabName = "inicio", icon = icon("home")),
                        menuItem(text = "Graficas", tabName = "graficas", icon = icon("chart-bar")), 
                                conditionalPanel(condition = "input.tabs == 'graficas'", 
                                                id="tabsS",
                                        selectInput(inputId='tipomapa', label = h3('Estudio:'),  choices = c("Percepcion de seguridad" = "PS", "Socioeconómico y ambiental" = "IS" ,"Población y migración" = "EJ"), selected = "EJ")
                                         
                                              
                                                

                                       # selectInput(inputId='seguridad1', label = h3('Graficas:'),choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1", " Inundaciones " = "V1I1")), 
                                        #selectInput(inputId='seguridad2', label = h3('Nube de palabras:'),choices = c("Accion Huracanes" = "HU" , "Accion Inundaciones" = "IN" )) 
                                ),
                        menuItem(text = "Mapas",  tabName = "mapas", icon = icon("map-marker-alt")),
                        menuItem(text = "Mosaicos",  tabName = "mosaics", icon = icon("chart-bar")),
                        conditionalPanel(
                                condition = "input.tabs == 'mosaics'",
                        
                                selectInput("var1","Primera variable de cruce",varsx,selected = "Origen"),
                        
                                selectInput("var2","Segunda variable de cruce",varsy,selected = "Puesto"
                                ), 
                                selectInput("var3","Tercera variable de cruce",varsz
                                )
                        
                        )
                        
                ))

# body
        body <- dashboardBody(
        tabItems(
                 # First tab content
                        tabItem(tabName = "inicio",
                                fluidRow(
                                        column(12,
                                         wellPanel(
                                        #'arg' should be one of “default”, “primary”, “warning”, “danger”, “success”, “royal”
                                              HTML(" <h2><b>Estudios Socio-Económicos, Percepción de Seguridad y Características sobre población y migración. </b></h2>"),
                                             # h1("Estudios Socio-Económicos, Percepción de Seguridad y Características sobre población y migración. "),
                                                br(),
                                                infoBox( "Población y migración",377 , icon=icon("user-alt"), color = "light-blue", fill = TRUE ),
                                                infoBox("Socieconimico y Ambiental",55, icon=icon("seedling"), color = "olive", fill = TRUE),
                                                infoBox("Percepcion de seguridad",376, icon=icon("eye"),color = "aqua", fill = TRUE),
                                                br(), br(), br(), br(),br(), br(), br(),
                                        )

                                        ),
                                        br(), br(), br(), br(),br(), br(), br(),
                                         
                                                        column(4,
                                                                wellPanel(
                                                                        HTML(" <h2><b>Características sobre población y migración.</b></h2><h3>  Zona Urbana Isla Mujeres</h3><h4>Enfoque exclusivo a la percepción de seguridad en la Zona Continental de Isla Mujeres tomando los resultados de ambos conjuntos de datos realizados por diferentes instituciones.<br> <br>Enfoque:<br> <ul><li>Economico</li><li>Social</li><li>Vivienda</li><li>Apreciación de encuestador </li> <br><br></h4>"),
                                                                         actionBttn(inputId = "PO1", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm")
                                                                )
                                                        ),
                                       
                                                        column(4,
                                                                wellPanel(
                                                                        
                                                                HTML(" <h2><b>Diagnóstico socio económico y ambiental.</b></h2><h3>  Salinas, Isla Mujeres.</h3><h4>Estudio enfocado en las colonias que colindan con las Salinas localizadas en el municipio de Isla Mujeres. <br> <br>Enfoque:<br> <ul><li>Economico</li><li>Social</li> <li>Ambiental</li> <br><br></h4>"),
                                                                actionBttn(inputId = "PO2", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm") 
                                                                )
                                                        ),
                                     
                                                        column(4,
                                                                wellPanel(
                                                                HTML(" <h2><b>Estudio de percepción de seguridad.</b></h2><h3>  Cancún, QRoo.</h3><h4> <br> <br>Enfoque:<br> <ul><li>Percepción de seguridad </li><br><br></h4>") ,
                                                                actionBttn(inputId = "PO3", label = "Cuestionario", style = "fill", color = "danger", icon = icon("poll-h"), size = "sm") 
                                                                
                                                                )
                                                        ),
                                        
                                    
                                )
                        ),

                # First tab content
                        tabItem(tabName = "graficas",
                                fluidRow(
                                       
                                         column(12, 
                                                wellPanel(
                                                        h1(textOutput("Analisis Exploratorio")),
                                                        h2(textOutput("TipoestudioG"), align = "center"),
                                                        h3(textOutput("localiz"), align = "left"),
                                         )),            
                                              
                                        column(6, 
                                                  wellPanel(
                                                          selectInput(inputId='enfoque', label = h3('Enfoque:'), choices= c("Datos generales del encuestado" = "DG", "Datos familiares" = "DF","Datos económicos" = "DE", "Identidad y Comunidad" = "ID" , "Vivienda" = "VI", "Apreciación del encuestador" = "AE"), selected = "VI") 
                                                         
                                        )), 
                                         column(6, 
                                                  wellPanel(
                                                          
                                                          selectInput(inputId='pregunta', label = h3('Graficas:'),choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1", " Inundaciones " = "V1I1"), selected = "V1P1R1")
                                        )),      
                                            
                                        
                                                    ##-- Visualizar ----
                                               # column(width = 2, style = "padding-top: 25px;",
                                                #        actionBttn(inputId = "generar_plot", 
                                                 #               label = "Buscar", 
                                                  #              style = "fill", 
                                                   #             color = "success", 
                                                    #            icon = icon("check"), size = "sm") 
                                                #)
                                      
                                  
                                        column(width = 10,
                                                           br(),
                                                           withSpinner(plotlyOutput("plot1"), type = 6)
                                                  ),
                                   

                                     
                                )
                        ),
        
                # Second tab content
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
                                        selectInput(inputId='showmapa', label = h3('Estudio:'), choices = c("Todos" = "ALL" ,"Percepcion de seguridad" = "PS", "Estudio Socioeconómico y ambiental" = "IS" ,"Población y migración" = "EJ")),
                                        plotOutput("plot3", height = 190, width = 330)                    
                                )),
                                           
                ),

                # third tab content
                tabItem(tabName = "mapa2",
                        div(class="outer2",
                        
                             
                         
                          
                          
                          
                        )#div
                ),#tabkl
                tabItem(tabName = "mosaics",
                        plotOutput("mosaico1", height = 500, width = "100%")
                )
        ))#FIN




# Create the UI using the header, sidebar, and body
ui <- dashboardPage(skin ="blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)
