library(shiny)


library(shinycssloaders)

# header
        header <- dashboardHeader( title="Analisis Exploratorio")


# Menu lateral 
        sidebar <- dashboardSidebar(
                sidebarMenu(id="tabs",
                        tags$head(includeCSS("style.css")),
                # Dashboard items
                        menuItem(text = "Inicio",  tabName = "inicio", icon = icon("home")),
                        menuItem(text = "Graficas", tabName = "graficas", icon = icon("chart-bar")), 
                                conditionalPanel(condition = "input.tabs == 'graficas'", 
                                                id="tabsS",
                                        selectInput(inputId='tipomapa', label = h3('Tipo de visualizacion grafica:'), choices = c("Barras" = "PO", "Polar" = "BA"  )),
                                        selectInput(inputId='seguridad1', label = h3('Graficas:'),choices = c("Situacion Vivienda" = "V1P1R1" , "Adquisicion Vivienda" = "V1P4R1"," Huracanes " = "V1H1")), 
                                        selectInput(inputId='seguridad2', label = h3('Nube de palabras:'),choices = c("Accion Huracanes" = "HU" , "Accion Inundaciones" = "IN" )) 
                                ),
                        menuItem(text = "Mapas",  tabName = "mapas", icon = icon("map-marker-alt"))
                        
                ))

# body
        body <- dashboardBody(
        tabItems(
                 # First tab content
                        tabItem(tabName = "inicio",
                                fluidRow(
                                        column(12,
                                                h2("Análisis Exploratorio de Estudios Socio-Económicos y de Percepción de Seguridad en el Municipio de Isla Mujeres.",style="color:#4F5152"),
                                                br(),
                                                infoBox("Población y migración",108 , icon=icon("user-alt"), color = "light-blue", fill = TRUE),
                                                infoBox("Socieconimico y Ambiental",55, icon=icon("seedling"), color = "olive", fill = TRUE),
                                                infoBox("Percepción de seguridad",376, icon=icon("eye"),color = "aqua", fill = TRUE)              
                                        ),
                                        br(), br(), br(), br(),br(), br(), br(),br(), br(), br(), br(),br(), br(), br(),
                                         column(12,
                                                wellPanel(
                                                        HTML(" <h2><b>Características sobre población y migración.</b></h2><h3>  Zona Urbana Isla Mujeres</h3><h4>Enfoque exclusivo a la percepción de seguridad en la Zona Continental de Isla Mujeres tomando los resultados de ambos conjuntos de datos realizados por diferentes instituciones.<br> Enfoque:<br> <ul><li>Economico</li><li>Social</li><li>Vivienda</li><li>Apreciación de encuestador </li> <br><br></h4>")
                                                )
                                        ),
                                        column(12,
                                                wellPanel(
                                                        
                                                      HTML(" <h2><b>Diagnóstico socio económico y ambiental.</b></h2><h3>  Salinas, Isla Mujeres.</h3><h4>Estudio enfocado en las colonias que colindan con las Salinas localizadas en el municipio de Isla Mujeres. <br> Enfoque:<br> <ul><li>Economico</li><li>Social</li> <li>Ambiental</li> <br><br></h4>")
                                                )
                                        ),
                                            column(12,
                                                wellPanel(
                                                   HTML(" <h2><b>Estudio de percepción de seguridad.</b></h2><h3>  Cancún, QRoo.</h3><h4> <br> Enfoque:<br> <ul><li>Percepción de seguridad </li><br><br></h4>")     
                                                   
                                                )
                                        ),
                                    
                                )
                        ),

                # First tab content
                        tabItem(tabName = "graficas",
                                fluidRow(
                                        column(5, 
                                               h2("Graficas")
                                        ),
                                        column(12,
                                                plotOutput("plot1", dblclick = "plot1_dblclick", brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE ) ),
                                                h2("Nube de palabras"),
                                                plotOutput("plot2")
                                        )
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
                                        h1("Análisis Exploratorio", style="color:#045a8d"),
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
                )#tabkl
        ))#FIN




# Create the UI using the header, sidebar, and body
ui <- dashboardPage(skin ="blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)
