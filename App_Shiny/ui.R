library(shinydashboard)
library(plotly)
library(shiny)
library(ggplot2)
library(leaflet)
library(RColorBrewer)


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
    menuItem(text = "Analisis",  tabName = "analisis", icon = icon("search"))
    
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
    )
  )
)




# Create the UI using the header, sidebar, and body
ui <- dashboardPage(skin ="blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)
