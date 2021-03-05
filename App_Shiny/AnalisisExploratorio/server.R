# server 

server <- function(input, output, session) {
  Sys.setenv(TZ='UTC')
  source("G.R")
  source("mapas.R")
 
  
  output$plot1=renderPlot({
   print(input$seguridad1)
    
    
      if(input$seguridad1=="V1P1R1"){V1P1R1}
      else if(input$seguridad1=="V1P4R1"){V1P4R1}
      else if(input$seguridad1=="V1H1"){V1H1}
   
  
    })
  
  
  output$plot2=renderPlot({
    #print(input$seguridad2)
    if(input$seguridad2=="HU"){HU}
    else if(input$seguridad2=="IN"){IN}
  })

 
  output$plot3=renderPlot({
    #print(input$seguridad2)
    V1P1R2
  })
  
  output$plot4=renderPlot({
    #print(input$seguridad2)
    V1P1R1
  })
  
  
  output$mymap <- renderLeaflet({
    
    mapp(Ejido)
  })
  output$mymap1 <- renderLeaflet({
    
    mapp(Isla)
  })
  output$ana1 <- renderPlot({
    reglas_seleccionadas_df %>%
      head(by = "gini", n = 10) %>%
      plot(method = "graph")
    
  })
  
  output$anaeco1 <- renderPlot({
  # Tipo grafo
  reglas_seleccionadas_eco %>%
    head(by = "gini", n = 10) %>%
    plot(method = "graph")
  
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Ejido
   
   
  }))
  

}