# server library(shinyjs)
library(shinyjs)
server <- function(input, output, session) {
  source("librerias.R")
  source("Funciones.R")
  source("G.R")
  source("Tablas.R")
  xdf  <- read.csv("./x.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")

    #infoBox( "Población y migración",108 , icon=icon("user-alt"), color = "light-blue", fill = TRUE ),
    #infoBox("Socieconimico y Ambiental",55, icon=icon("seedling"), color = "olive", fill = TRUE),
    #infoBox("Percepcion de seguridad",376, icon=icon("eye"),color = "aqua", fill = TRUE)  

    observeEvent(input$button, {

        if(input$button %% 2 == 1){
            shinyjs::hide(id = "myBox")
        }else{
            shinyjs::show(id = "myBox")
        }
    })

  # ---------------------------------------------------------------------
  # HOME
  output$box_01 <- renderValueBox({
    entry_01<-108
    box1<-valueBox(value=entry_01
                   ,icon = icon("user-alt",lib="font-awesome")
                   ,width=NULL
                   ,color = "light-blue"
                   ,href="#"
                   ,subtitle=HTML("<b>Población y migración</b>")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
  })

    observeEvent(input$button_box_01, {
    
    output$print1<-renderText({
      print("h2(Características sobre población y migración.")
    })})



  # ---------------------------------------------------------------------
  # ---------------------------------------------------------------------
    V1P1R1  # GRAFICAS 
  
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
    # Can use character(0) to remove all choices
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
    else if(input$showmapa=="PS"){ "Cancún, QRoo." }
    else if(input$showmapa=="IS"){ "Salinas, Isla Mujeres." }
    else if(input$showmapa=="EJ"){ " Zona Urbana Isla Mujeres "}
  })
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