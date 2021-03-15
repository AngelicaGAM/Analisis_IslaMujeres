# server 

xdf  <- read.csv("./CSV/ps/x.csv", header = TRUE, sep= ",",strip.white = TRUE,na.strings = "EMPTY", encoding = "UTF-8")
server <- function(input, output, session) {
  source("G.R", local=TRUE)
  source("mapas.R", local=TRUE)
  source("tablas.R")
  
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