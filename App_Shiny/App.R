source("librerias.R")
source("graficas.R")
source("graficarSE.R")

server <- source("server.R")
ui <- source("ui.R")

shinyApp(ui, server)