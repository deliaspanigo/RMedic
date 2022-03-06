


KM_SobrevidaGeneral_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
KM_SobrevidaGeneral_SERVER <- function(input, output, session, 
                                       minibase, 
                                       decimales,
                                       alfa,
                                       control_ejecucion) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  
  
  
  

  
 
  
 
  
 
  
  
  
  
  
  

  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
 
  
 
  
  
  
  
  
 
  
 
  # Salida de colores
  output$MODcolor_KM_general <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    label_armado <- "Color..."
    cantidad_colores <- 1 
    colores_internos <- "blue"
    
    lapply(1:cantidad_colores, function(i) {
      
      nombre_input <- paste("col_KM_general", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  colores_usuario_KM_General <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Solo un color en KM General
    cantidad <- 1
    
    # Creamos un vector vacio
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){ 
      nombre_input <- paste("col_KM_general", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    return(mis_colores)
  })
  
  
  output$tablaKM_General <- renderTable({
    
    KM_Tabla_General(base = minibase(), alfa = alfa())[[1]]
    
  })
  
  
  output$graficoKM_General <- renderPlot({
    
    # https://r-charts.com/es/r-base/ejes/
    
    objeto_KM <- KM_Tabla_General(base = minibase(), alfa = alfa())[[2]]
    
    if (is.null(objeto_KM)) return(NULL)
    if (is.null(colores_usuario_KM_General())) return(NULL)
    
    plot(objeto_KM, conf.int = input$agregado01, 
         mark.time = TRUE, lty = c(1,3,3), 
         col = colores_usuario_KM_General(), 
         xlab= "Tiempo", ylab= "Probabilidad de Sobrevida", 
         main= "Sobrevida General", yaxt = "n")
    
    axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 1) 
    
    
    
  })
  
  
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      h2("Tabla Resumen de Sobrevida Genetal de Kaplan-Meier"),
      tableOutput(ns("tablaKM_General")), br(), br(),
      
      fluidRow(
        h2("Gráfico de Sobrevida General de Kaplan-Meier"),
        column(6,
               plotOutput(ns("graficoKM_General")), br(), br(),
        ),
        column(4, 
               # Color
               uiOutput(ns("MODcolor_KM_general")),
               br(),
               
               #Intervalos
               checkboxInput(inputId = ns("agregado01"),
                             label = "Agregar intervalo de confianza",
                             value = FALSE))
      )
      
     
    )
  })
  
}


