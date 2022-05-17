


Ho2C_10_TestRegLogSimple_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho2C_10_TestRegLogSimple_SERVER <- function(input, output, session, 
                                    minibase, 
                                    decimales,
                                    control_ejecucion,
                                    alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  
  
  ##################################################
  
  
  
  
  # # # # #
  # 2C - 07 - Test de Homogeneidad de Varianzas de Fisher
  
  
  
  if (1 == 2) {
  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
               # Seleccion del valor bajo H0
               numericInput(inputId = ns("valor_bajo_ho"),
                            label = "Media poblacional (Valor esperado bajo hipótesis): ",
                            min = NA,  max = NA, step = 0.01, value = 0)
        ),
        column(4,
               
               # Seleccion del tipo de prueba
               radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
                            choices = c("Bilateral" = "two.sided",
                                        "Unilateral izquierda" = "less",
                                        "Unilateral derecha" = "greater")
               )
        )
        
        
      )
    )
    
    
    
  })
  }
  
  
  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
 #   if(is.null(input$tipo_prueba_ho)) return(NULL)
  #  if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    
    RegLogGeneral( base = minibase(),
                   columnas = c(1,2),
                   decimales = decimales(),
                   alfa = alfa(),
                   valor_x = input$valor_nuevo)
    
    
    
    
    
  })
  # #######################################################
  # 
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = TRUE, digits=decimales(), align = "c",{
    
    The_Test()$"Tabla Regresion Redondeada y completa"
    
  }))
  
  # Grafico Regresion
  observe( output$grafico_regresion <- renderPlot({
    
    GraficoRegLog(base = minibase(),
                  columnas = c(1,2),
                  decimales = decimales(),
                  alfa = alfa(),
                  logic_obs = input$logic_obs,
                  logic_esp = input$logic_esp,
                  logic_funcion = input$logic_funcion,
                  col_obs = input$color_obs,
                  col_esp = input$color_esp,
                  col_funcion = input$color_funcion,
                  logic_prediccion = input$logic_nuevo,#T,
                  valor_x = input$valor_nuevo)
    
  }))


  # Frase 1: Sobre AIC
  observe(output$frase_aic <- renderUI({
    HTML(paste0("El valor de IAC es: ", The_Test()$AIC, "."))
  }))
  

  # Frase 2: Odd Ratio
  observe(output$frase_odd_ratio <- renderUI({
    or <- The_Test()$"Odd ratio redondeado"
    pendiente <- The_Test()$"Tabla Regresion Redondeada y completa"[2,1]
    
    frase <- paste0("Odd Ratio = exp(", pendiente, ") = ", or)
    HTML(frase)
  }))
  
  
  # Frase 3: Ordenada
  observe(output$frase_ordenada <- renderUI({
    HTML(The_Test()$"Frase para la ordenada")
  }))
  
  
  # Frase 4: Pendiente
  observe(output$frase_pendiente <- renderUI({
    HTML(The_Test()$"Frase para la pendiente")
  }))
  
  
  # Frase 5: Frase Prediccion
  observe(output$frase_prediccion <- renderUI({
    HTML(The_Test()$"Frase Predicho")
  }))
  
  # Frase 6: Frase Hipotesis Pendiente
  observe(output$"frase_juego_hipotesis_pendiente" <- renderUI({
    HTML(The_Test()$"HipotesisPendiente")
  }))
  
  # Frase 7: Frase Prediccion
  observe(output$"frase_juego_hipotesis_ordenada" <- renderUI({
    HTML(The_Test()$"HipotesisOrdenada")
  }))
  
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2("Test de Regresión Logística Simple"),
      "Nota: para la utilización del test de Regresión Lineal Simple la variable Y debe tener solo dos valores.", 
      br(),
      br(),
      h3("Juego de Hipótesis de la Pendiente"),
      htmlOutput(ns("frase_juego_hipotesis_pendiente")),
      br(),
      h3("Juego de Hipótesis de la Ordenada"),
      htmlOutput(ns("frase_juego_hipotesis_ordenada")),
    #  h3("Elecciones del usuario"),
    #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      # h3("Juego de Hipótesis"),
      # htmlOutput(ns("frase_juego_hipotesis")),
      # br(),
      h3("Tabla Resumen del test de Regresión Logística Simple"),
      tableOutput(ns("tabla_resumen")),
      br(), br(),
      h3("Frase de la Pendiente"),
      htmlOutput(ns("frase_pendiente")),
      br(), br(),
      h3("Frase de la Ordenada"),
      htmlOutput(ns("frase_ordenada")),
      br(), br(),
      h3("Odd Ratio"),
      htmlOutput(ns("frase_odd_ratio")),
      br(), br(),
      h3("Ajuste del modelo"),
      htmlOutput(ns("frase_aic")),
      br(), br(),
    
      # h3("Frases y conclusiones"),
      # htmlOutput(ns("frase_estadistica")),
      # br(), br()
      h3("Gráfico de Regresión Logística Simple"),
      plotOutput(ns("grafico_regresion")),
    fluidRow(
      column(2,
      h3("Agregar al gráfico:"),
      checkboxInput(ns("logic_obs"), "Observados", TRUE),
      checkboxInput(ns("logic_esp"), "Esperados", TRUE),
      checkboxInput(ns("logic_funcion"), "Función", TRUE)
      ),
      column(3,
             h3("Elección de colores"),
             colourpicker::colourInput(inputId = ns("color_obs"),
                                       label = "Color valores observados", 
                                       value = "red"),
             br(),br(),
             colourpicker::colourInput(inputId = ns("color_esp"),
                                       label = "Color valores observados", 
                                       value = "green"),
             br(),br(),
             colourpicker::colourInput(inputId = ns("color_funcion"),
                                       label = "Color valores observados", 
                                       value = "black")
             ),
      column(1),
      column(6,
             h3("Agregar predicción"),
             checkboxInput(ns("logic_nuevo"), "Agregar predicción", FALSE),
             br(),br(),
             sliderInput(ns("valor_nuevo"), "Valor a predecir:",
                         min = min(minibase()[,1]), max = max(minibase()[,1]), 
                         value = mean(minibase()[,1], step = 0.01, width = '400px')
             ),
             htmlOutput(ns("frase_prediccion"))
          ),
    ),
      br()#
    )
    
  })
  
  
  
  
  
  
  
}


