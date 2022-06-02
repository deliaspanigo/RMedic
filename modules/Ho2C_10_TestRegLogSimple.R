


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
  
  control_2c_reglogsimple <- reactive({
    Control01_2C_RegLogSimple(minibase())
  })
  
  # Control interno 01
  control_interno01 <- reactive({
    
    control_interno <- FALSE
    
    if(is.null(control_ejecucion())) control_interno <- FALSE
    if(!is.null(control_ejecucion())) control_interno <- control_ejecucion()
    if(!control_2c_reglogsimple()[[3]]) control_interno <- FALSE
    
    else return(control_interno)
  })
  
  
  
  # Frase 2: Odd Ratio
  observe(output$frase_control_2c <- renderUI({
    # or <- The_Test()$"Odd ratio redondeado"
    # pendiente <- The_Test()$"Tabla Regresion Redondeada y completa"[2,1]
    
    frase <- control_2c_reglogsimple()[[2]]
    HTML(frase)
  }))
  
  
  observe(output$menu_cambios01 <- renderUI({
    
    if(control_interno01()) {
      if(control_2c_reglogsimple()[[1]] == 1) {
    div(
      selectInput(inputId = ns("x0"), 
                  label = "Referencia '0' X", 
                  choices = levels(as.factor(as.character(minibase()[,1]))), multiple = FALSE)
    )
        } else return(NULL)
    } else return(NULL)
    
  }))
  
  
  observe(output$menu_cambios02 <- renderUI({
    
    if(control_interno01()) {
      if(control_2c_reglogsimple()[[1]] <= 2) {
      div(
        selectInput(inputId = ns("y0"), 
                    label = "Referencia '0' Y", 
                    choices = levels(as.factor(as.character(minibase()[,2]))), multiple = FALSE)
      )
        } else return(NULL)
    } else return(NULL)
    
  }))
  
  
  x0_interno <- reactive({
    
    if(control_interno01()) {
      if(control_2c_reglogsimple()[[1]] == 1) {
         input$x0
      } else return(NULL)
    } else return(NULL)
  })


  y0_interno <- reactive({
    if(control_interno01()) {
      if(control_2c_reglogsimple()[[1]] <= 2) {
        input$y0
      } else return(NULL)
    } else return(NULL)
  })
  
  
 
  ##################################################
  
  

   

  
  # observe(output$aver <- renderTable({
  #   
  #   minibase()[c(1:10), ]
  # }))
  
  # # # # #
  # 2C - 07 - Test de Homogeneidad de Varianzas de Fisher
  
  
  
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
                   valor_x = input$valor_nuevo,
                   x0 = x0_interno(),
                   y0 = y0_interno())
    
    
    
    
    
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
                  valor_x = input$valor_nuevo,
                  x0 = x0_interno(),
                  y0 = y0_interno())
    
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
  output$armado_ho_1 <- renderUI({
    

    div(
      h2("Test de Regresión Logística Simple"),
      "Nota: para la utilización del test de Regresión Logística Simple la variable Y debe tener solo dos valores.", 
      br(),
      br(),
      span(htmlOutput(ns("frase_control_2c")), style="color:red")
      # htmlOutput(ns("frase_control_2c"))
      )
  })# ACAAAAAAAAAAAAAAAAAAAAAAAAAA
      
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho_2 <- renderUI({
    
    if(!control_interno01()) return(NULL)
    div(
      h3("Juego de Hipótesis de la Pendiente"),
      htmlOutput(ns("frase_juego_hipotesis_pendiente")),
      br(),
      h3("Juego de Hipótesis de la Ordenada"),
      htmlOutput(ns("frase_juego_hipotesis_ordenada")),
      #  h3("Elecciones del usuario"),
      #  uiOutput(ns("opciones_ho")),
      br(),
      h3("Cambio de Categorías"),
      uiOutput(ns("menu_cambios01")), br(),
      uiOutput(ns("menu_cambios02")), br(),
      
      #input$x0, input$y0, br(),
      #tableOutput(ns("aver")), br(),
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
                           min = min(minibase()[,1]),
                           max = max(minibase()[,1]),
                           value = mean(minibase()[,1],
                                        step = 0.01, 
                                        width = '400px')
               ),
               htmlOutput(ns("frase_prediccion"))
        ),
      ),
      br()#
    )
    
      

  })
      
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    

    div(
      uiOutput(ns("armado_ho_1")), br(),
      uiOutput(ns("armado_ho_2")), br()
    )
    
  })
  
  
  
  
  
  
  
}


