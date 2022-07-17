## Segmento del UI
SideBarDistribucionGeneral_UI <- function(id) {
  ns <- NS(id)
  
  
 
  uiOutput(ns("outMe_general"))
  
  
}



 ## Segmento del server
SideBarDistribucionGeneral_SERVER <- function(input, output, session,
                                              carpeta_distribuciones) {
   
  # NameSpaceasing for the session
  ns <- session$ns
  
   output$outMe_general <- renderUI({
     
     las_distribuciones <- list.files(carpeta_distribuciones)
     
     # Este es el sidebarpanel completo
     div(  
       selectInput(inputId = ns("distribuciones"), 
                   label = 'Qué tipo de Archivo quieres subir?',
                   choices = las_distribuciones, 
                   selectize = T)
     )
   })
   

  la_elegida <- reactive({
     
     if(is.null(input$distribuciones)) return(NULL)
     return(input$distribuciones)
     
   })
   
   # la_salida <- list(distribucionb_elegida = la_elegida)
    la_salida <- la_elegida
    
   #  observe(cat( la_elegida()))
    
    
   return(la_salida)
 }
 
 
########################################################################################


## Segmento del UI
SideBarDistribucionElegida_UI <- function(id) {
  ns <- NS(id)
  
  
  
  uiOutput(ns("outMe_elegida"))
  
  
}

## Segmento del UI
MainPanelDistribucionElegida_UI <- function(id) {
  ns <- NS(id)
  
  MainPanel01_Normal("aver2")
  # renderUI(ns("MainPanelNormal"))
 # uiOutput(ns("outMe_elegida02"))
  
 
}

## Segmento del server
SideBarDistribucionElegida_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(SideBar01_Normal("aver2")) else
      return(NULL)
    

  })
  
  
  # la_elegida <- reactive({
  #   
  #   if(is.null(input$distribuciones)) return(NULL)
  #   return(input$distribuciones)
  #   
  # })
  #
  #  return(la_elegida)
}

ServerDistribucionElegida_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
 # POR AHORA QUEDA VACIO!
  

}


########################################################################################

# Normal 
# # SidePanelNormal
## Segmento del UI
SideBar01_Normal <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    h3("Distribución Normal"),
    selectInput(inputId = ns("color_variable"), 
                label = "Color:",
                choices = c("Naranja" = "orange",
                            "Rojo" = "red",
                            "Verde" = "green",
                            "Azul" = "blue", 
                            "Amarillo" = "yellow", 
                            "Negro" = "black",
                            "Celeste" = "skyblue"), 
                multiple = FALSE),
    br(),
    numericInput(inputId = ns("mu"),
                 label = "Media Poblacional:",
                 min=-10000000000000000000000000,
                 max=10000000000000000000000000,
                 step= 0.01,
                 value= 106),
    numericInput(inputId = ns("sigma_cuadrado"),
                 label = "Varianza Poblacional: ",
                 min=0,  max = 10000000000000000000000000,
                 step= 0.01,  
                 value=64),
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor de la variable" = "original",
                              "Valor Z" = "valor_z",
                              "Probabilidad" = "probabilidad")),
    br(),
    radioButtons(inputId = ns("intervalo"), 
                 label = "Intervalo de cálculo:",
                 choices = c("Menores que..." = "menor",
                             "Mayores que..." = "mayor",
                             "Entre..." = "entre")
                 
    ),
    br(),

                     conditionalPanel('input.opciones == "original"', ns = ns,
                                      conditionalPanel('input.intervalo == "menor"', ns = ns,
                                                       numericInput(inputId = ns("var1"), 
                                                                    label = "Valor de la Variable Original:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 90)),
                                      
                                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                                       numericInput(inputId = ns("var2"),
                                                                    label = "Valor de la Variable Original:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 110)),
                                      
                                      conditionalPanel('input.intervalo == "entre"', ns = ns,
                                                       numericInput(inputId = ns("var3"), 
                                                                    label = "Valor Izquierdo de la Variable Original:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 90),
                                                       numericInput(inputId = ns("var4"),
                                                                    label = "Valor Derecho de la Variable Original:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 110))),  
                     
                     conditionalPanel('input.opciones == "valor_z"', ns = ns,
                                      conditionalPanel('input.intervalo == "menor"', ns = ns,
                                                       numericInput(inputId = ns("z_var1"),
                                                                    label = "Valor Z:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= -1.96)),
                                      
                                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                                       numericInput(inputId = ns("z_var2"),
                                                                    label = "Valor Z:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 1)),
                                      
                                      conditionalPanel('input.intervalo == "entre"', ns = ns,
                                                       numericInput(inputId = ns("z_var3"),
                                                                    label = "Valor Z Izquierdo :", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= -1.96),
                                                       numericInput(inputId = ns("z_var4"),
                                                                    label = "Valor Z Derecho:", 
                                                                    min=-10000000000000000000000000,  
                                                                    max=10000000000000000000000000, step= 0.01, value= 1))  
                     ),
                     conditionalPanel('input.opciones == "probabilidad"', ns = ns,
                                      numericInput(inputId = ns("prob_var1"),
                                                   label = "Probabilidad (Entre 0 y 1):",
                                                   min=0,  max=1, step= 0.01, value= 0.25))
    )
    
  
  
}


MainPanel01_Normal <- function(id){
  
  ns <- NS(id)
  
  
  uiOutput(ns("armado01"))
 
  
}


Server01_Normal_Server <- function(input, output, session,
                                   la_distribucion) {

  # NameSpaceasing for the session
  ns <- session$ns
  
  tabla_salida01 <- reactive({ 
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    color_variable <- input$color_variable
    mu_variable <- input$mu
    sigma_cuadrado_variable <- input$sigma_cuadrado
    sigma_variable <- sqrt(sigma_cuadrado_variable)
    opciones <- input$opciones
    intervalo <- input$intervalo
    
    out <- data.frame(mu_variable, sigma_cuadrado_variable, sigma_variable, opciones, intervalo, color_variable)
    out
    
    })
  
  tabla_salida02 <- reactive({
    
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    color_variable <- input$color_variable
    mu_variable <- input$mu
    sigma_cuadrado_variable <- input$sigma_cuadrado
    sigma_variable <- sqrt(sigma_cuadrado_variable)
    opciones <- input$opciones
    intervalo <- input$intervalo
    
    
    # Parametros
    # sigma <- 1
    # mu    <- 0
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/1000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- marcas_z*sigma_variable + mu_variable
    
    if(1 == 1){
      # Si los valores ingresados son de la variable original
      if(opciones == "original") {
        if(intervalo == "menor"){
          
          if(is.null(input$var1)) return(NULL)
          
          var_izquierdo <- min(marcas_variable)
          var_derecho <- input$var1
          
          z_izquierdo <- min_distribucion
          z_derecho <- (var_derecho - mu_variable)/sigma_variable
          
          la_probabilidad <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
          
        } else
          if(intervalo == "mayor"){
            
            var_izquierdo <- input$var2
            var_derecho <- max(marcas_variable)
            
            z_izquierdo <- (var_izquierdo - mu_variable)/sigma_variable
            z_derecho <- max_distribucion
            
            la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
            
          } else
            if(intervalo == "entre"){
              
              var_izquierdo <- input$var3
              var_derecho <- input$var4
              
              z_izquierdo <- (var_izquierdo - mu_variable)/sigma_variable
              z_derecho <- (var_derecho - mu_variable)/sigma_variable
              
              p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
              p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
              la_probabilidad <- p_der - p_izq
              
            }
      } else
        if(opciones == "valor_z") {
          if(intervalo == "menor"){
            z_izquierdo <- min_distribucion
            z_derecho <- as.numeric(as.character(input$z_var1))
            
            var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
            var_derecho <- z_derecho*sigma_variable + mu_variable
            
            la_probabilidad <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
            
          } else
            if(intervalo == "mayor"){
              z_izquierdo <- input$z_var2
              z_derecho <- max_distribucion
              
              var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
              var_derecho <- z_derecho*sigma_variable + mu_variable
              
              la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
              
            } else
              if(intervalo == "entre"){
                z_izquierdo <- input$z_var3
                z_derecho <- input$z_var4
                
                var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                var_derecho <- z_derecho*sigma_variable + mu_variable
                
                p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
                p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
                la_probabilidad <- p_der - p_izq
              }
        } else
          if(opciones == "probabilidad") {
            if(intervalo == "menor"){
              la_probabilidad <- as.numeric(as.character(input$prob_var1))
              
              z_izquierdo <- min_distribucion
              z_derecho <- qnorm(la_probabilidad, mean = 0, sd = 1, lower.tail = T)
              
              var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
              var_derecho <- z_derecho*sigma_variable + mu_variable
              
            } else
              if(intervalo == "mayor"){
                la_probabilidad <- input$prob_var1
                
                z_izquierdo <- qnorm(input$prob_var1, mean = 0, sd = 1, lower.tail = F)
                z_derecho <- max_distribucion
                
                var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                var_derecho <- z_derecho*sigma_variable + mu_variable
                
              } else
                if(intervalo == "entre"){
                  
                  la_probabilidad <- input$prob_var1
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  z_izquierdo <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = T)
                  z_derecho <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = F)
                  
                  var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                  var_derecho <- z_derecho*sigma_variable + mu_variable
                }
          }
    }
    
    
    
    armado <- matrix(NA, 2, 3)
    
    armado[,1]  <- c(z_izquierdo, z_derecho)
    armado[,2]  <- c(var_izquierdo, var_derecho)
    armado[,3]  <- c(la_probabilidad, la_probabilidad)
    
    colnames(armado) <- c("Z", "Var", "Probabilidad")
    rownames(armado) <- c("Izquierda", "Derecha")
    armado
  })
  
  
  output$tabla_normal01 <- renderTable({
    
    if(is.null(tabla_salida01())) return(NULL)
    tabla_salida01()
  })
  
  output$tabla_normal02 <- renderTable({
  
  if(is.null(tabla_salida02())) return(NULL)
  tabla_salida02()
})
  
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
   if(is.null(tabla_salida01())) return(NULL)
   if(is.null(tabla_salida02())) return(NULL)
    
    mu_variable <- tabla_salida01()[1,1]
    sigma_variable <- tabla_salida01()[1,3]
    color_variable <- tabla_salida01()[,ncol(tabla_salida01())]
    
  z_izquierdo <- tabla_salida02()[1,1] 
  z_derecho <- tabla_salida02()[2,1]
  
  var_izquierdo <- tabla_salida02()[1,2] 
  var_derecho <- tabla_salida02()[2,2]
  
  
    # Parametros
    # sigma <- 1
    # mu    <- 0
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- (marcas_z*sigma_variable) + mu_variable
    
  
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
     lower.x <- z_izquierdo
     upper.x <-  z_derecho
    
    # Grafico completo
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dnorm(x = x, mean = 0, sd = 1)
    plot(x, y ,type="l", lwd=2, col="blue", 
         xlim = c(min_distribucion, max_distribucion), 
         ylim=c(0,0.5),
         xlab = "Variable",
         ylab = "Frecuencia Relativa",
         axes = F)
    
    axis(side = 2,  las=1) # Eje Y
    axis(side = 1, at = marcas_z, labels = marcas_variable , las=1) # Eje X
    
    # Grafico Acotado
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    polygon(c(lower.x, x_mod, upper.x), c(0, y_mod, 0), col= color_variable)
    lines(x_mod, y_mod, col="blue", lwd=2)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    
    # Parametros
    sigma <- 1
    mu    <- 0
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/1000
    
    
    # Rango Acotado
    lower.x <- -0.0
    upper.x <-  2.1
    
    # Grafico completo
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dnorm(x = x, mean = 0, sd = 1)
    plot(x, y ,type="l", lwd=2, col="blue", xlim = c(min_distribucion, max_distribucion))
    
    # Grafico Acotado
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    polygon(c(lower.x, x_mod, upper.x), c(0, y_mod, 0), col= "orange")
    lines(x_mod, y_mod, col="blue", lwd=2)
    
  })
  
  
  output$armado01 <- renderUI({
    
    div(
      tableOutput(ns("tabla_normal01")),
      br(),
      tableOutput(ns("tabla_normal02")),
      br(),
      plotOutput(ns("distPlot1")),
      plotOutput(ns("distPlot2"))
    )
  })
}






