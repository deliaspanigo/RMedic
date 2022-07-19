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
                   label = 'Distribución:',
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
    fluidRow(
      column(6,
      selectInput(inputId = ns("color_variable"), 
                  label = "Color:",
                  choices = c("Naranja" = "orange",
                            "Rojo" = "red",
                            "Verde" = "green",
                            "Azul" = "blue", 
                            "Amarillo" = "yellow", 
                            "Negro" = "black",
                            "Celeste" = "skyblue"), 
                  multiple = FALSE)
      ),
      column(6,
             numericInput(inputId = ns("decimals"),
                          label = "Decimales", 
                          min = 0,
                          step = 1,
                          value = 2)
             )
    ),
    br(),
    numericInput(inputId = ns("mu"),
                 label = "Media Poblacional:",
                 step= 0.01,
                 value= 106),
    numericInput(inputId = ns("sigma_cuadrado"),
                 label = "Varianza Poblacional: ",
                 min=0,
                 step= 0.01,  
                 value=64),
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor de la variable" = "original",
                              "Valor Z" = "valor_z",
                              "Probabilidad" = "probabilidad",
                              "Porcentaje" = "porcentaje")),
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
                                                                    step= 0.01, 
                                                                    value= 90)),
                                      
                                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                                       numericInput(inputId = ns("var2"),
                                                                    label = "Valor de la Variable Original:",
                                                                    step= 0.01, 
                                                                    value= 110)),
                                      
                                      conditionalPanel('input.intervalo == "entre"', ns = ns,
                                                       numericInput(inputId = ns("var3"), 
                                                                    label = "Valor Izquierdo de la Variable Original:",
                                                                    step= 0.01, 
                                                                    value= 90),
                                                       numericInput(inputId = ns("var4"),
                                                                    label = "Valor Derecho de la Variable Original:",
                                                                    step= 0.01, 
                                                                    value= 110))),  
                     
                     conditionalPanel('input.opciones == "valor_z"', ns = ns,
                                      conditionalPanel('input.intervalo == "menor"', ns = ns,
                                                       numericInput(inputId = ns("z_var1"),
                                                                    label = "Valor Z:", 
                                                                    step= 0.01, 
                                                                    value= -1.96)),
                                      
                                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                                       numericInput(inputId = ns("z_var2"),
                                                                    label = "Valor Z:", 
                                                                    step= 0.01, 
                                                                    value= 1)),
                                      
                                      conditionalPanel('input.intervalo == "entre"', ns = ns,
                                                       numericInput(inputId = ns("z_var3"),
                                                                    label = "Valor Z Izquierdo :", 
                                                                    step= 0.01, value= -1.96),
                                                       numericInput(inputId = ns("z_var4"),
                                                                    label = "Valor Z Derecho:", 
                                                                    step= 0.01, 
                                                                    value= 1))  
                     ),
                     conditionalPanel('input.opciones == "probabilidad"', ns = ns,
                                      numericInput(inputId = ns("prob_var1"),
                                                   label = "Probabilidad (Un valor entre 0 y 1):",
                                                   min=0,  max=1, step= 0.01, value= 0.25)),
    conditionalPanel('input.opciones == "porcentaje"', ns = ns,
                     numericInput(inputId = ns("porcentaje_var1"),
                                  label = "Porcentaje (Un valor entre 0 y 100):",
                                  min=0,  max=100, step= 0.01, value= 25))
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
  

    
  tabla_interna01 <- reactive({ 
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
  
  tabla_interna02 <- reactive({
    
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    if(is.null(input$decimals)) return(NULL)
    
    color_variable <- input$color_variable
    mu_variable <- input$mu
    sigma_cuadrado_variable <- input$sigma_cuadrado
    sigma_variable <- sqrt(sigma_cuadrado_variable)
    opciones <- input$opciones
    intervalo <- input$intervalo
    decimals <- input$decimals

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
          el_porcentaje <- la_probabilidad*100
          
          frase01 <- "El valor de la variable original es _VariableDerecho_.<br/>
                      El valor estandarizado es z = _z_derecho_."
        
          frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                      La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
          
        } else
          if(intervalo == "mayor"){
            
            if(is.null(input$var2)) return(NULL)
            var_izquierdo <- input$var2
            var_derecho <- max(marcas_variable)
            
            z_izquierdo <- (var_izquierdo - mu_variable)/sigma_variable
            z_derecho <- max_distribucion
            
            la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor de la variable original es _VariableIzquierdo_.<br/>
                        El valor estandarizado es z = _z_izquierdo_."
            
            frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
            
            
          } else
            if(intervalo == "entre"){
              
              if(is.null(input$var3)) return(NULL)
              if(is.null(input$var4)) return(NULL)
              
              var_izquierdo <- input$var3
              var_derecho <- input$var4
              
              z_izquierdo <- (var_izquierdo - mu_variable)/sigma_variable
              z_derecho <- (var_derecho - mu_variable)/sigma_variable
              
              p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
              p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
              la_probabilidad <- p_der - p_izq
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_.<br/>
                          Los valores estandarizados son _z_izquierdo_ y _z_derecho_.
                          "
              
              frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_."
            }
      } else
        if(opciones == "valor_z") {
          if(intervalo == "menor"){
            
            if(is.null(input$z_var1)) return(NULL)
            
            z_izquierdo <- min_distribucion
            z_derecho <- as.numeric(as.character(input$z_var1))
            
            var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
            var_derecho <- z_derecho*sigma_variable + mu_variable
            
            la_probabilidad <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor estandarizado es z = _z_derecho_.<br/>
                        El valor de la variable original es _VariableDerecho_."
            
            frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.null(input$z_var2)) return(NULL)
              
              z_izquierdo <- input$z_var2
              z_derecho <- max_distribucion
              
              var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
              var_derecho <- z_derecho*sigma_variable + mu_variable
              
              la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El valor estandarizado es z = _z_izquierdo_.<br/>
                          El valor de la variable original es _VariableIzquierdo_."
              
              frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
            } else
              if(intervalo == "entre"){
                
                if(is.null(input$z_var3)) return(NULL)
                if(is.null(input$z_var4)) return(NULL)
                
                z_izquierdo <- input$z_var3
                z_derecho <- input$z_var4
                
                var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                var_derecho <- z_derecho*sigma_variable + mu_variable
                
                p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
                p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
                la_probabilidad <- p_der - p_izq
                el_porcentaje <- la_probabilidad*100
               
                frase01 <- "Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_.<br/>
                            Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                
                frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_." 
              }
        } else
          if(opciones == "probabilidad") {
            if(intervalo == "menor"){
              
              if(is.null(input$prob_var1)) return(NULL)
              
              la_probabilidad <- as.numeric(as.character(input$prob_var1))
              
              z_izquierdo <- min_distribucion
              z_derecho <- qnorm(la_probabilidad, mean = 0, sd = 1, lower.tail = T)
              
              var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
              var_derecho <- z_derecho*sigma_variable + mu_variable
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "La probabilidad es _probabilidad_.<br/>
                          El valor estandarizado es z = _z_derecho_.<br/>
                          El valor de la variable original es _VariableDerecho_.
                          "
              
              frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
              
            } else
              if(intervalo == "mayor"){
                
                if(is.null(input$prob_var1)) return(NULL)
                
                la_probabilidad <- input$prob_var1
                
                z_izquierdo <- qnorm(input$prob_var1, mean = 0, sd = 1, lower.tail = F)
                z_derecho <- max_distribucion
                
                var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                var_derecho <- z_derecho*sigma_variable + mu_variable
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es z = _z_izquierdo_.<br/>
                            El valor de la variable original es _VariableIzquierdo_."
                
                frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
                
              } else
                if(intervalo == "entre"){
                  
                  if(is.null(input$prob_var1)) return(NULL)
                  la_probabilidad <- input$prob_var1
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  z_izquierdo <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = T)
                  z_derecho <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = F)
                  
                  var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                  var_derecho <- z_derecho*sigma_variable + mu_variable
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "La probabilidad es _probabilidad_.<br/>
                              Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_.<br/>
                              Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                  
                  frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                              La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_." 
                }
          } else
            if(opciones == "porcentaje") {
              if(intervalo == "menor"){
                
                if(is.null(input$porcentaje_var1)) return(NULL)
                
                el_porcentaje <- input$porcentaje_var1
                la_probabilidad <- el_porcentaje/100
                
                z_izquierdo <- min_distribucion
                z_derecho <- qnorm(la_probabilidad, mean = 0, sd = 1, lower.tail = T)
                
                var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                var_derecho <- z_derecho*sigma_variable + mu_variable
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "El porcentaje es _porcentaje_.<br/>
                            La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es z = _z_derecho_.<br/>
                            El valor de la variable original es _VariableDerecho_.
                           "
                
                frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
                
              } else
                if(intervalo == "mayor"){
                  
                  if(is.null(input$porcentaje_var1)) return(NULL)
                  
                  el_porcentaje <- input$porcentaje_var1
                  la_probabilidad <- el_porcentaje/100
                  
                  z_izquierdo <- qnorm(input$prob_var1, mean = 0, sd = 1, lower.tail = F)
                  z_derecho <- max_distribucion
                  
                  var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                  var_derecho <- z_derecho*sigma_variable + mu_variable
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "El porcentaje es _porcentaje_.<br/>
                              La probabilidad es _probabilidad_.<br/>
                              El valor estandarizado es z = _z_izquierdo_.<br/>
                              El valor de la variable original es _VariableIzquierdo_."
                  
                  frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                              La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
                  
                } else
                  if(intervalo == "entre"){
                    
                    if(is.null(input$porcentaje_var1)) return(NULL)
                    
                    el_porcentaje <- input$porcentaje_var1
                    la_probabilidad <- el_porcentaje/100
                    el_resto <- 1 - la_probabilidad
                    la_mitad <- el_resto/2
                    
                    z_izquierdo <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = T)
                    z_derecho <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = F)
                    
                    var_izquierdo <- z_izquierdo*sigma_variable + mu_variable
                    var_derecho <- z_derecho*sigma_variable + mu_variable
                    
                    el_porcentaje <- la_probabilidad*100
                    
                    frase01 <- "El porcentaje es _porcentaje_.<br/>
                                La probabilidad es _probabilidad_.<br/>
                                Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_.<br/>
                                Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                    
                    frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                                La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_." 
                    
                  }
            }
      
      
      Pattern <- c("_VariableIzquierdo_", "_VariableDerecho_", "_z_izquierdo_", "_z_derecho_",
                   "_probabilidad_", "_porcentaje_")
      
      Replacement <- c(var_izquierdo, var_derecho, z_izquierdo, z_derecho,
                       la_probabilidad, el_porcentaje)
      
      Replacement <- round2(Replacement, decimals)
      
      frase01 <- stringi::stri_replace_all_fixed(str = frase01,
                                               pattern = Pattern,
                                               replacement = Replacement,
                                               vectorize_all = F)
      
      frase02 <- stringi::stri_replace_all_fixed(str = frase02,
                                                 pattern = Pattern,
                                                 replacement = Replacement,
                                                 vectorize_all = F)
    }
    
 
    
    las_columnas <- c("Variable", "Z", "Probabilidad", "Porcentaje", "Frase01", "Frase02")
    las_filas <- c("Izquierda", "Derecha")
    
    armado <- as.data.frame(matrix(NA, length(las_filas), length(las_columnas)))
    colnames(armado) <- las_columnas
    rownames(armado) <- las_filas
    
    armado$"Variable" <- c(var_izquierdo, var_derecho)
    armado$"Z" <- c(z_izquierdo, z_derecho)
    armado$"Probabilidad" <- c(la_probabilidad, la_probabilidad)
    armado$"Porcentaje" <- c(el_porcentaje, el_porcentaje)
    
    armado <- round2(armado, decimals)
    
    armado$"Frase01"  <- c(frase01, frase01)
    armado$"Frase02"  <- c(frase02, frase02)
    
    
    armado
  })
  
  tabla_externa01 <- reactive({
    
    if(is.null(tabla_interna01())) return(NULL)
    
    tabla <- tabla_interna01()
    tabla <- tabla[,c(1:3)]
    
    tabla <- as.matrix(tabla)
    colnames(tabla) <- c("Media Poblacional", "Varianza Poblacional", "Desvío Poblacional")
    tabla[,1] <- as.character(tabla[,1])
    
    tabla
  })
  
  tabla_externa02 <- reactive({
    
    if(is.null(tabla_interna01())) return(NULL)
    if(is.null(tabla_interna02())) return(NULL)
    
    intervalo <- tabla_interna01()$"intervalo"
    
    tabla <- tabla_interna02()
   # tabla <- as.matrix(tabla)
   # tabla[,1] <- as.character(tabla[,1])
    
    if(intervalo == "menor"){ 
      tabla <- as.data.frame(tabla[2,])
      #return(tabla)
      
    }else
      if(intervalo == "mayor"){ 
        tabla <- as.data.frame(tabla[1,])
        #return(tabla)
        
      }else
        if(intervalo == "entre"){
          
          Posicion <- c("Izquierdo", "Derecho")
          tabla <- cbind(Posicion, tabla)
          # return(tabla)
        }
    
    tabla <- tabla[,-c(ncol(tabla), ncol(tabla)-1)] # Quitamos las frases01 y frases02
    tabla <- as.matrix(tabla)
    tabla[,1] <- as.character(tabla[,1])
    tabla
    
  })
  
  output$tabla_normal01 <- renderTable(align = "c", {
    
    if(is.null(tabla_externa01())) return(NULL)
    tabla_externa01()
  })
  
  output$tabla_normal02 <- renderTable(align = "c",{
  
  if(is.null(tabla_externa02())) return(NULL)
    tabla_externa02()
})
  
  output$frase_final01 <- renderUI({
    
    frase <- tabla_interna02()$"Frase01"[1]
    HTML(frase)
  })
  
  output$frase_final02 <- renderUI({
    
    frase <- tabla_interna02()$"Frase02"[1]
    HTML(frase)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    # https://r-coder.com/plot-en-r/
    
   if(is.null(tabla_interna01())) return(NULL)
   if(is.null(tabla_interna02())) return(NULL)
    
    mu_variable <- tabla_interna01()[1,1]
    sigma_variable <- tabla_interna01()[1,3]
    color_variable <- tabla_interna01()$"color_variable"
    decimals <- input$decimals
    
  z_izquierdo <- tabla_interna02()$"Z"[1] 
  z_derecho <- tabla_interna02()$"Z"[2]
  
  var_izquierdo <- tabla_interna02()$"Variable"[1] 
  var_derecho <- tabla_interna02()$"Variable"[2]
  
  
    # Parametros
    # sigma <- 1
    # mu    <- 0
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- (marcas_z*sigma_variable) + mu_variable
    marcas_variable <- round2(marcas_variable, decimals)
  
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
     lower.x <- z_izquierdo
     upper.x <-  z_derecho
    
    # Grafico completo
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dnorm(x = x, mean = 0, sd = 1)
    
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    plot(x, y ,type="l", lwd=4, col="black", 
         xlim = c(min_distribucion, max_distribucion), 
         ylim=c(0,0.5),
         xlab = "Variable Original",
         ylab = "Frecuencia Relativa",
         axes = F,
         cex.lab = 2)
    

    axis(side = 2,  las=1, cex.axis=2) # Eje Y
    axis(side = 1, at = marcas_z, labels = marcas_variable , las=1, cex.axis=2) # Eje X
    
    # Grafico Acotado
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    polygon(c(lower.x, x_mod, upper.x), c(0, y_mod, 0), col = color_variable)
    lines(x_mod, y_mod, col="black", lwd=4)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    
    if(is.null(tabla_interna01())) return(NULL)
    if(is.null(tabla_interna02())) return(NULL)
    

    # mu_variable <- tabla_interna01()[1,1]
    # sigma_variable <- tabla_interna01()[1,3]
    
    mu_variable <- 0
    sigma_variable <- 1
    color_variable <- tabla_interna01()$"color_variable"
    decimals <- input$decimals
    
    z_izquierdo <- tabla_interna02()$"Z"[1] 
    z_derecho <- tabla_interna02()$"Z"[2]
    
    var_izquierdo <- tabla_interna02()$"Variable"[1] 
    var_derecho <- tabla_interna02()$"Variable"[2]
    
    
    # Parametros
    # sigma <- 1
    # mu    <- 0
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- (marcas_z*sigma_variable) + mu_variable
    marcas_variable <- round2(marcas_variable, decimals)
    
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
    lower.x <- z_izquierdo
    upper.x <-  z_derecho
    
    # Grafico completo
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dnorm(x = x, mean = 0, sd = 1)
    
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    plot(x, y ,type="l", lwd=4, col="black", 
         xlim = c(min_distribucion, max_distribucion), 
         ylim=c(0,0.5),
         xlab = "Variable Z",
         ylab = "Frecuencia Relativa",
         axes = F,
         cex.lab = 2)
    
    
    axis(side = 2,  las=1, cex.axis=2) # Eje Y
    axis(side = 1, at = marcas_z, labels = marcas_variable , las=1, cex.axis=2) # Eje X
    
    # Grafico Acotado
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    polygon(c(lower.x, x_mod, upper.x), c(0, y_mod, 0), col = color_variable)
    lines(x_mod, y_mod, col="black", lwd=4)
    
    
  })
  
  
  output$armado01 <- renderUI({
    
    if (la_distribucion() != "001_Normal") return(NULL)
    
    div( h2("Distribución de la Variable"),
            plotOutput(ns("distPlot1")),
         br(),
          h2("Distribución Normal Estándar (Z)"),
         plotOutput(ns("distPlot2")), br(),
         br(),
         br(),
         
         fluidRow(
           column(6, 
                  h2("Parámetros"),
                  tableOutput(ns("tabla_normal01")),
                  br(),
                  h2("Cálculos"),
                  tableOutput(ns("tabla_normal02"))),
           column(6, 
                  h2("Detalles"),
                  htmlOutput(ns("frase_final01")),
                  br(),br(),
                  h2("Frase Explicativa"),
                  htmlOutput(ns("frase_final02")),
                  tags$head(tags$style(
                                paste0("#", ns("frase_final01"), "{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                  )
                  ),
                  tags$head(tags$style(
                    paste0("#", ns("frase_final02"), "{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                  )
                  )
                  
                  )
         )
        , br(),
        # span(htmlOutput(ns("frase_control_qc")), style="color:red")
      
      br()
     
    )
  })
}






