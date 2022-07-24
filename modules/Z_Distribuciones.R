## Segmento del UI
Menu_DistribucionGeneral01_UI <- function(id) {
  ns <- NS(id)
  
  
 
  uiOutput(ns("outMe_distribucion01_genenral"))
  
  
}



 ## Segmento del server
Menu_DistribucionGeneral_SERVER <- function(input, output, session,
                                              carpeta_distribuciones) {
   
  # NameSpaceasing for the session
  ns <- session$ns
  
   output$outMe_distribucion01_genenral <- renderUI({
     
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
  
  
  
  uiOutput(ns("outMe_sidebar_elegida"))
  
  
}

## Segmento del UI
MainPanelDistribucionElegida_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("outMe_mainmenu_elegida"))
  # renderUI(ns("MainPanelNormal"))
 # uiOutput(ns("outMe_elegida02"))
  
 
}

## Segmento del server
SideBarDistribucionElegida_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_sidebar_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(SideBar01_Normal("aver2")) else
      if (la_distribucion() == "002_t_de_Student") return(SideBar02_t("aver2")) else
        if (la_distribucion() == "003_Chi_Cuadrado") return(SideBar03_chi("aver2")) else
          if (la_distribucion() == "004_F_de_Fisher") return(SideBar04_f("aver2")) else
      return(NULL)
    

  })
  
  

}


## Segmento del server
MainPanelDistribucionElegida_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_mainmenu_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(MainPanel01_Normal("aver2")) else
      if (la_distribucion() == "002_t_de_Student") return(MainPanel02_t("aver2")) else
        if (la_distribucion() == "003_Chi_Cuadrado") return(MainPanel03_chi("aver2")) else
          if (la_distribucion() == "004_F_de_Fisher") return(MainPanel04_f("aver2")) else
      return(NULL)
    
    
  })
  
  

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
  
  los_valores <- reactive({
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    mu_variable <- NA
    sigma_cuadrado_variable <- NA
    sigma_variable <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    z1 <- NA
    z2 <- NA
    z3 <- NA
    z4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    
    if(!is.null(input$color_variable))color_variable <- input$color_variable
    if(!is.null(input$mu)) mu_variable <- input$mu
    if(!is.null(input$sigma_cuadrado)){
      
      sigma_cuadrado_variable <- input$sigma_cuadrado
      sigma_variable <- sqrt(sigma_cuadrado_variable)
    } 
    if(!is.null(input$opciones)) opciones <- input$opciones
    if(!is.null(input$intervalo))intervalo <- input$intervalo
    if(!is.null(input$decimals)) decimals <- input$decimals
    
    if(!is.null(input$var1))var1 <- input$var1
    if(!is.null(input$var2))var2 <- input$var2
    if(!is.null(input$var3))var3 <- input$var3
    if(!is.null(input$var4))var4 <- input$var4
    
    if(!is.null(input$z_var1))z1 <- input$z_var1
    if(!is.null(input$z_var2))z2 <- input$z_var2
    if(!is.null(input$z_var3))z3 <- input$z_var3
    if(!is.null(input$z_var4))z4 <- input$z_var4
    
    if(!is.null(input$prob_var1))probabilidad_externo <- input$prob_var1
    if(!is.null(input$porcentaje_var1))porcentaje_externo <- input$porcentaje_var1
    
    
   los_valores <- Hmisc::llist(mu_variable, sigma_cuadrado_variable, sigma_variable,
                               opciones, intervalo, color_variable,
                               var1, var2, var3, var4, 
                               z1, z2, z3, z4, 
                               probabilidad_externo, porcentaje_externo, decimals)
   
   return(los_valores)
     
     
     
  })
  
  las_tablas <- reactive({

   if(is.null(los_valores())) return(NULL)
    
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "tablas"
    
  
   las_tablas <-  Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                                      sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                                      sigma_variable = mis_valores$sigma_variable,
                                      opciones = mis_valores$opciones,
                                      intervalo = mis_valores$intervalo, 
                                      color_variable = mis_valores$color_variable,
                                      var1 = mis_valores$var1, 
                                      var2 = mis_valores$var2, 
                                      var3 = mis_valores$var3, 
                                      var4 = mis_valores$var4, 
                                      z1 = mis_valores$z1, 
                                      z2 = mis_valores$z2, 
                                      z3 = mis_valores$z3, 
                                      z4 = mis_valores$z4, 
                                      probabilidad_externo = mis_valores$probabilidad_externo, 
                                      porcentaje_externo = mis_valores$porcentaje_externo, 
                                      decimals = mis_valores$decimals,
                                      ejecucion = mis_valores$ejecucion)
    
   return(las_tablas)
  })
    

  
  output$tabla_normal01 <- renderTable(align = "c", {
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa01
    out <- CharacterALL(out)
    out
  })
  
  output$tabla_normal02 <- renderTable(align = "c",{
  
  if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa02
    out <- CharacterALL(out)
    out
    # tabla_externa02()
})
  
  output$frase_final01 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase01
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase02
    HTML(out)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico01"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                                       sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                                       sigma_variable = mis_valores$sigma_variable,
                                       opciones = mis_valores$opciones,
                                       intervalo = mis_valores$intervalo, 
                                       color_variable = mis_valores$color_variable,
                                       var1 = mis_valores$var1, 
                                       var2 = mis_valores$var2, 
                                       var3 = mis_valores$var3, 
                                       var4 = mis_valores$var4, 
                                       z1 = mis_valores$z1, 
                                       z2 = mis_valores$z2, 
                                       z3 = mis_valores$z3, 
                                       z4 = mis_valores$z4, 
                                       probabilidad_externo = mis_valores$probabilidad_externo, 
                                       porcentaje_externo = mis_valores$porcentaje_externo, 
                                       decimals = mis_valores$decimals,
                                       ejecucion = mis_valores$ejecucion)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico02"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
    
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(la_distribucion())) return(NULL)
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


########################################################################################

# Normal 
# # SidePanelNormal
## Segmento del UI
SideBar02_t <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    h3("Distribución t"),
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


MainPanel02_t <- function(id){
  
  ns <- NS(id)
  
  
  uiOutput(ns("armado01_t"))
  
  
}


Server02_t_Server <- function(input, output, session,
                                   la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  los_valores <- reactive({
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    mu_variable <- NA
    sigma_cuadrado_variable <- NA
    sigma_variable <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    z1 <- NA
    z2 <- NA
    z3 <- NA
    z4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    
    if(!is.null(input$color_variable))color_variable <- input$color_variable
    if(!is.null(input$mu)) mu_variable <- input$mu
    if(!is.null(input$sigma_cuadrado)){
      
      sigma_cuadrado_variable <- input$sigma_cuadrado
      sigma_variable <- sqrt(sigma_cuadrado_variable)
    } 
    if(!is.null(input$opciones)) opciones <- input$opciones
    if(!is.null(input$intervalo))intervalo <- input$intervalo
    if(!is.null(input$decimals)) decimals <- input$decimals
    
    if(!is.null(input$var1))var1 <- input$var1
    if(!is.null(input$var2))var2 <- input$var2
    if(!is.null(input$var3))var3 <- input$var3
    if(!is.null(input$var4))var4 <- input$var4
    
    if(!is.null(input$z_var1))z1 <- input$z_var1
    if(!is.null(input$z_var2))z2 <- input$z_var2
    if(!is.null(input$z_var3))z3 <- input$z_var3
    if(!is.null(input$z_var4))z4 <- input$z_var4
    
    if(!is.null(input$prob_var1))probabilidad_externo <- input$prob_var1
    if(!is.null(input$porcentaje_var1))porcentaje_externo <- input$porcentaje_var1
    
    
    los_valores <- Hmisc::llist(mu_variable, sigma_cuadrado_variable, sigma_variable,
                                opciones, intervalo, color_variable,
                                var1, var2, var3, var4, 
                                z1, z2, z3, z4, 
                                probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_valores)
    
    
    
  })
  
  las_tablas <- reactive({
    
    if(is.null(los_valores())) return(NULL)
    
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "tablas"
    
    
    las_tablas <-  Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                                       sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                                       sigma_variable = mis_valores$sigma_variable,
                                       opciones = mis_valores$opciones,
                                       intervalo = mis_valores$intervalo, 
                                       color_variable = mis_valores$color_variable,
                                       var1 = mis_valores$var1, 
                                       var2 = mis_valores$var2, 
                                       var3 = mis_valores$var3, 
                                       var4 = mis_valores$var4, 
                                       z1 = mis_valores$z1, 
                                       z2 = mis_valores$z2, 
                                       z3 = mis_valores$z3, 
                                       z4 = mis_valores$z4, 
                                       probabilidad_externo = mis_valores$probabilidad_externo, 
                                       porcentaje_externo = mis_valores$porcentaje_externo, 
                                       decimals = mis_valores$decimals,
                                       ejecucion = mis_valores$ejecucion)
    
    return(las_tablas)
  })
  
  
  
  output$tabla_normal01 <- renderTable(align = "c", {
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa01
    out <- CharacterALL(out)
    out
  })
  
  output$tabla_normal02 <- renderTable(align = "c",{
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa02
    out <- CharacterALL(out)
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase01
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase02
    HTML(out)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico01"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico02"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
    
  })
  
  
  output$armado01_t <- renderUI({
    
    if(is.null(la_distribucion())) return(NULL)
    if (la_distribucion() != "002_t_de_Student") return(NULL)
    
    div( h2("Distribución de la Variable"),
         plotOutput(ns("distPlot1")),
         br(),
         h2("Distribución t Estándar (t)"),
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

################################################################################
SideBar03_chi <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    h3("Distribución Chi Cuadrado"),
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


MainPanel03_chi <- function(id){
  
  ns <- NS(id)
  
  
  uiOutput(ns("armado01_chi"))
  
  
}


Server03_chi_Server <- function(input, output, session,
                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  los_valores <- reactive({
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    mu_variable <- NA
    sigma_cuadrado_variable <- NA
    sigma_variable <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    z1 <- NA
    z2 <- NA
    z3 <- NA
    z4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    
    if(!is.null(input$color_variable))color_variable <- input$color_variable
    if(!is.null(input$mu)) mu_variable <- input$mu
    if(!is.null(input$sigma_cuadrado)){
      
      sigma_cuadrado_variable <- input$sigma_cuadrado
      sigma_variable <- sqrt(sigma_cuadrado_variable)
    } 
    if(!is.null(input$opciones)) opciones <- input$opciones
    if(!is.null(input$intervalo))intervalo <- input$intervalo
    if(!is.null(input$decimals)) decimals <- input$decimals
    
    if(!is.null(input$var1))var1 <- input$var1
    if(!is.null(input$var2))var2 <- input$var2
    if(!is.null(input$var3))var3 <- input$var3
    if(!is.null(input$var4))var4 <- input$var4
    
    if(!is.null(input$z_var1))z1 <- input$z_var1
    if(!is.null(input$z_var2))z2 <- input$z_var2
    if(!is.null(input$z_var3))z3 <- input$z_var3
    if(!is.null(input$z_var4))z4 <- input$z_var4
    
    if(!is.null(input$prob_var1))probabilidad_externo <- input$prob_var1
    if(!is.null(input$porcentaje_var1))porcentaje_externo <- input$porcentaje_var1
    
    
    los_valores <- Hmisc::llist(mu_variable, sigma_cuadrado_variable, sigma_variable,
                                opciones, intervalo, color_variable,
                                var1, var2, var3, var4, 
                                z1, z2, z3, z4, 
                                probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_valores)
    
    
    
  })
  
  las_tablas <- reactive({
    
    if(is.null(los_valores())) return(NULL)
    
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "tablas"
    
    
    las_tablas <-  Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                                       sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                                       sigma_variable = mis_valores$sigma_variable,
                                       opciones = mis_valores$opciones,
                                       intervalo = mis_valores$intervalo, 
                                       color_variable = mis_valores$color_variable,
                                       var1 = mis_valores$var1, 
                                       var2 = mis_valores$var2, 
                                       var3 = mis_valores$var3, 
                                       var4 = mis_valores$var4, 
                                       z1 = mis_valores$z1, 
                                       z2 = mis_valores$z2, 
                                       z3 = mis_valores$z3, 
                                       z4 = mis_valores$z4, 
                                       probabilidad_externo = mis_valores$probabilidad_externo, 
                                       porcentaje_externo = mis_valores$porcentaje_externo, 
                                       decimals = mis_valores$decimals,
                                       ejecucion = mis_valores$ejecucion)
    
    return(las_tablas)
  })
  
  
  
  output$tabla_normal01 <- renderTable(align = "c", {
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa01
    out <- CharacterALL(out)
    out
  })
  
  output$tabla_normal02 <- renderTable(align = "c",{
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa02
    out <- CharacterALL(out)
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase01
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase02
    HTML(out)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico01"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico02"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
    
  })
  
  
  output$armado01_chi <- renderUI({
    
    if(is.null(la_distribucion())) return(NULL)
    if (la_distribucion() != "003_Chi_Cuadrado") return(NULL)
    
    div( h2("Distribución de la Variable"),
         plotOutput(ns("distPlot1")),
         br(),
         h2("Distribución Chi Cuadrado Estándar (Chi)"),
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


################################################################################

SideBar04_f <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    h3("Distribución F"),
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


MainPanel04_f <- function(id){
  
  ns <- NS(id)
  
  
  uiOutput(ns("armado01_f"))
  
  
}


Server04_f_Server <- function(input, output, session,
                                la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  los_valores <- reactive({
    if(is.null(input$color_variable)) return(NULL)
    if(is.null(input$mu)) return(NULL)
    if(is.null(input$sigma_cuadrado)) return(NULL)
    if(is.null(input$opciones)) return(NULL)
    if(is.null(input$intervalo)) return(NULL)
    
    mu_variable <- NA
    sigma_cuadrado_variable <- NA
    sigma_variable <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    z1 <- NA
    z2 <- NA
    z3 <- NA
    z4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    
    if(!is.null(input$color_variable))color_variable <- input$color_variable
    if(!is.null(input$mu)) mu_variable <- input$mu
    if(!is.null(input$sigma_cuadrado)){
      
      sigma_cuadrado_variable <- input$sigma_cuadrado
      sigma_variable <- sqrt(sigma_cuadrado_variable)
    } 
    if(!is.null(input$opciones)) opciones <- input$opciones
    if(!is.null(input$intervalo))intervalo <- input$intervalo
    if(!is.null(input$decimals)) decimals <- input$decimals
    
    if(!is.null(input$var1))var1 <- input$var1
    if(!is.null(input$var2))var2 <- input$var2
    if(!is.null(input$var3))var3 <- input$var3
    if(!is.null(input$var4))var4 <- input$var4
    
    if(!is.null(input$z_var1))z1 <- input$z_var1
    if(!is.null(input$z_var2))z2 <- input$z_var2
    if(!is.null(input$z_var3))z3 <- input$z_var3
    if(!is.null(input$z_var4))z4 <- input$z_var4
    
    if(!is.null(input$prob_var1))probabilidad_externo <- input$prob_var1
    if(!is.null(input$porcentaje_var1))porcentaje_externo <- input$porcentaje_var1
    
    
    los_valores <- Hmisc::llist(mu_variable, sigma_cuadrado_variable, sigma_variable,
                                opciones, intervalo, color_variable,
                                var1, var2, var3, var4, 
                                z1, z2, z3, z4, 
                                probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_valores)
    
    
    
  })
  
  las_tablas <- reactive({
    
    if(is.null(los_valores())) return(NULL)
    
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "tablas"
    
    
    las_tablas <-  Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                                       sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                                       sigma_variable = mis_valores$sigma_variable,
                                       opciones = mis_valores$opciones,
                                       intervalo = mis_valores$intervalo, 
                                       color_variable = mis_valores$color_variable,
                                       var1 = mis_valores$var1, 
                                       var2 = mis_valores$var2, 
                                       var3 = mis_valores$var3, 
                                       var4 = mis_valores$var4, 
                                       z1 = mis_valores$z1, 
                                       z2 = mis_valores$z2, 
                                       z3 = mis_valores$z3, 
                                       z4 = mis_valores$z4, 
                                       probabilidad_externo = mis_valores$probabilidad_externo, 
                                       porcentaje_externo = mis_valores$porcentaje_externo, 
                                       decimals = mis_valores$decimals,
                                       ejecucion = mis_valores$ejecucion)
    
    return(las_tablas)
  })
  
  
  
  output$tabla_normal01 <- renderTable(align = "c", {
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa01
    out <- CharacterALL(out)
    out
  })
  
  output$tabla_normal02 <- renderTable(align = "c",{
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$tabla_externa02
    out <- CharacterALL(out)
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase01
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(las_tablas())) return(NULL)
    out <- las_tablas()$frase02
    HTML(out)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico01"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(los_valores())) return(NULL)
    mis_valores <- los_valores()
    mis_valores$ejecucion <- "grafico02"
    
    
    Distribucion.Normal(mu_variable = mis_valores$mu_variable, 
                        sigma_cuadrado_variable = mis_valores$sigma_cuadrado_variable, 
                        sigma_variable = mis_valores$sigma_variable,
                        opciones = mis_valores$opciones,
                        intervalo = mis_valores$intervalo, 
                        color_variable = mis_valores$color_variable,
                        var1 = mis_valores$var1, 
                        var2 = mis_valores$var2, 
                        var3 = mis_valores$var3, 
                        var4 = mis_valores$var4, 
                        z1 = mis_valores$z1, 
                        z2 = mis_valores$z2, 
                        z3 = mis_valores$z3, 
                        z4 = mis_valores$z4, 
                        probabilidad_externo = mis_valores$probabilidad_externo, 
                        porcentaje_externo = mis_valores$porcentaje_externo, 
                        decimals = mis_valores$decimals,
                        ejecucion = mis_valores$ejecucion)
    
    
  })
  
  
  output$armado01_f <- renderUI({
    
    if(is.null(la_distribucion())) return(NULL)
    if (la_distribucion() != "004_F_de_Fisher") return(NULL)
    
    div( h2("Distribución de la Variable"),
         plotOutput(ns("distPlot1")),
         br(),
         h2("Distribución F de Fisher (F)"),
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


