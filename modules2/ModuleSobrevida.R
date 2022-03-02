

ModuleSobrevidaUI <- function(id) {
  
  ns <- NS(id)
  
  
 
  
}



ModuleSobrevidaSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER3, 
                              id =  "sobrevida01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  batalla_naval <- UserSelection$batalla_naval
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  caso <- 4
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    ejecucion <- FALSE
    if(is.null(casoRMedic())) return(ejecucion)
    if(is.null(caso)) return(ejecucion)
    
    if(casoRMedic() == caso) {
      
      if(batalla_naval()[[6]]) ejecucion <- TRUE else ejecucion <- FALSE 
      
    } else ejecucion <- FALSE
    
    
    return(ejecucion)
    
  })
  
  
  decimales <- UserSelection$decimales
  
  alfa <- UserSelection$alfa
  
  minibase <- callModule(module = MiniBaseSERVER, 
                         id =  "sobrevida02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  
  callModule(module = KM_Grafico_SobrevidaGeneral_SERVER,
             id =  "sobrevida03",
             minibase = minibase,
             decimales = decimales,
             alfa = alfa,
             control_ejecucion = control_ejecucion)
  

  
  
 
  
  
  
  menuSOBREVIDA <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Sobrevida", 
      icon = icon("user-md"), 
      value = 3,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Sobrevida"),
               BatallaNavalUI(ns("sobrevida01")),
               MiniBaseUI(ns("sobrevida02")),
               div(
                 tabsetPanel(
                   tabPanel("Sobrevida General", 
                            KM_Grafico_SobrevidaGeneral_UI(ns("sobrevida03"))),
                   tabPanel("Sobrevida por Grupos",
                            column(4,
                                   selectInput(inputId = ns("var3"),
                                               label = "Grupo (Variable 3): ",
                                               choices = "")
                            ),)
                 )
               )
                    
               
              
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuSOBREVIDA)
  }