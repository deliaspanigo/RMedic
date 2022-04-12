

ModuleControlUI <- function(id) {
  
  ns <- NS(id)
  
  
  
  
}



ModuleControlSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "control01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "control02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  callModule(module = Control1Q_SERVER, id =  "control03",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Control1C_SERVER, id =  "control04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  
  
  
  
  menuCONTROL <- reactive({
    
    # Si no hay orden de salir a la cancha... Nadie sale...
    if(is.null(RMedic_general())) return(NULL)
    if(!RMedic_general()) return(NULL)
    
    # Si no hay status de BaseSalida(), nos vamos...
    if(is.null(status_BaseSalida())) return(NULL)
    if(!status_BaseSalida()) return(NULL)
    
    
    tabs <- list()
    
    
    tabs[[1]] <-  tabPanel(
      title = "Control", 
      icon = icon("user-md"), 
      value = 2,
      fluidRow(
        column(1),
        column(10,
               h3("Menú para Control"),
               BatallaNavalUI(ns("control01")),
               MiniBaseUI(ns("control02")),
               Control1Q_UI(ns("control03")),
               Control1C_UI(ns("control04"))#,
               # Tablas2Q_UI(ns("tablas05")),
               # Tablas2C_UI(ns("tablas06")),
               # TablasQC_UI(ns("tablas07"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuCONTROL)
}