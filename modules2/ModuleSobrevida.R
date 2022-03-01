

ModuleSobrevidaUI <- function(id) {
  
  ns <- NS(id)
  
  
 
  
}



ModuleSobrevidaSERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  UserSelection <- callModule(module = BatallaNavalSERVER, 
                              id =  "sobrevida01",
                              base = base,
                              zocalo_CIE = zocalo_CIE,
                              verbatim = FALSE)
  
  
  MiniBase <- callModule(module = MiniBaseSERVER, id =  "sobrevida02",
                         base = base,
                         batalla_naval = UserSelection$batalla_naval,
                         verbatim = FALSE)
  
  
  
  callModule(module = Tablas1Q_SERVER, id =  "sobrevida03",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas1C_SERVER, id =  "sobrevida04",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas2Q_SERVER, id =  "sobrevida05",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = Tablas2C_SERVER, id =  "sobrevida06",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  callModule(module = TablasQC_SERVER, id =  "sobrevida07",
             minibase = MiniBase,
             batalla_naval = UserSelection$batalla_naval,
             decimales = UserSelection$decimales)
  
  
  
  
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
               Tablas1Q_UI(ns("sobrevida03")),
               Tablas1C_UI(ns("sobrevida04")),
               Tablas2Q_UI(ns("sobrevida05")),
               Tablas2C_UI(ns("sobrevida06")),
               TablasQC_UI(ns("sobrevida07"))
        ),
        column(1)
      )
      
    ) # End TabPanel
    
    
    
    tabs
    
  })
  
  
  #Return del Modulo
  return(menuSOBREVIDA)
  }