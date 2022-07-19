

# tabPanel(title = "RMedic",
div(
  titlePanel("R+Medic"),
  
  br(), br(),
  selectInput(inputId = "selector_general",
              label = h2("Lista de Herramientas"), 
              choices = c("Distribución de Probabilidades" = "opc01",
                          "Tablas de Probabilidades" = "opc02",
                          "Otras Herramientas" = "opc03")
  ), br(), br(),
  sidebarLayout(
    div(id = "MySidebar2",
        
  
        sidebarPanel(id = "Sidebar2", 

       conditionalPanel("input.selector_general == 'opc01'",
                     SideBarDistribucionGeneral_UI("aver2"),
                     br(), br(),
                     SideBarDistribucionElegida_UI("aver2")
                     ),
       

        )
      ),
    mainPanel(id = "Main2",
              # uiOutput("RMedicSoft"),
              conditionalPanel("input.selector_general == 'opc01'",
              MainPanelDistribucionElegida_UI("aver2")
              ),
              br(), br(), br(),br(),br(),br(),br()
              
    )
    # End MainPanel ------------------------------------------
  ) 
)