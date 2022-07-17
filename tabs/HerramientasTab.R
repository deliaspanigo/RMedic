

# tabPanel(title = "RMedic",
div(
  titlePanel("R+Medic"),
  
  br(), br(),
 
  sidebarLayout(
    div(id = "MySidebar2",
        
        sidebarPanel(id = "Sidebar2", 
                     # SideBarBaseUI("base01")
                     "LALA",
                     SideBarDistribucionGeneral_UI("aver2"),
                     br(), br(),
                     SideBarDistribucionElegida_UI("aver2")
                     )
        ),
    mainPanel(id = "Main2",
              # uiOutput("RMedicSoft"),
              "LALA2",
              MainPanelDistribucionElegida_UI("aver2"),
              br(), br(), br(),br(),br(),br(),br()
              
    )
    # End MainPanel ------------------------------------------
  ) 
)