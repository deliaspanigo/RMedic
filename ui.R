


# fluidPage(
#   useShinyjs(),
#   theme = "styles.css", 
  
  navbarPage(theme = "styles.css",inverse=TRUE,
             useShinyjs(),
             title = strong("I need RMEDIC here!"),
             windowTitle = "RMedic - Medicina y R", 
             fluid = TRUE, 
             header = column(12, ""),
             # footer =  HTML(includeHTML("tools/footer.txt")),
             footer = column(12,
                             div(id = "footer",
                                 a("Consultoria Bioestadística de la Salud"), br(),
                                 "Contacto: ", a("d.eliaspanigo@gmail.com"),
                                 br(),
                                 HTML('&copy; David Elías Panigo (2016)')
                                 )
                             ),
             #footer = includeHTML("tools/footer.html"),
             # footer =  tags$iframe(src = "tools/footer.html"),
             id = "nav",

  
             # Tab Inicio (HOME)
             # Tab Inicio (HOME)
             shiny::tabPanel(title = "Inicio", icon = icon("home"), source("tabs/homeTab.R", encoding = "UTF-8")$value),
             shiny::tabPanel(title = "RMedic", source("tabs/RMedicTab.R", encoding = "UTF-8")$value)
)
