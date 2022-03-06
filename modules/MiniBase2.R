

# Esto es exclusivo para Sobrevida por grupos

## Segmento del UI
MiniBaseUI2 <- function(id) {
  ns <- NS(id)
  
  div(tableOutput(ns("MiniBase")))
  
}




## Segmento del server
MiniBaseSERVER2 <- function(input, output, session, 
                            base, 
                            var_general,
                            var_grupo) {
  
  
  # Las 3 variables...
  tres_variables <- reactive({ 
    
    if(is.null(var_general())) return(NULL)
    if(is.null(var_general())) return(NULL)
    
    las_tres <- c(var_general(), var_grupo())
    return(las_tres)
  })
    
    
  # Minibase para grupo
  minibase <- reactive({
    
    if(is.null(tres_variables())) return(NULL)

    

    # The minibase
    minibase <- na.omit(base()[tres_variables()])
    minibase[,3] <- as.character(minibase[,3])
    

    return(minibase)
  })
  
  
  

  # Modul Return!!!
  return(minibase)
  
  # return(
  #   list(
  #     frec_input = reactive({ 3333 }),
  #     max_input = reactive({ 4444 })
  #   )
  # )
  
  
  
  
  
}


