library(shiny)

# User Interface
ui <- navbarPage(title = 'Hello, Shiny World!')

# Server
server <- function(input, output) { #curly brackets explain what function does
  
}

# Create the Shiny App
shinyApp(ui, server) 
