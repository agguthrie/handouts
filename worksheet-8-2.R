# Data
popdata <- read.csv('data/citypopdata.csv') #create pop trend for city based on drop down menu

# User Interface
in1 <- selectInput(
  inputId = 'selected_city',
  label = 'Select a city', #displays at user interface
  choices = unique(popdata[['NAME']]))

out1 <- textOutput('city_label')

tab1 <- tabPanel(
  title = "city population",
  inl, out1)
ui <- navbarPage(
  title = 'Census Population Explorer',
  tab1)

# Server
server <- function(input, output) {
  output[['city_label']] <- renderText({
    input[["selected_city"]]
  })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
