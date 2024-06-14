library(shiny)


ui <- fluidPage(
  selectInput(inputId = "region", label = "Choose your region", choices = c("MEA", "R1"))
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server = server)