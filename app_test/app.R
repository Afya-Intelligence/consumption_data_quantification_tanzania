library(shiny)
library(shinydashboard)
source("randomModule.R")




ui <- fluidPage(
  randomUI("one"),
  randomUI("two"),
  randomUI("three"),
  randomUI("four")
)

server <- function(input, output, session) {
  randomServer("one")
  randomServer("two")
  randomServer("three")
  randomServer("four")
}

shinyApp(ui, server)

