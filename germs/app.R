#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("v_species", "Flower Species",
                choices = iris %>% distinct(Species)),
    
    downloadButton(
      "report", 
      "Download Iris report",
      style = "width:80%; display:block; margin-left:auto; margin-right:auto"
    )
    
    # downloadHandler(
    #   filename = function() {
    #     paste0("Testing_IRIS", Sys.Date(), "csv")
    #   },
    #   content = function
    # )
    
  ),
  dashboardBody(
    fluidPage(DTOutput('tbl')))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tbl = renderDT(
    iris %>% filter(Species %in% input$v_species), 
    options = list(lengthChange = FALSE)
    )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(
        iris %>% filter(Species %in% input$v_species), 
        file
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

# shinyApp(
#   ui = fluidPage(DTOutput('tbl')),
#   server = function(input, output) {
#     output$tbl = renderDT(
#       iris, options = list(lengthChange = FALSE)
#     )
#   }
# )
