library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

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
  ),
  dashboardBody(
    fluidPage(DTOutput('tbl')))
)

server <- function(input, output) {
  output$tbl = renderDT(
    iris %>% filter(Species %in% input$v_species), 
    options = list(lengthChange = FALSE)
    )
  output$report <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".html")
    },
    content = function(file) {
      temp_report <- file.path(tempdir(), "sample_iris_param_report.Rmd")
      file.copy("reports/sample_iris_param_report.Rmd", temp_report, overwrite = TRUE)
      params <- list(
        df = iris %>% filter(Species %in% input$v_species),
        species = input$v_species
      )
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)

