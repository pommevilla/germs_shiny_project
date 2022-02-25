library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "visualizations", icon = icon("dashboard")),
      menuItem("Statistics", tabName = "statistics", icon = icon("th"))
    ),
    selectInput("v_species", "Flower Species",
                choices = iris %>% distinct(Species)),
    
    downloadButton(
      "report", 
      "Download Iris report",
      style = "width:80%; display:block; margin-left:auto; margin-right:auto"
    )
  ),
  dashboardBody(
    tabItems( 
      tabItem(tabName = "visualizations", fluidPage(fluidRow(column(12, plotOutput("iris_plot")) ), DTOutput('tbl'))),
      tabItem(tabName = "statistics", h2("Stuff"),
              fluidPage(box("Does this work?")))
      ),
  )
)

server <- function(input, output) {
  filtered_df <- reactive(iris %>% filter(Species %in% input$v_species))
  
  
  output$iris_plot = renderPlot({
    
    this_title <- str_interp("Sepal Width Vs. Sepal Length for ${input$v_species}")
    
    ggplot(filtered_df(), aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point() +
      theme_minimal() + 
      labs(
        title = this_title
      )
    
  })
  output$tbl = renderDT(
    filtered_df(),
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

