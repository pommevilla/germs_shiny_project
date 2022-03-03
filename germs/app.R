library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

ci_options <- c(TRUE, FALSE)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "visualizations", icon = icon("dashboard")),
      menuItem("Statistics", tabName = "statistics", icon = icon("th"))
    ),
    selectInput("v_species", "Flower Species",
                choices = iris %>% distinct(Species)),
    
    #radioButtons("iris_ci", "Do you want to display the 95% confidence interval of iris regression line?", ci_options, selected = TRUE),
    
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
    fit <- lm(Sepal.Width ~ Sepal.Length, data = filtered_df())
    lm_annot <- str_interp("y == ${fit$coefficients['(Intercept)']} + ${fit$coefficients['Sepal.Length']} * x")
    
    ggplot(filtered_df(), aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point() +
      #geom_smooth(method = 'lm', se = input$iris_ci) +
      geom_smooth(method = 'lm', se = TRUE) +
      theme_minimal() + 
      labs(
        title = this_title
      ) +
      annotate("text", x = max(filtered_df()$Sepal.Length), y = min(filtered_df()$Sepal.Width), hjust = 1, vjust = 0, label = lm_annot, parse = TRUE)
    
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

