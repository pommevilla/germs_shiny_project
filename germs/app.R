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
    
    radioButtons("iris_ci", "Display 95% Confidence Interval", ci_options, selected = TRUE),
    
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
              fluidPage(box("Placeholder")))
      ),
  )
)

server <- function(input, output) {
  filtered_df <- reactive(iris %>% filter(Species %in% input$v_species))
  show_ci <- reactive(input$iris_ci %>% as.logical)
  
  output$iris_plot = renderPlot({
    
    this_title <- str_interp("Sepal Width Vs. Sepal Length for ${input$v_species}")
    fit <- lm(Sepal.Width ~ Sepal.Length, data = filtered_df())
    lm_annot <- str_interp("y == ${fit$coefficients['(Intercept)']} + ${fit$coefficients['Sepal.Length']} * x")
    
    ggplot(filtered_df(), aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point() +
      geom_smooth(method = 'lm', se = show_ci()) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = this_title,
        x = "Sepal Width",
        y = "Sepal Length"
      ) +
      annotate(
        "text", 
        x = max(filtered_df()$Sepal.Length), 
        y = min(filtered_df()$Sepal.Width), 
        hjust = 1, 
        vjust = 0, 
        label = lm_annot, 
        parse = TRUE,
        size = 6
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

