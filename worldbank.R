library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(httr)
library(jsonlite)


load("country_list.rda")
load("id_list.rda")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Comparison"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      fluidRow(
        box(
          width = NULL,
          background = "purple",
          tags$h2("fasduıhfasıofas"),
          "asdhıouasoıhuojısdaojıasdoıj"
        )
      ),
      fluidRow(
        column(8,
               fluidRow(
                 box(
                   "graph",
                   plotOutput("plot1"),
                   background = "purple",
                   width = NULL
                 )
               ),
               fluidRow(
                 column(4,
                        box(
                          selectizeInput(
                            "con_1",
                            "Country or Group 1:",
                            choices = country_list$country_name,
                            selected = "Belgium",
                            multiple = FALSE,
                            options = list(
                              placeholder = "Type to start...",
                              create = FALSE,
                              closeAfterSelect = TRUE
                            )
                          ),
                          background = "purple",
                          width = NULL
                        )
                 ),
                 column(4,
                        box(
                          selectizeInput(
                            "con_2",
                            "Country or Group 2:",
                            choices = country_list$country_name,
                            selected = "Netherlands",
                            multiple = FALSE,
                            options = list(
                              placeholder = "Type to start...",
                              create = FALSE,
                              closeAfterSelect = TRUE
                            )
                          ),
                          background = "purple",
                          width = NULL
                        )
                 ),
                 column(4,
                        box(
                          selectizeInput(
                            "ind",
                            "Indicator:",
                            choices = id_list$id_name,
                            selected = "Unemployed (%)",
                            multiple = FALSE,
                            options = list(
                              placeholder = "Type to start...",
                              create = FALSE,
                              closeAfterSelect = TRUE
                            )
                          ),
                          background = "purple",
                          width = NULL
                        )
                 )
               ),
               fluidRow(
                 box(
                   "info and errors",
                   background = "purple",
                   width = NULL
                 )
               )
        ),
        column(4,
               box(
                 "explanation",
                 width = NULL
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  plot_1_re <- reactive({
    country_1_code <- country_list$country_code[country_list$country_name == input$con_1]
    country_2_code <- country_list$country_code[country_list$country_name == input$con_2]
    indicator_code <- id_list$id_code[id_list$id_name == input$ind]
    
    url <- paste0("https://api.worldbank.org/v2/country/", country_1_code, ";", country_2_code, "/indicator/", indicator_code, "?format=json&per_page=300")
    
    response <- GET(url) %>% 
      content(as = "text") %>%
      fromJSON() %>%
      .[[2]] %>% 
      as_tibble() %>% 
      mutate(indicator_id_2 = indicator$id, 
             indicator_value_2 = indicator$value, 
             country_id_2 = country$id, 
             country_name_2 = country$value) %>% 
      select(indicator_id_2, indicator_value_2, country_id_2, country_name_2, date, value)
    
  })
  
  output$plot1 <- renderPlot({
    plot_1_data <- plot_1_re()

    ggplot(plot_1_data, aes(x = date, y = value)) +
      geom_line()
  })
}

shinyApp(ui, server)