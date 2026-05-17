library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

df <- read.csv("data/train.csv")

ui <- navbarPage(
  "Ames Explorer",
  tabPanel(
    "Overview",
    sidebarLayout(
      sidebarPanel(
        h4("Filter Neighborhoods"),
        selectInput("input1", "Neighborhood(s):",
          choices = unique(df$Neighborhood),
          selected = c("NAmes", "Edwards", "BrkSide"),
          multiple = TRUE
        ),
        br(),
        sliderInput("input2", "Overall Quality (1–10):",
          min = min(df$OverallQual, na.rm = TRUE),
          max = max(df$OverallQual, na.rm = TRUE),
          value = range(df$OverallQual, na.rm = TRUE),
          step = 1,
          ticks = FALSE
        ),
        helpText("Use the dropdown and slider to filter the data below.")
      ),
      mainPanel(
        fluidRow(
          column(4, value_box(
            title = "Oldest Home",
            value = textOutput("oldest_home"),
            theme = "bg-primary"
          )),
          column(4, value_box(
            title = "Common Quality",
            value = textOutput("top_quality"),
            theme = "bg-primary"
          )),
          column(4, value_box(
            title = "Total Homes",
            value = textOutput("home_count"),
            theme = "bg-primary"
          ))
        ),
        br(),
        fluidRow(
          column(6, plotlyOutput("price_hist")),
          column(6, plotlyOutput("scatter_plot"))
        )
      )
    )
  ),
  tabPanel(
    "Data",
    fluidPage(
      h4("Data"),
      DTOutput("datatable")
    )
  )
)

server <- function(input, output) {
  # data
  filtered_data <- reactive({
    req(input$input1)
    df[df$Neighborhood %in% input$input1 &
      df$OverallQual >= input$input2[1] &
      df$OverallQual <= input$input2[2], ]
  })

  # summary
  output$oldest_home <- renderText({
    formatC(min(filtered_data()$YearBuilt, na.rm = TRUE), format = "d")
  })

  output$top_quality <- renderText({
    as.character(names(sort(table(filtered_data()$OverallQual), decreasing = TRUE))[1])
  })

  output$home_count <- renderText({
    formatC(nrow(filtered_data()), format = "d", big.mark = ",")
  })

  # histogram
  output$price_hist <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(SalePrice)) +
        geom_histogram(fill = "#ff9933", color = "white", bins = 25) +
        scale_x_continuous(labels = dollar_format()) +
        theme_minimal() +
        labs(title = "How Prices Stack Up", x = "Sale Price", y = "Frequency")
    )
  })

  # scatter
  output$scatter_plot <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) +
        geom_point(size = 2) +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_continuous(labels = comma_format()) +
        theme_minimal() +
        labs(title = "Bigger = Pricier?", x = "Living Area (sq ft)", y = "Price")
    )
  })

  # data
  output$datatable <- renderDataTable({
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
