#---
#title: "Crab Age Analysis Shiny App"
#author: "Johnny Vogt and Micheal Flores"
#date: "04/11/2025"
#runtime: shiny
#---

library(shiny)
library(shinythemes)
library(readr)
#Pre-Loaded Crab Data
crab_age <- read.csv("C:/Users/jvogt/Desktop/School/SMU/git/School/DS6306/project_2/Crab_Age/train-1.csv")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Crab Age Analysis",
    tabPanel("Plots and Graphs",
             sidebarPanel(
               selectInput("select_sp", label = h3("Explanatory Variable (Only ScatterPlot)"), 
                               choices = list("Length", "Diameter", "Height", "Weight","Shucked Weight", "Viscera Weight", "Shell Weight"), 
                               selected = "Length"),
               selectInput("select_bp", label = h3("Explanatory Variable (Only Boxplot)"), 
                           choices = list("Sex","Length", "Diameter", "Height", "Weight","Shucked Weight", "Viscera Weight", "Shell Weight"), 
                           selected = "Length"),
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Scatter Point Graph",
                          h4("Explanatory Crab Feature vs. Crab Age"),
                          plotOutput(outputId = "scatterplot"),
                 ),
                 tabPanel("Box Plot", 
                          h4("Box plot of Crab Features vs. Crab Age"),
                          plotOutput(outputId = "boxplot"),
                 ),
                 tabPanel("Correlation Map",
                          "Map the correlations")
               )
             )
    ),
    tabPanel("Averages", 
             "This page will show the averages and trends found during the EDA"),
    tabPanel("Prediction Model", 
             "Plan on putting our Regression model here and have users us a slider to see how it affects age."),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterplot <- renderPlot({
      #Creating Scatter Plot for 
      crab_age %>% 
        ggplot(aes(y = Age, x = !!sym(input$select_sp))) +
        geom_point(position = "jitter") +
        geom_smooth(type = "lm")
    })
    output$boxplot <- renderPlot({
      #Creating Box Plot
      crab_age %>%
        ggplot(aes(x = Age, Y = !!sym(input$select_bp))) +
        geom_boxplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
