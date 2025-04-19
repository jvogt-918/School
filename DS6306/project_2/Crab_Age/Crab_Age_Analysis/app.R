#---
#title: "Crab Age Analysis Shiny App"
#author: "Johnny Vogt and Micheal Flores"
#date: "04/11/2025"
#runtime: shiny
#---

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(bslib)

#Pre-Loaded Crab Data and transforming data
#crab_age <- read.csv("C:/Users/jvogt/Desktop/School/SMU/git/School/DS6306/project_2/Crab_Age/Crab_Age_Analysis/train-1.csv")
crab_age <- read.csv("data/train-1.csv")

#Facotring Sex
crab_age$sex <- as.factor(crab_age$Sex)

#Making Ft into Inches
crab_age$Length <- crab_age$Length * 12
crab_age$Diameter <- crab_age$Diameter * 12
crab_age$Height <- crab_age$Height * 12

#Creating three groups
male_crabs      <- filter(crab_age, Sex == "M")
female_crabs    <- filter(crab_age, Sex == "F")
immature_crabs  <- filter(crab_age, Sex == "I")

#User Interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Crab Age Analysis",
    tabPanel("Plots and Graphs",
             sidebarPanel(
               selectInput("select_sp", label = h3("Explanatory Variable (Only ScatterPlot)"), 
                               choices = list(
                                 "Length",
                                 "Diameter",
                                 "Height",
                                 "Weight",
                                 "Shucked Weight" = "Shucked.Weight",
                                 "Viscera Weight" = "Viscera.Weight",
                                 "Shell Weight" = "Shell.Weight"
                                 ), 
                               selected = "Length"),
               selectInput("select_bp", label = h3("Explanatory Variable (Only Boxplot)"), 
                           choices = list(
                             "Sex",
                             "Length",
                             "Diameter",
                             "Height",
                             "Weight",
                             "Shucked Weight" = "Shucked.Weight",
                             "Viscera Weight" = "Viscera.Weight",
                             "Shell Weight" = "Shell.Weight"
                             ), 
                           selected = "Length"),
               selectInput("select_vp_x", label = h3("Y Variable (Violin Plot)"), 
                           choices = list(
                             "Sex",
                             "Length",
                             "Diameter",
                             "Height",
                             "Weight",
                             "Shucked Weight" = "Shucked.Weight",
                             "Viscera Weight" = "Viscera.Weight",
                             "Shell Weight" = "Shell.Weight"
                             ), 
                           selected = "Length"),
               selectInput("select_vp_y", label = h3("X Variable (Violin Plot)"), 
                           choices = list(
                             "Age",
                             "Sex",
                             "Length", 
                             "Diameter", 
                             "Height", 
                             "Weight",
                             "Shucked Weight" = "Shucked.Weight", 
                             "Viscera Weight" = "Viscera.Weight", 
                             "Shell Weight" = "Shell.Weight"
                             ), 
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
                 tabPanel("Violin Plot",
                          plotOutput(outputId = "violin"),
                          "Map the correlations")
               ),
             )
    ),
    tabPanel("Averages",
             sidebarPanel(
               selectInput("select_mean_median", label = h3("Mean or Median"), 
                           choices = list(
                             "Mean",
                             "Median"
                           ), 
                           selected = "Mean"),
             ),
             layout_columns(
               card(
                 card_header("Averages seperated by Sex"),
                 card_body(
                   tableOutput("average")
               )
             ),
             "This page will show the averages and trends found during the EDA")
             ),
    tabPanel("Prediction Model",
             sidebarPanel(
               h3("Create your own model!"),
               selectInput("model_sex", label = h3("Training your model with...?"), 
                           choices = list(
                             "All" = "crab_age",
                             "Immature" = "immature_crabs",
                             "Female" = "female_crabs",
                             "Male" = "male_crabs")
               ),
               checkboxGroupInput(
                 "predictors",
                 "Predictors",
                 c(
                   "Length", 
                   "Diameter", 
                   "Height", 
                   "Weight",
                   "Shucked Weight" = "Shucked.Weight", 
                   "Viscera Weight" = "Viscera.Weight", 
                   "Shell Weight" = "Shell.Weight"
                 )),
               h3("Create your own crab"),
               h3("And We'll guess the Age!"),
               selectInput("sex", label = h3("Gender"), 
                             choices = list(
                               "Immature" = "I",
                               "Female" = "F",
                               "Male" = "M"), 
                           ),
                 sliderInput("length", "Carapace Length (Inches)",
                             min = 0.0, max = 30, value = 5),
                 sliderInput("diameter", "Carapace Diameter (Inches)",
                             min = 0.0, max = 30, value = 5),
                 sliderInput("height", "Crab Height (Inches)",
                             min = 0.0, max = 30, value = 5),
                 sliderInput("weight", "Crab Weight (lbs)",
                             min = 0, max = 50, value = 10),
                 sliderInput("shucked.weight", "Shucked Weight (lbs)",
                             min = 0, max = 50, value = 10),
                 sliderInput("viscera.weight", "Viscera Weight (lbs)",
                             min = 0, max = 50, value = 10),
                 sliderInput("shell.weight", "Shell Weight (lbs)",
                             min = 0, max = 50, value = 10),
               ),
              mainPanel(
                verbatimTextOutput("Model Summary"),
                plotOutput("MAEDist"),
                h4("Predicted Age:"),
                textOutput("prediction"),
                
              ),
             ""),
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scatterplot <- renderPlot({
      #Creating Scatter Plot for 
      crab_age %>% 
        ggplot(aes(y = Age, x = .data[[input$select_sp]])) +
        geom_point(position = "jitter") +
        geom_smooth() +
        labs(
          title = paste("Scatter Plot of", input$select_sp, "vs Age"),
          y = input$select_sp,
          x = "Age"
        )
    })
    output$boxplot <- renderPlot({
      #Creating Box Plot
      crab_age %>%
        ggplot(aes(x = Age, y = !!sym(input$select_bp))) +
        geom_boxplot()
    })
    output$violin <- renderPlot({
      #Creating Violin Plot Base
      crab_age %>%
        ggplot(aes(y = !!sym(input$select_vp_y), x = !!sym(input$select_vp_x))) +
        geom_violin()
    })
    
    output$average <- renderTable({
      func <- switch(input$select_mean_median,
                     "Mean" = mean,
                     "Median" = median)
      
      averages <- crab_age %>%
      group_by(Sex) %>%
      summarize(
        avg_Age = func(Age, na.rm = TRUE),
        avg_length = func(Length, na.rm = TRUE),
        avg_diameter = func(Diameter, na.rm = TRUE),
        avg_height = func(Height, na.rm = TRUE),
        avg_weight = func(Weight, na.rm = TRUE),
        avg_shweight = func(Shucked.Weight, na.rm = TRUE),
        avg_visweight = func(Viscera.Weight, na.rm = TRUE),
        avg_shlweight = func(Shell.Weight, na.rm = TRUE)
      ) 
      }, rownames = FALSE)
    
    
    #Predictive Model 
    formula <- reactive({
      if (input$model_sex == "crab_age") {
      as.formula(paste("Age ~", paste("Sex +",input$predictors, collapse = "+")))
      } else {as.formula(paste("Age ~", paste(input$predictors, collapse = "+")))
        }
    }) 
    
    lm_model <- reactive({
      lm(formula(), data = get(input$model_sex))
    })
    
    output$model_summary <- renderPrint({
      summary(lm_model())
    })
    
    output$prediction <- renderText({
      new_data <- data.frame(
        Sex = input$sex,
        Length = input$length,
        Diameter = input$diameter,
        Height = input$height,
        Weight = input$weight,
        Shucked.Weight = input$shucked.weight,
        Viscera.Weight = input$viscera.weight,
        Shell.Weight = input$shell.weight
        )
     pred <- predict(lm_model(), newdata = new_data)
     round(pred, 2)
    })
    
    #Creating Train set and a Test set and a loop to
    masterMAE <- reactiveVal()
    
    # Add this observer to calculate MAE when inputsa change
    observe({
      req(input$model_sex, input$predictors) # Ensure inputs are selected
      
      data_crab <- get(input$model_sex) # Get selected dataset
      iterations <- 500
      splitPerc <- 0.7
      mae <- numeric(iterations)
      
      for (j in 1:iterations) {
        trainIndices <- sample(1:nrow(data_crab), round(splitPerc * nrow(data_crab)))
        train <- data_crab[trainIndices, ]
        test <- data_crab[-trainIndices, ]
        
        model <- lm(formula(), data = train) # Use dynamic formula
        pred <- predict(model, newdata = test)
        mae[j] <- mean(abs(test$Age - pred))
      }
      
      masterMAE(mae) # Update reactive value
    })
    
    # Add this output for the MAE histogram
    output$MAEDist <- renderPlot({
      req(masterMAE()) # Ensure MAE data exists
      ggplot(data.frame(MAE = masterMAE()), aes(x = MAE)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        labs(title = "MAE Distribution Across 500 Iterations")
    })
    
                                                                       
}

# Run the application 
shinyApp(ui = ui, server = server)
