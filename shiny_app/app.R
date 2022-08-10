library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(reshape2)
library(R6)
library(foreach)
library(doParallel)
library(parallel)

source("Bee Parameters.R")
source("Graphing Functions.R")
source("Hive Cell Finder Functions.R")
source("Hive Setup Functions.R")
source("Honey Pollen Brood Functions.R")
source("Queen Def.R")
source("Final Measures.R")

hive <- make_set_hive()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bee Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "n",
                  label = "Queen Cells Per Hour:",
                  min = 60, max = 120,
                  value = 60, step = 10),
      
      sliderInput(inputId = "rb",
                  label = "Max Brood Radius",
                  min = 1, max = 4,
                  value = 3, step = 1),
      
      sliderInput(inputId = "rn",
                  label = "Necter Consumption Radius",
                  min = 1, max = 4,
                  value = 3, step = 1),
      
      sliderInput(inputId = "w",
                  label = "Total Daily Honey",
                  min = 1000, max = 4000,
                  value = 2000, step = 250),
      
      sliderInput(inputId = "pph",
                  label = "Pollen Ratio",
                  min = 0.2, max = 1,
                  value = 0.9, step = 0.1),
      
      sliderInput(inputId = "ph",
                  label = "Honey Consumption Ratio",
                  min = 0.9, max = 1.1,
                  value = 1, step = 0.05),
      
      sliderInput(inputId = "pp",
                  label = "Pollen Consumption Ratio",
                  min = 0.9, max = 1.1,
                  value = 1, step = 0.05),
      
      sliderInput(inputId = "N_DAYS",
                  label = "Days to Run",
                  min = 1, max = 60,
                  value = 30, step = 5),
      
      actionButton("new_hive", "Reset Hive")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot", height = "800px")
      
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(1000)
  
  values <- reactiveValues(hive = NULL)

  # observeEvent(input$new_hive, {
  #   values$hive <- make_set_hive()
  # })
  
  observe({
    isolate({
      for(i in 1:5){
        values$hive <- make_set_hive()
      }
    })
    if (1 < 2){
      invalidateLater(0, session)
    }
  })
  
  output$distPlot <- renderPlot({
    hive_matrix_to_graph(values$hive,c())
  })
  
}



# Create Shiny app ----
shinyApp(ui = ui, server = server)