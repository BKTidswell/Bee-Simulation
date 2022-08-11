library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(reshape2)
library(R6)

source("Bee Parameters.R")
source("Graphing Functions.R")
source("Hive Cell Finder Functions.R")
source("Hive Setup Functions.R")
source("Honey Pollen Brood Functions.R")
source("Queen Def.R")
source("Final Measures.R")
source("Simulation Script.R")

hive <- make_set_hive()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Bee Simulation", align = "center")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 4,
      
      tabsetPanel(id = "tabset",
        tabPanel("Setup",
           sliderInput(inputId = "brood_rad",
                       label = "Brood Radius",
                       min = 1, max = 10,
                       value = 7, step = 1),
           
           sliderInput(inputId = "pollen_rad",
                       label = "Pollen Radius",
                       min = 1, max = 4,
                       value = 2, step = 1),
           
           checkboxInput("empty_start", "Empty Start", value = FALSE),
           
           actionButton("reset_hive", "Remake Hive"),
           
           
        ),
        tabPanel("Parameters",
          # Input: Slider for the number of bins ----
          sliderInput(inputId = "n",
                      label = "Queen Cells Visited Per Hour:",
                      min = 15, max = 40,
                      value = 20, step = 5),
          
          sliderInput(inputId = "rb",
                      label = "How Far Can Brood Be from Other Brood?",
                      min = 1, max = 4,
                      value = 3, step = 1),
          
          # sliderInput(inputId = "rn",
          #             label = "Necter Consumption Radius",
          #             min = 1, max = 4,
          #             value = 3, step = 1),
          
          sliderInput(inputId = "w",
                      label = "Total Daily Honey",
                      min = 200, max = 1000,
                      value = 300, step = 100),
          
          sliderInput(inputId = "pph",
                      label = "Pollen to Honey Ratio",
                      min = 0.2, max = 1,
                      value = 0.9, step = 0.1),
          
          sliderInput(inputId = "ph",
                      label = "Honey Eaten to Collected Ratio",
                      min = 0.9, max = 1.1,
                      value = 1, step = 0.05),
          
          sliderInput(inputId = "pp",
                      label = "Pollen Eaten to Collected Ratio",
                      min = 0.9, max = 1.1,
                      value = 1, step = 0.05),
          
          sliderInput(inputId = "N_DAYS",
                      label = "Days to Run",
                      min = 1, max = 60,
                      value = 30, step = 5),
          
          actionButton("run_hive", "Run Simulation"),
          
          actionButton("pause_hive", "Pause Simulation")
        )
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 8,
              
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Hive", textOutput("number_days"),
                                   plotOutput(outputId = "hivePlot")),
                  tabPanel("Contents", plotOutput(outputId = "trendPlot"))
      ),
    
      tags$style(type='text/css', "#number_days { text-align: center; font-size: 30px; display: block;}")
      
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  values <- reactiveValues()
  values$hive <- make_set_hive()
  values$queen <- Queen$new(median(hexdat_centers$x),median(hexdat_centers$y))
  values$counter <- 9999
  
  observeEvent(input$run_hive, {
    #n
    QUEEN_CELLS_PER_HOUR <<- input$n
    
    #r_b
    BROOD_RADIUS <<- input$rb
    #r_n
    NECTER_CONSUMP_RAD <<- input$rn
    
    #omega
    TOTAL_DAILY_HONEY <<- input$w
    
    #p_ph
    POLLEN_RATIO <<- input$pph
    
    #p_h
    HONEY_CONSUMPTION_RATIO <<- input$ph
    #p_p
    POLLEN_CONSUMPTION_RATIO <<- input$pp
    
    #k
    K <<- input$k
    
    #Only collecting during the day
    HONEY_BY_HOUR <<- ceiling(TOTAL_DAILY_HONEY/12)
    POLLEN_BY_HOUR <<- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO)/12)
    
    HONEY_EATEN_PER_HOUR <<- ceiling((TOTAL_DAILY_HONEY*HONEY_CONSUMPTION_RATIO)/24)
    POLLEN_EATEN_PER_HOUR <<- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO*POLLEN_CONSUMPTION_RATIO)/24)
    
    values$Count_Contents <<- tibble(Brood = length(which(values$hive[,,1] == BROOD)),
                             Honey = length(which(values$hive[,,1] == HONEY)),
                             Pollen = length(which(values$hive[,,1] == POLLEN)),
                             Empty = length(which(values$hive[,,1] == EMPTY)))
    
    values$counter <- 0
  })
  
  observeEvent(input$pause_hive, {
    values$counter <- 9999
  })
  
  observeEvent(input$reset_hive, {
    BROOD_RADIUS <<- input$brood_rad
    POLLEN_WIDTH <<- input$pollen_rad
    
    if(input$empty_start){
      values$hive <- make_empty_hive()
    } else{
      values$hive <- make_set_hive()
    }
    
    values$queen <- Queen$new(median(hexdat_centers$x),median(hexdat_centers$y))
    values$counter <- 9999
  })
  
  observe({
    isolate({
      if (values$counter < input$N_DAYS*24){
        values$counter <- values$counter + 1
        hour <- ((values$counter-1) %% 24) + 1
        values$hive <- runOneHour(values$hive,values$queen,hour)
        
        values$Count_Contents <<- values$Count_Contents %>% add_row(Brood = length(which(values$hive[,,1] == BROOD)),
                                                     Honey = length(which(values$hive[,,1] == HONEY)),
                                                     Pollen = length(which(values$hive[,,1] == POLLEN)),
                                                     Empty = length(which(values$hive[,,1] == EMPTY)))
      }
    })
    if (values$counter < input$N_DAYS*24){
      invalidateLater(0, session)
    }
  })
  
  output$hivePlot <- renderPlot({
    hive_matrix_to_graph(values$hive,c())
  }, height = 800)
  
  output$trendPlot <- renderPlot({
    graph_trends(values$Count_Contents,input$N_DAYS)
  }, height = 800)
  
  output$number_days <- renderText({
    if(values$counter == 9999){
      paste("Day", "0")
    } else{
      paste("Day", ceiling(values$counter/24))
    }
  })
  
}



# Create Shiny app ----
shinyApp(ui = ui, server = server)