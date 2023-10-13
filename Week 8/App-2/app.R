library(shiny)
library(tidyverse)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  h1("Challenge 8"),
  
  # App title ----
  titlePanel(strong("Datasets of Rock/Pressure/Car")),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      br(),
      img(src = "Rock.jpg", height = 200, width = 200),
      br(),
      span("This is a picture of a rock", style = "color:grey"),
      br(),
      img(src = "Pressure.jpg", height = 200, width = 200),
      br(),
      span("This is a picture of a pressure guage", style = "color:blue"),
      br(),
      img(src = "Car.jpg", height = 200, width = 200),
      br(),
      span("This is a picture of a car", style = "color:red"),
     
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 5)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      
      plotOutput(outputId = "plot")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
 
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
    output$plot <- renderPlot({
      datasetInput()
  
      
     ggplot(rock, aes(x = area)) +
    geom_density(adjust = 2) +
    labs( x = "Area", y = "Density", title = "Relationship between Area and peri" )
  
     
    ggplot(pressure, aes(x = temperature)) +
      geom_density(adjust = 2) +
      labs( x = "Temperature", y = "Density", title = "abc" )
   
      ggplot(cars, aes(x = speed)) +
        geom_density(adjust = 2) +
        labs( x = "Speed", y = "Density", title = "def" )
   
       })
  
  }

# Create Shiny app ----
shinyApp(ui = ui, server = server)

  