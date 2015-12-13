library(shiny)

# Data For Dickin Medal Winners
War_url <- read.csv(file="https://raw.githubusercontent.com/dbouquin/Pigeons/master/Dickin_Medal_Pigeons.csv", header=TRUE, sep=",")
Medal_tb <- subset(War_url, Distance.mi. > 1, select=c(-X,-Animal, -Time.min.))

# Data For Racing Pigeons Summary
Combined_url <- read.csv(file="https://raw.githubusercontent.com/dbouquin/Pigeons/master/Combined_Racing_Pigeons_Table.csv", header=TRUE, sep=",")
Combined_url$DISTANCE_MILES <- as.numeric(as.character(Combined_url$DISTANCE_MILES))
Combined_tb <- subset(Combined_url, DISTANCE_MILES > 34, select=(-X))


shinyServer(function(input, output) {

  # Dickin Medal Winners code: 
  output$table <- renderTable({
    data <- Medal_tb
    if (input$rec != "All"){
      data <- data[data$Recipiant == input$rec,]
    }
    data
  })

  # Racing Pigeons Summary code:
  datasetInput <- reactive({
    data <- Combined_tb
    if (input$sex != "All"){
      data <- data[data$SEX == input$sex,]
    }
    if (input$color != "All"){
      data <- data[data$COLOR == input$color,]
    }
    data
  })  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  }) 
    
})
