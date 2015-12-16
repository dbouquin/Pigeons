

library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pigeon Racing"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("Analysis", "Select Analysis", 
                               choices = list("Select Analysis" = 0,"Color vs Speed Analysis" = 1, "Significance Test(Residual vs Fitted)" = 2,
                                              "RandomForest Classification Analysis" = 3, "Correlation Plot for all Variables" = 4,
                                              "K-Mode Cluster Analysis" = 5,  "War Pigeon vs Racing Pigeon" = 6)),
    
      
      br(),
      selectInput("PigF", "Pigeon Fast Facts", 
                  choices = list("Select Facts" = 7,"US Pigeon Teams" = 8, "Fastest Young Pigeon" = 9,
                                 "Fastest Old Pigeon" = 10, "Pigeon vs Other Animal Speed" = 11
                                 ))
      
      
      
          ),
    
    
    
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary"))
                 # tabPanel('Table', dataTableOutput("table"))
      )
      
      
      
      )
  )
))

