library(shiny)

# Data For Dickin Medal Winners
War_url <- read.csv(file="https://raw.githubusercontent.com/dbouquin/Pigeons/master/Dickin_Medal_Pigeons.csv", header=TRUE, sep=",")
Medal_tb <- subset(War_url, Distance.mi. > 1, select=c(-X,-Animal, -Time.min.))

# Data For Racing Pigeons Summary
Combined_url <- read.csv(file="https://raw.githubusercontent.com/dbouquin/Pigeons/master/Combined_Racing_Pigeons_Table.csv", header=TRUE, sep=",")
Combined_url$DISTANCE_MILES <- as.numeric(as.character(Combined_url$DISTANCE_MILES))
Combined_tb <- subset(Combined_url, DISTANCE_MILES > 34, select=(-X))



shinyUI(fluidPage(

    # Dickin Medal Winners code:
    titlePanel("Dickin Medal Winners (with known distance flown)"),
    a(href="http://www.rpra.org/pigeon-history/pigeons-in-war/", target="_blank", "Link to the Dickin Medal winning pigeons' pictures."),
    fluidRow(
      column(4, 
             selectInput("rec", 
                         "Recipiant:", 
                         c("All", 
                           unique(as.character(Medal_tb$Recipiant))))
      )
    ),
      fluidRow(
      tableOutput(outputId="table")
    ),
    
    # Racing Pigeons Summary code:
    titlePanel("Racing Pigeons (with known distance flown)"),
    p(strong("The data was originally obtained from the folling two links:")),
    a("American Racing Pigeon Union,", href="http://www.pigeon-ndb.com/ndb.php", target="_blank"),
    a("Racing Pigeon Race Results Database", href="http://www.racingpigeonmall.com/racdata/", target="_blank"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("sex", 
                    "Pigeon's sex:", 
                    c("All", 
                      unique(as.character(Combined_tb$SEX)))
        ),
        selectInput("color", 
                    "Pigeon's Color:", 
                    c("All", 
                      unique(as.character(Combined_tb$COLOR)))
        ),
        
        numericInput("obs", "Number of observations to view:", 10),
        
        helpText("Note: While the data view will show only the specified",
                 "number of observations, the summary will still be based",
                 "on the total selected obervation.")
      ),
      
      mainPanel(
        h4("Summary"),
        verbatimTextOutput("summary"),
        
        h4("Observations"),
        tableOutput("view")
      )
    )
))
