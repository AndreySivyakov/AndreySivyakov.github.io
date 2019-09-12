library(shiny)
library(ggplot2)
library(dplyr)

#Read the pitching data
pitching <- read.csv('pitching.csv')[, -1]


# Set up the UI to show various pitching variables
ui <- fluidPage(
  headerPanel("Baseball Salary Analysis"),
  
  
  sidebarPanel(
    selectInput("variable", "Variable:",
                list(
                     "All" = "All Variables",
                     "Strike Out" = "SO",
                     "Walks" = "BB", 
                     "Errors" = "E", 
                     "Saves" = "SV",
                     "Games" = "G",         
                     "Wins" = "W",
                     "Hit By Pitch" = "HBP",
                     "Wild Pitches" = "WP",
                     "Grounded into double plays" = "GIDP",
                     "Intentional walks" = "IBB"))

  ),
  
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("mpgPlot")
  )
)

# Add the logic to show the scatter plot on the defined variables above against salary
server <- function(input, output) {
  
  formulaText <- reactive({
    paste("Variable:", input$variable,
          "against Salary")
  })
  
  
  output$caption <- renderText({
    formulaText()
  })
  
  
  output$mpgPlot <- renderPlot({
   
    
    pitching.2011_15 <- pitching[which(pitching$salaryYear != 2016), ]
    
    # Use selected variable
    if(input$variable != "All Variables")
    df <- pitching.2011_15[, c(input$variable,'salary')]
    else # Use all variables if option "all" is chosen
      df <- pitching.2011_15[, c('salary', 'SO', 'BB', 'E', 'SV', 'G', "W",
                                 'HBP', 'WP', 'GIDP', 'IBB')]
    
    plot(df)
    
  }, width=600, height=500)  
  
}

shinyApp(ui = ui, server = server)


