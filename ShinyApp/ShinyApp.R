library(shiny)
library(ggplot2)
library(dplyr)

#reading the csv in the vavriable
bcl <- read.csv("sentiment.csv", stringsAsFactors = FALSE)

# Code for Creating the ui of the app. 
ui <- fluidPage(
  #Title of the app
  titlePanel("Sentiments Histogram"),
 
  # Sidebar with controls to select a sentiment input,airline input and the input of negative reason.
    sidebarPanel(
      sliderInput("sentimentInput", "airline_sentiment_confidence", 0.0, 1.0, c(0.05, 0.45), pre = "#"),
      #creating an action button with label goButton
      actionButton("goButton", "Go!"),
      #Using function plotOutput with id distPlot to create a plot
      plotOutput("distPlot"),
      #Creating radioButtons of type airline keeping United as default
      radioButtons("typeInput", "airline",
                   choices = c("Virgin America","United","Southwest","Delta","US Airways","American"),
                   selected = "United"),
      #Creating a dropdown menu with various negative reasons
      selectInput("nreasonInput", "negativereason",
                  choices = c("Bad Flight","Can't Tell","Late Flight","Customer Service Issue","Flight Booking Problems","Lost Luggage","Flight Attendant Complaints","Cancelled Flight","Damaged Luggage","longlines")) 
      ),
       
  # Show the table and plot of the requested variables
       mainPanel(
         #Using function plotOutput with id coolplot to create a plot
      plotOutput("coolplot"),
      br(), br(),
      #Using function tableOutput with id results to create a table
      tableOutput("results")
  )
)

# Define server logic to plot various variables
server <- function(input, output) {
  
  # Generate a plot of the requested variable after filtering
  output$coolplot <- renderPlot({
    filtered <-
      bcl %>%
      #Filter the data according to the user inputs
      filter(airline_sentiment_confidence >= input$sentimentInput[1],
             airline_sentiment_confidence <= input$sentimentInput[2],
             airline==input$typeInput,
             negativereason==input$nreasonInput
      )         
    
    #create a ggplot histogram as per the filtered variable against airline sentiment confidence 
    ggplot(filtered, aes(airline_sentiment_confidence)) +
      geom_histogram()
  })
  
  
  # Generate a table of the requested variable after filtering
  output$results <- renderTable({
    filtered <-
      bcl %>%
      #Filter the data according to the user inputs
      filter(airline_sentiment_confidence >= input$sentimentInput[1],
             airline_sentiment_confidence <= input$sentimentInput[2],
             airline==input$typeInput,
             negativereason==input$nreasonInput
            )
    #displaying the table
    filtered
  })
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    dist <-isolate((input$sentimentInput))
    boxplot(dist)
  })
  
}

shinyApp(ui = ui, server = server)
