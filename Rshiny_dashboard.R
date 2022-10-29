############################################
# UN SCATTERPLOT REACTIF AVEC DEUX SLIDERS #
############################################

library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)

# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  theme=shinytheme("cosmo"),
  
  # App title
  titlePanel("Airbnb France Dashboard", windowTitle = "Airbnb"),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Overview"),      # Third level header: Plotting
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "Parameter 1:",
                  choices = c("neighbourhood_cleansed", "property_type", "room_type"), 
                  selected = "neighbourhood_cleansed"),
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Parameter 2:",
                  choices = c("price", "accommodates"), 
                  selected = "price"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color code:",
                  choices = c("neighbourhood_cleansed", "property_type", "room_type", "source"), 
                  selected = "Type")
    ),
    
    # Outputs
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  
                  #Performance chart
                  
                  #Bar chart for exploratory results
                  tabPanel(title = "Exploratory results", 
                           plotOutput(outputId = "bar_chart")),
                  
                  # New tab panel for sources
                  tabPanel("Sources", 
                           br(),
                           htmlOutput(outputId = "sources"))
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$bar_chart <- renderPlot({
    ggplot(data = listings, aes_string(x = input$x, y = input$y, fill=input$z)) +
      geom_bar(stat = "identity") + 
      coord_flip() 
  })
  
  
  
  ###########
  # SOURCES #
  ###########
  
  #Create sources and credits
  output$sources <- renderUI({
    HTML(
      paste(tags$p("The data used for this dashboard were collected by Airbnb from XXXX until XXX."), 
            tags$p("Area: France (Paris, Bordeaux, Lyon)."),
            tags$p("Methodological remark: XXX")),
      "<br>",
      paste(tags$p("Rshiny app developed by Margot MARCHAIS--MAURICE in October 2022"))
    )
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
