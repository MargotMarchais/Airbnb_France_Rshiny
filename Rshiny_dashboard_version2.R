###################################
#       AIRBNB DASHBOARD          #
#                                 #
# Case study: The French market   #
# By Margot MARCHAIS--MAURICE     #
#                                 #
###################################

# Load packages
#library(rsconnect)
library(scales)
library(htmlwidgets)
library(htmltools)
library(shiny)
require(shinydashboard)
library(ggplot2)
require(scales)
library(binr)
library(tidyr)
library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)}


# Load databases
g = listings
BAN_listings = BAN_listings
#gen_figures = gen_figures

#Disable scientific notation
options(scipen=999)


# SERVER  -----------------------------------------

server = function(input, output) {
  
  # Outputs --------------------
  
  #######################
  # 1. SERVER: Overview #
  #######################
  
  # 1.1. BANs
  
  # BAN : Number of French cities
  output$nb_cities = renderValueBox({
    valueBox(format(sum(BAN_listings$nb_cities), big.mark = ",")
             , "Cities", color = "maroon")
  })
  
  # BAN : Number of listings
  output$nb_listings = renderValueBox({
    valueBox(format(sum(BAN_listings$nb_listings), big.mark = ",")
             , "Total number of listings (2022)", color = "maroon")
  })

  # BAN : Number of hosts
  # output$nb_hosts = renderValueBox({
  #   valueBox(format(sum(BAN_listings$nb_hosts), big.mark = ",")
  #            , "Hosts", color = "blue")
  # })
  
  
  # BAN : Number of reviews
  output$nb_reviews = renderValueBox({
    valueBox(format(sum(BAN_reviews$nb_reviews), big.mark = ",")
             , "Total number of reviews (2021)", color = "maroon")
  })
  
  
  # 1.2. Evolution graphs
  
  # Graph n°1: Combien d'annonces par.... (année, ville, quartier, type de propriété, etc)
  output$gen_quantities <-renderPlot({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    gen = gen_figures  %>% filter(year <= upper_bound & year >= lower_bound)
    ggplot(data = gen, aes(x = year, y = quantities, fill = year)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip()+
      scale_fill_brewer()+
      theme_bw(base_size = 10) + theme(legend.position="none")+
      labs(x = "Year", y = "Sold quantities (in units)", title="Seller sold quantities per year (in units)")+
      geom_label(aes(label=comma(quantities), hjust = +0.7, fontface=2))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$bar_chart <- renderPlot({
    ggplot(data = listings, aes_string(x = input$x, y = input$y, fill=input$z)) +
      geom_bar(stat = "identity") + 
      coord_flip() 
  })
  
  
  ##################
  ##################
  # TYPES OVERVIEW #
  ##################
  ##################
  
  # QUANTITIES COLUMN #
  
  output$type_share_quantities = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    year_tot = overview_types %>% filter(year <= upper_bound & year >= lower_bound)
    cluster_types = overview_types %>% filter(year <= upper_bound & year >= lower_bound & Type == input$Type)
    total_quantities = sum(year_tot$total_sales_quantities)
    share_cluster = sum(cluster_types$total_sales_quantities) / total_quantities
    infoBox("Share of quantities",paste0(format(round(share_cluster*100),big.mark = ","), "%"), 
            color = "purple", fill = TRUE)
    
  })
  
  output$type_total_quantities = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
    infoBox(
      "Sold quantities", paste0(format(comma(sum(df$total_sales_quantities)))), "units",
      color = "purple", icon = icon("cart-arrow-down")
    )
  })
  
  output$type_average_quantities = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
    infoBox(
      "Average quantity per purchase", paste0(format(comma(mean(df$avg_number_quantities_per_purchase)))), "units",
      color = "purple", icon = icon("boxes")
    )
  })
  
  
  # REVENUES COLUMN #
  
  output$type_share_amount = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    year_tot = overview_types %>% filter(year <= upper_bound & year >= lower_bound)
    cluster_types = overview_types %>% filter(year <= upper_bound & year >= lower_bound & Type == input$Type)
    total_revenues = sum(year_tot$total_seller_sales_ttc_euro)
    share_cluster = sum(cluster_types$total_seller_sales_ttc_euro) / total_revenues
    infoBox("Share of amount",paste0(format(round(share_cluster*100),big.mark = ","), "%"),
            color = "blue", fill = TRUE)
  })
  
  output$type_total_amount = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
    infoBox(
      "Sales Revenues", paste0(format(comma(sum(df$total_seller_sales_ttc_euro)))), "euro",
      color = "purple", icon = icon("euro")
    )
  })
  
  output$type_average_amount = renderInfoBox({
    lower_bound = input$year[1]
    upper_bound = input$year[2]
    df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
    infoBox(
      "Mean price", paste0(format(comma(mean(df$mean_price_eur), na.rm=TRUE))), "euro",
      color = "purple", icon = icon("coins")
    )
  })
  
  
  
}





##  UI------------------------------------------------

#HEADER---------------------
header = dashboardHeader(
  title = 'Airbnb France'
)

#SIDEBAR--------------------
sidebar = dashboardSidebar(
  sidebarMenu(id = "tabs",
              sliderInput("year", "Year", min(g$year), max(g$year)+1,value=range(g$year),
                          step = 1),
              
              # Select variable for x-axis
              selectInput(inputId = "x", 
                          label = "Parameter 1:",
                          choices = c("city", "neighbourhood_cleansed", "property_type"), 
                          selected = "city"),
              
              # Select variable for y-axis
              selectInput(inputId = "y", 
                          label = "Parameter 2:",
                          choices = c("price", "beds"), 
                          selected = "price"),
              
              # Select variable for color
              selectInput(inputId = "z", 
                          label = "Color code:",
                          choices = c("city", "neighbourhood_cleansed", "property_type"), 
                          selected = "city"),
              menuItem(text = "Overview", tabName = "gen_fig"),
              menuItem(text = "Types focus", tabName ="focus_type"
              )
  )
)

#BODY--------------------
body = dashboardBody(
  tabItems(
    tabItem(
      tabName = 'gen_fig',
      fluidRow(
        infoBoxOutput("nb_cities"),
        infoBoxOutput("nb_listings"),
        #infoBoxOutput("nb_hosts"),
        infoBoxOutput("nb_reviews"),
        
        box(
          title = "Number of products sold per year", status = "primary",
          plotOutput("bar_chart", height = 250)
        ),
        box(
          title = "Sales revenues per year", status = "primary",
          plotOutput("gen_revenues", height = 250)
        )
      )
    ),
    
    tabItem(
      tabName = "focus_type",
      fluidRow(
        selectInput(inputId = "Type", "Product Category Type",
                    choices =unique(overview_types$Type),
                    selected = "Bio"),
        column(width = 4,
               box(width = NULL, background = 'blue', "Quantities"),
               infoBoxOutput("type_share_quantities", width = NULL),
               infoBoxOutput("type_total_quantities", width = NULL),
               infoBoxOutput("type_average_quantities", width = NULL)
        ),
        column(width = 4,
               box(width = NULL, background = 'red', "Revenues"),
               infoBoxOutput("type_share_amount", width = NULL),
               infoBoxOutput("type_total_amount", width = NULL),
               infoBoxOutput("type_average_amount", width = NULL)
        )
      )
    )
    
  )
)






#UI
ui = dashboardPage(skin = 'yellow',
                   header,
                   sidebar,
                   body
)
shinyApp(ui = ui, server = server)
