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
library(ggmosaic)
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
  
  ### 1.1. BANs ###
  
  # BAN : Number of French cities
  output$nb_cities = renderValueBox({
    valueBox(format(sum(BAN_listings$nb_cities), big.mark = ",")
             , "Cities", color = "navy")
  })

  # BAN : Number of hosts
   output$nb_hosts = renderValueBox({
     valueBox(format(sum(BAN_listings$nb_hosts), big.mark = ",")
              , "Hosts", color = "blue")
   })
  
   # BAN : Number of listings
   output$nb_listings = renderValueBox({
     valueBox(format(sum(BAN_listings$nb_listings), big.mark = ",")
              , "Total number of listings", color = "red")
   })
  
  # BAN : Number of reviews
  output$nb_reviews = renderValueBox({
    valueBox(format(sum(BAN_reviews$nb_reviews), big.mark = ",")
             , "Total number of reviews made in 2021", color = "maroon")
  })
  
  # BAN : Average satisfaction score
  output$avg_satcli = renderValueBox({
    valueBox(format(sum(BAN_listings$avg_satcli), big.mark = ",")
             , "Average satisfaction score (/5)", color = "purple")
  })
  
  # BAN : Average listings price
  output$avg_price = renderValueBox({
    valueBox(format(sum(BAN_listings$avg_price), big.mark = ",")
             , "Average price (in $, one night)", color = "yellow")
  })
  
  
  ### 1.2. Overview graphs ###
  
  # Graph n°1: Combien d'annonces par.... (année, ville, quartier, type de propriété, etc)
  # output$bar_chart <- renderPlot({
  #   ggplot(data = listings, aes_string(x = input$x, group = input$y, color=input$z)) +
  #     geom_bar(stat = "count") 
  # }, width = 800)
   # output$nb_listings_city <- renderPlot({
   #   ggplot(data = listings_per_city, aes_string(x = "city", y = "nb_listings", fill="room_type")) +
   #     geom_bar(stat = "identity") + 
   #     theme_bw(base_size = 10) + theme(legend.position="top") +
   #     geom_text(aes(label = nb_listings), vjust = 2 ) +
   #     labs(x = "City", y = "Number of listings")
   # })
   # 
   # output$nb_hosts_city <- renderPlot({
   #   ggplot(data = hosts_per_city, aes_string(x = "city", y = "nb_hosts", fill="room_type")) +
   #     geom_bar(stat = "identity") + 
   #     theme_bw(base_size = 10) + theme(legend.position="top") +
   #     geom_text(aes(label = nb_hosts), vjust = 2 ) +
   #     labs(x = "City", y = "Number of hosts")
   # })
   # 

  ###############################
  # 2. LISTINGS CHARACTERISTICS #
  ###############################
  
  # Graph: Number of listings per city
   output$nb_listings_city <- renderPlot({
      ggplot(data = listings_summary, aes_string(x = "city", y="nb_listings_charac", fill = "city")) +
        geom_col() + 
        #theme_bw(base_size = 10) + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="right") +
        geom_text(    aes(label = after_stat(y), group = city), 
                      stat = 'summary', fun = sum, vjust = +2) +
        labs(x = "City", y= "Number of Airbnb listings", title = "Paris: 6x times more Airbnb listings than any other big French city")
   })

  # Graph : Marimekko chart (room type and city)
  output$listings_mosaic <- renderPlot({   
   ggplot(data = listings) +
     geom_mosaic(aes(x = product(room_type, city), fill=room_type)) + 
     theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
     labs(x= "City", y= "Room type", title="Most listings offer the entire home/apartment service, whatever the city")
   })  
   
  #Graph: Top N (appliquer un reorder -> pas d'ordre alphabétique pour x)
  output$top_neighbo <- renderPlot({   
   ggplot(data = top_n(listings_summary, n=5, nb_listings_charac), 
         aes_string(x = "city_neighbourhood_cleansed", y="nb_listings_charac", fill = "city_neighbourhood_cleansed")) +
      geom_col() + 
      coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="right") +
      geom_text(    aes(label = after_stat(y), group = city), 
                  stat = 'summary', fun = sum, hjust = +2) +
      labs(x = "Neighbourhoods", y= "Number of Airbnb listings", title = "Neighbourhoods with the most listings are situated mostly in East Paris")
  }) 
  
  #Graph: Boxplot about the number of people the home can accommodate
  output$nb_accom <- renderPlot({   
    ggplot(listings, aes(x=room_type, y= accommodates, fill = city)) + geom_boxplot() +
      facet_grid(city ~ .) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="right") +
      labs(x = "City", y= "Number of guests the listing can accommodate", title = "")
  }) 
  

  
  
  # # QUANTITIES COLUMN #
  # 
  # # output$type_share_quantities = renderInfoBox({
  # #   lower_bound = input$year[1]
  # #   upper_bound = input$year[2]
  # #   year_tot = overview_types %>% filter(year <= upper_bound & year >= lower_bound)
  # #   cluster_types = overview_types %>% filter(year <= upper_bound & year >= lower_bound & Type == input$Type)
  # #   total_quantities = sum(year_tot$total_sales_quantities)
  # #   share_cluster = sum(cluster_types$total_sales_quantities) / total_quantities
  # #   infoBox("Share of quantities",paste0(format(round(share_cluster*100),big.mark = ","), "%"), 
  # #           color = "purple", fill = TRUE)
  # #   
  # # })
  # 
  # output$type_total_quantities = renderInfoBox({
  #   lower_bound = input$year[1]
  #   upper_bound = input$year[2]
  #   df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
  #   infoBox(
  #     "Sold quantities", paste0(format(comma(sum(df$total_sales_quantities)))), "units",
  #     color = "purple", icon = icon("cart-arrow-down")
  #   )
  # })
  # 
  # output$type_average_quantities = renderInfoBox({
  #   lower_bound = input$year[1]
  #   upper_bound = input$year[2]
  #   df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
  #   infoBox(
  #     "Average quantity per purchase", paste0(format(comma(mean(df$avg_number_quantities_per_purchase)))), "units",
  #     color = "purple", icon = icon("boxes")
  #   )
  # })
  # 
  # 
  # # REVENUES COLUMN #
  # 
  # output$type_share_amount = renderInfoBox({
  #   lower_bound = input$year[1]
  #   upper_bound = input$year[2]
  #   year_tot = overview_types %>% filter(year <= upper_bound & year >= lower_bound)
  #   cluster_types = overview_types %>% filter(year <= upper_bound & year >= lower_bound & Type == input$Type)
  #   total_revenues = sum(year_tot$total_seller_sales_ttc_euro)
  #   share_cluster = sum(cluster_types$total_seller_sales_ttc_euro) / total_revenues
  #   infoBox("Share of amount",paste0(format(round(share_cluster*100),big.mark = ","), "%"),
  #           color = "blue", fill = TRUE)
  # })
  # 
  # output$type_total_amount = renderInfoBox({
  #   lower_bound = input$year[1]
  #   upper_bound = input$year[2]
  #   df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
  #   infoBox(
  #     "Sales Revenues", paste0(format(comma(sum(df$total_seller_sales_ttc_euro)))), "euro",
  #     color = "purple", icon = icon("euro")
  #   )
  # })
  # 
  # output$type_average_amount = renderInfoBox({
  #   lower_bound = input$year[1]
  #   upper_bound = input$year[2]
  #   df = overview_types [which(overview_types$year <= upper_bound & overview_types$year >= lower_bound & overview_types$Type == input$Type),] 
  #   infoBox(
  #     "Mean price", paste0(format(comma(mean(df$mean_price_eur), na.rm=TRUE))), "euro",
  #     color = "purple", icon = icon("coins")
  #   )
  # })
  
  
  
}





##  UI------------------------------------------------

#HEADER---------------------
header = dashboardHeader(
  title = 'Airbnb France'
)

#SIDEBAR--------------------
sidebar = dashboardSidebar(
  sidebarMenu(id = "tabs",
              # sliderInput("year", "Year", min(g$year), max(g$year)+1,value=range(g$year),
              #             step = 1),
              
              # Select variable for x-axis
              # selectInput(inputId = "x", 
              #             label = "Parameter 1:",
              #             choices = c("city", "neighbourhood_cleansed", "property_type"), 
              #             selected = "city"),
              # 
              # # Select variable for y-axis
              # selectInput(inputId = "y", 
              #             label = "Parameter 2:",
              #             choices = c("id", "beds"), 
              #             selected = "price"),
              # 
              # # Select variable for color
              # selectInput(inputId = "z", 
              #             label = "Color code:",
              #             choices = c("city", "neighbourhood_cleansed", "property_type"), 
              #             selected = "city")
              #,
              menuItem(text = "Overview", tabName = "gen_fig"),
              menuItem(text = "Listings focus", tabName ="listings_characteristics")
  )
)

#BODY--------------------
body = dashboardBody(
  tabItems(
    tabItem(
      tabName = 'gen_fig',
      fluidRow(
        infoBoxOutput("nb_hosts"),
        infoBoxOutput("nb_listings"),
        infoBoxOutput("nb_reviews"),
        infoBoxOutput("nb_cities"),
        infoBoxOutput("avg_price"),
        infoBoxOutput("avg_satcli"),
        
        # box(
        #   title = "Number of Airbnb hosts per city", status = "primary",
        #   plotOutput("nb_hosts_city")
        # ),
        # box(
        #   title = "Number of Airbnb listings per city", status = "primary",
        #   plotOutput("nb_listings_city")
        # )
        
      )
    ),
    
     tabItem(
       tabName = "listings_characteristics",
       fluidRow(
         plotOutput("nb_listings_city", width = NULL,height = 350),
         plotOutput("top_neighbo", width = NULL,height = 350),
         plotOutput("listings_mosaic", width = NULL,height = 350),
         plotOutput("nb_accom", width = NULL,height = 700)
         # selectInput(inputId = "Type", "Product Category Type",
         #             choices =unique(overview_types$Type),
         #             selected = "Bio"),
         # column(width = 4,
         #        box(width = NULL, background = 'blue', "Quantities"),
         #        infoBoxOutput("type_share_quantities", width = NULL),
         #        infoBoxOutput("type_total_quantities", width = NULL),
         #        infoBoxOutput("type_average_quantities", width = NULL)
         #)
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
