###################################
#       AIRBNB DASHBOARD          #
#                                 #
# Case study: The French market   #
# By Margot MARCHAIS--MAURICE     #
#                                 #
###################################


### INTRODUCTION ###

# This code provides the main structure of the Rshiny app: a server, a UI and the connection between the 2.
# The data that are used by the app are collected and prepared in another R code.

#setwd("~/Documents/Formation/Github/Airbnb_Database_Rstudio")

# Load packages
#library(rsconnect)
library(scales)
library(htmlwidgets)
library(htmltools)
library(shiny)
require(shinydashboard)
library(ggplot2)
library(ggvis)
library(ggmosaic)
require(scales)
library(binr)
library(tidyr)
library(dplyr)
library(tm)  
library(wordcloud)  
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
        labs(x = "City", y= "Number of Airbnb listings", title = "Paris: 6x times more Airbnb listings \n than any other big French city")
   })

  # Graph : Marimekko chart (room type and city)
  output$listings_mosaic <- renderPlot({   
   ggplot(data = listings) +
     geom_mosaic(aes(x = product(room_type, city), fill=room_type)) + 
     theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
     labs(x= "City", y= "Room type", title="At least 75% of listings offer the entire home/apartment service,\n (whatever the city)")
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
      labs(x = "Neighbourhoods", y= "Number of Airbnb listings", title = "Neighbourhoods with the most listings \n are almost all situated in Paris 'Rive Droite'")
  }) 
  
  #Graph: Boxplot about the number of people the home can accommodate
  output$nb_accom <- renderPlot({   
    ggplot(listings, aes(x=room_type, y= accommodates, fill = city)) + geom_boxplot() +
      facet_grid(city ~ .) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="right") +
      labs(x = "City", y= "Number of guests the listing can accommodate", title = "")
  }) 
  
  #Wordcloud: Description of the amenities
  output$amenities_wordcloud <- renderPlot({ 
  wordcloud(words = df$word, freq = df$freq, min.freq = 50, 
            max.words=100, random.order=FALSE, rot.per=0.05, 
            colors=brewer.pal(8, "Dark2"))
  }) 
      
  
  
  ############################
  # 3. HOSTS CHARACTERISTICS #
  ############################

  
  
  ##########################
  # 4. GUESTS SATISFACTION #
  ##########################  
  
  
  
  ##############
  # 5. PRICING #
  ############## 
  
  # Pricing by city
  output$pricing_city <- renderPlot({
  ggplot(listings, aes(x=city, y=price, fill=city)) + 
    geom_boxplot() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = "City", y= "Price ($)", title = "Without much surprise, \n Paris is the most expensive French city")
  }) 
  
  # Pricing by number of accommodates
  output$pricing_accom <- renderPlot({
    ggplot(data = (listings %>% filter(accommodates!=0)) , aes(x=city, y=price)) + 
      geom_boxplot() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~accommodates_bins) +
      labs(x = "City", y= "Price ($)", title = "The higher the number of accommodates, \n the higher the price (and the variation in prices)")
  }) 
  
  
  
  ##########
  # 6. MAP #
  ########## 
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
              menuItem(text = "Listings", tabName ="listings_characteristics"),
              menuItem(text = "Map", tabName ="map"),
              menuItem(text = "Hosts segmentation", tabName ="hosts_segmentation"),
              menuItem(text = "Review scores", tabName ="SATCLI"),
              menuItem(text = "Pricing", tabName ="pricing"),
              menuItem(text = "About", tabName = "about")

              
  )
)

#BODY--------------------
body = dashboardBody(
  tabItems(
    
    # Overview page contains...
    tabItem(
      tabName = 'gen_fig',
      fluidRow(
        infoBoxOutput("nb_hosts"),
        infoBoxOutput("nb_listings"),
        infoBoxOutput("nb_reviews"),
        infoBoxOutput("nb_cities"),
        infoBoxOutput("avg_price"),
        infoBoxOutput("avg_satcli"),
      )
    ),
    
    # listings page contains...
    tabItem(
      tabName = "listings_characteristics",
       fluidRow(
         
         column(width = 6,
                box(width = NULL, background = 'red', "Listings by geography"),
                strong("Insights:"),
                p("In late 2022, Paris, Lyon and Bordeaux had about 83,000 Airbnb listings. 
                  The French capital accounted for the vast majority of listings (i.e. 75%). It had indeed 6x times more Airbnb listings than any other big French city.
                  Lyon (500,000 inhabitants) and Bordeaux (250,000 inhabitants) had the same amount of listings (ca. 11,000), although Lyon is much bigger than Bordeaux."),
                plotOutput("nb_listings_city", width = NULL,height = 350),
                br(),
                strong("Insights:"),
                p("Not all Paris neighbourhoods are equally represented in Airbnb listings.
                  Indeed, 4 out of the 5 neighbourhoods with the most listings are situated in Paris 'Rive droite' (i.e. north of the Seine river).
                  Such arrondissements are usually more lively than South of Paris, and may be very touristic (e.g. Butte Montmartre is very famous due to the Sacré Coeur Basilica)."),
                plotOutput("top_neighbo", width = NULL,height = 350),
                br()
         ),
         column(width = 6,
                box(width = NULL, background = 'red', "Listings types"),
                strong("Insights:"),
                p("In every city, hosts mostly rent their entire apartment or house. This is especially true in Paris (82% of listings vs 75% in Bordeaux or Lyon).
                  The second most common choice is the private room. On the other hand, shared rooms and hotel rooms are not not popular options at all on the Airbnb platform in France."),
                plotOutput("listings_mosaic", width = NULL,height = 350),
                br(),
                strong("Insights:"),
                p("The wordcloud indicates that amenities very often mention the Wifi connection and safety elements (smoke alarm). 
                  Hosts also emphasize services that are usually not available in hotels,
                  such as long-term stays or useful household appliances (oven, dryer, washer, iron, coffee machine, etc)"),
                #plotOutput("nb_accom", width = NULL,height = 700),
                plotOutput("amenities_wordcloud", width = NULL, height = 500),
                br()
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
     ),
    
    # Hosts segmentation page contains...
    tabItem(
      tabName = "hosts_segmentation",
      fluidRow(
        "Nothing here yet"
      )
    ),
    
    # Map page contains...
    tabItem(
      tabName = "map",
      fluidRow(
        "Nothing here yet"
      )
    ),
    
    # SATCLI page contains...
    tabItem(
      tabName = "SATCLI",
      fluidRow(
        "Nothing here yet"
      )
    ),
    
    # pricing page contains...
    tabItem(
      tabName = "pricing",
      fluidRow(
        column(width = 6,
               box(width = NULL, background = 'red', "Pricing by city"),
               strong("Insights:"),
               p("The median price of a listing in Paris is $100 (against $70 for Bordeaux or Lyon).
                 The mean price is also higher: $142 in Paris (against $101 and $92 for Bordeaux and Lyon respectively).
                 Indeed, Paris is the French capital and it attracts millions of tourists every year. Therefore, prices may reflect the high demand."),
               plotOutput("pricing_city", width = NULL,height = 350),
               br(),
               strong("Insights:"),
               p("As the number of accommodates increases, the price of listings tend to increase as well (this statement applies to all cities).
                 Indeed, for max 2 accommodates, the median price is around $50 in Bordeaux and Lyon ($80 in Paris). 
                 For groups of 10 and more, the median price reaches $350 in Bordeaux ($375 in Lyon and $466 in Paris). 
                 Moreover, prices tend to be more spread out as the number of accommodates increases, whereas the distribution is much more narrow for a small number of guests."),
               plotOutput("pricing_accom", width = NULL,height = 350),
               br()
        )
      )
    ),
    
    # About page contains...
    tabItem(
      tabName = "about",
      fluidRow(
        column(width = 6,
               box(width = NULL, background = 'red', "Credits"),
               p("Data source: InsideAirbnb.com"),
               p("Data analysis and Rshiny app design: Margot MARCHAIS--MAURICE")
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
