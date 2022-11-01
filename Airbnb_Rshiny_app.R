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
library(leaflet)
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
    ggplot(data = (listings %>% filter(accommodates!=0)) , aes(x=city, y=price, fill = city)) + 
      geom_boxplot() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      facet_wrap(~accommodates_bins) +
      labs(x = "City", y= "Price ($)", title = "The higher the number of accommodates, \n the higher the price (and the variation in prices)")
  }) 
  
  #Most expensive (top 5)
  output$most_expens <- renderPlot({
    ggplot(most_expensive, 
           aes(x= reorder(property_type, avg_price), y = avg_price, fill = "red")) +
      geom_col() +
      coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="none") +
      geom_text(    aes(label = after_stat(y), group = property_type), 
                    stat = 'summary', fun = sum, hjust = +2) +
      labs(x = "Property type", y= "Average price ($)", 
           title = "The scarcity of exceptional or historic properties \n make them the most expensive")
  }) 
  
  #Least expensive (bottom 5)
  output$least_expens <- renderPlot({
    ggplot(least_expensive, 
           aes(x= reorder(property_type, avg_price), y = avg_price, fill = "red")) +
      geom_col() +
      coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position="none") +
      geom_text(    aes(label = after_stat(y), group = property_type), 
                    stat = 'summary', fun = sum, hjust = +2) +
      labs(x = "Property type", y= "Average price ($)", 
           title = "Shared rooms and makeshift homes \n are the least expensive listings on Airbnb")
  }) 
  
  
  
  ###########
  # 6. MAPS #
  ########### 
  
  # Map Paris
  output$map_Paris <- renderLeaflet({
    
    # Reactive inputs (slider)
    data<- reactive({
     x <- listings_price[listings_price$price>= input$price_range[1] &
                           listings_price$price <= input$price_range[2],
                         ]
    })
     
    # Legend of colors
    bins <- c(0, 100, 200, 300, 400, Inf)
    pal <- colorBin("YlOrRd", domain = listings_price$price, bins = bins)
    
    # To allow data to be reactive
    listings_price <- data()
    
    # Map construction
    m_paris <- leaflet(data = listings_price %>% filter(city=="Paris" & price != "NA")) %>%
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude) )  %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~number_of_reviews/90, #Display correctly listings
                       fillColor = ~pal(price),
                       weight = 0.01,
                       opacity = 1,
                       color = "black",
                       dashArray = "1",
                       fillOpacity = 0.7,
                       popup = paste("<h3> Listings info </h3>",
                                     "Listings name: ", listings_price$name,"<br>",
                                     "Listings URL: ", listings_price$listing_url, "<br>",
                                      "Property type: ", listings_price$property_type,"<br>",
                                      "Neighbourhood: ", listings_price$neighbourhood_cleansed, "<br>",
                                      "Price: ", listings_price$price, "<br>",
                                      "Accommodates: ", listings_price$accommodates, "<br>",
                                      "Total reviews: ", listings_price$number_of_reviews, "<br>",
                                      "Review scores: ", listings_price$review_scores_rating, "<br>",
                                      "Host is superhost: ", listings_price$host_is_superhost, "<br>"
                       )
      )
    
    # Print the map
    m_paris
    
    # Add the color legend
    m_paris %>% addLegend(pal = pal, values = ~price, opacity = 0.7, title="Price ($)",
                     position = "bottomleft")
  
  })
  
  
  
  # Map Bordeaux
  output$map_Bordeaux <- renderLeaflet({
    
    # Reactive inputs (slider)
    data<- reactive({
      x <- listings_price[listings_price$price>= input$price_range[1] &
                            listings_price$price <= input$price_range[2],
      ]
    })
    
    # Legend of colors
    bins <- c(0, 100, 200, 300, 400, Inf)
    pal <- colorBin("YlOrRd", domain = listings_price$price, bins = bins)
    
    # To allow data to be reactive
    listings_price <- data()
    
    # Map construction
    m_bordeaux <- leaflet(data = listings_price %>% filter(city=="Bordeaux" & price != "NA")) %>%
      addTiles() %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude) )  %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~number_of_reviews/90, #Display correctly listings
                       fillColor = ~pal(price),
                       weight = 0.01,
                       opacity = 1,
                       color = "black",
                       dashArray = "1",
                       fillOpacity = 0.7,
                       popup = paste("<h3> Listings info </h3>",
                                     "Listings name: ", listings_price$name,"<br>",
                                     "Listings URL: ", listings_price$listing_url, "<br>",
                                     "Property type: ", listings_price$property_type,"<br>",
                                     "Neighbourhood: ", listings_price$neighbourhood_cleansed, "<br>",
                                     "Price: ", listings_price$price, "<br>",
                                     "Accommodates: ", listings_price$accommodates, "<br>",
                                     "Total reviews: ", listings_price$number_of_reviews, "<br>",
                                     "Review scores: ", listings_price$review_scores_rating, "<br>",
                                     "Host is superhost: ", listings_price$host_is_superhost, "<br>"
                       )
      )
    
    # Print the map
    m_bordeaux
    
    # Add the color legend
    m_bordeaux %>% addLegend(pal = pal, values = ~price, opacity = 0.7, title="Price ($) - excluding service and cleaning fees",
                          position = "bottomleft")
    
  })
  
  
  # Map Lyon
  output$map_Lyon <- renderLeaflet({
    
    # Reactive inputs (slider)
    data<- reactive({
      x <- listings_price[listings_price$price>= input$price_range[1] &
                            listings_price$price <= input$price_range[2],
      ]
    })
    
    # Legend of colors
    bins <- c(0, 100, 200, 300, 400, Inf)
    pal <- colorBin("YlOrRd", domain = listings_price$price, bins = bins)
    
    # To allow data to be reactive
    listings_price <- data()
    
    # Map construction
    m_lyon <- leaflet(data = listings_price %>% filter(city=="Lyon" & price != "NA")) %>%
      addTiles() %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude) )  %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~number_of_reviews/100, #Display correctly listings
                       fillColor = ~pal(price),
                       weight = 0.01,
                       opacity = 1,
                       color = "black",
                       dashArray = "1",
                       fillOpacity = 0.7,
                       popup = paste("<h3> Listings info </h3>",
                                     "Listings name: ", listings_price$name,"<br>",
                                     "Listings URL: ", listings_price$listing_url, "<br>",
                                     "Property type: ", listings_price$property_type,"<br>",
                                     "Neighbourhood: ", listings_price$neighbourhood_cleansed, "<br>",
                                     "Price: ", listings_price$price, "<br>",
                                     "Accommodates: ", listings_price$accommodates, "<br>",
                                     "Total reviews: ", listings_price$number_of_reviews, "<br>",
                                     "Review scores: ", listings_price$review_scores_rating, "<br>",
                                     "Host is superhost: ", listings_price$host_is_superhost, "<br>"
                       )
      )
    
    # Print the map
    m_lyon
    
    # Add the color legend
    m_lyon %>% addLegend(pal = pal, values = ~price, opacity = 0.7, title="Price ($)",
                             position = "bottomleft")
    
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
                  The French capital accounted for the vast majority of listings (i.e. 75%). It had namely 6x times more Airbnb listings than any other big French city.
                  On the other hand, Lyon (500,000 inhabitants) and Bordeaux (250,000 inhabitants) had the same amount of listings (ca. 11,000), although Lyon is much bigger than Bordeaux."),
                plotOutput("nb_listings_city", width = NULL,height = 350),
                br(),
                strong("Insights:"),
                p("Not all Paris neighbourhoods are as equally represented in Airbnb listings.
                  Indeed, 4 out of the 5 neighbourhoods with the most listings are situated in Paris 'Rive droite' (i.e. north of the Seine river).
                  Such arrondissements are usually more lively than South of Paris, and may be very touristic (e.g. Butte Montmartre is very famous due to the Sacré Coeur Basilica)."),
                plotOutput("top_neighbo", width = NULL,height = 350),
                br()
         ),
         column(width = 6,
                box(width = NULL, background = 'red', "Listings types"),
                strong("Insights:"),
                p("In every city, hosts usually rent their entire apartment or house. This is especially true in Paris (82% of listings vs 75% in Bordeaux or Lyon).
                  The second most common choice is the private room. As far as shared rooms and hotel rooms are concerned, they are not not popular options at all on the Airbnb platform in France."),
                plotOutput("listings_mosaic", width = NULL,height = 350),
                br(),
                strong("Insights:"),
                p("The wordcloud indicates that listings amenities very often mention the Wifi connection and safety elements (smoke alarm). 
                  Hosts also emphasize services that are usually not available in hotels,
                  such as long-term stays or useful household appliances (oven, dryer, washer, iron, refrigerator, coffee machine, etc)"),
                plotOutput("amenities_wordcloud", width = NULL, height = 500),
                br()
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
        p(HTML('&nbsp;'), "Disclaimer: Please kindly wait for about 5 to 10 seconds so that Rshiny loads the maps.
          Tip n°1: You may also open other tabs in Rshiny while waiting for the page to load."),
        p(HTML('&nbsp;'), "Tip n°2: Select a range of prices ($) if you want to filter out some listings from the maps."),
        br(),
        column(width = 6,
               box(width = NULL, background = 'red', "Map: Paris"),
               absolutePanel(top =0.6, left = 0.1,
                             sliderInput("price_range", "Price",
                                         min(listings_price$price), max(listings_price$price),
                                         value=range(listings_price$price), step=50)
               ),
               br(),
               strong("Insights"),
               p("Airbnb 'Paris' listings cover Paris 'intra muros' as well as the inner suburbs.
               Inner suburbs are however underrepresented as far as pricy listings are considered (except in some post West suburban towns).
               Indeed, the most expensive listings (red circles) tend to be located in very Central Paris, near the touristic spots (Champs-Elysees, Le Louvre, jardin des Tuileries, etc).
               "),
               leafletOutput("map_Paris")
        ),
        column(width = 6,
               box(width = NULL, background = 'red', "Map: Lyon"),
               br(),
               strong("Insights"),
               p("Compared to Paris, Lyon has very few listings with prices exceeding $200 (few red circles). 
                 Moreover, the latter seem to be located in the West / South outskirts of Lyon, instead of being centralized in the heart of the city.
                 "),
               leafletOutput("map_Lyon")
               )
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
        p(HTML('&nbsp;'), "Disclaimer: the price information in the underlying data does not include service and cleaning fees, so the total bill may be even higher."),
        column(width = 6,
               box(width = NULL, background = 'red', "Pricing vs listings characteristics"),
               strong("Insights:"),
               p("The median price of a listing in Paris is $100 (against $70 in Bordeaux and Lyon).
                 The mean price is also higher: $142 in Paris (against $101 and $92 in Bordeaux and Lyon respectively).
                 Indeed, Paris is the French capital and it attracts millions of tourists every year. Therefore, prices may reflect the high demand."),
               br(),
               plotOutput("pricing_city", width = NULL,height = 350),
               br(),
               strong("Insights:"),
               p("As the number of accommodates increases, the price of listings tend to increase as well (this statement applies to all cities).
                 Indeed, for max 2 accommodates, the median price is around $50 in Bordeaux and Lyon ($80 in Paris). 
                 For groups of 10 and more, the median price reaches $350 in Bordeaux ($375 in Lyon and $466 in Paris). 
                 Moreover, prices tend to be more spread out as the number of accommodates increases, whereas the distribution is much more narrow for a small number of guests."),
               plotOutput("pricing_accom", width = NULL,height = 350),
               br()
        ),
        column(width = 6,
               box(width = NULL, background = 'red', "Pricing: Top 5 vs Bottom 5"),
               strong("Insights:"),
               p("Listings price seems to follow the law of supply and demand. 
                  For instance, scarce properties such as French castles and historical gites tend to be very pricy.
                  Rooms in Parisian boutique hotels are also very expensive: they usually offer an exceptional location in Paris, luxurious family suites,...
                 Finally, the top 3 property types seem to be typos / outliers or even suspicious listings (an ice dome in central Paris ?!)."),
               plotOutput("most_expens", width = NULL,height = 350),
               br(),
               strong("Insights:"),
               p("Having some privacy and room for oneself costs money. Therefore, the least expensive listings on Airbnb involve shared rooms or even makeshift homes (tents, tipi, windmills)"),
               br(),
               br(),
               br(),
               br(),
               plotOutput("least_expens", width = NULL,height = 350),
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
               p("Data analysis and Rshiny app design: Margot MARCHAIS--MAURICE"),
               p("For further technical details, please visit: https://github.com/MargotMarchais/Airbnb_France_Rstudio")
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
