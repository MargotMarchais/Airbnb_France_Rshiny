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
  
  
  
  ##########
  # 6. MAP #
  ########## 
  
  output$mymap <- renderLeaflet({
    
    data<- reactive({
     x <- listings[listings$number_of_reviews>= input$nb_reviews_range[1] & listings$nb_number_of_reviews <= input$reviews_range[2],]
    })
     
    bins <- c(0, 100, 150, 200, 250, Inf)
    pal <- colorBin("YlOrRd", domain = input$nb_reviews_range, bins = bins)
    
    
    tag.map.title <- tags$style(HTML("
                                     .leaflet-control.map-title {
                                     transform: translate(-50%,20%);
                                     position: fixed !important;
                                     left: 10%;
                                     text-align: center;
                                     padding-left: 10px;
                                     padding-right: 10px;
                                     background: rgba(255,255,255,0.75);
                                     font-weight: bold;
                                     font-size: 20px;
                                     }
                                     "))
    
    title <- tags$div(
      tag.map.title, HTML("Airbnb listings in Paris")
    )  
    
    #listings <- data()
    
    m <- leaflet(data = listings %>% filter(city=="Paris" & price != "NA")) %>%
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude) )  %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~number_of_reviews/70, #Display correctly listings
                       fillColor = ~pal(price),
                       weight = 0.01,
                       opacity = 1,
                       color = "black",
                       dashArray = "1",
                       fillOpacity = 0.7,
                       popup = paste("<h3> Listings info </h3>",
                                     "Listings name: ", listings$name,"<br>",
                                      "Property type: ", listings$property_type,"<br>",
                                      "Neighbourhood: ", listings$neighbourhood_cleansed, "<br>",
                                      "Price: ", listings$price, "<br>",
                                      "Accommodates: ", listings$accommodates, "<br>",
                                      "Total reviews: ", listings$number_of_reviews, "<br>",
                                      "Review scores: ", listings$review_scores_rating, "<br>",
                                      "Host is superhost: ", listings$host_is_superhost, "<br>"
                       )
    ) %>%
      addControl(title, position = "topright", className="map-title")
    
    # Print the map
    m
    
    # Add the color legend
    m %>% addLegend(pal = pal, values = ~price, opacity = 0.7, title="Price ($) - excluding service and cleaning fees",
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
        p(HTML('&nbsp;'), "Map: Paris"),
        leafletOutput("mymap"),
         absolutePanel(top =1, right = 1,
                     sliderInput("nb_reviews_range", "Total number of reviews",
                                 min(listings$number_of_reviews), max(listings$number_of_reviews),
                                 value=range(listings$number_of_reviews), step=100)
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
        p("Disclaimer: the price information in the underlying data does not include service and cleaning fees, so the total bill may be even higher."),
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
