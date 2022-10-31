# Import relevant packages
library(dplyr)
library(lubridate) 
library(stringr)
library(ggplot2)
library(tm)  
library(wordcloud)  

#setwd("~/Documents/Formation/Github/Airbnb_Database_Rstudio")


##########################
### 1. Import the data ###
##########################

# Objective: For each city, the listings, reviews and calendar data are loaded in R workspace

string = "~/Documents/Formation/Github/0_Data/"

# Paris
listings_Paris <- read.csv(paste0(string, "Airbnb_Paris/listings.csv"), encoding="UTF-8")
reviews_Paris <- read.csv(paste0(string, "Airbnb_Paris/reviews.csv"), encoding="UTF-8")
calendar_Paris <- read.csv(paste0(string, "Airbnb_Paris/calendar.csv"), encoding="UTF-8")

# Bordeaux
listings_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/listings.csv"), encoding="UTF-8")
reviews_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/reviews.csv"), encoding="UTF-8")
calendar_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/calendar.csv"), encoding="UTF-8")

# Lyon
listings_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/listings.csv"), encoding="UTF-8")
reviews_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/reviews.csv"), encoding="UTF-8")
calendar_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/calendar.csv"), encoding="UTF-8")

# Add the data source in a column
listings_Paris = listings_Paris %>% mutate(city = 'Paris')
listings_Bordeaux = listings_Bordeaux %>% mutate(city = 'Bordeaux')
listings_Lyon = listings_Lyon %>% mutate(city = 'Lyon')


###################################
### 2. BUILD A CENTRAL DATABASE ###
###################################

# Objective : Build a centralized dataset for listings, reviews and calendar

listings = rbind(listings_Paris, listings_Bordeaux, listings_Lyon)
reviews = rbind(reviews_Paris, reviews_Bordeaux, reviews_Lyon)
calendar = rbind(calendar_Paris, calendar_Bordeaux, calendar_Lyon)

# Remove individual files to free memory
rm(listings_Paris, listings_Bordeaux, listings_Lyon,
   reviews_Paris, reviews_Bordeaux, reviews_Lyon,
   calendar_Paris, calendar_Bordeaux, calendar_Lyon)
gc()


##########################
### 3. DATA STRUCTURES ###
##########################

# Structure: Database "Listings"
str(listings)

# Transform date columns into date format
# Remarque : A optimiser, par l'usage de fonctions ????
listings <-listings %>%
  mutate(host_since = ymd(host_since),
         last_scraped = ymd(last_scraped),
         calendar_last_scraped = ymd(calendar_last_scraped),
         first_review = ymd(first_review),
         last_review = ymd(last_review))

# Tranformer les % en floats
  # Le signe % fait que c'est interprÈtÈ comme du texte
listings$host_response_rate <-gsub("%","", listings$host_response_rate)
listings$host_acceptance_rate <-gsub("%","", listings$host_acceptance_rate)
listings$host_response_rate = as.numeric(listings$host_response_rate) /100
listings$host_acceptance_rate = as.numeric(listings$host_acceptance_rate) /100

#Transformer les scores en variables numÈriques
# Je veux m'assurer que toutes les lignes sont exprimÈes en $
listings %>% filter(!grepl('$', price)) #Áa ne renvoie aucun rÈsultat -> OK on peut retirer le signe dollar
listings$price <- as.numeric(str_sub(listings$price, 2, -2))

# Transformer les variables boolÈennes en flags
listings = listings %>% 
  mutate(instant_bookable = case_when(instant_bookable=='f' ~ 0, instant_bookable=='t' ~ 1),
         has_availability = case_when(has_availability=='f' ~ 0, has_availability=='t' ~ 1),
         host_identity_verified = case_when(host_identity_verified=='f' ~ 0, host_identity_verified=='t' ~ 1),
         host_has_profile_pic = case_when(host_has_profile_pic=='f' ~ 0, host_has_profile_pic=='t' ~ 1),
         host_is_superhost = case_when(host_is_superhost=='f' ~ 0, host_is_superhost=='t' ~ 1),
         )
  
#Transformer facteurs les variables categories
listings$host_response_time = as.factor(listings$host_response_time)
listings$room_type = as.factor(listings$room_type)
listings$property_type = as.factor(listings$property_type)
listings_table = as.data.frame(table(listings$room_type, listings$property_type))

#CrÈer des ID en strings (on ne les somme pas, ce sont des IDs!)
listings$id = as.character(listings$id)
listings$host_id = as.character(listings$host_id)

# Base de donnÈes reviews
reviews$date = ymd(reviews$date)
reviews$year = year(reviews$date)



###########################
### 4. DATA PREPARATION ###
###########################

# Objectif: Je veux explorer les donnÈes Listings sous Rshiny, avec un dashboard reactif
summary(listings)

# 4.1 BANS / ordres de grandeur
BAN_listings = listings %>% 
  summarise(nb_listings = n(),
            nb_hosts = n_distinct(host_id),
            nb_cities = n_distinct(city),
            avg_satcli = round(mean(review_scores_rating, na.rm = TRUE),2),
            avg_price = round(mean(price, na.rm = TRUE),2))

BAN_reviews = reviews %>% 
  filter(year=='2021') %>%
  summarise(nb_reviews = n(),
            nb_reviewers = n_distinct(reviewer_id))

# 4.2. Listings charactertistics
listings_summary = listings %>% 
  group_by(city, room_type, property_type, neighbourhood_cleansed, accommodates, bedrooms) %>%
  summarise(nb_listings_charac = n()) %>%
  mutate(city_neighbourhood_cleansed = paste(city, "-", neighbourhood_cleansed))
listings_summary = as.data.frame(listings_summary)

listings$property_type_clean = gsub("Private room in","", listings$property_type)
listings = listings %>% 
  mutate(accommodates_bins = case_when(accommodates <3 ~ "[1;2]",
                                       accommodates>= 3 & accommodates<= 5 ~ "[3;5]",
                                       accommodates>= 6 & accommodates<= 9 ~ "[6;9]",
                                       accommodates>= 10 ~ "[More than 9]")
  )

#Focus : Wordcloud
text <- listings$amenities
docs <- Corpus(VectorSource(text))
docs <- docs %>%
   tm_map(removeNumbers) %>%
   tm_map(removePunctuation) %>%
   tm_map(stripWhitespace) %>%
   tm_map(function(x) removeWords(x, stopwords("english")))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
df = df %>% filter(freq >50)
rm(text, docs, dtm, matrix, words)
gc()

# Brainstorming pricing

  

  
listings_entire = listings %>% filter(room_type == "Private room")
ggplot(listings_entire, aes(x=city, y=price, fill=property_type_clean)) + 
  geom_boxplot() +
  facet_wrap(~property_type_clean, scale="free")

# Boxplot: Price vs City
ggplot(listings, aes(x=city, y=price, fill=city)) + 
  geom_boxplot() 

ggplot(data = (listings %>% filter(accommodates!=0)) , aes(x=city, y=price)) + 
  geom_boxplot() +
  facet_wrap(~accommodates_bins)



#Choses ‡ faire : 
# Ordonner le top N
# Harmoniser les couleurs
# Rajouter les inputs




####################
### SEGMENTATION ###
####################

# Objectifs : 

## Comprendre l'offre : 
# Combien d'annonces ? Comment sont-elles rÈparties entre les 3 villes FR ? Et dans quels quartiers ?
# Quelle saisonnalitÈ : le week-end ? l'ÈtÈ ? est-ce qu'on a plus d'annonces ‡ ce moment l‡ ?
# Quelle fourchette de prix ?
# Quels quartiers ?
# Combien de piËces ? salles de bain ?
# Pour quelle pÈriode de temps ?
# Amenities ?
# Pour combien de temps ?
# Review scores : clean, accurate, checkin, comm', location


## Comprendre les hosts :
# Framework RFM : Recency, Frequency, Monetary, 1st transaction
# qui sont ceux qui n'ont pas leur ID vÈrifiÈe : ceux qui ont rejoint rÈcemment la plateforme
# Analyse de cohorte

# Comprendre les guests : 
# Des gens rÈcidivistes ou des one-shots en France ?


# Bonus : carte des annonces en utilisant la geoloc
# en mettant en size le nombre d'annonces et en couleur le prix

# Bonus : corrÈlation entre les reviews / la localisation et le prix


## Comprendre les reviews
# Questions : 
# Combien d'hosts ? Combien ils louent d'apparts en moyenne ? (que des individuels ou des pros) ? A quel prix ?









# Questions : 
# Combien d'hosts ? Combien ils louent d'apparts en moyenne ? (que des individuels ou des pros) ? A quel prix ?


# BAN :  
# VOLUME : Calculer le nombre de listings 
# VOLUME : Nombre de reviews
# VOLUME : Nombre d'hÙtes
# VALEUR : Prix / revenus 
# Comparaison en % selon la pÈriode retenue

# Remarques personnelles sur les variables : 
# ID : n¬∞ unique de l'annonce
# Name & description : titre de l'annonce et sa description (texte mining)
# Host id : ID de la personne louant l'appart'
# Date d√©but de l'host : -> sera peut-√™tre utile pour de la segmentation
# Host location : ne vit pas n√©cessairement √† Paris
# Stats sur les hosts : temps de r√©ponse, taux de r√©ponse et taux d'acceptation, superhost
# host neighborhood -> le quartier
# host listings count & host total listings count -> ???
# latitude et longitude
# Caract√©ristiques de l'appart (nombre de lits, salles de bain, am√©nit√©s, etc)
# Prix
 