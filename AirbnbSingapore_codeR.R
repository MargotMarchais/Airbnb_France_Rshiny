# Import relevant packages
library(dplyr)
library(lubridate) # ne sert ‡ rien pour l'instant
library(stringr)
library(ggplot2)



#######################
### Import the data ###
#######################

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


################################
### BUILD A CENTRAL DATABASE ###
################################

# Objective : Build a centralized dataset for listings, reviews and calendar

listings = rbind(listings_Paris, listings_Bordeaux, listings_Lyon)
reviews = rbind(reviews_Paris, reviews_Bordeaux, reviews_Lyon)
calendar = rbind(calendar_Paris, calendar_Bordeaux, calendar_Lyon)

# Remove individual files to free memory
rm(listings_Paris, listings_Bordeaux, listings_Lyon,
   reviews_Paris, reviews_Bordeaux, reviews_Lyon,
   calendar_Paris, calendar_Bordeaux, calendar_Lyon)
gc()


########################
### DATA PREPARATION ###
########################

# Database "Listings"

# Note : Every variable has been converted to a string when csv imported
str(listings)


# Transform date columns into date format
# Remarque : A optimiser, par l'usage de fonctions ????
str(listings)

listings <-listings %>%
  mutate(host_since = ymd(host_since),
         calendar_last_scraped = ymd(calendar_last_scraped),
         first_review = ymd(first_review),
         last_review = ymd(last_review))

# Tranformer les % en floats
  # Le signe % fait que c'est interprÈtÈ comme du texte
listings$host_response_rate <-gsub("%","", listings$host_response_rate)
listings$host_acceptance_rate <-gsub("%","", listings$host_acceptance_rate)
listings$host_response_rate = as.numeric(listings$host_response_rate_nb) /100
listings$host_acceptance_rate = as.numeric(listings$host_acceptance_rate_nb) /100

#Transformer les scores en variables numÈriques
# Je veux m'assurer que toutes les lignes sont exprimÈes en $
listings %>% filter(!grepl('$', price)) #Áa ne renvoie aucun rÈsultat -> OK on peut retirer le signe dollar
listings$price <- as.numeric(str_sub(listings$price, 2, -2))

# Transformer les variables boolÈennes en flags
listings = listings %>% 
  mutate(instant_bookable = case_when(instant_bookable=='f' ~ 0, instant_bookable=='t' ~ 1),
         has_availability = case_when(has_availability=='f' ~ 0, has_availability=='t' ~ 1),
         host_identity_verified = case_when(host_identity_verified=='f' ~ 0, host_identity_verified=='t' ~ 1),
         host_has_profile_pic = case_when(host_has_profile_pic=='f' ~ 0, host_has_profile_pic=='t' ~ 1)
         )
  
#Transformer facteurs
host_response_time



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

# Comprendre les guests : 
# Des gens rÈcidivistes ou des one-shots en France ?


# Bonus : carte des annonces en utilisant la geoloc
# en mettant en size le nombre d'annonces et en couleur le prix

# Bonus : corrÈlation entre les reviews / la localisation et le prix


## Comprendre les reviews
# Questions : 
# Combien d'hosts ? Combien ils louent d'apparts en moyenne ? (que des individuels ou des pros) ? A quel prix ?




## Je transforme les colonnes boolÈennes en boolÈen




# Metadata : nom des colonnes, leur type, nombre de lignes et colonnes
str(calendar)
str(reviews)
str(listings) # Attention, tout a ÈtÈ importÈ en string

# Transformation ‡ opÈrer
test = select(calendar, date) 


%>% ymd()
# Transformer les dates en date
# Transformer les boolÈens
# Transformer les formats dollars

# Glimpse of the data: Ca ressemble ‡ quoi

#





# On veut voir la t√™te des donn√©es (que contiennent les colonnes)
# On veut voir la distribution des variables -> on a des valeurs aberrantes ? Bcp de valeurs nulles ?


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



# Listing ID et date : pour la jointure
# reviewer id
# Il manque les notes -> on a que le commentaire verbeux.... -> Il faudrait faire du text mining pour savoir si c'est positif ou n√©gatif...


# Permet de suivre au cours du temps l'√©volution des prix d'une annonce -> Pas le plus int√©ressant


# Questions : 
# Combien d'hosts ? Combien ils louent d'apparts en moyenne ? (que des individuels ou des pros) ? A quel prix ?




# But de l'√©tude : 
# 1) D√©montrer mes comp√©tences en SQL
  # Me servir uniquement de requ√™tes SQL utilisant la syntaxe suivante : MIN, MAX, COUNT, SUM, COUNT DISTINCT
  # Utiliser des op√©rateurs avanc√©s : WHERE, HAVING, PARTITION BY, QUALIFY ROW, DENSE_RANK etc
  # Utiliser des jointures et des UNION
# 2) Montrer mes comp√©tences √† explorer les donn√©es : recherche de doublons, extraire des insights, etc
# 3) Peut √™tre faire une segmentation sous R des annonces


 