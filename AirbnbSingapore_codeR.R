# Import relevant packages
library(dplyr)
library(lubridate)
library(ggplot2)


#######################
### Import the data ###
#######################

# Objective: For each city, the listings, reviews and calendar data are loaded in R workspace

string = "~/Documents/Formation/Github/0_Data/"

# Paris
listings_Paris <- read.csv(paste0(string, "Airbnb_Paris/listings.csv"), encoding="UTF-8", comment.char="#")
reviews_Paris <- read.csv(paste0(string, "Airbnb_Paris/reviews.csv"), encoding="UTF-8", comment.char="#")
calendar_Paris <- read.csv(paste0(string, "Airbnb_Paris/calendar.csv"), encoding="UTF-8", comment.char="#")

# Bordeaux
listings_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/listings.csv"), encoding="UTF-8", comment.char="#")
reviews_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/reviews.csv"), encoding="UTF-8", comment.char="#")
calendar_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/calendar.csv"), encoding="UTF-8", comment.char="#")

# Lyon
listings_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/listings.csv"), encoding="UTF-8", comment.char="#")
reviews_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/reviews.csv"), encoding="UTF-8", comment.char="#")
calendar_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/calendar.csv"), encoding="UTF-8", comment.char="#")


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


#####################
### DATA CLEANING ###
#####################

# Database "Listings"

# Transformation colonnes au format date

# Description du problËme : Les fichiers csv ont mal ÈtÈ importÈs T_T
# Raison : le sÈparateur de colonnes est une virgule et Áa a l'air de foutre la merde, notamment au niveau de la colonne amenities
# Solution: Je vise le quick win en prioritÈ. Pour cela, je regarde si la plus grande partie de mon dataset est exploitable. Si c'est le cas, je jette le reste
# Comment je me suis rendue compte du problËme : en voulant transformer en dates des colonnes string (issues du csv) mais erreur de R (Áa matche pas)


# Note : Every variable has been converted to a string when csv imported
str(listings)


# Transform date columns into date format : It does not work because data is messy
cols_date = c("last_scraped", "host_since", "calendar_last_scraped", "first_review", "last_review")
listings[cols_date] = sapply(listings[cols_date], as.Date)

# Pour cleaner, je cherche les colonnes au format bizarre 
suspicious_data = listings %>% mutate(nb_char = nchar(last_scraped)) %>% filter(nb_char > 10)
suspicious_data %>% glimpse()

# J'ai ca 6000 observations sur 89000 qui sont ‡ jeter (environ 6% Èchantillon) -> je jËte
listings_clean = listings %>% mutate(nb_char = nchar(last_scraped)) %>% filter(nb_char <= 10)
cols_date = c("last_scraped", "host_since")
listings_clean[cols_date] = sapply(listings_clean[cols_date], as.Date)
#RÈsultat : succËs !!!

# Autres rËgles : je vire les lignes o˘ mon URL ne commence par http
  # J'identifie ca 10 000 lignes -> L‡ encore, je les vire
suspicious_data2 = listings_clean %>% 
  mutate(url_link = startsWith(listing_url, 'http'),
         url2 = startsWith(picture_url, 'http')) %>%
  filter(url_link == FALSE)

listings_clean = listings_clean %>% 
  mutate(url_link = startsWith(listing_url, 'http'),
         url2 = startsWith(picture_url, 'http')) %>%
  filter(url_link == TRUE & url2 == TRUE)
# Il me reste 73 000 lignes au lieu de 89 000


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


 