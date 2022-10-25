# Import relevant packages
library(odbc)
library(DBI)
library(dplyr)

#######################
### Import the data ###
#######################

# Objective: For each city, the listings, reviews and calendar data are loaded

# Paris
listings_Paris <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Paris/listings.csv", comment.char="#")
reviews_Paris <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Paris/reviews.csv", comment.char="#")
calendar_Paris <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Paris/calendar.csv", comment.char="#")

# Bordeaux
listings_Bordeaux <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Bordeaux/listings.csv", comment.char="#")
reviews_Bordeaux <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Bordeaux/reviews.csv", comment.char="#")
calendar_Bordeaux <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Bordeaux/calendar.csv", comment.char="#")

# Lyon
listings_Lyon <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Lyon/listings.csv", comment.char="#")
reviews_Lyon <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Lyon/reviews.csv", comment.char="#")
calendar_Lyon <- read.csv("~/Documents/Formation/Github/0_Data/Airbnb_Lyon/calendar.csv", comment.char="#")


################################
### BUILD A CENTRAL DATABASE ###
################################

# Objective : All city data are centralized in one database for each topic

listings = rbind(listings_Paris, listings_Bordeaux, listings_Lyon)
rm(listings_Paris, listings_Bordeaux, listings_Lyon)

reviews = rbind(reviews_Paris, reviews_Bordeaux, reviews_Lyon)
rm(reviews_Paris, reviews_Bordeaux, reviews_Lyon)

calendar = rbind(calendar_Paris, calendar_Bordeaux, calendar_Lyon)
rm(calendar_Paris, calendar_Bordeaux, calendar_Lyon)


########################
### DATA EXPLORATION ###
########################



# On veut voir la tête des données (que contiennent les colonnes)
# On veut voir la distribution des variables -> on a des valeurs aberrantes ? Bcp de valeurs nulles ?


# Remarques personnelles sur les variables : 
# ID : n° unique de l'annonce
# Name & description : titre de l'annonce et sa description (texte mining)
# Host id : ID de la personne louant l'appart'
# Date début de l'host : -> sera peut-être utile pour de la segmentation
# Host location : ne vit pas nécessairement à Paris
# Stats sur les hosts : temps de réponse, taux de réponse et taux d'acceptation, superhost
# host neighborhood -> le quartier
# host listings count & host total listings count -> ???
# latitude et longitude
# Caractéristiques de l'appart (nombre de lits, salles de bain, aménités, etc)
# Prix



# Listing ID et date : pour la jointure
# reviewer id
# Il manque les notes -> on a que le commentaire verbeux.... -> Il faudrait faire du text mining pour savoir si c'est positif ou négatif...


# Permet de suivre au cours du temps l'évolution des prix d'une annonce -> Pas le plus intéressant


# Questions : 
# Combien d'hosts ? Combien ils louent d'apparts en moyenne ? (que des individuels ou des pros) ? A quel prix ?




# But de l'étude : 
# 1) Démontrer mes compétences en SQL
  # Me servir uniquement de requêtes SQL utilisant la syntaxe suivante : MIN, MAX, COUNT, SUM, COUNT DISTINCT
  # Utiliser des opérateurs avancés : WHERE, HAVING, PARTITION BY, QUALIFY ROW, DENSE_RANK etc
  # Utiliser des jointures et des UNION
# 2) Montrer mes compétences à explorer les données : recherche de doublons, extraire des insights, etc
# 3) Peut être faire une segmentation sous R des annonces


 