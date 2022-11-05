###################################
#       AIRBNB DASHBOARD          #
#                                 #
# Case study: The French market   #
# By Margot MARCHAIS--MAURICE     #
#                                 #
###################################

### INTRODUCTION ###
# This code describes how the data were prepared so that they can be used in the Rshiny app.
# Data source credits: Airbnbinside.com


# Import relevant packages
library(dplyr)
library(lubridate) 
library(stringr)
library(ggplot2)
library(tm)  
library(wordcloud)  

setwd("~/Documents/Formation/Github/Airbnb_Database_Rstudio")

#Disable scientific notation
options(scipen=999)


##########################
### 1. Import the data ###
##########################

string = "~/Documents/Formation/Github/0_Data/"

# Listings data
listings_Paris <- read.csv(paste0(string, "Airbnb_Paris/listings.csv"), encoding="UTF-8")
listings_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/listings.csv"), encoding="UTF-8")
listings_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/listings.csv"), encoding="UTF-8")

#Reviews data
reviews_Paris <- read.csv(paste0(string, "Airbnb_Paris/reviews.csv"), encoding="UTF-8")
reviews_Bordeaux <- read.csv(paste0(string, "Airbnb_Bordeaux/reviews.csv"), encoding="UTF-8")
reviews_Lyon <- read.csv(paste0(string, "Airbnb_Lyon/reviews.csv"), encoding="UTF-8")

# Add the city in each data source
listings_Paris = listings_Paris %>% mutate(city = 'Paris')
listings_Bordeaux = listings_Bordeaux %>% mutate(city = 'Bordeaux')
listings_Lyon = listings_Lyon %>% mutate(city = 'Lyon')

reviews_Paris = reviews_Paris %>% mutate(city = 'Paris')
reviews_Bordeaux = reviews_Bordeaux %>% mutate(city = 'Bordeaux')
reviews_Lyon = reviews_Lyon %>% mutate(city = 'Lyon')

# Build a centralized dataset for listings and reviews 
listings = rbind(listings_Paris, listings_Bordeaux, listings_Lyon)
reviews = rbind(reviews_Paris, reviews_Bordeaux, reviews_Lyon)

# Remove individual files to free memory
rm(listings_Paris, listings_Bordeaux, listings_Lyon,
   reviews_Paris, reviews_Bordeaux, reviews_Lyon)
gc()


##########################
### 2. DATA STRUCTURES ###
##########################

# Structure of the database "Listings": Some columns do not have the appropriate format
str(listings)

# Transform date columns (considered as strings) into date format
listings <-listings %>%
  mutate(host_since = ymd(host_since),
         last_scraped = ymd(last_scraped),
         calendar_last_scraped = ymd(calendar_last_scraped),
         first_review = ymd(first_review),
         last_review = ymd(last_review))

# Tranform percentage columns (considered as strings due to the '%' sign) into floats
listings$host_response_rate <-gsub("%","", listings$host_response_rate)
listings$host_acceptance_rate <-gsub("%","", listings$host_acceptance_rate)
listings$host_response_rate = as.numeric(listings$host_response_rate) /100
listings$host_acceptance_rate = as.numeric(listings$host_acceptance_rate) /100

#Transform price column (considered as string due to the '$' sign) into numeric variable
listings %>% filter(!grepl('$', price)) 
listings$price <- as.numeric(str_sub(listings$price, 2, -2))

# Transform boolean variables (considered as string) into integer flags
listings = listings %>% 
  mutate(instant_bookable = case_when(instant_bookable=='f' ~ 0, instant_bookable=='t' ~ 1),
         has_availability = case_when(has_availability=='f' ~ 0, has_availability=='t' ~ 1),
         host_identity_verified = case_when(host_identity_verified=='f' ~ 0, host_identity_verified=='t' ~ 1),
         host_has_profile_pic = case_when(host_has_profile_pic=='f' ~ 0, host_has_profile_pic=='t' ~ 1),
         host_is_superhost = case_when(host_is_superhost=='f' ~ 0, host_is_superhost=='t' ~ 1),
         )

#Transform strings into factors as they are categorical variables
listings$host_response_time = as.factor(listings$host_response_time)
listings$room_type = as.factor(listings$room_type)
listings$property_type = as.factor(listings$property_type)
listings_table = as.data.frame(table(listings$room_type, listings$property_type))

# Transform IDs into strings
listings$id = as.character(listings$id)
listings$host_id = as.character(listings$host_id)

# Finally, some date cleaning in the Reviews dataset
reviews$date = ymd(reviews$date)
reviews$year = year(reviews$date)



###########################
### 3. DATA PREPARATION ###
###########################

# Some descriptive statistics about the Listings database
summary(listings)

# 3.1 BANS / ordres de grandeur
BAN_listings = listings %>% 
  summarise(nb_listings = n(),
            nb_hosts = n_distinct(host_id),
            nb_cities = n_distinct(city),
            avg_satcli = round(mean(review_scores_rating, na.rm = TRUE),2),
            avg_price = round(mean(price, na.rm = TRUE),2))
BAN_listings


BAN_reviews = reviews %>% 
  filter(year=='2021') %>%
  summarise(nb_reviews = n(),
            nb_reviewers = n_distinct(reviewer_id))
BAN_reviews

# 3.2. Listings characteristics
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

# 3.3 Most and least expensive listings
most_expensive = listings %>% 
  group_by(property_type) %>% 
  summarise(median_price = median(price, na.rm = TRUE),
            avg_price = round(mean(price, na.rm =TRUE),2),
            nb_listings = n()
  ) %>%
  arrange(desc(avg_price)) %>%
  slice(1:5)

least_expensive = listings %>% 
  group_by(property_type) %>% 
  summarise(median_price = median(price, na.rm = TRUE),
            avg_price = round(mean(price, na.rm =TRUE),2),
            nb_listings = n()
  ) %>%
  arrange(avg_price) %>%
  slice(1:5)

#Database excluding the few listings with no price specified
listings_price = listings %>% filter(price != "NA")

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


  
#########################
# 4. HOSTS SEGMENTATION #
#########################

# Dataset preparation: Computing adapted RFM indicators for the listings dataset
data_segmentation = listings %>%
  select(host_id, host_since, last_review, price, host_is_superhost, number_of_reviews) %>%
  filter(price != "NA",
         host_is_superhost != "NA",
         !is.na(host_since),
         !is.na(last_review)) %>%
  mutate(
    length_relationship = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    recency = as.numeric(difftime(Sys.Date(), last_review, units = "days"))
  ) %>%
  group_by(host_id) %>%
  summarise(
    length_relationship_years = max(as.integer(length_relationship))/365,
    recency_months = min(recency)/30,
    monetary = mean(price, na.rm = TRUE),
    host_is_superhost = max(host_is_superhost, na.rm=TRUE),
    number_of_reviews = sum(number_of_reviews, na.rm = TRUE)
    )

# Assign contact id as row names, remove id from data
rownames(data_segmentation) = data_segmentation$host_id
data_segmentation = data_segmentation[, -1]

# Perform kmeans segmentation on standardized data
set.seed(10)
k = kmeans(x = scale(data_segmentation), centers = 5, nstart = 50)

# Print cluster size, standardized centers, and then un-standardized centers, one segment at a time
print(k$size)
print(k$centers)
for (i in 1:5) {
  print(colMeans(data_segmentation[k$cluster == i, ]))
}

#Final segmentation dataset
cluster = k[["cluster"]]
merged_data = cbind(data_segmentation, cluster)
rm(data_segmentation)
gc()

# Clusters colors for the graphs
couleurs = c("1" = "hotpink",
             "2" = "darkgoldenrod1",
             "3" = "blue4",
             "4" = "chocolate4",
             "5" = "chartreuse4")

# Recap table about clusters characteristics
seg_summary = merged_data %>% 
  group_by(cluster) %>% 
  summarize(nb_hosts = n(),
            mean_length_relationship = round(mean(length_relationship_years),0),
            mean_recency = round(mean(recency_months), 0),
            mean_price = round(mean(monetary),0),
            pct_superhosts = round(mean(host_is_superhost), 1),
            mean_number_of_reviews = round(mean(number_of_reviews),0)) %>%
  mutate(pct_hosts = round(nb_hosts / sum(nb_hosts),2))


#######################
# 5. REVIEWS ANALYSIS #
#######################

# 5.1 : Compute the correlation between the rating and listings/hosts characteristics
corr_listings = listings %>%
  mutate(is_paris = case_when(city=="Paris"~1, TRUE ~0),
         is_lyon = case_when(city=="Lyon"~1, TRUE ~0),
         is_bordeaux = case_when(city=="Bordeaux"~1, TRUE ~0),
         shared_room = case_when(room_type=="Shared room" ~1, TRUE ~0),
         entire_home = case_when(room_type=="Entire home/apt" ~1, TRUE ~0),
         private_room = case_when(room_type=="Private room" ~1, TRUE ~0)) %>%
  select(review_scores_rating,
         price, number_of_reviews, is_paris, is_lyon, is_bordeaux,
         shared_room, entire_home, private_room,
         host_is_superhost, host_response_rate, host_identity_verified,
         accommodates, beds, availability_30) %>%
  cor(use = "complete.obs")

# Rounding up the results
res_listings <- round(corr_listings, 2)
res_listings

liste = as.data.frame(res_listings) %>% 
  select(review_scores_rating) %>% 
  arrange(desc(review_scores_rating))

object <- rownames(liste)
liste = liste %>% cbind(object)


# 5.2 : To identify the amenities that bring the most satisfaction
listings$Pool = grepl("pool",listings$amenities)
listings$BBQ = grepl("BBQ",listings$amenities)
listings$Garden = grepl("garden",listings$amenities)
listings$Balcony = grepl("balcony",listings$amenities)
listings$Washer = grepl("washer",listings$amenities)
listings$Dryer = grepl("dryer",listings$amenities)
listings$Oven = grepl("oven",listings$amenities)
listings$Fridge = grepl("refrigerator",listings$amenities)
listings$Microwave = grepl("microwave",listings$amenities)
listings$Dishwasher = grepl("Dishwasher",listings$amenities)
listings$Elevator = grepl("Elevator",listings$amenities)
listings$Freezer = grepl("freezer",listings$amenities)
listings$Iron = grepl("iron",listings$amenities)
listings$TV = grepl("TV",listings$amenities)
listings$Game_console = grepl("Game console",listings$amenities)
listings$Parking = grepl("parking",listings$amenities)
listings$Aircon = grepl("Air conditioning",listings$amenities)
listings$Wifi = grepl("wifi",listings$amenities)
listings = listings %>% 
  mutate(sum_amenities =
           Pool + BBQ + Garden + Balcony +
           Washer + Dryer + Oven + Fridge + Microwave + Dishwasher + Elevator + 
           Freezer + Iron + Parking + Aircon + Wifi + Game_console + TV )

corr_amenities = listings %>%
  select(review_scores_rating,
         sum_amenities, Pool, BBQ, Garden, Balcony,
         Washer, Dryer, Oven, Fridge, Microwave, Dishwasher, Elevator , 
         Freezer, Iron, Parking, Aircon, Wifi, Game_console, TV,
         ) %>%
  cor(use = "complete.obs")

# Rounding up the results
res_amenities <- round(corr_amenities, 2)
res_amenities

liste_amenities = as.data.frame(res_amenities) %>% 
  select(review_scores_rating) %>% 
  arrange(desc(review_scores_rating))

Amenities <- rownames(liste_amenities)
liste_amenities = liste_amenities %>% cbind(Amenities)


#################################
# FINAL CLEANING FOR RSHINY APP #
#################################

# Remove useless objects
rm(i, k, cluster, object)

#Reduce the size of datasets
listings_price = listings_price %>%
  select(id, listing_url, name, property_type, neighbourhood_cleansed, price, city, latitude, longitude,
         accommodates, number_of_reviews, review_scores_rating, host_is_superhost)

listings = listings %>%
  select(-starts_with("maximum")) %>%
  select(-starts_with("calculated")) %>%
  select(-starts_with("minimum")) %>%
  select(-c("scrape_id", "last_scraped", "source", "description", 
            "neighborhood_overview", "neighbourhood","picture_url",
            "host_url", "host_name", "host_location", "host_about", "host_thumbnail_url", "host_picture_url",
            "calendar_updated"))

#Export all data into csv format to integrate them after in the Rshiny app
write.csv(Amenities, "Amenities.csv", row.names = FALSE)
write.csv(BAN_listings, "BAN_listings.csv", row.names = FALSE)
write.csv(BAN_reviews, "BAN_reviews.csv", row.names = FALSE)
write.csv(corr_amenities, "corr_amenities.csv", row.names = FALSE)
write.csv(corr_listings, "corr_listings.csv", row.names = FALSE)
write.csv(couleurs, "couleurs.csv", row.names = FALSE)
write.csv(df, "df.csv", row.names = FALSE)
write.csv(least_expensive, "least_expensive.csv", row.names = FALSE)
write.csv(liste, "liste.csv", row.names = FALSE)
write.csv(liste_amenities, "liste_amenities.csv", row.names = FALSE)
write.csv(listings, "listings.csv", row.names = FALSE)
write.csv(listings_price, "listings_price.csv", row.names = FALSE)
write.csv(listings_summary, "listings_summary.csv", row.names = FALSE)
write.csv(listings_table, "listings_table.csv", row.names = FALSE)
write.csv(merged_data, "merged_data.csv", row.names = FALSE)
write.csv(most_expensive, "most_expensive.csv", row.names = FALSE)
write.csv(res_amenities, "res_amenities.csv", row.names = FALSE)
write.csv(res_listings, "res_listings.csv", row.names = FALSE)
#write.csv(reviews, "reviews.csv")
write.csv(seg_summary, "seg_summary.csv", row.names = FALSE)
