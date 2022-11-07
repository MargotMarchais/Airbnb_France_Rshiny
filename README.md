# Airbnb_France_Rshiny

**Data preparation, visualization and analysis written in R, with a Rshiny web app deployment on shinyapps.io.**

**How to use:**

* _Airbnb_Rcode_French_market.R_: The R code that prepares the data for the Rshiny app. Note: For readibility purposes, you may rather read the Markdown file "Airbnb_France_data_prep_Markdown" (available in RMarkdown and PDF formats). It prints some outputs of the R code and gives some additional explanations so that is easier to understand.
* _Airbnb_Rshiny_app.R_ : The Rshiny app structure. The file contains both the server and the UI parts. It was used to deploy the app on the website shinyapps.io. 
* _End product_: You can access it here: https://margot-marchais-maurice.shinyapps.io/Airbnb_Database_Rstudio/. Note: I am using the free version of shinyapps.io website, therefore there may be some performance issues at times - especially regarding the maps. If you have troubles accessing the app, you can still refer to the preview video below to have a gist of what it is about :-)

Note: The underlying data are too large (about 700 Mo) to be hosted on Github. However, it is very easy to replicate what I have done by downloading your own datasets on http://insideairbnb.com/get-the-data.     
<br />
    
**Summary:** 
In this repositery, I explain how I built a Rshiny web app that gives an overview of the Airbnb French market (cities analyzed: Paris, Lyon, Bordeaux).
I detail the preparation of the data, the construction of the web app (server and UI) as well as the data analysis (through summary statistics, data visualization, kmeans clustering and leaflet maps). In the case study, I answered the following questions:
* How many listings and hosts are on Airbnb French website ?
* How many reviews are produced yearly ?
* What are the main characteristics of the listings ? Where are they located ?
* Who are the hosts / what are their different profiles (segmentation) ? 
* What is the distribution of prices, across cities and property types ?
* What is the average guest satisfaction and what are its main drivers ?    
<br />

**Learnings:** 
I made this project to refresh my R and Rshiny skills, and also have some fun while making the kmeans segmentation.
In this project, I encountered most of the difficulties in:
* Creating all the ggplot data visualizations: This project made me realize the incredible power of "drag and drop" BI tools such as Tableau software (that I tend to take for granted at work!).
* Keeping the code well documented, clean and organized: The code of the Rshiny app may become very easily a mess as it is a big file, with lots of objects and easy to forget punctuation (parenthesis, commas,...). While developing the app, I had to commit my changes very often. This allowed me to revert to previous versions of the code if my new changes broke the app.
* Deploying to production: I received many cryptic error messages which prevented me to publish my app and it took time to decipher / solve the problems. Some of the errors that I encountered: 
   * My web app could not find the data sources, therefore it could not display any graph (the import of csv files resolved the problem)
   * My web app had too voluminous data sources (more than 1 Go) and the free version of shinyapps.io refused to host the app (I reduced the size of the data sources by eliminating all the columns that I did not use in the case study, and I gave up the idea to merge my dataset with other data sources). 
   * The maps would be be displayed on the remote app, while it is working perfectly locally
   * etc
In a nutshell, it took quite some time to make everything work!
<br />

**Video overview of the end product:**


https://user-images.githubusercontent.com/116331323/200163557-1e13d41b-1bdc-4da7-b9bf-146c78409ea2.mp4

