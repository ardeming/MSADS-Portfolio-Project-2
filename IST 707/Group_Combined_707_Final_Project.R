
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
#                           Nolan 
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-


#Nolan Mercado
#Professor Gates
#IST 707 Project

#Selecting each column by `` in between the variable
#Example : `Very Ahead +5`

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(broom, caret, magrittr, skimr, tidyverse, data.table, psych, rio, rattle, ggplot2, corrplot)

# READING IN THE DATA ####################################

Air_BnB_Listing <- read_csv("/Users/nolanmercado/Downloads/IST 707/Assessments/Project/NY_AirBnB_listings.csv") %>%
  print()

Air_BnB_Reviews <- read_csv("/Users/nolanmercado/Downloads/IST 707/Assessments/Project/NY_AirBnB_reviews.csv") %>%
  print()

#Creating a Copy of the Data. (Must have "data.table" package installed)
nolan_df <- copy(Air_BnB_Listing)
nolan_km <- copy(Air_BnB_Listing)

nolan_df <- nolan_df %>%
  mutate(price = as.numeric(sub("\\$","", nolan_df$price))) %>%
  print()

nolan_km <- nolan_km %>%
  mutate(price = as.numeric(sub("\\$","", nolan_df$price))) %>%
  print()

nolan_df2 <- nolan_df %>%
  as_tibble() %>%
  select(neighbourhood_group_cleansed, latitude, longitude, 
         room_type, price, minimum_nights, number_of_reviews,
         reviews_per_month, calculated_host_listings_count, availability_365) %>%
  mutate(price = as.numeric(sub("\\$","", nolan_df$price))) %>%
  print()

# FILTER BY CERTAIN VARIABLES ####################################

NYC_Bronx <- nolan_df2 %>%
  filter(neighbourhood_group_cleansed == "Bronx") %>%
  print()

NYC_Brooklyn<- nolan_df2 %>%
  filter(neighbourhood_group_cleansed == "Brooklyn") %>%
  print()

NYC_Manhattan <- nolan_df2 %>%
  filter(neighbourhood_group_cleansed == "Manhattan") %>%
  print()

NYC_Queens <- nolan_df2 %>%
  filter(neighbourhood_group_cleansed == "Queens") %>%
  print()

NYC_Staten_Island <- nolan_df2 %>%
  filter(neighbourhood_group_cleansed == "Staten Island") %>%
  print()

# MAP VISUALIZATION ####################################
#states <- map_data("state")

#NYC <- map_data(map = "county", region = "New York")

#NYC_plot <- ggplot(NYC) + 
#  geom_polygon(mapping=aes(x=long, y = lat, group = group), color = "black", fill = "beige") +
#  coord_map() +
#  theme() +
#  geom_point(data = nolan_df, mapping = aes(x = longitude, y = latitude, color = "red")) + 
#  coord_cartesian(xlim=c(-72.54321, -75.54321), ylim = c(40.3, 41.25)) +
#  theme(legend.position = "none")

#NYC_plot

# SIMPLE ANALYSIS ####################################

#BarChart AirBnB Bookings by Borough

nolan_df %>%
  ggplot()+
  geom_bar(aes(neighbourhood_group_cleansed), fill = "#1D76B5", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(
    title = "Count of AirBnB Bookings By Boroughs",
    caption = "From AirBnB Listing Dataset",
    y = "Number of Bookings",
    x = NULL
  )

nolan_df %>% 
  count(neighbourhood_group_cleansed) %>%
  print()

#Boxplot of Median Price for AirBnB by Bourough

NYC_MedianPrice <- function(borough){
  nolan_df2 %>%
    filter(neighbourhood_group_cleansed == borough) %>%
    select(price) %>%
    boxplot(
      main = paste("Boxplot of 'Price of AirBnB' by Borough: ", borough), 
      col = "#1D76B5",
      ylab = "Price is $",
      xlab = paste("Borough:", borough),
      horizontal = TRUE
    )
}

NYC_MedianPrice('Bronx')
NYC_MedianPrice('Brooklyn')
NYC_MedianPrice('Manhattan')
NYC_MedianPrice('Queens')
NYC_MedianPrice('Staten Island')

unique(nolan_df2$neighbourhood_group_cleansed)

boxplot(price ~ neighbourhood_group_cleansed,
        data = nolan_df2, 
        horizontal =TRUE,
        main = "Boxplot of Prices in NYC",
        col = 'steelblue',
        ylab = "Borough",
        xlab = "Price in $")

tapply(nolan_df2$price, nolan_df2$neighbourhood_group_cleansed, summary) #Check Statistic Summary

summary(nolan_df$price)

#Check NA colSums
colSums(is.na(nolan_df2))

# AIR_BNB_DATAFRAME CLEAN ####################################

nolan_df2$reviews_per_month[is.na(nolan_df$reviews_per_month)] <- 0
nolan_df2$availability_365 = ifelse(nolan_df$availability_365 == 365, 1, 0)
nolan_df2$reviews_per_month[is.na(nolan_df2$reviews_per_month)] <- 0
nolan_df2 <- nolan_df2 %>% drop_na()
colSums(is.na(nolan_df2))
#nolan_df2$availability_365 <- as.factor(nolan_df2$availability_365)

#Converting Strings to Numeric
nolan_df2$room_type[nolan_df2$room_type == "Entire home/apt"] <- 1
nolan_df2$room_type[nolan_df2$room_type == "Private room"] <- 2
nolan_df2$room_type[nolan_df2$room_type == "Hotel room"] <- 3
nolan_df2$room_type[nolan_df2$room_type == "Shared room"] <- 4
nolan_df2$room_type <- as.numeric(nolan_df2$room_type)

#Converting Strings to Numeric
nolan_df2$neighbourhood_group_cleansed[nolan_df2$neighbourhood_group_cleansed == "Manhattan"] <- 1
nolan_df2$neighbourhood_group_cleansed[nolan_df2$neighbourhood_group_cleansed == "Brooklyn"] <- 2
nolan_df2$neighbourhood_group_cleansed[nolan_df2$neighbourhood_group_cleansed == "Queens"] <- 3
nolan_df2$neighbourhood_group_cleansed[nolan_df2$neighbourhood_group_cleansed == "Staten Island"] <- 4
nolan_df2$neighbourhood_group_cleansed[nolan_df2$neighbourhood_group_cleansed == "Bronx"] <- 5
nolan_df2$neighbourhood_group_cleansed <- as.numeric(nolan_df2$neighbourhood_group_cleansed)

str(nolan_df2)

#Check NA colSums
colSums(is.na(nolan_df2))

NYC_Brooklyn %>%
  as_tibble() %>%
  select(price) %>%
  filter(price == "Cheap") %>%
  print()

# CORRELATION MATRIX  ####################################
cor(nolan_df2)

#correlation matrix raw numbers
nolan_df2 %>%
  cor() %>%
  round(2)

nolan_df2 %>%
  cor() %>%
  corrplot(
    type = "upper",
    diag = F,
    order = "original",
    tl.col = "black",
    tl.srt = 45
  )

# LOGISTIC REGRESSION MODEL  ####################################
str(nolan_df2)
str(nolan_df)

fit <- glm(
  availability_365 ~ .,
  data = nolan_df2,
  family = "binomial"
)

# Summarize regression model
fit %>% summary()
fit %>% tidy()

# Confidence intervals for coefficients
fit %>% confint()

# PREDICTED VALUES  ####################################

# See the first few predicted values
predict(fit, type = 'response') %>% head()

# Add predicted values to df
nolan_df2 %<>%
  mutate(
    available = ifelse(
      availability_365 == 1,
      1,
      0
    )
  ) %>%
  select(
    availability_365, available, everything()
  ) %>% print()


predict_value_table<- nolan_df2 %<>%
  mutate(
    predicted_percentage = predict(fit, type = 'response'),
    pred_availability = ifelse(
      predicted > 0.5,
      "P_Available_365",
      "P_NotAvailable_365"
    )
  ) %>%
  select(
    availability_365,
    available,
    predicted,
    pred_availability,
    everything()
  ) %>%
  print()

predict_value_table %>%
  filter(pred_availability == "P_Available_365") %>%
  print()

#Confusion Matrix Probability

nolan_df2 %$%
  table(available, pred_availability) 

#Percentages for confusion matrix
nolan_df2 %$%
  table(available, pred_availability) %>%
  prop.table(1) %>%
  round(2)

# BUILDING THE DECISION TREE  ####################################

#Discretization to cut Price by the quartile

NYC_Bronx$price <- cut(NYC_Bronx$price, breaks = c(0,59,85,109,111,130,Inf), 
                       labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

NYC_Brooklyn$price <- cut(NYC_Brooklyn$price, breaks = c(0,68,108,141,143,175,Inf), 
                          labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

NYC_Manhattan$price <- cut(NYC_Manhattan$price, breaks = c(0,100,165,211,213,259,Inf), 
                           labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

NYC_Queens$price <- cut(NYC_Queens$price, breaks = c(0,56,85,115,117,140,Inf), 
                        labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

NYC_Staten_Island$price <- cut(NYC_Staten_Island$price, breaks = c(21,70,100,130,132,150,Inf), 
                               labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

nolan_df$price <- cut(nolan_df$price, breaks = c(0,75,125,164,167,200,Inf), 
                      labels = c("Cheap", "Great Price", "Affordable Price", "Average Price", "Moderate Price", "Expensive"))

# Partitioning all dataframes for into train and test sets

#NYC All Boroughs
NYC_All_train <- nolan_df %>% sample_frac(.70)
NYC_All_test <- nolan_df %>% anti_join(NYC_All_train)

NYC_All_train$reviews_per_month[is.na(NYC_All_train$reviews_per_month)] <- 0
NYC_All_train <- NYC_All_train %>% drop_na()

NYC_All_train$room_type <- as.factor(NYC_All_train$room_type)
NYC_All_train$neighbourhood_group_cleansed <- as.factor(NYC_All_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_All_train))

str(NYC_All_train)

dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_All_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Price from 8 Factors",
    sub = "Training Data"
  )

#BRONX
NYC_Bronx_train <- NYC_Bronx %>% sample_frac(.70)
NYC_Bronx_test <- NYC_Bronx %>% anti_join(NYC_Bronx_train)

NYC_Bronx_train$reviews_per_month[is.na(NYC_Bronx_train$reviews_per_month)] <- 0
NYC_Bronx_train <- NYC_Bronx_train %>% drop_na()

NYC_Bronx_train$room_type <- as.factor(NYC_Bronx_train$room_type)
NYC_Bronx_train$neighbourhood_group_cleansed <- as.factor(NYC_Bronx_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_Bronx_train))

str(NYC_Bronx_train)

Bronx_dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_Bronx_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

Bronx_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Bronx Price from 8 Factors",
    sub = "Training Data"
  )

#BROOKLYN
NYC_Brooklyn_train <- NYC_Brooklyn %>% sample_frac(.70)
NYC_Brooklyn_test <- NYC_Brooklyn %>% anti_join(NYC_Brooklyn_train)

NYC_Brooklyn_train$reviews_per_month[is.na(NYC_Brooklyn_train$reviews_per_month)] <- 0
NYC_Brooklyn_train <- NYC_Brooklyn_train %>% drop_na()

NYC_Brooklyn_train$room_type <- as.factor(NYC_Brooklyn_train$room_type)
NYC_Brooklyn_train$neighbourhood_group_cleansed <- as.factor(NYC_Brooklyn_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_Brooklyn_train))

str(NYC_Brooklyn_train)

Brooklyn_dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_Brooklyn_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

Brooklyn_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Brooklyn Price from 8 Factors",
    sub = "Training Data"
  )

#MAHATTAN
NYC_Manhattan_train <- NYC_Manhattan %>% sample_frac(.70)
NYC_Manhattan_test <- NYC_Manhattan %>% anti_join(NYC_Manhattan_train)

NYC_Manhattan_train$reviews_per_month[is.na(NYC_Manhattan_train$reviews_per_month)] <- 0
NYC_Manhattan_train <- NYC_Manhattan_train %>% drop_na()

NYC_Manhattan_train$room_type <- as.factor(NYC_Manhattan_train$room_type)
NYC_Manhattan_train$neighbourhood_group_cleansed <- as.factor(NYC_Manhattan_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_Manhattan_train))

str(NYC_Manhattan_train)

Manhattan_dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_Manhattan_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

Manhattan_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Manhattan Price from 8 Factors",
    sub = "Training Data"
  )

Manhattan_dt

#QUEENS
NYC_Queens_train <- NYC_Queens %>% sample_frac(.70)
NYC_Queens_test <- NYC_Queens %>% anti_join(NYC_Queens_train)

NYC_Queens_train$reviews_per_month[is.na(NYC_Queens_train$reviews_per_month)] <- 0
NYC_Queens_train <- NYC_Queens_train %>% drop_na()

NYC_Queens_train$room_type <- as.factor(NYC_Queens_train$room_type)
NYC_Queens_train$neighbourhood_group_cleansed <- as.factor(NYC_Queens_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_Queens_train))

str(NYC_Queens_train)

Queens_dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_Queens_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

Queens_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Queens Price from 8 Factors",
    sub = "Training Data"
  )

Queens_dt

#STATEN ISLAND
NYC_Staten_Island_train <- NYC_Staten_Island %>% sample_frac(.70)
NYC_Staten_Island_test <- NYC_Staten_Island %>% anti_join(NYC_Staten_Island_train)

NYC_Staten_Island_train$reviews_per_month[is.na(NYC_Staten_Island_train$reviews_per_month)] <- 0
NYC_Staten_Island_train <- NYC_Staten_Island_train %>% drop_na()

NYC_Staten_Island_train$room_type <- as.factor(NYC_Staten_Island_train$room_type)
NYC_Staten_Island_train$neighbourhood_group_cleansed <- as.factor(NYC_Staten_Island_train$neighbourhood_group_cleansed)

colSums(is.na(NYC_Staten_Island_train))

str(NYC_Staten_Island_train)

Staten_Island_dt <- train(
  price ~ longitude + room_type + minimum_nights + reviews_per_month,
  data = NYC_Staten_Island_train,
  method = "rpart",
  trControl = trainControl(method = "cv")
)

Staten_Island_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Staten_Island Price from 8 Factors",
    sub = "Training Data"
  )

Staten_Island_dt


# FILTER BY CERTAIN VARIABLES ####################################
#nolan_km <- select_if(nolan_km, is.numeric)

#nolan_km %>%
#  as_tibble() %>%
#  select(neighbourhood_group_cleansed, latitude, longitude, 
#         room_type, price, minimum_nights, number_of_reviews,
#         reviews_per_month, calculated_host_listings_count, availability_365) %>%
#  mutate(price = as.numeric(sub("\\$","", nolan_df$price))) %>%
#  print()




#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
#                           Allison 
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-



######################################################################################################################################
################################################### Allison R. Deming ################################################################
################################################## IST 77 Data Mining ################################################################
###################################################### Project File ##################################################################
###################################################### 09/22/2022 ####################################################################
######################################################################################################################################

################################################### PACKAGE LOADING ##################################################################

library(pacman) #Installing a Package Manager
pacman::p_load(tidyr, dplyr, visdat, tidyverse, RColorBrewer, wordcloud2, tm, magrittr, caret, e1071, wordcloud, ggplot2, data.table, plyr, rio, tidyverse, tidytext)

################################################ LOADING IN THE TRUNCATED DATA ########################################################

getwd() #Gets current working directory
setwd("C:/Users/16512/Desktop/")
getwd() #Checking to make sure it is what it should be which is the Desktop

#These are the smaller, truncated files visualization

Listings<-read.csv("C:/Users/16512/Desktop/listings.csv", 
                   na.string=c("")) #Reading in the data and putting it in a data frame
Reviews<-read.csv("C:/Users/16512/Desktop/reviews.csv", 
                  na.string=c("")) #Reading in the data and putting it in a data frame

################################################LOADING IN THE LARGE DATA#############################################################
#Getting the larger version of the files

getwd()
ListingsLarge<-read.csv("C:/Users/16512/Desktop/ListingsLarge.csv", 
                        na.string=c("")) #Reading in the data and putting it in a data frame
ReviewsLarge<-read.csv("C:/Users/16512/Desktop/ReviewsLarge.csv", 
                       na.string=c("")) #Reading in the data and putting it in a data frame

##########################################INITIAL STRUCTURE/SUMMARY OF LARGE DATA#####################################################

str(ListingsLarge) #Structure for FULL Listings
str(ReviewsLarge) #Structure for FULL Reviews
summary(ListingsLarge) #Summary for FULL Listings
summary(ReviewsLarge) #Summary for FULL Reviews

#####################################################NROW/NCOL DATA###################################################################

nrow(ListingsLarge) #37410
ncol(ListingsLarge) #74
nrow(ReviewsLarge) #985674
ncol(ReviewsLarge) #6
nrow(Listings) #39881
ncol(Listings) #18
nrow(Reviews) #1048575
ncol(Reviews) #2

#######################################SAMPLING THE DATA FOR VIZUALIZATIONS#################################################

ListingsLarge %>% sample_frac(0.01) %>% vis_dat() #Reducing the sample size for Visualization on the large data set
ReviewsLarge %>% sample_frac(0.01) %>% vis_dat() #Reducing the sample size for visualization on the large data set
Listings %>% sample_frac(0.1) %>% vis_dat() #Reducing the sample size for Viz on Missing Values on the small data set
Reviews %>% sample_frac(0.1) %>% vis_dat() #Reducing the sample size for Viz on Missing values on the smaller data set

########################################################PLOTS###############################################################

#Setting some basic parameters for some plots
basic=theme(panel.grid.major=element_blank(),#Setting some parameters where the graphs look the same
            panel.grid.minor=element_blank(), #Leaving these blank 
            panel.background=element_blank(), #leaving this blank
            axis.line.x =element_line(color ="black"), #Black X axis
            axis.line.y =element_line(color ="black"), #Y Axis also black
            legend.key =element_rect(fill ="grey"),  #Fill with grey for the legend
            text=element_text(size=10)) #Text size 10 had to play with this a bit to get it right

#Log transformed Price for Listings since it is right skewed a lot
ggplot(Listings, aes(price)) + #ggplot listings log transformed
  basic+ #Some basic Parameters
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + #Histogram
  geom_density(alpha = 0.2, fill = "purple") +ggtitle("Log-Transformed Distribution of Price",) + scale_x_log10() #Log transform to normal distribution

############################################Average Price by Type of Room Available###########################################################

ggplot(Listings, aes(x = room_type, y = mean(price), fill = room_type))+ #ggplot for room type by mean
  geom_bar(stat = "identity")+theme_minimal()+ #ggplot for bar graph
  basic+ #Some basic parameters
  labs(title = "Average Price by Type of Room Available", #Label on Top
       x = "Room Type", y = "Average Price") #X and Y Label

############################################Average Price by Type of Borough###########################################################

ggplot(Listings, aes(x = fct_infreq(neighbourhood_group), y = mean(price), fill = neighbourhood_group))+ #Price by Borough
  geom_bar(stat = "identity")+ #Bar plot
  basic+ #Some basic parameters
  labs(title = "Average Price By Borough", #Main Label
       x = "Neighbourhood Group", y = "Price") + #X and Y Label
  theme(legend.position = "right") #Putting the legend on the right

############################################Property type by Borough Stacked Bar###########################################################

ggplot(Listings, aes(x = fct_infreq(neighbourhood_group), fill= room_type))+ #Stacked Bar Chart
  geom_bar()+ #Bar Chart
  basic+ #Some basic parameters for all graphs
  labs(title = "Property Type by Borough", #Labels
       x = "Neighbourhood Group", y = "Number of Listings") + #Labels
  theme(legend.position = "right") #Legend to the right side

############################################Top 10 Neighborhoods by Listings###########################################################

Listings %>%
  group_by(neighbourhood) %>%
  dplyr::summarize(num_listings = n(), 
                   borough = unique(neighbourhood_group)) %>%
  top_n(n = 10, wt = num_listings) %>%
  ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
             y = num_listings, fill = borough)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Top 10 Neighborhoods by Listings",
       x = "Neighborhood", y = "Number of Listings")

############################################BOROUGH MAP###############################################################################

height <- max(Listings$latitude) - min(Listings$latitude)
width <- max(Listings$longitude) - min(Listings$longitude)
LA_borders <- c(bottom  = min(Listings$latitude)  - 0.1 * height, 
                top     = max(Listings$latitude)  + 0.1 * height,
                left    = min(Listings$longitude) - 0.1 * width,
                right   = max(Listings$longitude) + 0.1 * width)

map <- get_stamenmap(LA_borders, zoom = 10, maptype = "toner-lite")
ggmap(map) +
  geom_point(data = Listings, mapping = aes(x = longitude, y = latitude, 
                                            col = log(price))) +
  scale_color_distiller(palette = "RdYlGn", direction = 1)

ggplot(Listings, aes(latitude, longitude, color = neighbourhood_group)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Airbnb Locations",
       x = "Latitude",
       y = "Longitude",
       color = "Boroughs")

##############################################BOXPLOT OF ROOM TYPE BY BOROUGH#########################################################

ggplot(Listings, aes(x = room_type, y = price, fill = room_type)) + scale_y_log10() + 
  geom_boxplot() +
  theme_minimal() +
  labs (x="", y= "Price") +
  facet_wrap(~neighbourhood_group) +
  facet_grid(.~ neighbourhood_group) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "right") +
  labs(title = "Room Type by Borough",
       fill = "Type of Room")  
###############################################LISTINGS BY NEIGHBORHOOD MOST POPULAR###############################################

Listings %>%
  group_by(neighbourhood) %>%
  dplyr::summarize(num_listings = n(), 
                   borough = unique(neighbourhood_group)) %>%
  top_n(n = 10, wt = num_listings) %>%
  ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
             y = num_listings, fill = borough)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Most Popular Neighborhoods",
       x = "Neighborhood", y = "Number of Listings")

#############################################PROPERTY TYPE BY BOROUGH#################################################

ggplot(Listings, aes(x = (neighbourhood_group), fill= room_type))+
  geom_bar()+
  labs(title = "Property Types by Borough",
       x = "Group",  y= "Number of Listings")+
  theme(legend.position = "right") 

###########################################HISTOGRAM BY BOROUGH BY PRICE##############################################
Listings %>%
  filter(as.numeric(gsub("\\$", "", price)) < 1000) %>%
  mutate(price = as.numeric(gsub("\\$", "", price))) %>%
  ggplot() +
  geom_histogram(mapping = aes(price, fill=neighbourhood_group), binwidth=10) +
  labs(fill = NULL)

##################################################THE END#############################################################





#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
#                           Alex  
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-


# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(tidyverse, data.table, factoextra, cluster,ggmap,
               tidytext, tm)



# READING IN THE DATA ####################################
setwd("~/Documents/Graduate_School/IST_707/PROJECT")


Air_BnB_Listing <- read_csv("listings.csv") %>% glimpse()



Air_BnB_Reviews <- read_csv("reviews.csv") %>% glimpse()



#Creating a Copy of the Data. (Must have "data.table" package installed)
alex_df_listings <- copy(Air_BnB_Listing)
alex_df_reviews <- copy(Air_BnB_Reviews)


#google maps register api key
register_google(key="AIzaSyDHQQduZyjZGBKIwaY_nP_KtW1xg33bPnU", write=TRUE)

newyork.map <- get_map(location= 'New York', 
                       maptype='roadmap', color='bw',source='google',zoom=10)


newyork.map.locs <- get_map(location= 'New York', 
                            maptype='roadmap', color='bw',source='google',zoom=12)

#################### Cleaning

# Creating a list of columns to are not wanted
drop<-c('listing_url', 'scrape_id', 'last_scraped', 'name', 'description',
        'picture_url', 'host_url', 'host_since', 'host_location', 'host_about',
        'ost_response_time', 'host_response_rate', 'host_acceptance_rate',
        'host_is_superhost', 'host_thumbnail_url', 'host_picture_url', 'host_neighbourhood',
        'host_listings_count', 'host_total_listings_count','host_verifications', 'host_has_profile_pic',
        'host_identity_verified', 'first_review', 'last_review', 'license', 'instant_bookable',
        'calculated_host_listings_count','calculated_host_listings_count_entire_homes', 
        'calculated_host_listings_count_shared_rooms', 'calculated_host_listings_count_private_rooms',
        'reviews_per_month', 'has_availability', 'calendar_updated', 'bathrooms', 'calendar_last_scraped',
        'neighbourhood')

# Removing unwanted columns
alex_df_listings_clean <- alex_df_listings[,!(names(alex_df_listings) %in% drop)]

# Creating a list of words to remove from the Property types
remove_words<- c('Entire', 'Private room in', 'Shared room in', 'Room in', 'shared bath', 'private bath', 'bath',
                 'shared baths', 'baths', 'private baths', 'Half-', 'Shared half-', 'Private half-')

# Removing the unwanted words from the property types
alex_df_listings_clean$property_type <- trimws(gsub(paste0(remove_words, collapse = "|"),"", alex_df_listings_clean$property_type),'l')
alex_df_listings_clean$property_type<- as.factor(alex_df_listings_clean$property_type)

# Converting the neighbourhood_cleansed into a factor
alex_df_listings_clean$neighbourhood_cleansed<- as.factor(alex_df_listings_clean$neighbourhood_cleansed)

# Removing $ and , characters from the price and converting to a numeric
alex_df_listings_clean$price<- gsub("\\$|,","",alex_df_listings_clean$price) %>% as.numeric()

# Cleaning up the bathroom text column to have the number of bathrooms
alex_df_listings_clean$bathrooms_text<- trimws(gsub(paste0(remove_words, collapse = "|"),"", alex_df_listings_clean$bathrooms_text),'r')
alex_df_listings_clean$bathrooms_text<- as.numeric(alex_df_listings_clean$bathrooms_text)
alex_df_listings_clean$bathrooms_text[is.na(alex_df_listings_clean$bathrooms_text)] <- 0

alex_df_listings_clean %>% group_by(property_type) %>% summarize(count=n()) %>% arrange(desc(count))

# Exploring if the Price values has outliers for each of the 5 boroughs
boxplot(price~neighbourhood_group_cleansed, alex_df_listings_clean, horizontal = TRUE, ylab= 'Boroughs',
        xlab = 'Airbnb price', main ='Boxplot of Airbnb Price In The 5 Boroughs', col ='steelblue')

# Calculating the Quantile 1, 3 and Inter-Quantile Range
q1<- quantile(alex_df_listings_clean$price, prob = .25)
q3<- quantile(alex_df_listings_clean$price, prob = .75)

IQR<- IQR(alex_df_listings_clean$price)

#  Calculating the Upper and Lower Outlier values
Lower_qrt<-q1 - 1.5 * IQR
Upper_qrt<-q3 + 1.5 * IQR

# Removing the Outliers
df_no_outliers<- copy(alex_df_listings_clean)

df_no_outliers<- subset(df_no_outliers, df_no_outliers$price > Lower_qrt & df_no_outliers$price < Upper_qrt)

#  Cleaned Outliers data by borough
Manhattan_df<- df_no_outliers %>% filter(neighbourhood_group_cleansed %in% c('Manhattan'))
Queens_df<- df_no_outliers %>% filter(neighbourhood_group_cleansed %in% c('Queens'))
Brooklyn_df<- df_no_outliers %>% filter(neighbourhood_group_cleansed %in% c('Brooklyn'))
Bronx_df<- df_no_outliers %>% filter(neighbourhood_group_cleansed %in% c('Bronx'))
SI_df<- df_no_outliers %>% filter(neighbourhood_group_cleansed %in% c('Staten Island'))

# Boxplot of the Airbnb Prices for the 5 boroughs after removing the outliers
boxplot(price~neighbourhood_group_cleansed, df_no_outliers, horizontal = TRUE, ylab= 'Boroughs',
        xlab = 'Airbnb price', main ='Boxplot of Airbnb Price In The 5 Boroughs', col ='steelblue')

# Exploring the Airbnb Price for Manhattan
boxplot(price~neighbourhood_group_cleansed, Manhattan_df, xlab= 'Neighbourhood',
        ylab = 'price', main ='Boxplot of Airbnb price in Manhattan')


############# K-Means Analysis

# Creating a subset data frame from the no outliers data frame
df_Kmeans<- df_no_outliers %>% select(c(price,neighbourhood_group_cleansed, longitude,latitude))

# Converting each borough into a numeric for kmeans model
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == "Manhattan"] <- 1
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == "Brooklyn"] <- 2
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == "Queens"] <- 3
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == "Staten Island"] <- 4
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == "Bronx"] <- 5

#Converting Strings to Numeric
df_Kmeans$neighbourhood_group_cleansed<- as.numeric(df_Kmeans$neighbourhood_group_cleansed)

# Setting the seed and performing kmeans with k = 3
set.seed(005)
k_means_price<- kmeans(df_Kmeans,3)
k_means_price

# Transforming the clusters into a factor
df_Kmeans$ny_cluster<- k_means_price$cluster %>% as.factor()

# Converting the numeric back into the Neighborhood String
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == 1] <- 'Manhattan'
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == 2] <- 'Brooklyn'
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == 3] <- 'Queens'
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == 4] <- 'Staten Island'
df_Kmeans$neighbourhood_group_cleansed[df_Kmeans$neighbourhood_group_cleansed == 5] <- 'Bronx'

# Trasnforming the Neighborhood group cleansed into a factor
df_Kmeans$neighbourhood_group_cleansed<- as.factor(df_Kmeans$neighbourhood_group_cleansed)

# Grouping the clusters into there own data frames
Expensive_group <- df_Kmeans %>% filter(ny_cluster == 3)
Average_group<- df_Kmeans %>% filter(ny_cluster == 2)
Cheap_group<- df_Kmeans %>% filter(ny_cluster == 1)

# Barplot to visualize the breakdown of the Airbnb price groups in the 5 boroughs
ggplot(data = df_Kmeans, aes(x = price/10000, y = neighbourhood_group_cleansed, fill = ny_cluster)) + 
  geom_bar(position="stack",stat='identity') +
  scale_fill_discrete(labels = c('Expensive', 'Average', 'Cheap')) +
  guides(fill = guide_legend(title = "Pricing")) + 
  xlab('Airbnb Price') +
  ylab('Boroughs') +
  ggtitle('Airbnb Pricing Breakdown In The 5 Boroughs')

# Mapping out all three Airbnb price groups in the 5 Boroughs
k_means_map <- ggmap(newyork.map) + 
  geom_point(data= df_Kmeans,aes(x=longitude,y=latitude, color = ny_cluster),size=.5,alpha=0.75) +
  theme(axis.ticks = element_blank(),axis.text = element_blank()) +
  xlab('') +
  ylab('') +
  ggtitle('Airbnb Pricing Breakdown In The 5 Boroughs')

k_means_map

# Mapping out the Expensive Airbnbs in the 5 boroughs
Expensive_map <- ggmap(newyork.map) + 
  geom_point(data= Expensive_group,aes(x=longitude,y=latitude, color = ny_cluster),size=.5,alpha=0.75) +
  theme(axis.ticks = element_blank(),axis.text = element_blank()) +
  xlab('' ) +
  ylab('') +
  ggtitle('Location of Expensive Airbnb')

Expensive_map

# Mapping out the Average Airbnbs in the 5 boroughs
Average_map <- ggmap(newyork.map) + 
  geom_point(data= Average_group,aes(x=longitude,y=latitude, color = ny_cluster),size=.5,alpha=0.75)+
  theme(axis.ticks = element_blank(),axis.text = element_blank()) +
  xlab('' ) +
  ylab('') +
  ggtitle('Location of Average Airbnb')

Average_map

# Mapping out the Cheap Airbnbs in the 5 boroughs
Cheap_map <- ggmap(newyork.map) + 
  geom_point(data= Cheap_group,aes(x=longitude,y=latitude, color = ny_cluster),size=.5,alpha=0.75) +
  theme(axis.ticks = element_blank(),axis.text = element_blank()) +
  xlab('' ) +
  ylab('') +
  ggtitle('Location of Cheap Airbnb')

Cheap_map





#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
#                           MIKAYLA 
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

#Importing Libraries 
library("cowplot")
library("googleway")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("tigris")
library("dplyr")
library("leaflet")
library("sp")
library("ggmap")
library("maptools")
library("broom")
library("httr")
library("rgdal")
library("ggmap")
library("tidyr")
library("stringr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("knitr")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
library("rpart.plot")
library("rpart")
library(caret)
library(rattle)

#read csvs into data frames 
reviews = read.csv("NY_AirBnB_reviews.csv")
listings = read.csv("NY_AirBnB_listings.csv")
nycLocations = read.csv("nyc_data_cleaned.csv")
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


#character conversion and removing NAs and removing $
nycLocations$Lat = as.numeric(nycLocations$Lat)
nycLocations$Long = as.numeric(nycLocations$Long)

head(listings$longitude)

listings$price = gsub("\\$", "", listings$price)
listings$price = as.numeric(listings$price)

listings$longitude = as.numeric(listings$longitude)
listings$latitude = as.numeric(listings$latitude)


nycLocations = na.omit(nycLocations)

#Mapping the neighborhoods 
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))


#google maps register api key
register_google(key="AIzaSyCAwLNxEU-xmEE74hwZchoDIwzjlWmsF8o", write=TRUE)

newyork.map <- get_map(location= 'New York', 
                       maptype='roadmap', color='bw',source='google',zoom=10)


newyork.map.locs <- get_map(location= 'New York', 
                            maptype='roadmap', color='bw',source='google',zoom=12)

#Map of all the listings 
listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude),size=.5, colour="red",alpha=0.75)+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')


listings.map

#Map of all the listings by price
listings$longitude
listings$latitude

price.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=price),size=.25,alpha=0.75)+
  scale_colour_gradient(high="red",low='green')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

price.listings.map

##Map of all the listings by review score 
reviews.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=review_scores_rating ),size=.25,alpha=0.75)+
  scale_colour_gradient(high="green",low='red')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

reviews.listings.map

#Map of all the listings by 
availability.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=availability_365),size=.25,alpha=0.75)+
  scale_colour_gradient(high="green",low='red')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

availability.listings.map


#Map of tourist attractions 
nycLocations.map = ggmap(newyork.map.locs) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=price),size=.25,alpha=0.75)+
  geom_point(data=nycLocations,aes(x=Long,y=Lat),size=1, colour="black",alpha=0.75)+
  scale_colour_gradient(high="red",low='green')+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

nycLocations.map

#neighborhood map
listings$neighbourhood_group_cleansed

brooklyn = subset(listings, neighbourhood_group_cleansed=="Brooklyn")
queens = subset(listings, neighbourhood_group_cleansed=="Queens")
manhattan = subset(listings, neighbourhood_group_cleansed=="Manhattan")
bronx = subset(listings, neighbourhood_group_cleansed=="Bronx")
statenIsland = subset(listings, neighbourhood_group_cleansed=="Staten Island")

nrow(brooklyn)
nrow(queens)
nrow(manhattan)
nrow(bronx)
nrow(statenIsland)

listingsCount = c(nrow(brooklyn),
                  nrow(queens),
                  nrow(manhattan),
                  nrow(bronx),
                  nrow(statenIsland))
listingsCount


listings$neighbourhood_group_cleansed[listings$neighbourhood_group_cleansed == "Manhattan"] <- 1
listings$neighbourhood_group_cleansed[listings$neighbourhood_group_cleansed == "Brooklyn"] <- 2
listings$neighbourhood_group_cleansed[listings$neighbourhood_group_cleansed == "Queens"] <- 3
listings$neighbourhood_group_cleansed[listings$neighbourhood_group_cleansed == "Staten Island"] <- 4
listings$neighbourhood_group_cleansed[listings$neighbourhood_group_cleansed == "Bronx"] <- 5
listings$neighbourhood_group_cleansed <- as.numeric(listings$neighbourhood_group_cleansed)


nycNeighbor.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=neighbourhood_group_cleansed),size=.25,alpha=0.75)+
  scale_colour_gradient(high="red",low='green')+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

nycNeighbor.map

#+=+=+=++=+=+=+=+=+=+=+=+=+=+=+=++=+=+=+=+=+=+=+=+=+=+=+=++=+=+=+=+=+=+=+=+=


#NLP stuff 
reviews$comments<- gsub("@\\w+", "", reviews$comments)
reviews$comments <- gsub("https.+", "", reviews$comments)
reviews$comments <- gsub("\\d+\\w*\\d*", "", reviews$comments)
reviews$comments <- gsub("#\\w+", "", reviews$comments)
reviews$comments <- gsub("[^\x01-\x7F]", "", reviews$comments)
reviews$comments <- gsub("[[:punct:]]", " ", reviews$comments)
reviews$comments <- gsub("\n", " ", reviews$comments)
reviews$comments <- gsub("^\\s+", "", reviews$comments)
reviews$comments <- gsub("\\s+$", "", reviews$comments)
reviews$comments <- gsub("[ |\t]+", " ", reviews$comments)


#On small sample
sampleDf <- sample_n(reviews, 10000)

commentsCorpus = Corpus(VectorSource(sampleDf$comments))
commentsCorpus <- tm_map(commentsCorpus, content_transformer(tolower))
commentsCorpus <- tm_map(commentsCorpus, removeNumbers)
commentsCorpus <- tm_map(commentsCorpus, removeWords, stopwords("english"))
commentsCorpus <- tm_map(commentsCorpus, removePunctuation)
commentsCorpus <- tm_map(commentsCorpus, stripWhitespace)

TermDocMat <- TermDocumentMatrix(commentsCorpus)
mat <- as.matrix(TermDocMat)
words <- sort(rowSums(mat), decreasing=TRUE)
wordDF <- data.frame(word=names(words), freq=words)
inspect(TermDocMat)

wordcloud(words = wordDF$word, freq = wordDF$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
par(mar = c(1, 1, 1, 1))

freqTerms <- findFreqTerms(TermDocMat, 1000)
freqTerms

#memory.limit(size=56000)
#wordFreq = colSums(as.matrix(TermDocMat))

findAssocs(TermDocMat, c("great", "place", "clean", "stay", 
                         "apartment", "location", "clean", "host", 
                         "manhattan", "comfortable", "recommend", "perfect",
                         "nice", "wonderful"), corlimit=0.1)

#on big data set 
commentsCorpusBig = Corpus(VectorSource(reviews$comments))
commentsCorpusBig <- tm_map(commentsCorpusBig, content_transformer(tolower))
commentsCorpusBig <- tm_map(commentsCorpusBig, removeNumbers)
commentsCorpusBig <- tm_map(commentsCorpusBig, removeWords, stopwords("english"))
commentsCorpusBig <- tm_map(commentsCorpusBig, removePunctuation)
commentsCorpusBig <- tm_map(commentsCorpusBig, stripWhitespace)

TermDocMatBig <- TermDocumentMatrix(commentsCorpusBig)
inspect(TermDocMatBig)

freqTermsBig <- findFreqTerms(TermDocMatBig, 50000)
freqTermsBig

findAssocs(TermDocMatBig, c("great", "place", "clean", "stay", 
                            "apartment", "location", "clean", "host", 
                            "manhattan", "comfortable", "recommend"), corlimit=0.1)



polarityScores = read.csv("polarity_scores_labeled.csv")
summary(polarityScores)

polarityScores['id'] = polarityScores['listing_id']

df_merge_id <- merge(listings,polarityScores,by="id")


#listings by sentiment of reviews 
df_merge_id$tag[df_merge_id$tag == "positive"] <- 1
df_merge_id$tag[df_merge_id$tag == "neutral"] <- 2
df_merge_id$tag[df_merge_id$tag == "negative"] <- 3

df_merge_id$tag = as.numeric(df_merge_id$tag)

nycSentiment.map = ggmap(newyork.map) + 
  geom_point(data=df_merge_id,aes(x=longitude,y=latitude, color=tag),size=0.25,alpha=0.75)+
  scale_colour_gradient(high="red",low='green')+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

nycSentiment.map


dim(df_merge_id)
summary(df_merge_id)

#decision trees
testing_training <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


trainingSet = testing_training(df_merge_id)
dim(trainingSet)

testingSet=testing_training(df_merge_id, size = 0.2)
dim(testingSet)

prop.table(table(trainingSet$tag))
prop.table(table(testingSet$tag))
summary(trainingSet)
fit <- rpart(tag~pos+neu+neg+comp, data = trainingSet, method = 'class')
rpart.plot(fit, extra = 106)
print(fit)

predict<-predict(fit, testingSet, type = 'class')
table_mat <- table(testingSet$tag, predict)
table_mat

accu <- sum(diag(table_mat)) / sum(table_mat)

table_mat_2 <- table(traingingSet$tag)
table_mat_2

trainingSet$review_scores_rating <- as.numeric(trainingSet$review_scores_rating)
trainingSet$price <- as.numeric(trainingSet$price)
trainingSet

model3 <- train(tag ~ pos+neu+neg+comp, data = trainingSet, 
                metric = "class", method = "rpart")

prediction3 <- predict(model3, newdata = testingSet, 
                       na.action = na.omit, type = "raw")

plot(model3)

table_mat <- table(testingSet$tag, predict)
table_mat

plot(model2$trainingData)
