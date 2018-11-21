#We will get additional data per community level (Chicago consists of 77 communities)

#load libraries
#install.packages('tidyverse') #uncomment to install
library('tidyverse')
#install.packages('data.table') #uncomment to install
library('data.table')

#install.packages('stringr') #uncomment to install
#library for working with strings
library(stringr)

#library to convert between degrees and radians
#install.packages('pracma') #uncomment to install
library('pracma')

#load crime data
crime <- read_csv('data/crimes_2018.csv')

#let's drop unnessesary columns
crime <- crime %>% select(`Primary Type`, `Community Area`)

#let's sum number of crimes per community and drop na's
crime <- crime %>% group_by(`Community Area`) %>% summarise(n()) %>% filter(`Community Area`!= 0 & !is.na(`Community Area`))

#change column names for crime df
colnames(crime) <- c('community_no', 'crime_count')

#=============================

#next step is to load census data
census <- read_csv('data/census_2010.csv')

#we drop all columns but first three
census <- census[,1:3]

#change column names for census data
colnames(census) <- c('community', 'community_no', 'population')

#==============================
socioecon <- read_csv('data/Census_Data_socioeconomics.csv')

#drop summary row
socioecon <- socioecon[1:77,]

#change names for socioeconomic data
colnames(socioecon) <- c('community_no', 'community', 'perc_housing_crowded', 
                         'perc_household_below_poverty', 'perc_16+_unempl',
                         'perc_25+_no_school_diploma', 'perc_under18_over64', 
                         'income_per_capite', 'hardship_index')

#drop unnessasary columns
socioecon <- socioecon %>% select('community_no', 'perc_housing_crowded', 
                                  'perc_household_below_poverty', 'perc_16+_unempl',
                                  'perc_25+_no_school_diploma', 'perc_under18_over64', 
                                  'income_per_capite', 'hardship_index')

#==============================

#load life expectancy data
life_exp <- read_csv('data/life_exp.csv')
life_exp <- life_exp %>% select(`Community Area Number`, `2010 Life Expectancy`)

#drop summary row
life_exp <- life_exp[1:77,]

#change column names
colnames(life_exp) <- c('community_no', 'life_exp_2010')

#===============================

#load public health indicators data
ph <- read_csv('data/Public_Health_indicators.csv')

ph <- ph %>% select("Community Area", "Unemployment")

colnames(ph) <- c('community_no', 'unemployment')

#===============================

#let's join additional data by community number
extra_data <- inner_join(census, crime, by='community_no')

#calculate crime per 1000 people living in community 
extra_data <- extra_data %>% mutate(pop_in_1000 = population/1000) %>% 
  mutate(crime_per_1000 = crime_count/pop_in_1000) %>% select("community", "community_no", "crime_per_1000")

#join life_exp, unemployment, socioeconomics
extra_data <- inner_join(extra_data, life_exp, by='community_no')
extra_data <- inner_join(extra_data, ph, by='community_no')
extra_data <- inner_join(extra_data, socioecon, by='community_no')

#===============================

#load cta L stations
cta <- read_csv('data/CTA_L.csv')

#select only distinct station names
cta<-distinct(cta, cta$STATION_NAME, .keep_all=T)

#let's split location column into lat and long
l <- unlist(str_match_all(cta$Location, pattern = '-?\\d{2}.\\d{2,}'))
lat_long <- as.data.frame(matrix(l,ncol = 2, byrow=TRUE))
colnames(lat_long) <- c('stop_lat', 'stop_long')

#bind lat_long to cta df
cta <- bind_cols(cta,lat_long)

#select columns stop_id, lat, long
cta <- cta %>% select('STOP_ID', 'stop_lat', 'stop_long')

as.numeric(as.character(cta$stop_lat[1]))

#convert 'stop_lat', 'stop_long' into numeric
cta$stop_lat <- as.numeric(as.character(cta$stop_lat))
cta$stop_long <- as.numeric(as.character(cta$stop_long))

#function to calculate manhattan distance between two lat- and long- coordinates 
manhattanDist <- function(lat1, long1, lat2, long2){
  dlat = abs(deg2rad(lat1)-deg2rad(lat2))
  dlong = abs(deg2rad(long1) - deg2rad(long2))
  
  a <- sin(dlat/2)*sin(dlat/2)
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  latDist <- 6371 * c
  
  a <- sin(dlong/2)*sin(dlong/2)
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  longDist <- 6371 * c
  
  manh_dist <- abs(latDist) + abs(longDist)
  return (manh_dist)
}


#load house data
load('data/cleanedRedfin.Rdata')

data <- data_imputed
data_imputed <- NULL

#add new column 'min_dist_cta'
data$min_dist_cta <- ""
data$num_cta_1mile <- ""
cta$cur_dist <- ""

#calculate min distance to CTA L station and add as column 'min_dist_cta'
#count number of CTA L stations within 1 mile (1.60934 km)
for (i in 1:nrow(data)){
  for (m in 1:nrow(cta)){
    cta$cur_dist[m] <- manhattanDist(data$LATITUDE[i], data$LONGITUDE[i], cta$stop_lat[m], cta$stop_long[m])
  }
  data$min_dist_cta[i] <- min(cta$cur_dist)
  data$num_cta_1mile[i] <- sum(cta$cur_dist <= 1.60934) 
}

#adding extra_data (12 variables) to each observation in data
compiled_data <- inner_join(data, extra_data, by=c('LOCATION' = 'community'))

summary(compiled_data)

#save compiled_data
save(compiled_data, file = "data/compiled_data.RData")