#install.packages('ggcorrplot') #uncomment to install
#install.packages('leaflet') #uncomment to install
library('ggcorrplot') #plot correlations
library('tidyverse') #data manipulation package
library('leaflet') #for working with maps
library('scales') #for price representation
library('ggplot2') #plotting
library('RColorBrewer') #choose colors from palette
library('htmlwidgets') #to save maps
library('rgdal')  #working with shape files
library('ggmap')  #getting coordinates for place


#load data
load('data_final.Rdata')

#let's check summary statistics first
summary(data_final)

colnames(data_final)

#let's identify numeric variables
numericVars <- c('PRICE', 'BEDS', 'BATHS', 'SQUARE_FEET', 'YEAR_BUILT', 
                 'LATITUDE', 'LONGITUDE', 'min_dist_cta', 'num_cta_1mile', 
                 'crime_per_1000', 'life_exp_2010', 'unemployment', 'perc_housing_crowded', 
                 'perc_household_below_poverty', 'perc_16+_unempl', 'perc_25+_no_school_diploma', 
                 'perc_under18_over64', 'income_per_capite', 'hardship_index',
                 'percent_level1_school', 'percent_level2_school')

#let's identify categorical variables
catVars <- c('PROPERTY_TYPE', 'LOCATION')

#data_final[,numericVars]
#data_final[,catVars]

#======================================
#histograms of all numeric vars
#======================================
data_final[,numericVars] %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + labs(title='Histograms of numeric variables', x='', y='')


#======================================
#let's first compute correlation matrix
#and create correlation plot
#======================================
corr <- round(cor(data_final[,numericVars]), 2)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white", method = "circle")


#======================================
#Histogram of the house prices
#======================================
ggplot(data_final, aes(x=PRICE)) + geom_histogram(fill="steelblue", alpha = .8) + 
  ggtitle('House prices distribution') + 
  scale_x_continuous(labels = scales::comma)


#======================================
#Bar plots of all categorical variables
#======================================
ggplot(data_final[,catVars], aes(PROPERTY_TYPE)) + geom_bar(fill="steelblue", alpha = .8) + ggtitle('Distribution of offers by property type')

## set the levels in order
data_final <- within(data_final, LOCATION <- factor(LOCATION, levels=names(sort(table(LOCATION), decreasing=TRUE))))
ggplot(data_final, aes(LOCATION)) + geom_bar(fill="steelblue", alpha = .8) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('Distribution of offers by community')

#======================================
#Spatial analysis: data preparation
#======================================

#load shape file of Chicago communities
chicom <- readOGR("data/chicago.shp")

#get coordinates
geocode("Chicago", output = "more", source = "dsk")

#let's aggregate our data per community and join it with data in shape file
dataViz <- data_final %>% group_by(community_no) %>% 
  summarise(meanPrice = mean(PRICE), sdPrice=sd(PRICE), coefVar=(sdPrice/meanPrice), medianPrice=median(PRICE), dollarsPerSqf=(medianPrice/median(SQUARE_FEET)),
            minPrice=min(PRICE), maxPrice=max(PRICE), medianYearB=median(YEAR_BUILT), meanYearB=mean(YEAR_BUILT),
            crimePer1000=mean(crime_per_1000), percLevel1Schools=mean(percent_level1_school), percLevel2Schools=mean(percent_level2_school))

#convert area_number in chicom@data to numeric
chicom@data$area_numbe <- as.numeric(as.character(chicom@data$area_numbe))
chicom@data$area_num_1 <- as.numeric(as.character(chicom@data$area_num_1))

#now joining data with data in shape file
chicom@data <- chicom@data %>% 
  inner_join(dataViz, by = c("area_numbe" = "community_no"))

#check data for LOOP
chicom@data %>% filter(community=='LOOP')

#======================================
#Spatial analysis: median house price, Crime per 1000 per community level, Coefficient of price variation per community level,
#Distribution of house year built on map, Percent of schools of different levels,
#======================================

#choosing colors from palette
#display.brewer.all()

#setting pallette on a log scale
npal_crime <- colorNumeric("Reds", domain = chicom@data$crimePer1000)
npal_meanPrice <- colorNumeric("Blues", domain = chicom@data$meanPrice)
npal_medianPrice <- colorNumeric("Blues", domain = chicom@data$medianPrice)
npal_coefVar <- colorNumeric("Greens", domain = chicom@data$coefVar)
npal_minPrice <- colorNumeric("Blues", domain = chicom@data$minPrice)
npal_maxPrice <- colorNumeric("Blues", domain = chicom@data$maxPrice)
npal_dollarsPerSqf <- colorNumeric("Blues", domain = chicom@data$dollarsPerSqf)
npal_medianYearB <- colorNumeric("Blues", domain = chicom@data$medianYearB)
npal_meanYearB <- colorNumeric("Blues", domain = chicom@data$meanYearB)
npal_percLevel1Sch <- colorNumeric("Greens", domain = chicom@data$percLevel1Schools)
npal_percLevel2Sch <- colorNumeric("Greens", domain = chicom@data$percLevel2Schools)

#quantile palette
#qpal <- colorQuantile("Blues", chicom@data$crimePer1000, n = 7)

#to represent price in monetary format
usd <- dollar_format(prefix = "$")

finalmap <- chicom %>% leaflet() %>% 
  #add layer with crime rate
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_crime(crimePer1000), 
              label = ~paste(community, ' - ', "Crime per 1000: ", crimePer1000),
              highlight = highlightOptions(weight = 5, color = "red", bringToFront = TRUE), group = 'crime rate') %>% 
  #add layer with house mean price
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_meanPrice(meanPrice), 
              label = ~paste(community, ' - ', "House mean price: ", usd(meanPrice)),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'house mean price') %>%
  #add layer with house median price
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_medianPrice(medianPrice), 
              label = ~paste(community, ' - ', "House median price: ", usd(medianPrice)),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'house median price') %>%
  #add layer with house price coefficient of variation
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_coefVar(coefVar), 
              label = ~paste(community, ' - ', "Price coef. of Var: ", coefVar),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'price coef. of Var') %>%
  #add layer with house min price
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_minPrice(minPrice), 
              label = ~paste(community, ' - ', "House min price: ", usd(minPrice)),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'house min price') %>%
  #add layer with house max price
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_maxPrice(maxPrice), 
              label = ~paste(community, ' - ', "House max price: ", usd(maxPrice)),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'house max price') %>%
  #add layer with dollars per sqf
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_dollarsPerSqf(dollarsPerSqf), 
              label = ~paste(community, ' - ', "Dollars per Sqf: ", usd(dollarsPerSqf)),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'dollars per sqf') %>%
  #add layer with median year built
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_medianYearB(medianYearB), 
              label = ~paste(community, ' - ', "Median year built: ", medianYearB),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'median year built') %>%
  #add layer with mean year built
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_meanYearB(meanYearB), 
              label = ~paste(community, ' - ', "Mean year built: ", meanYearB),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'mean year built') %>%
  #add layer with percent level 1 schools
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_percLevel1Sch(percLevel1Schools), 
              label = ~paste(community, ' - ', "Percent level 1 schools: ", percLevel1Schools),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'level1 schools') %>%
  #add layer with percent level 2 schools
  addPolygons(weight = 1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.8, color = ~npal_percLevel2Sch(percLevel2Schools), 
              label = ~paste(community, ' - ', "Percent level 2 schools: ", percLevel2Schools),
              highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE), group = 'level2 schools') %>%
  
  #add legend layers
  addLegend(pal = npal_crime, values = ~crimePer1000, opacity = 0.75, title = "Crime rate per 1000", position = "bottomleft", group = 'crime rate') %>%
  addLegend(pal = npal_meanPrice, values = ~meanPrice, opacity = 0.75, title = "House mean price", position = "bottomleft", group = 'house mean price') %>%
  addLegend(pal = npal_medianPrice, values = ~medianPrice, opacity = 0.75, title = "House median price", position = "bottomleft", group = 'house median price') %>%
  addLegend(pal = npal_coefVar, values = ~coefVar, opacity = 0.75, title = "Price coef. of Var", position = "bottomleft", group = 'price coef. of Var') %>%
  addLegend(pal = npal_minPrice, values = ~minPrice, opacity = 0.75, title = "House min price", position = "bottomleft", group = 'house min price') %>%
  addLegend(pal = npal_maxPrice, values = ~maxPrice, opacity = 0.75, title = "House max price", position = "bottomleft", group = 'house max price') %>%
  addLegend(pal = npal_dollarsPerSqf, values = ~dollarsPerSqf, opacity = 0.75, title = "Dollars per Sqf", position = "bottomleft", group = 'dollars per sqf') %>%
  addLegend(pal = npal_medianYearB, values = ~medianYearB, opacity = 0.75, title = "Median year built", position = "bottomleft", group = 'median year built') %>%
  addLegend(pal = npal_meanYearB, values = ~meanYearB, opacity = 0.75, title = "Mean year built", position = "bottomleft", group = 'mean year built') %>%
  addLegend(pal = npal_percLevel1Sch, values = ~percLevel1Schools, opacity = 0.75, title = "Percent level 1 schools", position = "bottomleft", group = 'level1 schools') %>%
  addLegend(pal = npal_percLevel2Sch, values = ~percLevel2Schools, opacity = 0.75, title = "Percent level 2 schools", position = "bottomleft", group = 'level2 schools') %>%
  
  # add basemaps with groups
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB.PositronNoLabels", group = "Carto") %>% 
  addProviderTiles("Esri.NatGeoWorldMap", group = "Esri") %>%
  setView(lng = -87.65005, lat = 41.875919, zoom = 10) %>%
  # add layer controls for base and overlay groups
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), overlayGroups = c('crime rate','house mean price', 'house median price','price coef. of Var',
                                                                             'house min price', 'house max price', 'dollars per sqf', 'median year built', 
                                                                             'mean year built', 'level1 schools', 'level2 schools')) %>%
  hideGroup(c('crime rate','house median price', 'price coef. of Var', 'house min price', 'house max price', 'dollars per sqf', 
              'median year built','mean year built', 'level1 schools', 'level2 schools'))
  #set bounds for maps
  #setMaxBounds(lng1 = -87.47623 + .05, lat1 = 42.07454 + .05, 
  #             lng2 = -87.9828 + .05, lat2 = 41.60723 + .05)

finalmap

#saveWidget(finalmap, file="houseMap.html")

#======================================
#Distribuion of prices based on bath/beds
#======================================

#let's convert 'beds' and 'baths' variables to factor
data_final$BEDS <- factor(data_final$BEDS)
data_final$BATHS <- factor(data_final$BATHS)

#side-by-side boxplots: price distribution based on number of beds
ggplot(data=data_final, aes(x=BEDS, y=PRICE, fill=BEDS)) + geom_boxplot() + 
  labs(title = 'Distribution of house prices based on number of beds') + 
  scale_y_continuous(labels = scales::comma)

#side-by-side boxplots: price distribution based on number of baths
ggplot(data=data_final, aes(x=BATHS, y=PRICE, fill=BATHS)) + geom_boxplot() + 
  labs(title = 'Distribution of house prices based on number of baths') + 
  scale_y_continuous(labels = scales::comma)

#======================================
#House prices vs square feet
#======================================
#House price vs square feet
ggplot(data=data_final, aes(x=SQUARE_FEET, y=PRICE)) + geom_point() + geom_smooth() +
  labs(title = 'House prices vs square feet') + 
  scale_y_continuous(labels = scales::comma) 

#======================================
#Proximity to CTA / CTA stations with house prices
#======================================
#House price vs proximity to CTA
ggplot(data=data_final, aes(x=min_dist_cta, y=PRICE)) + geom_point() + geom_smooth() +
  labs(title = 'House prices vs minimum distance to CTA') + 
  scale_y_continuous(labels = scales::comma) 
