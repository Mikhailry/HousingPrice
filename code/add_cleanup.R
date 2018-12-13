library(ggplot2)
library(ISLR)
library(MASS)
library(lmtest)
library(ggthemes)
library(xlsx)
library(lmtest)
library(sandwich)
library(foreign)
library(caret)

#### Data Input and Data Cleaning
path = "/Users/Hailey/Documents/GitHub/HousingPrice"
setwd(dir = path)

#Load house data
load("data/compiled_data.RData")
housedata = compiled_data
dim(housedata)
#Load education data
edu_data = read.csv("Chicago_edu_data/Chicago_school_area_ratings.csv")
dim(edu_data)
summary(edu_data)
#Merge 2 datasets
data_final = merge(housedata, edu_data[c('Community.Area.Number','Percent_level1','Percent_level2')], 
                   by.x = 'community_no', by.y = 'Community.Area.Number' )
dim(data_final)

#Clean up some column names and variable types
colnames(data_final)[colnames(data_final)=="Percent_level1"] <- "percent_level1_school"
colnames(data_final)[colnames(data_final)=="Percent_level2"] <- "percent_level2_school"

summary(data_final)
View(data_final)

class(data_final$num_cta_1mile)
data_final$num_cta_1mile = as.numeric(data_final$num_cta_1mile)
data_final$min_dist_cta = as.numeric(data_final$min_dist_cta)

class(data_final$num_cta_1mile)
class(data_final$min_dist_cta)
#Save final data
save(data_final, file = 'data_final.RData')

table(data_final$PROPERTY_TYPE)

#### Set up variable selection process
#Set dummy property types
unique(data_final$ZIP)
data_final$ZIP = as.factor(data_final$ZIP)
data_final$community_no = as.factor(data_final$community_no)
dmy = dummyVars(~ PROPERTY_TYPE, data = data_final)
dmy1 = dummyVars(~ ZIP, data = data_final)
dmy2 = dummyVars(~ community_no, data = data_final)
data_dmy = data.frame(predict(dmy, newdata = data_final))
data_dmy2 = data.frame(predict(dmy2, newdata = data_final))
data_model = cbind(data_final, data_dmy, data_dmy2)
View(data_model)
summary(data_model)


#Drop some columns not using in the model
data_model_f = within(data_model, rm('ZIP','community_no','PROPERTY_TYPE','LOCATION','URL','PROPERTY_TYPESingle.Family.Residential','community_no.77'))
summary(data_model_f)

save(data_model_f, file = 'data_model.RData')


