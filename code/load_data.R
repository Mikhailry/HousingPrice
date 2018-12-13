setwd('/Users/mikhailrybalchenko/Documents/Projects/HousePrices/data')

#load libraries
#install.packages('tidyverse') #uncomment to install
library('tidyverse')
#install.packages('data.table') #uncomment to install
library('data.table')
#install.packages('rlist') #uncomment to install
library('rlist')

#get a list of files in the directory with the following pattern redfin.*.csv
files <- list.files(pattern = "redfin.*.csv")
files


#load all the files as data frames
listDf <- lapply(files, fread)


#let's aggregate all data frames into one
data<-rbindlist(listDf)

head(data)
summary(data)

#save data
save(data, file = "house.RData")
