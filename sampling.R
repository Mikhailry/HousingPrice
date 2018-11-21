#Split data into train/test datasets

#install.packages('caret') #uncomment to install
library('caret')

#load data
load('data_model.Rdata')

#setting seed
set.seed(11162018)

#Doing stratified sampling
trainInd <- createDataPartition(data_model_f$PRICE, p = 0.8, list = FALSE)
trainData <- data_model_f[trainInd,]
testData <- data_model_f[-trainInd,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(data_model_f))

save(testData, file = "data/testData.RData")
save(trainData, file = "data/trainData.RData")
