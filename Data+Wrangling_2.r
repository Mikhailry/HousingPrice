
#Load packages
library(ggplot2)
library(ISLR)
library(MASS)
library(lmtest)
library(lmtest)
library(sandwich)
library(tree)
library(randomForest)
library(rpart)
library(rpart.plot)
library(Metrics)
library(gbm)
library(xgboost)
library(leaps)
library(corrplot)
library(psych)
library(moments)
library(caret)
library(e1071)

#Load datafiles
load(file = 'data_final.RData')

dim(data_final)

#Fix naming variables
colnames(data_final)[colnames(data_final)=="perc_16+_unempl"] = "perc_16plus_unempl"
colnames(data_final)[colnames(data_final)=="perc_25+_no_school_diploma"] = "perc_25plus_no_school_diploma"

#Drop ZIP variable
data_final = within(data_final, rm('ZIP'))


summary(data_final)

names(data_final)

data_final$community_no = as.factor(data_final$community_no)
#data_final$BEDS = as.factor(data_final$BEDS)
#data_final$BATHS = as.factor(data_final$BATHS)
data_final$PROPERTY_TYPE = as.factor(data_final$PROPERTY_TYPE)
data_final$LOCATION = as.factor(data_final$LOCATION)

#Resolve community variables (too many categories)
nb1 = ggplot(data_final, aes(x=reorder(community_no, PRICE, FUN=median), y=PRICE)) +
        geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='community_no', y='Median Price') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

nb2 = ggplot(data_final, aes(x=reorder(community_no, PRICE, FUN=mean), y=PRICE)) +
        geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='community_no', y="Mean Price") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(nb1, nb2)

mean_p_community = aggregate( PRICE ~ community_no, data_final, mean )
mean_p_community[order(-mean_p_community$PRICE),][1:11,]

med_p_community = aggregate( PRICE ~ community_no, data_final, median )
med_p_community[order(-med_p_community$PRICE),][1:11,]

#Create categories for the extremes
data_final$NeighRich[data_final$community_no %in% c(7,8,32,5,24,6,22,4,33,28,12)] = 2
data_final$NeighRich[!data_final$community_no %in% c(54,67,68,51,46,30,7,8,32,5,24,6,22,4,33,28,12)] = 1
data_final$NeighRich[data_final$community_no %in% c(54,67,68,51,46,30)] = 0

table(data_final$NeighRich, data_final$community_no)

#Factor NeighRich
data_final$NeighRich = as.factor(data_final$NeighRich)

y1 = ggplot(data_final, aes(x=reorder(YEAR_BUILT, PRICE, FUN=median), y=PRICE)) +
        geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='YEAR_BUILT', y='Median Price') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

y2 = ggplot(data_final, aes(x=reorder(YEAR_BUILT, PRICE, FUN=mean), y=PRICE)) +
        geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='YEAR_BUILT', y="Mean Price") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice


grid.arrange(y1, y2)

# Create a new variable: Age of the building
# current (time data was retrieved) - year built
data_final$HouseAge = data_final$YEAR_BUILT
data_final$HouseAge = sapply(data_final$HouseAge, function(x) 2018 - x)


summary(data_final$HouseAge)

#Put Year_Built back to factor
data_final$YEAR_BUILT = as.factor(data_final$YEAR_BUILT)

qplot(PRICE, data = data_model_f, bins = 50, main = "Right skewed distribution")

ggplot(data_final, aes(x=BATHS, y=PRICE)) +
        geom_bar(stat='summary', fun.y = "median", fill='black')

numericVars = which(sapply(data_final, is.numeric))
factorVars = which(sapply(data_final, is.factor))
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

#Geting numeric variables
numericVars = data_final[,numericVars] #index vector numeric variables
numericVarNames = names(numericVars) #saving names vector for use later on
numericVarNames

#Check correlation on all numeric variables
cor_numVar = cor(numericVars, use="pairwise.complete.obs") 

#sort on decreasing correlations with PRICE
cor_sorted = as.matrix(sort(cor_numVar[,'PRICE'], decreasing = TRUE))
 #select only high corelations
CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3)))
cor_numVar = cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

plot(numericVars$SQUARE_FEET, numericVars$PRICE)

#Drop some variables
data_final2 = within(data_final, rm('LOCATION','YEAR_BUILT','URL'))

names(data_final2)


#Check skewness of numeric predictors:
skewed_stats = c()
var_names = c()
for (i in 1:ncol(numericVars)){
    skewed_stats = append(skewed_stats,abs(skew(numericVars[,i])))
    var_names = append(var_names, numericVarNames[i])
}

skewed_DF = data.frame(cbind(var_names, skewed_stats))
skewed_DF = skewed_DF[order(skewed_DF$skewed_stats, decreasing = TRUE),]

skewed_DF

#Set a new dataset before fixing skewness
data_final3 = data_final2

skewness(data_final3$SQUARE_FEET)

qqnorm(data_final3$PRICE)
qqline(data_final3$PRICE)

## Log transformation of the target variable
#data_final3$PRICE = log(data_final3$PRICE + 1)
data_final3$PRICE = log(data_final3$PRICE)

## Normal distribution after transformation
qplot(PRICE, data = data_final3, bins = 50, main = "Normal distribution after log transformation")

qqnorm(data_final3$PRICE)
qqline(data_final3$PRICE)

factorVars = which(sapply(data_final3, is.factor))
dummies = dummyVars(~., data_final3[names(factorVars)[-1]])
data_dmy = predict(dummies, data_final3[names(factorVars)[-1]])
data_model = cbind(data_final3, data_dmy)

names(data_model)

#Drop baseline variables
data_model = within(data_model, rm('PROPERTY_TYPE','NeighRich'))

dim(data_model)
names(data_model)

#setting seed
set.seed(11222018)

trainInd = createDataPartition(data_model$PRICE, p = 0.8, list = FALSE)
trainData = data_model[trainInd,]
testData =  data_model[-trainInd,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(data_model))

summary(trainInd)

dim(trainData)
dim(testData)

#Save data
save(data_model, file = 'data/newdata/data_model2.RData')
save(trainData, file = 'data/newdata/trainData3.RData')
save(testData, file = 'data/newdata/testData3.RData')




