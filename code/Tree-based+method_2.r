
#Load packages
library(ggplot2)
library(ISLR)
library(MASS)
library(lmtest)
library(sandwich)
library(tree)
library(randomForest)
library(rpart)
library(rpart.plot)
library(Metrics)
library(gbm)
library(xgverboost)
library(leaps)
library(corrplot)
library(caret)
library(e1071)
library(xgboost)
library(DiagrammeR)

#Load datafiles
load(file = 'data/newdata/trainData3.RData')
load(file = 'data/newdata/testData3.RData')

dim(trainData)
dim(testData)

skewness(trainData$SQUARE_FEET)

is.numeric(trainData$BEDS)

#Drop community_no
trainData = within(trainData, rm('community_no'))
testData = within(testData, rm('community_no'))

#Remove baseline variables:
trainData = within(trainData, rm('PROPERTY_TYPE.Condo/Co-op','NeighRich.1'))
testData = within(testData, rm('PROPERTY_TYPE.Condo/Co-op','NeighRich.1'))

#Fixing some naming issue:
colnames(trainData)[colnames(trainData)=='PROPERTY_TYPE.Single Family Residential'] = "PROPERTY_TYPE.Single.Family.Residential"
colnames(testData)[colnames(testData)=='PROPERTY_TYPE.Single Family Residential'] = "PROPERTY_TYPE.Single.Family.Residential"


names(trainData)

#Using rpart package
#This differs from the tree function in S mainly in its handling of surrogate variables. In most
#details it follows Breiman et. al (1984) quite closely. R package tree provides a re-implementation
#of tree.

tree2 = rpart(PRICE ~., data = trainData)

rpart.plot(tree2)

#Check prediction:
y_true = testData$PRICE
preds2 = predict(tree2, newdata=testData)
plot(preds2, y_true)
abline(0,1)
print('RMSE')
rmse(y_true, preds2)

##Random Forest Model
#========================
set.seed(123456)
bag.chicago = randomForest(PRICE~., data = trainData, ntree = 500, importance = TRUE)
bag.chicago

preds.bag = predict(bag.chicago, newdata = testData)
plot(preds.bag, testData$PRICE)
abline(0,1)
print('RMSE')
rmse(testData$PRICE, preds.bag)


imp_RF = importance(bag.chicago)
imp_RF_features = data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_RF_features = imp_RF_features[order(imp_RF_features$MSE, decreasing = TRUE),]

ggplot(imp_RF_features[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill = MSE)) +
    geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') +
    theme(legend.position="none")


## GBM Model
#======================

train_control_gbm = trainControl(method = "repeatedcv", number = 5, repeats = 5, 
    verboseIter = FALSE, allowParallel = TRUE)

gbm.chicago = train(PRICE ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
    trControl = train_control_gbm, tuneGrid = expand.grid(n.trees = (4:10) * 
        50, interaction.depth = c(6), shrinkage = c(0.05), n.minobsinnode = c(10)), 
    data = trainData, verbose = FALSE)



gbm.chicago

# Predictions
preds.gbm = predict(gbm.chicago, newdata = testData)
rmse(testData$PRICE, preds.gbm)

## XGBoost Models
#=====================================
set.seed(123456)
xgb.chicago1 = xgboost(data = as.matrix(trainData[, -1]), nfold = 5, label = as.matrix(trainData$PRICE), 
    nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
    nthread = 8, eta = 0.05, gamma = 0, max_depth = 6, min_child_weight = 2, 
    subsample = 1, colsample_bytree = 1)

print(xgb.chicago1)

xgb.dump(xgb.chicago1, with_stats = T)
xgb.plot.tree(model = xgb.chicago1, trees = 1:2, render = TRUE)
xgb.plot.multi.trees(model = xgb.chicago1)

imp_xgb = xgb.importance (feature_names = colnames(trainData[,-1]),model = xgb.chicago1)

imp_xgb[1:20,]

ggplot(imp_xgb[1:10,], aes(x=reorder(Feature, Gain), y=Gain, fill = Gain)) +
    geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Feature', y= 'Gain') +
    theme(legend.position="none")

cor(trainData$income_per_capite, trainData$perc_under18_over64)
cor(trainData$income_per_capite, trainData$hardship_index)
cor(trainData$income_per_capite, trainData$min_dist_cta)

summary(lm(income_per_capite~ perc_under18_over64 + hardship_index, data = trainData))

#Drop perc_under18_over64 + hardship_index
trainData2 = data.frame(trainData)
testData2 = data.frame(testData)
trainData2 = within(trainData2, rm('perc_under18_over64', 'hardship_index'))
testData2 = within(testData2, rm('perc_under18_over64', 'hardship_index'))

set.seed(123)
xgb.chicago3 = xgboost(data = as.matrix(trainData2[, -1]), nfold = 5, label = as.matrix(trainData2$PRICE), 
    nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
    nthread = 8, eta = 0.05, gamma = 0, max_depth = 6, min_child_weight = 2, 
    subsample = 1, colsample_bytree = 1)

print(xgb.chicago3)

imp_xgb2 = xgb.importance (feature_names = colnames(trainData2[,-1]),model = xgb.chicago3)
ggplot(imp_xgb2[1:10,], aes(x=reorder(Feature, Gain), y=Gain, fill = Gain)) +
    geom_bar(stat = 'identity') + coord_flip() +
    labs(x = 'Feature', y= 'Gain') +
    theme(legend.position="none")

plot.window(c(0, 20), c(0,100))
xgb.plot.shap(as.matrix(testData2[, -1]), model = xgb.chicago3, top_n = 5)

##Compare predictions with test data
#==========================================
#XGBoost all variables
test_preds.xgb1 = predict(xgb.chicago1, newdata = as.matrix(testData[, -1]))
rmse(testData$PRICE, test_preds.xgb1)

#XGBoost drop multicollinearity
test_preds.xgb3 = predict(xgb.chicago3, newdata = as.matrix(testData2[, -1]))
rmse(testData2$PRICE, test_preds.xgb3)

# Random Forest method
test_preds.RF = predict(bag.chicago, newdata = testData)
rmse(testData$PRICE, test_preds.RF)

#GBM method
test_preds.gbm = predict(gbm.chicago, newdata = testData)
rmse(testData$PRICE, test_preds.gbm)


