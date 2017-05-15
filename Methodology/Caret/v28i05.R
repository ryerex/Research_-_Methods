## Data preparation

library("caret")
library("QSARdata")
data(Mutagen)
mutagen <- Mutagen_Outcome[1:400]
descr <- Mutagen_Dragon[1:400]

set.seed(1)
inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr  <- descr[-inTrain,]

trainClass <- mutagen[inTrain]
testClass  <- mutagen[-inTrain]

prop.table(table(mutagen))
prop.table(table(trainClass))

ncol(trainDescr)

## Remove predictors with one distinct value
isZV <- apply(trainDescr, 2, function(u) length(unique(u)) == 1)
trainDescr <- trainDescr[, !isZV]
testDescr  <-  testDescr[, !isZV]

descrCorr <- cor(trainDescr)
highCorr <- findCorrelation(descrCorr, 0.90)

trainDescr <- trainDescr[, -highCorr]
testDescr  <-  testDescr[, -highCorr]
ncol(trainDescr)

xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr  <- predict(xTrans,  testDescr)

## Building and tuning models

bootControl <- trainControl(number = 200)
set.seed(2)
svmFit <- train(trainDescr, trainClass,
                method = "svmRadial",
                tuneLength = 5,
                trControl = bootControl,
                scaled = FALSE)
svmFit
svmFit$finalModel

gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25,
                       .shrinkage = .1)
set.seed(2)
gbmFit <- train(trainDescr, trainClass,
                method = "gbm",
                trControl = bootControl,
                verbose = FALSE,
                bag.fraction = 0.5,                
                tuneGrid = gbmGrid)

## Prediction of new samples

predict(svmFit$finalModel, newdata = testDescr)[1:5]

predict(svmFit, newdata = testDescr)[1:5]
models <- list(svm = svmFit,
               gbm = gbmFit)
testPred <- predict(models, newdata = testDescr)
lapply(testPred, function(x) x[1:5])
predValues <- extractPrediction(models)
                       
testValues <- subset(predValues,
                     dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testDescr)

probValues <- extractProb(models)

testProbs <- subset(
                    probValues,
                    dataType == "Test")
str(testProbs)



## Characterizing performance

svmPred <- subset(testValues, model == "svmRadial")
confusionMatrix(svmPred$pred, svmPred$obs)

svmProb <- subset(testProbs, model == "svmRadial")
svmROC <- roc(svmProb$mutagen, svmProb$obs)
str(svmROC)



## Predictor importance

gbmImp <- varImp(gbmFit, scale = FALSE)
gbmImp


