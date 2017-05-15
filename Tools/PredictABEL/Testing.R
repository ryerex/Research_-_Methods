###############################################################################
#
# Project: 	ACO Projects
# Script:	Testing.R
# Version:	
# Created:	
# Updated:	Mar 10, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(PredictABEL)
data(ExampleData)
# specify column number of the outcome variable
cOutcome <- 2
# specify column numbers of non-genetic predictors
cNonGenPred1 <- c(3:10)
cNonGenPred2 <- c(3:10)
# specify column numbers of non-genetic predictors that are categorical
cNonGenPredCat1 <- c(6:8)
cNonGenPredCat2 <- c(6:8)
# specify column numbers of genetic predictors
cGenPred1 <- c(0)
cGenPred2 <- c(11:16)
# specify column numbers of genetic predictors that are categorical
cGenPredsCat1 <- c(0)
cGenPredsCat2 <- c(0)

# fit logistic regression models
riskmodel1 <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,
		cNonGenPreds=cNonGenPred1, cNonGenPredsCat=cNonGenPredCat1,
		cGenPreds=cGenPred1, cGenPredsCat=cGenPredsCat1)
riskmodel2 <- fitLogRegModel(data=ExampleData, cOutcome=cOutcome,
		cNonGenPreds=cNonGenPred2, cNonGenPredsCat=cNonGenPredCat2,
		cGenPreds=cGenPred2, cGenPredsCat=cGenPredsCat2)

# combine output in a list 
ExampleModels <- list(riskModel1=riskmodel1, riskModel2=riskmodel2)

cOutcome <- 2

# fit logistic regression models
# all steps needed to construct a logistic regression model are written in a function
# called 'ExampleModels', which is described on page 4-5
riskmodel1 <- ExampleModels()$riskModel1
riskmodel2 <- ExampleModels()$riskModel2

# obtain predicted risks
predRisk1 <- predRisk(riskmodel1)
predRisk2 <- predRisk(riskmodel2)
# specify cutoff values for risk categories
cutoff <- c(0,.10,.30,1)    

# compute reclassification measures
reclassification(data=ExampleData, cOutcome=cOutcome, 
		predrisk1=predRisk1, predrisk2=predRisk2, cutoff)
