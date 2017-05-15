###############################################################################
#
# Project: 	ACO Projects
# Script:	pecTest.R
# Version:	
# Created:	
# Updated:	Sep 30, 2014
# Author: 	ry5t
###############################################################################
library("pec")
library(parallel)
cl <- makeCluster(detectCores())
library("survival")
library("rms")
library("randomForestSRC")
library("party")
library("prodlim")
data("cost")

cl <- makeCluster(detectCores())

newData <- cost[1,]

newData[1,"age"] = as.integer(28)
newData[1,"othDisease"] = "no"
newData[1,"smoke"] = "no"
newData[1,"strokeScore"] = as.integer(38)
newData[1,"cholest"] = 6

newData <- rbind(newData,newData[1,])
newData <- rbind(newData,newData[1,])

newData[2,"age"] = as.integer(74)
newData[3,"age"] = as.integer(95)

fitform <- Surv(time,status) ~ age + sex + hypTen + ihd + prevStroke + othDisease + alcohol + diabetes + 
		smoke + atrialFib + hemor + strokeScore + cholest

fitcox <- selectCox(fitform, data = cost, rule = "aic")
set.seed(13)
fitrsf <- rfsrc(fitform, data = cost, forest = TRUE, ntree = 100)
set.seed(13)
fitcforest <- pecCforest(fitform, data = cost, controls = cforest_classical(ntree = 100))

pcox <- predictSurvProb(fitcox, newdata = newData, times = 10 * 365.25)
prsf <- predictSurvProb(fitrsf, newdata = newData, times = 10 * 365.25)
extends <- function(...) TRUE
pcf <- predictSurvProb(fitcforest, newdata = newData, times = 10 * 365.25)

par(mfrow = c(1,3))
lapply(1:3, function(x){
			plotPredictSurvProb(fitcox, newdata = newData[x,],lty = 1)
			plotPredictSurvProb(fitrsf, newdata = newData[x, ], add = TRUE, lty = 2)
			plotPredictSurvProb(fitcforest, newdata = newData[x,], add = TRUE, lty = 3)
		})

set.seed(13)
fitpec <- pec(list("selectcox" = fitcox, "rsf" = fitrsf, "cforest" = fitcforest), data = cost, formula = Surv(time, status) ~ 1,
			splitMethod = "Boot632plus", B = 100, M = 350, keep.index = TRUE, keep.matrix = TRUE)
fitpec