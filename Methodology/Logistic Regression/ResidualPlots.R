###############################################################################
#
# Project: 	ACO Projects
# Script:	ResidualPlots.R
# Version:	
# Created:	
# Updated:	Mar 26, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(data.table)
library(aod)
library(pec)
library(timereg)
library(xtable)
library(plyr)
library(reshape)
library(survival)
library(rms)
library(randomForestSRC)
library(party)
library(prodlim)
library(ggplot2)
library(kimisc)
set.seed(410)
#source("http://freakonometrics.free.fr/probit.R")
#reg=glm(Y~X1+X2,family=binomial)
#x11()
#plot(reg,which=1)
result_1 <- cbind(cbind(Y,reg$fitted.values),reg$residuals)
load('Data/TranPathData.Rdata')
test <- sample.rows(observations, 500)
Y <- test$readmit30Days
X1 <- test$Charlson
X2 <- test$LOSp
reg=glm(Y~X1,family=binomial)
x11()
plot(reg,which=1)
result_2 <- cbind(cbind(Y,reg$fitted.values),reg$residuals)