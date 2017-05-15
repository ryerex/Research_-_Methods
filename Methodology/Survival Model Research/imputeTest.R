###############################################################################
#
# Project: 	ACO Projects
# Script:	imputeTest.R
# Version:	
# Created:	
# Updated:	Jun 25, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
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
library(caret)
library(data.table)
library(kimisc)
library(beepr)
set.seed(410)
## Not run: 
## ------------------------------------------------------------
## example of survival imputation
## ------------------------------------------------------------

#imputation using outcome splitting
data(pbc, package = "randomForestSRC")
pbc.d <- impute.rfsrc(Surv(days, status) ~ ., data = pbc, nsplit = 3)

#when no formula is given we default to unsupervised splitting
pbc2.d <- impute.rfsrc(data = pbc, nodesize = 1, nsplit = 10, nimpute = 5)

#random splitting can be reasonably good
pbc3.d <- impute.rfsrc(Surv(days, status) ~ ., data = pbc,
		splitrule = "random", nodesize = 1, nimpute = 5)

## ------------------------------------------------------------
## example of regression imputation
## ------------------------------------------------------------

air.d <- impute.rfsrc(Ozone ~ ., data = airquality, nimpute = 5)
air2.d <- impute.rfsrc(data = airquality, nimpute = 5, nodesize = 1)
air3.d <- impute.rfsrc(Ozone ~ ., data = airquality, nimpute = 5,
		splitrule = "random", nodesize = 1)


