###############################################################################
#
# Project: 	ACO Projects
# Script:	TestCI.R
# Version:	
# Created:	
# Updated:	Mar 16, 2015
# Author: 	ry5t
###############################################################################


# simulate data based on Weibull regression
library(prodlim)
library(pec)
library(Hmisc)
library(riskRegression)

set.seed(13)
dat <- SimSurv(100)
# fit three different Cox models and a random survival forest
# note: low number of trees for the purpose of illustration
library(survival)
library(randomForestSRC)
cox12 <- coxph(Surv(time,status)~X1+X2,data=dat)
cox1 <- coxph(Surv(time,status)~X1,data=dat)
cox2 <- coxph(Surv(time,status)~X2,data=dat)
rsf1 <- rfsrc(Surv(time,status)~X1+X2,data=dat,ntree=15,forest=TRUE)
#
# compute the apparent estimate of the C-index at different time points
#
ApparrentCindex  <- cindex(list("Cox X1"=cox1,
				"Cox X2"=cox2,
				"Cox X1+X2"=cox12,
				"RSF"=rsf1),
		formula=Surv(time,status)~X1+X2,
		data=dat,
		eval.times=seq(5,50))
print(ApparrentCindex)
x11()
plot(ApparrentCindex)
#
# compute the bootstrap-crossvalidation estimate of
# the C-index at different time points
#
set.seed(142)
bcvCindex  <- cindex(list("Cox X1"=cox1,
				"Cox X2"=cox2,
				"Cox X1+X2"=cox12,
				"RSF"=rsf1),
		formula=Surv(time,status)~X1+X2,
		data=dat,
		splitMethod="bootcv",
		B=5,
		eval.times=seq(5,50))
print(bcvCindex)
x11()
plot(bcvCindex)
# for uncensored data the results are the same
# as those obtained with the function rcorr.cens from Hmisc

set.seed(16)
dat <- SimSurv(30,cens=FALSE)
fit12 <- coxph(Surv(time,status)~X1+X2,data=dat)
fit1 <- coxph(Surv(time,status)~X1,data=dat)
fit2 <- coxph(Surv(time,status)~X2,data=dat)
Cpec <- cindex(list("Cox X1+X2"=fit12,"Cox X1"=fit1,"Cox X2"=fit2),
		formula=Surv(time,status)~1,
		data=dat,
		eval.times=Inf)
p1 <- predictSurvProb(fit1,newdata=dat,times=100)
p2 <- predictSurvProb(fit2,newdata=dat,times=100)
p12 <- predictSurvProb(fit12,newdata=dat,times=100)
harrelC1 <- rcorr.cens(p1,with(dat,Surv(time,status)))
harrelC2 <- rcorr.cens(p2,with(dat,Surv(time,status)))
harrelC12 <- rcorr.cens(p12,with(dat,Surv(time,status)))
harrelC1[["C Index"]]==Cpec$AppCindex[["Cox.X1"]]
harrelC2[["C Index"]]==Cpec$AppCindex[["Cox.X2"]]
harrelC12[["C Index"]]==Cpec$AppCindex[["Cox.X1.X2"]]
#
# competing risks
#

set.seed(30)
dcr.learn <- SimCompRisk(30)
dcr.val <- SimCompRisk(30)
cindex(CSC(Hist(time,event)~X1+X2,data=dcr.learn),data=dcr.val)
