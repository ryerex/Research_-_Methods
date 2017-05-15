###############################################################################
#
# Project: 	ACO Projects
# Script:	gemsExamples.R
# Version:	
# Created:	
# Updated:	Apr 29, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2015
###############################################################################


###################################################
### 3. Using gems
###################################################
set.seed(123)
library("gems")

###################################################
### 3.1 Specifying the model
###################################################
hf <- generateHazardMatrix(3)
print(hf)

hf[[1, 2]] <- function(t, mu) rep(1 / mu, length(t))
hf[[1, 3]] <- "Exponential"
hf[[2, 3]] <- "Weibull"
print(hf)

par <- generateParameterMatrix(hf)
par[[1, 2]] <- list(mu = 3.1)
par[[1, 3]] <- list(rate = 0.3)
par[[2, 3]] <- list(shape = 3, scale = 3)

###################################################
### 3.2 Simulation and post-processing
###################################################
cohortSize <- 10000
cohort <- simulateCohort(transitionFunctions = hf, 
		parameters = par, cohortSize = cohortSize, to = 10)
head(cohort)

post <- transitionProbabilities(cohort, times = seq(0,5, .1))
cinc <- cumulativeIncidence(cohort, times = seq(0,5, .1))
head(post)
head(cinc)

plot(post, main = "Transition probabilities", ci = TRUE)
plot(cinc, main = "Cumulative incidence", ci = TRUE)

###################################################
### 3.3 Parameter uncertainty
###################################################
cov <- generateParameterCovarianceMatrix(par)
cov[[2, 3]] <- diag(.5, 2)

cohortSize <- 10
cohort <- simulateCohort(transitionFunctions = hf, 
		parameters = par, parameterCovariances = cov,
		cohortSize = cohortSize, to = 10)

###################################################
### 3.4 Baseline characteristics
###################################################
bl <- data.frame(sex = rbinom(cohortSize, 1, .5), 
		age = runif(cohortSize, 20, 50))
head(bl)

hf[[1, 2]] <- function(t, bl, rate, rr) {
	rep(rate[bl["sex"] + 1], length(t)) * rr ^ (bl["age"] - 20)
}
par[[1, 2]] <- list(rate = c(0.2,0.3), rr = 1.02)
cov[[1, 2]] <- diag(0, 3)

cohort <- simulateCohort(transitionFunctions = hf, 
		parameters = par, cohortSize = cohortSize, 
		parameterCovariances = cov, baseline = bl, to = 5)

###################################################
### 3.5 History dependence
###################################################
gems:::auxcounter(3)
hf[[2, 3]] <- function(t, shape, scale, history) {
	shape/scale * ((t + sum(history)) / scale) ^ (shape - 1)  
}

###################################################
### 3.6 Time to transition functions
###################################################
hf[[1, 3]] <- function() 3
par[[1, 3]] <- list()
ttt <- matrix(FALSE, nrow = 3, ncol = 3)
ttt[1, 3] <- TRUE
cohort <- simulateCohort(transitionFunctions = hf, 
		parameters = par, cohortSize = cohortSize, 
		parameterCovariances = cov, timeToTransition = ttt,
		baseline = bl, to = 5)

###################################################
### 4 Case study: Transcatheter aortic valve implantation
###################################################
pkgs2load <- c("gems", "mstate", "msm", "muhaz")
invisible(sapply(pkgs2load, require, character.only=TRUE))
set.seed(123)

###################################################
### 4.2 Statistical analysis
###################################################
data("tavi", package = "gems")
head(tavi)

tmat <- transMat(x = list(c(2, 3, 4), c(4), c(4), c()), 
		names = c("TAVI", "Kidney Injury", "Bleeding", "Stroke/Death"))
tmat

mstavi <- msprep(data = tavi, trans = tmat, 
		time = c(NA, "kidney.dur", "bleeding.dur", "death.dur"),
		status = c(NA, "kidney", "bleeding",  "death"))
head(mstavi)
mstavi$time[mstavi$time == 0] <- .Machine$double.eps
msplit <- split(mstavi, mstavi$trans)
head(msplit[[5]])

exp.fit <- sapply(msplit, function(x) 
			summary(survreg(Surv(time, status) ~ 1, 
							data = x, dist = "exponential")))
exp.coef <- unlist(exp.fit["coefficients", ])
exp.var <- unlist(exp.fit["var", ])

states <- 4
maxtime <- max(mstavi$time)
ind <- which(!is.na(tmat), arr.ind = TRUE)

hm <- generateHazardMatrix(states)
for (i in 1:dim(ind)[1]){
	hm[[ind[i, 1], ind[i, 2]]] <- "Weibull"
}

par <- generateParameterMatrix(hm)
for (i in 1:dim(ind)[1]){
	par[[ind[i, 1], ind[i, 2]]] <- list(shape = 1, 
			scale = exp(exp.coef[i]))
}

cov <- generateParameterCovarianceMatrix(par)
for (i in 1:dim(ind)[1]){
	cov[[ind[i, 1], ind[i, 2]]] <- matrix(c(0, 0, 0, exp.var[i]), nrow=2)
}

ds <- simulateCohort(transitionFunctions = hm, 
		parameters = par, cohortSize = 100 * nrow(tavi), 
		parameterCovariances = cov, to = maxtime)
cinc <- cumulativeIncidence(ds, 0:maxtime, colnames(tmat), M = 100)

timeStep <- 30
pwexp <- sapply(msplit, function(x)
			pehaz(x$time, x$status, width = timeStep, 
					min.time = 0, max.time = max(mstavi$time)))
cuts <- pwexp["Cuts", ]
pwhazard <- pwexp["Hazard", ]

hm2 <- generateHazardMatrix(states)
for (i in 1:dim(ind)[1]){
	hm2[[ind[i, 1], ind[i, 2]]] <- function(t, rates) 
		rates[t / timeStep + 1]
}
par2 <- generateParameterMatrix(hm2)
for (i in 1:dim(ind)[1]) {
	par2[[ind[i, 1], ind[i, 2]]] <- list(rates = pwhazard[[i]])
}
ds2 <- simulateCohort(transitionFunctions = hm2, 
		parameters = par2, cohortSize = 100 * nrow(tavi), 
		to = maxtime)
cinc2 <- cumulativeIncidence(ds2, 0:maxtime, colnames(tmat), M = 100)

plot(cinc, states = 4, axes = FALSE, frame = TRUE, col = 2, 
		xlab = "Time (in months)", main = "Mortality", ci = TRUE)
lines(survfit(Surv(death.dur, death) ~ 1, data = tavi), 
		fun = "event", lwd = 2)
lines(survfit(Surv(death.dur, death) ~ 1, data = tavi), 
		fun = "event", lwd = 2, conf.int = TRUE, lty = 2)
par(new = TRUE)
plot(cinc2, states = 4, axes = FALSE, frame = TRUE, col = 3, 
		xlab = "", main = "", ci = TRUE)
axis(2); axis(4)
axis(1, at = (0:13 * 90)[0:6 * 2 + 1], labels = (0:13 * 3)[0:6 * 2 + 1])
legend(200, .8, c("Data", "Simulation: exponential", 
				"Simulation: piecewise exponential"), lty = 1, col = c(1:3), lwd = 2)

###################################################
### 4.3 Interventation modeling
###################################################
hm3 <- hm2
par3 <- par2
par3[[1, 3]]$rates <- par3[[1, 3]]$rates / 5
ds3 <- simulateCohort(transitionFunctions = hm3, 
		parameters = par3, cohortSize = 100 * nrow(tavi), to = maxtime)
cinc3 <- cumulativeIncidence(ds3, 0:maxtime, colnames(tmat), M = 100)

plot(cinc2, states = 4, axes = FALSE, frame = TRUE, col = 1, ci = TRUE, 
		xlab = "Time (in months)", main = "Mortality")
par(new = TRUE)
plot(cinc3, states = 4, axes = FALSE, frame = TRUE, col = 2, ci = TRUE, 
		xlab = "", main = "")
axis(2); axis(4)
axis(1, at = (0:13 * 90)[0:6*2 + 1], labels = (0:13 * 3)[0:6 * 2 + 1])
legend(200, .8, c("No intervention", "Intervention"), 
		lty = 1, col = 1:2, lwd = 2)

xx <- cinc2@probabilities[nrow(cinc2@probabilities), ncol(cinc2@probabilities)]
yy <- cinc3@probabilities[nrow(cinc3@probabilities), ncol(cinc3@probabilities)]
signifs <- 1
c1 <- round(xx * 100, signifs)
c2 <- round(yy * 100, signifs)
c3 <- round((xx - yy) / xx * 100, signifs)
