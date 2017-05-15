###############################################################################
#
# Project: 	ACO Projects
# Script:	jsonExamples.R
# Version:	
# Created:	
# Updated:	May 20, 2015
# Author: 	ry5t
#####################v##########################################################


library(jsonlite)
library(data.table)

options(stringsAsFactors=FALSE)
x <- data.table(patientID = c('p1','p2','p3'))
claim1 <- data.table(patientID = 'p1', claimID = 'c_01', startDT = as.Date('2013-01-01'), endDT = as.Date('2013-02-01'))
claim2 <- data.table(patientID = 'p1', claimID = 'c_02', startDT = as.Date('2013-02-02'), endDT = as.Date('2013-02-04'))
claim3 <- data.table(patientID = 'p2', claimID = 'c_03', startDT = as.Date('2013-03-11'), endDT = as.Date('2013-03-21'))
claim4 <- data.table(patientID = 'p3', claimID = 'c_04', startDT = as.Date('2013-04-21'), endDT = as.Date('2013-04-15'))
claim5 <- data.table(patientID = 'p3', claimID = 'c_05', startDT = as.Date('2013-05-21'), endDT = as.Date('2013-05-12'))
claim6 <- data.table(patientID = 'p3', claimID = 'c_06', startDT = as.Date('2013-06-21'), endDT = as.Date('2013-06-01'))
claim7 <- data.table(patientID = 'p3', claimID = 'c_07', startDT = as.Date('2013-07-31'), endDT = as.Date('2013-07-01'))
claim8 <- data.table(patientID = 'p3', claimID = 'c_08', startDT = as.Date('2013-08-21'), endDT = as.Date('2013-08-01'))
claimsDT <- claim1
claimsDT <- rbind(claimsDT, claim2)
claimsDT <- rbind(claimsDT, claim3)
claimsDT <- rbind(claimsDT, claim4)
claimsDT <- rbind(claimsDT, claim5)
claimsDT <- rbind(claimsDT, claim6)
claimsDT <- rbind(claimsDT, claim7)
claimsDT <- rbind(claimsDT, claim8)
#
##	Method 1
#x1 <- x
#claimset_1 <- list(claim1, claim2)
#claimset_2 <- list(claim3)
#claimset_3 <- list(claim4,claim5,claim6,claim7)
#claimList[1] <- claimset_1
#claimList[2] <- claimset_2
#claimList[3] <- claimset_3
#claimList <- list(claimset_1, claimset_2, claimset_3)
#x1$claims <- list(claimset_1, claimset_2, claimset_3)
#
##	Method 2
#x2 <- x
#claimset_1 <- list(claimsDT[patientID == 'p1'])
#claimset_2 <- list(claimsDT[patientID == 'p2'])
#claimset_3 <- list(claimsDT[patientID == 'p3'])
#for(i in 1:3){
#	if(i == 1){
#		claimList[i] <- claimset_1
#	}
#	if(i == 2){
#		claimList[i] <- claimset_2
#	}
#	if(i == 3){
#		claimList[i] <- claimset_3
#	}
#}
#x2$claims <- claimList
#
##	Method 3
#x3 <- x
#claimList <- list(claimsDT[patientID == 'p1'])
#claimList[2] <- list(claimsDT[patientID == 'p2'])
#claimList[3] <- list(claimsDT[patientID == 'p3'])
#x3$claims <- claimList
#

#	Method 4
x4 <- x
patientList <- data.table(patientID = c('p1','p2','p3'))
claimList <- list(claimsDT[patientID == patientList$patientID[1]])
for(i in 2:3){
	claimList[i] <- list(claimsDT[patientID == patientList$patientID[i]])
}
x4$claims <- claimList
x4json <- toJSON(x4)
x4flat <- as.data.table(flatten(fromJSON(x4json)))

#cat(' -------------------- X1 ------------------------\n')
#cat(prettify(toJSON(x1)))
#cat(' -------------------- X2 ------------------------\n')
#cat(prettify(toJSON(x2)))
#cat(' -------------------- X3 ------------------------\n')
#cat(prettify(toJSON(x3)))
cat(' -------------------- X4 ------------------------\n')
cat(prettify(x4json))
#str(flatten(x))
#str(flatten(x, recursive = FALSE))

