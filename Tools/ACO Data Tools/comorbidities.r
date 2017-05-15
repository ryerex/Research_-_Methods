###############################################################################
#
# Project: 	ACO Projects
# Script:	comorbidities.r.R
# Version:	
# Created:	
# Updated:	Jun 24, 2015
# Author: 	ry5t
###############################################################################

#######################
##Deyo-Charlson index##
#######################

#Combining the above functions, deyo takes a 5 digit ICD-9-CM code and produces a list of 3 items:
#1. the total Charlson Score 
#2. a binary data frame of which comorbidites patients have - 1 for having it and 0 if not
#3. The point value data frame of comorbidites - 1-6 for having it, and 0 for not
deyo <- function(input.frame) {
	# Convert icd9 codes to numeric values and convert V434X to 44390 
	apply.icd9.deyo <- function(input.frame) {
		ICD9.5digit.deyo <- function(icd.code){
			if (is.na(icd.code)) {icd.code <- "00000"}
			icd9.3 <- substr(icd.code, 1, 3)
			icd9.4 <- substr(icd.code, 4, 4)
			icd9.5 <- substr(icd.code, 5, 5)
			if (icd9.4 == "X") {icd9.4 <- 0}
			if (icd9.5 == "X") {icd9.5 <- 0}
			icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
			if (icd9.result == "V4340") {icd9.result <- 44390}
			return(as.numeric(icd9.result))
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- ICD9.5digit.deyo(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	# Deal with NA values
	
	apply.convert.na <- function(input.frame) {
		convert.na <- function(input.val) {
			if (is.na(input.val)) {input.val <- 0}
			output.val <- input.val
			return(output.val)
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- convert.na(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	# The following function develops a matrix with rows devoted to respondents and each 
	# column a comorbidity.  The number in each column is the point value of having the comorbidity
	comorbidities.deyo <- function(input.frame) {
		#create lists of comorbidities
		mi <- c(seq(from=41000, to=41090, by=1), 41200)
		chf <- c(seq(from=42800, to=42890, by=1))
		pvd <- c(44390, 44100, 44190, 78540) #v code v43.4 not included in this list
		cvd <- c(seq(from=43000, to=43800, by=1))
		dementia <- c(seq(from=29000, to=29090, by=1))
		copd <- c(seq(from=49000, to=49600, by=1), seq(from=50000, to=50500, by=1), 50640)
		rheum <- c(71000, 71010, 71040, seq(from =71400, to=71420, by=1), 71481, 72500)
		pud <- c(seq(from=53100, to=53490, by=1))
		mild.liver <- c(57120, 57150, 57160, seq(from=57140, to=57149, by=1))
		dm <- c(seq(from=25000,to=25030,by=1), 25070)
		dm.comp <- c(seq(from=25040, to=25060, by=1)) #2 point items start here
		plegia <- c(34410, seq(from=34200, to=34290, by=1))
		renal <- c(seq(from=58200, to=58290, by=1), seq(from=58300, to=58370, by=1), 58500, 58600,seq(from=58800, to=58890, by=1))
		malignancy <- c(seq(from=14000, to=17290, by=1), seq(from=17400, to=19580, by=1), seq(from=20000, to=20890, by=1))
		severe.liver <- c(seq(from=57220, to=57280, by=1),seq(from=45600, to=45621, by=1)) # 3 point item
		mets <- c(seq(from=19600, to=19910, by=1)) # 6 point items
		hiv <- c(seq(from=4200, to=4493, by=1))
		
		deyo.list <- list(mi,chf,pvd,cvd,dementia,copd,rheum,pud,mild.liver,dm,dm.comp,plegia,renal,malignancy,severe.liver,mets,hiv)
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=17)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				for (k in 1:length(deyo.list)) {
					if(input.frame[i, j] %in% deyo.list[[k]]){
						output.frame[i,k] <- 1
					}
				}
			}
		}
		
		output.frame <- as.data.frame(output.frame)
		colnames(output.frame) <- c("MI","CHF","PVD","CVD","DEMENTIA","COPD","RHEUM","PUD","MILD.LIVER","DM","DM.COMP","PLEGIA","RENAL","MALIGNANCY","SEVERE.LIVER", "METS", "HIV")
		return(output.frame)
	}
	
	# Convert the frame of point values to a frame of 0 for not having and 1 for having
	convert.to.points <- function(input.frame) {
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- input.frame
		output.frame[,11] <- output.frame[,11] *2
		output.frame[,12] <- output.frame[,12] *2
		output.frame[,13] <- output.frame[,13] *2
		output.frame[,14] <- output.frame[,14] *2
		output.frame[,15] <- output.frame[,15] *3
		output.frame[,16] <- output.frame[,17] *6
		output.frame[,16] <- output.frame[,17] *6
		return(output.frame)
	}
	
	#The following function sums the points in the comorbidites matrix produced above
	total.points <- function (input.frame) {
		n.rows <- length(input.frame[,1])
		output.vector <- matrix(0, nrow=n.rows, ncol=1)
		for (i in 1:n.rows) {
			output.vector[i] <- sum(input.frame[i,])
		}
		return(output.vector)
	}
	
	
	
	interim.frame.1 <- apply.icd9.deyo(input.frame)
	interim.frame.2 <- apply.convert.na(interim.frame.1)
	interim.frame.3 <- comorbidities.deyo(interim.frame.2)
	interim.frame.4 <- convert.to.points(interim.frame.3)
	POINTS <- total.points(interim.frame.4)
	deyo.data <- list(POINTS, interim.frame.3,interim.frame.4)
	names(deyo.data) <- c("CHARLSON.SCORE", "COMORBIDITIES", "COMORBIDITIES.POINTS")
	return(deyo.data)
}


#############################
##Original elixhauser index##
#############################

# elixhauser() takes a 5 digit ICD-9-CM code and produces a list of 2 items:
#1. the total count of elixhauser comorbidities 
#2. a binary data frame of which comorbidites patients have - 1 for having it and 0 if not
elixhauser <- function(input.frame) {
	# Convert icd9 codes to numeric values and convert v codes
	apply.icd9.elixhauser <- function(input.frame) { 
		ICD9.5digit.elixhauser <- function(icd.code){ 
			process.v.codes <- function(v.code) {
				icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
				if (icd9.2.5 == 4500) {v.code <- 42610}
				if (icd9.2.5 == 5330) {v.code <- 42610}
				if (icd9.2.5 == 4220) {v.code <- 09320}
				if (icd9.2.5 == 4330) {v.code <- 09320}
				if (icd9.2.5 == 4340) {v.code <- 44000}
				if (icd9.2.5 == 4200) {v.code <- 40311}
				if (icd9.2.5 == 4510) {v.code <- 40311}
				if (icd9.2.5 == 5600) {v.code <- 40311}
				if (icd9.2.5 == 5680) {v.code <- 40311}
				if (icd9.2.5 == 4270) {v.code <- 07032}
				if (icd9.2.5 == 1271) {v.code <- 53170}
				if ((icd9.2.5 >= 1000) & (icd9.2.5 <= 1090)) {v.code <- 14000}
				if (icd9.2.5 == 1071) {v.code <- 20000}
				if (icd9.2.5 == 1072) {v.code <- 20000}
				if (icd9.2.5 == 1079) {v.code <- 20000}
				if (icd9.2.5 == 1130) {v.code <- 29110}
				
				return (v.code)
			}
			
			if (is.na(icd.code)) {icd.code <- "00000"}
			icd9.1 <- substr(icd.code, 1, 1)
			icd9.3 <- substr(icd.code, 1, 3)
			icd9.4 <- substr(icd.code, 4, 4)
			icd9.5 <- substr(icd.code, 5, 5)
			if (icd9.4 == "X") {icd9.4 <- 0}
			if (icd9.5 == "X") {icd9.5 <- 0}
			icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
			if (icd9.1 == "V") {icd9.result <- process.v.codes(icd9.result)}
			
			return(as.numeric(icd9.result))
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- ICD9.5digit.elixhauser(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	apply.convert.na <- function(input.frame) {
		convert.na <- function(input.val) {
			if (is.na(input.val)) {input.val <- 0}
			output.val <- input.val
			return(output.val)
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- convert.na(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	# The following function develops a matrix with rows devoted to respondents and each 
	# column a comorbidity.
	points.elixhauser.30 <- function(input.frame) {
		#create lists of comorbidities
		chf <- c(39891,40211,40291,40411,40413,40491,40493,seq(from=42800, to=42890, by=1))
		arrhythmia <- c(42610,42611,42613,seq(from=42620, to=42653, by=1),seq(from=42660, to=42689, by=1),42700,42720,42731,42760,42790,78500)
		valve <- c(seq(from=9320, to=9324, by=1),seq(from=39400, to=39710, by=1),seq(from=42400, to=42491, by=1),seq(from=74630, to=74660, by=1)) 
		pulm.circ <- c(seq(from=41600, to=41690, by=1), 41790) 
		pvd <- c(seq(from=44000, to=44090, by=1),44120,44140,44170,44190,seq(from=44310, to=44390, by=1),44710,55710,55790)
		htn <- c(40110,40190,40210,40290,40410,40490,40511,40519,40591,40599) 
		paralysis <- c(seq(from =34200, to=34212, by=1), seq(from=34290, to=34490, by=1)) 
		neuro.other <- c(33190,33200,33340,33350,seq(from=33400, to=33590, by=1),34000,seq(from=34110, to=34190, by=1),seq(from=34500, to=34511, by=1),seq(from=34540, to=34551, by=1),seq(from=34580, to=34591, by=1),34810,34830,78030,78430)
		chronic.pulm <- c(seq(from=49000, to=49280, by=1), seq(from=49300, to=49391, by=1),49400,seq(from=49500, to=50500, by=1),50640)
		dm.uncomp <- c(seq(from=25000,to=25033,by=1))
		dm.comp <- c(seq(from=25040, to=25073, by=1),seq(from=25090, to=25093, by=1))
		hypothyroid <- c(seq(from=24300, to=24420, by=1),24480,24490)
		renal <- c(40311,40391,40412,40492,58500,58600)
		liver <- c(7032,7033,7054,45600,45610,45620,45621,57100,57120,57130,seq(from=57140, to=57149, by=1),57150,57160,57180,57190,57230,57280)
		pud <- c(53170,53190,53270,53290,53370,53390,53470,53490) 
		hiv <- c(seq(from=4200, to=4490, by=1)) 
		lymphoma <- c(seq(from=20000,to=20238, by=1),seq(from=20250,to=20301, by=1),seq(from=20380,to=20381, by=1),23860,27330)
		mets <- c(seq(from=19600,to=19910, by=1))
		solid.tumor <- c(seq(from=14000,to=17290, by=1),seq(from=17400,to=17590, by=1),seq(from=17900,to=19580, by=1))
		rheum <- c(70100,seq(from=71000,to=71090, by=1),seq(from=71400,to=71490, by=1),seq(from=72000,to=72090, by=1),72500)
		coag <- c(seq(from=28600,to=28690, by=1),28710,seq(from=28730,to=28750, by=1))
		obesity <- c(27800)
		wt.loss <- c(seq(from=26000,to=26390, by=1))
		lytes <- c(seq(from=27600,to=27690, by=1))
		anemia.loss <- c(28000)
		anemia.def <- c(seq(from=28010,to=28190, by=1),28590)
		etoh <- c(29110,29120,29150,29180,29190,seq(from=30390,to=30393, by=1),seq(from=30500,to=30503, by=1))
		drugs <- c(29200,seq(from=29282,to=29289, by=1),29290,seq(from=30400,to=30493, by=1),seq(from=30520,to=30593, by=1))
		psychoses <- c(seq(from=29500,to=29890, by=1),seq(from=29910,to=29911, by=1))
		depression <- c(30040,30112,30900,30910,31100)
		
		elixhauser.list <- list(chf,arrhythmia,valve,pulm.circ,pvd,htn,paralysis,neuro.other,chronic.pulm,dm.uncomp,dm.comp,hypothyroid,renal,liver,pud,hiv,lymphoma,mets,solid.tumor,rheum,coag,obesity,wt.loss,lytes,anemia.loss,anemia.def,etoh,drugs,psychoses,depression)
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=30)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				for (k in 1:length(elixhauser.list)){
					if (input.frame[i, j] %in% elixhauser.list[[k]]) {
						output.frame[i,k] <- 1
					}
				}
			}
		}
		
		#Apply the elixhauser hierarchy
		for (i in 1:length(output.frame[,1])){
			if (output.frame[i,11]==1) {output.frame[i,10] <- 0}
			if (output.frame[i,18]==1) {output.frame[i,19] <- 0}
		}
		
		output.frame <- as.data.frame(output.frame)
		colnames(output.frame) <- c("CHF","ARRHTHMIA","VALVE","PULM.CIRC","PVD","HTN","PARALYSIS","NEURO.OTHER","CHRONIC.PULM","DM.UNCOMP","DM.COMP","HYPOTHYROID","RENAL","LIVER","PUD","HIV","LYMPHOMA","METS","SOLID.TUMOR","RHEUM","COAG","OBESITY","WT.LOSS","LYTES","ANEMIA.LOSS","ANEMIA.DEF","ETOH","DRUGS","PSYCHOSES","DEPRESSION" )
		return(output.frame)
	}
	
	
#The following function sums the points in the comorbidites matrix produced above
	total.points <- function (input.frame) {
		n.rows <- length(input.frame[,1])
		output.vector <- matrix(0, nrow=n.rows, ncol=1)
		for (i in 1:n.rows) {
			output.vector[i] <- sum(input.frame[i,])
		}
		return(output.vector)
	}
	
	
	
	interim.frame.1 <- apply.icd9.elixhauser(input.frame)
	interim.frame.2 <- apply.convert.na(interim.frame.1)
	interim.frame.3 <- points.elixhauser.30(interim.frame.2)
	POINTS <- total.points(interim.frame.3)
	elixhauser.data <- list(POINTS, interim.frame.3)
	names(elixhauser.data) <- c("COMORBIDITY.CT", "COMORBIDITIES")
	return(elixhauser.data)
}


###################################################
##AHRQ comorbidites v3.6 index (newer elixhauser)##
###################################################

# ahrq_v3.6() takes a 5 digit ICD-9-CM code and produces a list of 2 items:
#1. the total count of elixhauser comorbidities 
#2. a binary data frame of which comorbidites patients have - 1 for having it and 0 if not
ahrq <- function(input.frame) {
	# Convert icd9 codes to numeric values and convert v codes
	apply.icd9.ahrq <- function(input.frame) { 
		ICD9.5digit.ahrq <- function(icd.code){ 
			process.v.codes <- function(v.code) {
				icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
				#Valvular disease
				if (icd9.2.5 == 4220) {v.code <- 09320} 
				if (icd9.2.5 == 4330) {v.code <- 09320}
				#PVD
				if (icd9.2.5 == 4340) {v.code <- 44000} 
				#Renal Failure
				if (icd9.2.5 == 4200) {v.code <- 58530} 
				if (icd9.2.5 == 4510) {v.code <- 58530} 
				if ((icd9.2.5 >= 5600) & (icd9.2.5 <= 5632)) {v.code <- 58530}  
				if (icd9.2.5 == 5680) {v.code <- 58530} 
				if (icd9.2.5 == 4511) {v.code <- 58530}  
				if (icd9.2.5 == 4512) {v.code <- 58530}  
				#Liver Diseae
				if (icd9.2.5 == 4270) {v.code <- 07022}
				#Obsesity
				if ((icd9.2.5 >= 8530) & (icd9.2.5 <= 8539)) {v.code <- 02780}
				if ((icd9.2.5 >= 8541) & (icd9.2.5 <= 8545)) {v.code <- 02780}				
				if (icd9.2.5 == 8554) {v.code <- 02780}
				
				return (v.code)
			}
			
			if (is.na(icd.code)) {icd.code <- "00000"}
			icd9.1 <- substr(icd.code, 1, 1)
			icd9.3 <- substr(icd.code, 1, 3)
			icd9.4 <- substr(icd.code, 4, 4)
			icd9.5 <- substr(icd.code, 5, 5)
			if (icd9.4 == "X") {icd9.4 <- 0}
			if (icd9.5 == "X") {icd9.5 <- 0}
			icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
			if (icd9.1 == "V") {icd9.result <- process.v.codes(icd9.result)}
			
			return(as.numeric(icd9.result))
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- ICD9.5digit.ahrq(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	apply.convert.na <- function(input.frame) {
		convert.na <- function(input.val) {
			if (is.na(input.val)) {input.val <- 0}
			output.val <- input.val
			return(output.val)
		}
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				output.frame[i,j] <- convert.na(input.frame[i,j])
			}
		}
		return(output.frame)
	}
	
	# The following function develops a matrix with rows devoted to respondents and each 
	# column a comorbidity.
	points.ahrq <- function(input.frame) {
		#create lists of comorbidities
		chf <- c(39891,seq(from=42800, to=42890, by=1), 40201,40211,40291, 40401,40411,40491, 40403,40413,40493) 
		valve <- c(seq(from=9320, to=9324, by=1),seq(from=39400, to=39710, by=1),39790,seq(from=42400, to=42499, by=1),seq(from=74630, to=74660, by=1)) 
		pulm.circ <- c(seq(from =41511, to=41519, by=1),seq(from=41600, to=41690, by=1), 41790) 
		pvd <- c(seq(from=44000, to=44090, by=1),seq(from=44000, to=44190, by=1),seq(from =44200, to=44290, by=1),seq(from =44310, to=44390, by=1),44421,44122,44710,44900,55710,55790)
		htn <- c(40110,40190,seq(from =64200, to=64204, by=1),40100,43720,seq(from =64220, to=64224, by=1),40200,40210,40290,40509,40519,40599, 40201,40211,40291, 40300,40310,40390,40501,40511,40591,seq(from=64210, to=64214, by=1),40301,40311,40391, 40400,40410,40490, 40401,40411,40491, 40402,40412,40492, 40403,40413,40493, seq(from =64270, to=64274, by=1),seq(from =64290, to=64294, by=1))  
		paralysis <- c(seq(from =34200, to=34490, by=1), seq(from=43820, to=43853, by=1),78072) 
		neuro.other <- c(seq(from=33000, to=33190, by=1),33200,33340,33350,33370,33371,33372,33379,33385,33394,seq(from=33400, to=33590, by=1),33800,34000,seq(from=34110, to=34190, by=1),seq(from=34500, to=34511, by=1),seq(from =34520, to=34530, by=1),seq(from=34540, to=34591, by=1),34700,34701,34710,34711,seq(from =64940, to=64944, by=1),78670,seq(from =78670, to=78673, by=1),78030,78031,78032,78039,78097,78430)
		chronic.pulm <- c(seq(from=49000, to=49280, by=1), seq(from=49300, to=49392, by=1),seq(from =49400, to=49410, by=1),seq(from=49500, to=50500, by=1),50640)
		dm.uncomp <- c(seq(from=25000,to=25033,by=1),seq(from=64800, to=64804, by=1),seq(from=24900, to=24931, by=1))
		dm.comp <- c(seq(from=25040, to=25093, by=1),77510,seq(from=24940, to=24991, by=1))
		hypothyroid <- c(seq(from=24300, to=24420, by=1),24480,24490)
		renal <- c(58530,58540,58550,58560,58590, 40301,40311,40391,40402,40412,40492, 40403,40413,40493) 
		liver <- c(7022,7023,7032,7033,7044,7054,45600,45610,45620,45621,57100,57120,57130,seq(from=57140, to=57149, by=1),57150,57160,57180,57190,57230,57280)
		pud <- c(53141,53151,53161,53170,53171,53191,53241,53251,53261,53270,53271,53291,53341,53351,53361,53370,53371,53391,53441,53451,53461,53470,53471,53491) 
		hiv <- c(seq(from=4200, to=4490, by=1)) 
		lymphoma <- c(seq(from=20000,to=20238, by=1),seq(from=20250,to=20301, by=1),23860,27330,seq(from=20302,to=20382, by=1))
		mets <- c(seq(from=19600,to=19910, by=1),seq(from=20970,to=20975, by=1),20979,78951)
		solid.tumor <- c(seq(from=14000,to=17290, by=1),seq(from=17400,to=17590, by=1),seq(from=17900,to=19580, by=1),seq(from=20900,to=20924, by=1),seq(from=20925,to=20930, by=1),seq(from=20931,to=20936, by=1),seq(from=25801,to=25803, by=1)  )
		rheum <- c(70100,seq(from=71000,to=71090, by=1),seq(from=71400,to=71490, by=1),seq(from=72000,to=72090, by=1),72500) 
		coag <- c(seq(from=28600,to=28690, by=1),28710,seq(from=28730,to=28750, by=1),seq(from=64930,to=64934, by=1),28984)
		obesity <- c(27800,27801,27803,seq(from=64910,to=64914, by=1),79391) 
		wt.loss <- c(seq(from=26000,to=26390, by=1),78321,78322)
		lytes <- c(seq(from=27600,to=27690, by=1))
		anemia.loss <- c(28000,seq(from=64820,to=64824, by=1))
		anemia.def <- c(seq(from=28010,to=28190, by=1),seq(from=28521,to=28529, by=1),28590) 
		etoh <- c(seq(from=29100,to=29130, by=1),29150,29180,29181,29182,29189,29190,seq(from=30300,to=30393, by=1),seq(from=30500,to=30503, by=1))
		drugs <- c(29200,seq(from=29282,to=29289, by=1),29290,seq(from=30400,to=30493, by=1),seq(from=30520,to=30593, by=1),seq(from=64830,to=64834, by=1))
		psychoses <- c(seq(from=29500,to=29890, by=1),29910,29911)
		depression <- c(30040,30112,30900,30910,31100)
		
		ahrq.list <- list(chf,valve,pulm.circ,pvd,htn,paralysis,neuro.other,chronic.pulm,dm.uncomp,dm.comp,hypothyroid,renal,liver,pud,hiv,lymphoma,mets,solid.tumor,rheum,coag,obesity,wt.loss,lytes,anemia.loss,anemia.def,etoh,drugs,psychoses,depression)
		
		n.rows <- length(input.frame[,1])
		n.cols <- length(input.frame[1,])
		output.frame <- matrix(0, nrow=n.rows, ncol=29)
		for (i in 1:n.rows){
			for (j in 1:n.cols) {
				for (k in 1:length(ahrq.list)){
					if (input.frame[i, j] %in% ahrq.list[[k]]) {
						output.frame[i,k] <- 1
					}
				}
			}
		}
		
		#Apply the elixhauser hierarchy
		for (i in 1:length(output.frame[,1])){
			if (output.frame[i,10]==1) {output.frame[i,9] <- 0}
			if (output.frame[i,17]==1) {output.frame[i,18] <- 0}
		}
		
		output.frame <- as.data.frame(output.frame)
		colnames(output.frame) <- c("CHF","VALVE","PULM.CIRC","PVD","HTN","PARALYSIS","NEURO.OTHER","CHRONIC.PULM","DM.UNCOMP","DM.COMP","HYPOTHYROID","RENAL","LIVER","PUD","HIV","LYMPHOMA","METS","SOLID.TUMOR","RHEUM","COAG","OBESITY","WT.LOSS","LYTES","ANEMIA.LOSS","ANEMIA.DEF","ETOH","DRUGS","PSYCHOSES","DEPRESSION" )
		return(output.frame)
	}
	
	#The following function sums the points in the comorbidites matrix produced above
	total.points <- function (input.frame) {
		n.rows <- length(input.frame[,1])
		output.vector <- matrix(0, nrow=n.rows, ncol=1)
		for (i in 1:n.rows) {
			output.vector[i] <- sum(input.frame[i,])
		}
		return(output.vector)
	}
	
	
	
	interim.frame.1 <- apply.icd9.ahrq(input.frame)
	interim.frame.2 <- apply.convert.na(interim.frame.1)
	interim.frame.3 <- points.ahrq(interim.frame.2)
	POINTS <- total.points(interim.frame.3)
	elixhauser.data <- list(POINTS, interim.frame.3)
	names(elixhauser.data) <- c("COMORBIDITY.CT", "COMORBIDITIES")
	return(elixhauser.data)
}
