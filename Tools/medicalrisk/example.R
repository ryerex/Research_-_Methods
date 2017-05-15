###############################################################################
#
# Project: 	Tools
# Script:	example.R
# Version:	
# Created:	
# Updated:	Mar 1, 2016
# Author: 	Robert P. Yerex
# Copyright Robert P., Yerex, 2016
###############################################################################

library(medicalrisk)
library(knitr)
library(plyr)
data(vt_inp_sample)
x <- count(vt_inp_sample, c('id'))
cat("average count of ICD codes per patient is: ", mean(x$freq))
## average count of ICD codes per patient is:  11.52
y <- count(vt_inp_sample, c('icd9cm'))
library(knitr)
kable(head(y[order(-y$freq),], n=5), row.names=F,
		caption='Top 5 most popular ICD-9-CM codes in this dataset')
