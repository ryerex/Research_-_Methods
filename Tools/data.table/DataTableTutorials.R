###############################################################################
#
# Project: 	Tools
# Script:	DataTableTutorials.R
# Version:	01
# Created:	Mar 20, 2017
# Updated:	
# Author: 	Robert P. Yerex
# Copyright Robert P. Yerex, 2016
#
###############################################################################
library(data.table)
dt <- data.table(mtcars)[, .(cyl, gear)]
dt

dt[,gearsL:=.(list(unique(gear))), by=cyl]
dt