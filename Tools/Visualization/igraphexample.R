###############################################################################
#
# Project: 	ACO Projects
# Script:	igraphexample.R
# Version:	
# Created:	
# Updated:	Mar 18, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(igraph)
g <- graph( c(a,b, c,d, e,f, 3,5, 1,2, 1,2, 1,2, 1,2, 2,4), n=5 )
V(g)
E(g)

