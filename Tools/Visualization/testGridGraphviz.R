###############################################################################
#
# Project: 	UVA Projects
# Script:	testGridGraphviz.R
# Version:	
# Created:	
# Updated:	Aug 4, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(gridGraphviz)
gnel <- new("graphNEL",
		nodes=letters[1:3],
		edgeL=list(a=list(edges=c("b", "c")),
				b=list(),
				c=list()),
		edgemode="directed")
rag <- agopenTrue(gnel, "")
grid.graph(rag)
plot(rag)