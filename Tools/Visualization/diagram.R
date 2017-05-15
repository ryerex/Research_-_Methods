###############################################################################
#
# Project: 	ACO Projects
# Script:	diagram.R
# Version:	
# Created:	
# Updated:	Mar 25, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(diagram)

M[2, 1] <- M[3, 1] <- M[4, 2] <- M[4, 3] <- "flow"
plotmat(M, pos = c(1, 2, 1), curve = 0, name = names, lwd = 1,
		box.lwd = 2, cex.txt = 0.8, box.type = "circle", box.prop = 1.0)
#
diag(M) <- "self"
plotmat(M, pos = c(2, 2), curve = 0, name = names, lwd = 1, box.lwd = 2,
		cex.txt = 0.8, self.cex = 0.5, self.shiftx = c(-0.1, 0.1, -0.1, 0.1),
		box.type = "diamond", box.prop = 0.5)
M <- matrix(nrow = 4, ncol = 4, data = 0)
M[2, 1] <- 1 ; 
M[4, 2] <- 2 ; 
M[3, 4] <- 3 ; 
M[1, 3] <- 4

Col <- M
Col[] <- "black"
Col[4, 2] <- "darkred"
pp <- plotmat(M, pos = c(1, 2, 1), curve = 0.2, name = names, lwd = 1,
	 box.lwd = 2, cex.txt = 0.8, arr.type = "triangle",
	 box.size = 0.1, box.type = "hexa", box.prop = 0.25,
	 arr.col = Col, arr.len = 1)
mtext(outer = TRUE, side = 3, line = -1.5, cex = 1.5, "plotmat")
#
par(mfrow = c(1, 1))

# Eample 1 ---------------------------------------------------------------------
Numgenerations <- 6
DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
AA <- as.data.frame(DiffMat)
AA[[1,4]] <- "f[3]"
AA[[1,5]] <- "f[4]"
AA[[1,6]] <- "f[5]"
AA[[2,1]] <- "s[list(0,1)]"
AA[[3,2]] <- "s[list(1,2)]"
AA[[4,3]] <- "s[list(2,3)]"
AA[[5,4]] <- "s[list(3,4)]"
AA[[6,5]] <- "s[list(4,5)]"
name <- c(expression(Age[0]), expression(Age[1]), expression(Age[2]),
		expression(Age[3]), expression(Age[4]), expression(Age[5]))
x11()
plotmat(A = AA, pos = 6, curve = 0.7, name = name, lwd = 2,
			arr.len = 0.6, arr.width = 0.25, my = -0.2,
			box.size = 0.05, arr.type = "triangle", dtext = 0.95,
			main = "Age-structured population model 1")
		
# Eample 2 ---------------------------------------------------------------------
curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
curves[3, 1] <- curves[1, 6] <- -0.35
curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
curves[3, 6] <- 0.35
print(Teasel)
x11()
plotmat(Teasel, pos = c(3, 2, 1), curve = curves,
		name = colnames(Teasel), lwd = 1, box.lwd = 2,
		cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
		arr.length = 0.5, box.type = "circle", box.prop = 1,
		shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
		relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
		self.shifty = 0, main = "Teasel population model")