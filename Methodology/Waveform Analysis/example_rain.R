###############################################################################
#
# Project: 	Methodology
# Script:	example_rain.R
# Version:	
# Created:	
# Updated:	Mar 8, 2016
# Author: 	Robert P. Yerex
# Copyright Robert P., Yerex, 2016
###############################################################################

# Simple Example

set.seed(123)
times <- c(1: 24) * 2
sin <- 1 + 0.5 * sin(times / 24 * 2 * pi) + rnorm(24, 0, 0.3)
saw <- rep(13:24 / 18 , 2) + rnorm(24, 0, 0.3)
measure <- cbind(sin, saw)
require('lattice')
x11()
xyplot(t(measure)~rep(times, each=2) | c('sin', 'saw'),
		layout = c(1, 2), type = 'o', xlab = 'time', ylab = 'value', cex.lab = 0.6)


rainresult <- rain(measure, period=24, 
		deltat=2, peak.border=c(0.1,0.9),
		verbose=FALSE)
rainresult


# Mouse RNA Example
data(menetRNASeqMouseLiver)
colnames(menetRNASeqMouseLiver)

results <- rain(t(menetRNASeqMouseLiver), deltat=4, period=24, nr.series=2,
		peak.border=c(0.3, 0.7), verbose=FALSE) 

best <- order(results$pVal)[1:10]
X11()
xyplot(as.matrix(menetRNASeqMouseLiver
								[best, (0:5 * 2 + rep(c(1, 2), each = 6))]) 
				~rep(0:11 * 4 + 2, each = 10) |rownames(menetRNASeqMouseLiver)[best], 
		scales = list(y = list(relation = 'free')),
		layout = c(2, 5), type = 'b', pch = 16, xlab = 'time', 
		ylab = 'expression value', cex.lab = 1)

x11()
plot(NULL, NULL, xlim = c(0, 1),ylim = c(0, 1), bty = 'n', xaxt = "n", 
		yaxt = "n", xlab = '', ylab = '', mar=c(0,0,1,0))
lines(c(0.2, 0.8), c(1.02, 1.02), lwd = 15, col = 'grey85', lend = 1)
lines(c(0, 0.3, 1), c(0, 1, 0), lwd = 2)
axis(3, c(0, 0.3, 1), labels = c('0', '', '1'), cex = 0.6)
text(0.65, 1.2, "c(0.2, 0.8)", col='grey75', xpd=TRUE)

rainresult

