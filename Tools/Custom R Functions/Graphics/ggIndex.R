###############################################################################
#
# Project: 	UVA Projects
# Script:	ggIndex.R
# Version:	
# Created:	
# Updated:	Aug 6, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################

#Function gg.cindex(obj, colorvec)
#Takes a Cindex object from the pec package and creates a ggplot of the resulting concordance indices plotted vs. time
#Can handle up to 4 different models in one Cindex object

#Define the function: MUST specify the object; you can supply different colors if you so choose.
gg.cindex <- function(obj, colorvec=c("green","blue","orange","purple")){
	
	#Requires ggplot and reshape2
	require(reshape2)
	require(ggplot2)
	
	#Grab the days from the object
	day.vec <- obj$time
	
	#Make a data frame with the days as the first column.
	#Use this for ggplotting 
	gg.df <- data.frame(day.vec)
	
	#Bind the Cindex values from the object to the data frame
	for (i in 1:length(obj$AppCindex)){
		gg.df <- cbind(gg.df,obj$AppCindex[i])
	}
	
	#Using the reshape package, melt the data frame, where the ID is the day.
	#This helps with plotting, and particularly makes it easy to generate a legend.
	gg.melt <- melt(gg.df,id="day.vec")
	
	#Begin the ggplot: ggplot the new data frame, gg.melt
	ggplot(gg.melt)+
			
			#Draw a line for each of the values. 
			#Set the size = 1.15 so it's a little larger than normal.
			#Color it based on the variable (which is a new column in the gg.melt data frame.)
			geom_line(aes(x=day.vec,y=value,colour=variable),size=1.15)+
			
			#Give it the colors from the color vector. 
			#If you change this in the function call, it'll provide different colors.
			scale_colour_manual(values=colorvec[1:length(obj$AppCindex)])+
			
			#Provide x and y labels
			xlab(label="Days after discharge")+
			ylab(label="Concordance index")+
			
			#Add a red dashed line at y = 0.5. Set the size to be 1.03 so it's noticeable.
			geom_hline(aes(yintercept=0.50),linetype="dashed",color="red",size=1.03)+
			
			#Add a title
			ggtitle(label="Concordance index vs. Days after discharge")+
			
			#Scale the y and x axes accordingly.
			#Y: goes from 0.3 to 1.0, with breaks every 0.1
			scale_y_continuous(breaks=seq(.3,1.0,.1),limit=c(0.3,1.0)) + 
			#X: goes from 0 to the total # of days in 10 day increments
			scale_x_continuous(breaks=seq(0,length(day.vec),10),limit=c(0,length(day.vec)))+
			
			#Change the font size for the plot title
			theme(plot.title=element_text(size=22))+
			
			#Change the font size for the plot title
			theme(axis.title=element_text(size=16))+
			
			#Change the font size for the axis text (these are the numeric labels: 10-60 and .3-1.0)
			theme(axis.text=element_text(size=14))
}

