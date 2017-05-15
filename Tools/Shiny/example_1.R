###############################################################################
#
# Project: 	UVA Projects
# Script:	server.R
# Version:	
# Created:	
# Updated:	Dec 3, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(shiny)

exitValue <- runApp(list(
				ui = basicPage(
						h2('Cars'),
						dataTableOutput('carsTable'),
						actionButton("exitBtn", "Exit")
				),
				server = function(input, output) {
					output$carsTable = renderDataTable({
								mtcars
							})
					observe({
								if(input$exitBtn > 0){
									stopApp(7)
								}
							})
				}
		))
