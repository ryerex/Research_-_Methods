###############################################################################
#
# Project: 	UVA Projects
# Script:	Inroduction_1.R
# Version:	
# Created:	
# Updated:	Oct 29, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################

library(R6)

Person <- R6Class("Person",
		public = list(
				name = NA,
				hair = NA,
				initialize = function(name, hair) {
					if (!missing(name)) self$name <- name
					if (!missing(hair)) self$hair <- hair
					self$greet()
				},
				set_hair = function(val) {
					self$hair <- val
				},
				greet = function() {
					cat(paste0("Hello, my name is ", self$name, ".\n"))
				}
		)
)
robert <- Person$new("Robert", "Yerex")

Numbers <- R6Class("Numbers",
		public = list(
				x = 100
		),
		active = list(
				x2 = function(value) {
					if (missing(value)) return(self$x * 2)
					else self$x <- value/2
				},
				rand = function() rnorm(1)
		)
)

a <- Numbers$new()