###############################################################################
#
# Project: 	ACO Projects
# Script:	generateSpellSet.R
# Version:	1
# Created:	Jun 04, 2015
# Updated:	Jun 10, 2015
# Author: 	RY5T
# Copyright University of Virginia, 2015
###############################################################################
suppressMessages(require(data.table, quietly = TRUE))
buildSpellSet <- function(patientClaims){
	setkeyv(patientClaims, 'startDT')
	totalClaims <- nrow(patientClaims)
	for(j in 1:totalClaims){
		if(j == 1){
			continueEvent <- 	TRUE
			continueEpisode <-	TRUE
			continueSpell <- 	TRUE
			anchorClaim <- 		FALSE
			claimCount <- 0
			eventCount <- 0
			episodeCount <- 0
			spellCount <- 0
		}
		if( j < totalClaims){
			currentClaim <- patientClaims[j]
			nextClaim <- patientClaims[j+1]
			if((nextClaim$startDT - currentClaim$endDT) <= 3){									# Dates overlap
				if(nextClaim$claimFacilityID == currentClaim$claimFacilityID){					# Facility is the same
					if(nextClaim$claimFacilityService == currentClaim$claimFacilityService){	# Same Department/Service
						if(nextClaim$dschrgCD != '30'){											# Continuation of current Event
							# Start new Event, current Episode, current Spell
							inc(eventCount) <- 1
							continueEvent <- FALSE
							continueEpisode <- TRUE
							continueSpell <- TRUE
						}
					}
					else{																		# Different Department/Service 
						# Start new Event, current Episode, current Spell
						inc(eventCount) <- 1
						continueEvent <- FALSE
						continueEpisode <- TRUE
						continueSpell <- TRUE
					}
				}																				
				else{																			# Different Facility
					# Start new Event, new Episode, current Spell
					inc(eventCount) <- 1
					continueEvent <- FALSE
					inc(episodeCount) <- 1
					continueEpisode <- FALSE
					continueSpell <- TRUE
				}
			}
			else{																				# Dates do not overlap
				# Start new Event, new Episode, new Spell
				inc(eventCount) <- 1
				continueEvent <- FALSE
				inc(episodeCount) <- 1
				continueEpisode <- FALSE
				inc(spellCount) <- 1
				continueSpell <- FALSE
			}	
			if(continueEvent & continueEpisode & continueSpell){
				if(!anchorClaim){
					claimSet <- as.data.table(patientClaims[j])										# Start a claimSet with the anchor claim
					inc(claimCount) <- 1
					anchorClaim <- TRUE
				}
				else{
					claimSet <- rbind(claimSet, currentClaim)										# Add the claim to the current Event
					inc(claimCount) <- 1
				}
			}
			if(!continueEvent & continueEpisode & continueSpell){
				if(!anchorClaim){
					claimSet <- as.data.table(patientClaims[j])										# Start a claimSet with the anchor claim
					inc(claimCount) <- 1
					anchorClaim <- TRUE
				}
				else{
					claimSet <- rbind(claimSet, currentClaim)										# Add the claim to the current Event
					inc(claimCount) <- 1
				}
				if(eventCount == 1){
					eventSet <- data.table(eventID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, eventNumber = '1', 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
				}
				else{
					currentEvent <- data.table(eventID = paste(currentClaim$patientID, as.character(eventCount), sep = '_'),
							patientID = currentClaim$patientID, eventNumber = as.character(eventCount), 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
					eventSet <- rbind(eventSet, currentEvent[1])
				}
				continueEvent <- TRUE
				eventCount <- 0
				anchorClaim <- FALSE
				claimCount <- 0
			}
			else if(!continueEvent & !continueEpisode & continueSpell){
				if(!anchorClaim){
					claimSet <- as.data.table(patientClaims[j])										# Start a claimSet with the anchor claim
					inc(claimCount) <- 1
					anchorClaim <- TRUE
				}
				else{
					claimSet <- rbind(claimSet, currentClaim)										# Add the claim to the current Event
					inc(claimCount) <- 1
				}
				if(eventCount == 1){
					eventSet <- data.table(eventID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, eventNumber = '1', 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
				}
				else{
					currentEvent <- data.table(eventID = paste(currentClaim$patientID, as.character(eventCount), sep = '_'),
							patientID = currentClaim$patientID, eventNumber = as.character(eventCount), 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
					eventSet <- rbind(eventSet, currentEvent[1])
				}
				if(episodeCount == 1){
					episodeSet <- data.table(episodeID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, episodeNumber = '1' , 
							startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
							events = list(eventSet))
				}
				else{
					currentEpisode <- data.table(episodeID = paste(currentClaim$patientID, as.character(episodeCount), sep = '_'),
							patientID = currentClaim$patientID, episodeNumber = as.character(episodeCount), 
							startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
							events = list(eventSet))
					episodeSet <- rbind(episodeSet, currentEpisode[1])
				}
				continueEpisode <- TRUE
				episodeCount <- 0
				continueEvent <- TRUE
				eventCount <- 0
				anchorClaim <- FALSE
				claimCount <- 0
			}
			else if(!continueEvent & !continueEpisode & !continueSpell){
				if(!anchorClaim){
					claimSet <- as.data.table(patientClaims[j])										# Start a claimSet with the anchor claim
					inc(claimCount) <- 1
					anchorClaim <- TRUE
				}
				else{
					claimSet <- rbind(claimSet, currentClaim)										# Add the claim to the current Event
					inc(claimCount) <- 1
				}
				if(eventCount == 1){
					eventSet <- data.table(eventID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, eventNumber = '1', 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
				}
				else{
					currentEvent <- data.table(eventID = paste(currentClaim$patientID, as.character(eventCount), sep = '_'),
							patientID = currentClaim$patientID, eventNumber = as.character(eventCount), 
							startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
							claims = list(claimSet))
					eventSet <- rbind(eventSet, currentEvent[1])
				}
				if(episodeCount == 1){
					episodeSet <- data.table(episodeID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, episodeNumber = '1' , 
							startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
							events = list(eventSet))
				}
				else{
					currentEpisode <- data.table(episodeID = paste(currentClaim$patientID, as.character(epsiodeCount), sep = '_'),
							patientID = currentClaim$patientID, episodeNumber = as.character(episodeCount), 
							startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
							events = list(eventSet))
					episodeSet <- rbind(episodeSet, currentEpisode[1])
				}
				if(spellCount == 1){
					spellSet <- data.table(spellID = paste(currentClaim$patientID, '1', sep = '_'),
							patientID = currentClaim$patientID, spellNumber = '1' , 
							startDT = min(episodeSet$startDT), endDT = max(episodeSet$endDT), 
							episodes = list(episodeSet))
				}
				else{
					currentSpell <- data.table(spellID = paste(currentClaim$patientID, as.character(spellCount), sep = '_'),
							patientID = currentClaim$patientID, spellNumber = as.character(spellCount), 
							startDT = min(episodeSet$startDT), endDT = max(episodeSet$endDT), 
							episodes = list(episodeSet))
					spellSet <- rbind(spellSet, currentSpell[1])
				}
				continueSpell <- TRUE
				continueEpisode <- TRUE
				episodeCount <- 0
				continueEvent <- TRUE
				eventCount <- 0
				anchorClaim <- FALSE
				claimCount <- 0
			}
		}
		else{
			currentClaim <- patientClaims[j]
			if(!anchorClaim){
				claimSet <- as.data.table(patientClaims[j])										# Start a claimSet with the anchor claim
				inc(claimCount) <- 1
				anchorClaim <- TRUE
			}
			else{
				claimSet <- rbind(claimSet, currentClaim)										# Add the claim to the current Event
				inc(claimCount) <- 1
			}
			if(eventCount < 1){
				eventSet <- data.table(eventID = paste(currentClaim$patientID, '1', sep = '_'),
						patientID = currentClaim$patientID, eventNumber = '1', 
						startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
						claims = list(claimSet))
				inc(eventCount) <- 1
			}
			else{
				currentEvent <- data.table(eventID = paste(currentClaim$patientID, as.character(eventCount), sep = '_'),
						patientID = currentClaim$patientID, eventNumber = as.character(eventCount), 
						startDT = min(claimSet$startDT), endDT = max(claimSet$endDT), 
						claims = list(claimSet))
				eventSet <- rbind(eventSet, currentEvent[1])
				inc(eventCount) <- 1
			}
			if(episodeCount < 1){
				episodeSet <- data.table(episodeID = paste(currentClaim$patientID, '1', sep = '_'),
						patientID = currentClaim$patientID, episodeNumber = '1' , 
						startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
						events = list(eventSet))
				inc(episodeCount) <- 1
			}
			else{
				currentEpisode <- data.table(episodeID = paste(currentClaim$patientID, as.character(epsiodeCount), sep = '_'),
						patientID = currentClaim$patientID, episodeNumber = as.character(episodeCount), 
						startDT = min(eventSet$startDT), endDT = max(eventSet$endDT), 
						events = list(eventSet))
				episodeSet <- rbind(episodeSet, currentEpisode[1])
				inc(episodeCount) <- 1
			}
			if(spellCount < 1){
				spellSet <- data.table(spellID = paste(currentClaim$patientID, '1', sep = '_'),
						patientID = currentClaim$patientID, spellNumber = '1' , 
						startDT = min(episodeSet$startDT), endDT = max(episodeSet$endDT), 
						episodes = list(episodeSet))
				inc(spellCount) <- 1
			}
			else{
				currentSpell <- data.table(spellID = paste(currentClaim$patientID, as.character(spellCount), sep = '_'),
						patientID = currentClaim$patientID, spellNumber = as.character(spellCount), 
						startDT = min(episodeSet$startDT), endDT = max(episodeSet$endDT), 
						episodes = list(episodeSet))
				spellSet <- rbind(spellSet, currentSpell[1])
				inc(spellCount) <- 1
			}
			continueSpell <- FALSE
			continueEpisode <- FALSE
			continueEvent <- FALSE
			anchorClaim <- FALSE
		}
	}
	if(spellCount > 0){
		return(spellSet)
	}
}


