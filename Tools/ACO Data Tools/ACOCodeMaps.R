###############################################################################
#
# Project: 	ACO Projects
# Script:	ACOCodeMaps.R
# Version:	
# Created:	Apr 6, 2015
# Updated:	Apr 9, 2015
# Author: 	ry5t
###############################################################################
library(hash)
claimTypeCDMap <- hash(c('10'='HHA claim','20'='Non swing bed SNF claim','30'='Swing bed SNF claim',
				'40'='Outpatient claim','50'='Hospice claim','60'='Inpatient claim','61'='Inpatient Full-Encounter'))
dschrgCDMap <- hash(c("1"="DSCHRG Home","2"="DSCHRG/XFER Other Hospital","3"="DSCHRG/XFER SNF","4"="DSCHRG/XFER ICF","5"="DSCHRG/XFER Other",
				"6"="DSCHRG/XFER Organized Home HC","7"="Left Against Advice","8"="DSCHRG Home with IV","9"="XFER Same Hospital",
				"20"="Deceased","21"="DSCHRG/XFER Court","30"="Continue","40"="Deceased","41"="Deceased","42"="Deceased","43"="DSCHRG/XFER Fed Hospital",
				"50"="DSCHRG Hospice","51"="DSCHRG Hospice","61"="XFER Same Hospital","62"="DSCHRG/XFER IRF","63"="DSCHRG/XFER Long Term",
				"65"="DSCHRG/XFER Psychiatric Hospital","66"="DSCHRG/XFER CAH","69"="DSCHRG/XFER Disaster alternative","70"="DSCHRG/XFER Other",
				"71"="DSCHRG/XFER Outpatient Other Hospital","81"="DSCHRG Home planned readmit","82"="DSCHRG/XFER Short term general hospital planned readmit",
				"83"="DSCHRG/XFER SNF planned readmit","84"="DSCHRG/XFER Custodial care planned readmit","85"="DSCHRG/XFER cancer center planned readmit",
				"86"="DSCHRG/XFER Organized Home HC planned readmit","87"="DSCHRG/XFER court planned readmit","88"="DSCHRG/XFER Federal health care facility planned readmit",
				"89"="DSCHRG/XFER to a hospital-based Medicare approved swing bed planned readmit","90"="DSCHRG/XFER IRF planned readmit",
				"91"="DSCHRG/XFER Long Term planned readmit","92"="DSCHRG/XFER Nursing Facility planned readmit","93"="DSCHRG/XFER Psychiatric hospital planned readmit",
				"94"="DSCHRG/XFER CAH planned readmit","95"="DSCHRG/XFER Otherplanned readmit"))

admitSrcMap <- hash(c('~'='~','1'='Refer', '2'='Refer', '3'='Refer', '4'='XFER', '5'='XFER', '6'='Refer', '7'='XFER', '8'='XFER',
				'9'= 'Unknown', 'D'='Same' , 'E'='XFER' , 'F'='XFER'))

admitTypeMap <- hash(c('0'='Blank','1'='Emergency','2'='Urgent','3'='Elective','4'='Newborn','5'='Trauma'))

claimFacilityServiceMap <- hash(c('11'='Hospital_Inpatient','12'='Hospital_Inpatient','13'='Hospital_Outpatient','14'='Hospital_Other',
								  '18'='Hospital_SNF','21'='SNF_Inpatient','22'='SNF_Inpatient','23'='SNF_Outpatient','32'='HHA_Inpatient',
								  '33'='HHA_Outpatient','34'='HHA_Other','71'='Renal_RHC','72'='Renal_HB','74'='Renal_REHAB','75'='Renal_CORF',
								  '77'='Renal_Reserved','81'='ACS_HospiceHB','82'='ACS_HospiceNHB','85'='ACS_CAH')) 

mapDschrgCD <- function(key){return(values((dschrgCDMap[key])))}
mapAdmitSrc <- function(key){return(admitSrcMap[key])}
mapAdmitType <- function(key){return(admitTypeMap[key])}
mapclaimFacilityServiceMap <- function(key){return(claimFacilityServiceMap[key])}

mapCodes <- function(key, type){
	if(type == 'dschrg'){return(values(dschrgCDMap[key]))}
	else if(type == 'admitType'){return(values(admitTypeMap[key]))}
	else if(type == 'admitSrc'){return(values(admitSrcMap[key]))}
	else {return('unknown')}
}
