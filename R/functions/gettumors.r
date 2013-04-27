# Returns a dataframe for all available Cancer Types in TCGA Data Portal
library(RCurl)
library(rjson)
library(plyr)

getTumors <- function() {
	tumormain <- "https://tcga-data.nci.nih.gov/tcga/damws/tumormain/json"
	json <- getURL(tumormain)
	rjson <- fromJSON(json)

	tumors <- NULL
	for (i in rjson$tumorMainCount) {
		 tumors <- rbind(tumors, c(i$tumorName, i$tumorAbbreviation, i$patientSamples, i$downloadableTumorSamples, i$lastUpdate))
	}
	dataframe <- adply(tumors, c(1))
	colnames(dataframe) <- c(NA, "Available Cancer Types","Cancer Abbreviation", "# Patients with Samples", "# Downloadable Tumor Samples", "Date Last Updated (mm/dd/yy)")
	return(dataframe[, c(2,3,4,5,6)])
}

getTumors()