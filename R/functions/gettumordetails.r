# Returns a dataframe with details of a specific type of tumor, giving number of samples
library(RCurl)
library(rjson)
library(plyr)

source("gettumors.r")

getTumorDetails <- function(tumor) {
	if (!(toupper(tumor) %in% getTumors()$"Cancer Abbreviation")) {
		return("Incorrect type of tumor")
	}
	
	uri <- paste0("https://tcga-data.nci.nih.gov/tcga/damws/tumordetails/json?diseaseType=",toupper(tumor))
	json <- getURL(uri)
	rjson <- fromJSON(json)

	samples <- NULL
	for (i in rjson$tumorSampleTypeCount) {
		 samples <- rbind(samples, c(i$sampleType, i$total, i$copyNumber, i$methylation, i$geneExpression, i$miRnaExpression))
	}
	dataframe <- adply(samples, c(1))
	colnames(dataframe) <- c(NA, "Data Type", "Total", "Copy Number", "Methylation", "Gene Expression", "miRNA Expression")
	return(dataframe[, c(2,3,4,5,6,7)])
}