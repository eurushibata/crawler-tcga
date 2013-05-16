# Sends a form POST request to get Data Reports to build metadata for TCGA barcodes. See for MANIFEST.
library(RCurl)
library(rjson)


# TGGA uri for tab-delimited export
uri <- "https://tcga-data.nci.nih.gov/datareports/codeTablesExport.htm?exportType=tab&codeTablesReport="

# regex for TCGA barcode
pattern <- "*(TCGA)-([A-Z0-9]{2})-([A-Z0-9]{4})-(0[1-9]|[1-2][0-9])([A-Z])-(0[1-9]|[1-9][0-9])([DGHRTWX])-([A-Z0-9]{4})-([A-Z0-9]{2})*"

tss <- render(postForm(paste0(uri, "tissueSourceSite"))[1])
participant <- ""
sample <- render(postForm(paste0(uri, "sampleType"))[1])
vial <- ""
portion <- ""
analyte <- render(postForm(paste0(uri, "portionAnalyte"))[1])
plate <- ""
center <- render(postForm(paste0(uri, "centerCode"))[1])

render <- function(t) {
	u <- strsplit(t[1],"\n")[[1]]
	for (i in seq(u)) {
		arr <- matrix(strsplit(u[i], "\t")[[1]], nrow=1)
		if (i==1) {
			v <- data.frame()
			header <- arr
		} else {
			v <- rbind(v, as.data.frame(arr))
		}
	}
	colnames(v) <- header
	return(v)
}

# get MANIFEST data.frame from barcode given or array of barcodes
getMetadata <- function(data){
	header <- c("barcode",
				"source.site",
				"study.name",
				"biospecimen.core.research",
				"participant",
				"sample.code",
			 	"sample.type",
				"sample.definition",
				"vial",
				"portion.order",
				"analyte.code",
				"analyte.definition",
				"plate.order",
				"center.code",
				"center.name",
				"center.type",
				"center.full.name",
				"center.short.name"
				)
	v <- data.frame()

	# df <- data.frame(stringAsFactors=FALSE)
	for (i in seq(data)) {
		f <- data[i]

		a <- str_match(f, pattern)

		# TSS
		stss <- subset(tss, tss$"TSS Code"==a[3])
		tss.source.site <- levels(droplevels(stss$"Source Site"))[1]
		tss.study.name <- levels(droplevels(stss$"Study Name"))[1]
		tss.bcr <- levels(droplevels(stss$"BCR"))[1]

		# Sample
		ssample <- subset(sample, sample$"Code"==a[5])
		if ((as.integer(a[5]) >= 1)&&(as.integer(a[5]) <= 9)) {
			sample.type <- "TUMOR"
		} else if ((as.integer(a[5]) >= 10)&&(as.integer(a[5]) <= 19)) {
			sample.type <- "NORMAL"
		} else if ((as.integer(a[5]) >= 20)&&(as.integer(a[5]) <= 29)) {
			sample.type <- "CONTROL"
		} else {
			sample.type <- "UNKNOWN"
		}
		sample.definition <- levels(droplevels(ssample$"Definition"))[1]

		# Analyte
		sanalyte <- subset(analyte, analyte$"Code"==a[8])
		analyte.definition <- levels(droplevels(sanalyte$"Definition"))[1]

		# Center
		scenter <- subset(center, center$"Code"==a[10])
		center.name <- levels(droplevels(scenter$"Center Name"))[1]
		center.type <- levels(droplevels(scenter$"Center Type"))[1]
		center.full.name <- levels(droplevels(scenter$"Display Name"))[1]
		center.short.name <- levels(droplevels(scenter$"Short Name"))[1]

		# mount array to be append
		arr <- matrix(c(a[1], tss.source.site, tss.study.name, tss.bcr, a[4], a[5], sample.type, sample.definition, a[6], a[7], a[8], analyte.definition, a[9], a[10], center.name, center.type, center.full.name, center.short.name), nrow=1)
		
		# append array to data.frame
		v <- rbind(v, as.data.frame(arr))
	}
	colnames(v) <- header
	return(v)
}
