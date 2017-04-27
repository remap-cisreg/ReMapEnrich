library(RColorBrewer)

#-- Get ReMap
targetDir="~/roken_demo"
catFile <- DownloadRemapCatalog(targetDir, fileName = "nrPeaks_all.bed")

#DownloadEncodeBed
#https://www.encodeproject.org/files/ENCFF001VCU/@@download/ENCFF001VCU.bed.gz

download.file(url='https://www.encodeproject.org/files/ENCFF001VCU/@@download/ENCFF001VCU.bed.gz', destfile = file.path(targetDir, "ENCFF001VCU.bed.gz"))
## ADD gunzip !!!!
queryFile <- file.path(targetDir, "ENCFF001VCU.bed")    

#-- Set Files 
#queryFile="/Users/benoit/projects/roken/inst/extdata/ENCFF001VCU.bed"
#catFile="/Users/benoit/projects/roken/inst/extdata/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)


#-- Color scale
n <- 10
cols <- colorRampPalette(brewer.pal(8,"BuPu"))(n)

#-- Compute
enrich <- Enrichment(query, cat, shuffles = 10, lower = FALSE)

#-- Plots
op <- par(mfrow = c(1, 2))

EnrichmentVolcanoPlot(enrich,aRisk = 0.05)


EnrichmentBarPlot(enrich,top = 30)

par(op) # Reset default parameters

