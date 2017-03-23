library(RColorBrewer)

#-- Get ReMap
targetDir="/Users/benoit/projects/roken/inst/extdata"
DownloadRemapCatalog(targetDir, fileName = "nrPeaks_all.bed", force = TRUE)

#DownloadEncodeBed
#https://www.encodeproject.org/files/ENCFF001VCU/@@download/ENCFF001VCU.bed.gz

#-- Set Files 
queryFile="/Users/benoit/projects/roken/inst/extdata/ENCFF001VCU.bed"
catFile="/Users/benoit/projects/roken/inst/extdata/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)


#-- Color scale
n=10
cols <- colorRampPalette(brewer.pal(8,"BuPu"))(n)

#-- Compute
enrich <- GrEnrichment(query, cat, shuffles = 100, lower = FALSE)


#-- Plots
op <- par(mfrow = c(1, 2))

EnrichmentVolcanoPlot(enrich,aRisk = 0.05)

#EnrichmentBarPlot(enrich,lengthData = 10, col=cols)
EnrichmentBarPlot(enrich,lengthData = 10)

par(op)

