# D2monstration de l'exmple 1 Annotbed avec deux plots en sorti: les 10 premiers peaks et 20 premiers


queryFile <- "big_data/ENCFF001VCU.bed"
catFile <- "big_data/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- GrEnrichment(query, cat, shuffles = 10, lower = FALSE)
par(mfrow = c(1, 2))
EnrichmentBarPlot(enrich,lengthData = 10)
EnrichmentBarPlot(enrich,lengthData = 20)
# ExportEnrichment(enrich, format = "csv") 
# EnrichmentPiePlot(enrich, lengthData = 10)