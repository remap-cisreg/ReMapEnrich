queryFile <- "big_data/ENCFF001VCU.bed"
catFile <- "big_data/nrPeaks_all.bed"
query <- BedToGranges(queryFile)
cat <- BedToGranges(catFile)
enrich <- GrEnrichment(query, cat, lower = FALSE)
par(mfrow = c(1, 2))
EnrichmentBarPlot(enrich, lengthData = 10)
EnrichmentVolcanoPlot(enrich)
