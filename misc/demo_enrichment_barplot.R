# Démonstration de l'exemple 1 de Annotbed avec deux plots en sorti: les 10 premiers peaks et 20 premiers

demo_time <- function()
{   queryFile <- "big_data/ENCFF001VCU.bed"
    catFile <- "big_data/nrPeaks_all.bed"
    query <- BedToGranges(queryFile)
    cat <- BedToGranges(catFile)
    enrich <- GrEnrichment(query, cat, shuffles = 10, lower = FALSE)
    par(mfrow = c(1, 2))
    EnrichmentBarPlot(enrich,lengthData = 10, aRisk = 0.0001)
    EnrichmentVolcanoPlot(enrich, aRisk=0.0001)
}
print(system.time(demo_time()))
