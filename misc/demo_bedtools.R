# Démonstration de l'exemple 1 de Annotbed avec Bedtools avec deux plots en sorti: les 10 premiers peaks et 20 premiers

demo_time <- function()
{   
    queryFile <- "big_data/ENCFF001VCU.bed"
    catFile <- "big_data/nrPeaks_all.bed"
    enrich <- BedEnrichment(queryFile, catFile, shuffles = 1, lower = FALSE)
    par(mfrow = c(1, 2))
    EnrichmentBarPlot(enrich,lengthData = 10)
    EnrichmentVolcanoPlot(enrich)
}
print(system.time(demo_time()))
