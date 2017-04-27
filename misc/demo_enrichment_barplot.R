# Démonstration de l'exemple 1 de Annotbed avec deux plots en sorti: les 10 premiers peaks et 20 premiers

demo_time <- function(queryFile = "big_data/ENCFF001SUX.bed",
                      catFile = "big_data/nrPeaks_all.bed") {
    query <- BedToGranges(queryFile)
    cat <- BedToGranges(catFile)
    enrich <- Enrichment(query, cat, shuffles = 10)
    par(mfrow = c(1, 2))
    EnrichmentBarPlot(enrich,top = 20)
    EnrichmentVolcanoPlot(enrich,aRisk = 0.05)
    return(enrich)
}

print(system.time(enrich <- demo_time()))

