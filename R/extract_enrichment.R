ExtractEnrichment <- function (categories, lower, catCount, shuffleCatCount, shuffles) {
    catNumber <-length(categories)
    # The theorical means are calculated from the shuffles overlaps.
    theoricalMeans <- shuffleCatCount / shuffles
    # The p values are get with log transformation for computing extreme values.
    logPvals <- ppois(catCount, theoricalMeans, lower = lower, log = TRUE)
    # The vectors are sorted for the calculation of the q values.
    order <- order(logPvals)
    theoricalMeans <- theoricalMeans[order]
    catCount <- catCount[order]
    shuffleCatCount <- shuffleCatCount[order]
    logPvals <- sort(logPvals)
    categories <- names(logPvals)
    # Different logarithmic values are calculated for the q value.
    logN <- log(catNumber)
    logI <- log(1:catNumber)
    logC <- log(sum(1/1:catNumber))
    # This is the logarithm of the q values.
    logQVals <- ((logPvals + logN) - logI) + logC
    # This is the logarithm of the e values.
    logEVals <- logPvals + log(catNumber)
    # The different significances are computed from the logarithmic p and q values.
    sigPVals <- - (logPvals / log(10))
    sigQVals <- - (logQVals / log(10))
    sigEVals <- - (logEVals / log(10))
    # The p e and q values are retrieved from the corresponding logarithmic values.
    pVals <- exp(logPvals)
    qVals <- exp(logQVals)
    eVals <- exp(logEVals)
    #The enrichment informations don't make any sense for theorical means at 0.
    sigPVals[theoricalMeans == 0] = NA
    pVals[theoricalMeans == 0] = NA
    sigQVals[theoricalMeans == 0] = NA
    qVals[theoricalMeans == 0] = NA
    sigEVals[theoricalMeans == 0] = NA
    eVals[theoricalMeans == 0] = NA
    # Computation of the effecct size.
    effectSizes <- log(catCount / theoricalMeans, base = 2)
    # Creation of the data frame with all the enrichment informations.
    enrichment <- data.frame(categories, catCount, theoricalMeans, effectSizes, sigPVals, pVals, sigQVals, qVals, sigEVals, eVals)
    # Naming of the columns and reordering the data frame.
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "effect.size",
                              "p.significance", "p.value", "q.significance", "q.value", "e.significance", "e.value")
    enrichment <- enrichment[order(enrichment$q.significance, decreasing = TRUE),]
    return(enrichment)
}