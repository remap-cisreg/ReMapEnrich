#' @title Extract enrichment
#' @author Zacharie Menetrier
#' @description Extracts the information for genomic enrichment.
#' 
#' @param categories=unique(catalog@elementMetadata$id) The categories contained in the catalog.
#' This option is leaved for faster calculation when this function is runned multiple times.
#' @param lower=FALSE If FALSE (default), probabilities are P[X > x], otherwise, P[X <= x].
#' @param categoriesOverlaps The number of overlaps for each category.
#' @param theoricalMeans The mean number of overlaps for each category.
#' @param categoriesCount A vector representing the number of time a category is found in the catalog.
#' 
#' @return A data frame containing the enrichment informations.
ExtractEnrichment <- function (categories, lower, categoriesOverlaps, theoricalMeans, categoriesCount) {
    catNumber <-length(categories)

    # The p values are get with log transformation for computing extreme values.
    logPVals <- ppois(categoriesOverlaps, theoricalMeans, lower = lower, log = TRUE)
    
    # Creation of the data frame with all the enrichment informations.
    enrichment <- data.frame(categories, stringsAsFactors = FALSE)
    enrichment = cbind(enrichment,
                       categoriesOverlaps[categories],
                       theoricalMeans[categories],
                       categoriesOverlaps[categories] / categoriesCount[categories])

    # The data frame is sorted for the calculation of the q values.
    enrichment = enrichment[order(logPVals[categories]),]
    
    logPVals <- sort(logPVals)
    theoricalMeans <- enrichment[,3]
    categoriesOverlaps <- enrichment[,2]
    
    # Different logarithmic values are calculated for the q value.
    logN <- log(catNumber)
    logI <- log(1:catNumber)
    logC <- log(sum(1/(1:catNumber)))

    # This is the logarithm of the q values.
    logQVals <- ((logPVals + logN) - logI)
    logQVals <- cummax(logQVals)
    
    # This is the logarithm of the e values.
    logEVals <- logPVals + log(catNumber)
    
    # The different significances are computed from the logarithmic p and q values.
    sigPVals <- - (logPVals / log(10))
    sigQVals <- - (logQVals / log(10))
    sigEVals <- - (logEVals / log(10))
    
    # The p e and q values are retrieved from the corresponding logarithmic values.
    pVals <- exp(logPVals)
    qVals <- exp(logQVals)
    eVals <- exp(logEVals)
    
    # The enrichment informations don't make any sense for theorical means at 0.
    sigPVals[theoricalMeans == 0] = NA
    pVals[theoricalMeans == 0] = NA
    sigQVals[theoricalMeans == 0] = NA
    qVals[theoricalMeans == 0] = NA
    sigEVals[theoricalMeans == 0] = NA
    eVals[theoricalMeans == 0] = NA

    # Computation of the effecct size.
    effectSizes <- log(categoriesOverlaps / theoricalMeans, base = 2)
    enrichment <- cbind(enrichment, effectSizes, sigPVals, pVals, sigQVals, qVals, sigEVals, eVals)
    # Naming of the columns and reordering the data frame.
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "mapped.peaks.ratio", "effect.size",
                              "p.significance", "p.value", "q.significance", "q.value", "e.significance", "e.value")
    enrichment <- enrichment[order(enrichment$q.significance, decreasing = TRUE),]
    return(enrichment)
}