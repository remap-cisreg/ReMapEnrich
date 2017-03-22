#' @title Computes enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each category of a genomic ranges object.
#' 
#' @param query The genomic ranges object containing the genomic regions to analyze.
#' @param catalog The genomic ranges object containing the database used for annotation.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome lengths for the species in consideration.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit must exceed to be accounted.
#' @param shuffles=6 The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' @param lower=FALSE If FALSE (default), probabilities are P[X > x], otherwise, P[X <= x].
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
GrEnrichment <- function(query, catalog, chromSizes = ImportChromSizes("hg19"), fractionQuery = 0.1, fractionCatalog = 0.1, shuffles = 6, lower = FALSE) {
    # Creation of the two vectors containing the count for each category.
    categories <- unique(catalog@elementMetadata$id)
    catNumber <- length(categories)
    if(catNumber < 1)
        stop("The catalog does not comprize any category.")
    # Computes the intersections betwen query and catalog.
    catCount <- GrIntersect(query, catalog, fractionQuery, fractionCatalog)
    # Shuffles are created and computed as the query for bootstrapping.
    shuffleCatCount <- vector()
    shuffleCatCount[categories] <- 0
    for(i in 1:shuffles) {
        shuffle <- GrShuffle(query, chromSizes)
        # Computes the intersections betwen shuffle and catalog.
        count <- GrIntersect(shuffle, catalog, fractionQuery, fractionCatalog)
        # Adds the found overlaps in the count.
        shuffleCatCount <- shuffleCatCount + count
    }
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