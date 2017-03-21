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
    #
    logPvals <- ppois(catCount, theoricalMeans, lower = lower, log = TRUE)
    sigEVals <- - (logPvals + log(catNumber)) / log(10)
    pVals <- 10 ** logPvals
    eVals <- pVals * catNumber
    
    ## TO DO NEXT TIME: MANUAL COMPUTATION OF VALUE FROM LOG(PVAL)
    
    effectSizes <- log(catCount / theoricalMeans, base = 2)
    enrichment <- data.frame(categories, catCount, theoricalMeans, pVals, sigEVals, eVals)
    
   ## enrichment = data.frame(categories, catCount, theoricalMeans, pValues, significances, adjustedPValues, adjustedSignificances, effectSizes)
  ##  colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "p.value", "significance", "adjusted.p.value", "adjusted.significance", "effect.size")
##    enrichment <- enrichment[order(enrichment$adjusted.significance, decreasing = TRUE),]
    return(enrichment)
}