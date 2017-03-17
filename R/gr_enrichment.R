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
    if(catNumber < 2)
        stop("There is less than 2 categories found in the catalog.")
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
    adjustedSignificances <- (ppois(catCount, theoricalMeans, lower = lower, log = TRUE) + log(catNumber)) / 2.302585
    adjustedPValues <- 10 ** adjustedSignificances 
    adjustedSignificances <- - adjustedSignificances
    #
    significances <- (ppois(catCount, theoricalMeans, lower = lower, log = TRUE)) / 2.302585
    pValues <- 10 ** significances 
    significances <- - significances
    # If the theorical means are at 0 then the pvalues and significance are not numbers.
    pValues[theoricalMeans == 0] <- NA
    significances[theoricalMeans == 0] <- NA
    adjustedPValues[theoricalMeans == 0] <- NA
    adjustedSignificances[theoricalMeans == 0] <- NA
    enrichment = data.frame(categories, catCount, theoricalMeans, pValues, significances, adjustedPValues, adjustedSignificances)
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "p.value", "significance", "adjusted.p.value", "adjusted.significance")
    enrichment <- enrichment[order(enrichment$adjusted.significance, decreasing = TRUE),]
    return(enrichment)
}