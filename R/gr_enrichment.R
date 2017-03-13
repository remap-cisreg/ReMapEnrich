#' Computes enrichment
#' 
#' Gets the value of genomic enrichment for each feature of a genomic ranges object.
#' 
#' @param query The genomic ranges object containing the genomic regions to analyze.
#' @param catalog The genomic ranges object containing the database used for annotation.
#' @param chromSizes A vector containing all the chromosome lengths for the species in consideration.
#' @param shuffles The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
GrEnrichment <- function(query, catalog, chromSizes = GetChromSizes("hg19"), shuffles = 20)
{
    # Creation of the two vectors containing the count for each category.
    categories <- unique(catalog@elementMetadata$id)
    catNumber <- length(categories)
    # This vector is for the query.
    catCount <- vector()
    # This vector is for the shuffles that will be created.
    shuffleCatCount <- vector()
    catCount[categories] <- 0
    shuffleCatCount[categories] <- 0
    # Computes the intersections betwen query and catalog.
    count <- GrIntersect(query, catalog)
    # Adds the found overlaps in the count.
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    # Shuffles are created and computed as the query for bootstrapping.
    for(i in 1:shuffles)
    {
        shuffle <- GrShuffle(query, chromSizes)
        # Computes the intersections betwen shuffle and catalog.
        count <- GrIntersect(shuffle, catalog)
        # Adds the found overlaps in the count.
        shuffleCatCount[names(count)] <- shuffleCatCount[names(count)] + count[names(count)]
    }
    # The theorical means are calculated from the shuffles overlaps.
    theoricalMeans <- shuffleCatCount / shuffles
    # The significance is a number directly related to the p value.
    # The significance is in a range that is more understandable and computable.
    significances <- ppois(catCount, theoricalMeans, lower = FALSE, log = TRUE) / 2.302585
    pValues <- 10 ** significances 
    significances <- - significances
    # The p values are adjusted by the Benjamini-Hochberg method.
    pValues <- p.adjust(pValues, method = "BH")
    # If the theorical means are at 0 then the pvalues and significance are not numbers.
    pValues[theoricalMeans == 0] <- NA
    significances[theoricalMeans == 0] <- NA
    enrichment = data.frame(categories, catCount, theoricalMeans, pValues, significances)
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "p.value", "significance")
    return(enrichment[order(enrichment$significance, decreasing = TRUE),])
}