#' Computes enrichment
#' 
#' Gets the value of genomic enrichment for each feature of a bed file.
#' 
#' @param queryFile The bed file containing the genomic regions to analyze.
#' @param catalog The bed file containing the database used for annotation.
#' @param chromFile A file containing all the chromosome lengths for the species in consideration.
#' @param fraction The fraction that intersections shall exceed to be considered.
#' @param shuffles The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
BedEnrichment <- function(queryFile, catalogFile, chromFile = GetChromFile("hg19"), fraction = 0, shuffles = 10, lower = FALSE){
    # Creation of the two vectors containing the count for each category.
    categories <- unique(BedImport(catalogFile)$name)
    catNumber <- length(categories)
    # This vector is for the query.
    catCount <- vector()
    # This vector is for the shuffles that will be created.
    shuffleCatCount <- vector()
    catCount[categories] <- 0
    shuffleCatCount[categories] <- 0
    # Computes the intersections betwen query and catalog.
    overlaps <- BedIntersect(catalogFile, queryFile, fraction)
    count <- lengths(split(overlaps$name, overlaps$name))
    # Adds the found overlaps in the count.
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    # Shuffles are created and computed as the query for bootstrapping.
    for(i in 1:shuffles){
        shuffleFile <- BedShuffleTempFile(queryFile, chromFile)
        # Computes the intersections betwen shuffle and catalog.
        shuffleOverlaps <- BedIntersect(catalogFile, shuffleFile, fraction)
        count <- lengths(split(shuffleOverlaps$name, shuffleOverlaps$name))
        # Adds the found overlaps in the count.
        shuffleCatCount[names(count)] <- shuffleCatCount[names(count)] + count[names(count)]
    }
    # The theorical means are calculated from the shuffles overlaps.
    theoricalMeans <- shuffleCatCount / shuffles
    # The significance is a number directly related to the p value.
    # The significance is in a range that is more understandable and computable.
    significances <- ppois(catCount, theoricalMeans, lower = lower, log = TRUE) / 2.302585
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