#' @title Computes enrichment
#' @author Zacharie Menetrier
#' @description Gets the number of overlaps for the query and the shuffles for
#'  each category of the catalog.
#' 
#' @param query The object containing the genomic regions to analyze.
#' @param catalog The object containing the database used for annotation.
#' @param chromSizes An object containing all the chromosome lengths for the 
#'  species in consideration.
#' @param fractionQuery The fraction of coverage (query on catalog) a hit must
#'  exceed to be accounted.
#' @param fractionCatalog The fraction of coverage (catalog on query) a hit must
#'  exceed to be accounted.
#' @param shuffles The number of shuffled genomic regions to be created for 
#' theorical distribution (higher means more accurate).
#' @param categories The categories contained in the catalog.
#' This option is leaved for faster calculation when this function is runned
#'  multiple times.
#' 
#' @return A list containing the number of overlaps for the query and the 
#' mean number of overlaps for the shuffles.
ComputeEnrichment <- function(query,
                              catalog, 
                              chromSizes,
                              fractionQuery, 
                              fractionCatalog, 
                              shuffles,
                               intersectFunction,
                              shuffleFunction, 
                              categories) {
    catNumber <- length(categories)
    if(catNumber < 1)
        stop("The catalog does not comprize any category.")
    # Computes the intersections betwen query and catalog.
    catCount <- intersectFunction(query, catalog, fractionQuery, 
                                  fractionCatalog, categories)
    # Shuffles are created and computed as the query for bootstrapping.
    shuffleCatCount <- vector()
    shuffleCatCount[categories] <- 0
    for(i in 1:shuffles) {
        cat("\r", "Shuffle # ", i)
        flush.console()
        shuffle <- shuffleFunction(query, chromSizes)
        # Computes the intersections betwen shuffle and catalog.
        count <- intersectFunction(shuffle, catalog, fractionQuery,
                                   fractionCatalog, categories)
        # Adds the found overlaps in the count.
        shuffleCatCount <- shuffleCatCount + count
        if(is.character(shuffle)){
            unlink(shuffle)
        }
    }
    # The theorical means are calculated from the shuffles overlaps.
    theoricalMeans <- shuffleCatCount / shuffles
    return(list(catCount, theoricalMeans))
}
