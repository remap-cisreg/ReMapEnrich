ComputeEnrichment <- function (query, catalog, chromSizes, fractionQuery, fractionCatalog, shuffles, lower,
                               intersectFunction, shuffleFunction, categories) {
    catNumber <- length(categories)
    if(catNumber < 1)
        stop("The catalog does not comprize any category.")
    # Computes the intersections betwen query and catalog.
    catCount <- intersectFunction(query, catalog, fractionQuery, fractionCatalog, categories)
    # Shuffles are created and computed as the query for bootstrapping.
    shuffleCatCount <- vector()
    shuffleCatCount[categories] <- 0
    for(i in 1:shuffles) {
        cat("\r", "Shuffle # ", i)
        flush.console()
        shuffle <- shuffleFunction(query, chromSizes)
        # Computes the intersections betwen shuffle and catalog.
        count <- intersectFunction(shuffle, catalog, fractionQuery, fractionCatalog, categories)
        # Adds the found overlaps in the count.
        shuffleCatCount <- shuffleCatCount + count
        if(is.character(shuffle)){
            unlink(shuffle)
        }
    }
    return(list(catCount, shuffleCatCount))
}