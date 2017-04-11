#' @export
RandomTest <- function(catalog, iterations, regionNb, regionSize) {
    ## Instantiate result table
    catalogNames <- unique(catalog@elementMetadata$id)
    result <- data.frame(matrix(nrow=iterations, ncol=length(catalogNames)))
    colnames(result) <- catalogNames
    for (i in 1:iterations) {
        cat("\r", i, "/", iterations)
        flush.console()
        overlaps <- GrIntersect(GenRegions(n = regionNb, size = regionSize), catalog)
        result[i, catalogNames] <- overlaps[catalogNames]
    }
    return(result)
}
