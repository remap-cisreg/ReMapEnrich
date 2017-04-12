#' @export
RandomIntersections <- function(catalog, iterations, regionNb, regionSize, chromSizes = LoadChromSizes("hg19")) {
    ## Instantiate result table
    categories <- unique(catalog@elementMetadata$id)
    cat("Generating random regions.\n")
    randomRegions <- pbapply::pbreplicate(iterations, GenRegions(regionNb, regionSize, chromSizes))
    cat("Computing intersections.\n")
    result <- pbapply::pblapply(randomRegions, GrIntersect, catalog = catalog, categories = categories)
    result <- matrix(unlist(result), ncol = length(categories), byrow = TRUE)
    colnames(result) <- categories
    return(result)
}
