#' @export
GrEnrichment <- function(query, catalog, chromSizes = GetChromSizes("hg19"), shuffles = 10)
{
    # Creation of the two vectors containing the count for teach category.
    categories <- unique(catalog@elementMetadata$id)
    catNumber <- length(categories)
    catCount <- vector()
    shuffleCatCount <- vector()
    catCount[categories] <- 0
    shuffleCatCount[categories] <- 0
    # Computes the intersections betwen query and catalog.
    overlaps <- subsetByOverlaps(catalog, query)
    count <- lengths(split(overlaps@elementMetadata$id, overlaps@elementMetadata$id))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    for(i in 1:shuffles)
    {
        shuffle <- GrShuffle(query, chromSizes)
        shuffleOverlaps <- subsetByOverlaps(catalog, shuffle)
        count <- lengths(split(shuffleOverlaps@elementMetadata$id, shuffleOverlaps@elementMetadata$id))
        shuffleCatCount[names(count)] <- shuffleCatCount[names(count)] + count[names(count)]
    }
    theoricalMeans = shuffleCatCount / shuffles
    enrichPValues = ppois(catCount, theoricalMeans, lower = FALSE)
    depletionPValues = ppois(catCount, theoricalMeans, lower = TRUE)
    enrichPValues[theoricalMeans == 0] = NA
    depletionPValues[theoricalMeans == 0] = NA
    enrichment = data.frame(categories, catCount, theoricalMeans, enrichPValues, depletionPValues)
    colnames(enrichment) <- c("category", "nb.overlaps", "random.average", "enrich.p.value", "depletion.p.value")
    return(enrichment[order(enrichment$enrich.p.value, decreasing = FALSE),])
}