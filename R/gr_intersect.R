#' Genomic ranges intersections
#' 
#' Computes the intersections between two genomic ranges object.
#' 
#' @param query A genomic ranges objecct to be analyzed.
#' @param catalog A genomic ranges object to be compared to the query.
#' 
#' @return A vector containing the numbers of hits for each category of the catalog.
#' 
#' @export
GrIntersect <- function(query, catalog){
    categories <- unique(catalog@elementMetadata$id)
    catCount <- vector()
    catCount[categories] <- 0
    overlaps <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    catOverlaps <- catalog[queryHits(overlaps),]
    count <- lengths(split(catOverlaps@elementMetadata$id, catOverlaps@elementMetadata$id))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    return(catCount)
}