#' @titleGenomic ranges intersections
#' @author Zacharie Menetrier
#' @description Computes the intersections between two genomic ranges object.
#' 
#' @param query A genomic ranges object to be analyzed.
#' @param catalog A genomic ranges object to be compared to the query.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit must exceed to be accounted.
#' 
#' @return A vector containing the numbers of hits for each category of the catalog.
#' 
#' @export
GrIntersect <- function(query, catalog, fractionQuery = 0.1, fractionCatalog = 0.1, categories = unique(catalog@elementMetadata$id)) {
    catCount <- vector()
    catCount[categories] <- 0
    hits <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    overlaps <- GenomicRanges::pintersect(catalog[queryHits(hits)], query[subjectHits(hits)])
    percentQuery <- width(overlaps) / width(query[subjectHits(hits)]) >= fractionQuery
    percentCatalog <- width(overlaps) / width(catalog[queryHits(hits)]) >= fractionCatalog
    percent <- percentQuery & percentCatalog
    hits <- hits[percent]
    catOverlaps <- catalog[queryHits(hits),]
    count <- lengths(split(catOverlaps@elementMetadata$id, catOverlaps@elementMetadata$id))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    return(catCount)
}