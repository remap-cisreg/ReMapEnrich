#' @title Intersect
#' @author Zacharie Menetrier
#' @description Computes the intersections between two genomic ranges object.
#' 
#' @param query A genomic ranges object to be analyzed.
#' @param catalog A genomic ranges object to be compared to the query.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit
#'  must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit
#'  must exceed to be accounted.
#' @param categories=unique(catalog@elementMetadata$id) The categories 
#' contained in the catalog.
#' This option is leaved for faster calculation when this function is runned 
#' multiple times.
#' 
#' @return A vector containing the numbers of hits for each category of the catalog.
#' 
#' @usage intersect(query, catalog, fractionQuery = 0.1, fractionCatalog = 0.1,
#'                    categories = unique(catalog@elementMetadata$id))
#' 
#' @examples
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed",
#'                           package = "ReMapEnrich")
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                             package = "ReMapEnrich")
#' query <- bedToGranges(queryFile)
#' catalog <- bedToGranges(catalogFile)
#' intersects <- intersect(query, catalog)
#' 
#' @export
intersect <- function(query,
                        catalog, 
                        fractionQuery = 0.1, 
                        fractionCatalog = 0.1, 
                        categories = unique(catalog@elementMetadata$id)) {
    # Creation of the vector that will contain all the number of overlaps 
    # for each category.
    catCount <- vector()
    catCount[categories] <- 0
    # Retrives the hits from the overlapping of the query and catalog.
    hits <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    # Selecting only the hiths that satisfies the fractionQuery 
    # and fractionCatalog parameters.
    overlaps <- GenomicRanges::pintersect(catalog[S4Vectors::queryHits(hits)], 
                                          query[S4Vectors::subjectHits(hits)])
    percentQuery <- 
        IRanges::width(overlaps) / IRanges::width(query[S4Vectors::subjectHits(hits)]) >= fractionQuery
    percentCatalog <- 
        IRanges::width(overlaps) / IRanges::width(catalog[S4Vectors::queryHits(hits)]) >= fractionCatalog
    percent <- percentQuery & percentCatalog
    hits <- hits[percent]
    catOverlaps <- catalog[S4Vectors::queryHits(hits),]
    # Fills the counting vector with the selected hits.
    count <- lengths(split(catOverlaps@elementMetadata$id, 
                           catOverlaps@elementMetadata$id))
    catCount[names(count)] <- catCount[names(count)] + count[names(count)]
    return(catCount)
}
