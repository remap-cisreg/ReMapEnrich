#' @title Count mapped peaks
#' @author Zacharie Menetrier
#' @description This function counts the number of peaks that are mapped with 
#' two genomic ranges object.
#' It counts the number of peaks in the query that are mapped in the catalog.
#'
#' @param query A genomic ranges object containing the peaks to analyze.
#' @param catalog A genomic ranges object containing the peaks that will be 
#' compared to the query.
#' 
#' @return An integer that represents the number of peaks mapped.
#' 
#' @usage CountMappedPeaks(query, catalog)
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed",
#'                           package = "roken")
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                             package = "roken")
#' query <- BedToGranges(queryFile)
#' catalog <- BedToGranges(catalogFile)
#' countMappedPeaks <- CountMappedPeaks(query, catalog)
#' 
#' @export
CountMappedPeaks <- function(query, catalog) {
    # Sets an id for each element of the query.
    query@elementMetadata$id = 1:length(query)
    # Overlaps the query and catalog and find the number of ids that are 
    # still existing.
    overlaps <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    queryOverlaps <- query[S4Vectors::subjectHits(overlaps),]
    count <- length(unique(queryOverlaps))
    return(count)
}
