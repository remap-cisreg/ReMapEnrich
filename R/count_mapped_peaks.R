#' @export
GrCountMappedPeaks <- function(query, catalog)
{
    query@elementMetadata$id = 1:length(query)
    overlaps <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    queryOverlaps <- query[subjectHits(overlaps),]
    count <- length(unique(queryOverlaps))
    return(count)
}