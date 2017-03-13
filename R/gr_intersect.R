#' @export
GrIntersect <- function(query, catalog)
{
    overlaps <- GenomicRanges::findOverlaps(catalog, query, type = "any")
    catOverlaps <- catalog[queryHits(overlaps),]
    count <- lengths(split(catOverlaps@elementMetadata$id, catOverlaps@elementMetadata$id))
    return(count)
}