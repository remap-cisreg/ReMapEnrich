#' @export
GrShuffle <- function(regions, chromSizes = GetChromSizes("hg19"))
{
    regionsLength <- regions@ranges@width
    possibleStarts <- chromSizes[as.vector(regions@seqnames), ] - regionsLength
    randomStarts <- sample.int(possibleStarts, size = length(regions))
    granges <- GRanges(regions@seqnames, IRanges(start = randomStarts, width = regionsLength), strand=regions@strand)
    return(granges)
}