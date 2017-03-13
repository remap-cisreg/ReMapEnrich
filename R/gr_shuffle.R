#' Genomic ranges shuffle
#' 
#' Shuffle genomic regions among a defined genome.
#' 
#' @param regions The genomic ranges object containing the genomic regions to shuffle.
#' @param chromSizes A vector containing all the chromosome lengths for the species in consideration.
#' 
#' @return A genomic ranges containing the new shuffled chromosic regions.
#' 
#' @export
GrShuffle <- function(regions, chromSizes = GetChromSizes("hg19"))
{
    regionsLength <- regions@ranges@width
    possibleStarts <- chromSizes[as.vector(regions@seqnames), ] - regionsLength
    randomStarts <- sample.int(possibleStarts, size = length(regions))
    granges <- GRanges(regions@seqnames, IRanges(start = randomStarts, width = regionsLength), strand=regions@strand)
    return(granges)
}