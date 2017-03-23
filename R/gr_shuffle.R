#' @title Genomic ranges shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param regions The genomic ranges object containing the genomic regions to shuffle.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome lengths for the species in consideration.
#' 
#' @return A genomic ranges containing the new shuffled chromosic regions.
#' 
#' @export
GrShuffle <- function(regions, chromSizes = ImportChromSizes("hg19")) {
    regionsLength <- regions@ranges@width
    possibleStarts <- chromSizes[as.vector(regions@seqnames), ] - regionsLength
    randomStarts <- unlist(lapply(possibleStarts, sample.int, size = 1))
    granges <- GRanges(regions@seqnames, IRanges(start = randomStarts, width = regionsLength), strand=regions@strand)
    return(granges)
}