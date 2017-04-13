#' @title Genomic ranges shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param regions The genomic ranges object containing the genomic 
#'  regions to shuffle.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all 
#'  the chromosome lengths for the species in consideration.
#' 
#' @return A genomic ranges containing the new shuffled chromosic regions.
#' 
#' @usage GrShuffle(regions, chromSizes = LoadChromSizes("hg19"))
#' 
#' @examples 
#' regionsFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                             package = "roken")
#' regions <- BedToGranges(regionsFile)
#' shuffledRegions <- GrShuffle(regions)
#' 
#' @export
GrShuffle <- function(regions, chromSizes = LoadChromSizes("hg19"), universe = NULL) {
    if (is.null(universe)) {
        regionsLength <- regions@ranges@width
        possibleStarts <- chromSizes[as.vector(regions@seqnames), ] - regionsLength
        # Gets all the random starts from sampling the possible starts.
        randomStarts <- unlist(lapply(possibleStarts, sample.int, size = 1))
        granges <- GRanges(regions@seqnames, IRanges(start = randomStarts,
                                                     width = regionsLength),
                           strand=regions@strand)
        return(granges)
    }
    chroms <- as.vector(regions@seqnames)
    universeChroms <- unique(universe@seqnames)
    if (sum(!is.element(unique(chroms), universeChroms)) > 0){
        warning("The regions to be shuffled contain chromosomes that are not in the universe.\nThose regions will be ignored and the resulting shuffles will be smaller.")
        regions <- regions[is.element(chroms, universeChroms)]
    }
    toShuffle <- regions
    toShuffle@elementMetadata$id <- c(1:length(toShuffle))
    result <- GRanges()
    while (length(toShuffle) > 1) {
        cat("\r", length(toShuffle), "\r")
        flush.console()
        regionsLength <- toShuffle@ranges@width
        possibleStarts <- chromSizes[as.vector(toShuffle@seqnames), ] - regionsLength
        randomStarts <- unlist(lapply(possibleStarts, sample.int, size = 1))
        granges <- GRanges(toShuffle@seqnames, IRanges(start = randomStarts,
                                                     width = regionsLength),
                           strand = toShuffle@strand, id = toShuffle@elementMetadata$id)
        overlaps <- findOverlaps(granges, universe)
        inUniverse <- granges[unique(queryHits(overlaps))]
        toShuffle <- toShuffle[!toShuffle@elementMetadata$id %in% inUniverse@elementMetadata$id]
        result <- c(result, inUniverse)
        flush.console()
    }
    return(result)
}
