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
GrShuffle <- function(regions, chromSizes = LoadChromSizes("hg19"), universe = NULL) {
    # Gets all the regions lengths from the query.
    regionsLength <- regions@ranges@width
    # The possible starts are the chromosome sizes - the regions lengths.
    if (is.null(universe)) {
        possibleStarts <- chromSizes[as.vector(regions@seqnames), ] - regionsLength
        # Gets all the random starts from sampling the possible starts.
        randomStarts <- unlist(lapply(possibleStarts, sample.int, size = 1))
    } else {
        chroms <- as.vector(regions@seqnames)
        universeChroms <- unique(universe@seqnames)
        if (sum(!is.element(unique(chroms), universeChroms)) > 0){
            warning("The regions to be shuffled contain chromosomes that are not in the universe.\nThose regions will be ignored and the resulting shuffles will be smaller.")
            regions <- regions[is.element(chroms, universeChroms)]
            regionsLength <- regions@ranges@width
        }
        splittedUniverse <- split(universe, universe@seqnames)
        randomUniverseRegions <- unlist(pbapply::pblapply(splittedUniverse[chroms], sample, 1))
        possibleStarts <- randomUniverseRegions@ranges@start - regions@ranges@width + 1
        possibleStarts[possibleStarts < 0] <- 0
        maximumEnd <-randomUniverseRegions@ranges@start + randomUniverseRegions@ranges@width - 1
        maximumEnd[(maximumEnd + regions@ranges@width) >= chromSizes[as.vector(randomUniverseRegions@seqnames),]] <- 
            chromSizes[as.vector(randomUniverseRegions@seqnames)] - regions@ranges@width - 1
        randomStarts <- apply(mapply(seq,possibleStarts,maximumEnd), 2, sample, size = 1)
    }
    granges <- GRanges(regions@seqnames, IRanges(start = randomStarts,
                                                 width = regionsLength),
                       strand=regions@strand)
    return(granges)
}
