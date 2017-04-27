#' @title Genomic ranges shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param regions The genomic ranges object containing the genomic 
#' regions to shuffle.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all 
#' the chromosome lengths for the species in consideration.
#' @param universe=NULL A set of genomic regions that prevent shuffles
#' for occuring outside of it.
#' @param included=1 Represents the fraction of each regions that can
#' be outside of the universe.
#' @param byChrom=FALSE Will the shuffles stay in the chromosome they originate (TRUE)
#' or can they be placed everywhere on the genome (FALSE)
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
GrShuffle <- function(regions, chromSizes = LoadChromSizes("hg19"), universe = NULL, 
                      included = 1, byChrom = FALSE) {
    if (is.null(universe)) {
        universe <- GRanges(rownames(chromSizes),
                            IRanges(start = 0,
                                    width = as.vector(chromSizes[,1]))
                            )
    }
    universe <- reduce(universe)
    if (included < 0 || included > 1) {
        stop("The parameter included should be comprised between 0 and 1.")
    }
    if (byChrom == FALSE) {
        return(GrShuffleUniverse(regions, chromSizes, universe, included))
    } else {
        return(GrShuffleUniverseByChrom(regions, chromSizes, universe, included))
    }
}

GrShuffleUniverseByChrom <- function(regions, chromSizes, universe, included) {
    chroms <- rownames(chromSizes)
    results <- GRanges()
    for (chrom in chroms) {
        regionsChrom <- regions[regions@seqnames == chrom]
        universeChrom <- universe[universe@seqnames == chrom]
        if (length(regionsChrom) > 0) {
            if (length(universeChrom) > 0) {
                results <- c(results, GrShuffleUniverse(regionsChrom, chromSizes, universeChrom, included))
            } else {
                stop(paste("The universe does not contain regions for", chrom, "but the query regions does."))
            }
        }
    }
    return(results)
}

GrShuffleUniverse <- function(regions, chromSizes, universe, included) {
    # Sorting the universe in order to pick only the universe regions that are large enough for the regions.
    universe <- universe[order(universe@ranges@width, decreasing = TRUE)]
    # The query widths are shortened by the included parameter.
    queryWidths <- regions@ranges@width - (regions@ranges@width * (1-included))
    # The regions and the universe is trimmed conditionally.
    overUniverse <- universe[universe@ranges@width >= min(queryWidths)]
    overUniverseLength <- length(universe) - length(overUniverse)
    if (overUniverseLength > 0) {
        warning(paste(overUniverseLength, "universe regions are shorter than the shortest query regions and will be ignored."))
        if (length(overUniverse) >= length(universe)) {
            stop("All universe regions are shorter than the shortest query.")
        }
    }
    overQuery <- regions[regions@ranges@width < universe[1]@ranges@width]
    overQueryLength <- length(regions) - length(overQuery)
    if (overQueryLength > 0) {
        warning(paste(overQueryLength, "query regions are longer than the longest universe regions and will be ignored.")) 
    }
    regions <- overQuery
    if (length(regions) <= 0) {
        stop("All regions are longer than the longest universe regions.")
    }
    queryWidths <- regions@ranges@width - (regions@ranges@width * (1-included))
    universeWidths <- universe@ranges@width
    # The cumulative lengths are calculated to be picked randomly.
    cumLengths <- cumsum(as.numeric(universe@ranges@width))
    # The max indexes represent the first index at which a universe region is large enough for the query.
    maxIndexes <- unlist(lapply(queryWidths, function(queryWidth){return(which.max(universeWidths < queryWidth) - 1)}))
    maxIndexes[maxIndexes <= 0] = length(universe)
    # A random integer is sampled from the maximum cumulative length.
    maxCumLength <- cumLengths[maxIndexes]
    randomsInCum <- unlist(lapply(maxCumLength, sample, size = 1))
    # The universe regions is retrieved from the previous random sample.
    randomIndexes <-unlist(lapply(randomsInCum, function(randomInCum){return(which.max(cumLengths > randomInCum))}))
    sampledRegions <- universe[randomIndexes]
    sampledWidth <- sampledRegions@ranges@width
    # The minimum starts are shortened by the included parameter.
    minStarts <- -(regions@ranges@width - (regions@ranges@width * (included)))
    maxStarts <- sampledWidth - queryWidths
    # Random values between 0 and 1.
    randomValues <- runif(length(regions))
    # The random values are multiplied by the range of the query.
    randomValues <- randomValues * (minStarts + maxStarts)
    # The random values are subtracted by the minimum starts.
    randomValues <- randomValues - minStarts
    # The random values are rounded to become random starts positions.
    randomStarts <- round(randomValues)
    starts <-sampledRegions@ranges@start + randomStarts
    # The starts and ends of each shuffled regions are corrected if they fell outside of the chromosome.
    starts[starts < 0] <- 0
    ends <- starts + queryWidths
    chromWidths <- chromSizes[as.character(sampledRegions@seqnames),]
    overIndexes <- ends > chromWidths
    overWidths <- ends[overIndexes] - chromWidths[overIndexes]
    starts[overIndexes] <- starts[overIndexes] - overWidths
    if (sum(starts < 0) > 0) {
       warning("Some query regions are longer than the chromosome they fell in. They will be shortened.")
        starts[starts < 0] <- 0
    }
    shuffles <- GRanges(sampledRegions@seqnames,
                        IRanges(start = starts,
                                width = queryWidths),
                        strand = sampledRegions@strand)
    return(shuffles)
}
