#' @title Generate regions
#' @author Zacharie Menetrier
#' @description Generates random genomic ranges object.
#' 
#' @param n The number of regions.
#' @param size The width of the regions.
#' @param chromSizes=ImportChromSizes("hg19") The chromosome sizes for the 
#' species in consideration.
#' @param universe=NULL A set of genomic regions that prevent random regions
#' for occuring outside of it.
#' @param included=1 Represents the fraction of each regions that can
#' be outside of the universe.
#' 
#' @return A random generated genomic ranges object.
#' 
#' @usage GenRegions(n, size, chromSizes = LoadChromSizes("hg19")), 
#' universe = NULL, included = 1
#' 
#' @examples 
#' randomRegions <- GenRegions(1000,1000)
#' 
#' @export
GenRegions <- function(n, size, chromSizes = LoadChromSizes("hg19"), universe = NULL, included = 1, ...) {
    # Random chromosomes are sampled.
    chroms <- sample(rownames(chromSizes), size = n, replace = TRUE)
    randomValues <- runif(n)
    randomStarts <- round(randomValues * (chromSizes[chroms,] - size))
    if (sum(randomStarts < 0) > 0) {
        warning("Some randomly generated regions are longer than the chromosome they fell in. They will be shortened.")
        randomStarts[randomStarts < 0] = 0
    }
    granges <- GenomicRanges::GRanges(chroms, IRanges::IRanges(start = randomStarts, width = size))
    if (!is.null(universe)) {
        granges <- Shuffle(granges, chromSizes, universe, included)
    }
    return(granges)
}
