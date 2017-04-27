#' @title Generate regions
#' @author Zacharie Menetrier
#' @description Generates random genomic ranges object.
#' 
#' @param n The number of regions.
#' @param size The width of the regions.
#' @param chromSizes=ImportChromSizes("hg19") The chromosome sizes for the 
#' species in consideration.
#' 
#' @return A random generated genomic ranges object.
#' 
#' @usage GenRegions(n, size, chromSizes = LoadChromSizes("hg19"))
#' 
#' @examples 
#' randomRegions <- GenRegions(1000,1000)
#' 
#' @export
GenRegions <- function(n, size, chromSizes = LoadChromSizes("hg19"), ...) {
    # Random chromosomes are sampled.
    chroms <- sample(rownames(chromSizes), size = n, replace = TRUE)
    randomValues <- runif(n)
    randomStarts <- round(randomValues * (chromSizes[chroms,] - size))
    if (sum(randomStarts < 0) > 0) {
        warning("Some randomly generated regions are longer than the chromosome they fell in. They will be shortened.")
        randomStarts[randomStarts < 0] = 0
    }
    granges <- GenomicRanges::GRanges(chroms, IRanges::IRanges(start = randomStarts, width = size))
    return(granges)
}
