#' @title Generate regions
#' @author Zacharie Menetrier
#' @description Generates random genomic ranges object.
#' 
#' @param n The number of regions.
#' @param size The width of the regions.
#' @param chromSizes=ImportChromSizes("hg19") The chromosome sizes for the species in consideration.
#' 
#' @return A random generated genomic ranges object.
#' 
#' @usage GenRegions(n, size, chromSizes = LoadChromSizes("hg19"))
#' 
#' @examples 
#' randomRegions <- GenRegions(1000,1000)
#' 
#' @export
GenRegions <- function(n, size, chromSizes = LoadChromSizes("hg19")) {
    # Random chromosomes are sampled.
    chroms <- sample(rownames(chromSizes), size = n, replace = TRUE)
    # The most extreme starts are retrived from the chromosome sizes.
    extremeStarts <- chromSizes[chroms,] - size + 1
    # If the extreme starts are < 1 then set them to 1.
    extremeStarts[extremeStarts < 1] = 1
    randomStarts <- unlist(lapply(extremeStarts, sample.int, size = 1))
    # If the size of the region is greater than the size of the chromosome then throw error.
    if(sum(randomStarts[randomStarts < 0]) > 0)
        stop("The given size for random chromosomic regions is too large and outpass one of the chromosome size.")
    granges <- GRanges(chroms, IRanges(start = randomStarts, width = size))
    return(granges)
}
