#' @export
GenRegions <- function(n, size, chromSizes = ImportChromSizes("hg19")) {
    chroms <- sample(rownames(chromSizes), size = n, replace = TRUE)
    extremeStarts <- chromSizes[chroms,] - size + 1
    extremeStarts[extremeStarts < 1] = 1
    randomStarts <- unlist(lapply(extremeStarts, sample.int, size = 1))
    if(sum(randomStarts[randomStarts < 0]) > 0)
        stop("The given size for random chromosomic regions is too large and outpass one of the chromosome size.")
    granges <- GRanges(chroms, IRanges(start = randomStarts, width = size))
    return(granges)
}