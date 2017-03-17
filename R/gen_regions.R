#' @export
GenRegions <- function(n, size, chromSizes = GetChromSizes("hg19")) {
    chroms <- sample(rownames(chromSizes), n, replace = TRUE)
    extremeStarts <- chromSizes[chroms,] - size
    randomStarts <- sample.int(extremeStarts, n)
    if(sum(randomStarts[randomStarts < 0]) > 0)
        stop("The given size for random chromosomic regions is too large and outpass one of the chromosome size.")
    granges <- GRanges(chroms, IRanges(start = randomStarts, width = size))
    return(granges)
}