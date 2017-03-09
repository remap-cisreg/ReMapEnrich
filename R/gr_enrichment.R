#' @export
GrEnrichment <- function(query, catalog, chromSizes = GetChromSizes(GetChromFile("hg19")), shuffles = 10)
{
    overlaps <- subsetByOverlaps(catalog, query)
    catCount <- lengths(split(overlaps@elementMetadata$id, overlaps@elementMetadata$id))
    shuffleCatCount <- vector()
    shuffleCatCount[names(catCount)] <- 0
    for(i in 1:shuffles)
    {
        shuffle <- GrShuffle(query, chromSizes)
        shuffleOverlaps <- subsetByOverlaps(catalog, shuffle)
        count <- lengths(split(shuffleOverlaps@elementMetadata$id, shuffleOverlaps@elementMetadata$id))
        shuffleCatCount[names(count)] <- shuffleCatCount[names(count)] + count[names(count)]
    }
    categories <- unique(c(names(catCount), names(shuffleCatCount)))
    catLength <- length(categories)
    output <- matrix(ncol = 5, nrow = catLength)
    for(i in 1:catLength)
    {
        catName <- categories[i]
        randomCount <- shuffleCatCount[catName]
        count <- catCount[catName]
        if(is.na(randomCount) || randomCount == 0)
        {
            pValue <- NA
            significance <- NA
            theoricalMean = 0
        }
        else
        {
            theoricalMean <- randomCount / shuffles
            significance <- ppois(count - 1, lambda = theoricalMean, lower.tail = FALSE, log = TRUE) / log(10)
            pValue <- 10 ** significance
            significance <- - significance
        }
        output[i, ] <- c(catName, count, theoricalMean, pValue, significance)
    }
    colnames(output) <- c("category", "nb.overlaps", "random.average", "pValue", "significance")
    output <- data.frame(output, stringsAsFactors = FALSE)
    output$pValue <- as.numeric(output$pValue)
    output$significance <- as.numeric(output$significance) 
    return(output[order(output$significance, decreasing = TRUE),])
}