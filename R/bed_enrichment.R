#' Computes enrichment
#' 
#' Gets the value of genomic enrichment for each feature of a bed file.
#' 
#' @param regions The bed file containing the genomic regions to analyze.
#' @param catalog The bed file containing the database used for annotation.
#' @param chromFile A file containing all the chromosome lengths for the species in consideration.
#' @param fraction The fraction that intersections shall exceed to be considered.
#' @param shuffles The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
BedEnrichment <- function(queryFile, catalogFile, chromFile = GetChromFile("hg19"), fraction = 0.1, shuffles = 10)
{
    cat("Computing overlaps.\n")
    # Extracting all the informations for the overlaps between the regions and the catalog.
    overlaps <- BedIntersect(catalogFile, queryFile, fraction)
    # Splitting the overlaps in a list by the name/category it has been intersectng in the catalog.
    categories <- split(overlaps, overlaps$name)
    # Gets the number of distincts categories.
    catLength <- length(categories)
    # Creates the list that will contain the count for the overlaps of each categories.
    overlapsCount <- list()
    cat(catLength, "categories have been found.\n")
    for(i in 1:catLength)
    {
        # Feeds the counting of the overlaps for each categories.
        overlapsCount[categories[[i]]$name[1]] <- nrow(categories[[i]])
    }
    cat("Computing", shuffles,"shuffles.\n")
    # Creates the list that will contain the count for the overlaps of each categories with suffled regions.
    shuffleOverlapsCount <- list()
    for(i in 1:shuffles)
    {
        cat(i, "\r")
        # Shuffles the given regions.
        shuffle <- ShuffleBed(regions, chromFile, tempFile = TRUE)
        # Gets the overlaps between the shuffled regions and the catalog.
        shuffleOverlaps <- BedIntersect(catalog, shuffle, fraction)
        unlink(shuffle)
        # Splitting the overlaps in a list by the name/category it has been intersectng in the catalog.
        shuffledCat <- split(shuffleOverlaps, shuffleOverlaps$name)
        # Gets the number of differents categories.
        shuffledCatLength <- length(shuffledCat)
        # Break if no category has been found
        if(shuffledCatLength == 0) break
        # For each category of the suffles.
        for(j in 1:shuffledCatLength)
        {
            # Gets the name of the current categories.
            name <- shuffledCat[[j]]$name[1]
            # If the name is found in the shuffled overlaps then add the count to the previous value,
            # else creates a new entry in the count of the shuffled overlaps with the name of the current category.
            if(name %in% names(shuffleOverlapsCount))
            {
                shuffleOverlapsCount[name] <- shuffleOverlapsCount[[name]] + nrow(shuffledCat[[j]])
            }
            else
            {
                shuffleOverlapsCount[name] <- nrow(shuffledCat[[j]])
            }
        }
    }
    # Creates the matrix that will contain the enrichment informations.
    output <- matrix(ncol = 5, nrow = catLength)
    for(i in 1:catLength)
    {
        name <- categories[[i]]$name[1]
        count <- overlapsCount[[name]]
        # If the category is not found in the shuffles, the count is set to 0.
        if(!name %in% names(shuffleOverlapsCount))
        {
            randomCount <- 0
        }
        else
        {
            randomCount <- shuffleOverlapsCount[[name]]
        }
        # Computes the theorical mean from the random count divided by the number of shuffles.
        theoricalMean = randomCount / shuffles
        # If the theorical mean is 0 then the pValue and significance shall not be numbers.
        if(theoricalMean == 0)
        {
            pValue <- NA
            significance <- NA
        }
        else
        {
            # Computing the significance from the poisson distribution.
            significance <- ppois(count - 1, lambda = theoricalMean, lower.tail = FALSE, log = TRUE) / log(10)
            # Computing the pValue from the significance.
            pValue <- 10 ** significance
            significance <- - significance
        }
        output[i, ] <- c(name, count, theoricalMean, pValue, significance)
    }
    #Creation of the data frame from the matrix and reordering by significance.
    colnames(output) <- c("category", "nb.overlaps", "random.average", "pValue", "significance")
    output <- data.frame(output, stringsAsFactors = FALSE)
    output$pValue <- as.numeric(output$pValue)
    output$significance <- as.numeric(output$significance) 
    return(output[order(output$significance, decreasing = TRUE),])
}