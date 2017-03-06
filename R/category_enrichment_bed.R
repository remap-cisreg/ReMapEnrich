#' Compute categorical enrichment
#' 
#' Gets the value of genomic enrichment for each feature of a bed file.
#' 
#' @param regions The bed file containing the genomic regions to analyze.
#' @param catalog The bed file containing the database used for annotation.
#' @param genome A file containing all the chromosome lengths for the species in consideration.
#' @param fraction The fraction that intersections shall exceed to be considered.
#' @param shuffles The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
category.enrichment.bed = function(regions, catalog, genome, fraction = 0.1, shuffles = 10)
{
    cat("Computing overlaps.\n")
    # Extracting all the informations for the overlaps between the regions and the catalog.
    overlaps = intersect.bed(catalog, regions, fraction)
    # Splitting the overlaps in a list by the name/category it has been intersectng in the catalog.
    categories = split(overlaps, overlaps$name)
    # Gets the number of distincts categories.
    categories.length = length(categories)
    # Creates the list that will contain the count for the overlaps of each categories.
    overlaps.count = list()
    cat(categories.length, "categories have been found.\n")
    for(i in 1:categories.length)
    {
        # Feeds the counting of the overlaps for each categories.
        overlaps.count[categories[[i]]$name[1]] = nrow(categories[[i]])
    }
    cat("Computing", shuffles,"shuffles.\n")
    # Creates the list that will contain the count for the overlaps of each categories with suffled regions.
    shuffle.overlaps.count = list()
    for(i in 1:shuffles)
    {
        cat(i, "\r")
        # Shuffles the given regions.
        shuffle = shuffle.bed.temp.file(regions, genome)
        # Gets the overlaps between the shuffled regions and the catalog.
        shuffle.overlaps = intersect.bed(catalog, shuffle, fraction)
        unlink(shuffle)
        # Splitting the overlaps in a list by the name/category it has been intersectng in the catalog.
        shuffle.categories = split(shuffle.overlaps, shuffle.overlaps$name)
        # Gets the number of differents categories.
        shuffle.categories.length = length(shuffle.categories)
        # For each category of the suffles.
        if(shuffle.categories.length == 0) break
        for(j in 1:shuffle.categories.length)
        {
            # Gets the name of the current categories.
            name = shuffle.categories[[j]]$name[1]
            # If the name is found in the shuffled overlaps then add the count to the previous value,
            # else creates a new entry in the count of the shuffled overlaps with the name of the current category.
            if(name %in% names(shuffle.overlaps.count))
            {
                shuffle.overlaps.count[name] = shuffle.overlaps.count[[name]] + nrow(shuffle.categories[[j]])
            }
            else
            {
                shuffle.overlaps.count[name] = nrow(shuffle.categories[[j]])
            }
        }
    }
    # Creates the matrix that will contain the enrichment informations.
    output = matrix(ncol = 5, nrow = categories.length)
    for(i in 1:categories.length)
    {
        name = categories[[i]]$name[1]
        count = overlaps.count[[name]]
        # If the category is not found in the shuffles, the count is set to 0.
        if(!name %in% names(shuffle.overlaps.count))
        {
            random.count = 0
        }
        else
        {
            random.count = shuffle.overlaps.count[[name]]
        }
        # Computes the theorical mean from the random count divided by the number of shuffles.
        theorical.mean = random.count / shuffles
        # If the theorical mean is 0 then the p.value and significance shall not be numbers.
        if(theorical.mean == 0)
        {
            p.value = NA
            significance = NA
        }
        else
        {
            # Computing the significance from the poisson distribution.
            significance = ppois(count - 1, lambda = theorical.mean, lower.tail = FALSE, log = TRUE) / log(10)
            # Computing the p.value from the significance.
            p.value = 10 ** significance
            significance = - significance
        }
        output[i,] = c(name, count, theorical.mean, p.value, significance)
    }
    #Creation of the data frame from the matrix and reordering by significance.
    colnames(output) = c("category", "nb.overlaps", "random.average", "p.value", "significance")
    output = data.frame(output, stringsAsFactors = FALSE)
    output$p.value = as.numeric(output$p.value)
    output$significance = as.numeric(output$significance) 
    return(output[order(output$significance, decreasing = TRUE),])
}