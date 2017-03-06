#' Compute enrichment
#' 
#' Gets the value of genomic enrichment from bed files.
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
enrichment.bed = function(regions, catalog, genome, fraction = 0, shuffles = 100)
{
    cat("COMPUTING OVERLAPS\n")
    # Counting the number of overlaps between the regions and the catalog.
    overlaps = count.overlaps(catalog, regions, fraction)
    random.overlaps = 0
    cat("COMPUTING", shuffles, "SHUFFLES\n")
    # Creating the shuffled genomic regions.
    for(i in 1:shuffles)
    {
        cat("#",i, "\n")
        shuffle = shuffle.bed.temp.file(regions, genome)
        # Counting the number of overlaps between the shuffled regions and the catalog.
        random.overlaps = random.overlaps + count.overlaps(shuffle, catalog, fraction)
        unlink(shuffle)
    }
    # Creating the theorical mean for the poisson distribution.
    theorical.mean = random.overlaps / shuffles
    # If the theorical mean is 0 then the p.value and significance shall not be numbers.
    if(theorical.mean == 0)
    {
        p.value = NA
        significance = NA
    }
    else
    {
        # Computing the significance from the poisson distribution.
        significance = ppois(overlaps - 1, lambda = theorical.mean, lower.tail = FALSE, log = TRUE) / log(10)
        # Computing the p.value from the significance.
        p.value = 10 ** significance
        significance = - significance
    }
    # Creating a data frame from the calculation of the enrichment.
    enrich.tab = data.frame(overlaps, theorical.mean, p.value, significance)
    return(enrich.tab)
}