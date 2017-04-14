#' @title Random intersections
#' @author Zacharie Menetrier
#' @description Creates random query regions
#' and returns the number of overlaps with a catalogue for each iteration.
#' 
#' @param catalog A GRanges object containing the regions to be overlapped with.
#' @param iterations The number of time the intersections will be computed.
#' @param regionNb The number of regions to be created for each random query.
#' @param regionSize The size of each regions for each random query.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome 
#'  lengths for the species in consideration.
#' 
#' @return A data frame with each row containing
#' a number of overlap for each category in the columns.
#' 
#' @usage RandomIntersections(catalog, iterations, regionNb,
#' regionSize, chromSizes = LoadChromSizes("hg19"))
#' 
#' @examples
#' catalog <- BedToGranges(system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                           package = "roken"))
#' randoms <- RandomIntersections(catalog, 500, 1000, 1000)
#' 
#' @export
RandomIntersections <- function(catalog, iterations, regionNb, regionSize, chromSizes = LoadChromSizes("hg19")) {
    ## Instantiate result table
    categories <- unique(catalog@elementMetadata$id)
    # Creating all the replicates, this should be quick.
    cat("Generating random regions.\n")
    randomRegions <- pbapply::pbreplicate(iterations, GenRegions(regionNb, regionSize, chromSizes))
    # Creating all the intersections may take a long time.
    cat("Computing intersections.\n")
    result <- pbapply::pblapply(randomRegions, GrIntersect, catalog = catalog, categories = categories)
    result <- matrix(unlist(result), ncol = length(categories), byrow = TRUE)
    colnames(result) <- categories
    return(result)
}
