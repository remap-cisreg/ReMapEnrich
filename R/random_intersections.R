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
#' lengths for the species in consideration.
#' @param nCores="auto" The number of cores to be used for parallel computations.
#' By default it is the number of available cores minus one.
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
RandomIntersections <- function(catalog, iterations, regionNb, regionSize, chromSizes = LoadChromSizes("hg19"), nCores = "auto") {
    ## Instantiate result table
    categories <- unique(catalog@elementMetadata$id)
    # Computing the number of cores available.
    if(nCores == "auto"){
        nCores <- parallel::detectCores() - 1
        if(nCores <= 0){
            nCores = 1
        }
    }
    cluster <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(cluster, library(S4Vectors))
    parallel::clusterEvalQ(cluster, library(IRanges))
    parallel::clusterEvalQ(cluster, library(GenomicRanges))
    # Creating all the replicates, this should be quick.
    cat("Generating random regions.\n")
    randomRegions <- parallel::parSapply(cl = cluster, X = integer(iterations), FUN = GenRegions, n = regionNb, size = regionSize, chromSizes = chromSizes)
    # Creating all the intersections may take a long time.
    cat("Computing intersections.\n")
    result <- parallel::parLapply(cluster, randomRegions, GrIntersect, catalog = catalog, categories = categories)
    stopCluster(cluster)
    result <- matrix(unlist(result), ncol = length(categories), byrow = TRUE)
    colnames(result) <- categories
    return(result)
}
