#' @title Random intersections
#' @author Zacharie Menetrier
#' @description Creates random query regions
#' and returns the number of overlaps with a catalogue for each iteration.
#' 
#' @param catalog A GRanges object containing the regions to be overlapped with.
#' @param iterations The number of time the intersections will be computed.
#' @param regionNb=1000 The number of regions to be created for each random query.
#' @param regionSize=1000 The size of each regions for each random query.
#' @param universe=NULL A set of genomic regions that prevent random regions
#' for occuring outside of it.
#' @param included=1 Represents the fraction of each regions that can
#' be outside of the universe.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome 
#' lengths for the species in consideration.
#' @param byChrom=FALSE Will the shuffles/random stay in the chromosome they 
#' originate (TRUE) or can they be placed everywhere on the genome (FALSE)
#' @param shuffle=NULL A set of genomic regions to shuffle for random
#' intersections instead of complete randomly generated regions.
#' @param nCores="auto" The number of cores to be used for parallel computations.
#' By default it is the number of available cores minus one.
#' 
#' @return A data frame with each row containing
#' a number of overlap for each category in the columns.
#' 
#' @usage randomIntersections(catalog, iterations, regionNb,
#' regionSize, chromSizes = LoadChromSizes("hg19")), nCores = "auto"
#' 
#' @examples
#' catalog <- bedToGranges(system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                           package = "roken"))
#' randoms <- randomIntersections(catalog, 500, 1000, 1000)
#' 
#' @export
randomIntersections <- function(catalog, iterations, regionNb = 1000, regionSize = 1000, universe = NULL,
                                included = 1, chromSizes = loadChromSizes("hg19"), 
                                byChrom = FALSE, shuffle = NULL, nCores = "auto") {
    ## Instantiate result table
    categories <- unique(catalog@elementMetadata$id)
    # Computing the number of cores available.
    if(nCores == "auto"){
        nCores <- parallel::detectCores() - 1
        if(nCores <= 0){
            nCores = 1
        }
    } else {
        realNCores <-  parallel::detectCores()
        if (nCores > realNCores) {
            warning("The given number of cores is larger than possible on this computer. 
                    It will be reduced to its maximum.")
            nCores <- realNCores
        }
    }
    cluster <- parallel::makeCluster(nCores)
    parallel::clusterEvalQ(cluster, library(S4Vectors))
    parallel::clusterEvalQ(cluster, library(IRanges))
    parallel::clusterEvalQ(cluster, library(GenomicRanges))
    if (is.null(shuffle)) {
        # Creating all the replicates, this should be quick.
        cat("Generating random regions.\n")
        randomRegions <- parallel::parSapply(cl = cluster, X = integer(iterations), 
                                             FUN = genRegions, n = regionNb, size = regionSize, 
                                             chromSizes = chromSizes, universe = NULL, 
                                             included = 1, byChrom = byChrom)
        
    } else {
        # Creating all the replicates, this should be quick.
        cat("Generating shuffled regions.\n")
        randomRegions <- parallel::parSapply(cl = cluster, X = integer(iterations), FUN = shuffle, 
                                             regions = shuffle, size = regionSize, 
                                             chromSizes = chromSizes, universe = NULL, 
                                             included = 1, byChrom = byChrom)
    }
    # Creating all the intersections may take a long time.
    cat("Computing intersections. This may be long.\n")
    result <- parallel::parLapply(cluster, randomRegions, intersect, 
                                  catalog = catalog, categories = categories)
    parallel::stopCluster(cluster)
    result <- matrix(unlist(result), ncol = length(categories), byrow = TRUE)
    colnames(result) <- categories
    return(result)
}
