#' @title Computes enrichment
#' @author Zacharie Menetrier
#' @description Gets the number of overlaps for the query and the shuffles for
#'  each category of the catalog.
#' 
#' @param query The object containing the genomic regions to analyze.
#' @param catalog The object containing the database used for annotation.
#' @param chromSizes An object containing all the chromosome lengths for the 
#'  species in consideration.
#' @param fractionQuery The fraction of coverage (query on catalog) a hit must
#'  exceed to be accounted.
#' @param fractionCatalog The fraction of coverage (catalog on query) a hit must
#'  exceed to be accounted.
#' @param shuffles The number of shuffled genomic regions to be created for 
#' theorical distribution (higher means more accurate).
#' @param categories The categories contained in the catalog.
#' This option is leaved for faster calculation when this function is runned
#' multiple times.
#' @param universe=NULL A set of genomic regions that prevent shuffles
#' for occuring outside of it.
#' @param byChrom=FALSE Will the shuffles stay in the chromosome they originate (TRUE)
#' or can they be placed everywhere on the genome (FALSE)
#' @param included=1 Represents the fraction of each regions that can
#' be outside of the universe.
#' 
#' @return A list containing the number of overlaps for the query and the 
#' mean number of overlaps for the shuffles.
computeEnrichment <- function(query,
                              catalog, 
                              chromSizes,
                              fractionQuery, 
                              fractionCatalog, 
                              shufflesNumber,
                              categories,
                              universe,
                              byChrom,
                              included,
                              nCores) {
    catNumber <- length(categories)
    if(catNumber < 1)
        stop("The catalog does not contain any category.")
    # Computes the intersections betwen query and catalog.
    cat("Computing intersections.\n")
    catCount <- intersect(query, catalog, fractionQuery, 
                                  fractionCatalog, categories)
    # Shuffles are created and computed as the query for bootstrapping.
    shuffleCatCount <- vector()
    shuffleCatCount[categories] <- 0
    if(nCores == 1){
        cat("Computing shuffles. May take time.\nConsider using parallelization with the 'nCores' parameter.\n")
        shuffles <- replicate(shufflesNumber, shuffle(query, chromSizes, universe, included, byChrom))
        # The theorical means are calculated from the shuffles overlaps.
        shuffleCatCount <- sapply(shuffles, intersect, catalog = catalog, fractionQuery = fractionQuery,
                                  fractionCatalog = fractionCatalog, categories = categories)
    }else{
        cat(paste("Computing shuffles with ", nCores, " cores."," May take time.\n", sep = ""))
        realNCores <-  parallel::detectCores()
        if (nCores > realNCores) {
            warning("The given number of cores is larger than possible on this computer. It will be reduced to its maximum.")
            nCores <- realNCores
        }
        cluster <- parallel::makeCluster(nCores)
        parallel::clusterEvalQ(cluster, library(S4Vectors))
        parallel::clusterEvalQ(cluster, library(IRanges))
        parallel::clusterEvalQ(cluster, library(GenomicRanges))
        shuffles <- parallel::parSapply(cl = cluster, X = integer(shufflesNumber), FUN = shuffle, regions = query, chromSizes = chromSizes,
                                        universe = universe, included = included, byChrom = byChrom)
        shuffleCatCount <- parallel::parSapply(cl = cluster, X = shuffles, FUN = intersect, catalog = catalog, fractionQuery = fractionQuery,
                                  fractionCatalog = fractionCatalog, categories = categories)
        parallel::stopCluster(cluster)
    }
    shuffleCatCount <- rowMeans(shuffleCatCount)
    return(list(catCount, shuffleCatCount))
}
