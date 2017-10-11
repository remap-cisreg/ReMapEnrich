#' @title Enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each category of a
#' genomic ranges object against a catalogue.
#' 
#' @param query The genomic ranges object containing the genomic regions to analyze.
#' @param catalog The genomic ranges object containing the database used for annotation.
#' @param chromSizes=LoadChromSizes("hg38") A vector containing all the chromosome 
#' lengths for the species in consideration.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must 
#' exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit
#' must exceed to be accounted.
#' @param shuffles=6 The number of shuffled genomic regions to be created for 
#' theorical distribution (higher means more accurate).
#' @param tail="lower" If "lower" then, probabilities are P[X > x], 
#' if "higher", P[X <= x], if "both" then higher or lower is selected
#' depending on the number of overlaps vs the theorical mean.
#' @param pAdjust="BY" The method that will be used for correcting the p-values.
#' BH, BY and bonferroni are available.
#' @param universe=NULL A set of genomic regions that prevent shuffles
#' for occuring outside of it.
#' @param included=1 Represents the fraction of each regions that can
#' be outside of the universe.
#' @param byChrom=FALSE Will the shuffles stay in the chromosome they originate (TRUE)
#' or can they be placed everywhere on the genome (FALSE)
#' @param nCores=1 The number of cores to be used for parallel computations.
#' 1 by default will not be using parallel calculations.
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @usage enrichment(query, catalog, chromSizes = LoadChromSizes("hg38"),
#'                     fractionQuery = 0.1, fractionCatalog = 0.1,
#'                     shuffles = 6, lower = FALSE, pAdjust = "BY",
#'                      byChrom = FALSE, included = 1, nCores = 1)
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed", 
#'                          package = "roken")
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                            package = "roken")
#' query <- bedToGranges(queryFile)
#' catalog <- bedToGranges(catalogFile)
#' enrichment <- enrichment(query, catalog)
#' 
#' @export
enrichment <- function(query, catalog, universe = NULL, chromSizes = loadChromSizes("hg38"),
                         fractionQuery = 0.1,fractionCatalog = 0.1, 
                         shuffles = 6, tail = "lower", pAdjust = "BY", byChrom = FALSE, included = 1, nCores = 1) {
    # The categories are extracted from the catalog.
    categories <- unique(catalog@elementMetadata$id)
    categoriesCount <- lengths(split(catalog@elementMetadata$id, 
                                     catalog@elementMetadata$id))
    # Gets the counts list containing the number of overlaps for the query 
    # and the mean number of overlaps for the shuffles.
    countsList <- computeEnrichment(query, catalog, chromSizes, fractionQuery,
                                    fractionCatalog, shuffles, categories, universe, byChrom, included, nCores)
    # Returns all the information from the counts list.
    return(extractEnrichment(categories, tail, countsList[[1]], 
                             countsList[[2]], categoriesCount, pAdjust))
}
