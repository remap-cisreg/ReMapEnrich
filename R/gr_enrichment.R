#' @title Genomic Ranges enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each category of a
#'  genomic ranges object.
#' 
#' @param query The genomic ranges object containing the genomic regions to analyze.
#' @param catalog The genomic ranges object containing the database used for annotation.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome 
#'  lengths for the species in consideration.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must 
#'  exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit
#'  must exceed to be accounted.
#' @param shuffles=6 The number of shuffled genomic regions to be created for 
#' theorical distribution (higher means more accurate).
#' @param tail="lower" If "lower" then, probabilities are P[X > x], 
#'  if "higher", P[X <= x], if "both" then higher or lower is selected
#'  depending on the number of overlaps vs the theorical mean.
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @usage GrEnrichment(query, catalog, chromSizes = LoadChromSizes("hg19"),
#'                     fractionQuery = 0.1,
#' fractionCatalog = 0.1, shuffles = 6, lower = FALSE, pAdjust = "BY")
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed", 
#'                          package = "roken")
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed",
#'                            package = "roken")
#' query <- BedToGranges(queryFile)
#' catalog <- BedToGranges(catalogFile)
#' enrichment <- GrEnrichment(query, catalog)
#' 
#' @export
GrEnrichment <- function(query, catalog, universe = NULL, chromSizes = LoadChromSizes("hg19"),
                         fractionQuery = 0.1,fractionCatalog = 0.1, 
                         shuffles = 6, tail = "lower", pAdjust = "BY") {
    # The categories are extracted from the catalog.
    categories <- unique(catalog@elementMetadata$id)
    categoriesCount <- lengths(split(catalog@elementMetadata$id, 
                                     catalog@elementMetadata$id))
    # Gets the counts list containing the number of overlaps for the query 
    # and the mean number of overlaps for the shuffles.
    countsList <- ComputeEnrichment(query, catalog, chromSizes, fractionQuery,
                                    fractionCatalog, shuffles,
                                    GrIntersect, GrShuffle, categories, universe)
    # Returns all the information from the counts list.
    return(ExtractEnrichment(categories, tail, countsList[[1]], 
                             countsList[[2]], categoriesCount, pAdjust))
}
