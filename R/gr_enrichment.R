#' @title Computes enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each category of a genomic ranges object.
#' 
#' @param query The genomic ranges object containing the genomic regions to analyze.
#' @param catalog The genomic ranges object containing the database used for annotation.
#' @param chromSizes=LoadChromSizes("hg19") A vector containing all the chromosome lengths for the species in consideration.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit must exceed to be accounted.
#' @param shuffles=6 The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' @param lower=FALSE If FALSE (default), probabilities are P[X > x], otherwise, P[X <= x].
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
GrEnrichment <- function(query,
                         catalog,
                         chromSizes = ImportChromSizes("hg19"),
                         fractionQuery = 0.1,
                         fractionCatalog = 0.1,
                         shuffles = 6,
                         lower = FALSE) {
    # The categories are extracted from the catalog.
    categories <- unique(catalog@elementMetadata$id)
    countsList <- ComputeEnrichment(query, catalog, chromSizes, fractionQuery, fractionCatalog, shuffles, lower,
                                    GrIntersect, GrShuffle, categories)
    return(ExtractEnrichment(categories, lower, countsList[[1]], countsList[[2]], shuffles))
}