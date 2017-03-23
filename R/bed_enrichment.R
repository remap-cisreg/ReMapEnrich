#' @title Bed enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each feature of a bed file.
#' 
#' @param queryFile The bed file containing the genomic regions to analyze.
#' @param catalog The bed file containing the database used for annotation.
#' @param chromFile=LoadChromFile("hg19") A file containing all the chromosome lengths for the species in consideration.
#' @param fractionQuery=0.1 The fraction of coverage (query on catalog) a hit must exceed to be accounted.
#' @param fractionCatalog=0.1 The fraction of coverage (catalog on query) a hit must exceed to be accounted.
#' @param shuffles=6 The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' @param lower=FALSE If FALSE (default), probabilities are P[X > x], otherwise, P[X <= x].
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
BedEnrichment <- function(queryFile, catalogFile, chromFile = ImportChromFile("hg19"),
                          fractionQuery = 0.1, fractionCatalog = 0.1, shuffles = 6, lower = FALSE) {
    # Retrieves all the categories from the catalog for future calculations.
    categories <- unique(BedImport(catalogFile)$name)
    # Gets the counts list containing the number of overlaps for the query and the mean number of overlaps for the shuffles.
    countsList <- ComputeEnrichment(queryFile, catalogFile, chromFile, fractionQuery, fractionCatalog, shuffles,
                                    BedIntersect, BedShuffleTempFile, categories)
    # Returns all the information from the counts list.
    return(ExtractEnrichment(categories, lower, countsList[[1]], countsList[[2]]))
}