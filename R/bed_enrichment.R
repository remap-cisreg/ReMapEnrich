#' @title Computes enrichment
#' @author Zacharie Menetrier
#' @description Gets the value of genomic enrichment for each feature of a bed file.
#' 
#' @param queryFile The bed file containing the genomic regions to analyze.
#' @param catalog The bed file containing the database used for annotation.
#' @param chromFile=LoadChromFile("hg19") A file containing all the chromosome lengths for the species in consideration.
#' @param fraction=0.1 The fraction that intersections shall exceed to be considered.
#' @param shuffles=6 The number of shuffled genomic regions to be created for theorical distribution (higher means more accurate).
#' @param lower=FALSE If FALSE (default), probabilities are P[X > x], otherwise, P[X <= x].
#' 
#' @return A data frame containing the enrichment informations.
#' 
#' @export
BedEnrichment <- function(queryFile,
                          catalogFile,
                          chromFile = ImportChromFile("hg19"),
                          fractionQuery = 0.1,
                          fractionCatalog = 0.1,
                          shuffles = 6,
                          lower = FALSE) {
    categories <- unique(BedImport(catalogFile)$name)
    countsList <- ComputeEnrichment(queryFile, catalogFile, chromFile, fractionQuery, fractionCatalog, shuffles, lower,
                                    BedIntersect, BedShuffleTempFile, categories)
    return(ExtractEnrichment(categories, lower, countsList[[1]], countsList[[2]], shuffles))
}