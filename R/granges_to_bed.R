#' @title Granges to bed
#' @author Zacharie Menetrier
#' @description This function creates a bed data frame from a 
#' genomic ranges object.
#' The data frame will be ordered as 'chrom', 'chromStart', 'chromEnd',
#' 'name', 'score', 'strand'.
#' 
#' @param granges The genomic ranges object to convert.
#' 
#' @return A data frame containing the informations from the given 
#' genomic ranges object.
#' 
#' @usage grangesToBed <- function(granges)
#' 
#' @examples 
#' catalogFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22.bed", 
#'                             package = "roken")
#' catalog <- bedToGranges(catalogFile)
#' catalog <- grangesToBed(catalogFile)
#' View(catalog)
#' 
#' @export
grangesToBed <- function(granges) {
    chrom <- granges@seqnames
    chromStart <- granges@ranges@start
    chromEnd <- granges@ranges@start + granges@ranges@width
    name <- granges@elementMetadata$id
    strand <- as.vector(granges@strand)
    strand <- gsub(pattern = "[*]", replacement = ".", strand)
    score <- granges@elementMetadata$score
    return(data.frame(chrom, chromStart, chromEnd, name, score, strand))
}
