#' GRanges to bed
#'
#' This function create a bed like data frame from a genomic ranges object.
#' The data frame will be ordered as 'chrom', 'chromStart', 'chromEnd', 'name', 'score', 'strand'.
#' 
#' @param granges The genomic ranges object to convert.
#' 
#' @return A data frame containing the informations from the given genomic ranges object.
#' 
#' @export
GrangesToBed <- function(granges)
{
    chrom <- granges@seqnames
    chromStart <- granges@ranges@start
    chromEnd <- granges@ranges@start + granges@ranges@width
    name <- granges@elementMetadata$id
    strand <- as.vector(granges@strand)
    strand <-gsub(pattern = "[*]", replacement = ".", strand)
    score <- granges@elementMetadata$score
    return(data.frame(chrom, chromStart, chromEnd, name, score, strand))
}