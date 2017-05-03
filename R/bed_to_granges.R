#' @title Bed to GRanges
#' @author Martin Mestdagh
#' @description This function loads a bed file and stores it as a genomic 
#' ranges object.
#' The tab-delimited file must be ordered as 'chrom', 'chromStart', 
#' 'chromEnd', 'name', 'score', 'strand'.
#' The minimal bed file must have the 'chrom', 'chromStart', 
#' 'chromEnd' columns.
#' Any columns after the strand column are ignored.
#' 
#' @param path Location of your file.
#' 
#' @return A genomic ranges object containing the informations from the given
#' bed file.
#' 
#' @usage BedToGranges(path)
#' 
#' @examples 
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed",
#'                          package = "roken")
#' granges <- BedToGranges(queryFile)
#' 
#' @export
BedToGranges <- function(path) {
    # Imports the bed file in a data frame.
    bedData <- BedImport(path)
    # If the data frame has more than 6 columns then remove them.
    if(ncol(bedData) > 6)
        bedData <- bedData[,-c(7:ncol(bedData))]
    # If the data frame has less than 3 columns then throw an error.
    if(ncol(bedData)<3)
        stop("File has less than 3 columns")
    # If the strand is known in the data frame then replace it by the
    # granges equivalent.
    if ('strand' %in% colnames(bedData))
        bedData$strand <- gsub(pattern= "[^+-]+", replacement = '*',
                               x = bedData$strand)
    # Construct the grangesanges object depending on the number of columns.
    if (ncol(bedData) == 3) {
        granges <- with(bedData, GenomicRanges::GRanges(chrom, IRanges::IRanges(chromStart, chromEnd)))
    } else if (ncol(bedData)==4) {
        granges = with(bedData, GenomicRanges::GRanges(chrom, IRanges::IRanges(chromStart, chromEnd),
                                        id = name))
    } else if (ncol(bedData)==5) {
        granges <- with(bedData, GenomicRanges::GRanges(chrom, IRanges::IRanges(chromStart, chromEnd),
                                         id = name, score = score))
    } else if (ncol(bedData)==6) {
        granges <- with(bedData, GenomicRanges::GRanges(chrom, IRanges::IRanges(chromStart, chromEnd),
                                         id = name, score = score, strand = strand))
    } else {
        stop("Error while constructing the GRanges object. 
             No number of columns have been matched.")
    }
    return(granges)
}
