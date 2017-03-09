#' Genomic regions shuffle
#' 
#' Shuffle genomic regions among a defined genome.
#' 
#' @param regions The bed file containing the genomic regions to shuffle.
#' @param genome A file containing all the chromosome lengths for the species in consideration.
#' 
#' @return A data frame containing the new shuffled chromosic regions.
#' 
#' @export
BedShuffle <- function(regions, chromFile = GetChromFile("hg19"), tempFile = FALSE)
{
    tempPath <- tempfile()
    command <- paste("shuffleBed -i", regions, "-g", chromFile, "-chrom >", tempPath)
    system(command)
    if(tempFile)
    {
        return(tempFile)
    }
    shuffle <- BedImport(tempPath)
    unlink(tempPath)
    return(shuffle)
}