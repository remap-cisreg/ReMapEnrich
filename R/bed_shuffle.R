#' @title bed regions shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param bedFile The bed file containing the genomic regions to shuffle.
#' @param chromFile=GetChromFile("hg19") A file containing all the chromosome lengths for the species in consideration.
#' @param outputFile="" A path where the output file will be created, if this string is empty then a data frame is returned.
#' 
#' @return A data frame containing the new shuffled chromosic regions or the path to a file containing this informations.
#' 
#' @export
BedShuffle <- function(bedFile, chromFile = ImportChromFile("hg19"), outputFile = "") {
    path <- outputFile
    if(outputFile == "")
        path <- tempfile()
    command <- paste("shuffleBed -i", bedFile, "-g", chromFile, "-chrom >", path)
    system(command)
    if(outputFile != "")
        return(path)
    shuffle <- BedImport(path)
    unlink(path)
    return(shuffle)
}

BedShuffleTempFile <- function(bedFile, chromFile = ImportChromFile("hg19")) {
    tempPath <- tempfile()
    command <- paste("shuffleBed -i", bedFile, "-g", chromFile, "-chrom >", tempPath)
    system(command)
    return(tempPath)
}