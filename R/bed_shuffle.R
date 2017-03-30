#' @title bed regions shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param bedFile The bed file containing the genomic regions to shuffle.
#' @param chromFile=ImportChromFile("hg19") A file containing all the chromosome lengths for the species in consideration.
#' @param outputFile="" A path where the output file will be created, if this string is empty then a data frame is returned.
#' 
#' @return A data frame containing the new shuffled chromosic regions or the path to a file containing this informations.
#' 
#' @usage BedShuffle(bedFile, chromFile = LoadChromFile("hg19"), outputFile = "")
#' 
#' @example
#' queryFile <- system.file("extdata", "ReMap_nrPeaks_public_chr22_SOX2.bed", package = "roken")
#' shuffle <- BedShuffle(queryFile)
#' 
#' @export
BedShuffle <- function(bedFile, chromFile = LoadChromFile("hg19"), outputFile = "") {
    # If no output file is given then the file path will be a temporary file.
    path <- outputFile
    if(outputFile == "")
        path <- tempfile()
    # Calls bedtools with the corresponding parameters.
    command <- paste("shuffleBed -i", bedFile, "-g", chromFile, "-chrom >", path)
    system(command)
    # If an output file has been given then return the new file.
    if(outputFile != "")
        return(path)
    # Else destroy the temporary file and return the data frame.
    shuffle <- BedImport(path)
    unlink(path)
    return(shuffle)
}