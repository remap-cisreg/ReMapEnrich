#' @title bed temporary file regions shuffle
#' @author Zacharie Menetrier
#' @description Shuffle genomic regions among a defined genome.
#' 
#' @param bedFile The bed file containing the genomic regions to shuffle.
#' @param chromFile=ImportChromFile("hg19") A file containing all the 
#' chromosome lengths for the species in consideration.
#' 
#' @return A temporary file containing the new shuffled chromosic regions.
BedShuffleTempFile <- function(bedFile, universe = NULL, chromFile = LoadChromFile("hg19")) {
    tempPath <- tempfile()
    # Calls bedtools with the corresponding parameters.
    if (is.null(universe)) {
        command <- paste("shuffleBed -i", bedFile, "-g", chromFile, "-chrom >", 
                         tempPath)
    } else {
        command <- paste("shuffleBed -i", bedFile, "-g", chromFile, "-incl", universe, "-chrom >", 
                         tempPath)
    }

    system(command)
    return(tempPath)
}
