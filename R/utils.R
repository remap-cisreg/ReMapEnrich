#' @title Get chromosome file
#' @author Zacharie Menetrier
#' @description Load preloaded chromosome sizes files.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return The path to a file that contains the chromosome lengths.
#' 
#' @export
LoadChromFile <- function(genome) {
    return(system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken"))
}

#' @title Get chromosome sizes
#' @author Zacharie Menetrier
#' @description Load preloaded chromosome sizes file into a table.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return A table that contains the chromosome lengths.
#' 
#' @export
LoadChromSizes <- function(genome) {
    chromFile <- LoadChromFile(genome)
    chromSizes <- read.table(chromFile, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
    return(chromSizes)
}