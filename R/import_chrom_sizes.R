#' @title Import chromosome sizes
#' @author Zacharie Menetrier
#' @description Import preloaded chromosome sizes file into a table.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return A table that contains the chromosome lengths.
#' 
#' @export
ImportChromSizes <- function(genome) {
    chromFile <- ImportChromFile(genome)
    chromSizes <- read.table(chromFile, header = FALSE, sep = "\t",stringsAsFactors = FALSE, quote = "", row.names = 1)
    colnames(chromSizes) = "size"
    return(chromSizes)
}