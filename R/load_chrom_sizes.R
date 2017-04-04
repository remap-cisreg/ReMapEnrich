#' @title Load chromosomes sizes
#' @author Zacharie Menetrier
#' @description Load preloaded chromosome sizes file into a table.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return A table that contains the chromosome lengths.
#' 
#' @examples 
#' hg19ChromSizes <- LoadChromSizes("hg19")
#' 
#' @export
LoadChromSizes <- function(genome) {
    chromFile <- LoadChromFile(genome)
    return(ImportChromSizes(chromFile))
}
