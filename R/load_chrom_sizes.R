#' @title Load chromosomes sizes
#' @author Zacharie Menetrier
#' @description Load preloaded chromosome sizes file into a table.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return A table that contains the chromosome lengths.
#' 
#' @usage loadChromSizes <- function(genome)
#' 
#' @examples 
#' hg19ChromSizes <- loadChromSizes("hg19")
#' 
#' @export
loadChromSizes <- function(genome) {
    chromFile <- loadChromFile(genome)
    return(importChromSizes(chromFile))
}
