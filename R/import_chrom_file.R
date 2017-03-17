#' @title Import chromosome file
#' @author Zacharie Menetrier
#' @description Import preloaded chromosome sizes files.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return The path to a file that contains the chromosome lengths.
#' 
#' @export
ImportChromFile <- function(genome) {
    return(system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken"))
}