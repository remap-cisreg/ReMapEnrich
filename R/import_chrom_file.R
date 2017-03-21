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
    file <- system.file("extdata", paste(genome, ".genome", sep = ""), package = "roken")
    if(file == "")
        stop("No chromosome sizes found for ", genome)
    return(file)
}