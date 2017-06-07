#' @title Load chromosomes sizes file
#' @author Zacharie Menetrier
#' @description Load preloaded chromosome sizes files.
#' 
#' @param genome The name of the species to import the chromosomes from.
#' 
#' @return The path to a file that contains the chromosome lengths.
#' 
#' @usage loadChromFile <- function(genome)
#' 
#' @examples 
#' hg19ChromFile <- loadChromFile("hg19")
#' 
#' @export
loadChromFile <- function(genome) {
    file <- system.file("extdata", paste(genome, ".genome", sep = ""),
                        package = "roken")
    if(file == "")
        stop("No chromosome sizes found for ", genome)
    return(file)
}
